(** Spec DSL - SSOT-Driven Validation System

    설계 문서(Spec)가 Single Source of Truth가 되어
    코드, Validator, 테스트가 모두 이로부터 파생됩니다.

    Core Concepts:
    - SPEC: 사전/사후 조건, 불변조건, 상태 전이 규칙
    - IMPL: SPEC을 만족하는 구현 (witness)
    - Make_verified: 런타임 계약 검증 자동 래핑
    - Migration: 버전 변경 시 호환성 검증

    사용 예:
    {[
      (* 1. 스펙 정의 (SSOT) *)
      module Counter_spec : SPEC = struct
        type input = int
        type output = int
        type state = { value : int }

        let precondition s i = i > 0 && s.value + i <= 100
        let postcondition s i o = o = s.value + i
        let invariant s = s.value >= 0 && s.value <= 100
        let transition s i = { value = s.value + i }
      end

      (* 2. 구현 *)
      module Counter_impl : IMPL = struct
        include Counter_spec
        let run s i = (s.value + i, { value = s.value + i })
      end

      (* 3. 검증된 조합 *)
      module Verified_counter = Make_verified(Counter_spec)(Counter_impl)
    ]}
*)

open Validator_eio

(** {1 Core Specification Types} *)

(** 기본 스펙 시그니처 *)
module type SPEC = sig
  type input
  type output
  type state

  (** 사전 조건: 입력이 유효한가? *)
  val precondition : state -> input -> bool

  (** 사후 조건: 출력이 스펙을 만족하는가? *)
  val postcondition : state -> input -> output -> bool

  (** 불변 조건: 상태가 항상 유효한가? *)
  val invariant : state -> bool

  (** 상태 전이: 입력에 의해 상태가 어떻게 변하는가? *)
  val transition : state -> input -> state
end

(** 버전이 있는 스펙 *)
module type VERSIONED_SPEC = sig
  include SPEC

  val version : int
  val changelog : string list
end

(** 구현 시그니처 *)
module type IMPL = sig
  include SPEC

  (** 실제 구현: 입력 → (출력, 새 상태) *)
  val run : state -> input -> output * state
end

(** {1 Contract Violations} *)

type violation =
  | Precondition_violated of string
  | Postcondition_violated of string
  | Invariant_violated of string
  | Transition_mismatch of string

type 'a verified_result =
  | Verified of 'a
  | Violation of violation

(** {1 Verified Wrapper} *)

(** 모든 계약을 런타임 검증하는 래퍼 *)
module Make_verified
    (S : SPEC)
    (I : IMPL with type input = S.input
                and type output = S.output
                and type state = S.state) = struct

  type input = S.input
  type output = S.output
  type state = S.state

  let run_verified state input : (output * state) verified_result =
    (* 1. 사전 조건 검증 *)
    if not (S.precondition state input) then
      Violation (Precondition_violated "Input violates precondition")

    (* 2. 불변 조건 (실행 전) *)
    else if not (S.invariant state) then
      Violation (Invariant_violated "State invariant violated before execution")

    else
      let (output, new_state) = I.run state input in

      (* 3. 사후 조건 검증 *)
      if not (S.postcondition state input output) then
        Violation (Postcondition_violated "Output violates postcondition")

      (* 4. 불변 조건 (실행 후) *)
      else if not (S.invariant new_state) then
        Violation (Invariant_violated "State invariant violated after execution")

      (* 5. 상태 전이 일치 검증 *)
      else
        let expected_state = S.transition state input in
        if new_state <> expected_state then
          Violation (Transition_mismatch "Actual state doesn't match expected transition")
        else
          Verified (output, new_state)

  (** 예외 발생 버전 *)
  let run_verified_exn state input =
    match run_verified state input with
    | Verified result -> result
    | Violation v ->
      let msg = match v with
        | Precondition_violated s -> "Precondition: " ^ s
        | Postcondition_violated s -> "Postcondition: " ^ s
        | Invariant_violated s -> "Invariant: " ^ s
        | Transition_mismatch s -> "Transition: " ^ s
      in
      failwith msg

  (** Validator로 변환 *)
  let to_validator
      ~name:validator_name
    : (module VALIDATOR with type state = state * input and type context = output option) =
    (module struct
      type state = S.state * S.input
      type context = S.output option

      let name = validator_name

      let validate (st, inp) =
        match run_verified st inp with
        | Verified (out, _) ->
          { verdict = Pass "All contracts satisfied";
            confidence = 1.0;
            context = Some out;
            children = [];
            metadata = [("spec", validator_name)]; }
        | Violation v ->
          let (reason, violation_type) = match v with
            | Precondition_violated s -> (s, "precondition")
            | Postcondition_violated s -> (s, "postcondition")
            | Invariant_violated s -> (s, "invariant")
            | Transition_mismatch s -> (s, "transition")
          in
          { verdict = Fail reason;
            confidence = 0.0;
            context = None;
            children = [];
            metadata = [("violation", violation_type)]; }
    end)
end

(** {1 Spec Composition} *)

(** 두 스펙의 순차 합성: A의 출력이 B의 입력 *)
module Compose_spec
    (A : SPEC)
    (B : SPEC with type input = A.output) = struct

  type input = A.input
  type output = B.output
  type state = A.state * B.state

  let precondition (sa, _sb) i = A.precondition sa i

  let postcondition (sa, sb) i o =
    (* A를 실행한 결과로 B를 검증 *)
    let a_out = A.transition sa i in
    (* 중간 출력 추정 (실제론 run 필요) *)
    A.postcondition sa i (Obj.magic a_out) &&
    B.postcondition sb (Obj.magic a_out) o

  let invariant (sa, sb) = A.invariant sa && B.invariant sb

  let transition (sa, sb) i =
    let sa' = A.transition sa i in
    let b_input = Obj.magic sa' in  (* A의 새 상태를 B의 입력으로 *)
    let sb' = B.transition sb b_input in
    (sa', sb')
end

(** {1 Migration Framework} *)

(** 스펙 마이그레이션 시그니처 *)
module type SPEC_MIGRATION = sig
  module Old : VERSIONED_SPEC
  module New : VERSIONED_SPEC

  (** 하위 호환성: Old 입력이 New에서도 유효? *)
  val is_backward_compatible : Old.state -> Old.input -> bool

  (** 상위 호환성: New 출력이 Old 소비자에서도 유효? *)
  val is_forward_compatible : New.output -> bool

  (** 상태 마이그레이션 *)
  val migrate_state : Old.state -> New.state

  (** 입력 마이그레이션 *)
  val migrate_input : Old.input -> New.input option
end

(** 마이그레이션 검증기 *)
module Verify_migration (M : SPEC_MIGRATION) = struct
  (** 하위 호환성 검사 *)
  let check_backward_compat old_states old_inputs =
    List.for_all2 M.is_backward_compatible old_states old_inputs

  (** 상위 호환성 검사 *)
  let check_forward_compat new_outputs =
    List.for_all M.is_forward_compatible new_outputs

  (** 불변조건 보존 검사 *)
  let check_invariant_preservation old_state =
    if not (M.Old.invariant old_state) then
      true  (* 이미 유효하지 않은 상태는 스킵 *)
    else
      let new_state = M.migrate_state old_state in
      M.New.invariant new_state

  (** Validator로 변환 *)
  let to_validator ~sw:_sw
    : (module VALIDATOR with type state = M.Old.state * M.Old.input list and type context = unit) =
    (module struct
      type state = M.Old.state * M.Old.input list
      type context = unit

      let name = Printf.sprintf "migration_v%d_to_v%d" M.Old.version M.New.version

      let validate (old_state, old_inputs) =
        (* 불변조건 보존 *)
        let invariant_ok = check_invariant_preservation old_state in

        (* 하위 호환성 *)
        let compat_ok = List.for_all (M.is_backward_compatible old_state) old_inputs in

        if invariant_ok && compat_ok then
          { verdict = Pass "Migration safe";
            confidence = 1.0;
            context = ();
            children = [];
            metadata = [
              ("from_version", string_of_int M.Old.version);
              ("to_version", string_of_int M.New.version);
            ]; }
        else
          let reasons = [] in
          let reasons = if not invariant_ok then "invariant_broken" :: reasons else reasons in
          let reasons = if not compat_ok then "backward_incompat" :: reasons else reasons in
          { verdict = Fail (String.concat ", " reasons);
            confidence = 0.0;
            context = ();
            children = [];
            metadata = [("issues", String.concat "," reasons)]; }
    end)
end

(** {1 Property-Based Testing Support} *)

(** QuickCheck 스타일 속성 정의 *)
module type PROPERTY = sig
  type input
  type state

  val name : string
  val generate_input : unit -> input
  val generate_state : unit -> state
  val property : state -> input -> bool
end

(** 스펙에서 속성 자동 추출 *)
module Extract_properties (S : SPEC) = struct
  (** 사전조건 속성 *)
  module Precondition_property = struct
    type input = S.input
    type state = S.state
    let name = "precondition_holds"
    let generate_input () = Obj.magic ()  (* 실제론 생성기 필요 *)
    let generate_state () = Obj.magic ()
    let property state input = S.precondition state input
  end

  (** 불변조건 속성 *)
  module Invariant_property = struct
    type input = S.input
    type state = S.state
    let name = "invariant_preserved"
    let generate_input () = Obj.magic ()
    let generate_state () = Obj.magic ()
    let property state input =
      if not (S.invariant state) then true  (* 전제 조건 *)
      else
        let new_state = S.transition state input in
        S.invariant new_state
  end
end

(** {1 Refinement Types (Lightweight)} *)

(** 제약된 값 타입 *)
module type REFINED = sig
  type raw
  type t

  val refine : raw -> t option
  val unrefine : t -> raw
  val predicate : raw -> bool
end

(** 범위 제한 정수 *)
module Bounded_int (Config : sig val min : int val max : int end) : REFINED
  with type raw = int and type t = private int = struct
  type raw = int
  type t = int

  let predicate n = n >= Config.min && n <= Config.max
  let refine n = if predicate n then Some n else None
  let unrefine n = n
end

(** 비어있지 않은 문자열 *)
module Non_empty_string : REFINED
  with type raw = string and type t = private string = struct
  type raw = string
  type t = string

  let predicate s = String.length s > 0
  let refine s = if predicate s then Some s else None
  let unrefine s = s
end

(** {1 Contract Annotations (Documentation)} *)

(** 계약 문서화 *)
type contract_doc = {
  name : string;
  description : string;
  preconditions : string list;
  postconditions : string list;
  invariants : string list;
  examples : (string * string) list;  (** (input, expected_output) *)
}

(** 문서에서 테스트 생성 *)
let doc_to_test_cases doc =
  List.map (fun (inp, out) ->
    Printf.sprintf "test_%s: input=%s, expected=%s" doc.name inp out
  ) doc.examples

(** {1 SSOT Registry} *)

(** 전역 스펙 레지스트리 (런타임 검색용) *)
module Spec_registry = struct
  type spec_entry = {
    name : string;
    version : int;
    doc : contract_doc option;
  }

  let registry : (string, spec_entry) Hashtbl.t = Hashtbl.create 16

  let register ~name ~version ?doc () =
    Hashtbl.replace registry name { name; version; doc }

  let find name = Hashtbl.find_opt registry name

  let list_all () =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) registry []

  let find_by_version ~name ~version =
    match find name with
    | Some entry when entry.version = version -> Some entry
    | _ -> None
end

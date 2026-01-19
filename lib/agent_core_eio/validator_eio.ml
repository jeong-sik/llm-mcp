(** Composable Meta-Validator for Eio

    재귀적/계층적 검증 구조를 지원합니다.
    Validator의 Validator (메타 검증) 가능.

    특징:
    - Eio Fiber 기반 병렬 검증
    - 구조적 동시성 (Structured Concurrency)
    - 취소 가능한 검증 체인
    - 무한 중첩 가능한 Validator 구조
*)

(** {1 Core Types} *)

(** 검증 판정 *)
type verdict =
  | Pass of string          (** 통과, 이유 *)
  | Warn of string          (** 경고, 계속 진행 *)
  | Fail of string          (** 실패, 중단 *)
  | Defer of string         (** 판단 보류, 상위로 위임 *)

(** 검증 결과 *)
type 'ctx result = {
  verdict : verdict;
  confidence : float;           (** 0.0 - 1.0 *)
  context : 'ctx;               (** 검증 컨텍스트 전달 *)
  children : 'ctx result list;  (** 하위 검증 결과들 (메타 검증용) *)
  metadata : (string * string) list;
}

(** {1 Validator Signature} *)

(** 기본 Validator 시그니처 *)
module type VALIDATOR = sig
  type state
  type context

  val name : string
  val validate : state -> context result
end

(** {1 Composable Validators} *)

(** Validator 합성 연산자들 *)
module Compose = struct

  (** 순차 검증: A 통과 후 B 검증 *)
  let sequence
      (type s c)
      (module V1 : VALIDATOR with type state = s and type context = c)
      (module V2 : VALIDATOR with type state = s and type context = c)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "%s >> %s" V1.name V2.name

      let validate state =
        let r1 = V1.validate state in
        match r1.verdict with
        | Fail _ -> r1  (* 첫 번째 실패시 중단 *)
        | _ ->
          let r2 = V2.validate state in
          { r2 with
            children = [r1; r2];
            confidence = (r1.confidence +. r2.confidence) /. 2.0;
          }
    end)

  (** 병렬 검증: A와 B를 Fiber로 동시 실행 *)
  let parallel
      (type s c)
      ~sw
      (module V1 : VALIDATOR with type state = s and type context = c)
      (module V2 : VALIDATOR with type state = s and type context = c)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "%s || %s" V1.name V2.name

      let validate state =
        (* Eio Fiber로 병렬 실행 *)
        let r1_promise = Eio.Fiber.fork_promise ~sw (fun () -> V1.validate state) in
        let r2_promise = Eio.Fiber.fork_promise ~sw (fun () -> V2.validate state) in

        let r1 = Eio.Promise.await_exn r1_promise in
        let r2 = Eio.Promise.await_exn r2_promise in

        (* 결과 합성: 하나라도 Fail이면 Fail *)
        let verdict = match r1.verdict, r2.verdict with
          | Fail reason, _ -> Fail reason
          | _, Fail reason -> Fail reason
          | Warn w1, Warn w2 -> Warn (w1 ^ "; " ^ w2)
          | Warn w, _ | _, Warn w -> Warn w
          | Pass p1, Pass p2 -> Pass (p1 ^ " & " ^ p2)
          | Defer d, _ | _, Defer d -> Defer d
        in

        { verdict;
          confidence = Float.min r1.confidence r2.confidence;
          context = r1.context;  (* 첫 번째 컨텍스트 사용 *)
          children = [r1; r2];
          metadata = r1.metadata @ r2.metadata;
        }
    end)

  (** 조건부 검증: 조건 만족시에만 검증 *)
  let when_
      (type s c)
      ~condition
      (module V : VALIDATOR with type state = s and type context = c)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "when?(%s)" V.name

      let validate state =
        if condition state then
          V.validate state
        else
          { verdict = Pass "조건 미충족, 검증 스킵";
            confidence = 1.0;
            context = (V.validate state).context;  (* 기본 컨텍스트 *)
            children = [];
            metadata = [("skipped", "true")];
          }
    end)

  (** N개 중 K개 통과시 Pass *)
  let quorum
      (type s c)
      ~sw
      ~required
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "quorum(%d/%d)" required (List.length validators)

      let validate state =
        (* 모든 Validator 병렬 실행 *)
        let promises = List.map (fun (module V : VALIDATOR with type state = s and type context = c) ->
            Eio.Fiber.fork_promise ~sw (fun () -> V.validate state)
          ) validators
        in

        let results = List.map Eio.Promise.await_exn promises in

        (* Pass 개수 카운트 *)
        let pass_count = List.fold_left (fun acc r ->
            match r.verdict with
            | Pass _ -> acc + 1
            | Warn _ -> acc + 1  (* Warn도 통과로 취급 *)
            | _ -> acc
          ) 0 results
        in

        let verdict =
          if pass_count >= required then
            Pass (Printf.sprintf "%d/%d validators passed" pass_count (List.length validators))
          else
            Fail (Printf.sprintf "Only %d/%d passed (required: %d)"
                    pass_count (List.length validators) required)
        in

        let avg_confidence =
          let sum = List.fold_left (fun acc r -> acc +. r.confidence) 0.0 results in
          sum /. float_of_int (List.length results)
        in

        { verdict;
          confidence = avg_confidence;
          context = (List.hd results).context;
          children = results;
          metadata = [("pass_count", string_of_int pass_count)];
        }
    end)
end

(** {1 Meta-Validator} *)

(** Validator를 검증하는 Meta-Validator *)
module Meta = struct

  (** 하위 Validator의 결과를 검증 *)
  type meta_policy =
    | AllMustPass          (** 모든 하위 검증 통과 필요 *)
    | MajorityPass         (** 과반수 통과 *)
    | AnyPass              (** 하나라도 통과 *)
    | WeightedVote of (string * float) list  (** 가중치 투표 *)

  (** Meta-Validator 생성 *)
  let create
      (type s c)
      ~sw
      ~name:meta_name
      ~policy
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "Meta<%s>" meta_name

      let validate state =
        (* 모든 하위 Validator 병렬 실행 *)
        let promises = List.map (fun (module V : VALIDATOR with type state = s and type context = c) ->
            Eio.Fiber.fork_promise ~sw (fun () -> (V.name, V.validate state))
          ) validators
        in

        let named_results = List.map Eio.Promise.await_exn promises in
        let results = List.map snd named_results in

        (* 정책에 따른 최종 판정 *)
        let verdict = match policy with
          | AllMustPass ->
            let all_pass = List.for_all (fun r ->
                match r.verdict with Pass _ | Warn _ -> true | _ -> false
              ) results
            in
            if all_pass then Pass "All validators passed"
            else Fail "Not all validators passed"

          | MajorityPass ->
            let pass_count = List.fold_left (fun acc r ->
                match r.verdict with Pass _ | Warn _ -> acc + 1 | _ -> acc
              ) 0 results
            in
            if pass_count > List.length results / 2 then
              Pass (Printf.sprintf "Majority passed (%d/%d)" pass_count (List.length results))
            else
              Fail (Printf.sprintf "Majority failed (%d/%d)" pass_count (List.length results))

          | AnyPass ->
            let any_pass = List.exists (fun r ->
                match r.verdict with Pass _ -> true | _ -> false
              ) results
            in
            if any_pass then Pass "At least one validator passed"
            else Fail "No validator passed"

          | WeightedVote weights ->
            let score = List.fold_left2 (fun acc (name, _) r ->
                let weight = try List.assoc name weights with Not_found -> 1.0 in
                match r.verdict with
                | Pass _ -> acc +. weight
                | Warn _ -> acc +. (weight *. 0.5)
                | _ -> acc
              ) 0.0 named_results results
            in
            let max_score = List.fold_left (fun acc (_, w) -> acc +. w) 0.0 weights in
            if score >= max_score *. 0.5 then
              Pass (Printf.sprintf "Weighted score: %.2f/%.2f" score max_score)
            else
              Fail (Printf.sprintf "Weighted score too low: %.2f/%.2f" score max_score)
        in

        let avg_confidence =
          let sum = List.fold_left (fun acc r -> acc +. r.confidence) 0.0 results in
          sum /. float_of_int (List.length results)
        in

        { verdict;
          confidence = avg_confidence;
          context = (List.hd results).context;
          children = results;
          metadata = [
            ("policy", match policy with
              | AllMustPass -> "all_must_pass"
              | MajorityPass -> "majority"
              | AnyPass -> "any"
              | WeightedVote _ -> "weighted");
            ("validator_count", string_of_int (List.length validators));
          ];
        }
    end)

  (** 2단계 Meta-Validator: Validator의 Validator

      더 깊은 중첩이 필요하면 수동으로 조합:
      {[
        let level1 = Meta.create ~sw ~name:"L1" ~policy:AllMustPass validators in
        let level2 = Meta.create ~sw ~name:"L2" ~policy:MajorityPass [level1; other] in
        let level3 = Meta.create ~sw ~name:"L3" ~policy:AnyPass [level2] in
      ]}
  *)
  let nested
      (type s c)
      ~sw
      ~name:meta_name
      ~outer_policy
      ~inner_policy
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    let inner = create ~sw ~name:(meta_name ^ "_inner") ~policy:inner_policy validators in
    create ~sw ~name:meta_name ~policy:outer_policy [inner]
end

(** {1 Built-in Validators} *)

(** 진행률 기반 Validator Functor *)
module Make_progress_validator (S : sig
    type t
    val get_progress : t -> float
  end) = struct
  type state = S.t
  type context = unit

  let name = "progress"

  let validate state =
    let progress = S.get_progress state in
    let threshold = 0.8 in  (* 80% 기본 임계값 *)
    let verdict =
      if progress >= threshold then
        Pass (Printf.sprintf "Progress %.1f%% >= %.1f%%" (progress *. 100.0) (threshold *. 100.0))
      else if progress >= threshold *. 0.5 then
        Warn (Printf.sprintf "Progress %.1f%% (target: %.1f%%)" (progress *. 100.0) (threshold *. 100.0))
      else
        Fail (Printf.sprintf "Progress %.1f%% too low" (progress *. 100.0))
    in
    { verdict;
      confidence = progress;
      context = ();
      children = [];
      metadata = [("progress", Printf.sprintf "%.2f" progress)];
    }
end

(** 타임아웃 기반 Validator - 함수로 생성 *)
let make_timeout_validator ~get_now ~deadline =
  (module struct
    type state = unit
    type context = float

    let name = "timeout"

    let validate _state =
      let now = get_now () in
      let remaining = deadline -. now in
      let verdict =
        if remaining > 60.0 then
          Pass (Printf.sprintf "%.0f seconds remaining" remaining)
        else if remaining > 0.0 then
          Warn (Printf.sprintf "Only %.0f seconds left!" remaining)
        else
          Fail "Deadline exceeded"
      in
      { verdict;
        confidence = Float.max 0.0 (Float.min 1.0 (remaining /. 300.0));
        context = remaining;
        children = [];
        metadata = [("remaining_seconds", Printf.sprintf "%.0f" remaining)];
      }
  end : VALIDATOR with type state = unit and type context = float)

(** {1 Runner} *)

(** 검증과 함께 에이전트 루프 실행 *)
let run_with_validation
    (type s c)
    ~sw:_sw  (* Validators에서 사용됨 *)
    ~clock
    (module V : VALIDATOR with type state = s and type context = c)
    ~check_every
    ~on_result
    ~max_iterations
    ~iterate
    ~initial_state
  =
  let rec loop state iteration =
    if iteration >= max_iterations then
      `Max_iterations state
    else begin
      (* 검증 주기 확인 *)
      let () =
        if iteration > 0 && iteration mod check_every = 0 then begin
          let result = V.validate state in
          on_result iteration result;
          match result.verdict with
          | Fail reason -> raise (Failure reason)
          | _ -> ()
        end
      in

      (* 이터레이션 실행 *)
      let state' = iterate state iteration in

      (* 타임슬라이싱: 다른 Fiber에게 양보 *)
      Eio.Time.sleep clock 0.0;

      loop state' (iteration + 1)
    end
  in

  try loop initial_state 0
  with Failure reason -> `Validation_failed reason

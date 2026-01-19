(** Validator Presets - Common Validation Patterns

    자주 사용되는 검증 패턴들의 프리셋 모음.
    각 프리셋은 특정 사용 사례에 최적화되어 있습니다.

    사용 예:
    {[
      open Agent_core_eio.Validator_presets

      (* CI Pipeline *)
      let ci = Pipeline.create ~sw [
        (module Lint);
        (module Test);
        (module Build);
      ]

      (* Code Review *)
      let review = Quorum.create ~sw ~required:2 [
        (module Reviewer1);
        (module Reviewer2);
        (module Reviewer3);
      ]
    ]}
*)

open Validator_eio

(** Logger source for validator presets *)
let src = Logs.Src.create "validator.presets" ~doc:"Validator preset patterns"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Pipeline Pattern}
    순차 실행, 하나라도 실패하면 중단 *)
module Pipeline = struct
  let create
      (type s c)
      ~sw:_sw
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    match validators with
    | [] -> failwith "Pipeline requires at least one validator"
    | [v] -> v
    | first :: rest ->
      List.fold_left (fun acc v -> Compose.sequence acc v) first rest

  (** 이름 있는 파이프라인 *)
  let named
      (type s c)
      ~sw
      ~name:pipeline_name
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    let pipeline = create ~sw validators in
    (module struct
      type state = s
      type context = c

      let name = pipeline_name

      let validate state =
        let module P = (val pipeline) in
        let result = P.validate state in
        { result with
          metadata = ("pattern", "pipeline") :: result.metadata }
    end)
end

(** {1 Fanout Pattern}
    병렬 실행, 모든 결과 수집 *)
module Fanout = struct
  let create
      (type s c)
      ~sw
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    match validators with
    | [] -> failwith "Fanout requires at least one validator"
    | [v] -> v
    | first :: second :: rest ->
      let initial = Compose.parallel ~sw first second in
      List.fold_left (fun acc v -> Compose.parallel ~sw acc v) initial rest

  (** 결과 집계 정책 선택 가능 *)
  let with_policy
      (type s c)
      ~sw
      ~policy
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    Meta.create ~sw ~name:"fanout" ~policy validators
end

(** {1 Diamond Pattern}
    공통 전처리 후 병렬 분기, 결과 합류 *)
module Diamond = struct
  let create
      (type s c)
      ~sw
      ~(preprocess : (module VALIDATOR with type state = s and type context = c))
      ~(branches : (module VALIDATOR with type state = s and type context = c) list)
      ~(postprocess : (module VALIDATOR with type state = s and type context = c) option)
    : (module VALIDATOR with type state = s and type context = c) =
    let parallel_branches = Fanout.create ~sw branches in
    let after_pre = Compose.sequence preprocess parallel_branches in
    match postprocess with
    | None -> after_pre
    | Some post -> Compose.sequence after_pre post

  (** 간단한 다이아몬드: pre >> (a || b) >> post *)
  let simple
      (type s c)
      ~sw
      ~(pre : (module VALIDATOR with type state = s and type context = c))
      ~(branch_a : (module VALIDATOR with type state = s and type context = c))
      ~(branch_b : (module VALIDATOR with type state = s and type context = c))
      ~(post : (module VALIDATOR with type state = s and type context = c))
    : (module VALIDATOR with type state = s and type context = c) =
    create ~sw ~preprocess:pre ~branches:[branch_a; branch_b] ~postprocess:(Some post)
end

(** {1 Quorum Pattern}
    N개 중 K개 통과 필요 (합의 기반) *)
module Quorum = struct
  let create = Compose.quorum

  (** 과반수 필요 *)
  let majority
      (type s c)
      ~sw
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    let required = (List.length validators / 2) + 1 in
    Compose.quorum ~sw ~required validators

  (** 전원 동의 필요 *)
  let unanimous
      (type s c)
      ~sw
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    Compose.quorum ~sw ~required:(List.length validators) validators

  (** 최소 1개 통과 *)
  let any
      (type s c)
      ~sw
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    Compose.quorum ~sw ~required:1 validators
end

(** {1 Gate Pattern}
    조건부 실행, 조건 불충족 시 스킵 *)
module Gate = struct
  let create = Compose.when_

  (** 상태 기반 게이트 *)
  let on_state
      (type s c)
      ~condition
      (validator : (module VALIDATOR with type state = s and type context = c))
    : (module VALIDATOR with type state = s and type context = c) =
    Compose.when_ ~condition validator

  (** Feature flag 기반 게이트 *)
  let feature_flag
      (type s c)
      ~flag_name
      ~is_enabled
      (validator : (module VALIDATOR with type state = s and type context = c))
    : (module VALIDATOR with type state = s and type context = c) =
    let module V = (val validator) in
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "flag[%s](%s)" flag_name V.name

      let validate state =
        if is_enabled () then
          V.validate state
        else
          { verdict = Pass (Printf.sprintf "Feature '%s' disabled, skipped" flag_name);
            confidence = 1.0;
            context = (V.validate state).context;
            children = [];
            metadata = [("skipped", "true"); ("flag", flag_name)]; }
    end)
end

(** {1 Retry Pattern}
    실패 시 재시도 *)
module Retry = struct
  type retry_config = {
    max_attempts : int;
    delay_ms : int;
    backoff_multiplier : float;
  }

  let default_config = {
    max_attempts = 3;
    delay_ms = 100;
    backoff_multiplier = 2.0;
  }

  let create
      (type s c)
      ~clock
      ?(config = default_config)
      (validator : (module VALIDATOR with type state = s and type context = c))
    : (module VALIDATOR with type state = s and type context = c) =
    let module V = (val validator) in
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "retry(%d)(%s)" config.max_attempts V.name

      let validate state =
        let rec attempt n delay =
          let result = V.validate state in
          match result.verdict with
          | Pass _ | Warn _ -> result
          | Fail _reason when n < config.max_attempts ->
            Eio.Time.sleep clock (float_of_int delay /. 1000.0);
            let next_delay = int_of_float (float_of_int delay *. config.backoff_multiplier) in
            attempt (n + 1) next_delay
          | Fail _ | Defer _ ->
            { result with
              metadata = ("attempts", string_of_int n) :: result.metadata }
        in
        attempt 1 config.delay_ms
    end)
end

(** {1 Timeout Pattern}
    시간 제한 검증 *)
module Timeout = struct
  let create
      (type s c)
      ~sw
      ~clock
      ~timeout_sec
      (validator : (module VALIDATOR with type state = s and type context = c))
    : (module VALIDATOR with type state = s and type context = c) =
    let module V = (val validator) in
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "timeout(%.1fs)(%s)" timeout_sec V.name

      let validate state =
        let result_ref = ref None in
        let timed_out = ref false in

        Eio.Fiber.both
          (fun () ->
            Eio.Time.sleep clock timeout_sec;
            timed_out := true)
          (fun () ->
            let _ = Eio.Fiber.fork_promise ~sw (fun () ->
              result_ref := Some (V.validate state)
            ) in
            ());

        match !result_ref with
        | Some r when not !timed_out -> r
        | _ ->
          { verdict = Fail (Printf.sprintf "Timeout after %.1fs" timeout_sec);
            confidence = 0.0;
            context = (Obj.magic () : c);  (* 타임아웃 시 컨텍스트 없음 *)
            children = [];
            metadata = [("timeout", Printf.sprintf "%.1f" timeout_sec)]; }
    end)

  (** 간단한 데드라인 기반 검증 *)
  let with_deadline = make_timeout_validator
end

(** {1 Circuit Breaker Pattern}
    연속 실패 시 빠른 실패 *)
module Circuit_breaker = struct
  type state = {
    mutable failure_count : int;
    mutable last_failure : float;
    mutable is_open : bool;
  }

  type config = {
    failure_threshold : int;
    reset_timeout_sec : float;
  }

  let default_config = {
    failure_threshold = 5;
    reset_timeout_sec = 30.0;
  }

  let create_state () = {
    failure_count = 0;
    last_failure = 0.0;
    is_open = false;
  }

  let create
      (type s c)
      ~clock
      ?(config = default_config)
      ~breaker_state
      (validator : (module VALIDATOR with type state = s and type context = c))
    : (module VALIDATOR with type state = s and type context = c) =
    let module V = (val validator) in
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "circuit_breaker(%s)" V.name

      let validate state =
        let now = Eio.Time.now clock in

        (* 회로가 열려있고 리셋 시간이 지났으면 half-open *)
        if breaker_state.is_open &&
           now -. breaker_state.last_failure > config.reset_timeout_sec then begin
          Log.info (fun m -> m "[%s] Circuit HALF-OPEN after %.1fs timeout" name config.reset_timeout_sec);
          breaker_state.is_open <- false;
          breaker_state.failure_count <- 0;
        end;

        (* 회로가 열려있으면 빠른 실패 *)
        if breaker_state.is_open then begin
          Log.warn (fun m -> m "[%s] Circuit OPEN - fast failing" name);
          { verdict = Fail "Circuit breaker open";
            confidence = 0.0;
            context = (Obj.magic () : c);
            children = [];
            metadata = [("circuit", "open")]; }
        end
        else
          let result = V.validate state in
          (match result.verdict with
           | Fail _ ->
             breaker_state.failure_count <- breaker_state.failure_count + 1;
             breaker_state.last_failure <- now;
             if breaker_state.failure_count >= config.failure_threshold then begin
               Log.err (fun m -> m "[%s] Circuit OPENED after %d consecutive failures"
                 name breaker_state.failure_count);
               breaker_state.is_open <- true
             end else
               Log.debug (fun m -> m "[%s] Failure %d/%d"
                 name breaker_state.failure_count config.failure_threshold)
           | Pass _ | Warn _ ->
             if breaker_state.failure_count > 0 then
               Log.debug (fun m -> m "[%s] Reset failure count (was %d)" name breaker_state.failure_count);
             breaker_state.failure_count <- 0
           | Defer _ -> ());
          { result with
            metadata = ("failure_count",
              string_of_int breaker_state.failure_count) :: result.metadata }
    end)
end

(** {1 Layered Meta Pattern}
    계층적 검증 그룹 *)
module Layered = struct
  type 'a layer = {
    name : string;
    policy : Meta.meta_policy;
    validators : 'a list;
  }

  let create
      (type s c)
      ~sw
      ~outer_policy
      (layers : (module VALIDATOR with type state = s and type context = c) layer list)
    : (module VALIDATOR with type state = s and type context = c) =
    let layer_validators = List.map (fun layer ->
        Meta.create ~sw ~name:layer.name ~policy:layer.policy layer.validators
      ) layers
    in
    Meta.create ~sw ~name:"layered" ~policy:outer_policy layer_validators

  (** 2계층 단축 *)
  let two_layer
      (type s c)
      ~sw
      ~layer1_name ~layer1_policy
      ~layer2_name ~layer2_policy
      ~outer_policy
      (layer1 : (module VALIDATOR with type state = s and type context = c) list)
      (layer2 : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    create ~sw ~outer_policy [
      { name = layer1_name; policy = layer1_policy; validators = layer1 };
      { name = layer2_name; policy = layer2_policy; validators = layer2 };
    ]
end

(** {1 Weighted Vote Pattern}
    가중치 기반 투표 *)
module Weighted_vote = struct
  type 'a weighted = {
    weight : float;
    validator : 'a;
  }

  let create
      (type s c)
      ~sw
      ~threshold
      (weighted_validators : (module VALIDATOR with type state = s and type context = c) weighted list)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "weighted(threshold=%.2f)" threshold

      let validate state =
        (* 모든 Validator 병렬 실행 *)
        let promises = List.map (fun wv ->
            let module V = (val wv.validator : VALIDATOR with type state = s and type context = c) in
            Eio.Fiber.fork_promise ~sw (fun () ->
              (V.name, wv.weight, V.validate state))
          ) weighted_validators
        in

        let results = List.map Eio.Promise.await_exn promises in

        (* 가중치 점수 계산 *)
        let total_weight = List.fold_left (fun acc wv -> acc +. wv.weight) 0.0 weighted_validators in
        let score = List.fold_left (fun acc (_, weight, result) ->
            match result.verdict with
            | Pass _ -> acc +. weight
            | Warn _ -> acc +. (weight *. 0.5)
            | _ -> acc
          ) 0.0 results
        in

        let ratio = score /. total_weight in
        let verdict =
          if ratio >= threshold then
            Pass (Printf.sprintf "Weighted score: %.2f/%.2f (%.1f%%)" score total_weight (ratio *. 100.0))
          else
            Fail (Printf.sprintf "Weighted score too low: %.2f/%.2f (%.1f%% < %.1f%%)"
                    score total_weight (ratio *. 100.0) (threshold *. 100.0))
        in

        let children = List.map (fun (_, _, r) -> r) results in
        let avg_confidence =
          let sum = List.fold_left (fun acc (_, _, r) -> acc +. r.confidence) 0.0 results in
          sum /. float_of_int (List.length results)
        in

        { verdict;
          confidence = avg_confidence;
          context = (List.hd children).context;
          children;
          metadata = [
            ("weighted_score", Printf.sprintf "%.2f" score);
            ("total_weight", Printf.sprintf "%.2f" total_weight);
            ("threshold", Printf.sprintf "%.2f" threshold);
          ]; }
    end)

  (** 전문가 리뷰용 단축 *)
  let expert_review
      (type s c)
      ~sw
      ~senior_weight ~mid_weight ~junior_weight
      (senior : (module VALIDATOR with type state = s and type context = c))
      (mid : (module VALIDATOR with type state = s and type context = c))
      (junior : (module VALIDATOR with type state = s and type context = c))
    : (module VALIDATOR with type state = s and type context = c) =
    create ~sw ~threshold:0.5 [
      { weight = senior_weight; validator = senior };
      { weight = mid_weight; validator = mid };
      { weight = junior_weight; validator = junior };
    ]
end

(** {1 Saga Pattern}
    장기 실행 트랜잭션, 실패 시 보상 *)
module Saga = struct
  type ('s, 'c) step = {
    name : string;
    validator : (module VALIDATOR with type state = 's and type context = 'c);
    compensate : 's -> unit;  (** 롤백 액션 *)
  }

  let create
      (type s c)
      ~sw:_sw
      (steps : (s, c) step list)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = "saga[" ^ String.concat " -> " (List.map (fun s -> s.name) steps) ^ "]"

      let validate state =
        Log.info (fun m -> m "[%s] Starting saga with %d steps" name (List.length steps));
        let completed = ref [] in
        let rec run_steps remaining =
          match remaining with
          | [] ->
            Log.info (fun m -> m "[%s] Saga completed successfully (%d steps)" name (List.length steps));
            { verdict = Pass "All saga steps completed";
              confidence = 1.0;
              context = (Obj.magic () : c);
              children = [];
              metadata = [("completed_steps", string_of_int (List.length steps))]; }
          | step :: rest ->
            let module V = (val step.validator : VALIDATOR with type state = s and type context = c) in
            Log.debug (fun m -> m "[%s] Executing step '%s'" name step.name);
            let result = V.validate state in
            match result.verdict with
            | Pass _ | Warn _ ->
              completed := step :: !completed;
              run_steps rest
            | Fail reason ->
              (* 보상 트랜잭션 실행 *)
              let comp_count = List.length !completed in
              Log.err (fun m -> m "[%s] Step '%s' failed: %s. Compensating %d steps..."
                name step.name reason comp_count);
              List.iter (fun s ->
                Log.debug (fun m -> m "[%s] Compensating step '%s'" name s.name);
                s.compensate state
              ) !completed;
              Log.warn (fun m -> m "[%s] Compensation complete (%d steps rolled back)" name comp_count);
              { verdict = Fail (Printf.sprintf "Saga failed at '%s': %s (compensated %d steps)"
                                  step.name reason comp_count);
                confidence = 0.0;
                context = result.context;
                children = [result];
                metadata = [
                  ("failed_step", step.name);
                  ("compensated", string_of_int comp_count);
                ]; }
            | Defer reason ->
              Log.debug (fun m -> m "[%s] Step '%s' deferred: %s" name step.name reason);
              { verdict = Defer (Printf.sprintf "Saga deferred at '%s': %s" step.name reason);
                confidence = result.confidence;
                context = result.context;
                children = [result];
                metadata = [("deferred_step", step.name)]; }
        in
        run_steps steps
    end)
end

(** {1 Checkpoint Pattern}
    진행 상황 저장 및 복구 *)
module Checkpoint = struct
  type ('s, 'c) checkpoint_fn = {
    save : 's -> int -> unit;
    load : unit -> (int * 's) option;
  }

  let create
      (type s c)
      ~sw:_sw
      ~checkpoint
      (validators : (module VALIDATOR with type state = s and type context = c) list)
    : (module VALIDATOR with type state = s and type context = c) =
    (module struct
      type state = s
      type context = c

      let name = Printf.sprintf "checkpoint(%d steps)" (List.length validators)

      let validate state =
        (* 이전 체크포인트에서 복구 *)
        let start_idx, current_state =
          match checkpoint.load () with
          | Some (idx, s) when idx < List.length validators ->
            Log.info (fun m -> m "[%s] Resuming from checkpoint %d/%d"
              name idx (List.length validators));
            (idx, s)
          | _ ->
            Log.debug (fun m -> m "[%s] Starting fresh (no checkpoint)" name);
            (0, state)
        in

        let validators_array = Array.of_list validators in
        let results = ref [] in
        let final_state = ref current_state in

        let rec run idx =
          if idx >= Array.length validators_array then begin
            Log.info (fun m -> m "[%s] All %d checkpoints passed" name (List.length validators));
            { verdict = Pass "All checkpoints passed";
              confidence = 1.0;
              context = (Obj.magic () : c);
              children = List.rev !results;
              metadata = [("checkpoints", string_of_int (List.length validators))]; }
          end
          else begin
            let module V = (val validators_array.(idx)) in
            Log.debug (fun m -> m "[%s] Running checkpoint %d/%d" name idx (List.length validators));
            let result = V.validate !final_state in
            results := result :: !results;

            match result.verdict with
            | Pass _ | Warn _ ->
              Log.debug (fun m -> m "[%s] Checkpoint %d saved" name (idx + 1));
              checkpoint.save !final_state (idx + 1);
              run (idx + 1)
            | Fail reason ->
              Log.err (fun m -> m "[%s] Failed at checkpoint %d: %s" name idx reason);
              { verdict = Fail (Printf.sprintf "Failed at checkpoint %d: %s" idx reason);
                confidence = 0.0;
                context = result.context;
                children = List.rev !results;
                metadata = [("failed_checkpoint", string_of_int idx)]; }
            | Defer reason ->
              { verdict = Defer (Printf.sprintf "Deferred at checkpoint %d: %s" idx reason);
                confidence = result.confidence;
                context = result.context;
                children = List.rev !results;
                metadata = [("deferred_checkpoint", string_of_int idx)]; }
          end
        in
        run start_idx
    end)
end

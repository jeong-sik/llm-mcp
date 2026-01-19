(** Test Orchestration - Real Multi-Agent Scenario

    Demonstrates the full orchestration pattern:
    - 실행: Agent Loop with Eio Fibers
    - 감시: Validator hierarchy (SPEC, Metric, Goal, Temporal)
    - 개입: AI/Human Intervention on stall
*)

open Agent_core_eio.Validator
open Agent_core_eio.Validation_stack

(** Simulated task state *)
type task_state = {
  score: float;
  attempts: int;
  strategy: [`Fast | `Accurate | `Hybrid] [@warning "-69"];
  history: float list;
}

let initial_state = {
  score = 0.0;
  attempts = 0;
  strategy = `Fast;
  history = [];
}

(** Agent strategies as Validators *)
module Fast_strategy : VALIDATOR with type state = task_state and type context = task_state = struct
  type state = task_state
  type context = task_state

  let name = "fast_strategy"

  let validate state =
    (* Fast strategy: +15 points per attempt, 70% success rate *)
    let success = Random.float 1.0 < 0.7 in
    let delta = if success then 15.0 else 5.0 in
    let new_score = Float.min 100.0 (state.score +. delta) in
    let new_state = {
      state with
      score = new_score;
      attempts = state.attempts + 1;
      history = new_score :: state.history;
    } in
    if new_score >= 90.0 then
      { verdict = Pass (Printf.sprintf "Target reached: %.1f" new_score);
        confidence = 1.0;
        context = new_state;
        children = [];
        metadata = [("strategy", "fast"); ("delta", Printf.sprintf "%.1f" delta)]; }
    else
      { verdict = Warn (Printf.sprintf "Progress: %.1f" new_score);
        confidence = new_score /. 100.0;
        context = new_state;
        children = [];
        metadata = [("strategy", "fast")]; }
end

module Accurate_strategy : VALIDATOR with type state = task_state and type context = task_state = struct
  type state = task_state
  type context = task_state

  let name = "accurate_strategy"

  let validate state =
    (* Accurate strategy: +8 points per attempt, 95% success rate *)
    let success = Random.float 1.0 < 0.95 in
    let delta = if success then 8.0 else 2.0 in
    let new_score = Float.min 100.0 (state.score +. delta) in
    let new_state = {
      state with
      score = new_score;
      attempts = state.attempts + 1;
      history = new_score :: state.history;
    } in
    if new_score >= 90.0 then
      { verdict = Pass (Printf.sprintf "Target reached: %.1f" new_score);
        confidence = 1.0;
        context = new_state;
        children = [];
        metadata = [("strategy", "accurate"); ("delta", Printf.sprintf "%.1f" delta)]; }
    else
      { verdict = Warn (Printf.sprintf "Progress: %.1f" new_score);
        confidence = new_score /. 100.0;
        context = new_state;
        children = [];
        metadata = [("strategy", "accurate")]; }
end

let () = Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
    Random.init 42;  (* Reproducible results *)

    (* ===== Test 1: Dynamic Strategy Selection with bind ===== *)
    let () =
      Printf.printf "\n=== Test 1: Dynamic Strategy Selection (bind) ===\n%!";

      (* Initial assessment validator *)
      let module Assess : VALIDATOR with type state = task_state and type context = task_state = struct
        type state = task_state
        type context = task_state
        let name = "assess"
        let validate state =
          { verdict = Pass "assessed";
            confidence = 1.0;
            context = state;
            children = [];
            metadata = [("current_score", Printf.sprintf "%.1f" state.score)]; }
      end in

      (* Dynamic strategy selector based on current progress *)
      let select_strategy (r : task_state result) =
        let state = r.context in
        if state.score < 50.0 then begin
          Printf.printf "  → Score %.1f < 50: Using FAST strategy\n%!" state.score;
          (module Fast_strategy : VALIDATOR with type state = task_state and type context = task_state)
        end else begin
          Printf.printf "  → Score %.1f >= 50: Using ACCURATE strategy\n%!" state.score;
          (module Accurate_strategy : VALIDATOR with type state = task_state and type context = task_state)
        end
      in

      (* Compose with bind for dynamic selection *)
      let module Dynamic = (val Compose.bind (module Assess) select_strategy) in

      (* Run multiple iterations *)
      let rec run state iterations =
        if iterations <= 0 || state.score >= 90.0 then state
        else begin
          let r = Dynamic.validate state in
          Printf.printf "  Iteration %d: score=%.1f, verdict=%s\n%!"
            (initial_state.attempts + iterations - state.attempts)
            r.context.score
            (match r.verdict with Pass s -> "Pass:" ^ s | Warn s -> "Warn:" ^ s | _ -> "Other");
          run r.context (iterations - 1)
        end
      in

      let final = run initial_state 15 in
      Printf.printf "  Final: score=%.1f, attempts=%d\n%!" final.score final.attempts;
      assert (final.attempts > 0);
      Printf.printf "[OK] Dynamic strategy selection\n%!"
    in

    (* ===== Test 2: Parallel Strategy Race ===== *)
    let () =
      Printf.printf "\n=== Test 2: Parallel Strategy Race ===\n%!";

      (* Race fast vs accurate - first to reach 90 wins *)
      let module Race = (val Compose.parallel ~sw
        (module Fast_strategy)
        (module Accurate_strategy)) in

      let state = { initial_state with score = 80.0 } in
      let r = Race.validate state in

      Printf.printf "  Race result: %s\n%!"
        (match r.verdict with
         | Pass s -> "Pass - " ^ s
         | Warn s -> "Warn - " ^ s
         | Fail s -> "Fail - " ^ s
         | Defer s -> "Defer - " ^ s);
      Printf.printf "  Children count: %d\n%!" (List.length r.children);

      assert (List.length r.children = 2);
      Printf.printf "[OK] Parallel strategy race\n%!"
    in

    (* ===== Test 3: Quorum-based Consensus ===== *)
    let () =
      Printf.printf "\n=== Test 3: Quorum Consensus (2/3 must pass) ===\n%!";

      let module V1 : VALIDATOR with type state = task_state and type context = task_state = struct
        type state = task_state
        type context = task_state
        let name = "validator_1"
        let validate state =
          { verdict = Pass "v1 approved";
            confidence = 0.9;
            context = { state with score = 95.0 };
            children = []; metadata = []; }
      end in

      let module V2 : VALIDATOR with type state = task_state and type context = task_state = struct
        type state = task_state
        type context = task_state
        let name = "validator_2"
        let validate state =
          { verdict = Fail "v2 rejected";
            confidence = 0.1;
            context = state;
            children = []; metadata = []; }
      end in

      let module V3 : VALIDATOR with type state = task_state and type context = task_state = struct
        type state = task_state
        type context = task_state
        let name = "validator_3"
        let validate state =
          { verdict = Pass "v3 approved";
            confidence = 0.8;
            context = { state with score = 92.0 };
            children = []; metadata = []; }
      end in

      let validators = [
        (module V1 : VALIDATOR with type state = task_state and type context = task_state);
        (module V2);
        (module V3);
      ] in

      let module Quorum = (val Compose.quorum ~sw ~required:2 validators) in
      let r = Quorum.validate initial_state in

      Printf.printf "  Quorum result: %s\n%!"
        (match r.verdict with
         | Pass s -> "PASS - " ^ s
         | Fail s -> "FAIL - " ^ s
         | _ -> "OTHER");

      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Quorum consensus\n%!"
    in

    (* ===== Test 4: Meta-Validator Hierarchy ===== *)
    let () =
      Printf.printf "\n=== Test 4: Meta-Validator (Validator of Validators) ===\n%!";

      let module Strategy_validator : VALIDATOR with type state = task_state and type context = task_state = struct
        type state = task_state
        type context = task_state
        let name = "strategy_check"
        let validate state =
          let passed = state.score >= 50.0 in
          { verdict = if passed then Pass "strategy ok" else Warn "strategy needs work";
            confidence = state.score /. 100.0;
            context = state;
            children = []; metadata = []; }
      end in

      let module Progress_validator : VALIDATOR with type state = task_state and type context = task_state = struct
        type state = task_state
        type context = task_state
        let name = "progress_check"
        let validate state =
          let passed = state.attempts < 20 in
          { verdict = if passed then Pass "within budget" else Fail "too many attempts";
            confidence = if passed then 0.9 else 0.1;
            context = state;
            children = []; metadata = []; }
      end in

      let module Quality_validator : VALIDATOR with type state = task_state and type context = task_state = struct
        type state = task_state
        type context = task_state
        let name = "quality_check"
        let validate state =
          let passed = state.score >= 90.0 in
          { verdict = if passed then Pass "quality achieved" else Warn "quality improving";
            confidence = state.score /. 100.0;
            context = state;
            children = []; metadata = []; }
      end in

      let validators = [
        (module Strategy_validator : VALIDATOR with type state = task_state and type context = task_state);
        (module Progress_validator);
        (module Quality_validator);
      ] in

      (* Meta-validator: majority must pass *)
      let module Meta = (val Meta.create ~sw ~name:"task_meta" ~policy:Meta.MajorityPass validators) in

      let state = { initial_state with score = 75.0; attempts = 5 } in
      let r = Meta.validate state in

      Printf.printf "  Meta result: %s\n%!"
        (match r.verdict with
         | Pass s -> "PASS - " ^ s
         | Fail s -> "FAIL - " ^ s
         | _ -> "OTHER");
      Printf.printf "  Children results: %d validators checked\n%!" (List.length r.children);

      assert (List.length r.children = 3);
      Printf.printf "[OK] Meta-validator hierarchy\n%!"
    in

    (* ===== Test 5: Intervenor Integration ===== *)
    let () =
      Printf.printf "\n=== Test 5: AI Intervenor Integration ===\n%!";

      let module AI = Make_ai_intervenor(struct
        let stall_threshold_sec = 60.0
        let max_failures = 3
      end) in

      (* Scenario 1: Good progress - no intervention *)
      let obs1 = AI.observe (0.7, 30.0, 0) in
      let trigger1 = AI.should_intervene obs1 in
      assert (trigger1 = None);
      Printf.printf "  Good progress (70%%): No intervention needed ✓\n%!";

      (* Scenario 2: Stalled - intervention needed *)
      let obs2 = AI.observe (0.2, 120.0, 0) in
      let trigger2 = AI.should_intervene obs2 in
      (match trigger2 with
       | Some (Stalled { progress; duration_sec }) ->
         Printf.printf "  Stalled (%.0f%% for %.0fs): Intervention triggered ✓\n%!" (progress *. 100.0) duration_sec;
         let decision = AI.decide obs2 (Stalled { progress; duration_sec }) in
         Printf.printf "  Decision: %s\n%!"
           (match decision with
            | Redirect _ -> "REDIRECT to alternative"
            | Redefine _ -> "REDEFINE problem"
            | Continue -> "CONTINUE"
            | _ -> "OTHER")
       | _ -> assert false);

      (* Scenario 3: Repeated failures *)
      let obs3 = AI.observe (0.5, 10.0, 5) in
      let trigger3 = AI.should_intervene obs3 in
      (match trigger3 with
       | Some (RepeatedFailure { attempts; _ }) ->
         Printf.printf "  Repeated failures (%d): Intervention triggered ✓\n%!" attempts
       | _ -> assert false);

      Printf.printf "[OK] AI Intervenor integration\n%!"
    in

    (* ===== Test 6: Full Orchestration Flow ===== *)
    let () =
      Printf.printf "\n=== Test 6: Full Orchestration Flow ===\n%!";
      Printf.printf "  (실행 + 감시 + 개입 통합)\n%!";

      (* Combined orchestration: run_with_validation *)
      let module Progress_validator = Make_progress_validator(struct
        type t = task_state
        let get_progress state = state.score /. 100.0
      end) in

      let iterations_run = ref 0 in

      let result = run_with_validation
        ~sw
        ~clock:(Eio.Stdenv.clock _env)
        (module Progress_validator)
        ~check_every:3  (* Check every 3 iterations *)
        ~on_result:(fun iter r ->
          Printf.printf "  [Check @%d] Progress=%.1f%%, Verdict=%s\n%!"
            iter (r.confidence *. 100.0)
            (match r.verdict with Pass s -> s | Warn s -> s | Fail s -> s | Defer s -> s))
        ~max_iterations:20
        ~iterate:(fun state iter ->
          iterations_run := iter + 1;
          let delta = 8.0 +. Random.float 7.0 in
          { state with
            score = Float.min 100.0 (state.score +. delta);
            attempts = state.attempts + 1;
            history = state.score :: state.history; })
        ~initial_state
      in

      (match result with
       | `Max_iterations final ->
         Printf.printf "  Completed %d iterations, final score: %.1f\n%!" !iterations_run final.score
       | `Validation_failed reason ->
         Printf.printf "  Validation failed: %s\n%!" reason);

      Printf.printf "[OK] Full orchestration flow\n%!"
    in

    (* ===== Test 7: Temporal Property Check ===== *)
    let () =
      Printf.printf "\n=== Test 7: Temporal Property (Eventually reaches goal) ===\n%!";

      let module Float_temporal = Make_temporal(struct type t = float end) in

      (* Simulate score history *)
      let history = [10.0; 25.0; 40.0; 55.0; 70.0; 85.0; 92.0] in

      (* Eventually reaches 90+ *)
      let eventually_goal = Float_temporal.eventually (fun s -> s >= 90.0) history in
      assert eventually_goal;
      Printf.printf "  Eventually reaches 90+: %b ✓\n%!" eventually_goal;

      (* Always positive *)
      let always_positive = Float_temporal.always (fun s -> s > 0.0) history in
      assert always_positive;
      Printf.printf "  Always positive: %b ✓\n%!" always_positive;

      (* Eventually within 5 steps *)
      let within_bound = Float_temporal.eventually_within 10 (fun s -> s >= 90.0) history in
      assert within_bound;
      Printf.printf "  Eventually within 10 steps: %b ✓\n%!" within_bound;

      Printf.printf "[OK] Temporal property checks\n%!"
    in

    Printf.printf "\n✅ All 7 orchestration tests passed!\n%!"

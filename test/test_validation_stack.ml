(** Test Validation Stack - Multi-Level Validation Hierarchy *)

open Agent_core_eio.Validation_stack
module Spec_dsl = Agent_core_eio.Spec_dsl

let () = Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun _sw ->

    (* ===== Test 1: Level 1 - Float Metric ===== *)
    let () =
      let module Quality_metric = Make_float_metric(struct
        let name = "quality"
        let min_value = 0.0
        let max_value = 100.0
      end) in

      let m = Quality_metric.measure () 85.0 in
      assert (Quality_metric.to_float m = 0.85);
      assert (Quality_metric.compare m 70.0 = `Better);
      assert (Quality_metric.compare m 90.0 = `Worse);
      assert (String.length (Quality_metric.describe m) > 0);

      Printf.printf "[OK] Float metric\n%!"
    in

    (* ===== Test 2: Level 1 - Coverage Metric ===== *)
    let () =
      let m = Coverage_metric.measure 100 75 in
      assert (m.Metrics.covered = 75);
      assert (m.total = 100);
      assert (abs_float (m.percentage -. 75.0) < 0.001);
      assert (abs_float (Coverage_metric.to_float m -. 0.75) < 0.001);

      let m2 = Coverage_metric.measure 100 80 in
      assert (Coverage_metric.compare m2 m = `Better);

      Printf.printf "[OK] Coverage metric\n%!"
    in

    (* ===== Test 3: Level 1 - SSIM Metric ===== *)
    let () =
      let m = SSIM_metric.measure "ref.png" "out.png" in
      assert (m.Metrics.method_name = "SSIM");
      assert (m.score >= 0.0 && m.score <= 1.0);
      assert (SSIM_metric.to_float m = m.score);

      Printf.printf "[OK] SSIM metric\n%!"
    in

    (* ===== Test 4: Level 2 - Threshold Goal ===== *)
    let () =
      let module Coverage_goal = Make_threshold_goal(Coverage_metric)(struct
        let threshold = 0.80  (* 80% coverage target *)
        let direction = `Higher_is_better
      end) in

      (* Below threshold *)
      let m1 = Coverage_metric.measure 100 70 in
      assert (not (Coverage_goal.achieved m1 Coverage_goal.target));
      assert (Coverage_goal.distance m1 Coverage_goal.target > 0.0);
      assert (Coverage_goal.progress m1 Coverage_goal.target < 1.0);

      (* Above threshold *)
      let m2 = Coverage_metric.measure 100 85 in
      assert (Coverage_goal.achieved m2 Coverage_goal.target);
      assert (Coverage_goal.distance m2 Coverage_goal.target = 0.0);
      assert (Coverage_goal.progress m2 Coverage_goal.target >= 1.0);

      Printf.printf "[OK] Threshold goal\n%!"
    in

    (* ===== Test 5: Level 2 - Composite Goal ===== *)
    let () =
      let sub_goals = [
        { Composite_goal.name = "coverage"; weight = 2.0;
          check = fun n ->
            let passed = n >= 80 in
            { achieved = passed; distance = if passed then 0.0 else 0.1;
              progress = float_of_int n /. 100.0; description = "Coverage check" } };
        { name = "complexity"; weight = 1.0;
          check = fun n ->
            let passed = n >= 50 in
            { achieved = passed; distance = if passed then 0.0 else 0.5;
              progress = float_of_int n /. 100.0; description = "Complexity check" } };
      ] in

      let composite = Composite_goal.create ~policy:`All sub_goals in
      let result = Composite_goal.evaluate composite 85 in
      assert result.achieved;

      let composite_any = Composite_goal.create ~policy:`Any sub_goals in
      let result_any = Composite_goal.evaluate composite_any 60 in
      (* 60 fails coverage (need 80) but passes complexity (need 50) *)
      assert result_any.achieved;

      Printf.printf "[OK] Composite goal\n%!"
    in

    (* ===== Test 6: Level 3 - Temporal Eventually ===== *)
    let () =
      let module Int_temporal = Make_temporal(struct type t = int end) in

      let trace = [1; 2; 3; 5; 8; 13] in

      (* Eventually reaches 5 *)
      assert (Int_temporal.eventually (fun x -> x = 5) trace);

      (* Eventually > 10 *)
      assert (Int_temporal.eventually (fun x -> x > 10) trace);

      (* Never reaches 100 *)
      assert (not (Int_temporal.eventually (fun x -> x = 100) trace));

      Printf.printf "[OK] Temporal eventually\n%!"
    in

    (* ===== Test 7: Level 3 - Temporal Always ===== *)
    let () =
      let module Int_temporal = Make_temporal(struct type t = int end) in

      let trace = [2; 4; 6; 8; 10] in

      (* Always even *)
      assert (Int_temporal.always (fun x -> x mod 2 = 0) trace);

      (* Not always > 5 *)
      assert (not (Int_temporal.always (fun x -> x > 5) trace));

      Printf.printf "[OK] Temporal always\n%!"
    in

    (* ===== Test 8: Level 3 - Temporal Until ===== *)
    let () =
      let module Int_temporal = Make_temporal(struct type t = int end) in

      let trace = [1; 2; 3; 10; 20; 30] in

      (* Small until big *)
      assert (Int_temporal.until (fun x -> x < 10) (fun x -> x >= 10) trace);

      (* Positive until negative - fails because no negative *)
      assert (not (Int_temporal.until (fun x -> x > 0) (fun x -> x < 0) trace));

      Printf.printf "[OK] Temporal until\n%!"
    in

    (* ===== Test 9: Level 3 - Temporal Bounded ===== *)
    let () =
      let module Int_temporal = Make_temporal(struct type t = int end) in

      let trace = [1; 2; 3; 100; 5; 6] in

      (* Eventually within 5 steps reaches 100 *)
      assert (Int_temporal.eventually_within 5 (fun x -> x = 100) trace);

      (* Eventually within 2 steps does NOT reach 100 *)
      assert (not (Int_temporal.eventually_within 2 (fun x -> x = 100) trace));

      (* Always for 3 steps < 50 *)
      assert (Int_temporal.always_for 3 (fun x -> x < 50) trace);

      (* Always for 5 steps < 50 - fails at step 4 (100) *)
      assert (not (Int_temporal.always_for 5 (fun x -> x < 50) trace));

      Printf.printf "[OK] Temporal bounded\n%!"
    in

    (* ===== Test 10: Level 4 - Emergent Health ===== *)
    let () =
      (* Define component type outside the functor *)
      let module Service_component = struct
        type t = { name : string; healthy : bool }
        let name c = c.name  (* Use the field to avoid warning 69 *)
      end in

      let module Service_health = Make_health_emergent(struct
        type component = Service_component.t
        let is_healthy c = c.Service_component.healthy
        let threshold = 0.8
      end) in
      let _ = Service_component.name in  (* Reference to suppress warning *)

      let healthy_system : Service_component.t list = [
        { name = "api"; healthy = true };
        { name = "db"; healthy = true };
        { name = "cache"; healthy = true };
        { name = "queue"; healthy = true };
        { name = "worker"; healthy = true };
      ] in

      let obs = Service_health.observe healthy_system in
      assert (Service_health.check_property obs);
      assert (Service_health.measure obs = 1.0);

      let degraded_system : Service_component.t list = [
        { name = "api"; healthy = true };
        { name = "db"; healthy = true };
        { name = "cache"; healthy = false };
        { name = "queue"; healthy = false };
        { name = "worker"; healthy = false };
      ] in

      let obs2 = Service_health.observe degraded_system in
      assert (not (Service_health.check_property obs2));  (* 2/5 = 40% < 80% *)
      assert (Service_health.measure obs2 < 0.8);

      Printf.printf "[OK] Emergent health\n%!"
    in

    (* ===== Test 11: Level 5 - Approval Judgment ===== *)
    let () =
      let (is_positive, confidence) = Approval_judgment.interpret Feedback.Approved in
      assert is_positive;
      assert (confidence = 1.0);

      let (is_positive2, confidence2) = Approval_judgment.interpret Feedback.Rejected in
      assert (not is_positive2);
      assert (confidence2 = 1.0);

      let (is_positive3, confidence3) = Approval_judgment.interpret (Feedback.NeedsRevision "Fix typo") in
      assert (not is_positive3);
      assert (confidence3 = 0.5);

      Printf.printf "[OK] Approval judgment\n%!"
    in

    (* ===== Test 12: Level 5 - Rating Judgment ===== *)
    let () =
      let module Rating_judge = Rating_judgment(struct let threshold = 0.7 end) in

      let good_rating = { Feedback.score = 4; max_score = 5; comment = Some "Good!" } in
      let (score, _) = Rating_judge.interpret good_rating in
      assert (score = 0.8);  (* 4/5 *)
      assert (Rating_judge.is_positive (score, 1.0));

      let bad_rating = { Feedback.score = 2; max_score = 5; comment = None } in
      let (score2, _) = Rating_judge.interpret bad_rating in
      assert (score2 = 0.4);  (* 2/5 *)
      assert (not (Rating_judge.is_positive (score2, 1.0)));

      Printf.printf "[OK] Rating judgment\n%!"
    in

    (* ===== Test 13: Unified Builder - Basic ===== *)
    let () =
      let builder = Unified_builder.create () in
      let builder = Unified_builder.with_metric (fun _ output -> output) builder in
      let builder = Unified_builder.with_goal (fun _ output ->
        { achieved = output >= 0.8; distance = max 0.0 (0.8 -. output);
          progress = output /. 0.8; description = "Quality goal" }
      ) builder in

      let result = Unified_builder.validate builder () () 0.9 in
      assert result.all_passed;
      assert (List.length result.levels = 2);

      Printf.printf "[OK] Unified builder basic\n%!"
    in

    (* ===== Test 14: Unified Builder - With Spec ===== *)
    let () =
      let module Counter_spec : Spec_dsl.SPEC with type input = int
                                               and type output = int
                                               and type state = int = struct
        type input = int
        type output = int
        type state = int
        let precondition _ i = i > 0
        let postcondition s i o = o = s + i
        let invariant s = s >= 0
        let transition s i = s + i
      end in

      let builder = Unified_builder.create () in
      let builder = Unified_builder.with_spec (module Counter_spec) builder in
      let builder = Unified_builder.with_metric (fun _ output ->
        float_of_int output /. 100.0
      ) builder in

      (* Valid case *)
      let result = Unified_builder.validate builder 10 5 15 in
      assert result.all_passed;

      (* Invalid case - precondition violation *)
      let result2 = Unified_builder.validate builder 10 (-1) 9 in
      assert (not result2.all_passed);

      Printf.printf "[OK] Unified builder with spec\n%!"
    in

    (* ===== Test 15: Unified Builder - With Temporal ===== *)
    let () =
      let builder = Unified_builder.create () in

      (* Record some states *)
      Unified_builder.record_state 1 builder;
      Unified_builder.record_state 2 builder;
      Unified_builder.record_state 5 builder;
      Unified_builder.record_state 10 builder;

      let builder = Unified_builder.with_temporal (fun trace ->
        (* Check that values eventually reach 10 *)
        List.exists (fun s -> s >= 10) trace
      ) builder in

      let result = Unified_builder.validate builder 0 () () in
      assert result.all_passed;

      Printf.printf "[OK] Unified builder with temporal\n%!"
    in

    (* ===== Test 16: Pretty Printing ===== *)
    let () =
      let level_result = {
        level = 1;
        name = "METRIC";
        passed = true;
        score = 0.85;
        details = "Quality score: 0.85";
      } in

      let str = Pretty.result_to_string level_result in
      assert (String.length str > 0);
      (* Check that the string contains key information *)
      assert (String.length str > 20);  (* Should be substantial output *)

      let stack_result = {
        all_passed = true;
        levels = [level_result];
        summary = "✅ VALID - 1/1 levels passed";
      } in

      let full_str = Pretty.stack_result_to_string stack_result in
      assert (String.length full_str > 0);

      Printf.printf "[OK] Pretty printing\n%!"
    in

    (* ===== Test 17: Level 6 - AI Intervenor ===== *)
    let () =
      let module AI_monitor = Make_ai_intervenor(struct
        let stall_threshold_sec = 60.0
        let max_failures = 3
      end) in

      (* No intervention needed - good progress *)
      let obs1 = AI_monitor.observe (0.7, 30.0, 0) in
      assert (AI_monitor.should_intervene obs1 = None);

      (* Stall detected - low progress after threshold *)
      let obs2 = AI_monitor.observe (0.2, 120.0, 0) in
      let trigger2 = AI_monitor.should_intervene obs2 in
      assert (match trigger2 with Some (Stalled _) -> true | _ -> false);

      (* Repeated failures detected *)
      let obs3 = AI_monitor.observe (0.5, 10.0, 3) in
      let trigger3 = AI_monitor.should_intervene obs3 in
      assert (match trigger3 with Some (RepeatedFailure _) -> true | _ -> false);

      Printf.printf "[OK] AI Intervenor\n%!"
    in

    (* ===== Test 18: Intervenor Decision Making ===== *)
    let () =
      let module AI_monitor = Make_ai_intervenor(struct
        let stall_threshold_sec = 60.0
        let max_failures = 3
      end) in

      (* Very low progress -> Redefine *)
      let trigger1 = Stalled { duration_sec = 120.0; progress = 0.05 } in
      let decision1 = AI_monitor.decide (0.0, 0.0, 0) trigger1 in
      assert (match decision1 with Redefine _ -> true | _ -> false);

      (* Low progress -> Redirect *)
      let trigger2 = Stalled { duration_sec = 120.0; progress = 0.25 } in
      let decision2 = AI_monitor.decide (0.0, 0.0, 0) trigger2 in
      assert (match decision2 with Redirect _ -> true | _ -> false);

      (* Moderate progress -> Continue *)
      let trigger3 = Stalled { duration_sec = 120.0; progress = 0.5 } in
      let decision3 = AI_monitor.decide (0.0, 0.0, 0) trigger3 in
      assert (match decision3 with Continue -> true | _ -> false);

      Printf.printf "[OK] Intervenor decisions\n%!"
    in

    (* ===== Test 19: Stall Detector ===== *)
    let () =
      let clock = Eio.Stdenv.clock _env in

      let detector = Stall_detector.create
        ~importance:0.8
        ~extract_progress:(fun (p, _) -> p)
        ~clock in

      (* Progress update - no stall *)
      let result1 = Stall_detector.update detector clock (0.5, "task1") in
      assert (result1 = None);

      (* Another progress update *)
      let result2 = Stall_detector.update detector clock (0.7, "task1") in
      assert (result2 = None);

      Printf.printf "[OK] Stall detector\n%!"
    in

    (* ===== Test 20: Human Intervenor Types ===== *)
    let () =
      assert (Human_intervenor.is_human = true);

      let obs = Human_intervenor.observe "test context" in
      assert (obs = "test context");

      (* Human intervenor doesn't auto-detect *)
      let should = Human_intervenor.should_intervene obs in
      assert (should = None);

      Printf.printf "[OK] Human intervenor types\n%!"
    in

    Printf.printf "\n✅ All 20 validation stack tests passed!\n%!"

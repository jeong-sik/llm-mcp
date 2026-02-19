(** Chain Composer Integration Tests *)

open Alcotest

(** Helper: check if string contains substring *)
let contains_substring haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(** Test: Build design context from MASC-like tasks *)
let test_build_design_context () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "task-001"; title = "Fetch user data";
      description = Some "Call API to get user profile";
      priority = 1; status = "todo"; assignee = None; metadata = [] };
    { task_id = "task-002"; title = "Process data";
      description = Some "Transform and validate user data";
      priority = 2; status = "todo"; assignee = None; metadata = [] };
    { task_id = "task-003"; title = "Store result";
      description = Some "Save processed data to database";
      priority = 3; status = "todo"; assignee = None; metadata = [] };
  ] in

  let context = Chain_composer.build_design_context
    ~goal:"Process user registration workflow"
    ~tasks
  in

  (* Verify context contains key elements *)
  check bool "contains goal" true
    (contains_substring context "Process user registration");
  check bool "contains task-001" true
    (contains_substring context "task-001");
  check bool "contains instructions" true
    (contains_substring context "Dependencies");

  Printf.printf "\n📋 Design Context Preview (first 500 chars):\n%s...\n"
    (String.sub context 0 (min 500 (String.length context)))

(** Test: State management lifecycle *)
let test_composer_state_lifecycle () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "t1"; title = "Task 1"; description = None;
      priority = 1; status = "todo"; assignee = None; metadata = [] };
  ] in

  (* Initialize state *)
  let state = Chain_composer.init_state
    ~session_id:"session-001"
    ~goal:"Test goal"
    ~tasks
    ~max_replans:3
  in

  check string "session_id" "session-001" state.session_id;
  check string "goal" "Test goal" state.goal;
  check int "replan_count" 0 state.replan_count;
  check int "max_replans" 3 state.max_replans;
  check bool "no chain yet" true (state.current_chain = None);

  (* Increment replan *)
  let state2 = Chain_composer.increment_replan state in
  check int "replan_count after increment" 1 state2.replan_count;

  Printf.printf "✅ State lifecycle test passed\n"

(** Test: Verification context generation *)
let test_build_verification_context () =
  let metrics : Chain_evaluator.chain_metrics = {
    chain_id = "chain-001";
    goal = "Complete user registration";
    started_at = Unix.gettimeofday ();
    completed_at = Some (Unix.gettimeofday ());
    total_duration_ms = 1500;
    total_nodes = 3;
    nodes_succeeded = 2;
    nodes_failed = 1;
    nodes_skipped = 0;
    nodes_pending = 0;
    parallel_groups = 1;
    max_depth = 2;
    success_rate = 0.67;
    parallelization_efficiency = 0.85;
    estimation_accuracy = 0.90;
    node_metrics = [
      { node_id = "n1"; node_type = "llm"; status = Chain_evaluator.Succeeded;
        started_at = Some 0.0; completed_at = Some 0.5; duration_ms = 500;
        estimated_duration_ms = Some 600; retry_count = 0;
        error_message = None; output_preview = Some "User data fetched" };
      { node_id = "n2"; node_type = "tool"; status = Chain_evaluator.Succeeded;
        started_at = Some 0.5; completed_at = Some 1.0; duration_ms = 500;
        estimated_duration_ms = Some 400; retry_count = 0;
        error_message = None; output_preview = Some "Data processed" };
      { node_id = "n3"; node_type = "tool"; status = Chain_evaluator.Failed;
        started_at = Some 1.0; completed_at = Some 1.5; duration_ms = 500;
        estimated_duration_ms = Some 300; retry_count = 2;
        error_message = Some "Database connection failed";
        output_preview = None };
    ];
    verification = None;
  } in

  let context = Chain_composer.build_verification_prompt
    ~goal:"Complete user registration"
    ~metrics
  in

  check bool "contains goal" true
    (contains_substring context "Complete user registration");
  check bool "contains succeeded count" true
    (contains_substring context "Succeeded: 2");
  check bool "contains failed count" true
    (contains_substring context "Failed: 1");

  Printf.printf "\n🔍 Verification Context Preview:\n%s\n"
    (String.sub context 0 (min 800 (String.length context)))

(** Test: Decision making based on metrics *)
let test_decide_next_action () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "t1"; title = "Task 1"; description = None;
      priority = 1; status = "done"; assignee = None; metadata = [] };
  ] in

  let state = Chain_composer.init_state
    ~session_id:"session-002"
    ~goal:"Test decision"
    ~tasks
    ~max_replans:3
  in

  (* Test 1: All succeeded with high confidence verification -> Complete *)
  let metrics_success : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "Test"; started_at = 0.0; completed_at = Some 1.0;
    total_duration_ms = 1000; total_nodes = 2; nodes_succeeded = 2;
    nodes_failed = 0; nodes_skipped = 0; nodes_pending = 0;
    parallel_groups = 1; max_depth = 1; success_rate = 1.0;
    parallelization_efficiency = 1.0; estimation_accuracy = 1.0;
    node_metrics = []; verification = None;
  } in

  let verification_complete : Chain_evaluator.verification_result = {
    is_complete = true;
    confidence = 0.95;
    reason = "All tasks completed successfully";
    missing_criteria = [];
    suggested_next_steps = [];
  } in

  let decision1 = Chain_composer.decide_next_action
    ~state ~metrics:metrics_success ~verification:(Some verification_complete) in

  (match decision1 with
   | Chain_composer.Complete _ -> Printf.printf "✅ Decision 1: Complete (correct)\n"
   | _ -> failwith "Expected Complete decision");

  (* Test 2: Has failures, under max replans -> Replan *)
  let metrics_failed : Chain_evaluator.chain_metrics = {
    metrics_success with
    nodes_succeeded = 1; nodes_failed = 1; nodes_pending = 0;
    success_rate = 0.5;
    node_metrics = [
      { node_id = "n1"; node_type = "llm"; status = Chain_evaluator.Failed;
        started_at = Some 0.0; completed_at = Some 0.5; duration_ms = 500;
        estimated_duration_ms = None; retry_count = 0;
        error_message = Some "API Error"; output_preview = None };
    ];
  } in

  let decision2 = Chain_composer.decide_next_action
    ~state ~metrics:metrics_failed ~verification:None in

  (match decision2 with
   | Chain_composer.Replan _ -> Printf.printf "✅ Decision 2: Replan (correct)\n"
   | _ -> failwith "Expected Replan decision");

  (* Test 3: Max replans exceeded -> Abort *)
  let state_max_replans = { state with replan_count = 3 } in
  let decision3 = Chain_composer.decide_next_action
    ~state:state_max_replans ~metrics:metrics_failed ~verification:None in

  (match decision3 with
   | Chain_composer.Abort _ -> Printf.printf "✅ Decision 3: Abort (correct)\n"
   | Chain_composer.Continue -> Printf.printf "✅ Decision 3: Continue (acceptable)\n"
   | _ -> failwith "Expected Abort or Continue decision")

(** Test: Evaluation timing control *)
let test_evaluation_timing () =
  let metrics : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "Test"; started_at = 0.0; completed_at = None;
    total_duration_ms = 0; total_nodes = 3; nodes_succeeded = 1;
    nodes_failed = 0; nodes_skipped = 0; nodes_pending = 2;
    parallel_groups = 1; max_depth = 1; success_rate = 1.0;
    parallelization_efficiency = 1.0; estimation_accuracy = 1.0;
    node_metrics = [
      { node_id = "critical-1"; node_type = "llm";
        status = Chain_evaluator.Succeeded;
        started_at = Some 0.0; completed_at = Some 0.5; duration_ms = 500;
        estimated_duration_ms = None; retry_count = 0;
        error_message = None; output_preview = Some "Done" };
      { node_id = "worker-1"; node_type = "tool";
        status = Chain_evaluator.Running;
        started_at = Some 0.5; completed_at = None; duration_ms = 0;
        estimated_duration_ms = None; retry_count = 0;
        error_message = None; output_preview = None };
    ];
    verification = None;
  } in

  (* Should evaluate when critical node completes *)
  let should_eval_critical = Chain_evaluator.should_evaluate
    ~trigger:(Chain_evaluator.OnNodeComplete "critical-1")
    ~metrics in
  check bool "should eval on critical complete" true should_eval_critical;

  (* Should NOT evaluate when pending node not complete *)
  let should_eval_worker = Chain_evaluator.should_evaluate
    ~trigger:(Chain_evaluator.OnNodeComplete "worker-1")
    ~metrics in
  check bool "should NOT eval on running node" false should_eval_worker;

  (* Should evaluate on failure *)
  let metrics_with_failure = { metrics with
    nodes_failed = 1;
    node_metrics = [
      { (List.hd metrics.node_metrics) with
        status = Chain_evaluator.Failed;
        error_message = Some "Error!" };
    ];
  } in
  let should_eval_failure = Chain_evaluator.should_evaluate
    ~trigger:Chain_evaluator.OnFailure
    ~metrics:metrics_with_failure in
  check bool "should eval on failure" true should_eval_failure;

  Printf.printf "✅ Evaluation timing tests passed\n"

(** Test: Generate summary report *)
let test_generate_summary () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "t1"; title = "Test Task"; description = None;
      priority = 1; status = "done"; assignee = None; metadata = [] };
  ] in

  let state = Chain_composer.init_state
    ~session_id:"summary-test"
    ~goal:"Generate a complete workflow"
    ~tasks
    ~max_replans:2
  in

  let summary = Chain_composer.generate_summary state in

  check bool "summary not empty" true (String.length summary > 0);
  check bool "contains session id" true
    (contains_substring summary "summary-test");

  Printf.printf "\n📊 Session Summary:\n%s\n" summary

(** Test: build_replan_context all 5 reason variants *)
let test_build_replan_context () =
  let chain : Chain_types.chain = {
    id = "replan-chain";
    nodes = [
      { id = "n1"; node_type = Llm { model = "gemini"; system = None; prompt = "test"; timeout = None; tools = None; prompt_ref = None; prompt_vars = []; thinking = false };
        input_mapping = []; output_key = None; depends_on = None };
    ];
    output = "n1";
    config = Chain_types.default_config;
    name = None;
    description = None;
    version = None;
    input_schema = None;
    output_schema = None;
    metadata = None;
  } in
  let metrics : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "test"; started_at = 0.0; completed_at = Some 1.0;
    total_duration_ms = 1000; total_nodes = 1; nodes_succeeded = 0;
    nodes_failed = 1; nodes_skipped = 0; nodes_pending = 0;
    parallel_groups = 1; max_depth = 1; success_rate = 0.0;
    parallelization_efficiency = 1.0; estimation_accuracy = 1.0;
    node_metrics = [
      { node_id = "n1"; node_type = "llm"; status = Chain_evaluator.Failed;
        started_at = Some 0.0; completed_at = Some 1.0; duration_ms = 1000;
        estimated_duration_ms = None; retry_count = 0;
        error_message = Some "API Error"; output_preview = None };
    ];
    verification = None;
  } in
  let reasons = [
    Chain_composer.TaskFailed "n1";
    Chain_composer.GoalNotAchieved;
    Chain_composer.NewTaskAdded "n2";
    Chain_composer.ContextChanged;
    Chain_composer.TimeoutApproaching;
  ] in
  List.iter (fun reason ->
    let ctx = Chain_composer.build_replan_context ~goal:"test goal" ~original_chain:chain ~reason ~metrics in
    check bool "contains Re-Planning" true (contains_substring ctx "Re-Planning");
    check bool "not empty" true (String.length ctx > 0)
  ) reasons

(** Test: determine_eval_triggers *)
let test_determine_eval_triggers () =
  let chain : Chain_types.chain = {
    id = "eval-chain"; nodes = []; output = "x";
    config = Chain_types.default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None;
  } in
  let triggers = Chain_composer.determine_eval_triggers ~_chain:chain ~critical_path:["n1"; "n2"] in
  check bool "has OnChainComplete" true
    (List.exists (fun t -> t = Chain_evaluator.OnChainComplete) triggers);
  check bool "has OnFailure" true
    (List.exists (fun t -> t = Chain_evaluator.OnFailure) triggers);
  check bool "has critical node n1" true
    (List.exists (fun t -> t = Chain_evaluator.OnNodeComplete "n1") triggers);
  check bool "has critical node n2" true
    (List.exists (fun t -> t = Chain_evaluator.OnNodeComplete "n2") triggers)

(** Test: should_replan branches *)
let test_should_replan () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "t1"; title = "Task 1"; description = None;
      priority = 1; status = "done"; assignee = None; metadata = [] };
  ] in

  (* State at max replans - should return None *)
  let state_at_max = Chain_composer.init_state
    ~session_id:"s1" ~goal:"test" ~tasks ~max_replans:0 in
  let empty_metrics : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "test"; started_at = 0.0; completed_at = None;
    total_duration_ms = 0; total_nodes = 0; nodes_succeeded = 0;
    nodes_failed = 0; nodes_skipped = 0; nodes_pending = 0;
    parallel_groups = 0; max_depth = 0; success_rate = 0.0;
    parallelization_efficiency = 0.0; estimation_accuracy = 0.0;
    node_metrics = []; verification = None;
  } in
  let r1 = Chain_composer.should_replan ~state:state_at_max ~metrics:empty_metrics ~verification:None in
  check (option string) "max replans => None" None (Option.map (fun _ -> "some") r1);

  (* Failures present with room for replans *)
  let state_ok = Chain_composer.init_state
    ~session_id:"s2" ~goal:"test" ~tasks ~max_replans:3 in
  let metrics_with_fail = { empty_metrics with
    nodes_failed = 1;
    node_metrics = [
      { node_id = "n1"; node_type = "llm"; status = Chain_evaluator.Failed;
        started_at = Some 0.0; completed_at = Some 1.0; duration_ms = 1000;
        estimated_duration_ms = None; retry_count = 0;
        error_message = Some "fail"; output_preview = None };
    ];
  } in
  let r2 = Chain_composer.should_replan ~state:state_ok ~metrics:metrics_with_fail ~verification:None in
  (match r2 with
   | Some (Chain_composer.TaskFailed _) -> ()
   | _ -> fail "expected TaskFailed");

  (* GoalNotAchieved: complete, not verified *)
  let verification_incomplete : Chain_evaluator.verification_result = {
    is_complete = false; confidence = 0.3; reason = "not done";
    missing_criteria = ["x"]; suggested_next_steps = [];
  } in
  let metrics_done = { empty_metrics with nodes_pending = 0; nodes_succeeded = 2 } in
  let r3 = Chain_composer.should_replan ~state:state_ok ~metrics:metrics_done ~verification:(Some verification_incomplete) in
  (match r3 with
   | Some Chain_composer.GoalNotAchieved -> ()
   | _ -> fail "expected GoalNotAchieved")

(** Test: decide_next_action - Continue and Abort paths *)
let test_decide_next_action_continue_and_abort () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "t1"; title = "Task 1"; description = None;
      priority = 1; status = "done"; assignee = None; metadata = [] };
  ] in
  let state = Chain_composer.init_state
    ~session_id:"s1" ~goal:"test" ~tasks ~max_replans:3 in
  let empty_metrics : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "test"; started_at = 0.0; completed_at = None;
    total_duration_ms = 0; total_nodes = 2; nodes_succeeded = 1;
    nodes_failed = 0; nodes_skipped = 0; nodes_pending = 1;
    parallel_groups = 1; max_depth = 1; success_rate = 0.5;
    parallelization_efficiency = 1.0; estimation_accuracy = 1.0;
    node_metrics = []; verification = None;
  } in

  (* Continue: pending nodes, no failures *)
  let d1 = Chain_composer.decide_next_action ~state ~metrics:empty_metrics ~verification:None in
  (match d1 with
   | Chain_composer.Continue -> ()
   | _ -> fail "expected Continue");

  (* Abort: failures with max replans exceeded *)
  let state_at_max = { state with replan_count = 3 } in
  let metrics_fail = { empty_metrics with nodes_failed = 1; nodes_pending = 0;
    node_metrics = [
      { node_id = "n1"; node_type = "llm"; status = Chain_evaluator.Failed;
        started_at = Some 0.0; completed_at = Some 1.0; duration_ms = 1000;
        estimated_duration_ms = None; retry_count = 0;
        error_message = Some "err"; output_preview = None };
    ];
  } in
  let d2 = Chain_composer.decide_next_action ~state:state_at_max ~metrics:metrics_fail ~verification:None in
  (match d2 with
   | Chain_composer.Abort _ -> ()
   | _ -> fail "expected Abort");

  (* Complete with low confidence (all done, no failures, no replan needed) *)
  let metrics_done = { empty_metrics with nodes_pending = 0; nodes_succeeded = 2; nodes_failed = 0 } in
  let d3 = Chain_composer.decide_next_action ~state ~metrics:metrics_done ~verification:None in
  (match d3 with
   | Chain_composer.Complete _ -> ()
   | _ -> fail "expected Complete for all-done-no-replan")

(** Test: get_replan_context with None chain *)
let test_get_replan_context_none () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "t1"; title = "Task 1"; description = None;
      priority = 1; status = "done"; assignee = None; metadata = [] };
  ] in
  let state = Chain_composer.init_state
    ~session_id:"s1" ~goal:"test" ~tasks ~max_replans:3 in
  let empty_metrics : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "test"; started_at = 0.0; completed_at = None;
    total_duration_ms = 0; total_nodes = 0; nodes_succeeded = 0;
    nodes_failed = 0; nodes_skipped = 0; nodes_pending = 0;
    parallel_groups = 0; max_depth = 0; success_rate = 0.0;
    parallelization_efficiency = 0.0; estimation_accuracy = 0.0;
    node_metrics = []; verification = None;
  } in
  let result = Chain_composer.get_replan_context state Chain_composer.GoalNotAchieved empty_metrics in
  check (option string) "None chain => None context" None (Option.map (fun _ -> "some") result)

(** Test: should_evaluate_now *)
let test_should_evaluate_now () =
  let tasks : Chain_composer.masc_task list = [] in
  let state = Chain_composer.init_state
    ~session_id:"s1" ~goal:"test" ~tasks ~max_replans:3 in
  let empty_metrics : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "test"; started_at = 0.0; completed_at = None;
    total_duration_ms = 0; total_nodes = 0; nodes_succeeded = 0;
    nodes_failed = 0; nodes_skipped = 0; nodes_pending = 0;
    parallel_groups = 0; max_depth = 0; success_rate = 0.0;
    parallelization_efficiency = 0.0; estimation_accuracy = 0.0;
    node_metrics = []; verification = None;
  } in
  let result = Chain_composer.should_evaluate_now ~_state:state ~trigger:Chain_evaluator.OnChainComplete ~metrics:empty_metrics in
  check bool "should evaluate on chain complete" true result

(** Test: generate_summary with checkpoints *)
let test_generate_summary_with_checkpoints () =
  let tasks : Chain_composer.masc_task list = [
    { task_id = "t1"; title = "Task 1"; description = None;
      priority = 1; status = "done"; assignee = None; metadata = [] };
  ] in
  let state = Chain_composer.init_state
    ~session_id:"checkpoint-test"
    ~goal:"Test with checkpoints"
    ~tasks
    ~max_replans:2
  in
  let analysis : Chain_composer.composition_analysis = {
    goal = "Test with checkpoints";
    tasks = [];
    relations = [];
    estimated_duration_ms = 1000;
    critical_path = ["n1"; "n2"];
    parallelizable_groups = [["n1"; "n2"]];
  } in
  let state = Chain_composer.set_analysis state analysis in
  let empty_metrics : Chain_evaluator.chain_metrics = {
    chain_id = "c1"; goal = "test"; started_at = 0.0; completed_at = None;
    total_duration_ms = 0; total_nodes = 0; nodes_succeeded = 0;
    nodes_failed = 0; nodes_skipped = 0; nodes_pending = 0;
    parallel_groups = 0; max_depth = 0; success_rate = 0.0;
    parallelization_efficiency = 0.0; estimation_accuracy = 0.0;
    node_metrics = []; verification = None;
  } in
  let state = Chain_composer.add_checkpoint state
    ~trigger:Chain_evaluator.OnChainComplete
    ~metrics:empty_metrics
    ~decision:`Complete
    ~reason:"All done" in
  let summary = Chain_composer.generate_summary state in
  check bool "contains checkpoint-test" true (contains_substring summary "checkpoint-test");
  check bool "contains analysis" true (contains_substring summary "Critical Path");
  check bool "contains checkpoint count" true (contains_substring summary "CHECKPOINTS: 1")

(** All tests *)
let () =
  run "Chain Composer" [
    "design", [
      test_case "build_design_context" `Quick test_build_design_context;
    ];
    "state", [
      test_case "lifecycle" `Quick test_composer_state_lifecycle;
    ];
    "verification", [
      test_case "build_verification_context" `Quick test_build_verification_context;
    ];
    "decision", [
      test_case "decide_next_action" `Quick test_decide_next_action;
      test_case "continue_and_abort" `Quick test_decide_next_action_continue_and_abort;
    ];
    "timing", [
      test_case "evaluation_timing" `Quick test_evaluation_timing;
      test_case "should_evaluate_now" `Quick test_should_evaluate_now;
    ];
    "replan", [
      test_case "build_replan_context" `Quick test_build_replan_context;
      test_case "determine_eval_triggers" `Quick test_determine_eval_triggers;
      test_case "should_replan" `Quick test_should_replan;
      test_case "get_replan_context_none" `Quick test_get_replan_context_none;
    ];
    "summary", [
      test_case "generate_summary" `Quick test_generate_summary;
      test_case "summary_with_checkpoints" `Quick test_generate_summary_with_checkpoints;
    ];
  ]

(** Tests for Chain_evaluator module — Metrics, verification, triggers, reports
    Targets: empty_node_metrics, empty_chain_metrics, mark_node_started/completed/failed,
    calculate_chain_stats, should_evaluate, build_verification_context,
    parse_verification_response, generate_report *)

open Alcotest

module CE = Chain_evaluator

(** {1 Empty Metrics} *)

let test_empty_node_metrics () =
  let m = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  check string "node_id" "n1" m.node_id;
  check string "node_type" "llm" m.node_type;
  check bool "pending" true (m.status = CE.Pending);
  check int "duration" 0 m.duration_ms;
  check int "retries" 0 m.retry_count;
  check (option string) "no error" None m.error_message;
  check (option string) "no preview" None m.output_preview

let test_empty_chain_metrics () =
  let m = CE.empty_chain_metrics ~chain_id:"chain1" ~goal:"Test goal" in
  check string "chain_id" "chain1" m.chain_id;
  check string "goal" "Test goal" m.goal;
  check int "total_nodes" 0 m.total_nodes;
  check int "succeeded" 0 m.nodes_succeeded;
  check int "failed" 0 m.nodes_failed;
  check (float 0.01) "success_rate" 0.0 m.success_rate;
  check bool "no verification" true (m.verification = None)

(** {1 Node Status Updates} *)

let test_mark_node_started () =
  let m = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let m' = CE.mark_node_started m in
  check bool "running" true (m'.status = CE.Running);
  check bool "has started_at" true (m'.started_at <> None)

let test_mark_node_completed () =
  let m = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let m' = CE.mark_node_started m in
  let m'' = CE.mark_node_completed m' ~output_preview:"result text here" in
  check bool "succeeded" true (m''.status = CE.Succeeded);
  check bool "has completed_at" true (m''.completed_at <> None);
  check bool "has preview" true (m''.output_preview <> None)

let test_mark_node_completed_long_preview () =
  let m = CE.empty_node_metrics ~node_id:"n1" ~node_type:"tool" in
  let m' = CE.mark_node_started m in
  let long_text = String.make 500 'x' in
  let m'' = CE.mark_node_completed m' ~output_preview:long_text in
  match m''.output_preview with
  | Some p -> check bool "truncated to 200" true (String.length p <= 200)
  | None -> fail "expected preview"

let test_mark_node_completed_no_start () =
  let m = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let m' = CE.mark_node_completed m ~output_preview:"test" in
  check int "duration 0 without start" 0 m'.duration_ms

let test_mark_node_failed () =
  let m = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let m' = CE.mark_node_started m in
  let m'' = CE.mark_node_failed m' ~error_message:"connection timeout" in
  check bool "failed" true (m''.status = CE.Failed);
  check (option string) "error msg" (Some "connection timeout") m''.error_message

let test_mark_node_failed_no_start () =
  let m = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let m' = CE.mark_node_failed m ~error_message:"immediate error" in
  check int "duration 0" 0 m'.duration_ms

(** {1 Calculate Chain Stats} *)

let test_calculate_chain_stats_empty () =
  let m = CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test" in
  let m' = CE.calculate_chain_stats m in
  check int "total" 0 m'.total_nodes;
  check (float 0.01) "success_rate" 0.0 m'.success_rate;
  check (float 0.01) "estimation_accuracy" 1.0 m'.estimation_accuracy

let test_calculate_chain_stats_mixed () =
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
             status = CE.Succeeded; duration_ms = 100 } in
  let n2 = { (CE.empty_node_metrics ~node_id:"n2" ~node_type:"tool") with
             status = CE.Failed; error_message = Some "err" } in
  let n3 = { (CE.empty_node_metrics ~node_id:"n3" ~node_type:"llm") with
             status = CE.Skipped } in
  let n4 = CE.empty_node_metrics ~node_id:"n4" ~node_type:"llm" in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
            node_metrics = [n1; n2; n3; n4] } in
  let m' = CE.calculate_chain_stats m in
  check int "total" 4 m'.total_nodes;
  check int "succeeded" 1 m'.nodes_succeeded;
  check int "failed" 1 m'.nodes_failed;
  check int "skipped" 1 m'.nodes_skipped;
  check int "pending" 1 m'.nodes_pending;
  (* success_rate = 1 / (1+1) = 0.5 *)
  check (float 0.01) "success_rate" 0.5 m'.success_rate

let test_calculate_chain_stats_all_succeed () =
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
             status = CE.Succeeded } in
  let n2 = { (CE.empty_node_metrics ~node_id:"n2" ~node_type:"llm") with
             status = CE.Succeeded } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
            node_metrics = [n1; n2] } in
  let m' = CE.calculate_chain_stats m in
  check (float 0.01) "success_rate" 1.0 m'.success_rate

let test_calculate_chain_stats_with_estimates () =
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
             status = CE.Succeeded; duration_ms = 100;
             estimated_duration_ms = Some 100 } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
            node_metrics = [n1] } in
  let m' = CE.calculate_chain_stats m in
  check (float 0.01) "estimation_accuracy" 1.0 m'.estimation_accuracy

(** {1 Should Evaluate (Triggers)} *)

let test_should_evaluate_on_node_complete () =
  let n1 = { (CE.empty_node_metrics ~node_id:"done_node" ~node_type:"llm") with
             status = CE.Succeeded } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
            node_metrics = [n1] } in
  check bool "triggers on completed node" true
    (CE.should_evaluate ~trigger:(CE.OnNodeComplete "done_node") ~metrics:m);
  check bool "does not trigger on other node" false
    (CE.should_evaluate ~trigger:(CE.OnNodeComplete "other_node") ~metrics:m)

let test_should_evaluate_on_group_complete () =
  let n1 = { (CE.empty_node_metrics ~node_id:"a" ~node_type:"llm") with
             status = CE.Succeeded } in
  let n2 = { (CE.empty_node_metrics ~node_id:"b" ~node_type:"llm") with
             status = CE.Failed } in
  let n3 = { (CE.empty_node_metrics ~node_id:"c" ~node_type:"llm") with
             status = CE.Skipped } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
            node_metrics = [n1; n2; n3] } in
  check bool "all completed" true
    (CE.should_evaluate ~trigger:(CE.OnGroupComplete ["a"; "b"; "c"]) ~metrics:m);
  let n4 = CE.empty_node_metrics ~node_id:"d" ~node_type:"llm" in
  let m2 = { m with node_metrics = [n1; n4] } in
  check bool "not all completed" false
    (CE.should_evaluate ~trigger:(CE.OnGroupComplete ["a"; "d"]) ~metrics:m2)

let test_should_evaluate_on_chain_complete () =
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
            nodes_pending = 0 } in
  check bool "chain complete" true
    (CE.should_evaluate ~trigger:CE.OnChainComplete ~metrics:m);
  let m2 = { m with nodes_pending = 1 } in
  check bool "chain not complete" false
    (CE.should_evaluate ~trigger:CE.OnChainComplete ~metrics:m2)

let test_should_evaluate_on_failure () =
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
            nodes_failed = 1 } in
  check bool "has failure" true
    (CE.should_evaluate ~trigger:CE.OnFailure ~metrics:m);
  let m2 = { m with nodes_failed = 0 } in
  check bool "no failure" false
    (CE.should_evaluate ~trigger:CE.OnFailure ~metrics:m2)

(** {1 Build Verification Context} *)

let test_build_verification_context () =
  let n1 = { (CE.empty_node_metrics ~node_id:"fetch" ~node_type:"tool") with
             status = CE.Succeeded; output_preview = Some "data loaded" } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"Load and process data") with
            total_nodes = 1; nodes_succeeded = 1;
            success_rate = 1.0; node_metrics = [n1] } in
  let ctx = CE.build_verification_context ~goal:"Load and process data" ~metrics:m in
  check bool "contains goal" true (String.length ctx > 0);
  check bool "contains Goal section" true
    (try let _ = Str.search_forward (Str.regexp_string "Original Goal") ctx 0 in true
     with Not_found -> false);
  check bool "contains Node Results" true
    (try let _ = Str.search_forward (Str.regexp_string "Node Results") ctx 0 in true
     with Not_found -> false)

(** {1 Parse Verification Response} *)

let test_parse_verification_response_json_block () =
  let response = {|Here is my analysis:
```json
{
  "is_complete": true,
  "confidence": 0.95,
  "reason": "All steps completed",
  "missing_criteria": [],
  "suggested_next_steps": []
}
```|} in
  let r = CE.parse_verification_response response in
  check bool "is_complete" true r.is_complete;
  check (float 0.01) "confidence" 0.95 r.confidence;
  check string "reason" "All steps completed" r.reason

let test_parse_verification_response_json_inline () =
  let response = {|{"is_complete": false, "confidence": 0.3, "reason": "Missing step", "missing_criteria": ["step 3"], "suggested_next_steps": ["retry"]}|} in
  let r = CE.parse_verification_response response in
  check bool "is_complete" false r.is_complete;
  check (float 0.01) "confidence" 0.3 r.confidence;
  check int "missing_criteria" 1 (List.length r.missing_criteria);
  check int "next_steps" 1 (List.length r.suggested_next_steps)

let test_parse_verification_response_text_true () =
  let response = "is_complete: true\nconfidence: 0.9" in
  let r = CE.parse_verification_response response in
  check bool "is_complete true" true r.is_complete

let test_parse_verification_response_text_false () =
  let response = "is_complete: false\nReason: Not done yet" in
  let r = CE.parse_verification_response response in
  check bool "is_complete false" false r.is_complete

let test_parse_verification_response_no_marker () =
  let response = "The chain ran but something unexpected happened." in
  let r = CE.parse_verification_response response in
  check bool "defaults to false" false r.is_complete;
  check (float 0.01) "low confidence" 0.3 r.confidence

let test_parse_verification_response_json_string_bool () =
  let response = {|{"is_complete": "true", "confidence": "0.85", "reason": "done"}|} in
  let r = CE.parse_verification_response response in
  check bool "string true" true r.is_complete;
  check (float 0.01) "string confidence" 0.85 r.confidence

let test_parse_verification_response_json_int_confidence () =
  let response = {|{"is_complete": true, "confidence": 1, "reason": "perfect"}|} in
  let r = CE.parse_verification_response response in
  check (float 0.01) "int confidence" 1.0 r.confidence

let test_parse_verification_response_json_missing_list_as_string () =
  let response = {|{"is_complete": false, "confidence": 0.2, "reason": "missing", "missing_criteria": "step A"}|} in
  let r = CE.parse_verification_response response in
  check int "string as list" 1 (List.length r.missing_criteria)

let test_parse_verification_response_text_complete () =
  let response = "complete: true" in
  let r = CE.parse_verification_response response in
  check bool "complete key" true r.is_complete

let test_parse_verification_response_text_goal_achieved () =
  let response = "goal_achieved: yes" in
  let r = CE.parse_verification_response response in
  check bool "goal_achieved key" true r.is_complete

let test_parse_verification_response_text_markdown () =
  let response = "**is_complete**: true" in
  let r = CE.parse_verification_response response in
  check bool "markdown stripped" true r.is_complete

(** {1 Generate Report} *)

let test_generate_report_empty () =
  let history = CE.{
    chain_id = "test-chain";
    checkpoints = [];
    final_result = None;
  } in
  let report = CE.generate_report ~history in
  check bool "has content" true (String.length report > 0);
  check bool "contains chain_id" true
    (try let _ = Str.search_forward (Str.regexp_string "test-chain") report 0 in true
     with Not_found -> false)

let test_generate_report_with_checkpoints () =
  let m = CE.empty_chain_metrics ~chain_id:"c1" ~goal:"Test goal" in
  let cp = CE.{
    timestamp = 1234567890.0;
    trigger = CE.OnChainComplete;
    metrics_snapshot = m;
    decision = `Complete;
    decision_reason = "All done";
  } in
  let history = CE.{
    chain_id = "c1";
    checkpoints = [cp];
    final_result = Some { m with nodes_succeeded = 3; total_nodes = 3 };
  } in
  let report = CE.generate_report ~history in
  check bool "has content" true (String.length report > 0)

let test_generate_report_various_triggers () =
  let m = CE.empty_chain_metrics ~chain_id:"c1" ~goal:"Test" in
  let make_cp trigger decision =
    CE.{ timestamp = 1.0; trigger; metrics_snapshot = m;
         decision; decision_reason = "reason" }
  in
  let history = CE.{
    chain_id = "c1";
    checkpoints = [
      make_cp (CE.OnNodeComplete "n1") `Continue;
      make_cp (CE.OnGroupComplete ["n1"; "n2"]) `Replan;
      make_cp CE.OnFailure `Abort;
      make_cp CE.OnTimeout `Continue;
      make_cp (CE.Periodic 10) `Complete;
    ];
    final_result = Some m;
  } in
  let report = CE.generate_report ~history in
  check bool "report generated" true (String.length report > 0)

(** {1 Test Suite} *)

let () =
  run "chain_evaluator_coverage" [
    ("empty_metrics", [
      test_case "node" `Quick test_empty_node_metrics;
      test_case "chain" `Quick test_empty_chain_metrics;
    ]);
    ("node_status", [
      test_case "started" `Quick test_mark_node_started;
      test_case "completed" `Quick test_mark_node_completed;
      test_case "completed long preview" `Quick test_mark_node_completed_long_preview;
      test_case "completed no start" `Quick test_mark_node_completed_no_start;
      test_case "failed" `Quick test_mark_node_failed;
      test_case "failed no start" `Quick test_mark_node_failed_no_start;
    ]);
    ("chain_stats", [
      test_case "empty" `Quick test_calculate_chain_stats_empty;
      test_case "mixed" `Quick test_calculate_chain_stats_mixed;
      test_case "all succeed" `Quick test_calculate_chain_stats_all_succeed;
      test_case "with estimates" `Quick test_calculate_chain_stats_with_estimates;
    ]);
    ("should_evaluate", [
      test_case "on_node_complete" `Quick test_should_evaluate_on_node_complete;
      test_case "on_group_complete" `Quick test_should_evaluate_on_group_complete;
      test_case "on_chain_complete" `Quick test_should_evaluate_on_chain_complete;
      test_case "on_failure" `Quick test_should_evaluate_on_failure;
    ]);
    ("verification_context", [
      test_case "build context" `Quick test_build_verification_context;
    ]);
    ("parse_verification", [
      test_case "json block" `Quick test_parse_verification_response_json_block;
      test_case "json inline" `Quick test_parse_verification_response_json_inline;
      test_case "text true" `Quick test_parse_verification_response_text_true;
      test_case "text false" `Quick test_parse_verification_response_text_false;
      test_case "no marker" `Quick test_parse_verification_response_no_marker;
      test_case "json string bool" `Quick test_parse_verification_response_json_string_bool;
      test_case "json int confidence" `Quick test_parse_verification_response_json_int_confidence;
      test_case "json missing as string" `Quick test_parse_verification_response_json_missing_list_as_string;
      test_case "text complete" `Quick test_parse_verification_response_text_complete;
      test_case "text goal_achieved" `Quick test_parse_verification_response_text_goal_achieved;
      test_case "text markdown" `Quick test_parse_verification_response_text_markdown;
    ]);
    ("report", [
      test_case "empty" `Quick test_generate_report_empty;
      test_case "with checkpoints" `Quick test_generate_report_with_checkpoints;
      test_case "various triggers" `Quick test_generate_report_various_triggers;
    ]);
  ]

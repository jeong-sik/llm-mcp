(** Tests for Chain_evaluator module — coverage push for metrics, should_evaluate,
    parse_verification_response, build_verification_context, generate_report *)

open Alcotest
module CE = Chain_evaluator

(* ============================================================
   Helpers
   ============================================================ *)

let make_node ~id ?(status=CE.Pending) ?(duration_ms=0) ?output_preview
    ?error_message ?(node_type="llm") ?started_at () : CE.node_metrics =
  { (CE.empty_node_metrics ~node_id:id ~node_type) with
    status; duration_ms; output_preview; error_message; started_at }

let make_chain_metrics ~goal ?(nodes=[]) ?(nodes_succeeded=0) ?(nodes_failed=0)
    ?(nodes_skipped=0) ?(nodes_pending=0) () : CE.chain_metrics =
  { (CE.empty_chain_metrics ~chain_id:"test" ~goal) with
    node_metrics = nodes;
    total_nodes = List.length nodes;
    nodes_succeeded; nodes_failed; nodes_skipped; nodes_pending }

(* ============================================================
   empty_node_metrics / mark_node_*
   ============================================================ *)

let test_empty_node_metrics () =
  let n = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  check string "id" "n1" n.node_id;
  check string "type" "llm" n.node_type;
  check bool "pending" true (n.status = CE.Pending)

let test_mark_node_started () =
  let n = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let n' = CE.mark_node_started n in
  check bool "running" true (n'.status = CE.Running);
  check bool "has started_at" true (n'.started_at <> None)

let test_mark_node_completed () =
  let n = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let n' = CE.mark_node_completed n ~output_preview:"hello" in
  check bool "succeeded" true (n'.status = CE.Succeeded);
  check (option string) "preview" (Some "hello") n'.output_preview

let test_mark_node_failed () =
  let n = CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm" in
  let n' = CE.mark_node_failed n ~error_message:"boom" in
  check bool "failed" true (n'.status = CE.Failed);
  check (option string) "error" (Some "boom") n'.error_message

(* ============================================================
   calculate_chain_stats
   ============================================================ *)

let test_stats_basic () =
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
    status = CE.Succeeded; duration_ms = 100; estimated_duration_ms = Some 200 } in
  let n2 = { (CE.empty_node_metrics ~node_id:"n2" ~node_type:"tool") with
    status = CE.Failed; duration_ms = 50 } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
    node_metrics = [n1; n2] } in
  let m' = CE.calculate_chain_stats m in
  check int "succeeded" 1 m'.nodes_succeeded;
  check int "failed" 1 m'.nodes_failed;
  check (float 0.01) "accuracy 50%" 0.5 m'.estimation_accuracy

let test_stats_zero_estimate () =
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
    status = CE.Succeeded; duration_ms = 100; estimated_duration_ms = Some 0 } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
    node_metrics = [n1] } in
  let m' = CE.calculate_chain_stats m in
  (* est=0 passes filter but contributes 0 in fold -> 0.0 *)
  check (float 0.01) "accuracy default" 0.0 m'.estimation_accuracy

let test_stats_running_as_pending () =
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
    status = CE.Running } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
    node_metrics = [n1] } in
  let m' = CE.calculate_chain_stats m in
  check int "pending (Running counted)" 1 m'.nodes_pending

let test_stats_retrying () = (* Retrying is not counted as pending *)
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
    status = CE.Retrying } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
    node_metrics = [n1] } in
  let m' = CE.calculate_chain_stats m in
  check int "not pending" 0 m'.nodes_pending

let test_stats_skipped () =
  let n1 = { (CE.empty_node_metrics ~node_id:"n1" ~node_type:"llm") with
    status = CE.Skipped } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c1" ~goal:"test") with
    node_metrics = [n1] } in
  let m' = CE.calculate_chain_stats m in
  check int "skipped" 1 m'.nodes_skipped

(* ============================================================
   should_evaluate
   ============================================================ *)

let test_should_eval_on_node_complete () =
  let n1 = make_node ~id:"n1" ~status:CE.Succeeded () in
  let m = make_chain_metrics ~goal:"test" ~nodes:[n1] ~nodes_succeeded:1 () in
  check bool "should eval" true (CE.should_evaluate ~trigger:(CE.OnNodeComplete "n1") ~metrics:m)

let test_should_eval_on_node_complete_pending () =
  let n1 = make_node ~id:"n1" ~status:CE.Pending () in
  let m = make_chain_metrics ~goal:"test" ~nodes:[n1] () in
  check bool "not yet" false (CE.should_evaluate ~trigger:(CE.OnNodeComplete "n1") ~metrics:m)

let test_should_eval_on_group_complete () =
  let n1 = make_node ~id:"n1" ~status:CE.Succeeded () in
  let n2 = make_node ~id:"n2" ~status:CE.Skipped () in
  let m = make_chain_metrics ~goal:"test" ~nodes:[n1; n2] ~nodes_succeeded:1 ~nodes_skipped:1 () in
  check bool "group done" true (CE.should_evaluate ~trigger:(CE.OnGroupComplete ["n1"; "n2"]) ~metrics:m)

let test_should_eval_on_chain_complete () =
  let m = make_chain_metrics ~goal:"test" ~nodes_pending:0 () in
  check bool "chain done" true (CE.should_evaluate ~trigger:CE.OnChainComplete ~metrics:m)

let test_should_eval_on_chain_complete_pending () =
  let m = make_chain_metrics ~goal:"test" ~nodes_pending:1 () in
  check bool "not done" false (CE.should_evaluate ~trigger:CE.OnChainComplete ~metrics:m)

let test_should_eval_on_failure () =
  let m = make_chain_metrics ~goal:"test" ~nodes_failed:1 () in
  check bool "has failure" true (CE.should_evaluate ~trigger:CE.OnFailure ~metrics:m)

let test_should_eval_on_failure_none () =
  let m = make_chain_metrics ~goal:"test" ~nodes_failed:0 () in
  check bool "no failure" false (CE.should_evaluate ~trigger:CE.OnFailure ~metrics:m)

(* ============================================================
   parse_verification_response
   ============================================================ *)

let test_parse_json_complete () =
  let r = CE.parse_verification_response
    {|```json
{"is_complete": true, "confidence": 0.95, "reason": "all done", "missing_criteria": [], "suggested_next_steps": []}
```|} in
  check bool "complete" true r.is_complete;
  check (float 0.01) "confidence" 0.95 r.confidence;
  check string "reason" "all done" r.reason

let test_parse_json_incomplete () =
  let r = CE.parse_verification_response
    {|{"is_complete": false, "confidence": 0.3, "reason": "needs more", "missing_criteria": ["step2"], "suggested_next_steps": ["do step2"]}|} in
  check bool "incomplete" false r.is_complete;
  check (list string) "missing" ["step2"] r.missing_criteria;
  check (list string) "next" ["do step2"] r.suggested_next_steps

let test_parse_json_string_confidence () =
  let r = CE.parse_verification_response
    {|{"is_complete": true, "confidence": "0.8", "reason": "ok"}|} in
  check bool "complete" true r.is_complete;
  check (float 0.01) "confidence" 0.8 r.confidence

let test_parse_json_int_confidence () =
  let r = CE.parse_verification_response
    {|{"is_complete": false, "confidence": 1, "reason": "ok"}|} in
  check (float 0.01) "confidence" 1.0 r.confidence

let test_parse_json_missing_confidence () =
  let r = CE.parse_verification_response
    {|{"is_complete": true, "reason": "ok"}|} in
  check (float 0.01) "default 0.9" 0.9 r.confidence

let test_parse_json_missing_confidence_incomplete () =
  let r = CE.parse_verification_response
    {|{"is_complete": false, "reason": "no"}|} in
  check (float 0.01) "default 0.3" 0.3 r.confidence

let test_parse_text_is_complete_true () =
  let r = CE.parse_verification_response "is_complete: true\nreason: all good" in
  check bool "complete" true r.is_complete

let test_parse_text_is_complete_false () =
  let r = CE.parse_verification_response "is_complete: false\nreason: not yet" in
  check bool "incomplete" false r.is_complete

let test_parse_text_no_key () =
  let r = CE.parse_verification_response "some random text without structure" in
  check bool "defaults to false" false r.is_complete;
  check (float 0.01) "low confidence" 0.3 r.confidence

let test_parse_json_string_bool () =
  let r = CE.parse_verification_response
    {|{"is_complete": "true", "confidence": 0.7, "reason": "yes"}|} in
  check bool "string true" true r.is_complete

let test_parse_json_single_string_criteria () =
  let r = CE.parse_verification_response
    {|{"is_complete": false, "confidence": 0.4, "reason": "x", "missing_criteria": "single item"}|} in
  check (list string) "single as list" ["single item"] r.missing_criteria

(* ============================================================
   build_verification_context
   ============================================================ *)

let test_build_context_basic () =
  let n1 = make_node ~id:"a" ~status:CE.Succeeded ~output_preview:"good" ~node_type:"llm" () in
  let n2 = make_node ~id:"b" ~status:CE.Failed ~error_message:"timeout" ~node_type:"tool" () in
  let metrics = make_chain_metrics ~goal:"analyze data" ~nodes:[n1; n2]
    ~nodes_succeeded:1 ~nodes_failed:1 () in
  let ctx = CE.build_verification_context ~goal:"analyze data" ~metrics in
  check bool "has goal" true
    (try let _ = Str.search_forward (Str.regexp_string "analyze data") ctx 0 in true
     with Not_found -> false);
  check bool "has succeeded" true
    (try let _ = Str.search_forward (Str.regexp_string "succeeded") ctx 0 in true
     with Not_found -> false);
  check bool "has failed" true
    (try let _ = Str.search_forward (Str.regexp_string "failed") ctx 0 in true
     with Not_found -> false)

(* ============================================================
   generate_report
   ============================================================ *)

let test_report_no_result () =
  let h : CE.evaluation_history = { chain_id = "c1"; checkpoints = []; final_result = None } in
  let report = CE.generate_report ~history:h in
  check bool "has chain id" true
    (try let _ = Str.search_forward (Str.regexp_string "c1") report 0 in true
     with Not_found -> false)

let test_report_with_result () =
  let v : CE.verification_result = {
    is_complete = true; confidence = 0.95; reason = "all done";
    missing_criteria = []; suggested_next_steps = []
  } in
  let m = { (CE.empty_chain_metrics ~chain_id:"c2" ~goal:"test goal") with
    nodes_succeeded = 2; nodes_failed = 0; total_nodes = 2;
    success_rate = 1.0; verification = Some v } in
  let cp : CE.checkpoint = {
    timestamp = 0.0; trigger = CE.OnChainComplete;
    metrics_snapshot = m; decision = `Complete; decision_reason = "done"
  } in
  let h : CE.evaluation_history = { chain_id = "c2"; checkpoints = [cp]; final_result = Some m } in
  let report = CE.generate_report ~history:h in
  check bool "has YES" true
    (try let _ = Str.search_forward (Str.regexp_string "YES") report 0 in true
     with Not_found -> false)

let test_report_abort () =
  let m = CE.empty_chain_metrics ~chain_id:"c3" ~goal:"abort test" in
  let cp : CE.checkpoint = {
    timestamp = 0.0; trigger = CE.OnFailure;
    metrics_snapshot = m; decision = `Abort; decision_reason = "too many errors"
  } in
  let h : CE.evaluation_history = { chain_id = "c3"; checkpoints = [cp]; final_result = Some m } in
  let report = CE.generate_report ~history:h in
  check bool "has Abort" true
    (try let _ = Str.search_forward (Str.regexp_string "Abort") report 0 in true
     with Not_found -> false)

let test_report_replan () =
  let m = CE.empty_chain_metrics ~chain_id:"c4" ~goal:"replan test" in
  let cp : CE.checkpoint = {
    timestamp = 0.0; trigger = (CE.Periodic 30);
    metrics_snapshot = m; decision = `Replan; decision_reason = "slow"
  } in
  let h : CE.evaluation_history = { chain_id = "c4"; checkpoints = [cp]; final_result = Some m } in
  let report = CE.generate_report ~history:h in
  check bool "has Replan" true
    (try let _ = Str.search_forward (Str.regexp_string "Replan") report 0 in true
     with Not_found -> false)

(* ============================================================
   Runner
   ============================================================ *)

let () =
  run "chain_evaluator_coverage2" [
    "node_metrics", [
      test_case "empty" `Quick test_empty_node_metrics;
      test_case "started" `Quick test_mark_node_started;
      test_case "completed" `Quick test_mark_node_completed;
      test_case "failed" `Quick test_mark_node_failed;
    ];
    "chain_stats", [
      test_case "basic" `Quick test_stats_basic;
      test_case "zero estimate" `Quick test_stats_zero_estimate;
      test_case "running as pending" `Quick test_stats_running_as_pending;
      test_case "retrying" `Quick test_stats_retrying;
      test_case "skipped" `Quick test_stats_skipped;
    ];
    "should_evaluate", [
      test_case "on_node_complete" `Quick test_should_eval_on_node_complete;
      test_case "on_node_pending" `Quick test_should_eval_on_node_complete_pending;
      test_case "on_group_complete" `Quick test_should_eval_on_group_complete;
      test_case "on_chain_complete" `Quick test_should_eval_on_chain_complete;
      test_case "on_chain_pending" `Quick test_should_eval_on_chain_complete_pending;
      test_case "on_failure" `Quick test_should_eval_on_failure;
      test_case "on_failure_none" `Quick test_should_eval_on_failure_none;
    ];
    "parse_verification", [
      test_case "json complete" `Quick test_parse_json_complete;
      test_case "json incomplete" `Quick test_parse_json_incomplete;
      test_case "json string confidence" `Quick test_parse_json_string_confidence;
      test_case "json int confidence" `Quick test_parse_json_int_confidence;
      test_case "json missing conf" `Quick test_parse_json_missing_confidence;
      test_case "json missing conf incomplete" `Quick test_parse_json_missing_confidence_incomplete;
      test_case "text is_complete true" `Quick test_parse_text_is_complete_true;
      test_case "text is_complete false" `Quick test_parse_text_is_complete_false;
      test_case "text no key" `Quick test_parse_text_no_key;
      test_case "json string bool" `Quick test_parse_json_string_bool;
      test_case "json single criteria" `Quick test_parse_json_single_string_criteria;
    ];
    "build_context", [
      test_case "basic" `Quick test_build_context_basic;
    ];
    "generate_report", [
      test_case "no result" `Quick test_report_no_result;
      test_case "with result" `Quick test_report_with_result;
      test_case "abort" `Quick test_report_abort;
      test_case "replan" `Quick test_report_replan;
    ];
  ]

(** Tests for Chain_evaluator module

    Pure function tests:
    - node_status: type serialization
    - empty_node_metrics, empty_chain_metrics: constructors
    - calculate_chain_stats: statistics computation
    - should_evaluate: trigger evaluation (non-time-based)
    - build_verification_context: context formatting
    - parse_verification_response: response parsing
    - generate_report: report generation
*)

open Alcotest
open Chain_evaluator

(** {1 Test Helpers} *)

let node_status_testable = testable
  (fun ppf s ->
    let str = match s with
      | Pending -> "Pending"
      | Running -> "Running"
      | Succeeded -> "Succeeded"
      | Failed -> "Failed"
      | Skipped -> "Skipped"
      | Retrying -> "Retrying"
    in
    Fmt.pf ppf "%s" str)
  (fun a b -> a = b)

(** Create test node metrics with specific status *)
let test_node ~id ~status ?(output=None) ?(error=None) () = {
  node_id = id;
  node_type = "llm";
  status;
  started_at = Some 1000.0;
  completed_at = Some 1100.0;
  duration_ms = 100;
  estimated_duration_ms = Some 90;
  retry_count = 0;
  error_message = error;
  output_preview = output;
}

(** Create test chain metrics *)
let test_chain_metrics ~nodes () =
  let base = empty_chain_metrics ~chain_id:"test-chain" ~goal:"Test goal" in
  { base with
    node_metrics = nodes;
    started_at = 1000.0;  (* Fixed timestamp *)
  }

(** {1 node_status Tests} *)

let test_node_status_yojson_roundtrip () =
  let statuses = [Pending; Running; Succeeded; Failed; Skipped; Retrying] in
  List.iter (fun s ->
    let json = node_status_to_yojson s in
    match node_status_of_yojson json with
    | Ok s2 -> check node_status_testable "roundtrip" s s2
    | Error e -> fail e
  ) statuses

(** {1 empty_node_metrics Tests} *)

let test_empty_node_metrics () =
  let m = empty_node_metrics ~node_id:"test" ~node_type:"llm" in
  check string "node_id" "test" m.node_id;
  check string "node_type" "llm" m.node_type;
  check node_status_testable "status pending" Pending m.status;
  check (option (float 0.001)) "no start" None m.started_at;
  check int "no duration" 0 m.duration_ms

(** {1 empty_chain_metrics Tests} *)

let test_empty_chain_metrics () =
  let m = empty_chain_metrics ~chain_id:"my-chain" ~goal:"Solve problem" in
  check string "chain_id" "my-chain" m.chain_id;
  check string "goal" "Solve problem" m.goal;
  check int "total_nodes" 0 m.total_nodes;
  check (float 0.001) "success_rate" 0.0 m.success_rate;
  check (list pass) "empty node_metrics" [] m.node_metrics

(** {1 calculate_chain_stats Tests} *)

let test_calculate_stats_empty () =
  let m = test_chain_metrics ~nodes:[] () in
  let stats = calculate_chain_stats m in
  check int "total" 0 stats.total_nodes;
  check int "succeeded" 0 stats.nodes_succeeded;
  check (float 0.001) "success_rate" 0.0 stats.success_rate

let test_calculate_stats_all_succeed () =
  let nodes = [
    test_node ~id:"a" ~status:Succeeded ~output:(Some "result") ();
    test_node ~id:"b" ~status:Succeeded ~output:(Some "result") ();
    test_node ~id:"c" ~status:Succeeded ~output:(Some "result") ();
  ] in
  let m = test_chain_metrics ~nodes () in
  let stats = calculate_chain_stats m in
  check int "total" 3 stats.total_nodes;
  check int "succeeded" 3 stats.nodes_succeeded;
  check int "failed" 0 stats.nodes_failed;
  check (float 0.01) "success_rate" 1.0 stats.success_rate

let test_calculate_stats_mixed () =
  let nodes = [
    test_node ~id:"a" ~status:Succeeded ~output:(Some "ok") ();
    test_node ~id:"b" ~status:Failed ~error:(Some "error") ();
    test_node ~id:"c" ~status:Skipped ();
    test_node ~id:"d" ~status:Pending ();
  ] in
  let m = test_chain_metrics ~nodes () in
  let stats = calculate_chain_stats m in
  check int "total" 4 stats.total_nodes;
  check int "succeeded" 1 stats.nodes_succeeded;
  check int "failed" 1 stats.nodes_failed;
  check int "skipped" 1 stats.nodes_skipped;
  check int "pending" 1 stats.nodes_pending;
  (* success_rate = 1 / (1 + 1) = 0.5 *)
  check (float 0.01) "success_rate" 0.5 stats.success_rate

(** {1 should_evaluate Tests} *)

let test_should_eval_on_node_complete () =
  let nodes = [
    test_node ~id:"a" ~status:Succeeded ();
    test_node ~id:"b" ~status:Running ();
  ] in
  let m = test_chain_metrics ~nodes () in
  check bool "node a complete" true (should_evaluate ~trigger:(OnNodeComplete "a") ~metrics:m);
  check bool "node b not complete" false (should_evaluate ~trigger:(OnNodeComplete "b") ~metrics:m)

let test_should_eval_on_group_complete () =
  let nodes = [
    test_node ~id:"a" ~status:Succeeded ();
    test_node ~id:"b" ~status:Failed ();
    test_node ~id:"c" ~status:Running ();
  ] in
  let m = test_chain_metrics ~nodes () in
  check bool "group [a,b] complete" true (should_evaluate ~trigger:(OnGroupComplete ["a"; "b"]) ~metrics:m);
  check bool "group [a,c] not complete" false (should_evaluate ~trigger:(OnGroupComplete ["a"; "c"]) ~metrics:m)

let test_should_eval_on_chain_complete () =
  let nodes_done = [
    test_node ~id:"a" ~status:Succeeded ();
    test_node ~id:"b" ~status:Failed ();
  ] in
  let m_done = { (test_chain_metrics ~nodes:nodes_done ()) with nodes_pending = 0 } in
  check bool "chain complete" true (should_evaluate ~trigger:OnChainComplete ~metrics:m_done);

  let nodes_pending = [
    test_node ~id:"a" ~status:Succeeded ();
    test_node ~id:"b" ~status:Running ();
  ] in
  let m_pending = { (test_chain_metrics ~nodes:nodes_pending ()) with nodes_pending = 1 } in
  check bool "chain not complete" false (should_evaluate ~trigger:OnChainComplete ~metrics:m_pending)

let test_should_eval_on_failure () =
  let m_ok = { (test_chain_metrics ~nodes:[] ()) with nodes_failed = 0 } in
  check bool "no failure" false (should_evaluate ~trigger:OnFailure ~metrics:m_ok);

  let m_fail = { (test_chain_metrics ~nodes:[] ()) with nodes_failed = 1 } in
  check bool "has failure" true (should_evaluate ~trigger:OnFailure ~metrics:m_fail)

(** {1 build_verification_context Tests} *)

let test_build_context_basic () =
  let nodes = [test_node ~id:"step1" ~status:Succeeded ~output:(Some "done") ()] in
  let m = { (test_chain_metrics ~nodes ()) with
            total_nodes = 1;
            nodes_succeeded = 1;
            success_rate = 1.0 } in
  let ctx = build_verification_context ~goal:"Test the system" ~metrics:m in
  check bool "contains goal" true (Common.contains ~substring:"Test the system" ctx);
  check bool "contains step1" true (Common.contains ~substring:"step1" ctx);
  check bool "contains succeeded" true (Common.contains ~substring:"succeeded" ctx)

let test_build_context_mixed_status () =
  let nodes = [
    test_node ~id:"a" ~status:Succeeded ~output:(Some "ok") ();
    test_node ~id:"b" ~status:Failed ~error:(Some "err") ();
    test_node ~id:"c" ~status:Skipped ();
  ] in
  let m = { (test_chain_metrics ~nodes ()) with
            total_nodes = 3;
            nodes_succeeded = 1;
            nodes_failed = 1;
            nodes_skipped = 1;
            success_rate = 0.5 } in
  let ctx = build_verification_context ~goal:"Mixed results" ~metrics:m in
  check bool "contains failed" true (Common.contains ~substring:"failed" ctx);
  check bool "contains skipped" true (Common.contains ~substring:"skipped" ctx)

(** {1 parse_verification_response Tests} *)

let test_parse_json_response () =
  let response = {|```json
{
  "is_complete": true,
  "confidence": 0.95,
  "reason": "All tests pass",
  "missing_criteria": [],
  "suggested_next_steps": []
}
```|} in
  let result = parse_verification_response response in
  check bool "is_complete" true result.is_complete;
  check (float 0.01) "confidence" 0.95 result.confidence;
  check string "reason" "All tests pass" result.reason

let test_parse_json_incomplete () =
  let response = {|```json
{
  "is_complete": false,
  "confidence": 0.3,
  "reason": "Tests failing",
  "missing_criteria": ["unit tests", "integration"],
  "suggested_next_steps": ["fix tests"]
}
```|} in
  let result = parse_verification_response response in
  check bool "is_complete false" false result.is_complete;
  check (float 0.01) "confidence" 0.3 result.confidence;
  check int "missing count" 2 (List.length result.missing_criteria);
  check int "steps count" 1 (List.length result.suggested_next_steps)

let test_parse_plain_text_complete () =
  let response = "is_complete: true\nThe goal was achieved successfully." in
  let result = parse_verification_response response in
  check bool "parsed as complete" true result.is_complete

let test_parse_plain_text_incomplete () =
  let response = "is_complete: false\nSome steps are missing." in
  let result = parse_verification_response response in
  check bool "parsed as incomplete" false result.is_complete

let test_parse_no_pattern () =
  let response = "This is a generic response without completion markers." in
  let result = parse_verification_response response in
  check bool "defaults to incomplete" false result.is_complete

(** {1 generate_report Tests} *)

let test_generate_report_basic () =
  let history = {
    chain_id = "test-chain";
    checkpoints = [];
    final_result = Some {
      (empty_chain_metrics ~chain_id:"test-chain" ~goal:"Run tests") with
      total_nodes = 2;
      nodes_succeeded = 2;
      success_rate = 1.0;
      total_duration_ms = 500;
    };
  } in
  let report = generate_report ~history in
  check bool "contains chain id" true (Common.contains ~substring:"test-chain" report);
  check bool "contains duration" true (Common.contains ~substring:"500ms" report)

let test_generate_report_with_checkpoints () =
  let checkpoint = {
    timestamp = 1000.0;
    trigger = OnNodeComplete "step1";
    metrics_snapshot = empty_chain_metrics ~chain_id:"test" ~goal:"test";
    decision = `Continue;
    decision_reason = "Keep going";
  } in
  let history = {
    chain_id = "test-chain";
    checkpoints = [checkpoint];
    final_result = None;
  } in
  let report = generate_report ~history in
  check bool "contains checkpoint" true (Common.contains ~substring:"Node:step1" report);
  check bool "contains decision" true (Common.contains ~substring:"Continue" report)

let test_generate_report_with_verification () =
  let verification = {
    is_complete = true;
    confidence = 0.95;
    reason = "All criteria met";
    missing_criteria = [];
    suggested_next_steps = [];
  } in
  let history = {
    chain_id = "verified-chain";
    checkpoints = [];
    final_result = Some {
      (empty_chain_metrics ~chain_id:"verified-chain" ~goal:"Verify") with
      verification = Some verification;
    };
  } in
  let report = generate_report ~history in
  check bool "contains YES" true (Common.contains ~substring:"YES" report);
  check bool "contains 95%" true (Common.contains ~substring:"95%" report)

(** {1 Test Suite} *)

let status_tests = [
  test_case "yojson roundtrip" `Quick test_node_status_yojson_roundtrip;
]

let constructor_tests = [
  test_case "empty_node_metrics" `Quick test_empty_node_metrics;
  test_case "empty_chain_metrics" `Quick test_empty_chain_metrics;
]

let stats_tests = [
  test_case "empty" `Quick test_calculate_stats_empty;
  test_case "all succeed" `Quick test_calculate_stats_all_succeed;
  test_case "mixed" `Quick test_calculate_stats_mixed;
]

let should_eval_tests = [
  test_case "OnNodeComplete" `Quick test_should_eval_on_node_complete;
  test_case "OnGroupComplete" `Quick test_should_eval_on_group_complete;
  test_case "OnChainComplete" `Quick test_should_eval_on_chain_complete;
  test_case "OnFailure" `Quick test_should_eval_on_failure;
]

let context_tests = [
  test_case "basic" `Quick test_build_context_basic;
  test_case "mixed status" `Quick test_build_context_mixed_status;
]

let parse_tests = [
  test_case "json complete" `Quick test_parse_json_response;
  test_case "json incomplete" `Quick test_parse_json_incomplete;
  test_case "plain text complete" `Quick test_parse_plain_text_complete;
  test_case "plain text incomplete" `Quick test_parse_plain_text_incomplete;
  test_case "no pattern" `Quick test_parse_no_pattern;
]

let report_tests = [
  test_case "basic" `Quick test_generate_report_basic;
  test_case "with checkpoints" `Quick test_generate_report_with_checkpoints;
  test_case "with verification" `Quick test_generate_report_with_verification;
]

let () =
  run "chain_evaluator" [
    ("node_status", status_tests);
    ("constructors", constructor_tests);
    ("calculate_chain_stats", stats_tests);
    ("should_evaluate", should_eval_tests);
    ("build_verification_context", context_tests);
    ("parse_verification_response", parse_tests);
    ("generate_report", report_tests);
  ]

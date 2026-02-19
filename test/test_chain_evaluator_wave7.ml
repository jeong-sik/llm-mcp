(** Wave 7: Additional chain_evaluator coverage tests.
    Focus on parse_verification_response edge cases,
    generate_report, build_verification_context, should_evaluate. *)

open Alcotest
open Chain_evaluator

let check_str = check string
let check_int = check int
let check_float msg expected actual =
  check (float 0.01) msg expected actual
let check_bool = check bool

(* ---- Helper: make node metrics with status ---- *)
let nm ?(status=Succeeded) ?(started_at=Some 1000.0) ?(completed_at=Some 1001.0)
    ?(output_preview=None) ?(error_message=None) ?(estimated_duration_ms=None)
    id node_type =
  { node_id = id; node_type; status; started_at; completed_at;
    duration_ms = 100; estimated_duration_ms; retry_count = 0;
    error_message; output_preview }

(* ---- empty_node_metrics ---- *)
let test_empty_node_metrics () =
  let m = empty_node_metrics ~node_id:"x" ~node_type:"llm" in
  check_str "id" "x" m.node_id;
  check_str "type" "llm" m.node_type;
  check_int "duration" 0 m.duration_ms;
  (match m.status with Pending -> () | _ -> fail "expected Pending")

(* ---- empty_chain_metrics ---- *)
let test_empty_chain_metrics () =
  let m = empty_chain_metrics ~chain_id:"c1" ~goal:"do stuff" in
  check_str "chain_id" "c1" m.chain_id;
  check_str "goal" "do stuff" m.goal;
  check_int "total_nodes" 0 m.total_nodes

(* ---- mark_node_started ---- *)
let test_mark_node_started () =
  let m = empty_node_metrics ~node_id:"n" ~node_type:"llm" in
  let m2 = mark_node_started m in
  (match m2.status with Running -> () | _ -> fail "expected Running");
  check_bool "started_at set" true (Option.is_some m2.started_at)

(* ---- mark_node_completed ---- *)
let test_mark_node_completed_long_preview () =
  let m = mark_node_started (empty_node_metrics ~node_id:"n" ~node_type:"llm") in
  let long_output = String.make 300 'x' in
  let m2 = mark_node_completed m ~output_preview:long_output in
  (match m2.status with Succeeded -> () | _ -> fail "expected Succeeded");
  check_bool "preview truncated" true
    (match m2.output_preview with Some s -> String.length s <= 200 | None -> false)

let test_mark_node_completed_no_start () =
  let m = empty_node_metrics ~node_id:"n" ~node_type:"llm" in
  let m2 = mark_node_completed m ~output_preview:"ok" in
  check_int "duration 0 when no start" 0 m2.duration_ms

(* ---- mark_node_failed ---- *)
let test_mark_node_failed () =
  let m = mark_node_started (empty_node_metrics ~node_id:"n" ~node_type:"llm") in
  let m2 = mark_node_failed m ~error_message:"timeout" in
  (match m2.status with Failed -> () | _ -> fail "expected Failed");
  check_str "error" "timeout" (match m2.error_message with Some s -> s | None -> "")

let test_mark_node_failed_no_start () =
  let m = empty_node_metrics ~node_id:"n" ~node_type:"llm" in
  let m2 = mark_node_failed m ~error_message:"err" in
  check_int "duration 0" 0 m2.duration_ms

(* ---- calculate_chain_stats ---- *)
let test_calc_stats_estimation_accuracy () =
  let nodes = [
    { (nm "a" "llm") with duration_ms = 100; estimated_duration_ms = Some 100 };
    { (nm "b" "tool") with duration_ms = 200; estimated_duration_ms = Some 100 };
  ] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with node_metrics = nodes } in
  let cm2 = calculate_chain_stats cm in
  check_int "total" 2 cm2.total_nodes;
  check_int "succeeded" 2 cm2.nodes_succeeded;
  check_float "success_rate" 1.0 cm2.success_rate;
  (* accuracy: node a = min(100,100)/max(100,100) = 1.0, node b = 100/200 = 0.5 -> avg = 0.75 *)
  check_float "estimation_accuracy" 0.75 cm2.estimation_accuracy

let test_calc_stats_mixed_status () =
  let nodes = [
    nm "a" "llm" ~status:Succeeded;
    nm "b" "tool" ~status:Failed ~error_message:(Some "err");
    nm "c" "llm" ~status:Skipped;
    nm "d" "llm" ~status:Running;
    nm "e" "llm" ~status:Retrying;
  ] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with node_metrics = nodes } in
  let cm2 = calculate_chain_stats cm in
  check_int "total" 5 cm2.total_nodes;
  check_int "succeeded" 1 cm2.nodes_succeeded;
  check_int "failed" 1 cm2.nodes_failed;
  check_int "skipped" 1 cm2.nodes_skipped;
  check_int "pending" 1 cm2.nodes_pending;
  check_float "success_rate" 0.5 cm2.success_rate

let test_calc_stats_zero_est () =
  let nodes = [
    { (nm "a" "llm") with duration_ms = 100; estimated_duration_ms = Some 0 };
  ] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with node_metrics = nodes } in
  let cm2 = calculate_chain_stats cm in
  (* est = 0 passes filter (Some 0 <> None), but then when est > 0 fails -> acc stays 0 *)
  check_float "accuracy default" 0.0 cm2.estimation_accuracy

(* ---- should_evaluate ---- *)
let test_should_eval_on_group_partial () =
  let nodes = [
    nm "a" "llm" ~status:Succeeded;
    nm "b" "tool" ~status:Pending;
  ] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with node_metrics = nodes } in
  check_bool "group not complete" false
    (should_evaluate ~trigger:(OnGroupComplete ["a"; "b"]) ~metrics:cm)

let test_should_eval_on_group_with_skipped () =
  let nodes = [
    nm "a" "llm" ~status:Succeeded;
    nm "b" "tool" ~status:Skipped;
  ] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with node_metrics = nodes } in
  check_bool "group complete with skip" true
    (should_evaluate ~trigger:(OnGroupComplete ["a"; "b"]) ~metrics:cm)

let test_should_eval_periodic () =
  let now = Unix.gettimeofday () in
  (* Periodic checks if elapsed mod interval = 0 *)
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with started_at = now } in
  (* elapsed ~ 0.0, 0 mod 10 = 0 -> true *)
  let result = should_evaluate ~trigger:(Periodic 10) ~metrics:cm in
  check_bool "periodic at start" true result

let test_should_eval_on_timeout_no_running () =
  let nodes = [nm "a" "llm" ~status:Succeeded] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with node_metrics = nodes } in
  check_bool "no running -> no timeout" false
    (should_evaluate ~trigger:OnTimeout ~metrics:cm)

let test_should_eval_on_timeout_not_expired () =
  let now = Unix.gettimeofday () in
  let nodes = [nm "a" "llm" ~status:Running ~started_at:(Some now)] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"g") with node_metrics = nodes } in
  check_bool "just started -> no timeout" false
    (should_evaluate ~trigger:OnTimeout ~metrics:cm)

(* ---- build_verification_context ---- *)
let test_build_context_with_outputs () =
  let nodes = [
    nm "a" "llm" ~status:Succeeded ~output_preview:(Some "result");
    nm "b" "tool" ~status:Failed ~error_message:(Some "err");
    nm "c" "llm" ~status:Running;
    nm "d" "llm" ~status:Skipped;
    nm "e" "llm" ~status:Retrying;
  ] in
  let cm = { (empty_chain_metrics ~chain_id:"c" ~goal:"test goal") with
    node_metrics = nodes; total_nodes = 5; nodes_succeeded = 1; nodes_failed = 1;
    nodes_skipped = 1; success_rate = 0.5; total_duration_ms = 500 } in
  let ctx = build_verification_context ~goal:"test goal" ~metrics:cm in
  check_bool "has goal" true (Common.contains ~substring:"test goal" ctx);
  check_bool "has succeeded" true (Common.contains ~substring:"succeeded" ctx);
  check_bool "has failed" true (Common.contains ~substring:"failed" ctx);
  check_bool "has running" true (Common.contains ~substring:"running" ctx);
  check_bool "has skipped" true (Common.contains ~substring:"skipped" ctx);
  check_bool "has retrying" true (Common.contains ~substring:"retrying" ctx);
  check_bool "has output preview" true (Common.contains ~substring:"result" ctx)

(* ---- parse_verification_response ---- *)
let test_parse_json_code_block () =
  let resp = {|Some text before.
```json
{
  "is_complete": true,
  "confidence": 0.95,
  "reason": "All steps passed",
  "missing_criteria": [],
  "suggested_next_steps": ["deploy"]
}
```
Some text after.|} in
  let r = parse_verification_response resp in
  check_bool "is_complete" true r.is_complete;
  check_float "confidence" 0.95 r.confidence;
  check_str "reason" "All steps passed" r.reason;
  check_int "next_steps" 1 (List.length r.suggested_next_steps)

let test_parse_json_bare_object () =
  let resp = {|The analysis shows:
{"is_complete": false, "confidence": 0.3, "reason": "Missing step B", "missing_criteria": ["step B"], "suggested_next_steps": ["run B", "verify"]}|} in
  let r = parse_verification_response resp in
  check_bool "is_complete" false r.is_complete;
  check_float "confidence" 0.3 r.confidence;
  check_int "missing_criteria" 1 (List.length r.missing_criteria);
  check_int "next_steps" 2 (List.length r.suggested_next_steps)

let test_parse_json_string_confidence () =
  let resp = {|```json
{"is_complete": true, "confidence": "0.85", "reason": "Good"}
```|} in
  let r = parse_verification_response resp in
  check_bool "complete" true r.is_complete;
  check_float "confidence from string" 0.85 r.confidence

let test_parse_json_int_confidence () =
  let resp = {|```json
{"is_complete": false, "confidence": 1, "reason": "Done"}
```|} in
  let r = parse_verification_response resp in
  check_float "confidence from int" 1.0 r.confidence

let test_parse_json_is_complete_string () =
  let resp = {|```json
{"is_complete": "true", "reason": "All done"}
```|} in
  let r = parse_verification_response resp in
  check_bool "is_complete string true" true r.is_complete

let test_parse_json_missing_confidence_complete () =
  let resp = {|```json
{"is_complete": true, "reason": "Ok"}
```|} in
  let r = parse_verification_response resp in
  check_float "default confidence for complete" 0.9 r.confidence

let test_parse_json_missing_confidence_incomplete () =
  let resp = {|```json
{"is_complete": false, "reason": "Not done"}
```|} in
  let r = parse_verification_response resp in
  check_float "default confidence for incomplete" 0.3 r.confidence

let test_parse_json_single_string_criteria () =
  let resp = {|```json
{"is_complete": false, "confidence": 0.4, "reason": "X", "missing_criteria": "step A"}
```|} in
  let r = parse_verification_response resp in
  check_int "single string as list" 1 (List.length r.missing_criteria);
  check_str "criteria value" "step A" (List.hd r.missing_criteria)

let test_parse_text_complete_true () =
  let resp = "is_complete: true\nconfidence: 0.9\nreason: All passed" in
  let r = parse_verification_response resp in
  check_bool "text true" true r.is_complete

let test_parse_text_complete_yes () =
  let resp = "**completed**: yes" in
  let r = parse_verification_response resp in
  check_bool "text yes" true r.is_complete

let test_parse_text_goal_achieved () =
  let resp = "goal_achieved: true\neverything worked" in
  let r = parse_verification_response resp in
  check_bool "goal_achieved" true r.is_complete

let test_parse_text_achieved () =
  let resp = "achieved: true" in
  let r = parse_verification_response resp in
  check_bool "achieved" true r.is_complete

let test_parse_text_result_true () =
  let resp = "result: true" in
  let r = parse_verification_response resp in
  check_bool "result true" true r.is_complete

let test_parse_text_complete_false () =
  let resp = "is_complete: false" in
  let r = parse_verification_response resp in
  check_bool "text false" false r.is_complete

let test_parse_text_no_key () =
  let resp = "The task was partially done but not finished." in
  let r = parse_verification_response resp in
  check_bool "no key -> false" false r.is_complete;
  check_float "default conf" 0.3 r.confidence

let test_parse_text_with_markdown () =
  let resp = "**is_complete**: true" in
  let r = parse_verification_response resp in
  check_bool "markdown stripped" true r.is_complete

let test_parse_text_comma_trailing () =
  let resp = "is_complete: true," in
  let r = parse_verification_response resp in
  check_bool "trailing comma" true r.is_complete

let test_parse_text_quoted_value () =
  let resp = {|is_complete: "true"|} in
  let r = parse_verification_response resp in
  check_bool "quoted true" true r.is_complete

let test_parse_json_invalid_json () =
  (* Invalid JSON but looks like it has { } *)
  let resp = {|```json
{invalid json here}
```|} in
  let r = parse_verification_response resp in
  (* Falls through to text parsing *)
  check_bool "fallback" false r.is_complete

let test_parse_empty_response () =
  let r = parse_verification_response "" in
  check_bool "empty -> false" false r.is_complete

(* ---- generate_report ---- *)
let test_report_no_checkpoints () =
  let h = { chain_id = "c1"; checkpoints = []; final_result = None } in
  let r = generate_report ~history:h in
  check_bool "has header" true (Common.contains ~substring:"EVALUATION REPORT" r);
  check_bool "not verified" true (Common.contains ~substring:"Not yet verified" r)

let test_report_with_verification () =
  let ver = {
    is_complete = true; confidence = 0.95;
    reason = "All passed"; missing_criteria = []; suggested_next_steps = []
  } in
  let cm = { (empty_chain_metrics ~chain_id:"c1" ~goal:"build it") with
    total_nodes = 3; nodes_succeeded = 3; success_rate = 1.0;
    total_duration_ms = 500; estimation_accuracy = 0.9;
    verification = Some ver } in
  let h = { chain_id = "c1"; checkpoints = []; final_result = Some cm } in
  let r = generate_report ~history:h in
  check_bool "has YES" true (Common.contains ~substring:"YES" r);
  check_bool "has 95%" true (Common.contains ~substring:"95%" r)

let test_report_with_checkpoints () =
  let cp1 = {
    timestamp = 1000.0; trigger = OnNodeComplete "a";
    metrics_snapshot = empty_chain_metrics ~chain_id:"c1" ~goal:"g";
    decision = `Continue; decision_reason = "still going"
  } in
  let cp2 = {
    timestamp = 1001.0; trigger = OnFailure;
    metrics_snapshot = empty_chain_metrics ~chain_id:"c1" ~goal:"g";
    decision = `Replan; decision_reason = "node failed"
  } in
  let cp3 = {
    timestamp = 1002.0; trigger = OnTimeout;
    metrics_snapshot = empty_chain_metrics ~chain_id:"c1" ~goal:"g";
    decision = `Abort; decision_reason = "timed out"
  } in
  let cp4 = {
    timestamp = 1003.0; trigger = OnChainComplete;
    metrics_snapshot = empty_chain_metrics ~chain_id:"c1" ~goal:"g";
    decision = `Complete; decision_reason = "done"
  } in
  let cp5 = {
    timestamp = 1004.0; trigger = Periodic 10;
    metrics_snapshot = empty_chain_metrics ~chain_id:"c1" ~goal:"g";
    decision = `Continue; decision_reason = "periodic check"
  } in
  let cp6 = {
    timestamp = 1005.0; trigger = OnGroupComplete ["a"; "b"];
    metrics_snapshot = empty_chain_metrics ~chain_id:"c1" ~goal:"g";
    decision = `Continue; decision_reason = "group done"
  } in
  let h = { chain_id = "c1"; checkpoints = [cp1; cp2; cp3; cp4; cp5; cp6]; final_result = None } in
  let r = generate_report ~history:h in
  check_bool "has Node:a" true (Common.contains ~substring:"Node:a" r);
  check_bool "has Failure" true (Common.contains ~substring:"Failure" r);
  check_bool "has Timeout" true (Common.contains ~substring:"Timeout" r);
  check_bool "has Continue" true (Common.contains ~substring:"Continue" r);
  check_bool "has Replan" true (Common.contains ~substring:"Replan" r);
  check_bool "has Abort" true (Common.contains ~substring:"Abort" r);
  check_bool "has Complete" true (Common.contains ~substring:"Complete" r);
  check_bool "has Periodic" true (Common.contains ~substring:"Periodic" r);
  check_bool "has Group" true (Common.contains ~substring:"Group:2" r)

(* ---- yojson roundtrips ---- *)
let test_node_status_yojson () =
  let variants = [Pending; Running; Succeeded; Failed; Skipped; Retrying] in
  List.iter (fun v ->
    let j = node_status_to_yojson v in
    let v2 = node_status_of_yojson j |> Result.get_ok in
    let j2 = node_status_to_yojson v2 in
    check_str "roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

let test_eval_trigger_yojson () =
  let variants = [
    OnNodeComplete "a";
    OnGroupComplete ["a"; "b"];
    OnChainComplete;
    OnFailure;
    OnTimeout;
    Periodic 10;
  ] in
  List.iter (fun v ->
    let j = eval_trigger_to_yojson v in
    let v2 = eval_trigger_of_yojson j |> Result.get_ok in
    let j2 = eval_trigger_to_yojson v2 in
    check_str "trigger roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

let test_verification_result_yojson () =
  let v = {
    is_complete = true; confidence = 0.9;
    reason = "all good"; missing_criteria = ["x"];
    suggested_next_steps = ["deploy"]
  } in
  let j = verification_result_to_yojson v in
  let v2 = verification_result_of_yojson j |> Result.get_ok in
  check_bool "complete" v.is_complete v2.is_complete;
  check_float "confidence" v.confidence v2.confidence

let test_node_metrics_yojson () =
  let m = nm "test" "llm" ~output_preview:(Some "out") in
  let j = node_metrics_to_yojson m in
  let m2 = node_metrics_of_yojson j |> Result.get_ok in
  check_str "id" m.node_id m2.node_id

let test_chain_metrics_yojson () =
  let cm = empty_chain_metrics ~chain_id:"c" ~goal:"g" in
  let j = chain_metrics_to_yojson cm in
  let cm2 = chain_metrics_of_yojson j |> Result.get_ok in
  check_str "chain_id" cm.chain_id cm2.chain_id

let test_checkpoint_yojson () =
  let cp = {
    timestamp = 1000.0; trigger = OnChainComplete;
    metrics_snapshot = empty_chain_metrics ~chain_id:"c" ~goal:"g";
    decision = `Complete; decision_reason = "done"
  } in
  let j = checkpoint_to_yojson cp in
  let cp2 = checkpoint_of_yojson j |> Result.get_ok in
  check_str "reason" cp.decision_reason cp2.decision_reason

let test_evaluation_history_yojson () =
  let h = { chain_id = "c1"; checkpoints = []; final_result = None } in
  let j = evaluation_history_to_yojson h in
  let h2 = evaluation_history_of_yojson j |> Result.get_ok in
  check_str "chain_id" h.chain_id h2.chain_id

let () =
  run "chain_evaluator_wave7" [
    "metrics", [
      test_case "empty_node" `Quick test_empty_node_metrics;
      test_case "empty_chain" `Quick test_empty_chain_metrics;
      test_case "mark_started" `Quick test_mark_node_started;
      test_case "mark_completed long" `Quick test_mark_node_completed_long_preview;
      test_case "mark_completed no start" `Quick test_mark_node_completed_no_start;
      test_case "mark_failed" `Quick test_mark_node_failed;
      test_case "mark_failed no start" `Quick test_mark_node_failed_no_start;
    ];
    "calc_stats", [
      test_case "estimation accuracy" `Quick test_calc_stats_estimation_accuracy;
      test_case "mixed status" `Quick test_calc_stats_mixed_status;
      test_case "zero est" `Quick test_calc_stats_zero_est;
    ];
    "should_eval", [
      test_case "group partial" `Quick test_should_eval_on_group_partial;
      test_case "group with skip" `Quick test_should_eval_on_group_with_skipped;
      test_case "periodic" `Quick test_should_eval_periodic;
      test_case "timeout no running" `Quick test_should_eval_on_timeout_no_running;
      test_case "timeout not expired" `Quick test_should_eval_on_timeout_not_expired;
    ];
    "build_context", [
      test_case "with outputs" `Quick test_build_context_with_outputs;
    ];
    "parse_verification", [
      test_case "json code block" `Quick test_parse_json_code_block;
      test_case "json bare" `Quick test_parse_json_bare_object;
      test_case "json string confidence" `Quick test_parse_json_string_confidence;
      test_case "json int confidence" `Quick test_parse_json_int_confidence;
      test_case "json string is_complete" `Quick test_parse_json_is_complete_string;
      test_case "json missing conf complete" `Quick test_parse_json_missing_confidence_complete;
      test_case "json missing conf incomplete" `Quick test_parse_json_missing_confidence_incomplete;
      test_case "json single string criteria" `Quick test_parse_json_single_string_criteria;
      test_case "text complete true" `Quick test_parse_text_complete_true;
      test_case "text complete yes" `Quick test_parse_text_complete_yes;
      test_case "text goal_achieved" `Quick test_parse_text_goal_achieved;
      test_case "text achieved" `Quick test_parse_text_achieved;
      test_case "text result" `Quick test_parse_text_result_true;
      test_case "text false" `Quick test_parse_text_complete_false;
      test_case "text no key" `Quick test_parse_text_no_key;
      test_case "text markdown" `Quick test_parse_text_with_markdown;
      test_case "text trailing comma" `Quick test_parse_text_comma_trailing;
      test_case "text quoted" `Quick test_parse_text_quoted_value;
      test_case "invalid json" `Quick test_parse_json_invalid_json;
      test_case "empty" `Quick test_parse_empty_response;
    ];
    "generate_report", [
      test_case "no checkpoints" `Quick test_report_no_checkpoints;
      test_case "with verification" `Quick test_report_with_verification;
      test_case "with checkpoints" `Quick test_report_with_checkpoints;
    ];
    "yojson", [
      test_case "node_status" `Quick test_node_status_yojson;
      test_case "eval_trigger" `Quick test_eval_trigger_yojson;
      test_case "verification_result" `Quick test_verification_result_yojson;
      test_case "node_metrics" `Quick test_node_metrics_yojson;
      test_case "chain_metrics" `Quick test_chain_metrics_yojson;
      test_case "checkpoint" `Quick test_checkpoint_yojson;
      test_case "evaluation_history" `Quick test_evaluation_history_yojson;
    ];
  ]

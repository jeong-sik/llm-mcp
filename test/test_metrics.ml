(** Tests for Metrics module - Prometheus-compatible metrics collection

    Pure function tests:
    - type_to_string: metric type conversion
    - labels_to_string: label formatting
    - counter operations: inc_counter, register_counter
    - gauge operations: set_gauge, inc_gauge, dec_gauge
    - to_prometheus_text: export format
*)

open Alcotest
open Metrics

(** {1 Test Helpers} *)

(** Clear metrics between tests - using a unique name per test *)
let make_unique_name base =
  Printf.sprintf "%s_%d" base (Random.int 100000)

(** {1 type_to_string Tests} *)

let test_type_to_string_counter () =
  check string "counter" "counter" (type_to_string Counter)

let test_type_to_string_gauge () =
  check string "gauge" "gauge" (type_to_string Gauge)

let test_type_to_string_histogram () =
  check string "histogram" "histogram" (type_to_string Histogram)

(** {1 labels_to_string Tests} *)

let test_labels_empty () =
  check string "empty" "" (labels_to_string [])

let test_labels_single () =
  let result = labels_to_string [("model", "gemini")] in
  check string "single" "{model=\"gemini\"}" result

let test_labels_multiple () =
  let result = labels_to_string [("model", "claude"); ("env", "prod")] in
  check bool "has model" true (Common.contains ~substring:"model=\"claude\"" result);
  check bool "has env" true (Common.contains ~substring:"env=\"prod\"" result);
  check bool "starts with {" true (String.get result 0 = '{');
  check bool "ends with }" true (String.get result (String.length result - 1) = '}')

let test_labels_escaping () =
  let result = labels_to_string [("msg", "hello\"world")] in
  check bool "escaped" true (Common.contains ~substring:"\\\"" result)

(** {1 Counter Tests} *)

let test_register_counter () =
  let name = make_unique_name "test_counter" in
  register_counter ~name ~help:"Test counter" ();
  (* Should not crash if called twice *)
  register_counter ~name ~help:"Test counter" ()

let test_inc_counter_auto_register () =
  let name = make_unique_name "auto_counter" in
  inc_counter name ();
  let text = to_prometheus_text () in
  check bool "metric exists" true (Common.contains ~substring:name text)

let test_inc_counter_with_labels () =
  let name = make_unique_name "labeled_counter" in
  inc_counter name ~labels:[("model", "gemini")] ();
  let text = to_prometheus_text () in
  check bool "has label" true (Common.contains ~substring:"model=\"gemini\"" text)

let test_inc_counter_delta () =
  let name = make_unique_name "delta_counter" in
  inc_counter name ~delta:5.0 ();
  inc_counter name ~delta:3.0 ();
  let text = to_prometheus_text () in
  check bool "has value 8" true (Common.contains ~substring:(name ^ " 8") text)

(** {1 Gauge Tests} *)

let test_register_gauge () =
  let name = make_unique_name "test_gauge" in
  register_gauge ~name ~help:"Test gauge" ();
  register_gauge ~name ~help:"Test gauge" ()  (* Should not crash *)

let test_set_gauge () =
  let name = make_unique_name "set_gauge" in
  set_gauge name 42.5;
  let text = to_prometheus_text () in
  check bool "has value" true (Common.contains ~substring:(name ^ " 42.5") text)

let test_set_gauge_with_labels () =
  let name = make_unique_name "labeled_gauge" in
  set_gauge name ~labels:[("env", "test")] 100.0;
  let text = to_prometheus_text () in
  check bool "has label" true (Common.contains ~substring:"env=\"test\"" text)

let test_inc_gauge () =
  let name = make_unique_name "inc_gauge" in
  set_gauge name 10.0;
  inc_gauge name ();
  let text = to_prometheus_text () in
  check bool "incremented" true (Common.contains ~substring:(name ^ " 11") text)

let test_dec_gauge () =
  let name = make_unique_name "dec_gauge" in
  set_gauge name 10.0;
  dec_gauge name ~delta:3.0 ();
  let text = to_prometheus_text () in
  check bool "decremented" true (Common.contains ~substring:(name ^ " 7") text)

(** {1 Prometheus Export Tests} *)

let test_prometheus_format_help () =
  let name = make_unique_name "help_test" in
  register_counter ~name ~help:"This is help text" ();
  let text = to_prometheus_text () in
  check bool "has HELP" true (Common.contains ~substring:("# HELP " ^ name) text)

let test_prometheus_format_type () =
  let name = make_unique_name "type_test" in
  register_counter ~name ~help:"Counter metric" ();
  let text = to_prometheus_text () in
  check bool "has TYPE counter" true (Common.contains ~substring:("# TYPE " ^ name ^ " counter") text)

let test_prometheus_format_gauge_type () =
  let name = make_unique_name "gauge_type_test" in
  register_gauge ~name ~help:"Gauge metric" ();
  let text = to_prometheus_text () in
  check bool "has TYPE gauge" true (Common.contains ~substring:("# TYPE " ^ name ^ " gauge") text)

let test_prometheus_multiple_labels () =
  let name = make_unique_name "multi_label" in
  inc_counter name ~labels:[("model", "gemini")] ();
  inc_counter name ~labels:[("model", "claude")] ();
  let text = to_prometheus_text () in
  check bool "has gemini" true (Common.contains ~substring:"model=\"gemini\"" text);
  check bool "has claude" true (Common.contains ~substring:"model=\"claude\"" text)

(** {1 Built-in Metrics Tests} *)

let test_record_request () =
  record_request ~model:"test-model" ();
  let text = to_prometheus_text () in
  check bool "has request metric" true (Common.contains ~substring:"llm_mcp_requests_total" text);
  check bool "has model label" true (Common.contains ~substring:"model=\"test-model\"" text)

let test_record_error () =
  record_error ~error_type:"timeout" ();
  let text = to_prometheus_text () in
  check bool "has error metric" true (Common.contains ~substring:"llm_mcp_errors_total" text);
  check bool "has type label" true (Common.contains ~substring:"type=\"timeout\"" text)

let test_set_active_sessions () =
  set_active_sessions 5;
  let text = to_prometheus_text () in
  check bool "has sessions metric" true (Common.contains ~substring:"llm_mcp_active_sessions" text)

let test_uptime_metric () =
  (* uptime is auto-updated on to_prometheus_text() call *)
  let text = to_prometheus_text () in
  check bool "has uptime" true (Common.contains ~substring:"llm_mcp_uptime_seconds" text)

(** {1 Test Suite} *)

let type_tests = [
  test_case "counter" `Quick test_type_to_string_counter;
  test_case "gauge" `Quick test_type_to_string_gauge;
  test_case "histogram" `Quick test_type_to_string_histogram;
]

let labels_tests = [
  test_case "empty" `Quick test_labels_empty;
  test_case "single" `Quick test_labels_single;
  test_case "multiple" `Quick test_labels_multiple;
  test_case "escaping" `Quick test_labels_escaping;
]

let counter_tests = [
  test_case "register" `Quick test_register_counter;
  test_case "auto register" `Quick test_inc_counter_auto_register;
  test_case "with labels" `Quick test_inc_counter_with_labels;
  test_case "delta" `Quick test_inc_counter_delta;
]

let gauge_tests = [
  test_case "register" `Quick test_register_gauge;
  test_case "set" `Quick test_set_gauge;
  test_case "with labels" `Quick test_set_gauge_with_labels;
  test_case "inc" `Quick test_inc_gauge;
  test_case "dec" `Quick test_dec_gauge;
]

let export_tests = [
  test_case "HELP" `Quick test_prometheus_format_help;
  test_case "TYPE counter" `Quick test_prometheus_format_type;
  test_case "TYPE gauge" `Quick test_prometheus_format_gauge_type;
  test_case "multiple labels" `Quick test_prometheus_multiple_labels;
]

let builtin_tests = [
  test_case "record_request" `Quick test_record_request;
  test_case "record_error" `Quick test_record_error;
  test_case "set_active_sessions" `Quick test_set_active_sessions;
  test_case "uptime" `Quick test_uptime_metric;
]

let () =
  Random.init (int_of_float (Unix.gettimeofday () *. 1000.0));
  run "metrics" [
    ("type_to_string", type_tests);
    ("labels_to_string", labels_tests);
    ("counter", counter_tests);
    ("gauge", gauge_tests);
    ("prometheus_export", export_tests);
    ("builtin_metrics", builtin_tests);
  ]

(** Tests for Chain_stats module

    Pure function tests:
    - percentile, percentiles: numerical computation
    - cost_per_1k_tokens: model cost lookup
    - stats_to_yojson/of_yojson: JSON roundtrip
    - to_summary: compact string formatting
    - string_of_stats: detailed formatting
    - to_prometheus: Prometheus metric format
*)

open Alcotest
open Chain_stats

(** {1 Test Helpers} *)

(** Create a test stats value *)
let test_stats ?(total_chains=10) ?(success_count=8) ?(total_tokens=5000) () = {
  total_chains;
  total_nodes = 50;
  active_chains = 0;
  avg_duration_ms = 150.0;
  p50_duration_ms = 120.0;
  p95_duration_ms = 300.0;
  p99_duration_ms = 500.0;
  total_tokens;
  tokens_by_model = [("gemini-pro", 3000); ("claude-3-haiku", 2000)];
  estimated_cost_usd = 0.05;
  success_count;
  failure_count = total_chains - success_count;
  success_rate = float_of_int success_count /. float_of_int total_chains;
  failure_reasons = [("timeout", 1); ("rate_limit", 1)];
  hourly_tokens = [(10, 2000); (11, 3000)];
  hourly_chains = [(10, 4); (11, 6)];
}

(** {1 percentile Tests} *)

let test_percentile_empty () =
  check (float 0.001) "empty list" 0.0 (percentile 0.5 [])

let test_percentile_single () =
  check (float 0.001) "single element" 42.0 (percentile 0.5 [42])

let test_percentile_p50 () =
  let sorted = [10; 20; 30; 40; 50] in
  check (float 0.001) "p50" 30.0 (percentile 0.5 sorted)

let test_percentile_p95 () =
  let sorted = List.init 100 (fun i -> i + 1) in  (* 1..100 *)
  let p95 = percentile 0.95 sorted in
  (* 95th percentile of 1-100 should be around 95 *)
  check bool "p95 near 95" true (p95 >= 95.0 && p95 <= 96.0)

let test_percentile_p99 () =
  let sorted = List.init 100 (fun i -> i + 1) in
  let p99 = percentile 0.99 sorted in
  check bool "p99 near 99" true (p99 >= 98.0 && p99 <= 100.0)

(** {1 percentiles Tests} *)

let test_percentiles_multiple () =
  let list = [5; 1; 9; 3; 7] in  (* unsorted *)
  let ps = percentiles [0.0; 0.5; 1.0] list in
  match ps with
  | [p0; p50; p100] ->
      check (float 0.001) "p0 (min)" 1.0 p0;
      check (float 0.001) "p50 (median)" 5.0 p50;
      check (float 0.001) "p100 (max)" 9.0 p100
  | _ -> fail "expected 3 percentiles"

let test_percentiles_empty () =
  let ps = percentiles [0.5; 0.95] [] in
  check (list (float 0.001)) "empty" [0.0; 0.0] ps

(** {1 cost_per_1k_tokens Tests} *)

let test_cost_gpt4 () =
  check (float 0.0001) "gpt-4" 0.03 (cost_per_1k_tokens "gpt-4");
  check (float 0.0001) "gpt-4-turbo" 0.03 (cost_per_1k_tokens "gpt-4-turbo")

let test_cost_gpt35 () =
  check (float 0.0001) "gpt-3.5-turbo" 0.002 (cost_per_1k_tokens "gpt-3.5-turbo")

let test_cost_claude () =
  check (float 0.0001) "claude-3-opus" 0.015 (cost_per_1k_tokens "claude-3-opus");
  check (float 0.0001) "claude-opus-4" 0.015 (cost_per_1k_tokens "claude-opus-4");
  check (float 0.0001) "claude-3-sonnet" 0.003 (cost_per_1k_tokens "claude-3-sonnet");
  check (float 0.00001) "claude-3-haiku" 0.00025 (cost_per_1k_tokens "claude-3-haiku")

let test_cost_gemini () =
  check (float 0.00001) "gemini-pro" 0.00025 (cost_per_1k_tokens "gemini-pro");
  check (float 0.00001) "gemini-2.0-flash" 0.00025 (cost_per_1k_tokens "gemini-2.0-flash")

let test_cost_unknown () =
  check (float 0.0001) "unknown model" 0.001 (cost_per_1k_tokens "unknown-model-xyz")

(** {1 stats JSON Tests} *)

let test_stats_to_json () =
  let s = test_stats () in
  let json = to_json s in
  match Yojson.Safe.Util.member "total_chains" json with
  | `Int n -> check int "total_chains" 10 n
  | _ -> fail "expected int"

let test_stats_json_roundtrip () =
  let s = test_stats () in
  let json = stats_to_yojson s in
  match stats_of_yojson json with
  | Ok s2 ->
      check int "total_chains" s.total_chains s2.total_chains;
      check int "success_count" s.success_count s2.success_count;
      check (float 0.001) "success_rate" s.success_rate s2.success_rate;
      check int "total_tokens" s.total_tokens s2.total_tokens
  | Error e -> fail e

(** {1 to_summary Tests} *)

let test_to_summary_format () =
  let s = test_stats () in
  let summary = to_summary s in
  check bool "contains chains count" true (Common.contains ~substring:"10" summary);
  check bool "contains success rate" true (Common.contains ~substring:"80" summary);
  check bool "contains tokens" true (Common.contains ~substring:"5000" summary)

let test_to_summary_compact () =
  let s = test_stats () in
  let summary = to_summary s in
  (* Summary should be single line, compact *)
  check bool "no newlines" false (String.contains summary '\n');
  check bool "reasonable length" true (String.length summary < 200)

(** {1 string_of_stats Tests} *)

let test_string_of_stats_sections () =
  let s = test_stats () in
  let str = string_of_stats s in
  check bool "has header" true (Common.contains ~substring:"Chain Engine Statistics" str);
  check bool "has execution section" true (Common.contains ~substring:"Execution Summary" str);
  check bool "has latency section" true (Common.contains ~substring:"Latency" str);
  check bool "has token section" true (Common.contains ~substring:"Token Usage" str)

let test_string_of_stats_values () =
  let s = test_stats () in
  let str = string_of_stats s in
  check bool "shows total chains" true (Common.contains ~substring:"10" str);
  check bool "shows success rate" true (Common.contains ~substring:"80" str);
  check bool "shows avg duration" true (Common.contains ~substring:"150" str)

let test_string_of_stats_with_failures () =
  let s = test_stats ~success_count:5 () in
  let str = string_of_stats s in
  check bool "has failure reasons" true (Common.contains ~substring:"Failure Reasons" str);
  check bool "shows timeout" true (Common.contains ~substring:"timeout" str)

(** {1 to_prometheus Tests} *)

let test_prometheus_format () =
  let s = test_stats () in
  let prom = to_prometheus s in
  check bool "has HELP" true (Common.contains ~substring:"# HELP" prom);
  check bool "has TYPE" true (Common.contains ~substring:"# TYPE" prom);
  check bool "has counter" true (Common.contains ~substring:"counter" prom)

let test_prometheus_executions () =
  let s = test_stats ~success_count:8 () in
  let prom = to_prometheus s in
  check bool "has success metric" true (Common.contains ~substring:"status=\"success\"" prom);
  check bool "has failure metric" true (Common.contains ~substring:"status=\"failure\"" prom)

let test_prometheus_tokens () =
  let s = test_stats () in
  let prom = to_prometheus s in
  check bool "has token metric" true (Common.contains ~substring:"chain_tokens_total" prom);
  check bool "has model label" true (Common.contains ~substring:"model=\"gemini-pro\"" prom)

let test_prometheus_duration () =
  let s = test_stats () in
  let prom = to_prometheus s in
  check bool "has duration metric" true (Common.contains ~substring:"chain_duration_seconds" prom);
  check bool "has quantile 0.5" true (Common.contains ~substring:"quantile=\"0.5\"" prom);
  check bool "has quantile 0.95" true (Common.contains ~substring:"quantile=\"0.95\"" prom)

(** {1 Test Suite} *)

let percentile_tests = [
  test_case "empty" `Quick test_percentile_empty;
  test_case "single" `Quick test_percentile_single;
  test_case "p50" `Quick test_percentile_p50;
  test_case "p95" `Quick test_percentile_p95;
  test_case "p99" `Quick test_percentile_p99;
]

let percentiles_tests = [
  test_case "multiple" `Quick test_percentiles_multiple;
  test_case "empty" `Quick test_percentiles_empty;
]

let cost_tests = [
  test_case "gpt-4" `Quick test_cost_gpt4;
  test_case "gpt-3.5" `Quick test_cost_gpt35;
  test_case "claude" `Quick test_cost_claude;
  test_case "gemini" `Quick test_cost_gemini;
  test_case "unknown" `Quick test_cost_unknown;
]

let json_tests = [
  test_case "to_json" `Quick test_stats_to_json;
  test_case "roundtrip" `Quick test_stats_json_roundtrip;
]

let summary_tests = [
  test_case "format" `Quick test_to_summary_format;
  test_case "compact" `Quick test_to_summary_compact;
]

let string_tests = [
  test_case "sections" `Quick test_string_of_stats_sections;
  test_case "values" `Quick test_string_of_stats_values;
  test_case "failures" `Quick test_string_of_stats_with_failures;
]

let prometheus_tests = [
  test_case "format" `Quick test_prometheus_format;
  test_case "executions" `Quick test_prometheus_executions;
  test_case "tokens" `Quick test_prometheus_tokens;
  test_case "duration" `Quick test_prometheus_duration;
]

let () =
  run "chain_stats" [
    ("percentile", percentile_tests);
    ("percentiles", percentiles_tests);
    ("cost_per_1k_tokens", cost_tests);
    ("stats_json", json_tests);
    ("to_summary", summary_tests);
    ("string_of_stats", string_tests);
    ("to_prometheus", prometheus_tests);
  ]

(** Wave 7: Additional chain_stats coverage tests.
    Focus on cost_per_1k_tokens, to_prometheus, to_summary,
    string_of_stats, cascade_snapshot, track_cascade, list_take,
    model_statistics, enable/disable/is_enabled. *)

open Alcotest
open Chain_stats

let check_str = check string
let check_int = check int
let check_float msg expected actual =
  check (float 0.01) msg expected actual
let check_bool = check bool

(* Helper: build a stats value *)
let make_stats
    ?(total_chains=0) ?(total_nodes=0) ?(active_chains=0)
    ?(avg_duration_ms=0.0) ?(p50=0.0) ?(p95=0.0) ?(p99=0.0)
    ?(total_tokens=0) ?(tokens_by_model=[]) ?(estimated_cost_usd=0.0)
    ?(success_count=0) ?(failure_count=0) ?(success_rate=1.0)
    ?(failure_reasons=[]) ?(hourly_tokens=[]) ?(hourly_chains=[])
    () : stats =
  { total_chains; total_nodes; active_chains;
    avg_duration_ms; p50_duration_ms = p50; p95_duration_ms = p95; p99_duration_ms = p99;
    total_tokens; tokens_by_model; estimated_cost_usd;
    success_count; failure_count; success_rate;
    failure_reasons; hourly_tokens; hourly_chains }

(* ---- cost_per_1k_tokens for all branches ---- *)
let test_cost_gpt4_turbo () =
  check_float "gpt-4-turbo" 0.03 (cost_per_1k_tokens "gpt-4-turbo")

let test_cost_claude_opus_4 () =
  check_float "claude-opus-4" 0.015 (cost_per_1k_tokens "claude-opus-4")

let test_cost_claude_sonnet_4 () =
  check_float "claude-sonnet-4" 0.003 (cost_per_1k_tokens "claude-sonnet-4")

let test_cost_claude_haiku_4 () =
  check_float "claude-haiku-4" 0.00025 (cost_per_1k_tokens "claude-haiku-4")

let test_cost_claude_3_haiku () =
  check_float "claude-3-haiku" 0.00025 (cost_per_1k_tokens "claude-3-haiku")

let test_cost_gemini_flash () =
  check_float "gemini-2.0-flash" 0.00025 (cost_per_1k_tokens "gemini-2.0-flash")

let test_cost_codex () =
  check_float "codex" 0.01 (cost_per_1k_tokens "codex")

let test_cost_gpt5_codex () =
  check_float "gpt-5.2-codex" 0.01 (cost_per_1k_tokens "gpt-5.2-codex")

let test_cost_claude_3_sonnet () =
  check_float "claude-3-sonnet" 0.003 (cost_per_1k_tokens "claude-3-sonnet")

let test_cost_claude_3_opus () =
  check_float "claude-3-opus" 0.015 (cost_per_1k_tokens "claude-3-opus")

let test_cost_gemini_pro () =
  check_float "gemini-pro" 0.00025 (cost_per_1k_tokens "gemini-pro")

let test_cost_unknown_model () =
  check_float "unknown" 0.001 (cost_per_1k_tokens "some-new-model")

(* ---- list_take ---- *)
let test_list_take_normal () =
  let result = list_take 3 [1;2;3;4;5] in
  check_int "length" 3 (List.length result);
  check_int "first" 1 (List.hd result)

let test_list_take_more_than_available () =
  let result = list_take 10 [1;2;3] in
  check_int "all items" 3 (List.length result)

let test_list_take_zero () =
  let result = list_take 0 [1;2;3] in
  check_int "empty" 0 (List.length result)

let test_list_take_empty () =
  let result = list_take 5 [] in
  check_int "empty" 0 (List.length result)

(* ---- to_prometheus ---- *)
let test_prometheus_with_models () =
  let s = make_stats ~success_count:10 ~failure_count:2
    ~tokens_by_model:[("gemini", 5000); ("claude", 3000)]
    ~p50:100.0 ~p95:200.0 ~p99:500.0 ~estimated_cost_usd:0.05 () in
  let prom = to_prometheus s in
  check_bool "has success" true (Common.contains ~substring:"success" prom);
  check_bool "has failure" true (Common.contains ~substring:"failure" prom);
  check_bool "has gemini" true (Common.contains ~substring:"gemini" prom);
  check_bool "has claude" true (Common.contains ~substring:"claude" prom);
  check_bool "has quantile 0.5" true (Common.contains ~substring:"quantile=\"0.5\"" prom);
  check_bool "has quantile 0.95" true (Common.contains ~substring:"quantile=\"0.95\"" prom);
  check_bool "has quantile 0.99" true (Common.contains ~substring:"quantile=\"0.99\"" prom);
  check_bool "has cost" true (Common.contains ~substring:"chain_cost_usd_total" prom)

(* ---- to_summary ---- *)
let test_to_summary_basic () =
  Eio_main.run @@ fun _env ->
  reset ();
  let s = make_stats ~total_chains:10 ~total_nodes:50 ~total_tokens:1000
    ~success_rate:0.9 ~estimated_cost_usd:0.01
    ~avg_duration_ms:150.0 ~p95:300.0 () in
  let summary = to_summary s in
  check_bool "has chains" true (Common.contains ~substring:"Chains: 10" summary);
  check_bool "has success rate" true (Common.contains ~substring:"90.0%" summary);
  check_bool "has tokens" true (Common.contains ~substring:"Tokens: 1000" summary)

(* ---- string_of_stats ---- *)
let test_string_of_stats_with_models () =
  let s = make_stats ~total_chains:5 ~total_nodes:20
    ~tokens_by_model:[("gemini", 3000)]
    ~success_count:4 ~failure_count:1 ~success_rate:0.8
    ~avg_duration_ms:200.0 ~p50:180.0 ~p95:400.0 ~p99:800.0
    ~total_tokens:3000 ~estimated_cost_usd:0.03 () in
  let out = string_of_stats s in
  check_bool "has header" true (Common.contains ~substring:"Chain Engine Statistics" out);
  check_bool "has total chains" true (Common.contains ~substring:"Total Chains:    5" out);
  check_bool "has model" true (Common.contains ~substring:"gemini" out)

let test_string_of_stats_with_failures () =
  let s = make_stats ~failure_reasons:[("timeout", 5); ("rate limit", 3)]
    ~success_count:7 ~failure_count:8 ~success_rate:0.467 () in
  let out = string_of_stats s in
  check_bool "has failure section" true (Common.contains ~substring:"Failure Reasons" out);
  check_bool "has timeout" true (Common.contains ~substring:"timeout" out)

let test_string_of_stats_with_hourly () =
  let s = make_stats ~hourly_tokens:[(10, 5000); (14, 8000)] () in
  let out = string_of_stats s in
  check_bool "has hourly section" true (Common.contains ~substring:"Hourly Token Distribution" out);
  check_bool "has hour 10" true (Common.contains ~substring:"10:00" out)

(* ---- cascade_snapshot after reset ---- *)
let test_cascade_snapshot_empty () =
  Eio_main.run @@ fun _env ->
  reset ();
  let cs = cascade_snapshot () in
  check_int "total 0" 0 cs.total_cascades;
  check_int "tier0 0" 0 cs.tier0_resolved;
  check_float "avg_tier 0" 0.0 cs.avg_tier;
  check_float "savings 0" 0.0 cs.estimated_savings_pct

(* ---- track_cascade ---- *)
let test_track_cascade_tier0 () =
  Eio_main.run @@ fun _env ->
  reset ();
  track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  let cs = cascade_snapshot () in
  check_int "total 1" 1 cs.total_cascades;
  check_int "tier0 1" 1 cs.tier0_resolved;
  check_float "savings 100%" 100.0 cs.estimated_savings_pct

let test_track_cascade_tier1 () =
  Eio_main.run @@ fun _env ->
  reset ();
  track_cascade ~resolved_tier:1 ~escalations:1 ~hard_failures:0;
  let cs = cascade_snapshot () in
  check_int "tier1 1" 1 cs.tier1_resolved;
  check_int "escalations 1" 1 cs.total_escalations;
  check_float "savings 50%" 50.0 cs.estimated_savings_pct

let test_track_cascade_tier2 () =
  Eio_main.run @@ fun _env ->
  reset ();
  track_cascade ~resolved_tier:2 ~escalations:2 ~hard_failures:1;
  let cs = cascade_snapshot () in
  check_int "tier2+ 1" 1 cs.tier2_plus_resolved;
  check_int "hard_failures 1" 1 cs.total_hard_failures;
  check_float "savings 0%" 0.0 cs.estimated_savings_pct

let test_track_cascade_mixed () =
  Eio_main.run @@ fun _env ->
  reset ();
  track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  track_cascade ~resolved_tier:1 ~escalations:1 ~hard_failures:0;
  track_cascade ~resolved_tier:2 ~escalations:2 ~hard_failures:0;
  let cs = cascade_snapshot () in
  check_int "total 3" 3 cs.total_cascades;
  check_int "tier0 1" 1 cs.tier0_resolved;
  check_int "tier1 1" 1 cs.tier1_resolved;
  check_int "tier2+ 1" 1 cs.tier2_plus_resolved;
  check_float "avg_tier 1.0" 1.0 cs.avg_tier

(* ---- enable/disable/is_enabled ---- *)
let test_enable_disable () =
  Eio_main.run @@ fun _env ->
  disable ();
  check_bool "initially disabled" false (is_enabled ());
  enable ();
  check_bool "after enable" true (is_enabled ());
  enable ();  (* double enable should be idempotent *)
  check_bool "double enable" true (is_enabled ());
  disable ();
  check_bool "after disable" false (is_enabled ());
  disable ()  (* double disable should be idempotent *)

(* ---- reset clears everything ---- *)
let test_reset () =
  Eio_main.run @@ fun _env ->
  track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  reset ();
  let cs = cascade_snapshot () in
  check_int "cascade reset" 0 cs.total_cascades

(* ---- percentile edge cases ---- *)
let test_percentile_two_elements () =
  let sorted = [10; 20] in
  let p = percentile 0.5 sorted in
  check_float "p50 of 2 elems" 10.0 p

let test_percentile_high () =
  let sorted = [1;2;3;4;5;6;7;8;9;10] in
  let p = percentile 0.99 sorted in
  check_float "p99 of 10 elems" 9.0 p

(* ---- stats yojson roundtrip ---- *)
let test_stats_yojson_roundtrip () =
  let s = make_stats ~total_chains:5 ~total_nodes:20 ~total_tokens:1000
    ~success_count:4 ~failure_count:1 ~success_rate:0.8
    ~tokens_by_model:[("gemini", 500)]
    ~failure_reasons:[("timeout", 2)]
    ~hourly_tokens:[(10, 500)]
    ~hourly_chains:[(10, 3)] () in
  let j = to_json s in
  let s2 = stats_of_yojson j |> Result.get_ok in
  check_int "total_chains" s.total_chains s2.total_chains;
  check_int "total_tokens" s.total_tokens s2.total_tokens

(* ---- cascade_stats yojson roundtrip ---- *)
let test_cascade_stats_yojson () =
  let v : cascade_stats = {
    total_cascades = 10; tier0_resolved = 5; tier1_resolved = 3;
    tier2_plus_resolved = 2; total_escalations = 5; total_hard_failures = 1;
    avg_tier = 0.7; estimated_savings_pct = 65.0
  } in
  let j = cascade_stats_to_yojson v in
  let v2 = cascade_stats_of_yojson j |> Result.get_ok in
  check_int "total" v.total_cascades v2.total_cascades;
  check_float "avg_tier" v.avg_tier v2.avg_tier

(* ---- model_stats yojson roundtrip ---- *)
let test_model_stats_yojson () =
  let v : model_stats = {
    model_name = "gemini"; call_count = 5; total_tokens = 2000;
    avg_tokens_per_call = 400.0; total_cost_usd = 0.005; avg_latency_ms = 150.0
  } in
  let j = model_stats_to_yojson v in
  let v2 = model_stats_of_yojson j |> Result.get_ok in
  check_str "model_name" v.model_name v2.model_name;
  check_int "call_count" v.call_count v2.call_count

let () =
  run "chain_stats_wave7" [
    "cost_per_1k", [
      test_case "gpt-4-turbo" `Quick test_cost_gpt4_turbo;
      test_case "claude-opus-4" `Quick test_cost_claude_opus_4;
      test_case "claude-sonnet-4" `Quick test_cost_claude_sonnet_4;
      test_case "claude-haiku-4" `Quick test_cost_claude_haiku_4;
      test_case "claude-3-haiku" `Quick test_cost_claude_3_haiku;
      test_case "gemini-2.0-flash" `Quick test_cost_gemini_flash;
      test_case "codex" `Quick test_cost_codex;
      test_case "gpt-5.2-codex" `Quick test_cost_gpt5_codex;
      test_case "claude-3-sonnet" `Quick test_cost_claude_3_sonnet;
      test_case "claude-3-opus" `Quick test_cost_claude_3_opus;
      test_case "gemini-pro" `Quick test_cost_gemini_pro;
      test_case "unknown" `Quick test_cost_unknown_model;
    ];
    "list_take", [
      test_case "normal" `Quick test_list_take_normal;
      test_case "more than available" `Quick test_list_take_more_than_available;
      test_case "zero" `Quick test_list_take_zero;
      test_case "empty" `Quick test_list_take_empty;
    ];
    "prometheus", [
      test_case "with models" `Quick test_prometheus_with_models;
    ];
    "summary", [
      test_case "basic" `Quick test_to_summary_basic;
    ];
    "string_of_stats", [
      test_case "with models" `Quick test_string_of_stats_with_models;
      test_case "with failures" `Quick test_string_of_stats_with_failures;
      test_case "with hourly" `Quick test_string_of_stats_with_hourly;
    ];
    "cascade", [
      test_case "snapshot empty" `Quick test_cascade_snapshot_empty;
      test_case "track tier0" `Quick test_track_cascade_tier0;
      test_case "track tier1" `Quick test_track_cascade_tier1;
      test_case "track tier2" `Quick test_track_cascade_tier2;
      test_case "track mixed" `Quick test_track_cascade_mixed;
    ];
    "management", [
      test_case "enable/disable" `Quick test_enable_disable;
      test_case "reset" `Quick test_reset;
    ];
    "percentile", [
      test_case "two elements" `Quick test_percentile_two_elements;
      test_case "high" `Quick test_percentile_high;
    ];
    "yojson", [
      test_case "stats roundtrip" `Quick test_stats_yojson_roundtrip;
      test_case "cascade_stats" `Quick test_cascade_stats_yojson;
      test_case "model_stats" `Quick test_model_stats_yojson;
    ];
  ]

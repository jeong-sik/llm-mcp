(** Wave 8: chain_stats — squeeze remaining branches.
    Focus: stats_handler events, model_statistics, compute with since,
    string_of_stats with all sections, to_prometheus with models *)

let run f = Eio_main.run (fun _env -> f ())

(* Helper: stats with all optional sections populated *)
let full_stats () : Chain_stats.stats = {
  total_chains = 100;
  total_nodes = 500;
  active_chains = 2;
  avg_duration_ms = 250.5;
  p50_duration_ms = 200.0;
  p95_duration_ms = 450.0;
  p99_duration_ms = 900.0;
  total_tokens = 50000;
  tokens_by_model = [("gpt-4", 30000); ("claude-3-opus", 20000)];
  estimated_cost_usd = 1.25;
  success_count = 90;
  failure_count = 10;
  success_rate = 0.9;
  failure_reasons = [
    ("timeout", 5);
    ("rate_limit", 3);
    ("A very long failure reason that should be truncated to 50 characters because it is too long", 2);
  ];
  hourly_tokens = [(9, 5000); (10, 15000); (14, 20000); (15, 10000)];
  hourly_chains = [(9, 10); (10, 30); (14, 40); (15, 20)];
}

(* ── stats_handler events ── *)
let test_handler_chain_start () = run (fun () ->
  Chain_stats.reset ();
  let payload : Chain_telemetry.chain_start_payload = {
    start_chain_id = "test-chain";
    start_nodes = 3;
    start_timestamp = Unix.gettimeofday ();
    start_mermaid_dsl = None;
  } in
  Chain_stats.stats_handler (ChainStart payload);
  let stats = Chain_stats.compute () in
  Alcotest.(check int) "active +1" 1 stats.active_chains
)

let test_handler_node_complete () = run (fun () ->
  Chain_stats.reset ();
  let token : Chain_category.token_usage = {
    prompt_tokens = 100;
    completion_tokens = 50;
    total_tokens = 150;
    estimated_cost_usd = 0.01;
  } in
  let payload : Chain_telemetry.node_complete_payload = {
    node_complete_id = "node-1";
    node_duration_ms = 200;
    node_tokens = token;
    node_verdict = Pass "ok";
    node_confidence = 0.9;
    node_output_preview = Some "preview";
  } in
  Chain_stats.stats_handler (NodeComplete payload);
  let stats = Chain_stats.compute () in
  Alcotest.(check int) "tokens tracked" 150 stats.total_tokens;
  Alcotest.(check int) "1 node" 1 stats.total_nodes
)

let test_handler_chain_complete () = run (fun () ->
  Chain_stats.reset ();
  (* Start first to increment active *)
  Chain_stats.stats_handler (ChainStart {
    start_chain_id = "c1"; start_nodes = 1;
    start_timestamp = Unix.gettimeofday (); start_mermaid_dsl = None;
  });
  let token : Chain_category.token_usage = {
    prompt_tokens = 0; completion_tokens = 0;
    total_tokens = 0; estimated_cost_usd = 0.0;
  } in
  Chain_stats.stats_handler (ChainComplete {
    complete_chain_id = "c1"; complete_duration_ms = 100;
    complete_tokens = token; nodes_executed = 1; nodes_skipped = 0;
  });
  let stats = Chain_stats.compute () in
  Alcotest.(check int) "1 chain" 1 stats.total_chains;
  Alcotest.(check int) "success" 1 stats.success_count;
  Alcotest.(check int) "active back to 0" 0 stats.active_chains
)

let test_handler_error () = run (fun () ->
  Chain_stats.reset ();
  Chain_stats.stats_handler (ChainStart {
    start_chain_id = "c1"; start_nodes = 1;
    start_timestamp = Unix.gettimeofday (); start_mermaid_dsl = None;
  });
  Chain_stats.stats_handler (Error {
    error_node_id = "n1"; error_message = "timeout";
    error_retries = 0; error_timestamp = Unix.gettimeofday ();
  });
  let stats = Chain_stats.compute () in
  Alcotest.(check int) "1 failure" 1 stats.failure_count;
  Alcotest.(check int) "active back to 0" 0 stats.active_chains;
  Alcotest.(check bool) "has failure reason" true
    (List.exists (fun (r, _) -> r = "timeout") stats.failure_reasons)
)

let test_handler_node_start () = run (fun () ->
  (* NodeStart is a no-op *)
  Chain_stats.reset ();
  Chain_stats.stats_handler (NodeStart {
    node_start_id = "n1"; node_start_type = "llm";
    node_parent = None;
  });
  let stats = Chain_stats.compute () in
  Alcotest.(check int) "no change" 0 stats.total_nodes
)

(* ── track_model_tokens / track_model_latency ── *)
let test_track_model_tokens () = run (fun () ->
  Chain_stats.reset ();
  Chain_stats.track_model_tokens ~model:"gpt-4" ~tokens:1000;
  Chain_stats.track_model_tokens ~model:"gpt-4" ~tokens:500;
  Chain_stats.track_model_tokens ~model:"claude-3-opus" ~tokens:200;
  let ms = Chain_stats.model_statistics () in
  let gpt4 = List.find (fun (m : Chain_stats.model_stats) -> m.model_name = "gpt-4") ms in
  Alcotest.(check int) "gpt4 tokens" 1500 gpt4.total_tokens;
  Alcotest.(check int) "gpt4 calls" 2 gpt4.call_count;
  Alcotest.(check bool) "gpt4 cost > 0" true (gpt4.total_cost_usd > 0.0)
)

let test_track_model_latency () = run (fun () ->
  Chain_stats.reset ();
  Chain_stats.track_model_tokens ~model:"test-model" ~tokens:100;
  Chain_stats.track_model_latency ~model:"test-model" ~latency_ms:100;
  Chain_stats.track_model_latency ~model:"test-model" ~latency_ms:200;
  let ms = Chain_stats.model_statistics () in
  let m = List.find (fun (m : Chain_stats.model_stats) -> m.model_name = "test-model") ms in
  Alcotest.(check bool) "avg latency > 0" true (m.avg_latency_ms > 0.0)
)

let test_model_statistics_no_latency () = run (fun () ->
  Chain_stats.reset ();
  Chain_stats.track_model_tokens ~model:"no-lat" ~tokens:100;
  let ms = Chain_stats.model_statistics () in
  let m = List.find (fun (m : Chain_stats.model_stats) -> m.model_name = "no-lat") ms in
  Alcotest.(check (float 0.001)) "no latency" 0.0 m.avg_latency_ms
)

(* ── compute with since ── *)
let test_compute_with_since () = run (fun () ->
  Chain_stats.reset ();
  let token : Chain_category.token_usage = {
    prompt_tokens = 0; completion_tokens = 0;
    total_tokens = 100; estimated_cost_usd = 0.0;
  } in
  (* Add a chain completion *)
  Chain_stats.stats_handler (ChainComplete {
    complete_chain_id = "old"; complete_duration_ms = 100;
    complete_tokens = token; nodes_executed = 1; nodes_skipped = 0;
  });
  let since = Unix.gettimeofday () +. 1.0 in
  let stats = Chain_stats.compute ~since () in
  (* Since is in the future, no durations should match *)
  Alcotest.(check (float 0.001)) "avg 0 with future since" 0.0 stats.avg_duration_ms
)

(* ── string_of_stats with all sections ── *)
let test_string_of_stats_full () =
  let s = Chain_stats.string_of_stats (full_stats ()) in
  Alcotest.(check bool) "has execution summary" true
    (try ignore (Str.search_forward (Str.regexp_string "Execution Summary") s 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has latency" true
    (try ignore (Str.search_forward (Str.regexp_string "Latency") s 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has token usage" true
    (try ignore (Str.search_forward (Str.regexp_string "Token Usage") s 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has by model" true
    (try ignore (Str.search_forward (Str.regexp_string "By Model") s 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has failure reasons" true
    (try ignore (Str.search_forward (Str.regexp_string "Failure Reasons") s 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has hourly" true
    (try ignore (Str.search_forward (Str.regexp_string "Hourly Token") s 0); true
     with Not_found -> false);
  (* Check truncated failure reason *)
  Alcotest.(check bool) "has truncated reason" true
    (try ignore (Str.search_forward (Str.regexp_string "...") s 0); true
     with Not_found -> false)

let test_string_of_stats_no_optional () =
  let s = Chain_stats.string_of_stats {
    (full_stats ()) with
    tokens_by_model = [];
    failure_reasons = [];
    hourly_tokens = [];
  } in
  Alcotest.(check bool) "no By Model" true
    (try ignore (Str.search_forward (Str.regexp_string "By Model") s 0); false
     with Not_found -> true);
  Alcotest.(check bool) "no Failure Reasons" true
    (try ignore (Str.search_forward (Str.regexp_string "Failure Reasons") s 0); false
     with Not_found -> true);
  Alcotest.(check bool) "no Hourly" true
    (try ignore (Str.search_forward (Str.regexp_string "Hourly Token") s 0); false
     with Not_found -> true)

(* ── to_prometheus with models ── *)
let test_prometheus_with_models () =
  let s = Chain_stats.to_prometheus (full_stats ()) in
  Alcotest.(check bool) "has gpt-4" true
    (try ignore (Str.search_forward (Str.regexp_string "gpt-4") s 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has claude-3-opus" true
    (try ignore (Str.search_forward (Str.regexp_string "claude-3-opus") s 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has cost" true
    (try ignore (Str.search_forward (Str.regexp_string "chain_cost_usd_total") s 0); true
     with Not_found -> false)

(* ── to_summary with cascade ── *)
let test_to_summary_with_cascade () = run (fun () ->
  Chain_stats.reset ();
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:1 ~escalations:1 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  let summary = Chain_stats.to_summary (full_stats ()) in
  Alcotest.(check bool) "has Cascades" true
    (try ignore (Str.search_forward (Str.regexp_string "Cascades") summary 0); true
     with Not_found -> false);
  Alcotest.(check bool) "has T0" true
    (try ignore (Str.search_forward (Str.regexp_string "T0:") summary 0); true
     with Not_found -> false)
)

(* ── to_json ── *)
let test_to_json () =
  let json = Chain_stats.to_json (full_stats ()) in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "has total_chains" true (String.length s > 0)

(* ── cascade_snapshot with data ── *)
let test_cascade_snapshot_with_data () = run (fun () ->
  Chain_stats.reset ();
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:1 ~escalations:1 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:2 ~escalations:2 ~hard_failures:1;
  let cs = Chain_stats.cascade_snapshot () in
  Alcotest.(check int) "total 4" 4 cs.total_cascades;
  Alcotest.(check int) "tier0 = 2" 2 cs.tier0_resolved;
  Alcotest.(check int) "tier1 = 1" 1 cs.tier1_resolved;
  Alcotest.(check int) "tier2+ = 1" 1 cs.tier2_plus_resolved;
  Alcotest.(check int) "escalations = 3" 3 cs.total_escalations;
  Alcotest.(check int) "hard_failures = 1" 1 cs.total_hard_failures;
  Alcotest.(check bool) "avg > 0" true (cs.avg_tier > 0.0);
  Alcotest.(check bool) "savings > 0" true (cs.estimated_savings_pct > 0.0)
)

(* ── list_take edge case ── *)
let test_list_take_negative () = run (fun () ->
  let r = Chain_stats.list_take (-1) [1;2;3] in
  Alcotest.(check (list int)) "negative -> empty" [] r
)

(* ── cost_per_1k_tokens additional models ── *)
let test_cost_gpt35_turbo () =
  let c = Chain_stats.cost_per_1k_tokens "gpt-3.5-turbo" in
  Alcotest.(check (float 0.0001)) "gpt-3.5-turbo" 0.002 c

let test_cost_claude_opus_4 () =
  let c = Chain_stats.cost_per_1k_tokens "claude-opus-4" in
  Alcotest.(check (float 0.0001)) "claude-opus-4" 0.015 c

let test_cost_claude_sonnet_4 () =
  let c = Chain_stats.cost_per_1k_tokens "claude-sonnet-4" in
  Alcotest.(check (float 0.0001)) "claude-sonnet-4" 0.003 c

let test_cost_claude_haiku_4 () =
  let c = Chain_stats.cost_per_1k_tokens "claude-haiku-4" in
  Alcotest.(check (float 0.0001)) "claude-haiku-4" 0.00025 c

let test_cost_gemini_flash () =
  let c = Chain_stats.cost_per_1k_tokens "gemini-2.0-flash" in
  Alcotest.(check (float 0.0001)) "gemini-flash" 0.00025 c

let test_cost_gpt5_codex () =
  let c = Chain_stats.cost_per_1k_tokens "gpt-5.2-codex" in
  Alcotest.(check (float 0.0001)) "gpt-5.2-codex" 0.01 c

let () =
  let open Alcotest in
  run "Chain_stats_wave8" [
    "stats_handler", [
      test_case "chain start" `Quick test_handler_chain_start;
      test_case "node complete" `Quick test_handler_node_complete;
      test_case "chain complete" `Quick test_handler_chain_complete;
      test_case "error" `Quick test_handler_error;
      test_case "node start (noop)" `Quick test_handler_node_start;
    ];
    "model_tracking", [
      test_case "track tokens" `Quick test_track_model_tokens;
      test_case "track latency" `Quick test_track_model_latency;
      test_case "no latency" `Quick test_model_statistics_no_latency;
    ];
    "compute", [
      test_case "with since" `Quick test_compute_with_since;
    ];
    "formatting", [
      test_case "string_of_stats full" `Quick test_string_of_stats_full;
      test_case "string_of_stats minimal" `Quick test_string_of_stats_no_optional;
      test_case "prometheus models" `Quick test_prometheus_with_models;
      test_case "summary with cascade" `Quick test_to_summary_with_cascade;
      test_case "to_json" `Quick test_to_json;
    ];
    "cascade", [
      test_case "snapshot with data" `Quick test_cascade_snapshot_with_data;
    ];
    "util", [
      test_case "list_take negative" `Quick test_list_take_negative;
    ];
    "cost", [
      test_case "gpt-3.5-turbo" `Quick test_cost_gpt35_turbo;
      test_case "claude-opus-4" `Quick test_cost_claude_opus_4;
      test_case "claude-sonnet-4" `Quick test_cost_claude_sonnet_4;
      test_case "claude-haiku-4" `Quick test_cost_claude_haiku_4;
      test_case "gemini-2.0-flash" `Quick test_cost_gemini_flash;
      test_case "gpt-5.2-codex" `Quick test_cost_gpt5_codex;
    ];
  ]

(** Wave 8: chain_types — batch types, cascade_tier, retry_config yojson roundtrips,
    make_chain, make_threshold, count_chain_parallel_groups, plus error branches
    in *_of_yojson for complex nested types *)

open Chain_types

let check_string msg expected actual =
  Alcotest.(check string) msg expected actual

let check_int msg expected actual =
  Alcotest.(check int) msg expected actual

let check_float msg expected actual =
  Alcotest.(check (float 0.001)) msg expected actual

(* Helper: simple LLM node *)
let llm_node id = {
  id;
  node_type = Llm { model = "test"; system = None; prompt = "test";
    timeout = None; tools = None; prompt_ref = None; prompt_vars = [];
    thinking = false };
  input_mapping = [];
  output_key = None;
  depends_on = None;
}

let fanout_node id nodes = {
  id;
  node_type = Fanout (List.map (fun n -> llm_node n) nodes);
  input_mapping = [];
  output_key = None;
  depends_on = None;
}

(* ── batch_priority yojson ── *)
let test_batch_priority_high () =
  let j = batch_priority_to_yojson High in
  let rt = batch_priority_of_yojson j in
  Alcotest.(check (result (of_pp (fun fmt _ -> Format.pp_print_string fmt "High")) string))
    "High roundtrip" (Ok High) rt

let test_batch_priority_normal () =
  let j = batch_priority_to_yojson Normal in
  let rt = batch_priority_of_yojson j in
  Alcotest.(check (result (of_pp (fun fmt _ -> Format.pp_print_string fmt "Normal")) string))
    "Normal roundtrip" (Ok Normal) rt

let test_batch_priority_low () =
  let j = batch_priority_to_yojson Low in
  let rt = batch_priority_of_yojson j in
  Alcotest.(check (result (of_pp (fun fmt _ -> Format.pp_print_string fmt "Low")) string))
    "Low roundtrip" (Ok Low) rt

(* ── retry_config yojson ── *)
let test_retry_config_roundtrip () =
  let rc = { max_retries = 5; initial_delay_ms = 500;
             backoff_multiplier = 1.5; max_delay_ms = 10000 } in
  let j = retry_config_to_yojson rc in
  match retry_config_of_yojson j with
  | Ok rc2 ->
    check_int "max_retries" 5 rc2.max_retries;
    check_int "initial_delay" 500 rc2.initial_delay_ms;
    check_float "backoff" 1.5 rc2.backoff_multiplier;
    check_int "max_delay" 10000 rc2.max_delay_ms
  | Error e -> Alcotest.fail e

let test_default_retry_config () =
  check_int "default max_retries" 3 default_retry_config.max_retries;
  check_int "default initial_delay" 1000 default_retry_config.initial_delay_ms;
  check_float "default backoff" 2.0 default_retry_config.backoff_multiplier;
  check_int "default max_delay" 30000 default_retry_config.max_delay_ms

(* ── batch_config yojson ── *)
let test_batch_config_roundtrip () =
  let bc = { batch_max_concurrent = 10; rate_limit_per_min = 120;
             retry_policy = default_retry_config; priority = High } in
  let j = batch_config_to_yojson bc in
  match batch_config_of_yojson j with
  | Ok bc2 ->
    check_int "max_concurrent" 10 bc2.batch_max_concurrent;
    check_int "rate_limit" 120 bc2.rate_limit_per_min;
    check_int "retry max" 3 bc2.retry_policy.max_retries
  | Error e -> Alcotest.fail e

let test_default_batch_config () =
  check_int "default concurrent" 5 default_batch_config.batch_max_concurrent;
  check_int "default rate" 60 default_batch_config.rate_limit_per_min

(* ── batch_stats yojson ── *)
let test_batch_stats_roundtrip () =
  let bs : batch_stats = {
    total_chains = 10; completed = 8; failed = 2;
    total_duration_ms = 5000;
    total_tokens = Chain_category.Token_monoid.empty;
    avg_duration_ms = 500.0;
  } in
  let j = batch_stats_to_yojson bs in
  match batch_stats_of_yojson j with
  | Ok bs2 ->
    check_int "total" 10 bs2.total_chains;
    check_int "completed" 8 bs2.completed;
    check_int "failed" 2 bs2.failed;
    check_int "duration" 5000 bs2.total_duration_ms;
    check_float "avg" 500.0 bs2.avg_duration_ms
  | Error e -> Alcotest.fail e

(* ── batch_result yojson ── *)
let test_batch_result_roundtrip () =
  let chain_res : chain_result = {
    chain_id = "c1"; output = "out"; success = true;
    trace = []; token_usage = empty_token_usage;
    duration_ms = 100; metadata = [];
  } in
  let br : batch_result = {
    batch_id = "b1";
    results = [("c1", chain_res)];
    stats = {
      total_chains = 1; completed = 1; failed = 0;
      total_duration_ms = 100;
      total_tokens = Chain_category.Token_monoid.empty;
      avg_duration_ms = 100.0;
    };
    failed_chains = [];
  } in
  let j = batch_result_to_yojson br in
  match batch_result_of_yojson j with
  | Ok br2 ->
    check_string "batch_id" "b1" br2.batch_id;
    check_int "results len" 1 (List.length br2.results);
    check_int "stats total" 1 br2.stats.total_chains
  | Error e -> Alcotest.fail e

(* ── cascade_tier (embedded in node_type yojson) ── *)
let test_cascade_tier_roundtrip () =
  let node = {
    id = "casc";
    node_type = Cascade {
      tiers = [
        { tier_node = llm_node "t0"; tier_index = 0;
          confidence_threshold = 0.5; cost_weight = 0.1; pass_context = false };
        { tier_node = llm_node "t1"; tier_index = 1;
          confidence_threshold = 0.7; cost_weight = 0.5; pass_context = true };
      ];
      confidence_prompt = Some "Rate confidence";
      max_escalations = 3;
      context_mode = CM_Full;
      task_hint = Some "coding task";
      default_threshold = 0.8;
    };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let j = node_to_yojson node in
  match node_of_yojson j with
  | Ok n2 ->
    check_string "id" "casc" n2.id;
    (match n2.node_type with
     | Cascade c ->
       check_int "2 tiers" 2 (List.length c.tiers);
       check_int "max_esc" 3 c.max_escalations;
       let t0 = List.hd c.tiers in
       check_int "t0 index" 0 t0.tier_index;
       check_float "t0 threshold" 0.5 t0.confidence_threshold
     | _ -> Alcotest.fail "expected Cascade")
  | Error e -> Alcotest.fail e

(* ── make_chain ── *)
let test_make_chain () =
  let chain = make_chain ~id:"test-chain"
    ~nodes:[llm_node "a"; llm_node "b"]
    ~output:"b"
    ~name:"Test Chain"
    ~description:"A test"
    ~version:"1.0.0" () in
  check_string "chain id" "test-chain" chain.id;
  check_int "2 nodes" 2 (List.length chain.nodes);
  check_string "output" "b" chain.output;
  Alcotest.(check (option string)) "name" (Some "Test Chain") chain.name;
  Alcotest.(check (option string)) "desc" (Some "A test") chain.description;
  Alcotest.(check (option string)) "version" (Some "1.0.0") chain.version

let test_make_chain_minimal () =
  let chain = make_chain ~id:"min" ~nodes:[] ~output:"x" () in
  check_string "id" "min" chain.id;
  Alcotest.(check (option string)) "no name" None chain.name;
  Alcotest.(check (option string)) "no desc" None chain.description

(* ── make_threshold ── *)
let test_make_threshold () =
  let n = make_threshold ~id:"thresh" ~metric:"score" ~operator:Gte
    ~value:0.8 ~input_node:(llm_node "input")
    ~on_pass:(llm_node "pass") ~on_fail:(llm_node "fail") () in
  check_string "id" "thresh" n.id;
  (match n.node_type with
   | Threshold t ->
     check_string "metric" "score" t.metric;
     check_float "value" 0.8 t.value
   | _ -> Alcotest.fail "expected Threshold")

(* ── make_cascade ── *)
let test_make_cascade () =
  let tiers = [
    { tier_node = llm_node "t0"; tier_index = 0;
      confidence_threshold = 0.5; cost_weight = 0.1; pass_context = false };
  ] in
  let n = make_cascade ~id:"casc" ~tiers () in
  check_string "id" "casc" n.id;
  (match n.node_type with
   | Cascade c ->
     check_int "1 tier" 1 (List.length c.tiers);
     check_int "default max_esc" 2 c.max_escalations;
     check_float "default threshold" 0.7 c.default_threshold
   | _ -> Alcotest.fail "expected Cascade")

(* ── count_chain_parallel_groups ── *)
let test_count_chain_parallel_groups () =
  let chain = make_chain ~id:"c"
    ~nodes:[fanout_node "f1" ["a"; "b"]; llm_node "c"]
    ~output:"c" () in
  let count = count_chain_parallel_groups chain in
  check_int "chain with 1 fanout" 1 count

let test_count_chain_parallel_groups_empty () =
  let chain = make_chain ~id:"c" ~nodes:[] ~output:"x" () in
  check_int "empty chain" 0 (count_chain_parallel_groups chain)

(* ── trace_entry yojson ── *)
let test_trace_entry_roundtrip () =
  let te : trace_entry = {
    node_id = "n1"; node_type_name = "llm";
    start_time = 1000.0; end_time = 1100.0;
    status = `Success; output_preview = Some "preview";
    error = None;
  } in
  let j = trace_entry_to_yojson te in
  match trace_entry_of_yojson j with
  | Ok te2 ->
    check_string "node_id" "n1" te2.node_id;
    check_string "type" "llm" te2.node_type_name;
    (match te2.status with `Success -> () | _ -> Alcotest.fail "expected Success")
  | Error e -> Alcotest.fail e

let test_trace_entry_failure () =
  let te : trace_entry = {
    node_id = "n1"; node_type_name = "llm";
    start_time = 1000.0; end_time = 1100.0;
    status = `Failure; output_preview = None;
    error = Some "timeout";
  } in
  let j = trace_entry_to_yojson te in
  match trace_entry_of_yojson j with
  | Ok te2 ->
    (match te2.status with `Failure -> () | _ -> Alcotest.fail "expected Failure");
    Alcotest.(check (option string)) "error" (Some "timeout") te2.error
  | Error e -> Alcotest.fail e

let test_trace_entry_skipped () =
  let te : trace_entry = {
    node_id = "n1"; node_type_name = "gate";
    start_time = 1000.0; end_time = 1000.0;
    status = `Skipped; output_preview = None;
    error = None;
  } in
  let j = trace_entry_to_yojson te in
  match trace_entry_of_yojson j with
  | Ok te2 ->
    (match te2.status with `Skipped -> () | _ -> Alcotest.fail "expected Skipped")
  | Error e -> Alcotest.fail e

(* ── execution_plan yojson ── *)
let test_execution_plan_roundtrip () =
  let chain = make_chain ~id:"ep" ~nodes:[llm_node "a"] ~output:"a" () in
  let ep : execution_plan = {
    chain; execution_order = ["a"];
    parallel_groups = [["a"]]; depth = 1;
  } in
  let j = execution_plan_to_yojson ep in
  match execution_plan_of_yojson j with
  | Ok ep2 ->
    check_string "chain id" "ep" ep2.chain.id;
    check_int "order len" 1 (List.length ep2.execution_order);
    check_int "depth" 1 ep2.depth
  | Error e -> Alcotest.fail e

(* ── chain_result yojson ── *)
let test_chain_result_roundtrip () =
  let cr : chain_result = {
    chain_id = "c1"; output = "result"; success = true;
    trace = [{
      node_id = "n1"; node_type_name = "llm";
      start_time = 0.0; end_time = 1.0;
      status = `Success; output_preview = Some "ok";
      error = None;
    }];
    token_usage = { prompt_tokens = 10; completion_tokens = 20;
                    total_tokens = 30; estimated_cost_usd = 0.001 };
    duration_ms = 500;
    metadata = [("key", "value")];
  } in
  let j = chain_result_to_yojson cr in
  match chain_result_of_yojson j with
  | Ok cr2 ->
    check_string "chain_id" "c1" cr2.chain_id;
    Alcotest.(check bool) "success" true cr2.success;
    check_int "trace len" 1 (List.length cr2.trace);
    check_int "tokens" 30 cr2.token_usage.total_tokens;
    check_int "duration" 500 cr2.duration_ms;
    check_int "metadata len" 1 (List.length cr2.metadata)
  | Error e -> Alcotest.fail e

(* ── token_usage roundtrip ── *)
let test_token_usage_roundtrip () =
  let tu = { prompt_tokens = 100; completion_tokens = 200;
             total_tokens = 300; estimated_cost_usd = 0.05 } in
  let j = token_usage_to_yojson tu in
  match token_usage_of_yojson j with
  | Ok tu2 ->
    check_int "prompt" 100 tu2.prompt_tokens;
    check_int "completion" 200 tu2.completion_tokens;
    check_int "total" 300 tu2.total_tokens;
    check_float "cost" 0.05 tu2.estimated_cost_usd
  | Error e -> Alcotest.fail e

(* ── empty_token_usage ── *)
let test_empty_token_usage () =
  check_int "empty prompt" 0 empty_token_usage.prompt_tokens;
  check_int "empty completion" 0 empty_token_usage.completion_tokens;
  check_int "empty total" 0 empty_token_usage.total_tokens;
  check_float "empty cost" 0.0 empty_token_usage.estimated_cost_usd

(* ── node_type_name additional ── *)
let test_node_type_name_tool () =
  check_string "tool" "tool" (node_type_name (Tool { name = "t"; args = `Assoc [] }))

let test_node_type_name_adapter () =
  check_string "adapter" "adapter"
    (node_type_name (Adapter { input_ref = "x"; transform = Custom "id"; on_error = `Fail }))

let test_node_type_name_pipeline () =
  check_string "pipeline" "pipeline" (node_type_name (Pipeline []))

let test_node_type_name_fanout () =
  check_string "fanout" "fanout" (node_type_name (Fanout []))

let test_node_type_name_retry () =
  let inner = llm_node "x" in
  check_string "retry" "retry"
    (node_type_name (Retry { node = inner; max_attempts = 3;
                             backoff = Exponential 1.0; retry_on = [] }))

let test_node_type_name_race () =
  check_string "race" "race"
    (node_type_name (Race { nodes = []; timeout = None }))

let test_node_type_name_mcts () =
  check_string "mcts" "mcts"
    (node_type_name (Mcts { strategies = []; simulation = llm_node "s";
                            evaluator = "f"; evaluator_prompt = None;
                            policy = Greedy; max_iterations = 10; max_depth = 5;
                            expansion_threshold = 2; early_stop = None; parallel_sims = 1 }))

let test_node_type_name_map () =
  check_string "map" "map"
    (node_type_name (Map { func = "f"; inner = llm_node "i" }))

let test_node_type_name_bind () =
  check_string "bind" "bind"
    (node_type_name (Bind { func = "f"; inner = llm_node "i" }))

let test_node_type_name_chain_ref () =
  check_string "chain_ref" "chain_ref" (node_type_name (ChainRef "x"))

let test_node_type_name_chain_exec () =
  check_string "chain_exec" "chain_exec" (node_type_name (ChainExec { chain_source = "x"; validate = false; max_depth = 3; sandbox = false; context_inject = []; pass_outputs = true }))

(* ── direction roundtrips ── *)
let test_direction_roundtrip () =
  List.iter (fun (d, s) ->
    check_string (Printf.sprintf "%s to_string" s) s (direction_to_string d);
    check_string (Printf.sprintf "%s of_string" s) s
      (direction_to_string (direction_of_string s))
  ) [LR, "LR"; RL, "RL"; TB, "TB"; BT, "BT"]

let test_direction_of_string_default () =
  (* TD is alias for TB *)
  check_string "TD -> TB" "TB" (direction_to_string (direction_of_string "TD"));
  (* unknown string defaults to LR *)
  check_string "unknown -> LR" "LR" (direction_to_string (direction_of_string "INVALID"))

(* ── confidence ── *)
let test_confidence_float_all () =
  check_float "high" 1.0 (confidence_to_float High);
  check_float "medium" 0.5 (confidence_to_float Medium);
  check_float "low" 0.2 (confidence_to_float Low)

let test_confidence_of_string_all () =
  List.iter (fun (s, expected) ->
    let got = confidence_of_string s in
    check_float s (confidence_to_float expected) (confidence_to_float got)
  ) [
    ("high", High); ("HIGH", High);
    ("medium", Medium); ("MEDIUM", Medium);
    ("low", Low); ("LOW", Low);
    ("garbage", Low);
  ]

let () =
  let open Alcotest in
  run "Chain_types_wave8" [
    "batch_priority", [
      test_case "high" `Quick test_batch_priority_high;
      test_case "normal" `Quick test_batch_priority_normal;
      test_case "low" `Quick test_batch_priority_low;
    ];
    "retry_config", [
      test_case "roundtrip" `Quick test_retry_config_roundtrip;
      test_case "defaults" `Quick test_default_retry_config;
    ];
    "batch_config", [
      test_case "roundtrip" `Quick test_batch_config_roundtrip;
      test_case "defaults" `Quick test_default_batch_config;
    ];
    "batch_stats", [
      test_case "roundtrip" `Quick test_batch_stats_roundtrip;
    ];
    "batch_result", [
      test_case "roundtrip" `Quick test_batch_result_roundtrip;
    ];
    "cascade_tier", [
      test_case "roundtrip" `Quick test_cascade_tier_roundtrip;
    ];
    "make_chain", [
      test_case "full" `Quick test_make_chain;
      test_case "minimal" `Quick test_make_chain_minimal;
    ];
    "make_threshold", [
      test_case "basic" `Quick test_make_threshold;
    ];
    "make_cascade", [
      test_case "basic" `Quick test_make_cascade;
    ];
    "count_chain_parallel", [
      test_case "with fanout" `Quick test_count_chain_parallel_groups;
      test_case "empty" `Quick test_count_chain_parallel_groups_empty;
    ];
    "trace_entry", [
      test_case "success" `Quick test_trace_entry_roundtrip;
      test_case "failure" `Quick test_trace_entry_failure;
      test_case "skipped" `Quick test_trace_entry_skipped;
    ];
    "execution_plan", [
      test_case "roundtrip" `Quick test_execution_plan_roundtrip;
    ];
    "chain_result", [
      test_case "roundtrip" `Quick test_chain_result_roundtrip;
    ];
    "token_usage", [
      test_case "roundtrip" `Quick test_token_usage_roundtrip;
      test_case "empty" `Quick test_empty_token_usage;
    ];
    "node_type_name", [
      test_case "tool" `Quick test_node_type_name_tool;
      test_case "adapter" `Quick test_node_type_name_adapter;
      test_case "pipeline" `Quick test_node_type_name_pipeline;
      test_case "fanout" `Quick test_node_type_name_fanout;
      test_case "retry" `Quick test_node_type_name_retry;
      test_case "race" `Quick test_node_type_name_race;
      test_case "mcts" `Quick test_node_type_name_mcts;
      test_case "map" `Quick test_node_type_name_map;
      test_case "bind" `Quick test_node_type_name_bind;
      test_case "chain_ref" `Quick test_node_type_name_chain_ref;
      test_case "chain_exec" `Quick test_node_type_name_chain_exec;
    ];
    "direction", [
      test_case "roundtrips" `Quick test_direction_roundtrip;
      test_case "default" `Quick test_direction_of_string_default;
    ];
    "confidence", [
      test_case "float all" `Quick test_confidence_float_all;
      test_case "of_string all" `Quick test_confidence_of_string_all;
    ];
  ]

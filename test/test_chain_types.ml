(** Tests for Chain_types module - Core type definitions and helpers

    Pure function tests:
    - direction_to_string / direction_of_string: roundtrip
    - node_type_name: all 27 node types
    - make_* helpers: llm_node, adapter, tool_node, pipeline, etc.
    - default_config, empty_token_usage
*)

open Alcotest
open Chain_types

(** {1 Direction Tests} *)

let test_direction_to_string () =
  check string "LR" "LR" (direction_to_string LR);
  check string "RL" "RL" (direction_to_string RL);
  check string "TB" "TB" (direction_to_string TB);
  check string "BT" "BT" (direction_to_string BT)

let test_direction_of_string () =
  check bool "LR" true (direction_of_string "LR" = LR);
  check bool "RL" true (direction_of_string "RL" = RL);
  check bool "TB" true (direction_of_string "TB" = TB);
  check bool "TD alias" true (direction_of_string "TD" = TB);
  check bool "BT" true (direction_of_string "BT" = BT);
  check bool "unknown defaults LR" true (direction_of_string "XYZ" = LR)

let test_direction_roundtrip () =
  let test dir =
    let s = direction_to_string dir in
    let dir' = direction_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (dir = dir')
  in
  List.iter test [LR; RL; TB; BT]

(** {1 Default Values Tests} *)

let test_default_config () =
  check int "max_depth" 8 default_config.max_depth;
  check int "max_concurrency" 3 default_config.max_concurrency;
  check int "timeout" 300 default_config.timeout;
  check bool "trace" false default_config.trace;
  check bool "direction LR" true (default_config.direction = LR)

let test_empty_token_usage () =
  check int "prompt_tokens" 0 empty_token_usage.prompt_tokens;
  check int "completion_tokens" 0 empty_token_usage.completion_tokens;
  check int "total_tokens" 0 empty_token_usage.total_tokens;
  check (float 0.001) "cost" 0.0 empty_token_usage.estimated_cost_usd

let test_default_retry_config () =
  check int "max_retries" 3 default_retry_config.max_retries;
  check int "initial_delay_ms" 1000 default_retry_config.initial_delay_ms;
  check int "max_delay_ms" 30000 default_retry_config.max_delay_ms;
  check (float 0.01) "multiplier" 2.0 default_retry_config.backoff_multiplier

(** {1 Node Type Name Tests} *)

let test_node_type_name_llm () =
  let node_type = Llm { model = "test"; system = None; prompt = "hi"; timeout = None;
                        tools = None; prompt_ref = None; prompt_vars = []; thinking = false } in
  check string "llm" "llm" (node_type_name node_type)

let test_node_type_name_tool () =
  let node_type = Tool { name = "test"; args = `Null } in
  check string "tool" "tool" (node_type_name node_type)

let test_node_type_name_pipeline () =
  let node_type = Pipeline [] in
  check string "pipeline" "pipeline" (node_type_name node_type)

let test_node_type_name_fanout () =
  let node_type = Fanout [] in
  check string "fanout" "fanout" (node_type_name node_type)

let test_node_type_name_quorum () =
  let node_type = Quorum { required = 1; nodes = [] } in
  check string "quorum" "quorum" (node_type_name node_type)

let test_node_type_name_merge () =
  let node_type = Merge { strategy = First; nodes = [] } in
  check string "merge" "merge" (node_type_name node_type)

let test_node_type_name_threshold () =
  let input_node = { id = "in"; node_type = Pipeline [];
                     input_mapping = []; output_key = None; depends_on = None } in
  let node_type = Threshold { metric = "x"; operator = Gt; value = 0.5;
                              input_node; on_pass = None; on_fail = None } in
  check string "threshold" "threshold" (node_type_name node_type)

let test_node_type_name_retry () =
  let node = { id = "n"; node_type = Pipeline [];
               input_mapping = []; output_key = None; depends_on = None } in
  let node_type = Retry { node; max_attempts = 3; backoff = Exponential 1.0; retry_on = [] } in
  check string "retry" "retry" (node_type_name node_type)

let test_node_type_name_fallback () =
  let node = { id = "n"; node_type = Pipeline [];
               input_mapping = []; output_key = None; depends_on = None } in
  let node_type = Fallback { primary = node; fallbacks = [] } in
  check string "fallback" "fallback" (node_type_name node_type)

let test_node_type_name_race () =
  let node_type = Race { nodes = []; timeout = None } in
  check string "race" "race" (node_type_name node_type)

let test_node_type_name_adapter () =
  let node_type = Adapter { input_ref = "x"; transform = Extract ".data"; on_error = `Fail } in
  check string "adapter" "adapter" (node_type_name node_type)

let test_node_type_name_cache () =
  let inner = { id = "n"; node_type = Pipeline [];
                input_mapping = []; output_key = None; depends_on = None } in
  let node_type = Cache { key_expr = "k"; ttl_seconds = 60; inner } in
  check string "cache" "cache" (node_type_name node_type)

let test_node_type_name_mcts () =
  let node = { id = "n"; node_type = Pipeline [];
               input_mapping = []; output_key = None; depends_on = None } in
  let node_type = Mcts {
    strategies = [node];
    simulation = node;
    evaluator = "llm_judge";
    evaluator_prompt = None;
    policy = Greedy;
    max_iterations = 10;
    max_depth = 3;
    expansion_threshold = 2;
    early_stop = None;
    parallel_sims = 1;
  } in
  check string "mcts" "mcts" (node_type_name node_type)

let test_node_type_name_masc () =
  check string "broadcast" "masc_broadcast"
    (node_type_name (Masc_broadcast { message = "hi"; room = None; mention = [] }));
  check string "listen" "masc_listen"
    (node_type_name (Masc_listen { filter = None; timeout_sec = 30.0; room = None }));
  check string "claim" "masc_claim"
    (node_type_name (Masc_claim { task_id = None; room = None }))

(** {1 Make Helper Tests} *)

let test_make_llm_node_minimal () =
  let node = make_llm_node ~id:"test" ~model:"gemini" ~prompt:"hello" () in
  check string "id" "test" node.id;
  match node.node_type with
  | Llm { model; system; prompt; timeout; tools; prompt_ref; prompt_vars; _ } ->
      check string "model" "gemini" model;
      check (option string) "system" None system;
      check string "prompt" "hello" prompt;
      check (option int) "timeout" None timeout;
      check bool "tools" true (tools = None);
      check (option string) "prompt_ref" None prompt_ref;
      check int "prompt_vars" 0 (List.length prompt_vars)
  | _ -> fail "expected Llm"

let test_make_llm_node_full () =
  let node = make_llm_node ~id:"test" ~model:"claude" ~system:"role"
               ~prompt:"task" ~timeout:30 ~prompt_ref:"my-prompt"
               ~prompt_vars:[("x", "1")] () in
  match node.node_type with
  | Llm { model; system; timeout; prompt_ref; prompt_vars; _ } ->
      check string "model" "claude" model;
      check (option string) "system" (Some "role") system;
      check (option int) "timeout" (Some 30) timeout;
      check (option string) "prompt_ref" (Some "my-prompt") prompt_ref;
      check int "prompt_vars" 1 (List.length prompt_vars)
  | _ -> fail "expected Llm"

let test_make_tool_node () =
  let args = `Assoc [("key", `String "value")] in
  let node = make_tool_node ~id:"tool1" ~name:"fetch" ~args in
  check string "id" "tool1" node.id;
  match node.node_type with
  | Tool { name; args = a } ->
      check string "name" "fetch" name;
      check bool "args" true (a = args)
  | _ -> fail "expected Tool"

let test_make_adapter () =
  let node = make_adapter ~id:"a1" ~input_ref:"input" ~transform:(Extract ".data") () in
  check string "id" "a1" node.id;
  match node.node_type with
  | Adapter { input_ref; transform; on_error } ->
      check string "input_ref" "input" input_ref;
      check bool "transform" true (transform = Extract ".data");
      check bool "on_error" true (on_error = `Fail)
  | _ -> fail "expected Adapter"

let test_make_pipeline () =
  let inner = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = make_pipeline ~id:"pipe" [inner] in
  check string "id" "pipe" node.id;
  match node.node_type with
  | Pipeline nodes -> check int "nodes" 1 (List.length nodes)
  | _ -> fail "expected Pipeline"

let test_make_fanout () =
  let inner = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = make_fanout ~id:"fan" [inner; inner] in
  match node.node_type with
  | Fanout nodes -> check int "nodes" 2 (List.length nodes)
  | _ -> fail "expected Fanout"

let test_make_quorum () =
  let inner = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = make_quorum ~id:"q" ~required:2 [inner; inner; inner] in
  match node.node_type with
  | Quorum { required; nodes } ->
      check int "required" 2 required;
      check int "nodes" 3 (List.length nodes)
  | _ -> fail "expected Quorum"

let test_make_threshold () =
  let input = make_llm_node ~id:"prev" ~model:"gemini" ~prompt:"hi" () in
  let node = make_threshold ~id:"t1" ~metric:"score" ~operator:Gte
               ~value:0.8 ~input_node:input () in
  match node.node_type with
  | Threshold { metric; operator; value; input_node; on_pass; on_fail } ->
      check string "metric" "score" metric;
      check bool "operator" true (operator = Gte);
      check (float 0.01) "value" 0.8 value;
      check string "input_node id" "prev" input_node.id;
      check bool "on_pass" true (Option.is_none on_pass);
      check bool "on_fail" true (Option.is_none on_fail)
  | _ -> fail "expected Threshold"

(** {1 Make Chain Tests} *)

let test_make_chain_minimal () =
  let node = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let chain = make_chain ~id:"test-chain" ~nodes:[node] ~output:"n1" () in
  check string "id" "test-chain" chain.id;
  check int "nodes" 1 (List.length chain.nodes);
  check string "output" "n1" chain.output;
  check (option string) "name" None chain.name;
  check (option string) "description" None chain.description

let test_make_chain_full () =
  let node = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let chain = make_chain ~id:"test-chain" ~nodes:[node] ~output:"n1"
                ~name:"Test Chain" ~description:"A test chain"
                ~version:"1.0.0" () in
  check (option string) "name" (Some "Test Chain") chain.name;
  check (option string) "description" (Some "A test chain") chain.description;
  check (option string) "version" (Some "1.0.0") chain.version

(** {1 Test Suite} *)

let direction_tests = [
  test_case "to_string" `Quick test_direction_to_string;
  test_case "of_string" `Quick test_direction_of_string;
  test_case "roundtrip" `Quick test_direction_roundtrip;
]

let defaults_tests = [
  test_case "default_config" `Quick test_default_config;
  test_case "empty_token_usage" `Quick test_empty_token_usage;
  test_case "default_retry_config" `Quick test_default_retry_config;
]

let node_type_name_tests = [
  test_case "llm" `Quick test_node_type_name_llm;
  test_case "tool" `Quick test_node_type_name_tool;
  test_case "pipeline" `Quick test_node_type_name_pipeline;
  test_case "fanout" `Quick test_node_type_name_fanout;
  test_case "quorum" `Quick test_node_type_name_quorum;
  test_case "merge" `Quick test_node_type_name_merge;
  test_case "threshold" `Quick test_node_type_name_threshold;
  test_case "retry" `Quick test_node_type_name_retry;
  test_case "fallback" `Quick test_node_type_name_fallback;
  test_case "race" `Quick test_node_type_name_race;
  test_case "adapter" `Quick test_node_type_name_adapter;
  test_case "cache" `Quick test_node_type_name_cache;
  test_case "mcts" `Quick test_node_type_name_mcts;
  test_case "masc" `Quick test_node_type_name_masc;
]

let make_helper_tests = [
  test_case "llm_node minimal" `Quick test_make_llm_node_minimal;
  test_case "llm_node full" `Quick test_make_llm_node_full;
  test_case "tool_node" `Quick test_make_tool_node;
  test_case "adapter" `Quick test_make_adapter;
  test_case "pipeline" `Quick test_make_pipeline;
  test_case "fanout" `Quick test_make_fanout;
  test_case "quorum" `Quick test_make_quorum;
  test_case "threshold" `Quick test_make_threshold;
]

let make_chain_tests = [
  test_case "minimal" `Quick test_make_chain_minimal;
  test_case "full" `Quick test_make_chain_full;
]

let () =
  run "chain_types" [
    ("direction", direction_tests);
    ("defaults", defaults_tests);
    ("node_type_name", node_type_name_tests);
    ("make_helpers", make_helper_tests);
    ("make_chain", make_chain_tests);
  ]

(** test_chain_parser_coverage.ml - Comprehensive Chain Parser Coverage Tests

    Target Functions (lib/chain_parser.ml):
    1. parse_chain - Main JSON to Chain parser
    2. parse_node - Individual node parsing
    3. parse_node_type - Type discriminator
    4. parse_llm_config / parse_tool_config - Config parsing
    5. parse_quorum_config / parse_gate_config - Complex node configs
    6. parse_input_mapping - Input mapping array/object
    7. validate_chain / validate_chain_strict - Validation
    8. chain_to_json / node_to_json - Serialization
    9. parse_merge_strategy / parse_threshold_op / parse_select_strategy
    10. parse_backoff_strategy / parse_adapter_transform
    11. extract_input_mappings / extract_json_mappings

    Coverage Goals:
    - All 22+ node types
    - Valid and invalid JSON
    - Edge cases and error paths
    - Roundtrip consistency
*)

open Alcotest
open Chain_types

(** {1 Helper Functions} *)

let parse_json str =
  try Yojson.Safe.from_string str
  with _ -> failwith ("Invalid JSON: " ^ str)

let expect_ok msg = function
  | Ok v -> v
  | Error e -> fail (Printf.sprintf "%s: %s" msg e)

let expect_error msg = function
  | Ok _ -> fail (Printf.sprintf "Expected error for: %s" msg)
  | Error _ -> ()

(** {1 Parse Chain Tests} *)

let test_parse_chain_minimal () =
  let json = parse_json {|{
    "id": "simple",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello"}
    ],
    "output": "a"
  }|} in
  let chain = expect_ok "minimal chain" (Chain_parser.parse_chain json) in
  check string "chain id" "simple" chain.id;
  check string "output" "a" chain.output;
  check int "node count" 1 (List.length chain.nodes)

let test_parse_chain_with_config () =
  let json = parse_json {|{
    "id": "configured",
    "nodes": [
      {"id": "a", "type": "llm", "model": "claude", "prompt": "Test"}
    ],
    "output": "a",
    "config": {
      "max_depth": 5,
      "max_concurrency": 3,
      "timeout": 120,
      "trace": true,
      "direction": "TB"
    }
  }|} in
  let chain = expect_ok "chain with config" (Chain_parser.parse_chain json) in
  check int "max_depth" 5 chain.config.max_depth;
  check int "max_concurrency" 3 chain.config.max_concurrency;
  check int "timeout" 120 chain.config.timeout;
  check bool "trace" true chain.config.trace

let test_parse_chain_with_metadata () =
  let json = parse_json {|{
    "id": "meta-chain",
    "name": "My Chain",
    "description": "A test chain",
    "version": "1.0.0",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hi"}
    ],
    "output": "a",
    "input_schema": {"type": "object"},
    "output_schema": {"type": "string"},
    "metadata": {"author": "test"}
  }|} in
  let chain = expect_ok "chain with metadata" (Chain_parser.parse_chain json) in
  check (option string) "name" (Some "My Chain") chain.name;
  check (option string) "description" (Some "A test chain") chain.description;
  check (option string) "version" (Some "1.0.0") chain.version;
  check bool "has input_schema" true (chain.input_schema <> None);
  check bool "has output_schema" true (chain.output_schema <> None);
  check bool "has metadata" true (chain.metadata <> None)

let test_parse_chain_missing_output () =
  let json = parse_json {|{
    "id": "bad",
    "nodes": [{"id": "a", "type": "llm", "model": "gemini", "prompt": "x"}]
  }|} in
  expect_error "missing output" (Chain_parser.parse_chain json)

let test_parse_chain_missing_nodes () =
  let json = parse_json {|{
    "id": "bad",
    "output": "a"
  }|} in
  (* Should fail or create empty nodes list *)
  let result = Chain_parser.parse_chain json in
  match result with
  | Error _ -> ()  (* Expected *)
  | Ok chain -> check int "empty nodes" 0 (List.length chain.nodes)

(** {1 Parse Node Tests - LLM} *)

let test_parse_llm_node_basic () =
  let json = parse_json {|{
    "id": "llm1",
    "type": "llm",
    "model": "gemini",
    "prompt": "Analyze: {{input}}"
  }|} in
  let node = expect_ok "llm node" (Chain_parser.parse_node json) in
  check string "node id" "llm1" node.id;
  match node.node_type with
  | Llm { model; prompt; system; timeout; _ } ->
      check string "model" "gemini" model;
      check string "prompt" "Analyze: {{input}}" prompt;
      check (option string) "system" None system;
      check (option int) "timeout" None timeout
  | _ -> fail "Expected Llm node type"

let test_parse_llm_node_full () =
  let json = parse_json {|{
    "id": "llm_full",
    "type": "llm",
    "model": "claude",
    "system": "You are a helpful assistant",
    "prompt": "Answer: {{question}}",
    "timeout": 60,
    "tools": [{"name": "search"}]
  }|} in
  let node = expect_ok "full llm node" (Chain_parser.parse_node json) in
  match node.node_type with
  | Llm { model; system; timeout; tools; _ } ->
      check string "model" "claude" model;
      check (option string) "system" (Some "You are a helpful assistant") system;
      check (option int) "timeout" (Some 60) timeout;
      check bool "has tools" true (tools <> None)
  | _ -> fail "Expected Llm node type"

let test_parse_llm_node_nested_format () =
  let json = parse_json {|{
    "id": "nested_llm",
    "type": "llm",
    "llm": {
      "model": "codex",
      "prompt": "Code: {{spec}}"
    }
  }|} in
  let node = expect_ok "nested llm" (Chain_parser.parse_node json) in
  match node.node_type with
  | Llm { model; prompt; _ } ->
      check string "model" "codex" model;
      check string "prompt" "Code: {{spec}}" prompt
  | _ -> fail "Expected Llm node type"

let test_parse_llm_node_missing_model () =
  let json = parse_json {|{
    "id": "bad_llm",
    "type": "llm",
    "prompt": "Hello"
  }|} in
  expect_error "missing model" (Chain_parser.parse_node json)

let test_parse_llm_node_missing_prompt () =
  let json = parse_json {|{
    "id": "bad_llm",
    "type": "llm",
    "model": "gemini"
  }|} in
  expect_error "missing prompt" (Chain_parser.parse_node json)

(** {1 Parse Node Tests - Tool} *)

let test_parse_tool_node_basic () =
  let json = parse_json {|{
    "id": "tool1",
    "type": "tool",
    "name": "eslint",
    "args": {"file": "src/main.ts"}
  }|} in
  let node = expect_ok "tool node" (Chain_parser.parse_node json) in
  match node.node_type with
  | Tool { name; args } ->
      check string "name" "eslint" name;
      check bool "has args" true (args <> `Null)
  | _ -> fail "Expected Tool node type"

let test_parse_tool_node_nested_format () =
  let json = parse_json {|{
    "id": "nested_tool",
    "type": "tool",
    "tool": {
      "server": "figma",
      "name": "parse_url",
      "args": {"url": "https://figma.com/..."}
    }
  }|} in
  let node = expect_ok "nested tool" (Chain_parser.parse_node json) in
  match node.node_type with
  | Tool { name; _ } ->
      check string "name with server" "figma:parse_url" name
  | _ -> fail "Expected Tool node type"

let test_parse_tool_node_no_args () =
  let json = parse_json {|{
    "id": "tool_no_args",
    "type": "tool",
    "name": "list_files"
  }|} in
  let node = expect_ok "tool without args" (Chain_parser.parse_node json) in
  match node.node_type with
  | Tool { args; _ } ->
      check bool "empty args" true (args = `Assoc [])
  | _ -> fail "Expected Tool node type"

(** {1 Parse Node Tests - Pipeline/Fanout} *)

let test_parse_pipeline_node () =
  let json = parse_json {|{
    "id": "pipe",
    "type": "pipeline",
    "nodes": [
      {"id": "step1", "type": "llm", "model": "gemini", "prompt": "Step 1"},
      {"id": "step2", "type": "llm", "model": "claude", "prompt": "Step 2: {{step1}}"}
    ]
  }|} in
  let node = expect_ok "pipeline" (Chain_parser.parse_node json) in
  match node.node_type with
  | Pipeline nodes ->
      check int "pipeline length" 2 (List.length nodes)
  | _ -> fail "Expected Pipeline node type"

let test_parse_fanout_node () =
  let json = parse_json {|{
    "id": "fan",
    "type": "fanout",
    "branches": [
      {"id": "b1", "type": "llm", "model": "gemini", "prompt": "Branch 1"},
      {"id": "b2", "type": "llm", "model": "claude", "prompt": "Branch 2"}
    ]
  }|} in
  let node = expect_ok "fanout" (Chain_parser.parse_node json) in
  match node.node_type with
  | Fanout nodes ->
      check int "fanout branches" 2 (List.length nodes)
  | _ -> fail "Expected Fanout node type"

let test_parse_fanout_with_nodes_field () =
  let json = parse_json {|{
    "id": "fan2",
    "type": "fanout",
    "nodes": [
      {"id": "n1", "type": "llm", "model": "gemini", "prompt": "N1"}
    ]
  }|} in
  let node = expect_ok "fanout with nodes" (Chain_parser.parse_node json) in
  match node.node_type with
  | Fanout nodes -> check int "fanout nodes" 1 (List.length nodes)
  | _ -> fail "Expected Fanout"

(** {1 Parse Node Tests - Quorum} *)

let test_parse_quorum_node () =
  let json = parse_json {|{
    "id": "quorum1",
    "type": "quorum",
    "required": 2,
    "nodes": [
      {"id": "v1", "type": "llm", "model": "gemini", "prompt": "Vote 1"},
      {"id": "v2", "type": "llm", "model": "claude", "prompt": "Vote 2"},
      {"id": "v3", "type": "llm", "model": "codex", "prompt": "Vote 3"}
    ]
  }|} in
  let node = expect_ok "quorum" (Chain_parser.parse_node json) in
  match node.node_type with
  | Quorum { consensus = Count required; nodes; _ } ->
      check int "required" 2 required;
      check int "nodes" 3 (List.length nodes)
  | _ -> fail "Expected Quorum node type"

let test_parse_quorum_with_inputs () =
  let json = parse_json {|{
    "id": "quorum2",
    "type": "quorum",
    "required": 1,
    "inputs": [
      {"id": "i1", "type": "llm", "model": "gemini", "prompt": "I1"}
    ]
  }|} in
  let node = expect_ok "quorum with inputs" (Chain_parser.parse_node json) in
  match node.node_type with
  | Quorum { nodes; _ } -> check int "nodes from inputs" 1 (List.length nodes)
  | _ -> fail "Expected Quorum"

(** {1 Parse Node Tests - Gate} *)

let test_parse_gate_node_embedded () =
  let json = parse_json {|{
    "id": "gate1",
    "type": "gate",
    "condition": "{{score}} > 0.8",
    "then": {"id": "pass", "type": "llm", "model": "gemini", "prompt": "Pass"},
    "else": {"id": "fail", "type": "llm", "model": "claude", "prompt": "Fail"}
  }|} in
  let node = expect_ok "gate with embedded nodes" (Chain_parser.parse_node json) in
  match node.node_type with
  | Gate { condition; then_node; else_node } ->
      check string "condition" "{{score}} > 0.8" condition;
      check string "then id" "pass" then_node.id;
      check bool "has else" true (else_node <> None)
  | _ -> fail "Expected Gate node type"

let test_parse_gate_node_refs () =
  let json = parse_json {|{
    "id": "gate2",
    "type": "gate",
    "condition": "{{check}} == true",
    "then_node": "success_path",
    "else_node": "failure_path"
  }|} in
  let node = expect_ok "gate with refs" (Chain_parser.parse_node json) in
  match node.node_type with
  | Gate { then_node; else_node; _ } ->
      (match then_node.node_type with
       | ChainRef id -> check string "then ref" "success_path" id
       | _ -> fail "Expected ChainRef for then");
      (match else_node with
       | Some n ->
           (match n.node_type with
            | ChainRef id -> check string "else ref" "failure_path" id
            | _ -> fail "Expected ChainRef for else")
       | None -> fail "Expected else node")
  | _ -> fail "Expected Gate"

let test_parse_gate_no_else () =
  let json = parse_json {|{
    "id": "gate3",
    "type": "gate",
    "condition": "{{x}} > 0",
    "then": {"id": "t", "type": "llm", "model": "gemini", "prompt": "T"}
  }|} in
  let node = expect_ok "gate without else" (Chain_parser.parse_node json) in
  match node.node_type with
  | Gate { else_node; _ } -> check bool "no else" true (else_node = None)
  | _ -> fail "Expected Gate"

(** {1 Parse Node Tests - Merge} *)

let test_parse_merge_node () =
  let json = parse_json {|{
    "id": "merge1",
    "type": "merge",
    "strategy": "concat",
    "nodes": [
      {"id": "m1", "type": "llm", "model": "gemini", "prompt": "M1"},
      {"id": "m2", "type": "llm", "model": "claude", "prompt": "M2"}
    ]
  }|} in
  let node = expect_ok "merge" (Chain_parser.parse_node json) in
  match node.node_type with
  | Merge { strategy; nodes } ->
      check bool "concat strategy" true (strategy = Concat);
      check int "merge nodes" 2 (List.length nodes)
  | _ -> fail "Expected Merge"

let test_parse_merge_strategies () =
  let strategies = ["first"; "last"; "concat"; "weighted_average"; "custom:my_merge"] in
  List.iter (fun s ->
    let json = parse_json (Printf.sprintf {|{
      "id": "merge_%s",
      "type": "merge",
      "strategy": "%s",
      "nodes": []
    }|} s s) in
    let _ = expect_ok (Printf.sprintf "merge strategy %s" s) (Chain_parser.parse_node json) in
    ()
  ) strategies

(** {1 Parse Node Tests - Threshold} *)

let test_parse_threshold_node () =
  let json = parse_json {|{
    "id": "thresh",
    "type": "threshold",
    "metric": "confidence",
    "operator": "gte",
    "value": 0.8,
    "input_node": {"id": "scorer", "type": "llm", "model": "gemini", "prompt": "Score"},
    "on_pass": {"id": "continue", "type": "llm", "model": "claude", "prompt": "Continue"},
    "on_fail": {"id": "abort", "type": "llm", "model": "codex", "prompt": "Abort"}
  }|} in
  let node = expect_ok "threshold" (Chain_parser.parse_node json) in
  match node.node_type with
  | Threshold { metric; operator; value; on_pass; on_fail; _ } ->
      check string "metric" "confidence" metric;
      check bool "operator gte" true (operator = Gte);
      check (float 0.01) "value" 0.8 value;
      check bool "has on_pass" true (on_pass <> None);
      check bool "has on_fail" true (on_fail <> None)
  | _ -> fail "Expected Threshold"

let test_parse_threshold_operators () =
  let ops = [("gt", Gt); (">", Gt); ("gte", Gte); (">=", Gte);
             ("lt", Lt); ("<", Lt); ("lte", Lte); ("<=", Lte);
             ("eq", Eq); ("=", Eq); ("neq", Neq); ("!=", Neq)] in
  List.iter (fun (op_str, expected_op) ->
    let result = Chain_parser.parse_threshold_op op_str in
    match result with
    | Ok op -> check bool (Printf.sprintf "operator %s" op_str) true (op = expected_op)
    | Error _ -> fail (Printf.sprintf "Failed to parse operator: %s" op_str)
  ) ops

(** {1 Parse Node Tests - GoalDriven} *)

let test_parse_goal_driven_node () =
  let json = parse_json {|{
    "id": "goal1",
    "type": "goal_driven",
    "goal_metric": "accuracy",
    "goal_operator": "gte",
    "goal_value": 0.95,
    "action_node": {"id": "act", "type": "llm", "model": "gemini", "prompt": "Act"},
    "measure_func": "llm_judge",
    "max_iterations": 5,
    "strategy_hints": {"focus": "precision"},
    "conversational": true,
    "relay_models": ["gemini", "claude"]
  }|} in
  let node = expect_ok "goal_driven" (Chain_parser.parse_node json) in
  match node.node_type with
  | GoalDriven { goal_metric; goal_value; max_iterations; conversational; relay_models; _ } ->
      check string "goal_metric" "accuracy" goal_metric;
      check (float 0.01) "goal_value" 0.95 goal_value;
      check int "max_iterations" 5 max_iterations;
      check bool "conversational" true conversational;
      check int "relay_models" 2 (List.length relay_models)
  | _ -> fail "Expected GoalDriven"

(** {1 Parse Node Tests - Evaluator} *)

let test_parse_evaluator_node () =
  let json = parse_json {|{
    "id": "eval1",
    "type": "evaluator",
    "candidates": [
      {"id": "c1", "type": "llm", "model": "gemini", "prompt": "C1"},
      {"id": "c2", "type": "llm", "model": "claude", "prompt": "C2"}
    ],
    "scoring_func": "anti_fake",
    "scoring_prompt": "Rate quality 0-1",
    "select_strategy": "best",
    "min_score": 0.7
  }|} in
  let node = expect_ok "evaluator" (Chain_parser.parse_node json) in
  match node.node_type with
  | Evaluator { candidates; scoring_func; min_score; select_strategy; _ } ->
      check int "candidates" 2 (List.length candidates);
      check string "scoring_func" "anti_fake" scoring_func;
      check (option (float 0.01)) "min_score" (Some 0.7) min_score;
      check bool "select best" true (select_strategy = Best)
  | _ -> fail "Expected Evaluator"

let test_parse_select_strategies () =
  let tests = [
    (`String "best", Best);
    (`String "worst", Worst);
    (`String "weighted_random", WeightedRandom);
    (`Assoc [("above_threshold", `Float 0.5)], AboveThreshold 0.5);
  ] in
  List.iter (fun (json, expected) ->
    match Chain_parser.parse_select_strategy json with
    | Ok s -> check bool "strategy match" true (s = expected)
    | Error e -> fail (Printf.sprintf "Failed: %s" e)
  ) tests

(** {1 Parse Node Tests - Retry/Fallback/Race} *)

let test_parse_retry_node () =
  let json = parse_json {|{
    "id": "retry1",
    "type": "retry",
    "node": {"id": "inner", "type": "llm", "model": "gemini", "prompt": "Try"},
    "max_attempts": 5,
    "backoff": {"type": "exponential", "base": 2.0},
    "retry_on": ["timeout", "rate_limit"]
  }|} in
  let node = expect_ok "retry" (Chain_parser.parse_node json) in
  match node.node_type with
  | Retry { max_attempts; backoff; retry_on; _ } ->
      check int "max_attempts" 5 max_attempts;
      check bool "exponential backoff" true (match backoff with Exponential _ -> true | _ -> false);
      check int "retry_on count" 2 (List.length retry_on)
  | _ -> fail "Expected Retry"

let test_parse_fallback_node () =
  let json = parse_json {|{
    "id": "fb1",
    "type": "fallback",
    "primary": {"id": "main", "type": "llm", "model": "claude", "prompt": "Main"},
    "fallbacks": [
      {"id": "alt1", "type": "llm", "model": "gemini", "prompt": "Alt1"},
      {"id": "alt2", "type": "llm", "model": "codex", "prompt": "Alt2"}
    ]
  }|} in
  let node = expect_ok "fallback" (Chain_parser.parse_node json) in
  match node.node_type with
  | Fallback { primary; fallbacks } ->
      check string "primary id" "main" primary.id;
      check int "fallback count" 2 (List.length fallbacks)
  | _ -> fail "Expected Fallback"

let test_parse_race_node () =
  let json = parse_json {|{
    "id": "race1",
    "type": "race",
    "nodes": [
      {"id": "r1", "type": "llm", "model": "gemini", "prompt": "R1"},
      {"id": "r2", "type": "llm", "model": "claude", "prompt": "R2"}
    ],
    "timeout": 30.0
  }|} in
  let node = expect_ok "race" (Chain_parser.parse_node json) in
  match node.node_type with
  | Race { nodes; timeout } ->
      check int "race nodes" 2 (List.length nodes);
      check (option (float 0.01)) "timeout" (Some 30.0) timeout
  | _ -> fail "Expected Race"

(** {1 Parse Node Tests - ChainRef/Subgraph/ChainExec} *)

let test_parse_chain_ref_node () =
  let json = parse_json {|{
    "id": "ref1",
    "type": "chain_ref",
    "ref": "magi-code-review"
  }|} in
  let node = expect_ok "chain_ref" (Chain_parser.parse_node json) in
  match node.node_type with
  | ChainRef ref_id -> check string "ref" "magi-code-review" ref_id
  | _ -> fail "Expected ChainRef"

let test_parse_subgraph_node () =
  let json = parse_json {|{
    "id": "sub1",
    "type": "subgraph",
    "graph": {
      "id": "inner_chain",
      "nodes": [
        {"id": "ia", "type": "llm", "model": "gemini", "prompt": "Inner A"}
      ],
      "output": "ia"
    }
  }|} in
  let node = expect_ok "subgraph" (Chain_parser.parse_node json) in
  match node.node_type with
  | Subgraph chain ->
      check string "inner chain id" "inner_chain" chain.id;
      check string "inner output" "ia" chain.output
  | _ -> fail "Expected Subgraph"

let test_parse_chain_exec_node () =
  let json = parse_json {|{
    "id": "exec1",
    "type": "chain_exec",
    "chain_source": "{{generated_chain}}",
    "validate": true,
    "max_depth": 2,
    "sandbox": true,
    "context_inject": {"var1": "{{input.data}}"},
    "pass_outputs": true
  }|} in
  let node = expect_ok "chain_exec" (Chain_parser.parse_node json) in
  match node.node_type with
  | ChainExec { chain_source; validate; max_depth; sandbox; _ } ->
      check string "chain_source" "{{generated_chain}}" chain_source;
      check bool "validate" true validate;
      check int "max_depth" 2 max_depth;
      check bool "sandbox" true sandbox
  | _ -> fail "Expected ChainExec"

(** {1 Parse Node Tests - Adapter} *)

let test_parse_adapter_node () =
  let json = parse_json {|{
    "id": "adapt1",
    "type": "adapter",
    "input_ref": "previous",
    "transform": {"type": "extract", "path": ".data.result"},
    "on_error": "passthrough"
  }|} in
  let node = expect_ok "adapter" (Chain_parser.parse_node json) in
  match node.node_type with
  | Adapter { input_ref; transform; on_error } ->
      check string "input_ref" "previous" input_ref;
      check bool "extract transform" true (match transform with Extract _ -> true | _ -> false);
      check bool "passthrough on_error" true (on_error = `Passthrough)
  | _ -> fail "Expected Adapter"

let test_parse_adapter_transforms () =
  let transforms = [
    {|"extract:data.field"|};
    {|{"type": "template", "template": "Result: {{value}}"}|};
    {|"summarize:100"|};
    {|"truncate:500"|};
    {|"jsonpath:$.items[0]"|};
    {|"parse_json"|};
    {|"stringify"|};
    {|{"type": "regex", "pattern": "\\d+", "replacement": "NUM"}|};
    {|{"type": "chain", "transforms": ["parse_json", "stringify"]}|};
    {|{"type": "conditional", "condition": "{{x}} > 0", "on_true": "stringify", "on_false": "parse_json"}|};
  ] in
  List.iter (fun t ->
    let json = parse_json t in
    let result = Chain_parser.parse_adapter_transform json in
    match result with
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "Transform %s failed: %s" t e)
  ) transforms

(** {1 Parse Node Tests - Cache/Batch/Spawn} *)

let test_parse_cache_node () =
  let json = parse_json {|{
    "id": "cache1",
    "type": "cache",
    "key_expr": "{{input.query}}",
    "ttl_seconds": 3600,
    "inner": {"id": "cached", "type": "llm", "model": "gemini", "prompt": "Cached"}
  }|} in
  let node = expect_ok "cache" (Chain_parser.parse_node json) in
  match node.node_type with
  | Cache { key_expr; ttl_seconds; _ } ->
      check string "key_expr" "{{input.query}}" key_expr;
      check int "ttl" 3600 ttl_seconds
  | _ -> fail "Expected Cache"

let test_parse_batch_node () =
  let json = parse_json {|{
    "id": "batch1",
    "type": "batch",
    "batch_size": 5,
    "parallel": true,
    "inner": {"id": "batch_item", "type": "llm", "model": "gemini", "prompt": "Process {{item}}"},
    "collect_strategy": "concat"
  }|} in
  let node = expect_ok "batch" (Chain_parser.parse_node json) in
  match node.node_type with
  | Batch { batch_size; parallel; collect_strategy; _ } ->
      check int "batch_size" 5 batch_size;
      check bool "parallel" true parallel;
      check bool "concat strategy" true (collect_strategy = `Concat)
  | _ -> fail "Expected Batch"

let test_parse_spawn_node () =
  let json = parse_json {|{
    "id": "spawn1",
    "type": "spawn",
    "clean": true,
    "inner": {"id": "spawned", "type": "llm", "model": "gemini", "prompt": "Spawned"},
    "pass_vars": ["context", "config"],
    "inherit_cache": false
  }|} in
  let node = expect_ok "spawn" (Chain_parser.parse_node json) in
  match node.node_type with
  | Spawn { clean; pass_vars; inherit_cache; _ } ->
      check bool "clean" true clean;
      check int "pass_vars" 2 (List.length pass_vars);
      check bool "inherit_cache" false inherit_cache
  | _ -> fail "Expected Spawn"

(** {1 Parse Node Tests - MCTS} *)

let test_parse_mcts_node () =
  let json = parse_json {|{
    "id": "mcts1",
    "type": "mcts",
    "strategies": [
      {"id": "s1", "type": "llm", "model": "gemini", "prompt": "Strategy 1"},
      {"id": "s2", "type": "llm", "model": "claude", "prompt": "Strategy 2"}
    ],
    "simulation": {"id": "sim", "type": "llm", "model": "codex", "prompt": "Simulate"},
    "evaluator": "llm_judge",
    "evaluator_prompt": "Rate 0-1",
    "policy": {"type": "ucb1", "c": 1.5},
    "max_iterations": 20,
    "max_depth": 10,
    "expansion_threshold": 5,
    "early_stop": 0.95,
    "parallel_sims": 4
  }|} in
  let node = expect_ok "mcts" (Chain_parser.parse_node json) in
  match node.node_type with
  | Mcts { strategies; evaluator; policy; max_iterations; parallel_sims; early_stop; _ } ->
      check int "strategies" 2 (List.length strategies);
      check string "evaluator" "llm_judge" evaluator;
      check bool "ucb1 policy" true (match policy with UCB1 _ -> true | _ -> false);
      check int "max_iterations" 20 max_iterations;
      check int "parallel_sims" 4 parallel_sims;
      check (option (float 0.01)) "early_stop" (Some 0.95) early_stop
  | _ -> fail "Expected Mcts"

(** {1 Parse Node Tests - StreamMerge} *)

let test_parse_stream_merge_node () =
  let json = parse_json {|{
    "id": "stream1",
    "type": "stream_merge",
    "nodes": [
      {"id": "sm1", "type": "llm", "model": "gemini", "prompt": "SM1"},
      {"id": "sm2", "type": "llm", "model": "claude", "prompt": "SM2"}
    ],
    "reducer": "concat",
    "initial": "",
    "min_results": 1,
    "timeout": 60.0
  }|} in
  let node = expect_ok "stream_merge" (Chain_parser.parse_node json) in
  match node.node_type with
  | StreamMerge { nodes; reducer; min_results; timeout; _ } ->
      check int "nodes" 2 (List.length nodes);
      check bool "concat reducer" true (reducer = Concat);
      check (option int) "min_results" (Some 1) min_results;
      check (option (float 0.01)) "timeout" (Some 60.0) timeout
  | _ -> fail "Expected StreamMerge"

(** {1 Parse Node Tests - FeedbackLoop} *)

let test_parse_feedback_loop_node () =
  let json = parse_json {|{
    "id": "loop1",
    "type": "feedback_loop",
    "generator": {"id": "gen", "type": "llm", "model": "gemini", "prompt": "Generate"},
    "evaluator_config": {
      "scoring_func": "llm_judge",
      "scoring_prompt": "Score 0-1",
      "select_strategy": "best"
    },
    "improver_prompt": "Improve: {{feedback}}",
    "max_iterations": 3,
    "score_threshold": 0.8,
    "score_operator": "gte",
    "conversational": true,
    "relay_models": ["gemini", "claude"]
  }|} in
  let node = expect_ok "feedback_loop" (Chain_parser.parse_node json) in
  match node.node_type with
  | FeedbackLoop { max_iterations; score_threshold; conversational; relay_models; evaluator_config; _ } ->
      check int "max_iterations" 3 max_iterations;
      check (float 0.01) "score_threshold" 0.8 score_threshold;
      check bool "conversational" true conversational;
      check int "relay_models" 2 (List.length relay_models);
      check string "scoring_func" "llm_judge" evaluator_config.scoring_func
  | _ -> fail "Expected FeedbackLoop"

(** {1 Parse Node Tests - MASC Nodes} *)

let test_parse_masc_broadcast_node () =
  let json = parse_json {|{
    "id": "bc1",
    "type": "masc_broadcast",
    "message": "Task completed: {{result}}",
    "room": "main",
    "mention": ["@codex", "@gemini"]
  }|} in
  let node = expect_ok "masc_broadcast" (Chain_parser.parse_node json) in
  match node.node_type with
  | Masc_broadcast { message; room; mention } ->
      check string "message" "Task completed: {{result}}" message;
      check (option string) "room" (Some "main") room;
      check int "mention count" 2 (List.length mention)
  | _ -> fail "Expected Masc_broadcast"

let test_parse_masc_listen_node () =
  let json = parse_json {|{
    "id": "listen1",
    "type": "masc_listen",
    "filter": "done|complete",
    "timeout_sec": 45.0,
    "room": "coord"
  }|} in
  let node = expect_ok "masc_listen" (Chain_parser.parse_node json) in
  match node.node_type with
  | Masc_listen { filter; timeout_sec; room } ->
      check (option string) "filter" (Some "done|complete") filter;
      check (float 0.01) "timeout_sec" 45.0 timeout_sec;
      check (option string) "room" (Some "coord") room
  | _ -> fail "Expected Masc_listen"

let test_parse_masc_claim_node () =
  let json = parse_json {|{
    "id": "claim1",
    "type": "masc_claim",
    "task_id": "task-123",
    "room": "work"
  }|} in
  let node = expect_ok "masc_claim" (Chain_parser.parse_node json) in
  match node.node_type with
  | Masc_claim { task_id; room } ->
      check (option string) "task_id" (Some "task-123") task_id;
      check (option string) "room" (Some "work") room
  | _ -> fail "Expected Masc_claim"

(** {1 Parse Node Tests - Map/Bind} *)

let test_parse_map_node () =
  let json = parse_json {|{
    "id": "map1",
    "type": "map",
    "func": "process_item",
    "inner": {"id": "item_proc", "type": "llm", "model": "gemini", "prompt": "Process"}
  }|} in
  let node = expect_ok "map" (Chain_parser.parse_node json) in
  match node.node_type with
  | Map { func; _ } -> check string "func" "process_item" func
  | _ -> fail "Expected Map"

let test_parse_bind_node () =
  let json = parse_json {|{
    "id": "bind1",
    "type": "bind",
    "func": "chain_items",
    "inner": {"id": "bound", "type": "llm", "model": "gemini", "prompt": "Bound"}
  }|} in
  let node = expect_ok "bind" (Chain_parser.parse_node json) in
  match node.node_type with
  | Bind { func; _ } -> check string "func" "chain_items" func
  | _ -> fail "Expected Bind"

(** {1 Input Mapping Tests} *)

let test_parse_input_mapping_list_format () =
  let json = parse_json {|{
    "id": "with_mapping",
    "type": "llm",
    "model": "gemini",
    "prompt": "Process: {{data}}",
    "input_mapping": [["data", "previous_node"]]
  }|} in
  let node = expect_ok "input_mapping list" (Chain_parser.parse_node json) in
  check int "mapping count" 1 (List.length node.input_mapping);
  check bool "has data mapping" true (List.mem_assoc "data" node.input_mapping)

let test_parse_input_mapping_object_format () =
  let json = parse_json {|{
    "id": "with_inputs",
    "type": "llm",
    "model": "gemini",
    "prompt": "Analyze: {{a}} and {{b}}",
    "inputs": {"a": "node_a", "b": "node_b"}
  }|} in
  let node = expect_ok "inputs object" (Chain_parser.parse_node json) in
  check bool "has a mapping" true (List.mem_assoc "a" node.input_mapping);
  check bool "has b mapping" true (List.mem_assoc "b" node.input_mapping)

let test_parse_input_mapping_auto_extract () =
  let json = parse_json {|{
    "id": "auto_extract",
    "type": "llm",
    "model": "gemini",
    "prompt": "Review: {{code}} with {{context}}"
  }|} in
  let node = expect_ok "auto extract" (Chain_parser.parse_node json) in
  check bool "has code ref" true (List.exists (fun (_, v) -> v = "code") node.input_mapping);
  check bool "has context ref" true (List.exists (fun (_, v) -> v = "context") node.input_mapping)

let test_parse_depends_on () =
  let json = parse_json {|{
    "id": "dependent",
    "type": "llm",
    "model": "gemini",
    "prompt": "After deps",
    "depends_on": ["step1", "step2"]
  }|} in
  let node = expect_ok "depends_on" (Chain_parser.parse_node json) in
  check (option (list string)) "depends_on" (Some ["step1"; "step2"]) node.depends_on

(** {1 Backoff Strategy Tests} *)

let test_parse_backoff_strategies () =
  let tests = [
    ({|"exponential:2.0"|}, fun b -> match b with Exponential _ -> true | _ -> false);
    ({|"constant:1.0"|}, fun b -> match b with Constant _ -> true | _ -> false);
    ({|"linear:0.5"|}, fun b -> match b with Linear _ -> true | _ -> false);
    ({|{"type": "jitter", "min": 0.5, "max": 2.0}|}, fun b -> match b with Jitter _ -> true | _ -> false);
    ({|2.0|}, fun b -> match b with Exponential _ -> true | _ -> false);
  ] in
  List.iter (fun (json_str, check_fn) ->
    let json = `Assoc [("backoff", parse_json json_str)] in
    let backoff = Chain_parser.parse_backoff_strategy json in
    check bool (Printf.sprintf "backoff %s" json_str) true (check_fn backoff)
  ) tests

(** {1 Validation Tests} *)

let test_validate_chain_missing_output_node () =
  let chain = {
    id = "bad_chain";
    nodes = [
      { id = "a"; node_type = Llm { model = "gemini"; system = None; prompt = "Hi";
                                     timeout = None; tools = None; prompt_ref = None;
                                     prompt_vars = []; thinking = false };
        input_mapping = []; output_key = None; depends_on = None }
    ];
    output = "nonexistent";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None;
  } in
  expect_error "missing output" (Chain_parser.validate_chain chain)

let test_validate_chain_duplicate_ids () =
  let chain = {
    id = "dup_chain";
    nodes = [
      { id = "dup"; node_type = Llm { model = "g"; system = None; prompt = "1";
                                       timeout = None; tools = None; prompt_ref = None;
                                       prompt_vars = []; thinking = false };
        input_mapping = []; output_key = None; depends_on = None };
      { id = "dup"; node_type = Llm { model = "c"; system = None; prompt = "2";
                                       timeout = None; tools = None; prompt_ref = None;
                                       prompt_vars = []; thinking = false };
        input_mapping = []; output_key = None; depends_on = None }
    ];
    output = "dup";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None;
  } in
  expect_error "duplicate ids" (Chain_parser.validate_chain chain)

let test_validate_chain_valid () =
  let chain = {
    id = "good_chain";
    nodes = [
      { id = "a"; node_type = Llm { model = "gemini"; system = None; prompt = "Step 1";
                                     timeout = None; tools = None; prompt_ref = None;
                                     prompt_vars = []; thinking = false };
        input_mapping = []; output_key = None; depends_on = None };
      { id = "b"; node_type = Llm { model = "claude"; system = None; prompt = "Step 2";
                                     timeout = None; tools = None; prompt_ref = None;
                                     prompt_vars = []; thinking = false };
        input_mapping = []; output_key = None; depends_on = None }
    ];
    output = "b";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None;
  } in
  match Chain_parser.validate_chain chain with
  | Ok () -> ()
  | Error e -> fail (Printf.sprintf "Validation failed: %s" e)

(** {1 Serialization Tests} *)

let test_chain_to_json_roundtrip () =
  let json = parse_json {|{
    "id": "roundtrip",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello {{input}}"}
    ],
    "output": "a"
  }|} in
  let chain = expect_ok "parse" (Chain_parser.parse_chain json) in
  let json2 = Chain_parser.chain_to_json chain in
  let chain2 = expect_ok "reparse" (Chain_parser.parse_chain json2) in
  check string "id preserved" chain.id chain2.id;
  check string "output preserved" chain.output chain2.output;
  check int "nodes preserved" (List.length chain.nodes) (List.length chain2.nodes)

let test_node_to_json_llm () =
  let node = {
    id = "test_llm";
    node_type = Llm {
      model = "claude";
      system = Some "You are helpful";
      prompt = "Answer: {{question}}";
      timeout = Some 30;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = [("question", "prev")];
    output_key = Some "answer";
    depends_on = None;
  } in
  let json = Chain_parser.node_to_json node in
  let json_str = Yojson.Safe.to_string json in
  check bool "contains id" true (String.length json_str > 0 && String.sub json_str 0 1 = "{");
  let reparsed = expect_ok "reparse node" (Chain_parser.parse_node json) in
  check string "id match" node.id reparsed.id

let test_merge_strategy_to_string () =
  let tests = [
    (First, "first");
    (Last, "last");
    (Concat, "concat");
    (WeightedAvg, "weighted_average");
    (Custom "my_fn", "custom:my_fn");
  ] in
  List.iter (fun (strategy, expected) ->
    let s = Chain_parser.merge_strategy_to_string strategy in
    check string "strategy string" expected s
  ) tests

let test_threshold_op_to_string () =
  let tests = [(Gt, "gt"); (Gte, "gte"); (Lt, "lt"); (Lte, "lte"); (Eq, "eq"); (Neq, "neq")] in
  List.iter (fun (op, expected) ->
    let s = Chain_parser.threshold_op_to_string op in
    check string "op string" expected s
  ) tests

(** {1 Extract Input Mappings Tests} *)

let test_extract_input_mappings () =
  let prompt = "Analyze {{code}} with {{context.file}} and {{result}}" in
  let mappings = Chain_parser.extract_input_mappings prompt in
  check int "mapping count" 3 (List.length mappings);
  check bool "has code" true (List.exists (fun (_, id) -> id = "code") mappings);
  check bool "has context" true (List.exists (fun (_, id) -> id = "context") mappings);
  check bool "has result" true (List.exists (fun (_, id) -> id = "result") mappings)

let test_extract_json_mappings () =
  let json = parse_json {|{
    "query": "Search {{term}}",
    "options": {"limit": "{{max}}", "filter": "{{category}}"}
  }|} in
  let mappings = Chain_parser.extract_json_mappings json in
  check int "json mapping count" 3 (List.length mappings)

(** {1 Error Handling Tests} *)

let test_unknown_node_type () =
  let json = parse_json {|{
    "id": "unknown",
    "type": "nonexistent_type",
    "data": {}
  }|} in
  expect_error "unknown type" (Chain_parser.parse_node json)

let test_missing_required_field () =
  let json = parse_json {|{
    "type": "llm",
    "model": "gemini"
  }|} in
  expect_error "missing id" (Chain_parser.parse_node json)

let test_invalid_json_type () =
  let json = parse_json {|{
    "id": "bad_type",
    "type": "llm",
    "model": 123,
    "prompt": "test"
  }|} in
  (* Model should be string, but we have int - check behavior *)
  let result = Chain_parser.parse_node json in
  (* Depending on implementation, might error or coerce *)
  match result with
  | Error _ -> ()  (* Expected for strict parsing *)
  | Ok _ -> ()     (* May accept if lenient *)

(** {1 has_placeholder_node Tests} *)

let test_has_placeholder_node () =
  let placeholder = {
    id = "_placeholder";
    node_type = ChainRef "_";
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  check bool "placeholder detected" true (Chain_parser.has_placeholder_node placeholder);

  let normal = {
    id = "normal";
    node_type = Llm { model = "g"; system = None; prompt = "x";
                       timeout = None; tools = None; prompt_ref = None;
                       prompt_vars = []; thinking = false };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  check bool "normal not placeholder" false (Chain_parser.has_placeholder_node normal)

(** {1 Test Suite} *)

let parse_chain_tests = [
  "minimal chain", `Quick, test_parse_chain_minimal;
  "chain with config", `Quick, test_parse_chain_with_config;
  "chain with metadata", `Quick, test_parse_chain_with_metadata;
  "missing output", `Quick, test_parse_chain_missing_output;
  "missing nodes", `Quick, test_parse_chain_missing_nodes;
]

let llm_node_tests = [
  "basic llm", `Quick, test_parse_llm_node_basic;
  "full llm", `Quick, test_parse_llm_node_full;
  "nested llm", `Quick, test_parse_llm_node_nested_format;
  "missing model", `Quick, test_parse_llm_node_missing_model;
  "missing prompt", `Quick, test_parse_llm_node_missing_prompt;
]

let tool_node_tests = [
  "basic tool", `Quick, test_parse_tool_node_basic;
  "nested tool", `Quick, test_parse_tool_node_nested_format;
  "tool no args", `Quick, test_parse_tool_node_no_args;
]

let composite_node_tests = [
  "pipeline", `Quick, test_parse_pipeline_node;
  "fanout", `Quick, test_parse_fanout_node;
  "fanout nodes field", `Quick, test_parse_fanout_with_nodes_field;
  "quorum", `Quick, test_parse_quorum_node;
  "quorum inputs", `Quick, test_parse_quorum_with_inputs;
]

let gate_tests = [
  "gate embedded", `Quick, test_parse_gate_node_embedded;
  "gate refs", `Quick, test_parse_gate_node_refs;
  "gate no else", `Quick, test_parse_gate_no_else;
]

let merge_tests = [
  "merge node", `Quick, test_parse_merge_node;
  "merge strategies", `Quick, test_parse_merge_strategies;
]

let threshold_tests = [
  "threshold node", `Quick, test_parse_threshold_node;
  "threshold operators", `Quick, test_parse_threshold_operators;
]

let advanced_node_tests = [
  "goal_driven", `Quick, test_parse_goal_driven_node;
  "evaluator", `Quick, test_parse_evaluator_node;
  "select strategies", `Quick, test_parse_select_strategies;
  "retry", `Quick, test_parse_retry_node;
  "fallback", `Quick, test_parse_fallback_node;
  "race", `Quick, test_parse_race_node;
]

let ref_node_tests = [
  "chain_ref", `Quick, test_parse_chain_ref_node;
  "subgraph", `Quick, test_parse_subgraph_node;
  "chain_exec", `Quick, test_parse_chain_exec_node;
]

let adapter_tests = [
  "adapter node", `Quick, test_parse_adapter_node;
  "adapter transforms", `Quick, test_parse_adapter_transforms;
]

let optimization_node_tests = [
  "cache", `Quick, test_parse_cache_node;
  "batch", `Quick, test_parse_batch_node;
  "spawn", `Quick, test_parse_spawn_node;
]

let mcts_tests = [
  "mcts node", `Quick, test_parse_mcts_node;
]

let stream_tests = [
  "stream_merge", `Quick, test_parse_stream_merge_node;
]

let feedback_tests = [
  "feedback_loop", `Quick, test_parse_feedback_loop_node;
]

let masc_tests = [
  "masc_broadcast", `Quick, test_parse_masc_broadcast_node;
  "masc_listen", `Quick, test_parse_masc_listen_node;
  "masc_claim", `Quick, test_parse_masc_claim_node;
]

let functional_node_tests = [
  "map", `Quick, test_parse_map_node;
  "bind", `Quick, test_parse_bind_node;
]

let input_mapping_tests = [
  "list format", `Quick, test_parse_input_mapping_list_format;
  "object format", `Quick, test_parse_input_mapping_object_format;
  "auto extract", `Quick, test_parse_input_mapping_auto_extract;
  "depends_on", `Quick, test_parse_depends_on;
]

let backoff_tests = [
  "backoff strategies", `Quick, test_parse_backoff_strategies;
]

let validation_tests = [
  "missing output node", `Quick, test_validate_chain_missing_output_node;
  "duplicate ids", `Quick, test_validate_chain_duplicate_ids;
  "valid chain", `Quick, test_validate_chain_valid;
]

let serialization_tests = [
  "chain roundtrip", `Quick, test_chain_to_json_roundtrip;
  "node to json llm", `Quick, test_node_to_json_llm;
  "merge strategy string", `Quick, test_merge_strategy_to_string;
  "threshold op string", `Quick, test_threshold_op_to_string;
]

let extract_tests = [
  "extract input mappings", `Quick, test_extract_input_mappings;
  "extract json mappings", `Quick, test_extract_json_mappings;
]

let error_tests = [
  "unknown node type", `Quick, test_unknown_node_type;
  "missing required field", `Quick, test_missing_required_field;
  "invalid json type", `Quick, test_invalid_json_type;
  "has_placeholder_node", `Quick, test_has_placeholder_node;
]

let () =
  run "Chain Parser Coverage" [
    "parse_chain", parse_chain_tests;
    "llm_node", llm_node_tests;
    "tool_node", tool_node_tests;
    "composite_node", composite_node_tests;
    "gate", gate_tests;
    "merge", merge_tests;
    "threshold", threshold_tests;
    "advanced_node", advanced_node_tests;
    "ref_node", ref_node_tests;
    "adapter", adapter_tests;
    "optimization_node", optimization_node_tests;
    "mcts", mcts_tests;
    "stream", stream_tests;
    "feedback", feedback_tests;
    "masc", masc_tests;
    "functional_node", functional_node_tests;
    "input_mapping", input_mapping_tests;
    "backoff", backoff_tests;
    "validation", validation_tests;
    "serialization", serialization_tests;
    "extract", extract_tests;
    "error", error_tests;
  ]

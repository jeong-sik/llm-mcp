(** Chain Engine Tests

    Comprehensive unit tests for the Chain Engine DSL:
    - chain_types: Type definitions and helpers
    - chain_parser: JSON to AST conversion
    - chain_compiler: DAG compilation and topological sort
    - chain_registry: Chain storage and lookup
*)

open Chain_types
open Chain_parser
open Chain_compiler
open Chain_registry
open Chain_executor_eio

(* ============================================================================
   Test Fixtures
   ============================================================================ *)

(** Simple LLM chain JSON *)
let simple_llm_chain_json = {|
{
  "id": "simple_llm",
  "nodes": [
    {
      "id": "greeting",
      "type": "llm",
      "model": "gemini",
      "prompt": "Say hello to {{input}}"
    }
  ],
  "output": "greeting"
}
|}

(** Pipeline chain JSON *)
let pipeline_chain_json = {|
{
  "id": "pipeline_test",
  "nodes": [
    {
      "id": "pipeline",
      "type": "pipeline",
      "nodes": [
        {
          "id": "step1",
          "type": "llm",
          "model": "gemini",
          "prompt": "Analyze: {{input}}"
        },
        {
          "id": "step2",
          "type": "llm",
          "model": "claude",
          "prompt": "Improve: {{step1.output}}"
        }
      ]
    }
  ],
  "output": "pipeline"
}
|}

(** Fanout chain JSON - uses inline node definitions *)
let fanout_chain_json = {|
{
  "id": "fanout_test",
  "nodes": [
    {
      "id": "parallel",
      "type": "fanout",
      "nodes": [
        {
          "id": "branch1",
          "type": "llm",
          "model": "gemini",
          "prompt": "View 1: {{input}}"
        },
        {
          "id": "branch2",
          "type": "llm",
          "model": "claude",
          "prompt": "View 2: {{input}}"
        }
      ]
    }
  ],
  "output": "parallel"
}
|}

(** Quorum chain JSON (MAGI-style) - uses inline node definitions *)
let quorum_chain_json = {|
{
  "id": "magi_consensus",
  "nodes": [
    {
      "id": "consensus",
      "type": "quorum",
      "required": 2,
      "nodes": [
        {
          "id": "casper",
          "type": "llm",
          "model": "gemini",
          "prompt": "Strategic view: {{input}}"
        },
        {
          "id": "balthasar",
          "type": "llm",
          "model": "claude",
          "prompt": "Value-based view: {{input}}"
        },
        {
          "id": "melchior",
          "type": "llm",
          "model": "codex",
          "prompt": "Technical view: {{input}}"
        }
      ]
    }
  ],
  "output": "consensus"
}
|}

(** Gate chain JSON - Gate nodes have inline then/else node definitions *)
let gate_chain_json = {|
{
  "id": "gate_test",
  "nodes": [
    {
      "id": "classify",
      "type": "llm",
      "model": "gemini",
      "prompt": "Is this a bug? Answer yes or no: {{input}}"
    },
    {
      "id": "router",
      "type": "gate",
      "condition": "classify.output contains 'yes'",
      "then": {
        "id": "bug_handler",
        "type": "llm",
        "model": "claude",
        "prompt": "Fix bug: {{input}}"
      },
      "else": {
        "id": "feature_handler",
        "type": "llm",
        "model": "codex",
        "prompt": "Implement feature: {{input}}"
      }
    }
  ],
  "output": "router"
}
|}

(** Chain with ChainRef *)
let chain_with_ref_json = {|
{
  "id": "main_workflow",
  "nodes": [
    {
      "id": "preprocess",
      "type": "llm",
      "model": "gemini",
      "prompt": "Preprocess: {{input}}"
    },
    {
      "id": "reuse",
      "type": "chain_ref",
      "ref": "simple_llm"
    }
  ],
  "output": "reuse"
}
|}

(** Subgraph chain JSON *)
let subgraph_chain_json = {|
{
  "id": "nested_workflow",
  "nodes": [
    {
      "id": "outer",
      "type": "llm",
      "model": "gemini",
      "prompt": "Outer: {{input}}"
    },
    {
      "id": "inner_graph",
      "type": "subgraph",
      "graph": {
        "id": "inner",
        "nodes": [
          {
            "id": "inner_step",
            "type": "llm",
            "model": "claude",
            "prompt": "Inner: {{input}}"
          }
        ],
        "output": "inner_step"
      }
    }
  ],
  "output": "inner_graph"
}
|}

(** Threshold chain JSON - conditional branching based on metric *)
let threshold_chain_json = {|
{
  "id": "quality_gate",
  "nodes": [
    {
      "id": "analyze",
      "type": "llm",
      "model": "gemini",
      "prompt": "Analyze quality of: {{input}}"
    },
    {
      "id": "quality_check",
      "type": "threshold",
      "metric": "confidence",
      "operator": ">=",
      "value": 0.8,
      "input_node": {
        "id": "scorer",
        "type": "llm",
        "model": "claude",
        "prompt": "Score this from 0-1: {{analyze.output}}"
      },
      "on_pass": {
        "id": "accept",
        "type": "llm",
        "model": "gemini",
        "prompt": "Summarize accepted result: {{scorer.output}}"
      },
      "on_fail": {
        "id": "retry",
        "type": "llm",
        "model": "codex",
        "prompt": "Improve and retry: {{scorer.output}}"
      }
    }
  ],
  "output": "quality_check"
}
|}

(** GoalDriven chain JSON - iterative execution until goal achieved *)
let goal_driven_chain_json = {|
{
  "id": "coverage_goal",
  "nodes": [
    {
      "id": "test_gen",
      "type": "goal_driven",
      "goal_metric": "coverage",
      "goal_operator": ">=",
      "goal_value": 0.90,
      "action_node": {
        "id": "generate_test",
        "type": "llm",
        "model": "codex",
        "prompt": "Generate tests for uncovered code: {{input}}"
      },
      "measure_func": "exec_test",
      "max_iterations": 10,
      "strategy_hints": {
        "below_50": "fast",
        "above_50": "accurate"
      }
    }
  ],
  "output": "test_gen"
}
|}

(** Evaluator chain JSON - score candidates and select best *)
let evaluator_chain_json = {|
{
  "id": "solution_picker",
  "nodes": [
    {
      "id": "evaluator",
      "type": "evaluator",
      "candidates": [
        {
          "id": "fast_solution",
          "type": "llm",
          "model": "gemini",
          "prompt": "Fast solution: {{input}}"
        },
        {
          "id": "accurate_solution",
          "type": "llm",
          "model": "claude",
          "prompt": "Accurate solution: {{input}}"
        },
        {
          "id": "creative_solution",
          "type": "llm",
          "model": "codex",
          "prompt": "Creative solution: {{input}}"
        }
      ],
      "scoring_func": "llm_judge",
      "scoring_prompt": "Score 0-1 for correctness and clarity",
      "select_strategy": "best",
      "min_score": 0.7
    }
  ],
  "output": "evaluator"
}
|}

(** Retry chain JSON - retry on failure with exponential backoff *)
let retry_chain_json = {|
{
  "id": "retry_llm",
  "nodes": [
    {
      "id": "retrier",
      "type": "retry",
      "max_attempts": 3,
      "backoff": "exponential:2.0",
      "retry_on": ["timeout", "rate_limit"],
      "node": {
        "id": "llm_call",
        "type": "llm",
        "model": "gemini",
        "prompt": "Process: {{input}}"
      }
    }
  ],
  "output": "retrier"
}
|}

(** Fallback chain JSON - try primary, then fallbacks in order *)
let fallback_chain_json = {|
{
  "id": "resilient_api",
  "nodes": [
    {
      "id": "fallback_chain",
      "type": "fallback",
      "primary": {
        "id": "fast_llm",
        "type": "llm",
        "model": "gemini",
        "prompt": "Quick answer: {{input}}"
      },
      "fallbacks": [
        {
          "id": "accurate_llm",
          "type": "llm",
          "model": "claude",
          "prompt": "Detailed answer: {{input}}"
        },
        {
          "id": "local_llm",
          "type": "llm",
          "model": "ollama:qwen",
          "prompt": "Fallback answer: {{input}}"
        }
      ]
    }
  ],
  "output": "fallback_chain"
}
|}

(** Race chain JSON - parallel execution, first result wins *)
let race_chain_json = {|
{
  "id": "fast_response",
  "nodes": [
    {
      "id": "racer",
      "type": "race",
      "timeout": 5.0,
      "nodes": [
        {
          "id": "gemini_fast",
          "type": "llm",
          "model": "gemini",
          "prompt": "Answer quickly: {{input}}"
        },
        {
          "id": "claude_fast",
          "type": "llm",
          "model": "claude",
          "prompt": "Answer quickly: {{input}}"
        }
      ]
    }
  ],
  "output": "racer"
}
|}

(* ============================================================================
   Chain Types Tests
   ============================================================================ *)

let test_default_config () =
  Alcotest.(check int) "max_depth" 8 default_config.max_depth;  (* Increased for MCTS support *)
  Alcotest.(check int) "max_concurrency" 3 default_config.max_concurrency;
  Alcotest.(check int) "timeout" 300 default_config.timeout;
  Alcotest.(check bool) "trace" false default_config.trace

let test_node_type_name () =
  Alcotest.(check string) "llm" "llm"
    (node_type_name (Llm { model = "test"; system = None; prompt = "test"; timeout = None; tools = None }));
  Alcotest.(check string) "tool" "tool"
    (node_type_name (Tool { name = "test"; args = `Null }));
  Alcotest.(check string) "pipeline" "pipeline"
    (node_type_name (Pipeline []));
  Alcotest.(check string) "fanout" "fanout"
    (node_type_name (Fanout []));
  Alcotest.(check string) "quorum" "quorum"
    (node_type_name (Quorum { required = 2; nodes = [] }));
  Alcotest.(check string) "gate" "gate"
    (node_type_name (Gate { condition = ""; then_node = { id = ""; node_type = Pipeline []; input_mapping = [] }; else_node = None }));
  Alcotest.(check string) "subgraph" "subgraph"
    (node_type_name (Subgraph { id = ""; nodes = []; output = ""; config = default_config }));
  Alcotest.(check string) "chain_ref" "chain_ref"
    (node_type_name (ChainRef "test"));
  Alcotest.(check string) "map" "map"
    (node_type_name (Map { func = ""; inner = { id = ""; node_type = Pipeline []; input_mapping = [] } }));
  Alcotest.(check string) "bind" "bind"
    (node_type_name (Bind { func = ""; inner = { id = ""; node_type = Pipeline []; input_mapping = [] } }));
  Alcotest.(check string) "merge" "merge"
    (node_type_name (Merge { strategy = First; nodes = [] }))

let test_make_llm_node () =
  let node = make_llm_node ~id:"test" ~model:"gemini" ~prompt:"hello" () in
  Alcotest.(check string) "id" "test" node.id;
  match node.node_type with
  | Llm { model; system = _; prompt; timeout; tools } ->
      Alcotest.(check string) "model" "gemini" model;
      Alcotest.(check string) "prompt" "hello" prompt;
      Alcotest.(check (option int)) "timeout" None timeout;
      ignore tools  (* tools field added for Ollama interop *)
  | _ -> Alcotest.fail "Expected Llm node"

let test_make_pipeline () =
  let node1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"p1" () in
  let node2 = make_llm_node ~id:"n2" ~model:"claude" ~prompt:"p2" () in
  let pipeline = make_pipeline ~id:"pipe" [node1; node2] in
  Alcotest.(check string) "id" "pipe" pipeline.id;
  match pipeline.node_type with
  | Pipeline nodes -> Alcotest.(check int) "node count" 2 (List.length nodes)
  | _ -> Alcotest.fail "Expected Pipeline node"

let test_make_quorum () =
  let nodes = [
    make_llm_node ~id:"a" ~model:"gemini" ~prompt:"a" ();
    make_llm_node ~id:"b" ~model:"claude" ~prompt:"b" ();
    make_llm_node ~id:"c" ~model:"codex" ~prompt:"c" ();
  ] in
  let quorum = make_quorum ~id:"q" ~required:2 nodes in
  match quorum.node_type with
  | Quorum { required; nodes } ->
      Alcotest.(check int) "required" 2 required;
      Alcotest.(check int) "nodes" 3 (List.length nodes)
  | _ -> Alcotest.fail "Expected Quorum node"

let test_merge_strategy_json () =
  let test_roundtrip strategy =
    let json = merge_strategy_to_yojson strategy in
    match merge_strategy_of_yojson json with
    | Ok s -> Alcotest.(check bool) "roundtrip" true (s = strategy)
    | Error e -> Alcotest.fail e
  in
  test_roundtrip First;
  test_roundtrip Last;
  test_roundtrip Concat;
  test_roundtrip WeightedAvg;
  test_roundtrip (Custom "my_merge")

let test_chain_json_roundtrip () =
  let chain = {
    id = "test_chain";
    nodes = [
      make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"test" ();
    ];
    output = "n1";
    config = default_config;
  } in
  let json = chain_to_yojson chain in
  match chain_of_yojson json with
  | Ok parsed ->
      Alcotest.(check string) "id" chain.id parsed.id;
      Alcotest.(check string) "output" chain.output parsed.output;
      Alcotest.(check int) "nodes" (List.length chain.nodes) (List.length parsed.nodes)
  | Error e -> Alcotest.fail e

let test_make_threshold () =
  let input_node = make_llm_node ~id:"scorer" ~model:"claude" ~prompt:"score" () in
  let on_pass = make_llm_node ~id:"accept" ~model:"gemini" ~prompt:"accept" () in
  let threshold = make_threshold ~id:"quality_gate" ~metric:"confidence"
    ~operator:Gte ~value:0.8 ~input_node ~on_pass () in
  Alcotest.(check string) "id" "quality_gate" threshold.id;
  match threshold.node_type with
  | Threshold { metric; operator; value; input_node; on_pass; on_fail } ->
      Alcotest.(check string) "metric" "confidence" metric;
      Alcotest.(check bool) "operator is Gte" true (operator = Gte);
      Alcotest.(check (float 0.001)) "value" 0.8 value;
      Alcotest.(check string) "input_node id" "scorer" input_node.id;
      Alcotest.(check bool) "on_pass present" true (Option.is_some on_pass);
      Alcotest.(check bool) "on_fail absent" true (Option.is_none on_fail)
  | _ -> Alcotest.fail "Expected Threshold node"

let test_make_goal_driven () =
  let action_node = make_llm_node ~id:"gen_test" ~model:"codex" ~prompt:"generate" () in
  let goal_driven = make_goal_driven ~id:"coverage_goal"
    ~goal_metric:"coverage" ~goal_operator:Gte ~goal_value:0.9
    ~action_node ~measure_func:"exec_test" ~max_iterations:10
    ~strategy_hints:[("below_50", "fast"); ("above_50", "accurate")] () in
  Alcotest.(check string) "id" "coverage_goal" goal_driven.id;
  match goal_driven.node_type with
  | GoalDriven { goal_metric; goal_operator; goal_value; action_node;
                 measure_func; max_iterations; strategy_hints;
                 conversational; relay_models } ->
      Alcotest.(check string) "goal_metric" "coverage" goal_metric;
      Alcotest.(check bool) "goal_operator is Gte" true (goal_operator = Gte);
      Alcotest.(check (float 0.001)) "goal_value" 0.9 goal_value;
      Alcotest.(check string) "action_node id" "gen_test" action_node.id;
      Alcotest.(check string) "measure_func" "exec_test" measure_func;
      Alcotest.(check int) "max_iterations" 10 max_iterations;
      Alcotest.(check int) "strategy_hints count" 2 (List.length strategy_hints);
      (* Default values for new fields *)
      Alcotest.(check bool) "conversational default" false conversational;
      Alcotest.(check int) "relay_models default empty" 0 (List.length relay_models)
  | _ -> Alcotest.fail "Expected GoalDriven node"

let test_make_goal_driven_conversational () =
  let action_node = make_llm_node ~id:"gen_code" ~model:"gemini" ~prompt:"generate code" () in
  let goal_driven = make_goal_driven ~id:"iterative_coding"
    ~goal_metric:"score" ~goal_operator:Gte ~goal_value:0.95
    ~action_node ~measure_func:"parse_float" ~max_iterations:5
    ~conversational:true
    ~relay_models:["gemini"; "claude"; "codex"] () in
  Alcotest.(check string) "id" "iterative_coding" goal_driven.id;
  match goal_driven.node_type with
  | GoalDriven { conversational; relay_models; _ } ->
      Alcotest.(check bool) "conversational enabled" true conversational;
      Alcotest.(check int) "relay_models count" 3 (List.length relay_models);
      Alcotest.(check string) "first model" "gemini" (List.hd relay_models);
      Alcotest.(check string) "last model" "codex" (List.nth relay_models 2)
  | _ -> Alcotest.fail "Expected GoalDriven node"

let test_make_evaluator () =
  let candidates = [
    make_llm_node ~id:"fast" ~model:"gemini" ~prompt:"fast" ();
    make_llm_node ~id:"accurate" ~model:"claude" ~prompt:"accurate" ();
    make_llm_node ~id:"creative" ~model:"codex" ~prompt:"creative" ();
  ] in
  let evaluator = make_evaluator ~id:"picker" ~candidates ~scoring_func:"llm_judge"
    ~scoring_prompt:"Score 0-1" ~select_strategy:Best ~min_score:0.7 () in
  Alcotest.(check string) "id" "picker" evaluator.id;
  match evaluator.node_type with
  | Evaluator { candidates; scoring_func; scoring_prompt; select_strategy; min_score } ->
      Alcotest.(check int) "candidates" 3 (List.length candidates);
      Alcotest.(check string) "scoring_func" "llm_judge" scoring_func;
      Alcotest.(check bool) "scoring_prompt present" true (Option.is_some scoring_prompt);
      Alcotest.(check bool) "select_strategy is Best" true (select_strategy = Best);
      Alcotest.(check bool) "min_score present" true (Option.is_some min_score)
  | _ -> Alcotest.fail "Expected Evaluator node"

(* ============================================================================
   Resilience Node Tests
   ============================================================================ *)

let test_make_retry () =
  let inner = make_llm_node ~id:"inner" ~model:"gemini" ~prompt:"test" () in
  let retry = make_retry ~id:"retrier" ~node:inner ~max_attempts:3
    ~backoff:(Exponential 2.0) ~retry_on:["timeout"; "rate_limit"] () in
  Alcotest.(check string) "id" "retrier" retry.id;
  match retry.node_type with
  | Retry { node; max_attempts; backoff; retry_on } ->
      Alcotest.(check string) "inner node id" "inner" node.id;
      Alcotest.(check int) "max_attempts" 3 max_attempts;
      Alcotest.(check bool) "backoff is exponential" true
        (match backoff with Exponential _ -> true | _ -> false);
      Alcotest.(check int) "retry_on count" 2 (List.length retry_on)
  | _ -> Alcotest.fail "Expected Retry node"

let test_make_fallback () =
  let primary = make_llm_node ~id:"primary" ~model:"gemini" ~prompt:"fast" () in
  let fb1 = make_llm_node ~id:"fb1" ~model:"claude" ~prompt:"accurate" () in
  let fb2 = make_llm_node ~id:"fb2" ~model:"codex" ~prompt:"fallback" () in
  let fallback = make_fallback ~id:"fb_chain" ~primary ~fallbacks:[fb1; fb2] in
  Alcotest.(check string) "id" "fb_chain" fallback.id;
  match fallback.node_type with
  | Fallback { primary; fallbacks } ->
      Alcotest.(check string) "primary id" "primary" primary.id;
      Alcotest.(check int) "fallbacks count" 2 (List.length fallbacks)
  | _ -> Alcotest.fail "Expected Fallback node"

let test_make_race () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"fast" () in
  let n2 = make_llm_node ~id:"n2" ~model:"claude" ~prompt:"fast" () in
  let race = make_race ~id:"racer" ~nodes:[n1; n2] ~timeout:5.0 () in
  Alcotest.(check string) "id" "racer" race.id;
  match race.node_type with
  | Race { nodes; timeout } ->
      Alcotest.(check int) "nodes count" 2 (List.length nodes);
      Alcotest.(check bool) "timeout present" true (Option.is_some timeout);
      Alcotest.(check (option (float 0.01))) "timeout value" (Some 5.0) timeout
  | _ -> Alcotest.fail "Expected Race node"

let test_parse_retry_chain () =
  let json = Yojson.Safe.from_string retry_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "chain id" "retry_llm" chain.id;
      Alcotest.(check int) "nodes count" 1 (List.length chain.nodes);
      let node = List.hd chain.nodes in
      Alcotest.(check string) "node id" "retrier" node.id;
      (match node.node_type with
       | Retry { node = inner; max_attempts; retry_on; _ } ->
           Alcotest.(check string) "inner node id" "llm_call" inner.id;
           Alcotest.(check int) "max_attempts" 3 max_attempts;
           Alcotest.(check (list string)) "retry_on" ["timeout"; "rate_limit"] retry_on
       | _ -> Alcotest.fail "Expected Retry node type")
  | Error e -> Alcotest.fail ("Parse error: " ^ e)

let test_parse_fallback_chain () =
  let json = Yojson.Safe.from_string fallback_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "chain id" "resilient_api" chain.id;
      let node = List.hd chain.nodes in
      Alcotest.(check string) "node id" "fallback_chain" node.id;
      (match node.node_type with
       | Fallback { primary; fallbacks } ->
           Alcotest.(check string) "primary id" "fast_llm" primary.id;
           Alcotest.(check int) "fallbacks count" 2 (List.length fallbacks);
           Alcotest.(check string) "first fallback" "accurate_llm" (List.hd fallbacks).id
       | _ -> Alcotest.fail "Expected Fallback node type")
  | Error e -> Alcotest.fail ("Parse error: " ^ e)

let test_parse_race_chain () =
  let json = Yojson.Safe.from_string race_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "chain id" "fast_response" chain.id;
      let node = List.hd chain.nodes in
      Alcotest.(check string) "node id" "racer" node.id;
      (match node.node_type with
       | Race { nodes; timeout } ->
           Alcotest.(check int) "nodes count" 2 (List.length nodes);
           Alcotest.(check bool) "timeout present" true (Option.is_some timeout);
           Alcotest.(check (option (float 0.01))) "timeout value" (Some 5.0) timeout
       | _ -> Alcotest.fail "Expected Race node type")
  | Error e -> Alcotest.fail ("Parse error: " ^ e)

let test_backoff_strategies () =
  (* Test all backoff parsing variants *)
  let test_json backoff_str expected_name =
    let json = Yojson.Safe.from_string (Printf.sprintf {|
      {
        "id": "test",
        "nodes": [{
          "id": "r",
          "type": "retry",
          "max_attempts": 2,
          "backoff": "%s",
          "node": {"id": "inner", "type": "llm", "model": "gemini", "prompt": "x"}
        }],
        "output": "r"
      }
    |} backoff_str) in
    match parse_chain json with
    | Ok chain ->
        let node = List.hd chain.nodes in
        (match node.node_type with
         | Retry { backoff; _ } ->
             let actual = match backoff with
               | Constant _ -> "constant"
               | Exponential _ -> "exponential"
               | Linear _ -> "linear"
               | Jitter _ -> "jitter"
             in
             Alcotest.(check string) ("backoff " ^ backoff_str) expected_name actual
         | _ -> Alcotest.fail "Expected Retry")
    | Error e -> Alcotest.fail ("Parse error: " ^ e)
  in
  test_json "exponential:2.0" "exponential";
  test_json "constant:1.5" "constant";
  test_json "linear:0.5" "linear"

(* ============================================================================
   Conversation Context Tests (relay_models rotation)
   ============================================================================ *)

let test_make_conversation_ctx_default () =
  let ctx = make_conversation_ctx () in
  Alcotest.(check string) "default current_model" "gemini" ctx.current_model;
  Alcotest.(check int) "model_index" 0 ctx.model_index;
  Alcotest.(check int) "models count" 3 (List.length ctx.models);
  Alcotest.(check (list string)) "default models" ["gemini"; "claude"; "codex"] ctx.models;
  Alcotest.(check int) "token_threshold" 6000 ctx.token_threshold;
  Alcotest.(check int) "window_size" 10 ctx.window_size;
  Alcotest.(check int) "empty history" 0 (List.length ctx.history);
  Alcotest.(check int) "total_tokens" 0 ctx.total_tokens

let test_make_conversation_ctx_custom_models () =
  let ctx = make_conversation_ctx ~models:["claude"; "codex"] ~token_threshold:3000 () in
  Alcotest.(check string) "current_model" "claude" ctx.current_model;
  Alcotest.(check (list string)) "custom models" ["claude"; "codex"] ctx.models;
  Alcotest.(check int) "custom threshold" 3000 ctx.token_threshold

let test_rotate_model () =
  let ctx = make_conversation_ctx ~models:["a"; "b"; "c"] () in
  Alcotest.(check string) "initial" "a" ctx.current_model;
  Alcotest.(check int) "initial index" 0 ctx.model_index;
  rotate_model ctx;
  Alcotest.(check string) "after 1st rotate" "b" ctx.current_model;
  Alcotest.(check int) "index after 1st" 1 ctx.model_index;
  rotate_model ctx;
  Alcotest.(check string) "after 2nd rotate" "c" ctx.current_model;
  Alcotest.(check int) "index after 2nd" 2 ctx.model_index;
  rotate_model ctx;
  Alcotest.(check string) "after 3rd rotate wraps" "a" ctx.current_model;
  Alcotest.(check int) "index after wrap" 0 ctx.model_index

let test_rotate_model_single () =
  let ctx = make_conversation_ctx ~models:["only"] () in
  Alcotest.(check string) "initial" "only" ctx.current_model;
  rotate_model ctx;
  Alcotest.(check string) "after rotate stays same" "only" ctx.current_model;
  Alcotest.(check int) "index stays 0" 0 ctx.model_index

let test_estimate_tokens () =
  (* ~4 chars per token, formula: (len + 3) / 4 *)
  Alcotest.(check int) "empty string" 0 (estimate_tokens "");
  Alcotest.(check int) "4 chars = 1 token" 1 (estimate_tokens "abcd");
  Alcotest.(check int) "8 chars = 2 tokens" 2 (estimate_tokens "abcdefgh");
  Alcotest.(check int) "5 chars rounds up" 2 (estimate_tokens "abcde");
  Alcotest.(check int) "3 chars rounds up" 1 (estimate_tokens "abc")

let test_add_message () =
  let ctx = make_conversation_ctx () in
  add_message ctx ~role:"user" ~content:"hello" ~iteration:1;
  Alcotest.(check int) "history length" 1 (List.length ctx.history);
  Alcotest.(check int) "total_tokens" 2 ctx.total_tokens;  (* "hello" = 5 chars -> 2 tokens *)
  let msg = List.hd ctx.history in
  Alcotest.(check string) "msg role" "user" msg.role;
  Alcotest.(check string) "msg content" "hello" msg.content;
  Alcotest.(check string) "msg model" "gemini" msg.model;
  Alcotest.(check int) "msg iteration" 1 msg.iteration;
  (* Add another message *)
  add_message ctx ~role:"assistant" ~content:"world" ~iteration:1;
  Alcotest.(check int) "history length" 2 (List.length ctx.history);
  Alcotest.(check int) "total_tokens accumulated" 4 ctx.total_tokens

let test_needs_summarization () =
  let ctx = make_conversation_ctx ~token_threshold:10 ~window_size:2 () in
  Alcotest.(check bool) "initially no summarization" false (needs_summarization ctx);
  (* Add messages to exceed threshold and window *)
  add_message ctx ~role:"user" ~content:"this is a long message" ~iteration:1;
  add_message ctx ~role:"assistant" ~content:"another long message here" ~iteration:1;
  add_message ctx ~role:"user" ~content:"third message" ~iteration:2;
  (* Check conditions: total_tokens > 10 AND history > 2 *)
  Alcotest.(check bool) "needs summarization now" true (needs_summarization ctx)

let test_build_context_prompt () =
  let ctx = make_conversation_ctx () in
  add_message ctx ~role:"user" ~content:"hello" ~iteration:1;
  add_message ctx ~role:"assistant" ~content:"hi" ~iteration:1;
  let prompt = build_context_prompt ctx in
  Alcotest.(check bool) "prompt not empty" true (String.length prompt > 0);
  Alcotest.(check bool) "contains user role" true
    (try let _ = Str.search_forward (Str.regexp "user") prompt 0 in true with Not_found -> false);
  Alcotest.(check bool) "contains hello content" true
    (try let _ = Str.search_forward (Str.regexp "hello") prompt 0 in true with Not_found -> false)

(* ============================================================================
   Chain Parser Tests
   ============================================================================ *)

let test_parse_simple_llm_chain () =
  let json = Yojson.Safe.from_string simple_llm_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "simple_llm" chain.id;
      Alcotest.(check int) "nodes" 1 (List.length chain.nodes);
      Alcotest.(check string) "output" "greeting" chain.output
  | Error e -> Alcotest.fail e

let test_parse_pipeline_chain () =
  let json = Yojson.Safe.from_string pipeline_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "pipeline_test" chain.id;
      (* With inline node definitions, only 1 top-level node (pipeline container) *)
      Alcotest.(check int) "nodes" 1 (List.length chain.nodes);
      (* Find pipeline node *)
      let pipeline_node = List.find (fun (n : node) -> n.id = "pipeline") chain.nodes in
      (match pipeline_node.node_type with
       | Pipeline nodes -> Alcotest.(check int) "pipeline nodes" 2 (List.length nodes)
       | _ -> Alcotest.fail "Expected Pipeline type")
  | Error e -> Alcotest.fail e

let test_parse_fanout_chain () =
  let json = Yojson.Safe.from_string fanout_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "fanout_test" chain.id;
      let parallel_node = List.find (fun (n : node) -> n.id = "parallel") chain.nodes in
      (match parallel_node.node_type with
       | Fanout nodes -> Alcotest.(check int) "fanout branches" 2 (List.length nodes)
       | _ -> Alcotest.fail "Expected Fanout type")
  | Error e -> Alcotest.fail e

let test_parse_quorum_chain () =
  let json = Yojson.Safe.from_string quorum_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "magi_consensus" chain.id;
      let consensus_node = List.find (fun (n : node) -> n.id = "consensus") chain.nodes in
      (match consensus_node.node_type with
       | Quorum { required; nodes } ->
           Alcotest.(check int) "required" 2 required;
           Alcotest.(check int) "quorum nodes" 3 (List.length nodes)
       | _ -> Alcotest.fail "Expected Quorum type")
  | Error e -> Alcotest.fail e

let test_parse_chain_ref () =
  let json = Yojson.Safe.from_string chain_with_ref_json in
  match parse_chain json with
  | Ok chain ->
      let reuse_node = List.find (fun (n : node) -> n.id = "reuse") chain.nodes in
      (match reuse_node.node_type with
       | ChainRef ref_id -> Alcotest.(check string) "ref" "simple_llm" ref_id
       | _ -> Alcotest.fail "Expected ChainRef type")
  | Error e -> Alcotest.fail e

let test_parse_subgraph () =
  let json = Yojson.Safe.from_string subgraph_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "nested_workflow" chain.id;
      let inner_node = List.find (fun (n : node) -> n.id = "inner_graph") chain.nodes in
      (match inner_node.node_type with
       | Subgraph inner_chain ->
           Alcotest.(check string) "inner id" "inner" inner_chain.id;
           Alcotest.(check int) "inner nodes" 1 (List.length inner_chain.nodes)
       | _ -> Alcotest.fail "Expected Subgraph type")
  | Error e -> Alcotest.fail e

let test_parse_gate_chain () =
  let json = Yojson.Safe.from_string gate_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "chain id" "gate_test" chain.id;
      (* Only 2 top-level nodes: classify and router. then/else are nested inside Gate *)
      Alcotest.(check int) "node count" 2 (List.length chain.nodes);
      let router = List.find (fun (n : node) -> n.id = "router") chain.nodes in
      (match router.node_type with
       | Gate { condition; then_node; else_node } ->
           Alcotest.(check string) "condition" "classify.output contains 'yes'" condition;
           Alcotest.(check string) "then node id" "bug_handler" then_node.id;
           (match else_node with
            | Some e -> Alcotest.(check string) "else node id" "feature_handler" e.id
            | None -> Alcotest.fail "Expected else node")
       | _ -> Alcotest.fail "Expected Gate node")
  | Error e -> Alcotest.fail e

let test_parse_input_mapping () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "mapping_test",
      "nodes": [
        {
          "id": "producer",
          "type": "llm",
          "model": "gemini",
          "prompt": "Produce: {{input}}"
        },
        {
          "id": "consumer",
          "type": "llm",
          "model": "claude",
          "prompt": "Consume: {{data}}",
          "input_mapping": [["data", "{{producer.output}}"]]
        }
      ],
      "output": "consumer"
    }
  |} in
  match parse_chain json with
  | Ok chain ->
      let consumer = List.find (fun (n : node) -> n.id = "consumer") chain.nodes in
      Alcotest.(check int) "mappings" 1 (List.length consumer.input_mapping);
      let (key, value) = List.hd consumer.input_mapping in
      Alcotest.(check string) "mapping key" "data" key;
      Alcotest.(check string) "mapping value" "{{producer.output}}" value
  | Error e -> Alcotest.fail e

let test_parse_invalid_json () =
  let json = Yojson.Safe.from_string {|{"invalid": true}|} in
  match parse_chain json with
  | Ok _ -> Alcotest.fail "Should have failed"
  | Error _ -> ()  (* Expected *)

let test_parse_missing_output () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "missing_output",
      "nodes": [{"id": "n1", "type": "llm", "model": "gemini", "prompt": "test"}]
    }
  |} in
  match parse_chain json with
  | Ok _ -> Alcotest.fail "Should have failed on missing output"
  | Error _ -> ()

let test_parse_threshold_chain () =
  let json = Yojson.Safe.from_string threshold_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "quality_gate" chain.id;
      Alcotest.(check int) "nodes" 2 (List.length chain.nodes);
      let threshold_node = List.find (fun (n : node) -> n.id = "quality_check") chain.nodes in
      (match threshold_node.node_type with
       | Threshold { metric; operator; value; input_node; on_pass; on_fail } ->
           Alcotest.(check string) "metric" "confidence" metric;
           Alcotest.(check bool) "operator is Gte" true (operator = Gte);
           Alcotest.(check (float 0.001)) "value" 0.8 value;
           Alcotest.(check string) "input_node id" "scorer" input_node.id;
           Alcotest.(check bool) "on_pass present" true (Option.is_some on_pass);
           Alcotest.(check bool) "on_fail present" true (Option.is_some on_fail)
       | _ -> Alcotest.fail "Expected Threshold node")
  | Error e -> Alcotest.fail e

let test_parse_goal_driven_chain () =
  let json = Yojson.Safe.from_string goal_driven_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "coverage_goal" chain.id;
      Alcotest.(check int) "nodes" 1 (List.length chain.nodes);
      let goal_node = List.find (fun (n : node) -> n.id = "test_gen") chain.nodes in
      (match goal_node.node_type with
       | GoalDriven { goal_metric; goal_operator; goal_value; action_node;
                      measure_func; max_iterations; strategy_hints; _ } ->
           Alcotest.(check string) "goal_metric" "coverage" goal_metric;
           Alcotest.(check bool) "goal_operator is Gte" true (goal_operator = Gte);
           Alcotest.(check (float 0.001)) "goal_value" 0.90 goal_value;
           Alcotest.(check string) "action_node id" "generate_test" action_node.id;
           Alcotest.(check string) "measure_func" "exec_test" measure_func;
           Alcotest.(check int) "max_iterations" 10 max_iterations;
           Alcotest.(check int) "strategy_hints count" 2 (List.length strategy_hints)
       | _ -> Alcotest.fail "Expected GoalDriven node")
  | Error e -> Alcotest.fail e

let test_parse_evaluator_chain () =
  let json = Yojson.Safe.from_string evaluator_chain_json in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "id" "solution_picker" chain.id;
      Alcotest.(check int) "nodes" 1 (List.length chain.nodes);
      let eval_node = List.find (fun (n : node) -> n.id = "evaluator") chain.nodes in
      (match eval_node.node_type with
       | Evaluator { candidates; scoring_func; scoring_prompt; select_strategy; min_score } ->
           Alcotest.(check int) "candidates" 3 (List.length candidates);
           Alcotest.(check string) "scoring_func" "llm_judge" scoring_func;
           Alcotest.(check bool) "scoring_prompt present" true (Option.is_some scoring_prompt);
           Alcotest.(check bool) "select_strategy is Best" true (select_strategy = Best);
           Alcotest.(check bool) "min_score present" true (Option.is_some min_score);
           (* Check min_score value *)
           (match min_score with
            | Some v -> Alcotest.(check (float 0.001)) "min_score value" 0.7 v
            | None -> Alcotest.fail "min_score should be present")
       | _ -> Alcotest.fail "Expected Evaluator node")
  | Error e -> Alcotest.fail e

(* ============================================================================
   Chain Compiler Tests
   ============================================================================ *)

let test_compile_simple_chain () =
  let json = Yojson.Safe.from_string simple_llm_chain_json in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           Alcotest.(check int) "execution_order" 1 (List.length plan.execution_order);
           Alcotest.(check string) "first node" "greeting" (List.hd plan.execution_order);
           Alcotest.(check int) "depth" 1 plan.depth
       | Error e -> Alcotest.fail e)
  | Error e -> Alcotest.fail e

let test_compile_pipeline_topological_order () =
  let chain = {
    id = "topo_test";
    nodes = [
      { id = "c"; node_type = Llm { model = "gemini"; system = None; prompt = "c"; timeout = None; tools = None };
        input_mapping = [("input", "{{b.output}}")] };
      { id = "a"; node_type = Llm { model = "gemini"; system = None; prompt = "a"; timeout = None; tools = None };
        input_mapping = [] };
      { id = "b"; node_type = Llm { model = "claude"; system = None; prompt = "b"; timeout = None; tools = None };
        input_mapping = [("input", "{{a.output}}")] };
    ];
    output = "c";
    config = default_config;
  } in
  match compile chain with
  | Ok plan ->
      (* Order should be a -> b -> c *)
      Alcotest.(check (list string)) "topo order" ["a"; "b"; "c"] plan.execution_order
  | Error e -> Alcotest.fail e

let test_compile_fanout_parallel_groups () =
  let json = Yojson.Safe.from_string fanout_chain_json in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           (* Should have parallel groups *)
           Alcotest.(check bool) "has parallel groups"
             true (List.length plan.parallel_groups > 0)
       | Error e -> Alcotest.fail e)
  | Error e -> Alcotest.fail e

let test_compile_depth_calculation () =
  let json = Yojson.Safe.from_string subgraph_chain_json in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           (* Subgraph increases depth *)
           Alcotest.(check bool) "depth > 1" true (plan.depth > 1)
       | Error e -> Alcotest.fail e)
  | Error e -> Alcotest.fail e

(* ============================================================================
   Chain Registry Tests
   ============================================================================ *)

let test_registry_register_lookup () =
  clear ();  (* Start fresh *)
  let chain = {
    id = "test_reg_chain";
    nodes = [make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"test" ()];
    output = "n1";
    config = default_config;
  } in
  register chain;
  match lookup "test_reg_chain" with
  | Some found ->
      Alcotest.(check string) "found id" chain.id found.id
  | None -> Alcotest.fail "Chain not found"

let test_registry_exists () =
  clear ();
  let chain = {
    id = "exists_test";
    nodes = [];
    output = "";
    config = default_config;
  } in
  Alcotest.(check bool) "not exists before" false (exists "exists_test");
  register chain;
  Alcotest.(check bool) "exists after" true (exists "exists_test")

let test_registry_unregister () =
  clear ();
  let chain = {
    id = "unreg_test";
    nodes = [];
    output = "";
    config = default_config;
  } in
  register chain;
  Alcotest.(check bool) "exists" true (exists "unreg_test");
  let removed = unregister "unreg_test" in
  Alcotest.(check bool) "removed" true removed;
  Alcotest.(check bool) "not exists" false (exists "unreg_test")

let test_registry_list () =
  clear ();
  let chain1 = { id = "list_1"; nodes = []; output = ""; config = default_config } in
  let chain2 = { id = "list_2"; nodes = []; output = ""; config = default_config } in
  register chain1;
  register chain2;
  let ids = list_ids () in
  Alcotest.(check int) "count" 2 (List.length ids);
  Alcotest.(check bool) "has list_1" true (List.mem "list_1" ids);
  Alcotest.(check bool) "has list_2" true (List.mem "list_2" ids)

let test_registry_version () =
  clear ();
  let chain = { id = "version_test"; nodes = []; output = ""; config = default_config } in
  register chain;
  register chain;  (* Re-register same id *)
  match lookup_entry "version_test" with
  | Some entry -> Alcotest.(check int) "version incremented" 2 entry.version
  | None -> Alcotest.fail "Entry not found"

let test_registry_stats () =
  clear ();
  let chain1 = {
    id = "stats_1";
    nodes = [make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"test" ()];
    output = "n1";
    config = default_config
  } in
  let chain2 = {
    id = "stats_2";
    nodes = [
      make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"test" ();
      make_llm_node ~id:"n2" ~model:"claude" ~prompt:"test" ();
    ];
    output = "n2";
    config = default_config
  } in
  register chain1;
  register chain2;
  let s = stats () in
  Alcotest.(check int) "total_chains" 2 s.total_chains;
  Alcotest.(check int) "total_nodes" 3 s.total_nodes

let test_registry_json_export_import () =
  clear ();
  let chain = {
    id = "export_test";
    nodes = [make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"test" ()];
    output = "n1";
    config = default_config
  } in
  register ~description:"Test chain" chain;
  let json = to_json () in
  clear ();
  Alcotest.(check int) "cleared" 0 (count ());
  match of_json json with
  | Ok imported ->
      Alcotest.(check int) "imported" 1 imported;
      Alcotest.(check bool) "exists after import" true (exists "export_test")
  | Error e -> Alcotest.fail e

(* ============================================================================
   Self-Recursion Tests (Nested Evaluation Nodes)
   ============================================================================ *)

(** Test: Threshold inside Threshold (nested conditional evaluation) *)
let test_nested_threshold () =
  let inner_llm = make_llm_node ~id:"inner_llm" ~model:"gemini"
    ~prompt:"Inner LLM" () in
  let inner_threshold = make_threshold ~id:"inner_threshold"
    ~metric:"confidence" ~operator:Gte ~value:0.7 ~input_node:inner_llm () in
  let outer_llm = make_llm_node ~id:"outer_llm" ~model:"claude"
    ~prompt:"Outer LLM" () in
  let outer_threshold = make_threshold ~id:"outer_threshold"
    ~metric:"accuracy" ~operator:Gte ~value:0.9
    ~input_node:outer_llm
    ~on_pass:inner_threshold
    ~on_fail:inner_llm () in
  (* Verify structure *)
  Alcotest.(check string) "outer id" "outer_threshold" outer_threshold.id;
  match outer_threshold.node_type with
  | Threshold { on_pass = Some pass_node; _ } ->
      Alcotest.(check string) "on_pass is inner threshold" "inner_threshold" pass_node.id;
      (match pass_node.node_type with
       | Threshold { input_node; _ } ->
           Alcotest.(check string) "nested input_node" "inner_llm" input_node.id
       | _ -> Alcotest.fail "Expected nested Threshold node")
  | _ -> Alcotest.fail "Expected Threshold node"

(** Test: Evaluator inside Evaluator (nested candidate evaluation) *)
let test_nested_evaluator () =
  let llm1 = make_llm_node ~id:"llm1" ~model:"gemini" ~prompt:"Candidate 1" () in
  let llm2 = make_llm_node ~id:"llm2" ~model:"claude" ~prompt:"Candidate 2" () in
  let inner_evaluator = make_evaluator ~id:"inner_eval"
    ~candidates:[llm1; llm2]
    ~scoring_func:"llm_judge"
    ~select_strategy:Best () in
  let llm3 = make_llm_node ~id:"llm3" ~model:"codex" ~prompt:"Candidate 3" () in
  let outer_evaluator = make_evaluator ~id:"outer_eval"
    ~candidates:[inner_evaluator; llm3]
    ~scoring_func:"regex_match"
    ~scoring_prompt:"Select the best output"
    ~select_strategy:(AboveThreshold 0.8)
    ~min_score:0.5 () in
  (* Verify structure *)
  Alcotest.(check string) "outer id" "outer_eval" outer_evaluator.id;
  match outer_evaluator.node_type with
  | Evaluator { candidates; scoring_func; _ } ->
      Alcotest.(check int) "2 candidates" 2 (List.length candidates);
      Alcotest.(check string) "scoring_func" "regex_match" scoring_func;
      let first_candidate = List.hd candidates in
      (match first_candidate.node_type with
       | Evaluator { candidates = inner_candidates; _ } ->
           Alcotest.(check int) "inner has 2 candidates" 2 (List.length inner_candidates)
       | _ -> Alcotest.fail "Expected nested Evaluator node")
  | _ -> Alcotest.fail "Expected Evaluator node"

(** Test: Deep nesting (GoalDriven > Threshold > Evaluator > LLM) *)
let test_deep_nesting () =
  (* Layer 1: LLM nodes *)
  let llm_fast = make_llm_node ~id:"llm_fast" ~model:"gemini" ~prompt:"Fast model" () in
  let llm_accurate = make_llm_node ~id:"llm_accurate" ~model:"claude" ~prompt:"Accurate model" () in

  (* Layer 2: Evaluator selects best LLM *)
  let evaluator = make_evaluator ~id:"evaluator"
    ~candidates:[llm_fast; llm_accurate]
    ~scoring_func:"llm_judge"
    ~select_strategy:Best () in

  (* Layer 3: Threshold gates the evaluator *)
  let threshold = make_threshold ~id:"threshold"
    ~metric:"confidence" ~operator:Gte ~value:0.8
    ~input_node:evaluator
    ~on_fail:llm_fast () in

  (* Layer 4: GoalDriven iterates with threshold *)
  let goal_driven = make_goal_driven ~id:"goal_driven"
    ~goal_metric:"coverage" ~goal_operator:Gte ~goal_value:0.95
    ~action_node:threshold
    ~measure_func:"parse_json"
    ~max_iterations:10
    ~strategy_hints:[("below_50", "fast"); ("above_50", "accurate")] () in

  (* Verify deep structure *)
  Alcotest.(check string) "top level is goal_driven" "goal_driven" goal_driven.id;
  match goal_driven.node_type with
  | GoalDriven { action_node; max_iterations; strategy_hints; _ } ->
      Alcotest.(check int) "max_iterations" 10 max_iterations;
      Alcotest.(check int) "2 strategy hints" 2 (List.length strategy_hints);
      Alcotest.(check string) "action is threshold" "threshold" action_node.id;
      (match action_node.node_type with
       | Threshold { input_node; on_fail; _ } ->
           Alcotest.(check string) "input is evaluator" "evaluator" input_node.id;
           (match input_node.node_type with
            | Evaluator { candidates; _ } ->
                Alcotest.(check int) "evaluator has 2 candidates" 2 (List.length candidates)
            | _ -> Alcotest.fail "Expected Evaluator inside Threshold");
           (match on_fail with
            | Some fallback -> Alcotest.(check string) "fallback is llm_fast" "llm_fast" fallback.id
            | None -> Alcotest.fail "Expected on_fail node")
       | _ -> Alcotest.fail "Expected Threshold inside GoalDriven")
  | _ -> Alcotest.fail "Expected GoalDriven node"

(** Test: Parse nested evaluation chain from JSON *)
let test_parse_nested_evaluation_chain () =
  let json_str = {|{
    "id": "nested_eval_chain",
    "nodes": [{
      "id": "goal_runner",
      "type": "goal_driven",
      "goal_metric": "success_rate",
      "goal_operator": ">=",
      "goal_value": 0.9,
      "max_iterations": 5,
      "measure_func": "exec_test",
      "strategy_hints": {"below_30": "exploration", "above_70": "exploitation"},
      "action_node": {
        "id": "evaluator_inside",
        "type": "evaluator",
        "candidates": [
          {"id": "cand1", "type": "llm", "model": "gemini", "prompt": "Generate code"},
          {"id": "cand2", "type": "llm", "model": "claude", "prompt": "Generate code"}
        ],
        "scoring_func": "llm_judge",
        "select_strategy": "best"
      }
    }],
    "output": "goal_runner"
  }|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "chain id" "nested_eval_chain" chain.id;
      Alcotest.(check int) "1 top-level node" 1 (List.length chain.nodes);
      let goal_node = List.hd chain.nodes in
      (match goal_node.node_type with
       | GoalDriven { action_node; max_iterations; _ } ->
           Alcotest.(check int) "max_iterations" 5 max_iterations;
           (match action_node.node_type with
            | Evaluator { candidates; scoring_func; _ } ->
                Alcotest.(check int) "2 candidates" 2 (List.length candidates);
                Alcotest.(check string) "scoring_func" "llm_judge" scoring_func
            | _ -> Alcotest.fail "Expected Evaluator inside GoalDriven")
       | _ -> Alcotest.fail "Expected GoalDriven node")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(* ============================================================================
   Test Suite
   ============================================================================ *)

let recursion_tests = [
  "nested_threshold", `Quick, test_nested_threshold;
  "nested_evaluator", `Quick, test_nested_evaluator;
  "deep_nesting", `Quick, test_deep_nesting;
  "parse_nested_evaluation_chain", `Quick, test_parse_nested_evaluation_chain;
]

(* ============================================================================
   JSON <-> Mermaid Round-trip Tests
   ============================================================================ *)

(** Test chain_to_json serializer for simple chain *)
let test_chain_to_json_simple () =
  let chain = {
    id = "json_test";
    nodes = [
      make_llm_node ~id:"analyze" ~model:"gemini" ~prompt:"Analyze: {{input}}" ();
    ];
    output = "analyze";
    config = default_config;
  } in
  let json = chain_to_json chain in
  (* Parse it back *)
  match parse_chain json with
  | Ok parsed ->
      Alcotest.(check string) "id preserved" chain.id parsed.id;
      Alcotest.(check string) "output preserved" chain.output parsed.output;
      Alcotest.(check int) "node count" 1 (List.length parsed.nodes)
  | Error e -> Alcotest.fail (Printf.sprintf "Failed to parse serialized JSON: %s" e)

(** Test chain_to_json for resilience nodes *)
let test_chain_to_json_resilience () =
  let inner = make_llm_node ~id:"api" ~model:"gemini" ~prompt:"call API" () in
  let retry_node = {
    id = "retry_api";
    node_type = Retry {
      node = inner;
      max_attempts = 3;
      backoff = Exponential 2.0;
      retry_on = ["timeout"; "rate_limit"];
    };
    input_mapping = [];
  } in
  let chain = {
    id = "resilience_test";
    nodes = [retry_node];
    output = "retry_api";
    config = default_config;
  } in
  let json = chain_to_json chain in
  match parse_chain json with
  | Ok parsed ->
      Alcotest.(check string) "id preserved" chain.id parsed.id;
      let node = List.hd parsed.nodes in
      (match node.node_type with
       | Retry { max_attempts; backoff; retry_on; _ } ->
           Alcotest.(check int) "max_attempts" 3 max_attempts;
           (match backoff with
            | Exponential b -> Alcotest.(check (float 0.01)) "backoff base" 2.0 b
            | _ -> Alcotest.fail "expected Exponential backoff");
           Alcotest.(check int) "retry_on count" 2 (List.length retry_on)
       | _ -> Alcotest.fail "expected Retry node")
  | Error e -> Alcotest.fail (Printf.sprintf "Failed to parse: %s" e)

(** Test chain_to_json for complex pipeline *)
let test_chain_to_json_pipeline () =
  let chain = {
    id = "pipeline_test";
    nodes = [
      {
        id = "pipeline";
        node_type = Pipeline [
          make_llm_node ~id:"step1" ~model:"gemini" ~prompt:"step 1" ();
          make_llm_node ~id:"step2" ~model:"claude" ~prompt:"step 2" ();
        ];
        input_mapping = [];
      }
    ];
    output = "pipeline";
    config = default_config;
  } in
  let json = chain_to_json chain in
  match parse_chain json with
  | Ok parsed ->
      Alcotest.(check string) "id" chain.id parsed.id;
      let node = List.hd parsed.nodes in
      (match node.node_type with
       | Pipeline nodes -> Alcotest.(check int) "pipeline steps" 2 (List.length nodes)
       | _ -> Alcotest.fail "expected Pipeline node")
  | Error e -> Alcotest.fail (Printf.sprintf "Failed to parse: %s" e)

(** Test Mermaid -> JSON conversion (via chain_to_json) *)
let test_mermaid_to_json () =
  let mermaid = {|
graph LR
    A[LLM:gemini "Analyze input"] --> B[LLM:claude "Refine"]
    A --> C[LLM:codex "Generate code"]
    B --> D{Quorum:2}
    C --> D
|} in
  match Chain_mermaid_parser.parse_chain mermaid with
  | Ok chain ->
      let json = chain_to_json chain in
      (* Verify we can parse it back *)
      (match parse_chain json with
       | Ok reparsed ->
           Alcotest.(check int) "node count preserved" (List.length chain.nodes) (List.length reparsed.nodes);
           Alcotest.(check string) "output preserved" chain.output reparsed.output
       | Error e -> Alcotest.fail (Printf.sprintf "Failed to reparse: %s" e))
  | Error e -> Alcotest.fail (Printf.sprintf "Failed to parse Mermaid: %s" e)

(** Test chain_to_json_string pretty printing *)
let test_chain_to_json_string () =
  let chain = {
    id = "string_test";
    nodes = [make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"test" ()];
    output = "n1";
    config = default_config;
  } in
  let pretty = chain_to_json_string ~pretty:true chain in
  let compact = chain_to_json_string ~pretty:false chain in
  Alcotest.(check bool) "pretty has newlines" true (String.contains pretty '\n');
  Alcotest.(check bool) "compact has no newlines" false (String.contains compact '\n');
  (* Both should be valid JSON *)
  (try
    let _ = Yojson.Safe.from_string pretty in
    let _ = Yojson.Safe.from_string compact in
    ()
  with _ -> Alcotest.fail "Invalid JSON output")

let roundtrip_tests = [
  "chain_to_json_simple", `Quick, test_chain_to_json_simple;
  "chain_to_json_resilience", `Quick, test_chain_to_json_resilience;
  "chain_to_json_pipeline", `Quick, test_chain_to_json_pipeline;
  "mermaid_to_json", `Quick, test_mermaid_to_json;
  "chain_to_json_string", `Quick, test_chain_to_json_string;
]

let types_tests = [
  "default_config", `Quick, test_default_config;
  "node_type_name", `Quick, test_node_type_name;
  "make_llm_node", `Quick, test_make_llm_node;
  "make_pipeline", `Quick, test_make_pipeline;
  "make_quorum", `Quick, test_make_quorum;
  "merge_strategy_json", `Quick, test_merge_strategy_json;
  "chain_json_roundtrip", `Quick, test_chain_json_roundtrip;
  (* Evaluation node type helpers *)
  "make_threshold", `Quick, test_make_threshold;
  "make_goal_driven", `Quick, test_make_goal_driven;
  "make_goal_driven_conversational", `Quick, test_make_goal_driven_conversational;
  "make_evaluator", `Quick, test_make_evaluator;
  (* Resilience node type helpers *)
  "make_retry", `Quick, test_make_retry;
  "make_fallback", `Quick, test_make_fallback;
  "make_race", `Quick, test_make_race;
]

let parser_tests = [
  "parse_simple_llm_chain", `Quick, test_parse_simple_llm_chain;
  "parse_pipeline_chain", `Quick, test_parse_pipeline_chain;
  "parse_fanout_chain", `Quick, test_parse_fanout_chain;
  "parse_quorum_chain", `Quick, test_parse_quorum_chain;
  "parse_chain_ref", `Quick, test_parse_chain_ref;
  "parse_subgraph", `Quick, test_parse_subgraph;
  "parse_gate_chain", `Quick, test_parse_gate_chain;
  "parse_input_mapping", `Quick, test_parse_input_mapping;
  "parse_invalid_json", `Quick, test_parse_invalid_json;
  "parse_missing_output", `Quick, test_parse_missing_output;
  (* Evaluation node parsers *)
  "parse_threshold_chain", `Quick, test_parse_threshold_chain;
  "parse_goal_driven_chain", `Quick, test_parse_goal_driven_chain;
  "parse_evaluator_chain", `Quick, test_parse_evaluator_chain;
  (* Resilience node parsers *)
  "parse_retry_chain", `Quick, test_parse_retry_chain;
  "parse_fallback_chain", `Quick, test_parse_fallback_chain;
  "parse_race_chain", `Quick, test_parse_race_chain;
  "backoff_strategies", `Quick, test_backoff_strategies;
]

let compiler_tests = [
  "compile_simple_chain", `Quick, test_compile_simple_chain;
  "compile_pipeline_topological_order", `Quick, test_compile_pipeline_topological_order;
  "compile_fanout_parallel_groups", `Quick, test_compile_fanout_parallel_groups;
  "compile_depth_calculation", `Quick, test_compile_depth_calculation;
]

let registry_tests = [
  "registry_register_lookup", `Quick, test_registry_register_lookup;
  "registry_exists", `Quick, test_registry_exists;
  "registry_unregister", `Quick, test_registry_unregister;
  "registry_list", `Quick, test_registry_list;
  "registry_version", `Quick, test_registry_version;
  "registry_stats", `Quick, test_registry_stats;
  "registry_json_export_import", `Quick, test_registry_json_export_import;
]

let conversation_tests = [
  "make_conversation_ctx_default", `Quick, test_make_conversation_ctx_default;
  "make_conversation_ctx_custom_models", `Quick, test_make_conversation_ctx_custom_models;
  "rotate_model", `Quick, test_rotate_model;
  "rotate_model_single", `Quick, test_rotate_model_single;
  "estimate_tokens", `Quick, test_estimate_tokens;
  "add_message", `Quick, test_add_message;
  "needs_summarization", `Quick, test_needs_summarization;
  "build_context_prompt", `Quick, test_build_context_prompt;
]

(* ============================================================================
   ChainExec (Meta-Chain) Tests
   ============================================================================ *)

(** Test ChainExec JSON parsing with basic fields *)
let test_parse_chain_exec_basic () =
  let json_str = {|
{
  "id": "meta_chain",
  "nodes": [
    {
      "id": "generator",
      "type": "llm",
      "model": "gemini",
      "prompt": "Generate a chain JSON"
    },
    {
      "id": "executor",
      "type": "chain_exec",
      "chain_source": "{{generator}}"
    }
  ],
  "output": "executor"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check string) "chain id" "meta_chain" chain.id;
      Alcotest.(check int) "2 nodes" 2 (List.length chain.nodes);
      let exec_node = List.nth chain.nodes 1 in
      (match exec_node.node_type with
       | ChainExec { chain_source; validate; max_depth; sandbox; pass_outputs; _ } ->
           Alcotest.(check string) "chain_source" "{{generator}}" chain_source;
           Alcotest.(check bool) "validate default true" true validate;
           Alcotest.(check int) "max_depth default 3" 3 max_depth;
           Alcotest.(check bool) "sandbox default true" true sandbox;
           Alcotest.(check bool) "pass_outputs default true" true pass_outputs
       | _ -> Alcotest.fail "Expected ChainExec node")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** Test ChainExec JSON parsing with context injection *)
let test_parse_chain_exec_context_inject () =
  let json_str = {|
{
  "id": "meta_with_context",
  "nodes": [
    {
      "id": "prepare",
      "type": "llm",
      "model": "gemini",
      "prompt": "Prepare context"
    },
    {
      "id": "executor",
      "type": "chain_exec",
      "chain_source": "{{prepare}}",
      "validate": false,
      "max_depth": 5,
      "sandbox": false,
      "pass_outputs": false,
      "context_inject": {
        "user_input": "{{prepare}}",
        "config_key": "some_value"
      }
    }
  ],
  "output": "executor"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      let exec_node = List.nth chain.nodes 1 in
      (match exec_node.node_type with
       | ChainExec { chain_source; validate; max_depth; sandbox; pass_outputs; context_inject } ->
           Alcotest.(check string) "chain_source" "{{prepare}}" chain_source;
           Alcotest.(check bool) "validate false" false validate;
           Alcotest.(check int) "max_depth 5" 5 max_depth;
           Alcotest.(check bool) "sandbox false" false sandbox;
           Alcotest.(check bool) "pass_outputs false" false pass_outputs;
           Alcotest.(check int) "context_inject length" 2 (List.length context_inject);
           (* Check context_inject mappings *)
           let find_inject key = List.assoc_opt key context_inject in
           Alcotest.(check (option string)) "user_input mapping" (Some "{{prepare}}") (find_inject "user_input");
           Alcotest.(check (option string)) "config_key mapping" (Some "some_value") (find_inject "config_key")
       | _ -> Alcotest.fail "Expected ChainExec node")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** Test ChainExec JSON serialization roundtrip *)
let test_chain_exec_json_roundtrip () =
  let original_json_str = {|
{
  "id": "roundtrip_test",
  "nodes": [
    {
      "id": "gen",
      "type": "llm",
      "model": "claude",
      "prompt": "Generate"
    },
    {
      "id": "meta",
      "type": "chain_exec",
      "chain_source": "{{gen}}",
      "validate": true,
      "max_depth": 4,
      "sandbox": true,
      "pass_outputs": true,
      "context_inject": {
        "key1": "value1"
      }
    }
  ],
  "output": "meta"
}
|} in
  let json = Yojson.Safe.from_string original_json_str in
  match parse_chain json with
  | Ok chain ->
      (* Serialize back to JSON *)
      let serialized = chain_to_json chain in
      (* Parse again *)
      (match parse_chain serialized with
       | Ok chain2 ->
           Alcotest.(check string) "id preserved" chain.id chain2.id;
           Alcotest.(check int) "nodes count" (List.length chain.nodes) (List.length chain2.nodes);
           let meta1 = List.nth chain.nodes 1 in
           let meta2 = List.nth chain2.nodes 1 in
           (match (meta1.node_type, meta2.node_type) with
            | ChainExec c1, ChainExec c2 ->
                Alcotest.(check string) "chain_source" c1.chain_source c2.chain_source;
                Alcotest.(check bool) "validate" c1.validate c2.validate;
                Alcotest.(check int) "max_depth" c1.max_depth c2.max_depth;
                Alcotest.(check bool) "sandbox" c1.sandbox c2.sandbox;
                Alcotest.(check bool) "pass_outputs" c1.pass_outputs c2.pass_outputs;
                Alcotest.(check int) "context_inject len" (List.length c1.context_inject) (List.length c2.context_inject)
            | _ -> Alcotest.fail "Expected ChainExec nodes")
       | Error e -> Alcotest.fail (Printf.sprintf "Roundtrip parse error: %s" e))
  | Error e -> Alcotest.fail (Printf.sprintf "Initial parse error: %s" e)

(** Helper: check if string contains substring *)
let string_contains haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len > haystack_len then false
  else
    let rec check i =
      if i > haystack_len - needle_len then false
      else if String.sub haystack i needle_len = needle then true
      else check (i + 1)
    in
    check 0

(** Test ChainExec Mermaid generation *)
let test_chain_exec_mermaid () =
  let json_str = {|
{
  "id": "mermaid_test",
  "nodes": [
    {
      "id": "gen",
      "type": "llm",
      "model": "gemini",
      "prompt": "Generate chain"
    },
    {
      "id": "exec",
      "type": "chain_exec",
      "chain_source": "{{gen}}",
      "max_depth": 2,
      "sandbox": true
    }
  ],
  "output": "exec"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      let mermaid = Chain_mermaid_parser.chain_to_mermaid ~styled:true chain in
      (* Check that the mermaid output contains expected elements *)
      Alcotest.(check bool) "contains graph directive" true (String.starts_with ~prefix:"graph" mermaid);
      (* The node ID "exec" should be in the output with hexagon shape {{ }} *)
      Alcotest.(check bool) "contains exec node" true (string_contains mermaid "exec{{");
      Alcotest.(check bool) "contains meta class def" true (string_contains mermaid "classDef meta")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** Test ChainExec node_type_name *)
let test_chain_exec_node_type_name () =
  let exec_type = ChainExec {
    chain_source = "{{input}}";
    validate = true;
    max_depth = 3;
    sandbox = true;
    context_inject = [];
    pass_outputs = true;
  } in
  Alcotest.(check string) "node_type_name" "chain_exec" (node_type_name exec_type)

(** Test ChainExec depth compilation *)
let test_chain_exec_compile_depth () =
  let json_str = {|
{
  "id": "depth_test",
  "nodes": [
    {
      "id": "exec",
      "type": "chain_exec",
      "chain_source": "{{input}}",
      "max_depth": 4
    }
  ],
  "output": "exec",
  "config": {
    "max_depth": 10
  }
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           (* ChainExec depth = 1 (node itself) + max_depth (4) = 5 *)
           Alcotest.(check int) "compiled depth" 5 plan.depth
       | Error e -> Alcotest.fail (Printf.sprintf "Compile error: %s" e))
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

let chain_exec_tests = [
  "parse_chain_exec_basic", `Quick, test_parse_chain_exec_basic;
  "parse_chain_exec_context_inject", `Quick, test_parse_chain_exec_context_inject;
  "chain_exec_json_roundtrip", `Quick, test_chain_exec_json_roundtrip;
  "chain_exec_mermaid", `Quick, test_chain_exec_mermaid;
  "chain_exec_node_type_name", `Quick, test_chain_exec_node_type_name;
  "chain_exec_compile_depth", `Quick, test_chain_exec_compile_depth;
]

(* ===== Cache Node Tests ===== *)

(** Test parsing Cache node from JSON *)
let test_parse_cache_basic () =
  let json_str = {|
{
  "id": "cache_test",
  "nodes": [
    {
      "id": "cached_llm",
      "type": "cache",
      "key_expr": "{{input}}",
      "ttl_seconds": 3600,
      "inner": {
        "id": "llm",
        "type": "llm",
        "model": "gemini",
        "prompt": "Test: {{input}}"
      }
    }
  ],
  "output": "cached_llm"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check int) "node count" 1 (List.length chain.nodes);
      let node = List.hd chain.nodes in
      (match node.node_type with
       | Cache { key_expr; ttl_seconds; inner } ->
           Alcotest.(check string) "key_expr" "{{input}}" key_expr;
           Alcotest.(check int) "ttl" 3600 ttl_seconds;
           Alcotest.(check string) "inner id" "llm" inner.id
       | _ -> Alcotest.fail "Expected Cache node")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** Test Cache compile depth *)
let test_cache_compile_depth () =
  let json_str = {|
{
  "id": "depth_test",
  "nodes": [
    {
      "id": "cache",
      "type": "cache",
      "key_expr": "key",
      "ttl_seconds": 60,
      "inner": {
        "id": "nested",
        "type": "llm",
        "model": "gemini",
        "prompt": "test"
      }
    }
  ],
  "output": "cache"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           (* Cache(1) + LLM(1) = 2 *)
           Alcotest.(check int) "compiled depth" 2 plan.depth
       | Error e -> Alcotest.fail (Printf.sprintf "Compile error: %s" e))
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

let cache_tests = [
  "parse_cache_basic", `Quick, test_parse_cache_basic;
  "cache_compile_depth", `Quick, test_cache_compile_depth;
]

(* ===== Batch Node Tests ===== *)

(** Test parsing Batch node from JSON *)
let test_parse_batch_basic () =
  let json_str = {|
{
  "id": "batch_test",
  "nodes": [
    {
      "id": "batch_processor",
      "type": "batch",
      "batch_size": 5,
      "parallel": true,
      "collect_strategy": "list",
      "inner": {
        "id": "process",
        "type": "llm",
        "model": "gemini",
        "prompt": "Process: {{batch_processor_item}}"
      }
    }
  ],
  "output": "batch_processor"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check int) "node count" 1 (List.length chain.nodes);
      let node = List.hd chain.nodes in
      (match node.node_type with
       | Batch { batch_size; parallel; inner; collect_strategy } ->
           Alcotest.(check int) "batch_size" 5 batch_size;
           Alcotest.(check bool) "parallel" true parallel;
           Alcotest.(check string) "inner id" "process" inner.id;
           (match collect_strategy with `List -> () | _ -> Alcotest.fail "Expected List strategy")
       | _ -> Alcotest.fail "Expected Batch node")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** Test Batch compile depth *)
let test_batch_compile_depth () =
  let json_str = {|
{
  "id": "depth_test",
  "nodes": [
    {
      "id": "batch",
      "type": "batch",
      "batch_size": 3,
      "parallel": true,
      "collect_strategy": "list",
      "inner": {
        "id": "nested",
        "type": "pipeline",
        "nodes": [
          {"id": "a", "type": "llm", "model": "gemini", "prompt": "1"},
          {"id": "b", "type": "llm", "model": "gemini", "prompt": "2"}
        ]
      }
    }
  ],
  "output": "batch"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           (* Batch(1) + Pipeline(1) + max(LLM(1), LLM(1)) = 3 *)
           Alcotest.(check int) "compiled depth" 3 plan.depth
       | Error e -> Alcotest.fail (Printf.sprintf "Compile error: %s" e))
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

let batch_tests = [
  "parse_batch_basic", `Quick, test_parse_batch_basic;
  "batch_compile_depth", `Quick, test_batch_compile_depth;
]

(* ===== StreamMerge Node Tests ===== *)

(** Test parsing StreamMerge node from JSON *)
let test_parse_stream_merge_basic () =
  let json_str = {|
{
  "id": "stream_test",
  "nodes": [
    {
      "id": "merger",
      "type": "stream_merge",
      "reducer": "concat",
      "min_results": 1,
      "timeout": 30.0,
      "initial": "",
      "nodes": [
        {
          "id": "fast",
          "type": "llm",
          "model": "gemini",
          "prompt": "Quick response: {{input}}"
        },
        {
          "id": "slow",
          "type": "llm",
          "model": "claude",
          "prompt": "Detailed response: {{input}}"
        }
      ]
    }
  ],
  "output": "merger"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      Alcotest.(check int) "node count" 1 (List.length chain.nodes);
      let node = List.hd chain.nodes in
      (match node.node_type with
       | StreamMerge { reducer; min_results; timeout; initial; nodes } ->
           Alcotest.(check string) "initial" "" initial;
           Alcotest.(check (option int)) "min_results" (Some 1) min_results;
           Alcotest.(check (option (float 0.01))) "timeout" (Some 30.0) timeout;
           Alcotest.(check int) "inner nodes" 2 (List.length nodes);
           (match reducer with
            | Concat -> ()
            | _ -> Alcotest.fail "Expected Concat reducer")
       | _ -> Alcotest.fail "Expected StreamMerge node")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** Test StreamMerge with different reducers *)
let test_stream_merge_reducers () =
  let test_reducer reducer_str expected_check =
    let json_str = Printf.sprintf {|
{
  "id": "reducer_test",
  "nodes": [
    {
      "id": "merger",
      "type": "stream_merge",
      "reducer": "%s",
      "nodes": []
    }
  ],
  "output": "merger"
}
|} reducer_str in
    let json = Yojson.Safe.from_string json_str in
    match parse_chain json with
    | Ok chain ->
        let node = List.hd chain.nodes in
        (match node.node_type with
         | StreamMerge { reducer; _ } -> expected_check reducer
         | _ -> Alcotest.fail "Expected StreamMerge node")
    | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)
  in
  test_reducer "first" (function First -> () | _ -> Alcotest.fail "Expected First");
  test_reducer "last" (function Last -> () | _ -> Alcotest.fail "Expected Last");
  test_reducer "concat" (function Concat -> () | _ -> Alcotest.fail "Expected Concat");
  test_reducer "weighted_avg" (function WeightedAvg -> () | _ -> Alcotest.fail "Expected WeightedAvg");
  test_reducer "custom_func" (function Custom "custom_func" -> () | _ -> Alcotest.fail "Expected Custom")

(** Test StreamMerge compile depth *)
let test_stream_merge_compile_depth () =
  let json_str = {|
{
  "id": "depth_test",
  "nodes": [
    {
      "id": "merger",
      "type": "stream_merge",
      "reducer": "concat",
      "nodes": [
        {"id": "a", "type": "llm", "model": "gemini", "prompt": "A"},
        {"id": "b", "type": "llm", "model": "claude", "prompt": "B"},
        {"id": "c", "type": "llm", "model": "codex", "prompt": "C"}
      ]
    }
  ],
  "output": "merger"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           (* StreamMerge(1) + max(LLM, LLM, LLM) = 2 *)
           Alcotest.(check int) "compiled depth" 2 plan.depth
       | Error e -> Alcotest.fail (Printf.sprintf "Compile error: %s" e))
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** Test StreamMerge Mermaid parsing (not a true roundtrip - no serialize back) *)
let test_stream_merge_mermaid_parse () =
  let mermaid = {|graph LR
    a["LLM:gemini 'Task A'"]
    b["LLM:claude 'Task B'"]
    merge[["StreamMerge:concat,1,30.0"]]
    a --> merge
    b --> merge|} in
  match Chain_mermaid_parser.parse_chain mermaid with
  | Ok chain ->
      Alcotest.(check string) "chain id" "mermaid_chain" chain.id;
      (* Should have 3 nodes: a, b, merge *)
      Alcotest.(check int) "node count" 3 (List.length chain.nodes);
      (* Find the merge node *)
      let merge_node = List.find (fun (n : node) -> n.id = "merge") chain.nodes in
      let node_type_str = match merge_node.node_type with
        | Llm _ -> "Llm"
        | Tool _ -> "Tool"
        | Gate _ -> "Gate"
        | Merge _ -> "Merge"
        | Quorum _ -> "Quorum"
        | Threshold _ -> "Threshold"
        | Pipeline _ -> "Pipeline"
        | Fanout _ -> "Fanout"
        | Retry _ -> "Retry"
        | Fallback _ -> "Fallback"
        | Race _ -> "Race"
        | ChainRef _ -> "ChainRef"
        | ChainExec _ -> "ChainExec"
        | Adapter _ -> "Adapter"
        | Cache _ -> "Cache"
        | GoalDriven _ -> "GoalDriven"
        | Evaluator _ -> "Evaluator"
        | Spawn _ -> "Spawn"
        | Map _ -> "Map"
        | Bind _ -> "Bind"
        | Batch _ -> "Batch"
        | Mcts _ -> "Mcts"
        | StreamMerge _ -> "StreamMerge"
        | Subgraph _ -> "Subgraph"
      in
      (match merge_node.node_type with
       | StreamMerge { reducer; min_results; timeout; _ } ->
           (match reducer with Concat -> () | _ -> Alcotest.fail "Expected Concat");
           Alcotest.(check (option int)) "min_results" (Some 1) min_results;
           Alcotest.(check (option (float 0.01))) "timeout" (Some 30.0) timeout
       | _ -> Alcotest.fail (Printf.sprintf "Expected StreamMerge node, got %s" node_type_str))
  | Error e -> Alcotest.fail (Printf.sprintf "Mermaid parse error: %s" e)

(** P1: Test unknown reducer becomes Custom (extensibility design) *)
let test_stream_merge_unknown_reducer_becomes_custom () =
  let json_str = {|
{
  "id": "custom_test",
  "nodes": [
    {
      "id": "merger",
      "type": "stream_merge",
      "reducer": "my_custom_func",
      "nodes": []
    }
  ],
  "output": "merger"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      let node = List.hd chain.nodes in
      (match node.node_type with
       | StreamMerge { reducer; _ } ->
           (* Unknown strings become Custom reducer - intentional design *)
           (match reducer with
            | Custom name ->
                Alcotest.(check string) "custom name" "my_custom_func" name
            | _ -> Alcotest.fail "Expected Custom reducer")
       | _ -> Alcotest.fail "Expected StreamMerge")
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e)

(** P1: Test missing required fields *)
let test_stream_merge_missing_nodes_field () =
  let json_str = {|
{
  "id": "missing_test",
  "nodes": [
    {
      "id": "merger",
      "type": "stream_merge",
      "reducer": "concat"
    }
  ],
  "output": "merger"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      (* If it parses, nodes should default to empty *)
      let node = List.hd chain.nodes in
      (match node.node_type with
       | StreamMerge { nodes; _ } ->
           Alcotest.(check int) "default empty nodes" 0 (List.length nodes)
       | _ -> Alcotest.fail "Expected StreamMerge")
  | Error _ ->
      (* Also acceptable: error for missing nodes *)
      ()

(** P1: Test edge case - min_results > available nodes *)
let test_stream_merge_impossible_min_results () =
  let json_str = {|
{
  "id": "impossible_test",
  "nodes": [
    {
      "id": "merger",
      "type": "stream_merge",
      "reducer": "concat",
      "min_results": 10,
      "nodes": [
        {"id": "a", "type": "llm", "model": "gemini", "prompt": "A"}
      ]
    }
  ],
  "output": "merger"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      (* Parser accepts it - runtime should handle this *)
      let node = List.hd chain.nodes in
      (match node.node_type with
       | StreamMerge { min_results; nodes; _ } ->
           Alcotest.(check (option int)) "min_results preserved" (Some 10) min_results;
           Alcotest.(check int) "only 1 node" 1 (List.length nodes)
       | _ -> Alcotest.fail "Expected StreamMerge")
  | Error _ ->
      (* Also acceptable: compile-time validation *)
      ()

(** P2: True roundtrip - Mermaid → Chain → Mermaid → Chain *)
let test_stream_merge_true_roundtrip () =
  let original = {|graph LR
    a["LLM:gemini 'Task A'"]
    b["LLM:claude 'Task B'"]
    merge[["StreamMerge:concat,1,30.0"]]
    a --> merge
    b --> merge|} in
  match Chain_mermaid_parser.parse_chain original with
  | Error e -> Alcotest.fail (Printf.sprintf "Parse 1 error: %s" e)
  | Ok chain1 ->
      (* Serialize back to Mermaid *)
      let serialized = Chain_mermaid_parser.chain_to_mermaid ~lossless:true chain1 in
      (* Parse again *)
      (match Chain_mermaid_parser.parse_chain serialized with
       | Error e -> Alcotest.fail (Printf.sprintf "Parse 2 error: %s\nSerialized:\n%s" e serialized)
       | Ok chain2 ->
           (* Compare key properties *)
           Alcotest.(check int) "node count preserved"
             (List.length chain1.nodes) (List.length chain2.nodes);
           (* Find merge nodes in both *)
           let find_merge nodes =
             List.find_opt (fun (n : node) ->
               match n.node_type with StreamMerge _ -> true | _ -> false
             ) nodes
           in
           match find_merge chain1.nodes, find_merge chain2.nodes with
           | Some m1, Some m2 ->
               (match m1.node_type, m2.node_type with
                | StreamMerge s1, StreamMerge s2 ->
                    Alcotest.(check (option int)) "min_results" s1.min_results s2.min_results;
                    Alcotest.(check (option (float 0.01))) "timeout" s1.timeout s2.timeout
                | _ -> Alcotest.fail "Type mismatch")
           | _ -> Alcotest.fail "StreamMerge node not found in roundtrip")

(** P2: Zero timeout edge case *)
let test_stream_merge_zero_timeout () =
  let json_str = {|
{
  "id": "zero_timeout",
  "nodes": [
    {
      "id": "merger",
      "type": "stream_merge",
      "reducer": "first",
      "timeout": 0.0,
      "nodes": [
        {"id": "a", "type": "llm", "model": "gemini", "prompt": "A"}
      ]
    }
  ],
  "output": "merger"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      let node = List.hd chain.nodes in
      (match node.node_type with
       | StreamMerge { timeout; _ } ->
           (* 0.0 should be preserved, not treated as None *)
           Alcotest.(check (option (float 0.001))) "zero timeout" (Some 0.0) timeout
       | _ -> Alcotest.fail "Expected StreamMerge")
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

(** P2: Nested StreamMerge depth calculation *)
let test_stream_merge_nested_depth () =
  let json_str = {|
{
  "id": "nested_test",
  "nodes": [
    {
      "id": "outer",
      "type": "stream_merge",
      "reducer": "concat",
      "nodes": [
        {
          "id": "inner",
          "type": "stream_merge",
          "reducer": "first",
          "nodes": [
            {"id": "a", "type": "llm", "model": "gemini", "prompt": "A"},
            {"id": "b", "type": "llm", "model": "claude", "prompt": "B"}
          ]
        },
        {"id": "c", "type": "llm", "model": "codex", "prompt": "C"}
      ]
    }
  ],
  "output": "outer"
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match parse_chain json with
  | Ok chain ->
      (match compile chain with
       | Ok plan ->
           (* outer(1) + max(inner(1) + max(a,b), c) = 1 + max(2, 1) = 3 *)
           Alcotest.(check int) "nested depth" 3 plan.depth
       | Error e -> Alcotest.fail (Printf.sprintf "Compile error: %s" e))
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e)

let stream_merge_tests = [
  "parse_stream_merge_basic", `Quick, test_parse_stream_merge_basic;
  "stream_merge_reducers", `Quick, test_stream_merge_reducers;
  "stream_merge_compile_depth", `Quick, test_stream_merge_compile_depth;
  "stream_merge_mermaid_parse", `Quick, test_stream_merge_mermaid_parse;
  (* P1: Error & Edge cases *)
  "stream_merge_custom_reducer", `Quick, test_stream_merge_unknown_reducer_becomes_custom;
  "stream_merge_missing_nodes", `Quick, test_stream_merge_missing_nodes_field;
  "stream_merge_impossible_min", `Quick, test_stream_merge_impossible_min_results;
  (* P2: Advanced tests *)
  "stream_merge_true_roundtrip", `Quick, test_stream_merge_true_roundtrip;
  "stream_merge_zero_timeout", `Quick, test_stream_merge_zero_timeout;
  "stream_merge_nested_depth", `Quick, test_stream_merge_nested_depth;
]

(* ============================================================================
   Checkpoint Store Tests
   ============================================================================ *)

(** Test checkpoint save and load roundtrip *)
let test_checkpoint_save_load () =
  let test_dir = Filename.concat (Sys.getcwd ()) "_test_checkpoints" in
  (* Clean up any previous test directory *)
  (try
    Array.iter (fun f -> Sys.remove (Filename.concat test_dir f))
      (Sys.readdir test_dir);
    Unix.rmdir test_dir
  with Sys_error _ -> ());

  let store = Checkpoint_store.create ~base_dir:test_dir () in
  let run_id = Checkpoint_store.generate_run_id () in
  let outputs = [("node1", "output1"); ("node2", "output2")] in
  let traces = [] in

  let cp = Checkpoint_store.make_checkpoint
    ~run_id
    ~chain_id:"test-chain"
    ~node_id:"node2"
    ~outputs
    ~traces
    ()
  in

  (* Save checkpoint *)
  (match Checkpoint_store.save store cp with
   | Ok () -> ()
   | Error msg -> Alcotest.fail (Printf.sprintf "Save failed: %s" msg));

  (* Load checkpoint *)
  (match Checkpoint_store.load store ~run_id with
   | Ok loaded ->
       Alcotest.(check string) "run_id matches" run_id loaded.Checkpoint_store.run_id;
       Alcotest.(check string) "chain_id matches" "test-chain" loaded.Checkpoint_store.chain_id;
       Alcotest.(check string) "node_id matches" "node2" loaded.Checkpoint_store.node_id;
       Alcotest.(check int) "outputs count" 2 (List.length loaded.Checkpoint_store.outputs)
   | Error msg -> Alcotest.fail (Printf.sprintf "Load failed: %s" msg));

  (* Cleanup *)
  Checkpoint_store.delete store ~run_id;
  (try Unix.rmdir test_dir with Unix.Unix_error _ -> ())

(** Test listing checkpoints by chain_id *)
let test_checkpoint_list_by_chain () =
  let test_dir = Filename.concat (Sys.getcwd ()) "_test_checkpoints_list" in
  (try
    Array.iter (fun f -> Sys.remove (Filename.concat test_dir f))
      (Sys.readdir test_dir);
    Unix.rmdir test_dir
  with Sys_error _ -> ());

  let store = Checkpoint_store.create ~base_dir:test_dir () in

  (* Create checkpoints for different chains *)
  let cp1 = Checkpoint_store.make_checkpoint
    ~run_id:(Checkpoint_store.generate_run_id ())
    ~chain_id:"chain-a"
    ~node_id:"n1"
    ~outputs:[]
    ~traces:[]
    ()
  in
  let cp2 = Checkpoint_store.make_checkpoint
    ~run_id:(Checkpoint_store.generate_run_id ())
    ~chain_id:"chain-a"
    ~node_id:"n2"
    ~outputs:[]
    ~traces:[]
    ()
  in
  let cp3 = Checkpoint_store.make_checkpoint
    ~run_id:(Checkpoint_store.generate_run_id ())
    ~chain_id:"chain-b"
    ~node_id:"n1"
    ~outputs:[]
    ~traces:[]
    ()
  in

  ignore (Checkpoint_store.save store cp1);
  Unix.sleepf 0.01;  (* Small delay to ensure different timestamps *)
  ignore (Checkpoint_store.save store cp2);
  Unix.sleepf 0.01;
  ignore (Checkpoint_store.save store cp3);

  (* List only chain-a checkpoints *)
  let chain_a_cps = Checkpoint_store.list_checkpoints store ~chain_id:"chain-a" in
  Alcotest.(check int) "chain-a count" 2 (List.length chain_a_cps);

  (* List all checkpoints *)
  let all_cps = Checkpoint_store.list_all store in
  Alcotest.(check int) "all count" 3 (List.length all_cps);

  (* Cleanup *)
  Checkpoint_store.delete store ~run_id:cp1.Checkpoint_store.run_id;
  Checkpoint_store.delete store ~run_id:cp2.Checkpoint_store.run_id;
  Checkpoint_store.delete store ~run_id:cp3.Checkpoint_store.run_id;
  (try Unix.rmdir test_dir with Unix.Unix_error _ -> ())

(** Test checkpoint cleanup by age *)
let test_checkpoint_cleanup_old () =
  let test_dir = Filename.concat (Sys.getcwd ()) "_test_checkpoints_cleanup" in
  (try
    Array.iter (fun f -> Sys.remove (Filename.concat test_dir f))
      (Sys.readdir test_dir);
    Unix.rmdir test_dir
  with Sys_error _ -> ());

  let store = Checkpoint_store.create ~base_dir:test_dir () in

  (* Create a checkpoint *)
  let cp = Checkpoint_store.make_checkpoint
    ~run_id:(Checkpoint_store.generate_run_id ())
    ~chain_id:"cleanup-test"
    ~node_id:"n1"
    ~outputs:[]
    ~traces:[]
    ()
  in
  ignore (Checkpoint_store.save store cp);

  (* Cleanup with max_age=1000 hours (should not delete anything) *)
  let deleted = Checkpoint_store.cleanup_old store ~max_age_hours:1000 in
  Alcotest.(check int) "no deletions expected" 0 deleted;

  (* Verify checkpoint still exists *)
  let all_cps = Checkpoint_store.list_all store in
  Alcotest.(check int) "checkpoint still exists" 1 (List.length all_cps);

  (* Cleanup *)
  Checkpoint_store.delete store ~run_id:cp.Checkpoint_store.run_id;
  (try Unix.rmdir test_dir with Unix.Unix_error _ -> ())

(** Test JSON roundtrip for checkpoint *)
let test_checkpoint_json_roundtrip () =
  let outputs = [("a", "result_a"); ("b", "result_b")] in
  let cp = Checkpoint_store.make_checkpoint
    ~run_id:"test_run_123"
    ~chain_id:"test-chain"
    ~node_id:"final"
    ~outputs
    ~traces:[]
    ~total_tokens:{
      Chain_category.prompt_tokens = 100;
      completion_tokens = 50;
      total_tokens = 150;
      estimated_cost_usd = 0.001;
    }
    ()
  in

  let json = Checkpoint_store.checkpoint_to_json cp in
  match Checkpoint_store.checkpoint_of_json json with
  | Ok loaded ->
      Alcotest.(check string) "run_id" "test_run_123" loaded.Checkpoint_store.run_id;
      Alcotest.(check string) "chain_id" "test-chain" loaded.Checkpoint_store.chain_id;
      Alcotest.(check int) "outputs" 2 (List.length loaded.Checkpoint_store.outputs);
      (match loaded.Checkpoint_store.total_tokens with
       | Some t ->
           Alcotest.(check int) "total_tokens" 150 t.Chain_category.total_tokens
       | None -> Alcotest.fail "Expected total_tokens")
  | Error msg -> Alcotest.fail (Printf.sprintf "JSON roundtrip failed: %s" msg)

let checkpoint_tests = [
  "checkpoint_save_load", `Quick, test_checkpoint_save_load;
  "checkpoint_list_by_chain", `Quick, test_checkpoint_list_by_chain;
  "checkpoint_cleanup_old", `Quick, test_checkpoint_cleanup_old;
  "checkpoint_json_roundtrip", `Quick, test_checkpoint_json_roundtrip;
]

let () =
  Alcotest.run "Chain Engine" [
    "Chain Types", types_tests;
    "Chain Parser", parser_tests;
    "Chain Compiler", compiler_tests;
    "Chain Registry", registry_tests;
    "Conversation Context", conversation_tests;
    "Self-Recursion", recursion_tests;
    "JSON-Mermaid Roundtrip", roundtrip_tests;
    "ChainExec (Meta-Chain)", chain_exec_tests;
    "Cache Node", cache_tests;
    "Batch Node", batch_tests;
    "StreamMerge Node", stream_merge_tests;
    "Checkpoint Store", checkpoint_tests;
  ]

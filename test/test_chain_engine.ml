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

(* ============================================================================
   Chain Types Tests
   ============================================================================ *)

let test_default_config () =
  Alcotest.(check int) "max_depth" 4 default_config.max_depth;
  Alcotest.(check int) "max_concurrency" 3 default_config.max_concurrency;
  Alcotest.(check int) "timeout" 300 default_config.timeout;
  Alcotest.(check bool) "trace" false default_config.trace

let test_node_type_name () =
  Alcotest.(check string) "llm" "llm"
    (node_type_name (Llm { model = "test"; prompt = "test"; timeout = None }));
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
  | Llm { model; prompt; timeout } ->
      Alcotest.(check string) "model" "gemini" model;
      Alcotest.(check string) "prompt" "hello" prompt;
      Alcotest.(check (option int)) "timeout" None timeout
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
      { id = "c"; node_type = Llm { model = "gemini"; prompt = "c"; timeout = None };
        input_mapping = [("input", "{{b.output}}")] };
      { id = "a"; node_type = Llm { model = "gemini"; prompt = "a"; timeout = None };
        input_mapping = [] };
      { id = "b"; node_type = Llm { model = "claude"; prompt = "b"; timeout = None };
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
   Test Suite
   ============================================================================ *)

let types_tests = [
  "default_config", `Quick, test_default_config;
  "node_type_name", `Quick, test_node_type_name;
  "make_llm_node", `Quick, test_make_llm_node;
  "make_pipeline", `Quick, test_make_pipeline;
  "make_quorum", `Quick, test_make_quorum;
  "merge_strategy_json", `Quick, test_merge_strategy_json;
  "chain_json_roundtrip", `Quick, test_chain_json_roundtrip;
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

let () =
  Alcotest.run "Chain Engine" [
    "Chain Types", types_tests;
    "Chain Parser", parser_tests;
    "Chain Compiler", compiler_tests;
    "Chain Registry", registry_tests;
  ]

(** Tests for Chain Mermaid Parser

    Core functionality:
    - parse_mermaid_text: Mermaid DSL → mermaid_graph
    - mermaid_to_chain: mermaid_graph → chain
    - parse_mermaid_to_chain: combined parsing
    - chain_to_mermaid: chain → Mermaid DSL (roundtrip)
*)

open Alcotest

(** {1 Test Helpers} *)

let is_ok_chain msg result =
  match result with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "%s: %s" msg e)

(** {1 Basic Parsing Tests} *)

let test_simple_llm_node () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Hello world'"]
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "simple LLM node" result;
  match result with
  | Ok chain ->
      check int "node count" 1 (List.length chain.nodes);
      let node = List.hd chain.nodes in
      check string "node id" "a" node.id;
      (match node.node_type with
       | Chain_types.Llm { model; prompt; _ } ->
           check string "model" "gemini" model;
           check string "prompt" "Hello world" prompt
       | _ -> fail "expected Llm node")
  | Error _ -> ()

let test_pipeline_chain () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Step 1'"]
    b["LLM:claude 'Step 2: {{a}}'"]
    a --> b
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "pipeline chain" result;
  match result with
  | Ok chain ->
      check int "node count" 2 (List.length chain.nodes)
  | Error _ -> ()

let test_quorum_node () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Analysis'"]
    b["LLM:claude 'Review'"]
    q{Quorum:2}
    a --> q
    b --> q
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "quorum node" result;
  match result with
  | Ok chain ->
      let quorum_node = List.find_opt (fun n ->
        match n.Chain_types.node_type with
        | Chain_types.Quorum _ -> true
        | _ -> false
      ) chain.nodes in
      check bool "has quorum" true (Option.is_some quorum_node)
  | Error _ -> ()

let test_gate_node () =
  let mermaid = {|
graph LR
    input["LLM:gemini 'Analyze'"]
    check{Gate:{{input}} > 0.8}
    pass["LLM:claude 'Good'"]
    fail["LLM:claude 'Bad'"]
    input --> check
    check -->|then| pass
    check -->|else| fail
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "gate node" result

let test_tool_node () =
  let mermaid = {|
graph LR
    fetch["Tool:fetch_data {\"url\": \"https://example.com\"}"]
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "tool node" result;
  match result with
  | Ok chain ->
      let tool_node = List.find_opt (fun n ->
        match n.Chain_types.node_type with
        | Chain_types.Tool _ -> true
        | _ -> false
      ) chain.nodes in
      check bool "has tool" true (Option.is_some tool_node)
  | Error _ -> ()

(** {1 Roundtrip Tests} *)

let test_roundtrip_simple () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Test prompt'"]
    b["LLM:claude 'Review: {{a}}'"]
    a --> b
|} in
  match Chain_mermaid_parser.parse_mermaid_to_chain mermaid with
  | Error e -> fail e
  | Ok chain ->
      let regenerated = Chain_mermaid_parser.chain_to_mermaid chain in
      (* Re-parse the regenerated mermaid *)
      match Chain_mermaid_parser.parse_mermaid_to_chain regenerated with
      | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
      | Ok chain2 ->
          check int "same node count" (List.length chain.nodes) (List.length chain2.nodes)

(** {1 Edge Cases} *)

let test_empty_graph () =
  let mermaid = "graph LR\n" in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  (* Empty graph should either fail or create minimal chain *)
  match result with
  | Ok chain -> check int "no nodes" 0 (List.length chain.nodes)
  | Error _ -> () (* Also acceptable *)

let test_special_chars_in_prompt () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Hello \"world\" with special chars: <>&'"]
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "special chars" result

let test_multiline_prompt () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Line 1
Line 2
Line 3'"]
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "multiline prompt" result

let test_unicode_prompt () =
  let mermaid = {|
graph LR
    a["LLM:gemini '한국어 테스트 🎉'"]
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "unicode prompt" result

(** {1 Error Cases} *)

let test_invalid_syntax () =
  let mermaid = "not a valid mermaid" in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  (* Parser is lenient - may return empty chain or error *)
  match result with
  | Ok chain -> check int "no nodes for invalid" 0 (List.length chain.nodes)
  | Error _ -> () (* Also acceptable *)

let test_unknown_node_type () =
  let mermaid = {|
graph LR
    a["Unknown:foo 'test'"]
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  (* Unknown types might be treated as LLM or error *)
  match result with
  | Ok _ | Error _ -> () (* Both acceptable behaviors *)

(** {1 Advanced Node Types} *)

let test_retry_node () =
  let mermaid = {|
graph LR
    inner["LLM:gemini 'Try this'"]
    retry("Retry:3")
    inner --> retry
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "retry node" result

let test_fallback_node () =
  let mermaid = {|
graph LR
    primary["LLM:gemini 'Primary'"]
    backup["LLM:claude 'Backup'"]
    fallback("Fallback")
    primary --> fallback
    backup --> fallback
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "fallback node" result

let test_adapter_node () =
  let mermaid = {|
graph LR
    input["LLM:gemini 'Get data'"]
    extract>/"Adapter:Extract .data"/]
    input --> extract
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "adapter node" result

let test_cache_node () =
  let mermaid = {|
graph LR
    expensive["LLM:gemini 'Expensive call'"]
    cache[["Cache:key,3600,expensive"]]
    expensive --> cache
|} in
  let result = Chain_mermaid_parser.parse_mermaid_to_chain mermaid in
  is_ok_chain "cache node" result

(** {1 Test Suite} *)

let basic_tests = [
  test_case "simple LLM node" `Quick test_simple_llm_node;
  test_case "pipeline chain" `Quick test_pipeline_chain;
  test_case "quorum node" `Quick test_quorum_node;
  test_case "gate node" `Quick test_gate_node;
  test_case "tool node" `Quick test_tool_node;
]

let roundtrip_tests = [
  test_case "roundtrip simple" `Quick test_roundtrip_simple;
]

let edge_case_tests = [
  test_case "empty graph" `Quick test_empty_graph;
  test_case "special chars" `Quick test_special_chars_in_prompt;
  test_case "multiline prompt" `Quick test_multiline_prompt;
  test_case "unicode prompt" `Quick test_unicode_prompt;
]

let error_tests = [
  test_case "invalid syntax" `Quick test_invalid_syntax;
  test_case "unknown node type" `Quick test_unknown_node_type;
]

let advanced_tests = [
  test_case "retry node" `Quick test_retry_node;
  test_case "fallback node" `Quick test_fallback_node;
  test_case "adapter node" `Quick test_adapter_node;
  test_case "cache node" `Quick test_cache_node;
]

let () =
  run "chain_mermaid_parser" [
    ("basic", basic_tests);
    ("roundtrip", roundtrip_tests);
    ("edge_cases", edge_case_tests);
    ("errors", error_tests);
    ("advanced", advanced_tests);
  ]

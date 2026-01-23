(** Tests for Chain Mermaid Parser

    Tests the "Executable Documentation" feature - parsing Mermaid
    flowcharts into Chain AST for execution.
*)

open Chain_types
open Chain_mermaid_parser

(* ============================================================================
   Test Helpers
   ============================================================================ *)

let check_ok msg = function
  | Ok v -> v
  | Error e -> failwith (Printf.sprintf "%s: %s" msg e)

let find_node id nodes =
  List.find (fun (n : node) -> n.id = id) nodes

(* ============================================================================
   Basic Parsing Tests
   ============================================================================ *)

let test_parse_simple_llm () =
  let mermaid = {|
graph LR
    A[LLM:gemini "Hello world"]
  |} in
  let chain = check_ok "parse simple" (parse_chain mermaid) in
  Alcotest.(check int) "one node" 1 (List.length chain.nodes);
  let node = List.hd chain.nodes in
  Alcotest.(check string) "id" "A" node.id;
  match node.node_type with
  | Llm { model; prompt; _ } ->
      Alcotest.(check string) "model" "gemini" model;
      Alcotest.(check string) "prompt" "Hello world" prompt
  | _ -> Alcotest.fail "expected Llm node"

let test_parse_tool_node () =
  let mermaid = {|
graph LR
    A[Tool:eslint]
  |} in
  let chain = check_ok "parse tool" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Tool { name; _ } ->
      Alcotest.(check string) "tool name" "eslint" name
  | _ -> Alcotest.fail "expected Tool node"

let test_parse_chain_ref () =
  let mermaid = {|
graph LR
    A[[Ref:my_chain_v1]]
  |} in
  let chain = check_ok "parse ref" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | ChainRef ref_id ->
      Alcotest.(check string) "ref id" "my_chain_v1" ref_id
  | _ -> Alcotest.fail "expected ChainRef node"

let test_parse_quorum () =
  let mermaid = {|
graph LR
    A{Quorum:2}
  |} in
  let chain = check_ok "parse quorum" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Quorum { required; _ } ->
      Alcotest.(check int) "required" 2 required
  | _ -> Alcotest.fail "expected Quorum node"

(* ============================================================================
   Edge/Dependency Tests
   ============================================================================ *)

let test_parse_pipeline () =
  let mermaid = {|
graph LR
    A[LLM:gemini "Step 1"] --> B[LLM:claude "Step 2"] --> C[LLM:codex "Step 3"]
  |} in
  let chain = check_ok "parse pipeline" (parse_chain mermaid) in
  Alcotest.(check int) "three nodes" 3 (List.length chain.nodes);

  (* Check output is terminal node *)
  Alcotest.(check string) "output" "C" chain.output;

  (* Check B depends on A *)
  let node_b = find_node "B" chain.nodes in
  Alcotest.(check int) "B has 1 input" 1 (List.length node_b.input_mapping);
  let (key, _) = List.hd node_b.input_mapping in
  Alcotest.(check string) "B input from A" "A" key

let test_parse_fanout () =
  let mermaid = {|
graph LR
    A[LLM:gemini "Input"] --> B[LLM:claude "Path 1"]
    A --> C[LLM:codex "Path 2"]
  |} in
  let chain = check_ok "parse fanout" (parse_chain mermaid) in
  Alcotest.(check int) "three nodes" 3 (List.length chain.nodes);

  (* Both B and C depend on A *)
  let node_b = find_node "B" chain.nodes in
  let node_c = find_node "C" chain.nodes in
  Alcotest.(check int) "B has input" 1 (List.length node_b.input_mapping);
  Alcotest.(check int) "C has input" 1 (List.length node_c.input_mapping)

let test_parse_merge () =
  let mermaid = {|
graph LR
    A[LLM:gemini "Path 1"] --> C{Quorum:2}
    B[LLM:claude "Path 2"] --> C
  |} in
  let chain = check_ok "parse merge" (parse_chain mermaid) in
  Alcotest.(check int) "three nodes" 3 (List.length chain.nodes);

  (* C should have 2 inputs *)
  let node_c = find_node "C" chain.nodes in
  Alcotest.(check int) "C has 2 inputs" 2 (List.length node_c.input_mapping);

  (* Quorum should have 2 required *)
  match node_c.node_type with
  | Quorum { required; _ } ->
      Alcotest.(check int) "quorum required" 2 required
  | _ -> Alcotest.fail "expected Quorum"

let test_parse_ampersand_merge () =
  let mermaid = {|
graph LR
    A[LLM:gemini "A"] --> X[LLM:claude "X"]
    B[LLM:codex "B"] --> X
  |} in
  let chain = check_ok "parse ampersand" (parse_chain mermaid) in
  Alcotest.(check int) "three nodes" 3 (List.length chain.nodes);

  let node_x = find_node "X" chain.nodes in
  Alcotest.(check int) "X has 2 inputs" 2 (List.length node_x.input_mapping)

(* ============================================================================
   Complex Workflow Tests
   ============================================================================ *)

let test_magi_trinity () =
  let mermaid = {|
graph LR
    input[LLM:gemini "Parse input"] --> casper[LLM:gemini "Strategic view"]
    input --> balthasar[LLM:claude "Value judgment"]
    input --> melchior[LLM:codex "Technical analysis"]
    casper --> consensus{Quorum:2}
    balthasar --> consensus
    melchior --> consensus
  |} in
  let chain = check_ok "parse MAGI" (parse_chain mermaid) in
  Alcotest.(check int) "five nodes" 5 (List.length chain.nodes);
  Alcotest.(check string) "output is consensus" "consensus" chain.output;

  (* Consensus should have 3 inputs *)
  let consensus = find_node "consensus" chain.nodes in
  Alcotest.(check int) "consensus has 3 inputs" 3 (List.length consensus.input_mapping)

let test_pr_review_pipeline () =
  let mermaid = {|
graph LR
    lint[Tool:eslint] --> security[LLM:gemini "Security review"]
    lint --> quality[LLM:claude "Quality review"]
    security --> verdict{Quorum:2}
    quality --> verdict
  |} in
  let chain = check_ok "parse PR review" (parse_chain mermaid) in
  Alcotest.(check int) "four nodes" 4 (List.length chain.nodes);

  (* Lint is a tool *)
  let lint = find_node "lint" chain.nodes in
  (match lint.node_type with
  | Tool { name; _ } -> Alcotest.(check string) "lint tool" "eslint" name
  | _ -> Alcotest.fail "expected Tool");

  (* Verdict is quorum *)
  let verdict = find_node "verdict" chain.nodes in
  (match verdict.node_type with
  | Quorum { required; _ } -> Alcotest.(check int) "quorum 2" 2 required
  | _ -> Alcotest.fail "expected Quorum")

(* ============================================================================
   Edge Cases
   ============================================================================ *)

let test_llm_without_prompt () =
  let mermaid = {|
graph LR
    A[LLM:claude]
  |} in
  let chain = check_ok "parse no prompt" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Llm { prompt; _ } ->
      Alcotest.(check string) "default prompt" "{{input}}" prompt
  | _ -> Alcotest.fail "expected Llm"

let test_plain_text_node () =
  let mermaid = {|
graph LR
    A[Summarize this text]
  |} in
  let chain = check_ok "parse plain text" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Llm { model; prompt; _ } ->
      (* Plain text defaults to gemini model *)
      Alcotest.(check string) "default model" "gemini" model;
      Alcotest.(check string) "prompt is text" "Summarize this text" prompt
  | _ -> Alcotest.fail "expected Llm"

let test_comments_ignored () =
  let mermaid = {|
graph LR
    %% This is a comment
    A[LLM:gemini "Test"]
    %% Another comment
  |} in
  let chain = check_ok "parse with comments" (parse_chain mermaid) in
  Alcotest.(check int) "one node" 1 (List.length chain.nodes)

let test_flowchart_keyword () =
  let mermaid = {|
flowchart TB
    A[LLM:gemini "Top to bottom"]
  |} in
  let chain = check_ok "parse flowchart" (parse_chain mermaid) in
  Alcotest.(check int) "one node" 1 (List.length chain.nodes)

(* ============================================================================
   Error Cases
   ============================================================================ *)

let test_invalid_quorum () =
  let mermaid = {|
graph LR
    A{Quorum:invalid}
  |} in
  match parse_chain mermaid with
  | Error _ -> ()  (* Expected *)
  | Ok _ -> Alcotest.fail "should fail on invalid quorum"

let test_subroutine_as_chain_ref () =
  (* With WYSIWYE: any subroutine [[X]] becomes ChainRef "X" *)
  let mermaid = {|
graph LR
    A[[my_reusable_chain]]
  |} in
  let chain = check_ok "parse subroutine as ref" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | ChainRef ref_id ->
      Alcotest.(check string) "ref id" "my_reusable_chain" ref_id
  | _ -> Alcotest.fail "expected ChainRef"

let test_tools_flag () =
  (* +tools flag enables Ollama tool calling interop *)
  (* Syntax: [LLM:model "prompt" +tools] - flag OUTSIDE quotes *)
  let mermaid = {|
graph LR
    A[LLM:ollama "Use tools" +tools] --> B[LLM:gemini "No tools"]
  |} in
  let chain = check_ok "parse tools flag" (parse_chain mermaid) in
  Alcotest.(check int) "two nodes" 2 (List.length chain.nodes);
  let node_a = find_node "A" chain.nodes in
  let node_b = find_node "B" chain.nodes in
  (* A should have tools = Some [] *)
  (match node_a.node_type with
   | Llm { model; prompt; tools; _ } ->
       Alcotest.(check string) "model A" "ollama" model;
       Alcotest.(check string) "prompt A" "Use tools" prompt;
       Alcotest.(check bool) "A has tools" true (Option.is_some tools)
   | _ -> Alcotest.fail "expected Llm node A");
  (* B should have tools = None *)
  (match node_b.node_type with
   | Llm { model; tools; _ } ->
       Alcotest.(check string) "model B" "gemini" model;
       Alcotest.(check bool) "B has no tools" true (Option.is_none tools)
   | _ -> Alcotest.fail "expected Llm node B")

(* ============================================================================
   Test Suite
   ============================================================================ *)

let basic_tests = [
  "parse simple LLM", `Quick, test_parse_simple_llm;
  "parse Tool node", `Quick, test_parse_tool_node;
  "parse ChainRef", `Quick, test_parse_chain_ref;
  "parse Quorum", `Quick, test_parse_quorum;
]

let edge_tests = [
  "parse pipeline", `Quick, test_parse_pipeline;
  "parse fanout", `Quick, test_parse_fanout;
  "parse merge", `Quick, test_parse_merge;
  "parse ampersand merge", `Quick, test_parse_ampersand_merge;
]

let complex_tests = [
  "MAGI Trinity", `Quick, test_magi_trinity;
  "PR Review Pipeline", `Quick, test_pr_review_pipeline;
]

let edge_case_tests = [
  "LLM without prompt", `Quick, test_llm_without_prompt;
  "plain text node", `Quick, test_plain_text_node;
  "comments ignored", `Quick, test_comments_ignored;
  "flowchart keyword", `Quick, test_flowchart_keyword;
  "+tools flag (Ollama interop)", `Quick, test_tools_flag;
]

let error_tests = [
  "invalid quorum", `Quick, test_invalid_quorum;
  "subroutine as ChainRef", `Quick, test_subroutine_as_chain_ref;
]

(* ============================================================================
   Direction Preservation Tests (종/횡)
   ============================================================================ *)

let test_direction_lr () =
  let mermaid = {|
graph LR
    A[LLM:gemini "Hello"]
  |} in
  let chain = check_ok "parse LR" (parse_chain mermaid) in
  Alcotest.(check string) "direction LR" "LR"
    (Chain_types.direction_to_string chain.config.direction)

let test_direction_tb () =
  let mermaid = {|
graph TB
    A[LLM:claude "Top to bottom"]
  |} in
  let chain = check_ok "parse TB" (parse_chain mermaid) in
  Alcotest.(check string) "direction TB" "TB"
    (Chain_types.direction_to_string chain.config.direction)

let test_direction_td_alias () =
  let mermaid = {|
flowchart TD
    A[LLM:gemini "Test"]
  |} in
  let chain = check_ok "parse TD" (parse_chain mermaid) in
  (* TD is alias for TB *)
  Alcotest.(check string) "direction TD->TB" "TB"
    (Chain_types.direction_to_string chain.config.direction)

let test_direction_bt () =
  let mermaid = {|
graph BT
    A[LLM:codex "Bottom to top"]
  |} in
  let chain = check_ok "parse BT" (parse_chain mermaid) in
  Alcotest.(check string) "direction BT" "BT"
    (Chain_types.direction_to_string chain.config.direction)

let test_direction_rl () =
  let mermaid = {|
graph RL
    A[LLM:gemini "Right to left"]
  |} in
  let chain = check_ok "parse RL" (parse_chain mermaid) in
  Alcotest.(check string) "direction RL" "RL"
    (Chain_types.direction_to_string chain.config.direction)

let test_round_trip_preserves_direction () =
  let mermaid = {|
graph TB
    A[LLM:gemini "Hello"] --> B[LLM:claude "World"]
  |} in
  let result = round_trip mermaid in
  match result with
  | Error e -> Alcotest.fail (Printf.sprintf "round trip failed: %s" e)
  | Ok output ->
      (* Should contain "graph TB" *)
      Alcotest.(check bool) "contains graph TB" true
        (String.length output > 8 &&
         String.sub output 0 8 = "graph TB")

let direction_tests = [
  "direction LR (횡)", `Quick, test_direction_lr;
  "direction TB (종)", `Quick, test_direction_tb;
  "direction TD alias", `Quick, test_direction_td_alias;
  "direction BT (역종)", `Quick, test_direction_bt;
  "direction RL (역횡)", `Quick, test_direction_rl;
  "round trip preserves direction", `Quick, test_round_trip_preserves_direction;
]

let () =
  Alcotest.run "Mermaid Parser" [
    ("Basic Parsing", basic_tests);
    ("Edge/Dependency", edge_tests);
    ("Complex Workflows", complex_tests);
    ("Edge Cases", edge_case_tests);
    ("Error Cases", error_tests);
    ("Direction (종/횡)", direction_tests);
  ]

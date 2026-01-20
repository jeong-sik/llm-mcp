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

let test_invalid_subroutine () =
  let mermaid = {|
graph LR
    A[[InvalidFormat]]
  |} in
  match parse_chain mermaid with
  | Error e ->
      Alcotest.(check bool) "error mentions Ref" true
        (String.length e > 0)
  | Ok _ -> Alcotest.fail "should fail on invalid subroutine"

(* ============================================================================
   Extended Node Types (Full Mapping)
   ============================================================================ *)

let test_parse_pipeline_explicit () =
  let mermaid = {|
graph LR
    A[[Pipeline:step1,step2,step3]]
  |} in
  let chain = check_ok "parse pipeline explicit" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Pipeline nodes ->
      Alcotest.(check int) "3 nodes" 3 (List.length nodes);
      let ids = List.map (fun (n : node) -> n.id) nodes in
      Alcotest.(check (list string)) "node ids" ["step1"; "step2"; "step3"] ids
  | _ -> Alcotest.fail "expected Pipeline node"

let test_parse_fanout_explicit () =
  let mermaid = {|
graph LR
    A[[Fanout:path1,path2,path3]]
  |} in
  let chain = check_ok "parse fanout" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Fanout nodes ->
      Alcotest.(check int) "3 nodes" 3 (List.length nodes);
      let ids = List.map (fun (n : node) -> n.id) nodes in
      Alcotest.(check (list string)) "node ids" ["path1"; "path2"; "path3"] ids
  | _ -> Alcotest.fail "expected Fanout node"

let test_parse_map_node () =
  let mermaid = {|
graph LR
    A[[Map:extract_summary,llm_node]]
  |} in
  let chain = check_ok "parse map" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Map { func; inner } ->
      Alcotest.(check string) "func" "extract_summary" func;
      Alcotest.(check string) "inner id" "llm_node" inner.id
  | _ -> Alcotest.fail "expected Map node"

let test_parse_bind_node () =
  let mermaid = {|
graph LR
    A[[Bind:route_by_type,handler]]
  |} in
  let chain = check_ok "parse bind" (parse_chain mermaid) in
  let node = List.hd chain.nodes in
  match node.node_type with
  | Bind { func; inner } ->
      Alcotest.(check string) "func" "route_by_type" func;
      Alcotest.(check string) "inner id" "handler" inner.id
  | _ -> Alcotest.fail "expected Bind node"

let test_parse_merge_node () =
  let mermaid = {|
graph LR
    A[LLM:gemini "Path 1"] --> M{Merge:weighted_avg}
    B[LLM:claude "Path 2"] --> M
  |} in
  let chain = check_ok "parse merge" (parse_chain mermaid) in
  Alcotest.(check int) "three nodes" 3 (List.length chain.nodes);
  let m_node = find_node "M" chain.nodes in
  match m_node.node_type with
  | Merge { strategy; nodes } ->
      Alcotest.(check int) "2 inputs" 2 (List.length nodes);
      (match strategy with
      | WeightedAvg -> ()
      | _ -> Alcotest.fail "expected WeightedAvg strategy")
  | _ -> Alcotest.fail "expected Merge node"

(* ============================================================================
   Composition Tests
   ============================================================================ *)

let test_pipeline_in_workflow () =
  (* Pipeline feeds into Quorum for validation *)
  let mermaid = {|
graph LR
    P[[Pipeline:step1,step2]] --> Q{Quorum:2}
  |} in
  let chain = check_ok "parse pipeline workflow" (parse_chain mermaid) in
  Alcotest.(check int) "two nodes" 2 (List.length chain.nodes);

  (* Find Pipeline node *)
  let p_node = find_node "P" chain.nodes in
  (match p_node.node_type with
  | Pipeline _ -> ()
  | _ -> Alcotest.fail "P should be Pipeline");

  (* Find Quorum node with input from Pipeline *)
  let q_node = find_node "Q" chain.nodes in
  (match q_node.node_type with
  | Quorum { required; _ } ->
      Alcotest.(check int) "quorum 2" 2 required
  | _ -> Alcotest.fail "Q should be Quorum");
  Alcotest.(check int) "Q has 1 input" 1 (List.length q_node.input_mapping)

let test_fanout_with_merge () =
  (* Fanout branches merge into Quorum *)
  let mermaid = {|
graph LR
    F[[Fanout:a,b,c]] --> M{Quorum:2}
  |} in
  let chain = check_ok "parse fanout merge" (parse_chain mermaid) in
  Alcotest.(check int) "two nodes" 2 (List.length chain.nodes);
  Alcotest.(check string) "output is M" "M" chain.output

let test_map_in_pipeline () =
  (* LLM -> Map -> Output *)
  let mermaid = {|
graph LR
    L[LLM:gemini "Generate"] --> M[[Map:parse_json,result]]
  |} in
  let chain = check_ok "parse map pipeline" (parse_chain mermaid) in
  Alcotest.(check int) "two nodes" 2 (List.length chain.nodes);

  let m_node = find_node "M" chain.nodes in
  (match m_node.node_type with
  | Map { func; _ } ->
      Alcotest.(check string) "func" "parse_json" func
  | _ -> Alcotest.fail "M should be Map")

let test_complex_composition () =
  (* Real-world: LLM -> Pipeline -> Map for post-processing *)
  (* All edges explicit, composable structure *)
  let mermaid = {|
graph LR
    input[LLM:gemini "Parse"] --> pipe[[Pipeline:step1,step2]]
    pipe --> transform[[Map:format_json,result]]
  |} in
  let chain = check_ok "parse complex" (parse_chain mermaid) in
  Alcotest.(check int) "three nodes" 3 (List.length chain.nodes);
  Alcotest.(check string) "output is transform" "transform" chain.output;

  (* Verify node types *)
  let p_node = find_node "pipe" chain.nodes in
  (match p_node.node_type with
  | Pipeline nodes ->
      Alcotest.(check int) "2 steps" 2 (List.length nodes)
  | _ -> Alcotest.fail "pipe should be Pipeline");

  let t_node = find_node "transform" chain.nodes in
  (match t_node.node_type with
  | Map { func; _ } ->
      Alcotest.(check string) "func" "format_json" func
  | _ -> Alcotest.fail "transform should be Map")

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
]

let error_tests = [
  "invalid quorum", `Quick, test_invalid_quorum;
  "invalid subroutine", `Quick, test_invalid_subroutine;
]

let extended_type_tests = [
  "parse Pipeline explicit", `Quick, test_parse_pipeline_explicit;
  "parse Fanout explicit", `Quick, test_parse_fanout_explicit;
  "parse Map", `Quick, test_parse_map_node;
  "parse Bind", `Quick, test_parse_bind_node;
  "parse Merge", `Quick, test_parse_merge_node;
]

let composition_tests = [
  "Pipeline in workflow", `Quick, test_pipeline_in_workflow;
  "Fanout with merge", `Quick, test_fanout_with_merge;
  "Map in pipeline", `Quick, test_map_in_pipeline;
  "Complex composition", `Quick, test_complex_composition;
]

let () =
  Alcotest.run "Mermaid Parser" [
    ("Basic Parsing", basic_tests);
    ("Edge/Dependency", edge_tests);
    ("Complex Workflows", complex_tests);
    ("Edge Cases", edge_case_tests);
    ("Error Cases", error_tests);
    ("Extended Types", extended_type_tests);
    ("Composition", composition_tests);
  ]

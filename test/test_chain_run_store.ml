(** Tests for Chain_run_store module

    Pure function tests:
    - truncate: string truncation with indicator
    - is_internal_key: internal key filtering
    - collect_all_nodes: recursive node collection
    - node_view_to_json: JSON serialization
    - run_summary_to_json: JSON serialization
    - output_entry_to_json: JSON serialization
*)

open Alcotest
open Chain_run_store
open Chain_types

(** {1 truncate Tests} *)

let test_truncate_short () =
  let text, truncated = truncate "hello" 10 in
  check string "unchanged" "hello" text;
  check bool "not truncated" false truncated

let test_truncate_exact () =
  let text, truncated = truncate "hello" 5 in
  check string "unchanged" "hello" text;
  check bool "not truncated" false truncated

let test_truncate_long () =
  let text, truncated = truncate "hello world" 5 in
  check string "truncated" "hello..." text;
  check bool "truncated" true truncated

let test_truncate_empty () =
  let text, truncated = truncate "" 10 in
  check string "empty" "" text;
  check bool "not truncated" false truncated

let test_truncate_zero () =
  let text, truncated = truncate "abc" 0 in
  check string "empty with ..." "..." text;
  check bool "truncated" true truncated

(** {1 is_internal_key Tests} *)

let test_internal_key_double_underscore () =
  check bool "__internal" true (is_internal_key "__internal");
  check bool "__chain_input" true (is_internal_key "__chain_input")

let test_internal_key_parent () =
  check bool "parent.node" true (is_internal_key "parent.node_id");
  check bool "parent." true (is_internal_key "parent.")

let test_internal_key_normal () =
  check bool "regular" false (is_internal_key "node_output");
  check bool "underscore" false (is_internal_key "_single");
  check bool "empty" false (is_internal_key "")

(** {1 collect_all_nodes Tests} *)

let make_llm_node ~id ?(model="gemini") prompt : node = {
  id;
  node_type = Llm {
    model;
    prompt;
    system = None;
    timeout = None;
    tools = None;
    prompt_ref = None;
    prompt_vars = [];
  };
  input_mapping = [];
  output_key = None;
  depends_on = None;
}

let make_tool_node ~id ~name : node = {
  id;
  node_type = Tool { name; args = `Null };
  input_mapping = [];
  output_key = None;
  depends_on = None;
}

let test_collect_simple_chain () =
  let node1 = make_llm_node ~id:"a" "test" in
  let node2 = make_tool_node ~id:"b" ~name:"test_tool" in
  let chain = make_chain ~id:"test-chain" ~nodes:[node1; node2] ~output:"b" () in
  let collected = collect_all_nodes chain in
  check int "2 nodes" 2 (List.length collected);
  let ids = List.map (fun (n : node) -> n.id) collected in
  check bool "has a" true (List.mem "a" ids);
  check bool "has b" true (List.mem "b" ids)

let test_collect_pipeline () =
  let inner1 = make_llm_node ~id:"inner1" "prompt1" in
  let inner2 = make_llm_node ~id:"inner2" ~model:"claude" "prompt2" in
  let pipeline : node = {
    id = "pipe";
    node_type = Pipeline [inner1; inner2];
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let chain = make_chain ~id:"pipe-chain" ~nodes:[pipeline] ~output:"pipe" () in
  let collected = collect_all_nodes chain in
  check int "3 nodes (pipe + 2 inner)" 3 (List.length collected)

(** {1 JSON Serialization Tests} *)

let test_node_view_to_json () =
  let nv : node_view = {
    id = "n1";
    node_type = "llm";
    status = "success";
    start_time = Some 1000.0;
    end_time = Some 1100.0;
    duration_ms = Some 100;
    input_mapping = [("a", "input")];
    depends_on = ["input"];
    output_preview = Some "result...";
    output_size = Some 1000;
    output_truncated = None;
    error = None;
  } in
  let json = node_view_to_json nv in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has type" true (List.mem_assoc "type" fields);
      check bool "has status" true (List.mem_assoc "status" fields);
      (match List.assoc "id" fields with
       | `String s -> check string "id value" "n1" s
       | _ -> fail "expected string");
      (match List.assoc "status" fields with
       | `String s -> check string "status value" "success" s
       | _ -> fail "expected string")
  | _ -> fail "expected Assoc"

let test_run_summary_to_json () =
  let run : run_record = {
    run_id = "run-123";
    chain_id = "test-chain";
    started_at = 1704067200.0;
    duration_ms = 500;
    success = true;
    mermaid = "graph LR";
    execution_order = ["a"; "b"];
    parallel_groups = [["a"]; ["b"]];
    nodes = [];
    trace = [];
    outputs = [];
    chain_json = `Null;
  } in
  let json = run_summary_to_json run in
  match json with
  | `Assoc fields ->
      (match List.assoc "run_id" fields with
       | `String s -> check string "run_id" "run-123" s
       | _ -> fail "expected string");
      (match List.assoc "success" fields with
       | `Bool b -> check bool "success" true b
       | _ -> fail "expected bool");
      (match List.assoc "node_count" fields with
       | `Int n -> check int "node_count" 0 n
       | _ -> fail "expected int")
  | _ -> fail "expected Assoc"

let test_run_record_to_json () =
  let run : run_record = {
    run_id = "run-456";
    chain_id = "full-chain";
    started_at = 1704067200.0;
    duration_ms = 1000;
    success = false;
    mermaid = "graph TB\n  a --> b";
    execution_order = ["a"; "b"; "c"];
    parallel_groups = [["a"; "b"]; ["c"]];
    nodes = [];
    trace = [];
    outputs = [];
    chain_json = `Assoc [("id", `String "full-chain")];
  } in
  let json = run_record_to_json run in
  match json with
  | `Assoc fields ->
      check bool "has mermaid" true (List.mem_assoc "mermaid" fields);
      check bool "has execution_order" true (List.mem_assoc "execution_order" fields);
      check bool "has parallel_groups" true (List.mem_assoc "parallel_groups" fields);
      check bool "has chain" true (List.mem_assoc "chain" fields);
      (match List.assoc "success" fields with
       | `Bool b -> check bool "success false" false b
       | _ -> fail "expected bool");
      (match List.assoc "execution_order" fields with
       | `List l -> check int "3 in order" 3 (List.length l)
       | _ -> fail "expected list")
  | _ -> fail "expected Assoc"

let test_output_entry_to_json () =
  let entry : output_entry = {
    id = "node1";
    text = "output text";
    size = 11;
    truncated = false;
  } in
  let json = output_entry_to_json entry in
  match json with
  | `Assoc fields ->
      (match List.assoc "id" fields with
       | `String s -> check string "id" "node1" s
       | _ -> fail "expected string");
      (match List.assoc "truncated" fields with
       | `Bool b -> check bool "not truncated" false b
       | _ -> fail "expected bool")
  | _ -> fail "expected Assoc"

(** {1 Test Suite} *)

let truncate_tests = [
  test_case "short string" `Quick test_truncate_short;
  test_case "exact length" `Quick test_truncate_exact;
  test_case "long string" `Quick test_truncate_long;
  test_case "empty string" `Quick test_truncate_empty;
  test_case "zero limit" `Quick test_truncate_zero;
]

let internal_key_tests = [
  test_case "double underscore" `Quick test_internal_key_double_underscore;
  test_case "parent prefix" `Quick test_internal_key_parent;
  test_case "normal keys" `Quick test_internal_key_normal;
]

let collect_tests = [
  test_case "simple chain" `Quick test_collect_simple_chain;
  test_case "pipeline" `Quick test_collect_pipeline;
]

let json_tests = [
  test_case "node_view_to_json" `Quick test_node_view_to_json;
  test_case "run_summary_to_json" `Quick test_run_summary_to_json;
  test_case "run_record_to_json" `Quick test_run_record_to_json;
  test_case "output_entry_to_json" `Quick test_output_entry_to_json;
]

let () =
  run "chain_run_store" [
    ("truncate", truncate_tests);
    ("is_internal_key", internal_key_tests);
    ("collect_all_nodes", collect_tests);
    ("json_serialization", json_tests);
  ]

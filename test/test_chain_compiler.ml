(** Tests for Chain_compiler module

    Tests for:
    - topological_sort: DAG ordering
    - get_dependencies: extract node dependencies
    - is_ready: check if node can execute
    - compile: full chain compilation
*)

open Alcotest
open Chain_types
open Chain_compiler

(** {1 Test Helpers} *)

(** Create a simple LLM node *)
let llm_node ~id ?(deps=[]) ?(depends_on=[]) prompt =
  {
    id;
    node_type = Llm {
      model = "gemini";
      system = None;
      prompt;
      timeout = None;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = List.map (fun d -> (d, d)) deps;
    output_key = None;
    depends_on = (if depends_on = [] then None else Some depends_on);
  }

(** Create a simple chain from nodes *)
let make_chain ~id nodes output =
  {
    id;
    nodes;
    output;
    config = Chain_types.default_config;
    name = None;
    description = None;
    version = None;
    input_schema = None;
    output_schema = None;
    metadata = None;
  }

(** {1 topological_sort Tests} *)

let test_topological_simple () =
  (* A -> B -> C *)
  let deps = Hashtbl.create 3 in
  Hashtbl.add deps "A" [];
  Hashtbl.add deps "B" ["A"];
  Hashtbl.add deps "C" ["B"];
  match topological_sort deps with
  | Ok sorted ->
      check int "3 nodes" 3 (List.length sorted);
      (* A must come before B, B before C *)
      let idx_of x = List.mapi (fun i v -> (i, v)) sorted
        |> List.find (fun (_, v) -> v = x) |> fst in
      let idx_a = idx_of "A" in
      let idx_b = idx_of "B" in
      let idx_c = idx_of "C" in
      check bool "A before B" true (idx_a < idx_b);
      check bool "B before C" true (idx_b < idx_c)
  | Error e -> fail e

let test_topological_parallel () =
  (* A -> C, B -> C (A and B can be parallel) *)
  let deps = Hashtbl.create 3 in
  Hashtbl.add deps "A" [];
  Hashtbl.add deps "B" [];
  Hashtbl.add deps "C" ["A"; "B"];
  match topological_sort deps with
  | Ok sorted ->
      check int "3 nodes" 3 (List.length sorted);
      let idx_of x = List.mapi (fun i v -> (i, v)) sorted
        |> List.find (fun (_, v) -> v = x) |> fst in
      let idx_a = idx_of "A" in
      let idx_b = idx_of "B" in
      let idx_c = idx_of "C" in
      check bool "A before C" true (idx_a < idx_c);
      check bool "B before C" true (idx_b < idx_c)
  | Error e -> fail e

let test_topological_cycle () =
  (* A -> B -> A (cycle!) *)
  let deps = Hashtbl.create 2 in
  Hashtbl.add deps "A" ["B"];
  Hashtbl.add deps "B" ["A"];
  match topological_sort deps with
  | Ok _ -> fail "expected cycle detection"
  | Error e -> check bool "error contains cycle" true (String.length e > 0)

let test_topological_single () =
  let deps = Hashtbl.create 1 in
  Hashtbl.add deps "A" [];
  match topological_sort deps with
  | Ok sorted -> check (list string) "single node" ["A"] sorted
  | Error e -> fail e

(** {1 get_dependencies Tests} *)

let test_get_dependencies_none () =
  let node = llm_node ~id:"a" "test" in
  check (list string) "no deps" [] (get_dependencies node)

let test_get_dependencies_with_mapping () =
  let node = llm_node ~id:"b" ~deps:["a"; "x"] "test {{a}}" in
  let deps = get_dependencies node in
  check bool "has a" true (List.mem "a" deps);
  check bool "has x" true (List.mem "x" deps)

let test_get_dependencies_depends_on () =
  let node = llm_node ~id:"b" ~depends_on:["a"; "x"] "test" in
  let deps = get_dependencies node in
  check bool "has a via depends_on" true (List.mem "a" deps);
  check bool "has x via depends_on" true (List.mem "x" deps)

(** {1 is_ready Tests} *)

let test_is_ready_no_deps () =
  let node = llm_node ~id:"a" "test" in
  check bool "ready with no deps" true (is_ready [] node)

let test_is_ready_deps_met () =
  let node = llm_node ~id:"b" ~deps:["a"] "test" in
  check bool "ready when deps complete" true (is_ready ["a"] node)

let test_is_ready_deps_not_met () =
  let node = llm_node ~id:"c" ~deps:["a"; "b"] "test" in
  check bool "not ready missing b" false (is_ready ["a"] node)

(** {1 compile Tests} *)

let test_compile_simple_chain () =
  let nodes = [
    llm_node ~id:"a" "step1";
    llm_node ~id:"b" ~deps:["a"] "step2: {{a}}";
  ] in
  let chain = make_chain ~id:"test" nodes "b" in
  match compile chain with
  | Ok plan ->
      check int "2 nodes in order" 2 (List.length plan.execution_order);
      check bool "a first" true (List.hd plan.execution_order = "a")
  | Error e -> fail e

let test_compile_diamond () =
  (* Diamond: A -> B, A -> C, B -> D, C -> D *)
  let nodes = [
    llm_node ~id:"a" "start";
    llm_node ~id:"b" ~deps:["a"] "branch1";
    llm_node ~id:"c" ~deps:["a"] "branch2";
    llm_node ~id:"d" ~deps:["b"; "c"] "merge";
  ] in
  let chain = make_chain ~id:"diamond" nodes "d" in
  match compile chain with
  | Ok plan ->
      check int "4 nodes" 4 (List.length plan.execution_order);
      (* a must be first, d must be last *)
      check string "first is a" "a" (List.hd plan.execution_order);
      check string "last is d" "d" (List.hd (List.rev plan.execution_order))
  | Error e -> fail e

let test_compile_depends_on_chain () =
  let nodes = [
    llm_node ~id:"a" "step1";
    llm_node ~id:"b" ~depends_on:["a"] "step2";
  ] in
  let chain = make_chain ~id:"depends_on" nodes "b" in
  match compile chain with
  | Ok plan ->
      let idx_of x =
        List.mapi (fun i v -> (i, v)) plan.execution_order
        |> List.find (fun (_, v) -> v = x)
        |> fst
      in
      check bool "a before b via depends_on" true (idx_of "a" < idx_of "b")
  | Error e -> fail e

(** {1 pp_plan Tests} *)

let test_pp_plan () =
  let nodes = [llm_node ~id:"a" "test"] in
  let chain = make_chain ~id:"simple" nodes "a" in
  match compile chain with
  | Ok plan ->
      let s = pp_plan plan in
      check bool "contains order" true (Common.contains ~substring:"order" s);
      check bool "contains node a" true (Common.contains ~substring:"a" s)
  | Error e -> fail e

(** {1 Test Suite} *)

let topo_tests = [
  test_case "simple chain" `Quick test_topological_simple;
  test_case "parallel nodes" `Quick test_topological_parallel;
  test_case "cycle detection" `Quick test_topological_cycle;
  test_case "single node" `Quick test_topological_single;
]

let deps_tests = [
  test_case "no dependencies" `Quick test_get_dependencies_none;
  test_case "with mapping" `Quick test_get_dependencies_with_mapping;
  test_case "with depends_on" `Quick test_get_dependencies_depends_on;
]

let ready_tests = [
  test_case "no deps" `Quick test_is_ready_no_deps;
  test_case "deps met" `Quick test_is_ready_deps_met;
  test_case "deps not met" `Quick test_is_ready_deps_not_met;
]

let compile_tests = [
  test_case "simple chain" `Quick test_compile_simple_chain;
  test_case "diamond pattern" `Quick test_compile_diamond;
  test_case "depends_on chain" `Quick test_compile_depends_on_chain;
]

let pp_tests = [
  test_case "pp_plan output" `Quick test_pp_plan;
]

let () =
  run "chain_compiler" [
    ("topological_sort", topo_tests);
    ("get_dependencies", deps_tests);
    ("is_ready", ready_tests);
    ("compile", compile_tests);
    ("pp_plan", pp_tests);
  ]

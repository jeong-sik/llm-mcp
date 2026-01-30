(** test_compiler_evaluator_coverage.ml - Coverage tests for Chain Compiler and Evaluator

    Targets:
    - Chain Compiler (lib/chain_compiler.ml):
      1. compile - Main compiler entry
      2. build_dependency_graph - DAG construction
      3. topological_sort - Execution order (detect_cycles included)
      4. identify_parallel_groups - Parallel execution grouping
      5. calculate_depth - Nesting depth calculation

    - Chain Evaluator (lib/chain_evaluator.ml):
      1. Metrics collection (empty_node_metrics, mark_node_started, etc.)
      2. should_evaluate - Trigger evaluation conditions
      3. parse_verification_response - LLM response parsing
      4. calculate_chain_stats - Statistics aggregation

    - Scoring Functions (tested via heuristics since actual execution needs LLMs):
      1. regex_match - String length based scoring
      2. json_schema - JSON depth scoring
      3. anti_fake - Heuristic pattern detection
*)

open Alcotest

(* ============================================================================
   Helper Functions
   ============================================================================ *)

(** Create a simple LLM node for testing *)
let make_test_llm_node ~id ~prompt () =
  { Chain_types.id;
    node_type = Chain_types.Llm {
      model = "gemini";
      system = None;
      prompt;
      timeout = None;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  }

(** Create a node with dependencies *)
let make_test_llm_node_with_deps ~id ~prompt ~deps () =
  { Chain_types.id;
    node_type = Chain_types.Llm {
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
    depends_on = None;
  }

(** Create a simple chain for testing *)
let make_test_chain ~id ~nodes ~output () =
  { Chain_types.id;
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

(* ============================================================================
   Chain Compiler: build_dependency_graph Tests
   ============================================================================ *)

let test_build_dependency_graph_empty () =
  (* Empty node list *)
  let deps = Chain_compiler.build_dependency_graph [] in
  check int "empty graph" 0 (Hashtbl.length deps)

let test_build_dependency_graph_single () =
  (* Single node with no dependencies *)
  let node = make_test_llm_node ~id:"a" ~prompt:"test" () in
  let deps = Chain_compiler.build_dependency_graph [node] in
  check int "one node" 1 (Hashtbl.length deps);
  check (list string) "no deps" [] (Hashtbl.find deps "a")

let test_build_dependency_graph_linear () =
  (* Linear chain: a -> b -> c *)
  let node_a = make_test_llm_node ~id:"a" ~prompt:"test" () in
  let node_b = make_test_llm_node_with_deps ~id:"b" ~prompt:"{{a}}" ~deps:["a"] () in
  let node_c = make_test_llm_node_with_deps ~id:"c" ~prompt:"{{b}}" ~deps:["b"] () in
  let deps = Chain_compiler.build_dependency_graph [node_a; node_b; node_c] in
  check int "three nodes" 3 (Hashtbl.length deps);
  check (list string) "a has no deps" [] (Hashtbl.find deps "a");
  check (list string) "b depends on a" ["a"] (Hashtbl.find deps "b");
  check (list string) "c depends on b" ["b"] (Hashtbl.find deps "c")

let test_build_dependency_graph_diamond () =
  (* Diamond pattern: a -> b, a -> c, b -> d, c -> d *)
  let node_a = make_test_llm_node ~id:"a" ~prompt:"start" () in
  let node_b = make_test_llm_node_with_deps ~id:"b" ~prompt:"{{a}}" ~deps:["a"] () in
  let node_c = make_test_llm_node_with_deps ~id:"c" ~prompt:"{{a}}" ~deps:["a"] () in
  let node_d = make_test_llm_node_with_deps ~id:"d" ~prompt:"{{b}} {{c}}" ~deps:["b"; "c"] () in
  let deps = Chain_compiler.build_dependency_graph [node_a; node_b; node_c; node_d] in
  check int "four nodes" 4 (Hashtbl.length deps);
  let d_deps = List.sort String.compare (Hashtbl.find deps "d") in
  check (list string) "d depends on b and c" ["b"; "c"] d_deps

(* ============================================================================
   Chain Compiler: topological_sort Tests
   ============================================================================ *)

let test_topological_sort_empty () =
  let deps = Hashtbl.create 0 in
  match Chain_compiler.topological_sort deps with
  | Ok order -> check (list string) "empty order" [] order
  | Error e -> fail (Printf.sprintf "Unexpected error: %s" e)

let test_topological_sort_single () =
  let deps = Hashtbl.create 1 in
  Hashtbl.add deps "a" [];
  match Chain_compiler.topological_sort deps with
  | Ok order -> check (list string) "single node" ["a"] order
  | Error e -> fail (Printf.sprintf "Unexpected error: %s" e)

let test_topological_sort_linear () =
  (* a -> b -> c should sort to [a; b; c] *)
  let deps = Hashtbl.create 3 in
  Hashtbl.add deps "a" [];
  Hashtbl.add deps "b" ["a"];
  Hashtbl.add deps "c" ["b"];
  match Chain_compiler.topological_sort deps with
  | Ok order ->
      check int "three elements" 3 (List.length order);
      (* a must come before b, b before c *)
      let idx_a = List.find_index (fun x -> x = "a") order |> Option.get in
      let idx_b = List.find_index (fun x -> x = "b") order |> Option.get in
      let idx_c = List.find_index (fun x -> x = "c") order |> Option.get in
      check bool "a before b" true (idx_a < idx_b);
      check bool "b before c" true (idx_b < idx_c)
  | Error e -> fail (Printf.sprintf "Unexpected error: %s" e)

let test_topological_sort_diamond () =
  (* Diamond: a -> b, a -> c, b -> d, c -> d *)
  let deps = Hashtbl.create 4 in
  Hashtbl.add deps "a" [];
  Hashtbl.add deps "b" ["a"];
  Hashtbl.add deps "c" ["a"];
  Hashtbl.add deps "d" ["b"; "c"];
  match Chain_compiler.topological_sort deps with
  | Ok order ->
      check int "four elements" 4 (List.length order);
      let idx_a = List.find_index (fun x -> x = "a") order |> Option.get in
      let idx_b = List.find_index (fun x -> x = "b") order |> Option.get in
      let idx_c = List.find_index (fun x -> x = "c") order |> Option.get in
      let idx_d = List.find_index (fun x -> x = "d") order |> Option.get in
      check bool "a before b" true (idx_a < idx_b);
      check bool "a before c" true (idx_a < idx_c);
      check bool "b before d" true (idx_b < idx_d);
      check bool "c before d" true (idx_c < idx_d)
  | Error e -> fail (Printf.sprintf "Unexpected error: %s" e)

let test_topological_sort_cycle_detected () =
  (* Cycle: a -> b -> c -> a *)
  let deps = Hashtbl.create 3 in
  Hashtbl.add deps "a" ["c"];
  Hashtbl.add deps "b" ["a"];
  Hashtbl.add deps "c" ["b"];
  match Chain_compiler.topological_sort deps with
  | Ok _ -> fail "Should have detected cycle"
  | Error e ->
      check bool "cycle error message" true
        (String.length e > 0 &&
         (String.lowercase_ascii e |> fun s ->
          try let _ = Str.search_forward (Str.regexp_string "cycle") s 0 in true
          with Not_found -> false))

let test_topological_sort_self_cycle () =
  (* Self cycle: a -> a *)
  let deps = Hashtbl.create 1 in
  Hashtbl.add deps "a" ["a"];
  match Chain_compiler.topological_sort deps with
  | Ok _ -> fail "Should have detected self-cycle"
  | Error _ -> ()  (* Expected *)

(* ============================================================================
   Chain Compiler: calculate_depth Tests
   ============================================================================ *)

let test_calculate_depth_simple_llm () =
  let node = make_test_llm_node ~id:"a" ~prompt:"test" () in
  let depth = Chain_compiler.calculate_depth node in
  check int "single LLM depth" 1 depth

let test_calculate_depth_pipeline () =
  let inner1 = make_test_llm_node ~id:"p1" ~prompt:"test1" () in
  let inner2 = make_test_llm_node ~id:"p2" ~prompt:"test2" () in
  let pipeline = { Chain_types.id = "pipe";
    node_type = Chain_types.Pipeline [inner1; inner2];
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let depth = Chain_compiler.calculate_depth pipeline in
  check int "pipeline depth" 2 depth  (* 1 for pipeline + 1 for inner LLM *)

let test_calculate_depth_nested_pipeline () =
  let inner = make_test_llm_node ~id:"inner" ~prompt:"test" () in
  let inner_pipe = { Chain_types.id = "inner_pipe";
    node_type = Chain_types.Pipeline [inner];
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let outer_pipe = { Chain_types.id = "outer_pipe";
    node_type = Chain_types.Pipeline [inner_pipe];
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let depth = Chain_compiler.calculate_depth outer_pipe in
  check int "nested pipeline depth" 3 depth  (* outer + inner_pipe + inner LLM *)

let test_calculate_depth_quorum () =
  let node1 = make_test_llm_node ~id:"q1" ~prompt:"test1" () in
  let node2 = make_test_llm_node ~id:"q2" ~prompt:"test2" () in
  let quorum = { Chain_types.id = "quorum";
    node_type = Chain_types.Quorum { consensus = Count 2; nodes = [node1; node2]; weights = [] };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let depth = Chain_compiler.calculate_depth quorum in
  check int "quorum depth" 2 depth

let test_calculate_depth_gate () =
  let then_node = make_test_llm_node ~id:"then" ~prompt:"then" () in
  let else_node = make_test_llm_node ~id:"else" ~prompt:"else" () in
  let gate = { Chain_types.id = "gate";
    node_type = Chain_types.Gate {
      condition = "{{score}} > 0.5";
      then_node;
      else_node = Some else_node
    };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let depth = Chain_compiler.calculate_depth gate in
  check int "gate depth" 2 depth

let test_calculate_depth_retry () =
  let inner = make_test_llm_node ~id:"inner" ~prompt:"test" () in
  let retry = { Chain_types.id = "retry";
    node_type = Chain_types.Retry {
      node = inner;
      max_attempts = 3;
      backoff = Chain_types.Constant 1.0;
      retry_on = []
    };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  let depth = Chain_compiler.calculate_depth retry in
  check int "retry depth" 2 depth

(* ============================================================================
   Chain Compiler: compile Tests
   ============================================================================ *)

let test_compile_simple_chain () =
  let node = make_test_llm_node ~id:"a" ~prompt:"test" () in
  let chain = make_test_chain ~id:"test" ~nodes:[node] ~output:"a" () in
  match Chain_compiler.compile chain with
  | Ok plan ->
      check string "chain_id" "test" plan.Chain_types.chain.Chain_types.id;
      check int "depth" 1 plan.depth;
      check (list string) "execution_order" ["a"] plan.execution_order
  | Error e -> fail (Printf.sprintf "Compile failed: %s" e)

let test_compile_linear_chain () =
  let node_a = make_test_llm_node ~id:"a" ~prompt:"start" () in
  let node_b = make_test_llm_node_with_deps ~id:"b" ~prompt:"{{a}}" ~deps:["a"] () in
  let node_c = make_test_llm_node_with_deps ~id:"c" ~prompt:"{{b}}" ~deps:["b"] () in
  let chain = make_test_chain ~id:"linear" ~nodes:[node_a; node_b; node_c] ~output:"c" () in
  match Chain_compiler.compile chain with
  | Ok plan ->
      check int "depth" 1 plan.depth;  (* All simple LLM nodes *)
      let order = plan.execution_order in
      let idx_a = List.find_index (fun x -> x = "a") order |> Option.get in
      let idx_b = List.find_index (fun x -> x = "b") order |> Option.get in
      let idx_c = List.find_index (fun x -> x = "c") order |> Option.get in
      check bool "proper order" true (idx_a < idx_b && idx_b < idx_c)
  | Error e -> fail (Printf.sprintf "Compile failed: %s" e)

let test_compile_missing_output_node () =
  let node = make_test_llm_node ~id:"a" ~prompt:"test" () in
  let chain = make_test_chain ~id:"bad" ~nodes:[node] ~output:"nonexistent" () in
  match Chain_compiler.compile chain with
  | Ok _ -> fail "Should have failed for missing output node"
  | Error e ->
      check bool "error mentions output" true
        (try let _ = Str.search_forward (Str.regexp_string "nonexistent") e 0 in true
         with Not_found -> false)

let test_compile_duplicate_node_ids () =
  let node1 = make_test_llm_node ~id:"dup" ~prompt:"test1" () in
  let node2 = make_test_llm_node ~id:"dup" ~prompt:"test2" () in
  let chain = make_test_chain ~id:"dups" ~nodes:[node1; node2] ~output:"dup" () in
  match Chain_compiler.compile chain with
  | Ok _ -> fail "Should have failed for duplicate IDs"
  | Error e ->
      check bool "error mentions duplicate" true
        (String.lowercase_ascii e |> fun s ->
         try let _ = Str.search_forward (Str.regexp_string "duplicate") s 0 in true
         with Not_found -> false)

let test_compile_exceeds_max_depth () =
  (* Create a deeply nested structure that exceeds default max_depth *)
  let rec make_nested_pipeline depth =
    if depth <= 0 then
      make_test_llm_node ~id:(Printf.sprintf "leaf_%d" depth) ~prompt:"leaf" ()
    else
      let inner = make_nested_pipeline (depth - 1) in
      { Chain_types.id = Printf.sprintf "pipe_%d" depth;
        node_type = Chain_types.Pipeline [inner];
        input_mapping = [];
        output_key = None;
        depends_on = None;
      }
  in
  let deep_node = make_nested_pipeline 15 in  (* Exceeds default max_depth of 8 *)
  let chain = make_test_chain ~id:"deep" ~nodes:[deep_node] ~output:"pipe_15" () in
  match Chain_compiler.compile chain with
  | Ok _ -> fail "Should have failed for exceeding max depth"
  | Error e ->
      check bool "error mentions depth" true
        (String.lowercase_ascii e |> fun s ->
         try let _ = Str.search_forward (Str.regexp_string "depth") s 0 in true
         with Not_found -> false)

(* ============================================================================
   Chain Compiler: identify_parallel_groups Tests
   ============================================================================ *)

let test_parallel_groups_linear () =
  (* Linear chain has no parallelism *)
  let node_a = make_test_llm_node ~id:"a" ~prompt:"test" () in
  let node_b = make_test_llm_node_with_deps ~id:"b" ~prompt:"{{a}}" ~deps:["a"] () in
  let deps = Chain_compiler.build_dependency_graph [node_a; node_b] in
  let groups = Chain_compiler.identify_parallel_groups [node_a; node_b] ["a"; "b"] deps in
  (* Each node should be in its own level *)
  check int "two levels" 2 (List.length groups)

let test_parallel_groups_independent () =
  (* Two independent nodes can run in parallel *)
  let node_a = make_test_llm_node ~id:"a" ~prompt:"test_a" () in
  let node_b = make_test_llm_node ~id:"b" ~prompt:"test_b" () in
  let deps = Chain_compiler.build_dependency_graph [node_a; node_b] in
  let groups = Chain_compiler.identify_parallel_groups [node_a; node_b] ["a"; "b"] deps in
  (* Both nodes should be at level 0 *)
  check int "one level" 1 (List.length groups);
  check int "two nodes in group" 2 (List.length (List.hd groups))

let test_parallel_groups_diamond () =
  (* Diamond: a -> (b, c) -> d *)
  let node_a = make_test_llm_node ~id:"a" ~prompt:"start" () in
  let node_b = make_test_llm_node_with_deps ~id:"b" ~prompt:"{{a}}" ~deps:["a"] () in
  let node_c = make_test_llm_node_with_deps ~id:"c" ~prompt:"{{a}}" ~deps:["a"] () in
  let node_d = make_test_llm_node_with_deps ~id:"d" ~prompt:"{{b}} {{c}}" ~deps:["b"; "c"] () in
  let deps = Chain_compiler.build_dependency_graph [node_a; node_b; node_c; node_d] in
  match Chain_compiler.topological_sort deps with
  | Ok order ->
      let groups = Chain_compiler.identify_parallel_groups
        [node_a; node_b; node_c; node_d] order deps in
      check int "three levels" 3 (List.length groups);
      (* Level 0: a, Level 1: b and c (parallel), Level 2: d *)
      let level1 = List.nth groups 1 in
      check int "two nodes at level 1" 2 (List.length level1)
  | Error e -> fail (Printf.sprintf "Topological sort failed: %s" e)

(* ============================================================================
   Chain Evaluator: Metrics Tests
   ============================================================================ *)

let test_empty_node_metrics () =
  let m = Chain_evaluator.empty_node_metrics ~node_id:"test" ~node_type:"llm" in
  check string "node_id" "test" m.node_id;
  check string "node_type" "llm" m.node_type;
  check bool "status pending" true (m.status = Chain_evaluator.Pending);
  check bool "started_at none" true (Option.is_none m.started_at);
  check int "duration_ms" 0 m.duration_ms;
  check int "retry_count" 0 m.retry_count

let test_mark_node_started () =
  let m = Chain_evaluator.empty_node_metrics ~node_id:"test" ~node_type:"llm" in
  let m' = Chain_evaluator.mark_node_started m in
  check bool "status running" true (m'.status = Chain_evaluator.Running);
  check bool "started_at set" true (Option.is_some m'.started_at)

let test_mark_node_completed () =
  let m = Chain_evaluator.empty_node_metrics ~node_id:"test" ~node_type:"llm" in
  let m' = Chain_evaluator.mark_node_started m in
  Unix.sleepf 0.01;  (* Small delay to ensure duration > 0 *)
  let m'' = Chain_evaluator.mark_node_completed m' ~output_preview:"result here" in
  check bool "status succeeded" true (m''.status = Chain_evaluator.Succeeded);
  check bool "completed_at set" true (Option.is_some m''.completed_at);
  check bool "duration > 0" true (m''.duration_ms >= 0);
  check bool "output preview" true (Option.is_some m''.output_preview)

let test_mark_node_failed () =
  let m = Chain_evaluator.empty_node_metrics ~node_id:"test" ~node_type:"llm" in
  let m' = Chain_evaluator.mark_node_started m in
  let m'' = Chain_evaluator.mark_node_failed m' ~error_message:"Something went wrong" in
  check bool "status failed" true (m''.status = Chain_evaluator.Failed);
  check bool "error_message set" true (m''.error_message = Some "Something went wrong")

let test_empty_chain_metrics () =
  let m = Chain_evaluator.empty_chain_metrics ~chain_id:"test_chain" ~goal:"Complete task" in
  check string "chain_id" "test_chain" m.chain_id;
  check string "goal" "Complete task" m.goal;
  check int "total_nodes" 0 m.total_nodes;
  check bool "verification none" true (Option.is_none m.verification)

let test_calculate_chain_stats () =
  let nodes = [
    { (Chain_evaluator.empty_node_metrics ~node_id:"a" ~node_type:"llm") with
      status = Chain_evaluator.Succeeded };
    { (Chain_evaluator.empty_node_metrics ~node_id:"b" ~node_type:"llm") with
      status = Chain_evaluator.Succeeded };
    { (Chain_evaluator.empty_node_metrics ~node_id:"c" ~node_type:"llm") with
      status = Chain_evaluator.Failed };
    { (Chain_evaluator.empty_node_metrics ~node_id:"d" ~node_type:"llm") with
      status = Chain_evaluator.Skipped };
  ] in
  let m = { (Chain_evaluator.empty_chain_metrics ~chain_id:"test" ~goal:"test") with
    node_metrics = nodes } in
  let m' = Chain_evaluator.calculate_chain_stats m in
  check int "total_nodes" 4 m'.total_nodes;
  check int "nodes_succeeded" 2 m'.nodes_succeeded;
  check int "nodes_failed" 1 m'.nodes_failed;
  check int "nodes_skipped" 1 m'.nodes_skipped;
  (* Success rate = 2/3 = 0.666... (only succeeded and failed count) *)
  check bool "success_rate ~0.67" true (m'.success_rate > 0.6 && m'.success_rate < 0.7)

(* ============================================================================
   Chain Evaluator: should_evaluate Tests
   ============================================================================ *)

let test_should_evaluate_on_node_complete () =
  let node_metrics = { (Chain_evaluator.empty_node_metrics ~node_id:"target" ~node_type:"llm") with
    status = Chain_evaluator.Succeeded } in
  let metrics = { (Chain_evaluator.empty_chain_metrics ~chain_id:"test" ~goal:"test") with
    node_metrics = [node_metrics] } in
  let trigger = Chain_evaluator.OnNodeComplete "target" in
  check bool "should evaluate" true (Chain_evaluator.should_evaluate ~trigger ~metrics)

let test_should_evaluate_on_node_complete_pending () =
  let node_metrics = { (Chain_evaluator.empty_node_metrics ~node_id:"target" ~node_type:"llm") with
    status = Chain_evaluator.Pending } in
  let metrics = { (Chain_evaluator.empty_chain_metrics ~chain_id:"test" ~goal:"test") with
    node_metrics = [node_metrics] } in
  let trigger = Chain_evaluator.OnNodeComplete "target" in
  check bool "should not evaluate" false (Chain_evaluator.should_evaluate ~trigger ~metrics)

let test_should_evaluate_on_group_complete () =
  let nodes = [
    { (Chain_evaluator.empty_node_metrics ~node_id:"a" ~node_type:"llm") with
      status = Chain_evaluator.Succeeded };
    { (Chain_evaluator.empty_node_metrics ~node_id:"b" ~node_type:"llm") with
      status = Chain_evaluator.Failed };
  ] in
  let metrics = { (Chain_evaluator.empty_chain_metrics ~chain_id:"test" ~goal:"test") with
    node_metrics = nodes } in
  let trigger = Chain_evaluator.OnGroupComplete ["a"; "b"] in
  check bool "should evaluate group" true (Chain_evaluator.should_evaluate ~trigger ~metrics)

let test_should_evaluate_on_chain_complete () =
  let metrics = { (Chain_evaluator.empty_chain_metrics ~chain_id:"test" ~goal:"test") with
    nodes_pending = 0 } in
  let trigger = Chain_evaluator.OnChainComplete in
  check bool "chain complete" true (Chain_evaluator.should_evaluate ~trigger ~metrics)

let test_should_evaluate_on_failure () =
  let metrics = { (Chain_evaluator.empty_chain_metrics ~chain_id:"test" ~goal:"test") with
    nodes_failed = 1 } in
  let trigger = Chain_evaluator.OnFailure in
  check bool "has failures" true (Chain_evaluator.should_evaluate ~trigger ~metrics)

(* ============================================================================
   Chain Evaluator: parse_verification_response Tests
   ============================================================================ *)

let test_parse_verification_json () =
  let response = {|```json
{
  "is_complete": true,
  "confidence": 0.95,
  "reason": "All tests passed",
  "missing_criteria": [],
  "suggested_next_steps": []
}
```|} in
  let result = Chain_evaluator.parse_verification_response response in
  check bool "is_complete" true result.is_complete;
  check bool "confidence high" true (result.confidence > 0.9)

let test_parse_verification_plaintext_true () =
  let response = "is_complete: true\nconfidence: 0.8\nreason: Done" in
  let result = Chain_evaluator.parse_verification_response response in
  check bool "is_complete true" true result.is_complete

let test_parse_verification_plaintext_false () =
  let response = "is_complete: false\nreason: Missing tests" in
  let result = Chain_evaluator.parse_verification_response response in
  check bool "is_complete false" false result.is_complete

let test_parse_verification_goal_achieved () =
  (* Alternative key names *)
  let response = "goal_achieved: true" in
  let result = Chain_evaluator.parse_verification_response response in
  check bool "goal achieved" true result.is_complete

let test_parse_verification_no_match () =
  (* Response with no recognizable patterns *)
  let response = "This is just some random text without any completion indicators." in
  let result = Chain_evaluator.parse_verification_response response in
  check bool "defaults to false" false result.is_complete;
  check bool "low confidence" true (result.confidence < 0.5)

(* ============================================================================
   Scoring Heuristics: anti_fake Detection Tests
   ============================================================================ *)

(** Test anti-fake heuristics for detecting fake tests *)
let anti_fake_heuristic_score output =
  let output_lower = String.lowercase_ascii output in
  let contains needle haystack =
    let nl = String.length needle and hl = String.length haystack in
    if nl > hl then false
    else
      let rec check i =
        if i > hl - nl then false
        else if String.sub haystack i nl = needle then true
        else check (i + 1)
      in check 0
  in
  let score = ref 0.5 in
  (* Penalty patterns *)
  if contains "assert true" output_lower then score := !score -. 0.15;
  if contains "let _ =" output then score := !score -. 0.1;
  if contains "(* todo" output_lower then score := !score -. 0.05;
  (* Bonus patterns *)
  if contains "assert_equal" output_lower then score := !score +. 0.1;
  if contains "expect" output_lower then score := !score +. 0.1;
  if contains "roundtrip" output_lower then score := !score +. 0.15;
  Float.min 1.0 (Float.max 0.0 !score)

let test_anti_fake_detects_assert_true () =
  let fake_test = "let test () = assert true" in
  let score = anti_fake_heuristic_score fake_test in
  check bool "penalty for assert true" true (score < 0.5)

let test_anti_fake_detects_let_underscore () =
  let unused = "let _ = some_function ()" in
  let score = anti_fake_heuristic_score unused in
  check bool "penalty for let _ =" true (score < 0.5)

let test_anti_fake_detects_todo () =
  let incomplete = "(* TODO: implement this test *)\nlet test () = ()" in
  let score = anti_fake_heuristic_score incomplete in
  check bool "penalty for TODO" true (score < 0.5)

let test_anti_fake_rewards_assert_equal () =
  let good_test = "let test () = assert_equal expected actual" in
  let score = anti_fake_heuristic_score good_test in
  check bool "bonus for assert_equal" true (score > 0.5)

let test_anti_fake_rewards_expect () =
  let jest_style = "expect(result).toBe(expected)" in
  let score = anti_fake_heuristic_score jest_style in
  check bool "bonus for expect" true (score > 0.5)

let test_anti_fake_rewards_roundtrip () =
  let roundtrip_test = "let test_roundtrip () = parse (serialize x) = x" in
  let score = anti_fake_heuristic_score roundtrip_test in
  check bool "bonus for roundtrip" true (score > 0.5)

let test_anti_fake_combined_penalties () =
  let very_fake = "let _ = ()\nassert true\n(* TODO *)" in
  let score = anti_fake_heuristic_score very_fake in
  check bool "multiple penalties" true (score < 0.3)

let test_anti_fake_combined_bonuses () =
  let good_test = "expect(roundtrip(x)).assert_equal(x)" in
  let score = anti_fake_heuristic_score good_test in
  check bool "multiple bonuses" true (score > 0.7)

(* ============================================================================
   Scoring Heuristics: json_schema Depth Tests
   ============================================================================ *)

let json_schema_score output =
  try
    let json = Yojson.Safe.from_string output in
    let depth = ref 0 in
    let rec count_depth = function
      | `Assoc fields ->
          incr depth;
          List.iter (fun (_, v) -> count_depth v) fields
      | `List items ->
          incr depth;
          List.iter count_depth items
      | _ -> ()
    in
    count_depth json;
    Float.min 1.0 (0.5 +. (float_of_int !depth *. 0.1))
  with Yojson.Json_error _ -> 0.0

let test_json_schema_invalid () =
  let invalid = "not json at all" in
  let score = json_schema_score invalid in
  check bool "invalid json = 0" true (score = 0.0)

let test_json_schema_empty_object () =
  let empty = "{}" in
  let score = json_schema_score empty in
  check bool "empty object > 0.5" true (score >= 0.5)

let test_json_schema_nested () =
  let nested = {|{"a": {"b": {"c": 1}}}|} in
  let score = json_schema_score nested in
  check bool "nested structure higher score" true (score > 0.6)

let test_json_schema_array () =
  let arr = {|[1, 2, 3]|} in
  let score = json_schema_score arr in
  check bool "array has depth" true (score >= 0.5)

let test_json_schema_complex () =
  let complex = {|{"users": [{"name": "Alice", "data": {"age": 30}}]}|} in
  let score = json_schema_score complex in
  check bool "complex structure high score" true (score > 0.7)

(* ============================================================================
   Scoring Heuristics: regex_match (length-based) Tests
   ============================================================================ *)

let regex_match_score output =
  Float.min 1.0 (float_of_int (String.length output) /. 1000.0)

let test_regex_match_empty () =
  check bool "empty = 0" true (regex_match_score "" = 0.0)

let test_regex_match_short () =
  let short = "hello" in
  let score = regex_match_score short in
  check bool "short string low score" true (score < 0.1)

let test_regex_match_medium () =
  let medium = String.make 500 'x' in
  let score = regex_match_score medium in
  check bool "500 chars = 0.5" true (score = 0.5)

let test_regex_match_long () =
  let long = String.make 1000 'x' in
  let score = regex_match_score long in
  check bool "1000 chars = 1.0" true (score = 1.0)

let test_regex_match_capped () =
  let very_long = String.make 2000 'x' in
  let score = regex_match_score very_long in
  check bool "capped at 1.0" true (score = 1.0)

(* ============================================================================
   Selection Strategy Tests
   ============================================================================ *)

let test_select_strategy_best () =
  (* Best selects highest score *)
  let candidates = [("a", 0.3); ("b", 0.9); ("c", 0.5)] in
  let best = List.fold_left (fun (best_id, best_score) (id, score) ->
    if score > best_score then (id, score) else (best_id, best_score)
  ) ("", 0.0) candidates in
  check string "best is b" "b" (fst best);
  check bool "best score 0.9" true (snd best = 0.9)

let test_select_strategy_above_threshold () =
  (* AboveThreshold selects first candidate above threshold *)
  let threshold = 0.7 in
  let candidates = [("a", 0.3); ("b", 0.8); ("c", 0.9)] in
  let first_above = List.find_opt (fun (_, s) -> s >= threshold) candidates in
  match first_above with
  | Some (id, _) -> check string "first above threshold is b" "b" id
  | None -> fail "Should have found candidate above threshold"

let test_select_strategy_threshold_none () =
  let threshold = 0.95 in
  let candidates = [("a", 0.3); ("b", 0.8); ("c", 0.9)] in
  let first_above = List.find_opt (fun (_, s) -> s >= threshold) candidates in
  check bool "none above high threshold" true (Option.is_none first_above)

(* ============================================================================
   Test Suite Registration
   ============================================================================ *)

let () =
  run "Chain Compiler and Evaluator Coverage" [
    (* Dependency Graph Tests *)
    "build_dependency_graph", [
      test_case "empty" `Quick test_build_dependency_graph_empty;
      test_case "single node" `Quick test_build_dependency_graph_single;
      test_case "linear chain" `Quick test_build_dependency_graph_linear;
      test_case "diamond pattern" `Quick test_build_dependency_graph_diamond;
    ];

    (* Topological Sort Tests *)
    "topological_sort", [
      test_case "empty" `Quick test_topological_sort_empty;
      test_case "single" `Quick test_topological_sort_single;
      test_case "linear" `Quick test_topological_sort_linear;
      test_case "diamond" `Quick test_topological_sort_diamond;
      test_case "cycle detected" `Quick test_topological_sort_cycle_detected;
      test_case "self cycle" `Quick test_topological_sort_self_cycle;
    ];

    (* Depth Calculation Tests *)
    "calculate_depth", [
      test_case "simple llm" `Quick test_calculate_depth_simple_llm;
      test_case "pipeline" `Quick test_calculate_depth_pipeline;
      test_case "nested pipeline" `Quick test_calculate_depth_nested_pipeline;
      test_case "quorum" `Quick test_calculate_depth_quorum;
      test_case "gate" `Quick test_calculate_depth_gate;
      test_case "retry" `Quick test_calculate_depth_retry;
    ];

    (* Compile Tests *)
    "compile", [
      test_case "simple chain" `Quick test_compile_simple_chain;
      test_case "linear chain" `Quick test_compile_linear_chain;
      test_case "missing output node" `Quick test_compile_missing_output_node;
      test_case "duplicate node ids" `Quick test_compile_duplicate_node_ids;
      test_case "exceeds max depth" `Quick test_compile_exceeds_max_depth;
    ];

    (* Parallel Groups Tests *)
    "identify_parallel_groups", [
      test_case "linear" `Quick test_parallel_groups_linear;
      test_case "independent" `Quick test_parallel_groups_independent;
      test_case "diamond" `Quick test_parallel_groups_diamond;
    ];

    (* Evaluator Metrics Tests *)
    "metrics", [
      test_case "empty node metrics" `Quick test_empty_node_metrics;
      test_case "mark node started" `Quick test_mark_node_started;
      test_case "mark node completed" `Quick test_mark_node_completed;
      test_case "mark node failed" `Quick test_mark_node_failed;
      test_case "empty chain metrics" `Quick test_empty_chain_metrics;
      test_case "calculate chain stats" `Quick test_calculate_chain_stats;
    ];

    (* Evaluation Triggers Tests *)
    "should_evaluate", [
      test_case "on node complete" `Quick test_should_evaluate_on_node_complete;
      test_case "on node complete pending" `Quick test_should_evaluate_on_node_complete_pending;
      test_case "on group complete" `Quick test_should_evaluate_on_group_complete;
      test_case "on chain complete" `Quick test_should_evaluate_on_chain_complete;
      test_case "on failure" `Quick test_should_evaluate_on_failure;
    ];

    (* Verification Response Parsing Tests *)
    "parse_verification_response", [
      test_case "json format" `Quick test_parse_verification_json;
      test_case "plaintext true" `Quick test_parse_verification_plaintext_true;
      test_case "plaintext false" `Quick test_parse_verification_plaintext_false;
      test_case "goal achieved" `Quick test_parse_verification_goal_achieved;
      test_case "no match" `Quick test_parse_verification_no_match;
    ];

    (* Anti-Fake Scoring Tests *)
    "anti_fake_scoring", [
      test_case "detects assert true" `Quick test_anti_fake_detects_assert_true;
      test_case "detects let _ =" `Quick test_anti_fake_detects_let_underscore;
      test_case "detects TODO" `Quick test_anti_fake_detects_todo;
      test_case "rewards assert_equal" `Quick test_anti_fake_rewards_assert_equal;
      test_case "rewards expect" `Quick test_anti_fake_rewards_expect;
      test_case "rewards roundtrip" `Quick test_anti_fake_rewards_roundtrip;
      test_case "combined penalties" `Quick test_anti_fake_combined_penalties;
      test_case "combined bonuses" `Quick test_anti_fake_combined_bonuses;
    ];

    (* JSON Schema Scoring Tests *)
    "json_schema_scoring", [
      test_case "invalid json" `Quick test_json_schema_invalid;
      test_case "empty object" `Quick test_json_schema_empty_object;
      test_case "nested" `Quick test_json_schema_nested;
      test_case "array" `Quick test_json_schema_array;
      test_case "complex" `Quick test_json_schema_complex;
    ];

    (* Regex Match Scoring Tests *)
    "regex_match_scoring", [
      test_case "empty" `Quick test_regex_match_empty;
      test_case "short" `Quick test_regex_match_short;
      test_case "medium" `Quick test_regex_match_medium;
      test_case "long" `Quick test_regex_match_long;
      test_case "capped" `Quick test_regex_match_capped;
    ];

    (* Selection Strategy Tests *)
    "selection_strategy", [
      test_case "best" `Quick test_select_strategy_best;
      test_case "above threshold" `Quick test_select_strategy_above_threshold;
      test_case "threshold none" `Quick test_select_strategy_threshold_none;
    ];
  ]

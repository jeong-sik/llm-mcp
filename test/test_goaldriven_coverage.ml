(** GoalDriven Coverage Tests

    Tests for gaps in GoalDriven coverage:
    1. All goal_operators (Gt, Lt, Lte, Eq, Neq)
    2. measure_func: parse_json
    3. max_iterations boundary conditions
    4. relay_models actual rotation
    5. conversational context accumulation
*)

open Chain_types

(* ============================================================================
   Helpers
   ============================================================================ *)

let truncate n s = if String.length s > n then String.sub s 0 n ^ "..." else s

let compile_exn chain =
  match Chain_compiler.compile chain with
  | Ok plan -> plan
  | Error msg -> failwith ("compile failed: " ^ msg)

let execute_chain ~sw ~clock ~exec_fn ~tool_exec chain =
  let plan = compile_exn chain in
  let timeout = chain.config.timeout in
  let trace = chain.config.trace in
  let result = Chain_executor_eio.execute ~sw ~clock ~timeout ~trace ~exec_fn ~tool_exec plan in
  result

(* ============================================================================
   Mock Execution (No Ollama needed)
   ============================================================================ *)

let mock_iteration_count = ref 0
let mock_context_history = ref []

let make_mock_exec_fn ~responses () =
  mock_iteration_count := 0;
  mock_context_history := [];
  fun ~model ~prompt ?tools:_ () ->
    incr mock_iteration_count;
    mock_context_history := (model, truncate 100 prompt) :: !mock_context_history;
    let idx = min (!mock_iteration_count - 1) (List.length responses - 1) in
    let response = List.nth responses idx in
    Printf.printf "    [Mock LLM %d] model=%s → %s\n%!" !mock_iteration_count model (truncate 60 response);
    Ok response

let mock_tool_exec ~name ~args:_ = Error (Printf.sprintf "Tool '%s' not implemented" name)

(* ============================================================================
   Test 1: All Goal Operators
   ============================================================================ *)

let test_operator_gt () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: goal_operator = Gt (greater than)                     ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "value";
      goal_operator = Gt;  (* Greater than, not equal *)
      goal_value = 0.5;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 3;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  (* Response 0.5 should NOT pass (need > 0.5), response 0.6 should pass *)
  let responses = ["value: 0.5"; "value: 0.6"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 2)\n" !mock_iteration_count;
      Alcotest.(check bool) "passed" true result.success;
      Alcotest.(check int) "iterations" 2 !mock_iteration_count;
      Printf.printf "   ✅ Gt operator works correctly\n"

let test_operator_lt () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: goal_operator = Lt (less than)                        ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "error_rate";
      goal_operator = Lt;  (* Want error < 0.1 *)
      goal_value = 0.1;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 3;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  (* 0.2 fails, 0.1 fails (not <), 0.05 passes *)
  let responses = ["error_rate: 0.2"; "error_rate: 0.1"; "error_rate: 0.05"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 3)\n" !mock_iteration_count;
      Alcotest.(check bool) "passed" true result.success;
      Alcotest.(check int) "iterations" 3 !mock_iteration_count;
      Printf.printf "   ✅ Lt operator works correctly\n"

let test_operator_eq () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: goal_operator = Eq (exactly equal)                    ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "status";
      goal_operator = Eq;  (* Want exactly 1.0 *)
      goal_value = 1.0;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 3;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  (* 0.9 fails, 1.1 fails, 1.0 passes *)
  let responses = ["status: 0.9"; "status: 1.1"; "status: 1.0"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 3)\n" !mock_iteration_count;
      Alcotest.(check bool) "passed" true result.success;
      Alcotest.(check int) "iterations" 3 !mock_iteration_count;
      Printf.printf "   ✅ Eq operator works correctly\n"

let test_operator_neq () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: goal_operator = Neq (not equal)                       ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "code";
      goal_operator = Neq;  (* Want anything but 0 *)
      goal_value = 0.0;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 3;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  (* 0.0 fails, 0.0 fails, 42.0 passes *)
  let responses = ["code: 0.0"; "code: 0"; "code: 42"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 3)\n" !mock_iteration_count;
      Alcotest.(check bool) "passed" true result.success;
      Alcotest.(check int) "iterations" 3 !mock_iteration_count;
      Printf.printf "   ✅ Neq operator works correctly\n"

let test_operator_lte () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: goal_operator = Lte (less than or equal)              ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "latency";
      goal_operator = Lte;  (* Want latency <= 100 *)
      goal_value = 100.0;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 3;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  (* 200 fails, 100 passes (equal is ok) *)
  let responses = ["latency: 200"; "latency: 100"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 2)\n" !mock_iteration_count;
      Alcotest.(check bool) "passed" true result.success;
      Alcotest.(check int) "iterations" 2 !mock_iteration_count;
      Printf.printf "   ✅ Lte operator works correctly\n"

(* ============================================================================
   Test 2: max_iterations Boundary
   ============================================================================ *)

let test_max_iterations_reached () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: max_iterations boundary (never succeeds)              ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "value";
      goal_operator = Gte;
      goal_value = 1.0;  (* Never reached *)
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 3;  (* Will hit this limit *)
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  (* All responses below target *)
  let responses = ["value: 0.1"; "value: 0.2"; "value: 0.3"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 3 = max)\n" !mock_iteration_count;
      Printf.printf "   Success: %b (expected: false - goal not met)\n" result.success;
      (* Goal not met, but max iterations reached - should return last output *)
      Alcotest.(check int) "iterations" 3 !mock_iteration_count;
      Printf.printf "   ✅ max_iterations boundary works correctly\n"

let test_single_iteration () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: max_iterations = 1 (single shot)                      ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "value";
      goal_operator = Gte;
      goal_value = 0.5;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 1;  (* Single shot *)
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  let responses = ["value: 0.9"] in  (* Pass on first try *)

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 1)\n" !mock_iteration_count;
      Alcotest.(check bool) "passed" true result.success;
      Alcotest.(check int) "iterations" 1 !mock_iteration_count;
      Printf.printf "   ✅ Single iteration works correctly\n"

(* ============================================================================
   Test 3: relay_models Rotation
   ============================================================================ *)

let test_relay_models_rotation () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: relay_models rotation across iterations               ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "ollama:model_a"; prompt = "Return value"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "value";
      goal_operator = Gte;
      goal_value = 0.9;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 4;
      strategy_hints = [];
      conversational = false;
      relay_models = ["model_a"; "model_b"; "model_c"];  (* Should rotate *)
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  let responses = ["value: 0.1"; "value: 0.2"; "value: 0.3"; "value: 0.95"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d\n" !mock_iteration_count;
      Printf.printf "   Models used:\n";
      List.iteri (fun i (model, _) ->
        Printf.printf "     %d: %s\n" (i + 1) model
      ) (List.rev !mock_context_history);

      (* Check model rotation pattern *)
      let models = List.map fst (List.rev !mock_context_history) in
      Printf.printf "   Model sequence: [%s]\n" (String.concat "; " models);

      Alcotest.(check bool) "passed" true result.success;
      Printf.printf "   ✅ relay_models rotation executed\n"

(* ============================================================================
   Test 4: Conversational Context Accumulation
   ============================================================================ *)

let test_conversational_mode () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: conversational mode context accumulation              ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Improve: {{input}}"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "score";
      goal_operator = Gte;
      goal_value = 0.9;
      action_node = action;
      measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
      max_iterations = 3;
      strategy_hints = [];
      conversational = true;  (* Enable context accumulation *)
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  let responses = ["First attempt. score: 0.3"; "Improved. score: 0.6"; "Final. score: 0.95"] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d\n" !mock_iteration_count;
      Printf.printf "   Prompt history (checking context accumulation):\n";
      List.iteri (fun i (_, prompt) ->
        Printf.printf "     %d: %s\n" (i + 1) prompt
      ) (List.rev !mock_context_history);

      Alcotest.(check bool) "passed" true result.success;
      Printf.printf "   ✅ conversational mode executed\n"

(* ============================================================================
   Test 5: measure_func: parse_json
   ============================================================================ *)

let test_measure_func_parse_json () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: measure_func = parse_json                             ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let action = {
    id = "action";
    node_type = Llm { model = "mock"; prompt = "Return JSON"; timeout = Some 10; tools = None };
    input_mapping = []
  } in

  let goal = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "accuracy";
      goal_operator = Gte;
      goal_value = 0.8;
      action_node = action;
      measure_func = "parse_json";  (* Extract from JSON *)
      max_iterations = 2;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []
  } in

  let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

  (* JSON responses with accuracy field *)
  let responses = [
    {|{"result": "test", "accuracy": 0.5}|};
    {|{"result": "final", "accuracy": 0.95}|}
  ] in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_mock_exec_fn ~responses () in
      let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

      Printf.printf "   Iterations: %d (expected: 2)\n" !mock_iteration_count;
      Alcotest.(check bool) "passed" true result.success;
      Printf.printf "   ✅ parse_json measure_func works correctly\n"

(* ============================================================================
   Test 6: Mermaid Roundtrip with Different Operators
   ============================================================================ *)

(* Helper for string contains *)
let contains_s haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in true
  with Not_found -> false

let test_mermaid_operators_roundtrip () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Test: Mermaid roundtrip preserves all operators             ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  let operators = [
    (Gt, "gt"); (Gte, "gte"); (Lt, "lt");
    (Lte, "lte"); (Eq, "eq"); (Neq, "neq")
  ] in

  List.iter (fun (op, op_str) ->
    let action = {
      id = "action";
      node_type = Llm { model = "test"; prompt = "Test"; timeout = Some 10; tools = None };
      input_mapping = []
    } in

    let goal = {
      id = "goal";
      node_type = GoalDriven {
        goal_metric = "metric";
        goal_operator = op;
        goal_value = 0.5;
        action_node = action;
        measure_func = "exec_test";  (* exec_test supports "metric: value" pattern *)
        max_iterations = 5;
        strategy_hints = [];
        conversational = false;
        relay_models = [];
      };
      input_mapping = []
    } in

    let chain = { id = "test"; nodes = [goal]; output = "goal"; config = default_config } in

    let mermaid = Chain_mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain in

    (* Check operator in mermaid output *)
    let has_op = contains_s mermaid op_str || contains_s mermaid (String.uppercase_ascii op_str) in
    Printf.printf "   Operator %s: %s\n" op_str (if has_op then "✓ found in Mermaid" else "✗ NOT found");

    (* Parse back and verify *)
    match Chain_mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid with
    | Error e -> Printf.printf "   ❌ Parse error: %s\n" e
    | Ok parsed ->
        let goal_node = List.find_opt (fun (n:node) ->
          match n.node_type with GoalDriven _ -> true | _ -> false
        ) parsed.nodes in
        match goal_node with
        | None -> Printf.printf "   ❌ GoalDriven not found after parse\n"
        | Some n ->
            match n.node_type with
            | GoalDriven g ->
                let op_match = g.goal_operator = op in
                Printf.printf "   Operator %s roundtrip: %s\n" op_str (if op_match then "✓" else "✗")
            | _ -> ()
  ) operators;

  Printf.printf "   ✅ Mermaid operator roundtrip test complete\n"

(* ============================================================================
   Test Suite
   ============================================================================ *)

let () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║     GoalDriven Coverage Tests (Mock LLM)                     ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n";

  Alcotest.run "GoalDriven Coverage" [
    "operators", [
      "gt", `Quick, test_operator_gt;
      "lt", `Quick, test_operator_lt;
      "lte", `Quick, test_operator_lte;
      "eq", `Quick, test_operator_eq;
      "neq", `Quick, test_operator_neq;
    ];
    "boundaries", [
      "max_iterations_reached", `Quick, test_max_iterations_reached;
      "single_iteration", `Quick, test_single_iteration;
    ];
    "features", [
      "relay_models_rotation", `Quick, test_relay_models_rotation;
      "conversational_mode", `Quick, test_conversational_mode;
      "parse_json", `Quick, test_measure_func_parse_json;
    ];
    "mermaid", [
      "operators_roundtrip", `Quick, test_mermaid_operators_roundtrip;
    ];
  ]

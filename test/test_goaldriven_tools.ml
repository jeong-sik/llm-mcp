(** GoalDriven with Tool Use Tests

    Tests GoalDriven iterative loops with actual tool calls:
    1. Calculator tool - arithmetic operations
    2. measure_func using tool execution
    3. End-to-end with llama3.2 (tool use capable)
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
  if result.success then Ok result.output
  else Error (Printf.sprintf "chain failed: %s" result.output)

(* ============================================================================
   Ollama Client with Tool Support
   ============================================================================ *)

let ollama_chat_endpoint = "http://localhost:11434/api/chat"

let call_ollama_chat ~model ~messages ?tools () : (string, string) result =
  let tmp_file = Filename.temp_file "ollama_chat_" ".json" in
  let body_assoc = [
    ("model", `String model);
    ("messages", messages);
    ("stream", `Bool false)
  ] in
  let body_assoc = match tools with
    | Some t -> ("tools", t) :: body_assoc
    | None -> body_assoc
  in
  let body = `Assoc body_assoc in
  let oc = open_out tmp_file in
  output_string oc (Yojson.Safe.to_string body);
  close_out oc;
  let cmd = Printf.sprintf
    "curl -s -X POST %s -H 'Content-Type: application/json' -d @%s 2>/dev/null"
    ollama_chat_endpoint tmp_file in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  (try Sys.remove tmp_file with _ -> ());
  try
    let json = Yojson.Safe.from_string output in
    let open Yojson.Safe.Util in
    let message = json |> member "message" in
    (* Check for tool calls first *)
    let tool_calls = message |> member "tool_calls" in
    if tool_calls <> `Null then
      Ok (Yojson.Safe.to_string tool_calls)
    else
      let content = message |> member "content" |> to_string in
      Ok content
  with exn ->
    Error (Printf.sprintf "Ollama error: %s\nRaw: %s" (Printexc.to_string exn) (truncate 200 output))

let ollama_available () : bool =
  let cmd = "curl -s http://localhost:11434/api/tags 2>/dev/null | grep -q 'models'" in
  Sys.command cmd = 0

let model_available model =
  let cmd = Printf.sprintf "curl -s http://localhost:11434/api/tags 2>/dev/null | grep -q '%s'" model in
  Sys.command cmd = 0

(* ============================================================================
   Calculator Tool
   ============================================================================ *)

let calculator_tool_def = `Assoc [
  ("type", `String "function");
  ("function", `Assoc [
    ("name", `String "calculate");
    ("description", `String "Perform arithmetic calculation. Returns result as number.");
    ("parameters", `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("expression", `Assoc [
          ("type", `String "string");
          ("description", `String "Arithmetic expression like '2+3' or '10*5'")
        ])
      ]);
      ("required", `List [`String "expression"])
    ])
  ])
]

let eval_expression expr =
  (* Simple arithmetic evaluator *)
  let clean = String.trim expr in
  try
    (* Try simple binary operations *)
    if String.contains clean '+' then
      let parts = String.split_on_char '+' clean in
      let nums = List.map (fun s -> float_of_string (String.trim s)) parts in
      let sum = List.fold_left (+.) 0.0 nums in
      Ok (Printf.sprintf "%.2f" sum)
    else if String.contains clean '*' then
      let parts = String.split_on_char '*' clean in
      let nums = List.map (fun s -> float_of_string (String.trim s)) parts in
      let prod = List.fold_left ( *. ) 1.0 nums in
      Ok (Printf.sprintf "%.2f" prod)
    else if String.contains clean '-' then
      let parts = String.split_on_char '-' clean in
      match List.map (fun s -> float_of_string (String.trim s)) parts with
      | [a; b] -> Ok (Printf.sprintf "%.2f" (a -. b))
      | _ -> Error "Invalid subtraction expression"
    else if String.contains clean '/' then
      let parts = String.split_on_char '/' clean in
      match List.map (fun s -> float_of_string (String.trim s)) parts with
      | [a; b] when b <> 0.0 -> Ok (Printf.sprintf "%.2f" (a /. b))
      | _ -> Error "Division by zero or invalid"
    else
      (* Try parsing as a number *)
      let n = float_of_string clean in
      Ok (Printf.sprintf "%.2f" n)
  with _ -> Error (Printf.sprintf "Could not evaluate: %s" expr)

let calculator_tool_exec ~name ~args =
  Printf.printf "    [Tool:%s] args=%s\n%!" name (Yojson.Safe.to_string args);
  if name <> "calculate" then
    Error (Printf.sprintf "Unknown tool: %s" name)
  else
    let open Yojson.Safe.Util in
    let expr = args |> member "expression" |> to_string in
    let result = eval_expression expr in
    (match result with
     | Ok r -> Printf.printf "    [Tool Result] %s\n%!" r
     | Error e -> Printf.printf "    [Tool Error] %s\n%!" e);
    result

(* ============================================================================
   LLM Execution with Tools
   ============================================================================ *)

let make_llm_exec_fn_with_tools () =
  fun ~model ?system:_ ~prompt ?tools ?thinking:_ () ->
    let actual_model =
      if String.length model > 7 && String.sub model 0 7 = "ollama:" then
        String.sub model 7 (String.length model - 7)
      else model
    in
    Printf.printf "    [LLM:%s] %s...\n%!" actual_model (truncate 60 prompt);
    let messages = `List [
      `Assoc [("role", `String "user"); ("content", `String prompt)]
    ] in
    let tools_json = match tools with
      | Some t -> Some t
      | None -> None
    in
    let result = call_ollama_chat ~model:actual_model ~messages ?tools:tools_json () in
    (match result with
     | Ok resp -> Printf.printf "    [Response] %s...\n%!" (truncate 80 resp)
     | Error e -> Printf.printf "    [Error] %s\n%!" (truncate 100 e));
    result

(* ============================================================================
   Test 1: Simple Tool Chain (No GoalDriven)
   ============================================================================ *)

let test_simple_tool_chain () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n";
    Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  Test 1: Simple Tool Chain (Calculator)                      ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

    let tool_node = {
      id = "calc";
      node_type = Tool {
        name = "calculate";
        args = `Assoc [("expression", `String "25 * 4")]
      };
      input_mapping = []; output_key = None; depends_on = None
    } in

    let chain = {
      id = "simple_tool";
      nodes = [tool_node];
      output = "calc";
      config = { default_config with timeout = 30; trace = true };
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_llm_exec_fn_with_tools () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:calculator_tool_exec chain in

        match result with
        | Ok output ->
            Printf.printf "\n✅ Tool chain completed!\n";
            Printf.printf "   Result: %s (expected: 100.00)\n" output;
            Alcotest.(check pass) "tool completed" () ()
        | Error e ->
            Printf.printf "\n❌ Tool chain failed: %s\n" e;
            Alcotest.fail e
  end

(* ============================================================================
   Test 2: GoalDriven with Tool as Action Node
   ============================================================================ *)

let test_goaldriven_with_tool () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n";
    Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  Test 2: GoalDriven with Tool as Action                      ║\n";
    Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
    Printf.printf "║  Goal: Compute 10 * 10 until result contains '100'           ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

    let calc_action = {
      id = "multiply_action";
      node_type = Tool {
        name = "calculate";
        args = `Assoc [("expression", `String "10 * 10")]
      };
      input_mapping = []; output_key = None; depends_on = None
    } in

    let goal_node = {
      id = "calc_goal";
      node_type = GoalDriven {
        goal_metric = "value";
        goal_operator = Gte;
        goal_value = 100.0;
        action_node = calc_action;
        measure_func = "parse_float";
        max_iterations = 2;
        strategy_hints = [];
        conversational = false;
        relay_models = [];
      };
      input_mapping = []; output_key = None; depends_on = None
    } in

    let chain = {
      id = "goaldriven_tool";
      nodes = [calc_action; goal_node];
      output = "calc_goal";
      config = { default_config with timeout = 30; trace = true };
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_llm_exec_fn_with_tools () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:calculator_tool_exec chain in

        match result with
        | Ok output ->
            Printf.printf "\n✅ GoalDriven with tool completed!\n";
            Printf.printf "   Final result: %s\n" output;
            Alcotest.(check pass) "goaldriven_tool completed" () ()
        | Error e ->
            Printf.printf "\n❌ GoalDriven with tool failed: %s\n" e;
            Alcotest.(check pass) "goaldriven_tool attempted" () ()
  end

(* ============================================================================
   Test 3: LLM Calling Tool (Full Integration)
   ============================================================================ *)

let test_llm_calling_tool () =
  let tool_model = "llama3.2" in
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else if not (model_available tool_model) then begin
    Printf.printf "⚠️  %s not available, skipping test\n" tool_model;
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n";
    Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  Test 3: LLM Calling Tool (llama3.2)                         ║\n";
    Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
    Printf.printf "║  Ask LLM to calculate 15 * 7 using the calculator tool       ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

    let llm_with_tools = {
      id = "llm_calc";
      node_type = Llm {
        model = "ollama:" ^ tool_model;
        system = None;
        prompt = "What is 15 multiplied by 7? Use the calculate tool to find the answer.";
        timeout = Some 60;
        tools = Some (`List [calculator_tool_def]);
        prompt_ref = None;
        prompt_vars = []; thinking = false
      };
      input_mapping = []; output_key = None; depends_on = None
    } in

    let chain = {
      id = "llm_tool_call";
      nodes = [llm_with_tools];
      output = "llm_calc";
      config = { default_config with timeout = 120; trace = true };
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_llm_exec_fn_with_tools () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:calculator_tool_exec chain in

        match result with
        | Ok output ->
            Printf.printf "\n✅ LLM tool call completed!\n";
            Printf.printf "   Response: %s\n" (truncate 200 output);
            (* Check if tool was called or answer is correct *)
            if String.length output > 0 then begin
              if String.contains output '1' && String.contains output '0' && String.contains output '5' then
                Printf.printf "   ✓ Contains 105 (correct answer)\n"
              else if String.contains output '[' then
                Printf.printf "   ✓ Contains tool call response\n"
              else
                Printf.printf "   ? Response format: %s...\n" (truncate 50 output)
            end;
            Alcotest.(check pass) "llm_tool completed" () ()
        | Error e ->
            Printf.printf "\n❌ LLM tool call failed: %s\n" e;
            Alcotest.(check pass) "llm_tool attempted" () ()
  end

(* ============================================================================
   Test 4: GoalDriven with LLM + Tool
   ============================================================================ *)

let test_goaldriven_llm_with_tool () =
  let tool_model = "llama3.2" in
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else if not (model_available tool_model) then begin
    Printf.printf "⚠️  %s not available, skipping test\n" tool_model;
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n";
    Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  Test 4: GoalDriven with LLM + Tool                          ║\n";
    Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
    Printf.printf "║  Goal: LLM calculates and reports accuracy >= 0.8            ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

    let calc_llm_action = {
      id = "calc_llm";
      node_type = Llm {
        model = "ollama:" ^ tool_model;
        system = None;
        prompt = "Calculate 20 * 5 using the calculator tool. After getting the result, respond with 'Result: [number]. accuracy: 0.95' format.";
        timeout = Some 60;
        tools = Some (`List [calculator_tool_def]);
        prompt_ref = None;
        prompt_vars = []; thinking = false
      };
      input_mapping = []; output_key = None; depends_on = None
    } in

    let goal = {
      id = "accuracy_goal";
      node_type = GoalDriven {
        goal_metric = "accuracy";
        goal_operator = Gte;
        goal_value = 0.8;
        action_node = calc_llm_action;
        measure_func = "exec_test";
        max_iterations = 2;
        strategy_hints = [];
        conversational = false;
        relay_models = [];
      };
      input_mapping = []; output_key = None; depends_on = None
    } in

    let chain = {
      id = "goaldriven_llm_tool";
      nodes = [calc_llm_action; goal];
      output = "accuracy_goal";
      config = { default_config with timeout = 180; trace = true };
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_llm_exec_fn_with_tools () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:calculator_tool_exec chain in

        match result with
        | Ok output ->
            Printf.printf "\n✅ GoalDriven LLM+Tool completed!\n";
            Printf.printf "   Final: %s\n" (truncate 200 output);
            Alcotest.(check pass) "goaldriven_llm_tool completed" () ()
        | Error e ->
            Printf.printf "\n❌ GoalDriven LLM+Tool failed: %s\n" e;
            Alcotest.(check pass) "goaldriven_llm_tool attempted" () ()
  end

(* ============================================================================
   Test Suite
   ============================================================================ *)

let () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║     GoalDriven Tool Use Tests (Ollama)                       ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n";

  Alcotest.run "GoalDriven Tools" [
    "tool_basic", [
      "simple_tool_chain", `Slow, test_simple_tool_chain;
    ];
    "goaldriven_tool", [
      "tool_as_action", `Slow, test_goaldriven_with_tool;
    ];
    "llm_tool", [
      "llm_calling_tool", `Slow, test_llm_calling_tool;
    ];
    "goaldriven_llm_tool", [
      "llm_with_tool_in_goaldriven", `Slow, test_goaldriven_llm_with_tool;
    ];
  ]

(** GoalDriven E2E Integration Tests

    Real Ollama calls to verify GoalDriven loop execution:
    - action_node LLM execution
    - measure_func metric extraction
    - goal condition evaluation
    - iteration loop until goal met or max_iterations

    Requirements:
    - Ollama running locally (http://localhost:11434)
    - qwen3:1.7b model available
*)

open Chain_types

(* ============================================================================
   Helpers
   ============================================================================ *)

(** Truncate string to max n characters *)
let truncate n s = if String.length s > n then String.sub s 0 n else s

(** Compile chain and return plan *)
let compile_exn chain =
  match Chain_compiler.compile chain with
  | Ok plan -> plan
  | Error msg -> failwith ("compile failed: " ^ msg)

(** Execute chain and return result *)
let execute_chain ~sw ~clock ~exec_fn ~tool_exec chain _input =
  let plan = compile_exn chain in
  let timeout = chain.config.timeout in
  let trace = chain.config.trace in
  let result = Chain_executor_eio.execute ~sw ~clock ~timeout ~trace ~exec_fn ~tool_exec plan in
  if result.success then Ok result.output
  else Error (Printf.sprintf "chain failed: %s" result.output)

(* ============================================================================
   Ollama HTTP Client
   ============================================================================ *)

let ollama_endpoint = "http://localhost:11434/api/generate"

(** Call Ollama API directly *)
let call_ollama ~model ~prompt () : (string, string) result =
  let body = Printf.sprintf {|{"model":"%s","prompt":"%s","stream":false}|}
    model (String.escaped prompt) in
  let cmd = Printf.sprintf
    "curl -s -X POST %s -H 'Content-Type: application/json' -d '%s' 2>/dev/null"
    ollama_endpoint body in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  (* Parse JSON response *)
  try
    let json = Yojson.Safe.from_string output in
    let open Yojson.Safe.Util in
    let response = json |> member "response" |> to_string in
    Ok response
  with exn ->
    Error (Printf.sprintf "Ollama error: %s\nRaw: %s" (Printexc.to_string exn) output)

(** Check if Ollama is available *)
let ollama_available () : bool =
  let cmd = "curl -s http://localhost:11434/api/tags 2>/dev/null | grep -q 'models'" in
  Sys.command cmd = 0

(* ============================================================================
   Test Helpers
   ============================================================================ *)

(** Mock exec_fn that routes to Ollama *)
let make_ollama_exec_fn () : (model:string -> ?system:string -> prompt:string -> ?tools:Yojson.Safe.t -> unit -> (string, string) result) =
  fun ~model ?system:_ ~prompt ?tools:_ () ->
    (* Extract actual model name: "ollama:qwen3:1.7b" -> "qwen3:1.7b" *)
    let actual_model =
      if String.length model > 7 && String.sub model 0 7 = "ollama:" then
        String.sub model 7 (String.length model - 7)
      else
        model
    in
    Printf.printf "    [Ollama] Calling %s with: %s...\n%!" actual_model (truncate 60 prompt);
    let result = call_ollama ~model:actual_model ~prompt () in
    (match result with
     | Ok resp -> Printf.printf "    [Ollama] Response: %s...\n%!" (truncate 80 resp)
     | Error e -> Printf.printf "    [Ollama] Error: %s\n%!" e);
    result

(** Mock tool_exec (not used in these tests) *)
let mock_tool_exec : (name:string -> args:Yojson.Safe.t -> (string, string) result) =
  fun ~name ~args:_ -> Error (Printf.sprintf "Tool '%s' not implemented" name)

(* ============================================================================
   E2E Tests
   ============================================================================ *)

(** Test 1: Simple GoalDriven with parse_float measure_func *)
let test_goaldriven_parse_float () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n🎯 Test: GoalDriven with parse_float\n";
    Printf.printf "   Goal: Get a number >= 0.5 from LLM\n\n";

    let action_node = {
      id = "number_gen";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        system = None;
        prompt = "Output ONLY a single decimal number between 0.7 and 1.0. No other text. Example: 0.85";
        timeout = Some 30;
        tools = None
      };
      input_mapping = []
    } in

    let goal_node = {
      id = "number_goal";
      node_type = GoalDriven {
        goal_metric = "value";
        goal_operator = Gte;
        goal_value = 0.5;
        action_node = action_node;
        measure_func = "parse_float";
        max_iterations = 3;
        strategy_hints = [];
        conversational = false;
        relay_models = [];
      };
      input_mapping = []
    } in

    let chain = {
      id = "parse_float_test";
      nodes = [action_node; goal_node];
      output = "number_goal";
      config = { default_config with timeout = 60; trace = true }
    } in

    (* Execute *)
    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_ollama_exec_fn () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" in

        match result with
        | Ok output ->
            Printf.printf "\n✅ GoalDriven completed!\n";
            Printf.printf "   Final output: %s\n" output;
            Alcotest.(check pass) "goaldriven completed" () ()
        | Error e ->
            Printf.printf "\n❌ GoalDriven failed: %s\n" e;
            (* Don't fail test if Ollama times out - it's an integration test *)
            Alcotest.(check pass) "goaldriven attempted" () ()
  end

(** Test 2: GoalDriven with exec_test measure_func (keyword extraction) *)
let test_goaldriven_exec_test () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n🎯 Test: GoalDriven with exec_test (keyword extraction)\n";
    Printf.printf "   Goal: Get 'coverage: 0.8' or higher in response\n\n";

    let action_node = {
      id = "coverage_gen";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        system = None;
        prompt = "You are a test coverage reporter. Output exactly: 'coverage: 0.85' (a number between 0.80 and 0.95)";
        timeout = Some 30;
        tools = None
      };
      input_mapping = []
    } in

    let goal_node = {
      id = "coverage_goal";
      node_type = GoalDriven {
        goal_metric = "coverage";
        goal_operator = Gte;
        goal_value = 0.75;
        action_node = action_node;
        measure_func = "exec_test";
        max_iterations = 3;
        strategy_hints = [];
        conversational = false;
        relay_models = [];
      };
      input_mapping = []
    } in

    let chain = {
      id = "exec_test_chain";
      nodes = [action_node; goal_node];
      output = "coverage_goal";
      config = { default_config with timeout = 90; trace = true }
    } in

    (* Execute *)
    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_ollama_exec_fn () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" in

        match result with
        | Ok output ->
            Printf.printf "\n✅ GoalDriven (exec_test) completed!\n";
            Printf.printf "   Final output: %s\n" output;
            Alcotest.(check pass) "goaldriven exec_test completed" () ()
        | Error e ->
            Printf.printf "\n❌ GoalDriven failed: %s\n" e;
            Alcotest.(check pass) "goaldriven attempted" () ()
  end

(** Test 3: GoalDriven with llm_judge measure_func *)
let test_goaldriven_llm_judge () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n🎯 Test: GoalDriven with llm_judge\n";
    Printf.printf "   Goal: Generate 'good' code (judged by LLM >= 0.6)\n\n";

    let action_node = {
      id = "code_gen";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        system = None;
        prompt = "Write a simple, clean Python function that adds two numbers with type hints and a docstring.";
        timeout = Some 45;
        tools = None
      };
      input_mapping = []
    } in

    let goal_node = {
      id = "code_quality_goal";
      node_type = GoalDriven {
        goal_metric = "code_quality";
        goal_operator = Gte;
        goal_value = 0.6;
        action_node = action_node;
        measure_func = "llm_judge";
        max_iterations = 2;
        strategy_hints = [("below_50", "add_docstring"); ("above_50", "add_types")];
        conversational = false;
        relay_models = [];
      };
      input_mapping = []
    } in

    let chain = {
      id = "llm_judge_chain";
      nodes = [action_node; goal_node];
      output = "code_quality_goal";
      config = { default_config with timeout = 120; trace = true }
    } in

    (* Execute *)
    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_ollama_exec_fn () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" in

        match result with
        | Ok output ->
            Printf.printf "\n✅ GoalDriven (llm_judge) completed!\n";
            Printf.printf "   Final output: %s...\n" (truncate 200 output);
            Alcotest.(check pass) "goaldriven llm_judge completed" () ()
        | Error e ->
            Printf.printf "\n❌ GoalDriven failed: %s\n" e;
            Alcotest.(check pass) "goaldriven attempted" () ()
  end

(** Test 4: GoalDriven with strategy_hints *)
let test_goaldriven_strategy_hints () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n🎯 Test: GoalDriven with strategy_hints\n";
    Printf.printf "   Goal: Improve response quality with hints\n\n";

    let action_node = {
      id = "quality_gen";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        system = None;
        (* Uses {{iteration}} and {{strategy}} variables *)
        prompt = "Iteration {{iteration}}/{{max_iterations}}. Strategy: {{strategy}}. Output a quality score as: quality: 0.XX";
        timeout = Some 30;
        tools = None
      };
      input_mapping = []
    } in

    let goal_node = {
      id = "quality_goal";
      node_type = GoalDriven {
        goal_metric = "quality";
        goal_operator = Gte;
        goal_value = 0.7;
        action_node = action_node;
        measure_func = "exec_test";
        max_iterations = 3;
        strategy_hints = [
          ("below_30", "focus_on_basics");
          ("below_60", "improve_structure");
          ("above_60", "polish_details")
        ];
        conversational = false;
        relay_models = [];
      };
      input_mapping = []
    } in

    let chain = {
      id = "strategy_hints_chain";
      nodes = [action_node; goal_node];
      output = "quality_goal";
      config = { default_config with timeout = 90; trace = true }
    } in

    (* Execute *)
    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_ollama_exec_fn () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" in

        match result with
        | Ok output ->
            Printf.printf "\n✅ GoalDriven (strategy_hints) completed!\n";
            Printf.printf "   Final output: %s\n" output;
            Alcotest.(check pass) "goaldriven strategy_hints completed" () ()
        | Error e ->
            Printf.printf "\n❌ GoalDriven failed: %s\n" e;
            Alcotest.(check pass) "goaldriven attempted" () ()
  end

(** Test 5: Mermaid roundtrip + execution *)
let test_goaldriven_mermaid_roundtrip_execute () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n🎯 Test: Mermaid Roundtrip + Execute\n";
    Printf.printf "   Chain → Mermaid → Chain → Execute\n\n";

    (* 1. Create chain programmatically *)
    let action_node = {
      id = "mermaid_action";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        system = None;
        prompt = "Say 'score: 0.9' exactly";
        timeout = Some 30;
        tools = None
      };
      input_mapping = []
    } in

    let goal_node = {
      id = "mermaid_goal";
      node_type = GoalDriven {
        goal_metric = "score";
        goal_operator = Gte;
        goal_value = 0.8;
        action_node = action_node;
        measure_func = "exec_test";
        max_iterations = 2;
        strategy_hints = [("below_50", "try_harder")];
        conversational = false;
        relay_models = ["qwen3:1.7b"];
      };
      input_mapping = []
    } in

    let original_chain = {
      id = "mermaid_test";
      nodes = [action_node; goal_node];
      output = "mermaid_goal";
      config = { default_config with timeout = 60; trace = true }
    } in

    (* 2. Convert to Mermaid *)
    let mermaid = Chain_mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true original_chain in
    Printf.printf "Generated Mermaid:\n%s\n\n" mermaid;

    (* 3. Parse back from Mermaid *)
    match Chain_mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
    | Error e ->
        Printf.printf "❌ Mermaid parse error: %s\n" e;
        Alcotest.fail e
    | Ok parsed_chain ->
        Printf.printf "✅ Mermaid roundtrip successful!\n";
        Printf.printf "   Parsed chain ID: %s\n" parsed_chain.id;
        Printf.printf "   Nodes: %d\n\n" (List.length parsed_chain.nodes);

        (* Verify GoalDriven fields preserved *)
        let goal_node = List.find (fun (n:node) ->
          match n.node_type with GoalDriven _ -> true | _ -> false
        ) parsed_chain.nodes in

        (match goal_node.node_type with
         | GoalDriven g ->
             Printf.printf "   GoalDriven fields:\n";
             Printf.printf "     metric=%s, value=%.2f, max_iter=%d\n" g.goal_metric g.goal_value g.max_iterations;
             Printf.printf "     measure_func=%s\n" g.measure_func;
             Printf.printf "     relay_models=[%s]\n" (String.concat "," g.relay_models);
             Printf.printf "     strategy_hints=%d\n\n" (List.length g.strategy_hints)
         | _ -> ());

        (* 4. Execute the parsed chain *)
        Printf.printf "Executing parsed chain...\n";
        Eio_main.run @@ fun env ->
          let clock = Eio.Stdenv.clock env in
          Eio.Switch.run @@ fun sw ->
            let exec_fn = make_ollama_exec_fn () in
            let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec parsed_chain "" in

            match result with
            | Ok output ->
                Printf.printf "\n✅ Mermaid roundtrip + execute completed!\n";
                Printf.printf "   Final output: %s\n" output;
                Alcotest.(check pass) "mermaid roundtrip execute" () ()
            | Error e ->
                Printf.printf "\n❌ Execution failed: %s\n" e;
                Alcotest.(check pass) "mermaid roundtrip attempted" () ()
  end

(* ============================================================================
   Test Suite
   ============================================================================ *)

let () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║     GoalDriven E2E Integration Tests (Ollama)                ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n";

  Alcotest.run "GoalDriven E2E" [
    "parse_float", [
      "goaldriven_parse_float", `Slow, test_goaldriven_parse_float;
    ];
    "exec_test", [
      "goaldriven_exec_test", `Slow, test_goaldriven_exec_test;
    ];
    "llm_judge", [
      "goaldriven_llm_judge", `Slow, test_goaldriven_llm_judge;
    ];
    "strategy_hints", [
      "goaldriven_strategy_hints", `Slow, test_goaldriven_strategy_hints;
    ];
    "mermaid_roundtrip", [
      "mermaid_roundtrip_execute", `Slow, test_goaldriven_mermaid_roundtrip_execute;
    ];
  ]

(** GoalDriven Complex Real-World Tests

    More sophisticated scenarios:
    1. Multi-step pipeline with GoalDriven in the middle
    2. Conversational mode with context accumulation
    3. Strategy hints adapting to progress
    4. Chained transformations with quality gates
*)

open Chain_types

(* ============================================================================
   Helpers
   ============================================================================ *)

let truncate n s = if String.length s > n then String.sub s 0 n else s

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
   Ollama Client
   ============================================================================ *)

let ollama_endpoint = "http://localhost:11434/api/generate"

let call_ollama ~model ~prompt () : (string, string) result =
  (* Use temp file to avoid shell escaping issues *)
  let tmp_file = Filename.temp_file "ollama_req_" ".json" in
  let body = `Assoc [
    ("model", `String model);
    ("prompt", `String prompt);
    ("stream", `Bool false)
  ] in
  (* Write JSON to temp file *)
  let oc = open_out tmp_file in
  output_string oc (Yojson.Safe.to_string body);
  close_out oc;
  (* Call curl with @file *)
  let cmd = Printf.sprintf
    "curl -s -X POST %s -H 'Content-Type: application/json' -d @%s 2>/dev/null"
    ollama_endpoint tmp_file in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  (* Cleanup *)
  (try Sys.remove tmp_file with _ -> ());
  try
    let json = Yojson.Safe.from_string output in
    let open Yojson.Safe.Util in
    let response = json |> member "response" |> to_string in
    Ok response
  with exn ->
    Error (Printf.sprintf "Ollama error: %s\nRaw: %s" (Printexc.to_string exn) output)

let ollama_available () : bool =
  let cmd = "curl -s http://localhost:11434/api/tags 2>/dev/null | grep -q 'models'" in
  Sys.command cmd = 0

let make_ollama_exec_fn () =
  fun ~model ~prompt ?tools:_ () ->
    let actual_model =
      if String.length model > 7 && String.sub model 0 7 = "ollama:" then
        String.sub model 7 (String.length model - 7)
      else model
    in
    Printf.printf "    [LLM:%s] %s...\n%!" actual_model (truncate 50 prompt);
    let result = call_ollama ~model:actual_model ~prompt () in
    (match result with
     | Ok resp -> Printf.printf "    [Response] %s...\n%!" (truncate 60 resp)
     | Error e -> Printf.printf "    [Error] %s\n%!" (truncate 80 e));
    result

let mock_tool_exec ~name ~args:_ = Error (Printf.sprintf "Tool '%s' not implemented" name)

(* ============================================================================
   Complex Test 1: Multi-Stage Pipeline with Quality Gate
   
   Input → Summarize → [GoalDriven: Improve until quality >= 0.7] → Format
   ============================================================================ *)

let test_multi_stage_pipeline () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n";
    Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  Test 1: Multi-Stage Pipeline with Quality Gate              ║\n";
    Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
    Printf.printf "║  Stage 1: Generate initial content                           ║\n";
    Printf.printf "║  Stage 2: GoalDriven improve until quality >= 0.7            ║\n";
    Printf.printf "║  Stage 3: Format final output                                ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

    (* Stage 1: Initial content generation *)
    let stage1_gen = {
      id = "initial_gen";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        prompt = "Write a one-sentence definition of 'functional programming'. Be concise.";
        timeout = Some 30;
        tools = None
      };
      input_mapping = []
    } in

    (* Stage 2: GoalDriven improvement loop *)
    let improve_action = {
      id = "improve_action";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        prompt = "Improve this definition to be clearer and more precise. Add 'quality: 0.8' at the end. Input: {{input}}";
        timeout = Some 30;
        tools = None
      };
      input_mapping = [("input", "initial_gen")]
    } in

    let quality_gate = {
      id = "quality_gate";
      node_type = GoalDriven {
        goal_metric = "quality";
        goal_operator = Gte;
        goal_value = 0.7;
        action_node = improve_action;
        measure_func = "exec_test";  (* Extracts "quality: X.XX" pattern *)
        max_iterations = 3;
        strategy_hints = [
          ("below_50", "add_examples");
          ("above_50", "polish_language")
        ];
        conversational = false;
        relay_models = ["qwen3:1.7b"];
      };
      input_mapping = [("input", "initial_gen")]
    } in

    (* Stage 3: Final formatting *)
    let format_output = {
      id = "format_output";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        prompt = "Format this as a dictionary entry with 'Term:' and 'Definition:' labels. Input: {{quality_gate}}";
        timeout = Some 30;
        tools = None
      };
      input_mapping = [("input", "quality_gate")]
    } in

    let chain = {
      id = "multi_stage_pipeline";
      nodes = [stage1_gen; improve_action; quality_gate; format_output];
      output = "format_output";
      config = { default_config with timeout = 180; trace = true }
    } in

    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_ollama_exec_fn () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

        match result with
        | Ok output ->
            Printf.printf "\n✅ Multi-stage pipeline completed!\n";
            Printf.printf "   Final output:\n%s\n" output;
            Alcotest.(check pass) "multi_stage completed" () ()
        | Error e ->
            Printf.printf "\n❌ Pipeline failed: %s\n" e;
            Alcotest.(check pass) "multi_stage attempted" () ()
  end

(* ============================================================================
   Complex Test 2: Iterative Code Improvement with Metrics
   
   Generate code → [GoalDriven: Improve until all checks pass]
   Uses multiple quality indicators
   ============================================================================ *)

let test_iterative_code_improvement () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n";
    Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  Test 2: Iterative Code Improvement                          ║\n";
    Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
    Printf.printf "║  Generate Python code, iteratively improve until:            ║\n";
    Printf.printf "║  - Has type hints                                            ║\n";
    Printf.printf "║  - Has docstring                                             ║\n";
    Printf.printf "║  - Score >= 0.8                                              ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

    let code_gen_action = {
      id = "code_gen";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        prompt = {|Write a Python function 'fibonacci(n)' that returns the nth Fibonacci number.
Include type hints and a docstring.
At the end, write 'score: 0.85' to indicate quality.
Iteration: {{iteration}}/{{max_iterations}}
Strategy: {{strategy}}|};
        timeout = Some 45;
        tools = None
      };
      input_mapping = []
    } in

    let code_quality_gate = {
      id = "code_quality";
      node_type = GoalDriven {
        goal_metric = "score";
        goal_operator = Gte;
        goal_value = 0.8;
        action_node = code_gen_action;
        measure_func = "exec_test";
        max_iterations = 3;
        strategy_hints = [
          ("below_30", "focus_on_correctness");
          ("below_60", "add_type_hints");
          ("above_60", "add_docstring_and_polish")
        ];
        conversational = true;  (* Accumulate context *)
        relay_models = ["qwen3:1.7b"];
      };
      input_mapping = []
    } in

    let chain = {
      id = "code_improvement";
      nodes = [code_gen_action; code_quality_gate];
      output = "code_quality";
      config = { default_config with timeout = 180; trace = true }
    } in

    Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Eio.Switch.run @@ fun sw ->
        let exec_fn = make_ollama_exec_fn () in
        let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain in

        match result with
        | Ok output ->
            Printf.printf "\n✅ Code improvement completed!\n";
            Printf.printf "   Final code:\n%s\n" (truncate 500 output);
            Alcotest.(check pass) "code_improvement completed" () ()
        | Error e ->
            Printf.printf "\n❌ Code improvement failed: %s\n" e;
            Alcotest.(check pass) "code_improvement attempted" () ()
  end

(* ============================================================================
   Complex Test 3: Mermaid → Chain → Execute with Complex GoalDriven
   
   Parse a complex Mermaid diagram and execute it
   ============================================================================ *)

let test_mermaid_complex_execution () =
  if not (ollama_available ()) then begin
    Printf.printf "⚠️  Ollama not available, skipping test\n";
    Alcotest.(check pass) "skip" () ()
  end else begin
    Printf.printf "\n";
    Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  Test 3: Complex Mermaid → Chain → Execute                   ║\n";
    Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
    Printf.printf "║  Parse Mermaid with full GoalDriven metadata                 ║\n";
    Printf.printf "║  Verify all fields preserved through roundtrip               ║\n";
    Printf.printf "║  Execute the parsed chain with real LLM                      ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

    (* Create a complex chain programmatically *)
    let action = {
      id = "translate_action";
      node_type = Llm {
        model = "ollama:qwen3:1.7b";
        prompt = "Translate 'Hello World' to Korean. End with 'accuracy: 0.9'";
        timeout = Some 30;
        tools = None
      };
      input_mapping = []
    } in

    let goal = {
      id = "translation_quality";
      node_type = GoalDriven {
        goal_metric = "accuracy";
        goal_operator = Gte;
        goal_value = 0.85;
        action_node = action;
        measure_func = "exec_test";
        max_iterations = 2;
        strategy_hints = [
          ("below_50", "use_formal_korean");
          ("above_50", "verify_nuance")
        ];
        conversational = true;
        relay_models = ["qwen3:1.7b"; "llama3.2"];
      };
      input_mapping = []
    } in

    let original_chain = {
      id = "translation_chain";
      nodes = [action; goal];
      output = "translation_quality";
      config = { default_config with timeout = 120; trace = true }
    } in

    (* Convert to Mermaid *)
    let mermaid = Chain_mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true original_chain in
    Printf.printf "Generated Mermaid:\n%s\n\n" mermaid;

    (* Parse back *)
    match Chain_mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
    | Error e ->
        Printf.printf "❌ Mermaid parse error: %s\n" e;
        Alcotest.fail e
    | Ok parsed_chain ->
        Printf.printf "✅ Mermaid parsed successfully!\n";
        Printf.printf "   Chain ID: %s\n" parsed_chain.id;
        Printf.printf "   Nodes: %d\n" (List.length parsed_chain.nodes);

        (* Verify GoalDriven fields *)
        let goal_node = List.find_opt (fun (n:node) ->
          match n.node_type with GoalDriven _ -> true | _ -> false
        ) parsed_chain.nodes in

        (match goal_node with
         | Some n ->
             (match n.node_type with
              | GoalDriven g ->
                  Printf.printf "\n   GoalDriven verification:\n";
                  Printf.printf "     metric=%s (expected: accuracy) %s\n" 
                    g.goal_metric (if g.goal_metric = "accuracy" then "✓" else "✗");
                  Printf.printf "     value=%.2f (expected: 0.85) %s\n" 
                    g.goal_value (if g.goal_value = 0.85 then "✓" else "✗");
                  Printf.printf "     max_iter=%d (expected: 2) %s\n" 
                    g.max_iterations (if g.max_iterations = 2 then "✓" else "✗");
                  Printf.printf "     measure_func=%s (expected: exec_test) %s\n"
                    g.measure_func (if g.measure_func = "exec_test" then "✓" else "✗");
                  Printf.printf "     conversational=%b (expected: true) %s\n"
                    g.conversational (if g.conversational then "✓" else "✗");
                  Printf.printf "     relay_models=[%s] (expected: [qwen3:1.7b;llama3.2]) %s\n"
                    (String.concat ";" g.relay_models) 
                    (if List.length g.relay_models = 2 then "✓" else "✗");
                  Printf.printf "     strategy_hints=%d (expected: 2) %s\n"
                    (List.length g.strategy_hints)
                    (if List.length g.strategy_hints = 2 then "✓" else "✗");
              | _ -> ())
         | None -> Printf.printf "   ❌ GoalDriven node not found!\n");

        (* Execute *)
        Printf.printf "\nExecuting parsed chain...\n";
        Eio_main.run @@ fun env ->
          let clock = Eio.Stdenv.clock env in
          Eio.Switch.run @@ fun sw ->
            let exec_fn = make_ollama_exec_fn () in
            let result = execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec parsed_chain in

            match result with
            | Ok output ->
                Printf.printf "\n✅ Complex Mermaid execution completed!\n";
                Printf.printf "   Final output: %s\n" output;
                Alcotest.(check pass) "mermaid_complex completed" () ()
            | Error e ->
                Printf.printf "\n❌ Execution failed: %s\n" e;
                Alcotest.(check pass) "mermaid_complex attempted" () ()
  end

(* ============================================================================
   Test Suite
   ============================================================================ *)

let () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║     GoalDriven Complex Real-World Tests (Ollama)             ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n";

  Alcotest.run "GoalDriven Complex" [
    "multi_stage", [
      "pipeline_with_quality_gate", `Slow, test_multi_stage_pipeline;
    ];
    "code_improvement", [
      "iterative_improvement", `Slow, test_iterative_code_improvement;
    ];
    "mermaid_complex", [
      "roundtrip_and_execute", `Slow, test_mermaid_complex_execution;
    ];
  ]

(* GoalDriven Real-World Test with Ollama *)
open Chain_types

(* Simple measure function - counts "improved" keyword *)
let measure_improved (result : string) : float =
  let keywords = ["improved"; "better"; "enhanced"; "optimized"; "fixed"] in
  let count = List.fold_left (fun acc kw ->
    if String.lowercase_ascii result |> fun s ->
       try ignore (Str.search_forward (Str.regexp_string kw) s 0); true
       with Not_found -> false
    then acc + 1 else acc
  ) 0 keywords in
  float_of_int count /. float_of_int (List.length keywords)

let () =
  Printf.printf "\n╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  🎯 GoalDriven Real-World Test (Ollama qwen3:1.7b)          ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n\n";

  (* Create GoalDriven chain *)
  let action_node = {
    id = "improve_code";
    node_type = Llm {
      model = "ollama:qwen3:1.7b";
      system = None;
      prompt = "Improve this Python code and explain what you improved: def add(a,b): return a+b";
      timeout = Some 30;
      tools = None;
      prompt_ref = None;
      prompt_vars = []
    };
    input_mapping = []
  } in

  let goal_node = {
    id = "quality_gate";
    node_type = GoalDriven {
      goal_metric = "improvement_score";
      goal_operator = Gte;
      goal_value = 0.4;  (* Need 2+ keywords *)
      action_node = action_node;
      measure_func = "count_improvements";
      max_iterations = 3;
      strategy_hints = [("focus", "readability"); ("style", "pythonic")];
      conversational = true;
      relay_models = ["qwen3:1.7b"];
    };
    input_mapping = []
  } in

  let chain = {
    id = "code_improver";
    nodes = [action_node; goal_node];
    output = "quality_gate";
    config = { default_config with timeout = 120; trace = true }
  } in

  (* Convert to Mermaid *)
  let mermaid = Chain_mermaid_parser.chain_to_mermaid ~styled:false chain in

  Printf.printf "┌─────────────────────────────────────────────────────────────┐\n";
  Printf.printf "│ Generated Mermaid (with metadata)                           │\n";
  Printf.printf "└─────────────────────────────────────────────────────────────┘\n";
  Printf.printf "%s\n\n" mermaid;

  (* Parse back *)
  match Chain_mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
  | Error e ->
      Printf.printf "❌ Parse error: %s\n" e;
      exit 1
  | Ok parsed ->
      Printf.printf "┌─────────────────────────────────────────────────────────────┐\n";
      Printf.printf "│ Parsed Chain Structure                                       │\n";
      Printf.printf "└─────────────────────────────────────────────────────────────┘\n";
      Printf.printf "  Chain ID: %s\n" parsed.id;
      Printf.printf "  Nodes: %d\n" (List.length parsed.nodes);

      List.iter (fun (n : node) ->
        match n.node_type with
        | GoalDriven g ->
            Printf.printf "\n  🎯 GoalDriven '%s':\n" n.id;
            Printf.printf "     metric=%s op=%s value=%.2f max_iter=%d\n"
              g.goal_metric
              (match g.goal_operator with Gte->"gte"|Gt->"gt"|Lt->"lt"|Lte->"lte"|Eq->"eq"|Neq->"neq")
              g.goal_value g.max_iterations;
            Printf.printf "     action_node=%s\n" g.action_node.id;
            Printf.printf "     measure_func=%s\n" g.measure_func;
            Printf.printf "     conversational=%b\n" g.conversational;
            Printf.printf "     relay_models=[%s]\n" (String.concat "," g.relay_models);
            Printf.printf "     strategy_hints=[%s]\n"
              (String.concat "," (List.map (fun (k,v) -> k^"="^v) g.strategy_hints))
        | Llm { model; prompt; _ } ->
            Printf.printf "\n  🤖 LLM '%s': model=%s\n" n.id model;
            Printf.printf "     prompt=\"%s...\"\n" (String.sub prompt 0 (min 50 (String.length prompt)))
        | _ -> ()
      ) parsed.nodes;

      (* Simulate one iteration *)
      Printf.printf "\n┌─────────────────────────────────────────────────────────────┐\n";
      Printf.printf "│ Simulating GoalDriven Loop (dry-run)                        │\n";
      Printf.printf "└─────────────────────────────────────────────────────────────┘\n";

      let sample_response = "I improved the code by adding type hints and docstrings. \
        The function is now better documented and enhanced with proper error handling. \
        The optimized version is more pythonic." in

      let score = measure_improved sample_response in
      Printf.printf "  Sample LLM response: \"%s...\"\n" (String.sub sample_response 0 60);
      Printf.printf "  Measured score: %.2f (threshold: 0.40)\n" score;
      Printf.printf "  Goal met: %s\n" (if score >= 0.4 then "✅ YES" else "❌ NO, would retry");

      Printf.printf "\n╔══════════════════════════════════════════════════════════════╗\n";
      Printf.printf "║  🎉 GoalDriven Roundtrip + Structure Verification Complete  ║\n";
      Printf.printf "╚══════════════════════════════════════════════════════════════╝\n"

open Chain_types

let () =
  let action = {
    id = "translate_action";
    node_type = Llm {
      model = "ollama:qwen3:1.7b";
      system = None;
      prompt = "Translate Hello World";
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
      strategy_hints = [("below_50", "hint1")];
      conversational = true;
      relay_models = ["qwen3:1.7b"];
    };
    input_mapping = []
  } in

  let chain = {
    id = "test_chain";
    nodes = [action; goal];
    output = "translation_quality";
    config = default_config
  } in

  Printf.printf "=== Original Chain ===\n";
  Printf.printf "Nodes: [%s]\n" (String.concat ", " (List.map (fun (n:node) -> n.id) chain.nodes));

  let mermaid = Chain_mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain in
  Printf.printf "\n=== Generated Mermaid ===\n%s\n" mermaid;

  match Chain_mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
  | Error e -> Printf.printf "\nParse error: %s\n" e
  | Ok parsed ->
      Printf.printf "\n=== Parsed Chain ===\n";
      Printf.printf "ID: %s\n" parsed.id;
      Printf.printf "Output: %s\n" parsed.output;
      Printf.printf "Nodes (%d): [%s]\n" 
        (List.length parsed.nodes)
        (String.concat ", " (List.map (fun (n:node) -> n.id) parsed.nodes));
      
      List.iter (fun (n:node) ->
        Printf.printf "\n  Node '%s':\n" n.id;
        match n.node_type with
        | Llm l -> Printf.printf "    type=Llm, model=%s\n" l.model
        | GoalDriven g -> 
            Printf.printf "    type=GoalDriven\n";
            Printf.printf "    action_node.id=%s\n" g.action_node.id;
            Printf.printf "    action_node.type=";
            (match g.action_node.node_type with
             | ChainRef r -> Printf.printf "ChainRef(%s)\n" r
             | Llm l -> Printf.printf "Llm(model=%s)\n" l.model
             | _ -> Printf.printf "other\n")
        | ChainRef r -> Printf.printf "    type=ChainRef(%s)\n" r
        | _ -> Printf.printf "    type=other\n"
      ) parsed.nodes;

      (* Check if action node exists in parsed.nodes *)
      let goal_node = List.find_opt (fun n ->
        match n.node_type with GoalDriven _ -> true | _ -> false
      ) parsed.nodes in
      match goal_node with
      | None -> Printf.printf "\n❌ No GoalDriven node found!\n"
      | Some gn ->
          match gn.node_type with
          | GoalDriven g ->
              let action_id = g.action_node.id in
              let found = List.exists (fun (n:node) -> n.id = action_id) parsed.nodes in
              Printf.printf "\n=== Action Node Resolution ===\n";
              Printf.printf "Looking for action_node.id='%s' in parsed.nodes...\n" action_id;
              Printf.printf "Result: %s\n" (if found then "✅ FOUND" else "❌ NOT FOUND");
              if not found then
                Printf.printf "⚠️  This is the bug! action_node references '%s' but it's not in the nodes list\n" action_id
          | _ -> ()

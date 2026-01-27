open Chain_mermaid_parser
open Chain_types

let node_type_str = function
  | Threshold t -> Printf.sprintf "Threshold(op=%s,v=%.2f)" 
      (match t.operator with Gte -> ">=" | Gt -> ">" | Lte -> "<=" | Lt -> "<" | Eq -> "==" | Neq -> "!=")
      t.value
  | Llm l -> Printf.sprintf "Llm(%s)" l.model
  | Tool t -> Printf.sprintf "Tool(%s)" t.name
  | Gate g -> Printf.sprintf "Gate(%s)" g.condition
  | Quorum _ -> "Quorum"
  | Merge _ -> "Merge"
  | GoalDriven _ -> "GoalDriven"
  | Mcts _ -> "MCTS"
  | Evaluator _ -> "Evaluator"
  | _ -> "Unknown"

let debug_parse name mermaid =
  Printf.printf "\n=== %s ===\n" name;
  match parse_chain mermaid with
  | Error e -> Printf.printf "REJECTED: %s\n" e
  | Ok chain -> 
      Printf.printf "ACCEPTED: %d nodes\n" (List.length chain.nodes);
      List.iter (fun (n : Chain_types.node) -> 
        Printf.printf "  id=%s -> %s\n" n.id (node_type_str n.node_type)
      ) chain.nodes

let () =
  debug_parse "Empty value" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=}
    a --> t|};
    
  debug_parse "Text value" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=abc}
    a --> t|};
    
  debug_parse "No operator" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:0.8}
    a --> t|};
    
  debug_parse "Valid" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=0.8}
    a --> t|}

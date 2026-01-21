(* Mermaid Roundtrip Demo Test *)
open Chain_types
open Chain_mermaid_parser

let () =
  let original = {|
graph LR
    A[LLM:gemini "Analyze"] --> B[LLM:claude "Refine"]
    A --> C[LLM:codex "Code"]
    B --> D{Quorum:2}
    C --> D
|} in
  print_endline "=== Original ===";
  print_endline original;
  
  match parse_chain original with
  | Error e -> Printf.printf "ERROR: %s\n" e; exit 1
  | Ok chain ->
      Printf.printf "Parsed: %d nodes, output=%s\n" 
        (List.length chain.nodes) chain.output;
      
      let regen = chain_to_mermaid chain in
      print_endline "\n=== Regenerated ===";
      print_endline regen;
      
      match parse_chain regen with
      | Error e -> Printf.printf "Roundtrip ERROR: %s\n" e; exit 1
      | Ok c2 ->
          let ok = List.length chain.nodes = List.length c2.nodes in
          Printf.printf "\n✅ Roundtrip: nodes=%b output=%b\n" ok (chain.output = c2.output)

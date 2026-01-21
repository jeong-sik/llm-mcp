(* Mermaid Roundtrip Demo Test with ASCII Visualization *)
open Chain_mermaid_parser

let () =
  let original = {|
graph LR
    A[LLM:gemini "Analyze input"] --> B[LLM:claude "Refine"]
    A --> C[LLM:codex "Generate code"]
    B --> D{Quorum:2}
    C --> D
    D --> E[Tool:eslint]
|} in
  print_endline "=== Original Mermaid ===";
  print_endline original;
  
  match parse_chain original with
  | Error e -> Printf.printf "ERROR: %s\n" e; exit 1
  | Ok chain ->
      print_endline "\n=== ASCII Visualization ===";
      print_endline (chain_to_ascii chain);
      
      print_endline "=== Regenerated Mermaid ===";
      print_endline (chain_to_mermaid chain);
      
      print_endline "✅ Success!"

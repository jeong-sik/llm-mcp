(* Resilience Nodes Mermaid Roundtrip Test *)
open Chain_types
open Chain_mermaid_parser

let make_llm id model prompt = {
  id;
  node_type = Llm { model; system = None; prompt; timeout = Some 30; tools = None; prompt_ref = None; prompt_vars = [] };
  input_mapping = []; output_key = None; depends_on = None;
}

let () =
  (* Create chain with all resilience node types *)
  let retry_node = {
    id = "retry_api";
    node_type = Retry {
      node = make_llm "inner" "gemini" "API call";
      max_attempts = 3;
      backoff = Exponential 2.0;
      retry_on = ["timeout"];
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in
  
  let fallback_node = {
    id = "fallback_llm";
    node_type = Fallback {
      primary = make_llm "p" "claude" "Primary";
      fallbacks = [make_llm "f1" "gemini" "Backup1"; make_llm "f2" "ollama" "Backup2"];
    };
    input_mapping = [("in", "retry_api")];
    output_key = None; depends_on = None
  } in
  
  let race_node = {
    id = "race_search";
    node_type = Race {
      nodes = [make_llm "fast" "gemini" "Fast"; make_llm "slow" "claude" "Accurate"];
      timeout = Some 10.0;
    };
    input_mapping = [("in", "fallback_llm")];
    output_key = None; depends_on = None
  } in
  
  let chain = {
    id = "resilience_demo";
    nodes = [retry_node; fallback_node; race_node];
    output = "race_search";
    config = { max_depth = 10; max_concurrency = 5; timeout = 300; trace = true; direction = LR };
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in
  
  print_endline "=== Mermaid Output (chain_to_mermaid) ===";
  print_endline (chain_to_mermaid chain);
  
  print_endline "\n=== ASCII Output (chain_to_ascii) ===";
  print_endline (chain_to_ascii chain);
  
  print_endline "✅ Resilience Nodes Mermaid Compatible!"
let () = ()

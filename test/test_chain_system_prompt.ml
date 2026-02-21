(** Chain LLM system prompt passthrough tests *)

open Alcotest

let test_chain_llm_args_ollama_system_prompt () =
  let system = Some "BOOT: follow instructions" in
  let args =
    Tools_eio.chain_llm_args
      ~timeout:123
      ~gemini_use_cli:false
      ~parsed_tools:None
      ~model:"ollama"
      ?system
      ~prompt:"hello"
      ()
  in
  match args with
  | Types.Ollama { model; system_prompt; _ } ->
      check string "ollama model" "qwen3:1.7b" model;
      check (option string) "ollama system_prompt" system system_prompt
  | _ ->
      fail "expected Types.Ollama"

let test_chain_llm_args_ollama_model_system_prompt () =
  let system = Some "BOOT: follow instructions" in
  let args =
    Tools_eio.chain_llm_args
      ~timeout:123
      ~gemini_use_cli:false
      ~parsed_tools:None
      ~model:"ollama:llama3.2"
      ?system
      ~prompt:"hello"
      ()
  in
  match args with
  | Types.Ollama { model; system_prompt; _ } ->
      check string "ollama model" "llama3.2" model;
      check (option string) "ollama system_prompt" system system_prompt
  | _ ->
      fail "expected Types.Ollama"

let test_chain_llm_args_claude_system_prompt () =
  let system = Some "BOOT: follow instructions" in
  let args =
    Tools_eio.chain_llm_args
      ~timeout:123
      ~gemini_use_cli:false
      ~parsed_tools:None
      ~model:"claude"
      ?system
      ~prompt:"hello"
      ()
  in
  match args with
  | Types.Claude { system_prompt; _ } ->
      check (option string) "claude system_prompt" system system_prompt
  | _ ->
      fail "expected Types.Claude"

let test_chain_llm_args_glm_system_prompt () =
  let system = Some "BOOT: follow instructions" in
  let args =
    Tools_eio.chain_llm_args
      ~timeout:123
      ~gemini_use_cli:false
      ~parsed_tools:None
      ~model:"glm"
      ?system
      ~prompt:"hello"
      ()
  in
  match args with
  | Types.Glm { model; system_prompt; _ } ->
      check string "glm model" "glm-5" model;
      check (option string) "glm system_prompt" system system_prompt
  | _ ->
      fail "expected Types.Glm"

let () =
  run "chain_system_prompt" [
    ("chain_llm_args", [
      test_case "ollama default model system_prompt" `Quick test_chain_llm_args_ollama_system_prompt;
      test_case "ollama:MODEL system_prompt" `Quick test_chain_llm_args_ollama_model_system_prompt;
      test_case "claude system_prompt" `Quick test_chain_llm_args_claude_system_prompt;
      test_case "glm system_prompt" `Quick test_chain_llm_args_glm_system_prompt;
    ]);
  ]

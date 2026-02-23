(** Tests for pure helper functions in Tools_eio.

    Targets: env_truthy, with_system_prompt, chain_llm_args (branches),
    validate_chain, masc_enabled, masc_agent_base, masc_heartbeat_interval. *)

open Alcotest

(* --- env_truthy --- *)

let test_env_truthy_1 () = check bool "1" true (Tools_eio.env_truthy "1")
let test_env_truthy_true () = check bool "true" true (Tools_eio.env_truthy "true")
let test_env_truthy_yes () = check bool "yes" true (Tools_eio.env_truthy "yes")
let test_env_truthy_on () = check bool "on" true (Tools_eio.env_truthy "on")
let test_env_truthy_0 () = check bool "0" false (Tools_eio.env_truthy "0")
let test_env_truthy_false () = check bool "false" false (Tools_eio.env_truthy "false")
let test_env_truthy_no () = check bool "no" false (Tools_eio.env_truthy "no")
let test_env_truthy_off () = check bool "off" false (Tools_eio.env_truthy "off")
let test_env_truthy_random () = check bool "random" false (Tools_eio.env_truthy "maybe")
let test_env_truthy_empty () = check bool "empty" false (Tools_eio.env_truthy "")

let test_env_truthy_case () =
  check bool "TRUE" true (Tools_eio.env_truthy "TRUE");
  check bool "True" true (Tools_eio.env_truthy "True");
  check bool "YES" true (Tools_eio.env_truthy "YES");
  check bool "ON" true (Tools_eio.env_truthy "ON")

(* --- with_system_prompt --- *)

let test_wsp_none () =
  let args = Types.Gemini {
    prompt = "t"; model = "gemini"; thinking_level = Types.Low;
    yolo = false; output_format = Types.Text; timeout = 30;
    stream = false; use_cli = false; fallback_to_api = false;
  } in
  check bool "unchanged" true (Tools_eio.with_system_prompt None args = args)

let test_wsp_claude () =
  let args = Types.Claude {
    prompt = "t"; model = "claude"; long_context = false;
    system_prompt = None; output_format = Types.Text;
    allowed_tools = []; working_directory = "/tmp"; timeout = 30;
    stream = false; use_cli = false; fallback_to_api = false; api_key = None;
  } in
  match Tools_eio.with_system_prompt (Some "sys") args with
  | Types.Claude c -> check (option string) "set" (Some "sys") c.system_prompt
  | _ -> fail "expected Claude"

let test_wsp_ollama () =
  let args = Types.Ollama {
    prompt = "t"; model = "q"; system_prompt = None;
    temperature = 0.7; timeout = 30; stream = false; tools = None;
  } in
  match Tools_eio.with_system_prompt (Some "sys") args with
  | Types.Ollama o -> check (option string) "set" (Some "sys") o.system_prompt
  | _ -> fail "expected Ollama"

let test_wsp_glm () =
  let args = Types.Glm {
    prompt = "t"; model = "glm-5"; system_prompt = None;
    modality = "text"; cascade = false; cascade_models = None; min_context_tokens = None;
    temperature = 0.7; max_tokens = Some 4096; timeout = 30;
    stream = false; thinking = false; do_sample = true;
    web_search = false; tools = []; api_key = None;
  } in
  match Tools_eio.with_system_prompt (Some "sys") args with
  | Types.Glm g -> check (option string) "set" (Some "sys") g.system_prompt
  | _ -> fail "expected Glm"

let test_wsp_gemini_ignored () =
  let args = Types.Gemini {
    prompt = "t"; model = "gemini"; thinking_level = Types.Low;
    yolo = false; output_format = Types.Text; timeout = 30;
    stream = false; use_cli = false; fallback_to_api = false;
  } in
  check bool "unchanged" true (Tools_eio.with_system_prompt (Some "x") args = args)

let test_wsp_codex_ignored () =
  let args = Types.Codex {
    prompt = "t"; model = "gpt-5.2"; reasoning_effort = Types.RXhigh;
    sandbox = Types.WorkspaceWrite; working_directory = None;
    timeout = 30; stream = false; use_cli = true; fallback_to_api = true;
  } in
  check bool "unchanged" true (Tools_eio.with_system_prompt (Some "x") args = args)

(* --- chain_llm_args branches --- *)

let test_cla_stub () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"stub" ~prompt:"t" () with
  | Types.Gemini g -> check string "model" "stub" g.model
  | _ -> fail "stub->Gemini"

let test_cla_mock () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"mock" ~prompt:"t" () with
  | Types.Gemini g -> check string "model" "stub" g.model
  | _ -> fail "mock->Gemini"

let test_cla_gemini () =
  match Tools_eio.chain_llm_args ~timeout:60 ~gemini_use_cli:true
    ~parsed_tools:None ~model:"gemini" ~prompt:"t" () with
  | Types.Gemini g ->
    check string "model" "gemini" g.model;
    check bool "use_cli" true g.use_cli;
    check int "timeout" 60 g.timeout
  | _ -> fail "expected Gemini"

let test_cla_flash () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"flash" ~prompt:"t" () with
  | Types.Gemini _ -> check pass "flash->Gemini" () ()
  | _ -> fail "expected Gemini"

let test_cla_claude () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"claude" ~prompt:"t" () with
  | Types.Claude c ->
    check string "model" "claude" c.model;
    check bool "long_context" true c.long_context
  | _ -> fail "expected Claude"

let test_cla_codex () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"codex" ~prompt:"t" () with
  | Types.Codex c -> check string "model" "gpt-5.2" c.model
  | _ -> fail "expected Codex"

let test_cla_ollama () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"ollama" ~prompt:"t" () with
  | Types.Ollama o -> check string "model" "qwen3:1.7b" o.model
  | _ -> fail "expected Ollama"

let test_cla_ollama_specific () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"ollama:deepseek-r1:32b" ~prompt:"t" () with
  | Types.Ollama o -> check string "model" "deepseek-r1:32b" o.model
  | _ -> fail "expected Ollama"

let test_cla_glm () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"glm" ~prompt:"t" () with
  | Types.Glm g -> check string "model" "glm-5" g.model
  | _ -> fail "expected Glm"

let test_cla_glm47 () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"glm-4.7" ~prompt:"t" () with
  | Types.Glm g -> check string "model" "glm-4.7" g.model
  | _ -> fail "expected Glm"

let test_cla_unknown () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"xyz" ~prompt:"t" () with
  | Types.Gemini g -> check string "model" "gemini-3.1-pro-preview" g.model
  | _ -> fail "unknown->Gemini"

let test_cla_with_system () =
  let system = Some "Be concise" in
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"claude" ?system ~prompt:"t" () with
  | Types.Claude c -> check (option string) "sys" (Some "Be concise") c.system_prompt
  | _ -> fail "expected Claude"

let test_cla_with_tools () =
  let tools = Some [Types.{ name = "s"; description = "d"; input_schema = `Null }] in
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:tools ~model:"ollama" ~prompt:"t" () with
  | Types.Ollama o -> check bool "tools" true (o.tools <> None)
  | _ -> fail "expected Ollama"

let test_cla_opus () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"opus" ~prompt:"t" () with
  | Types.Claude _ -> check pass "opus->Claude" () ()
  | _ -> fail "expected Claude"

let test_cla_haiku () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"haiku" ~prompt:"t" () with
  | Types.Claude _ -> check pass "haiku->Claude" () ()
  | _ -> fail "expected Claude"

let test_cla_pro () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"pro" ~prompt:"t" () with
  | Types.Gemini _ -> check pass "pro->Gemini" () ()
  | _ -> fail "expected Gemini"

let test_cla_3flash () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"3-flash" ~prompt:"t" () with
  | Types.Gemini _ -> check pass "3-flash->Gemini" () ()
  | _ -> fail "expected Gemini"

let test_cla_gemini_dash () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"gemini-2.5-flash" ~prompt:"t" () with
  | Types.Gemini _ -> check pass "gemini-2.5-flash" () ()
  | _ -> fail "expected Gemini"

let test_cla_gpt52 () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"gpt-5.2" ~prompt:"t" () with
  | Types.Codex _ -> check pass "gpt-5.2->Codex" () ()
  | _ -> fail "expected Codex"

let test_cla_sonnet () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"sonnet" ~prompt:"t" () with
  | Types.Claude _ -> check pass "sonnet->Claude" () ()
  | _ -> fail "expected Claude"

let test_cla_flash_lite () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"flash-lite" ~prompt:"t" () with
  | Types.Gemini _ -> check pass "flash-lite->Gemini" () ()
  | _ -> fail "expected Gemini"

let test_cla_glm45 () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"glm-4.5" ~prompt:"t" () with
  | Types.Glm g -> check string "model" "glm-4.5" g.model
  | _ -> fail "expected Glm"

let test_cla_glm46 () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"glm-4.6" ~prompt:"t" () with
  | Types.Glm g -> check string "model" "glm-4.6" g.model
  | _ -> fail "expected Glm"

let test_cla_haiku45 () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"haiku-4.5" ~prompt:"t" () with
  | Types.Claude _ -> check pass "haiku-4.5->Claude" () ()
  | _ -> fail "expected Claude"

let test_cla_opus4 () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"opus-4" ~prompt:"t" () with
  | Types.Claude _ -> check pass "opus-4->Claude" () ()
  | _ -> fail "expected Claude"

let test_cla_3pro () =
  match Tools_eio.chain_llm_args ~timeout:30 ~gemini_use_cli:false
    ~parsed_tools:None ~model:"3-pro" ~prompt:"t" () with
  | Types.Gemini _ -> check pass "3-pro->Gemini" () ()
  | _ -> fail "expected Gemini"

(* --- glm cascade helpers --- *)

let test_glm_alias_normalize () =
  check (option string) "5-coder -> glm-5" (Some "glm-5")
    (Tools_eio.normalize_glm_model_alias "5-coder");
  check (option string) "glm -> glm-5" (Some "glm-5")
    (Tools_eio.normalize_glm_model_alias "glm")

let test_glm_resolve_no_cascade () =
  let models =
    Tools_eio.resolve_glm_models_for_call
      ~model:"glm" ~modality:"text" ~cascade:false
      ~cascade_models:None ~min_context_tokens:None
  in
  check (list string) "single model" [ "glm-5" ] models

let test_glm_resolve_text_cascade_default () =
  let models =
    Tools_eio.resolve_glm_models_for_call
      ~model:"glm-5" ~modality:"text" ~cascade:true
      ~cascade_models:None ~min_context_tokens:(Some 200000)
  in
  check bool "has 4.7" true (List.mem "glm-4.7" models);
  check bool "no 5-code in default cascade" false (List.mem "glm-5-code" models);
  check bool "filters out 4.5 (128k)" false (List.mem "glm-4.5" models)

let test_glm_resolve_text_cascade_filter () =
  let models =
    Tools_eio.resolve_glm_models_for_call
      ~model:"glm-5" ~modality:"text" ~cascade:true
      ~cascade_models:(Some [ "glm-4.5"; "glm-4.7"; "glm-5" ])
      ~min_context_tokens:(Some 200000)
  in
  check (list string) "200k filtered models" [ "glm-5"; "glm-4.7" ] models

let test_glm_resolve_image_cascade () =
  let models =
    Tools_eio.resolve_glm_models_for_call
      ~model:"glm-image" ~modality:"image" ~cascade:true
      ~cascade_models:None ~min_context_tokens:(Some 200000)
  in
  check (list string) "no implicit non-text fallback" [ "glm-image" ] models

let test_glm_runtime_modality_support () =
  check bool "text is supported" true
    (Tools_eio.glm_runtime_supports_modality "text");
  check bool "image not yet supported" false
    (Tools_eio.glm_runtime_supports_modality "image");
  check bool "video not yet supported" false
    (Tools_eio.glm_runtime_supports_modality "video")

(* --- validate_chain --- *)

let test_validate_valid () =
  let chain_json = `Assoc [
    ("id", `String "test");
    ("nodes", `List [
      `Assoc [
        ("id", `String "a"); ("type", `String "llm");
        ("model", `String "stub"); ("prompt", `String "t");
      ];
    ]);
    ("output", `String "a");
  ] in
  let r = Tools_eio.validate_chain ~chain_json in
  check int "rc 0" 0 r.returncode

let test_validate_invalid () =
  let r = Tools_eio.validate_chain ~chain_json:(`String "bad") in
  check bool "rc!=0" true (r.returncode <> 0)

let test_validate_missing_output () =
  let chain_json = `Assoc [
    ("id", `String "bad");
    ("nodes", `List [
      `Assoc [
        ("id", `String "a"); ("type", `String "llm");
        ("model", `String "stub"); ("prompt", `String "t");
      ];
    ]);
    ("output", `String "nonexistent");
  ] in
  let r = Tools_eio.validate_chain ~chain_json in
  check bool "rc!=0" true (r.returncode <> 0)

(* --- masc helpers --- *)

let test_masc_enabled_default () =
  let saved = Sys.getenv_opt "LLM_MCP_MASC_HOOK" in
  Unix.putenv "LLM_MCP_MASC_HOOK" "";
  let r = Tools_eio.masc_enabled () in
  (match saved with Some v -> Unix.putenv "LLM_MCP_MASC_HOOK" v | None -> ());
  check bool "disabled" false r

let test_masc_enabled_true () =
  let saved = Sys.getenv_opt "LLM_MCP_MASC_HOOK" in
  Unix.putenv "LLM_MCP_MASC_HOOK" "true";
  let r = Tools_eio.masc_enabled () in
  (match saved with Some v -> Unix.putenv "LLM_MCP_MASC_HOOK" v | None -> Unix.putenv "LLM_MCP_MASC_HOOK" "");
  check bool "enabled" true r

let test_masc_agent_custom () =
  let saved = Sys.getenv_opt "LLM_MCP_MASC_AGENT" in
  Unix.putenv "LLM_MCP_MASC_AGENT" "my-agent";
  let r = Tools_eio.masc_agent_base () in
  (match saved with Some v -> Unix.putenv "LLM_MCP_MASC_AGENT" v | None -> Unix.putenv "LLM_MCP_MASC_AGENT" "");
  check string "custom" "my-agent" r

let test_masc_heartbeat_default () =
  let saved = Sys.getenv_opt "LLM_MCP_MASC_HEARTBEAT_SEC" in
  Unix.putenv "LLM_MCP_MASC_HEARTBEAT_SEC" "";
  let r = Tools_eio.masc_heartbeat_interval () in
  (match saved with Some v -> Unix.putenv "LLM_MCP_MASC_HEARTBEAT_SEC" v | None -> ());
  check int "30" 30 r

let test_masc_heartbeat_custom () =
  let saved = Sys.getenv_opt "LLM_MCP_MASC_HEARTBEAT_SEC" in
  Unix.putenv "LLM_MCP_MASC_HEARTBEAT_SEC" "60";
  let r = Tools_eio.masc_heartbeat_interval () in
  (match saved with Some v -> Unix.putenv "LLM_MCP_MASC_HEARTBEAT_SEC" v | None -> Unix.putenv "LLM_MCP_MASC_HEARTBEAT_SEC" "");
  check int "60" 60 r

(* --- Test Suite --- *)

let () =
  run "tools_eio_helpers" [
    ("env_truthy", [
      test_case "1" `Quick test_env_truthy_1;
      test_case "true" `Quick test_env_truthy_true;
      test_case "yes" `Quick test_env_truthy_yes;
      test_case "on" `Quick test_env_truthy_on;
      test_case "0" `Quick test_env_truthy_0;
      test_case "false" `Quick test_env_truthy_false;
      test_case "no" `Quick test_env_truthy_no;
      test_case "off" `Quick test_env_truthy_off;
      test_case "random" `Quick test_env_truthy_random;
      test_case "empty" `Quick test_env_truthy_empty;
      test_case "case" `Quick test_env_truthy_case;
    ]);
    ("with_system_prompt", [
      test_case "None" `Quick test_wsp_none;
      test_case "Claude" `Quick test_wsp_claude;
      test_case "Ollama" `Quick test_wsp_ollama;
      test_case "Glm" `Quick test_wsp_glm;
      test_case "Gemini ignored" `Quick test_wsp_gemini_ignored;
      test_case "Codex ignored" `Quick test_wsp_codex_ignored;
    ]);
    ("chain_llm_args", [
      test_case "stub" `Quick test_cla_stub;
      test_case "mock" `Quick test_cla_mock;
      test_case "gemini" `Quick test_cla_gemini;
      test_case "flash" `Quick test_cla_flash;
      test_case "claude" `Quick test_cla_claude;
      test_case "codex" `Quick test_cla_codex;
      test_case "ollama" `Quick test_cla_ollama;
      test_case "ollama:specific" `Quick test_cla_ollama_specific;
      test_case "glm" `Quick test_cla_glm;
      test_case "glm-4.7" `Quick test_cla_glm47;
      test_case "unknown" `Quick test_cla_unknown;
      test_case "with system" `Quick test_cla_with_system;
      test_case "with tools" `Quick test_cla_with_tools;
      test_case "opus" `Quick test_cla_opus;
      test_case "haiku" `Quick test_cla_haiku;
      test_case "pro" `Quick test_cla_pro;
      test_case "3-flash" `Quick test_cla_3flash;
      test_case "gemini-2.5-flash" `Quick test_cla_gemini_dash;
      test_case "gpt-5.2" `Quick test_cla_gpt52;
      test_case "sonnet" `Quick test_cla_sonnet;
      test_case "flash-lite" `Quick test_cla_flash_lite;
      test_case "glm-4.5" `Quick test_cla_glm45;
      test_case "glm-4.6" `Quick test_cla_glm46;
      test_case "haiku-4.5" `Quick test_cla_haiku45;
      test_case "opus-4" `Quick test_cla_opus4;
      test_case "3-pro" `Quick test_cla_3pro;
    ]);
    ("glm_cascade_helpers", [
      test_case "alias normalize" `Quick test_glm_alias_normalize;
      test_case "no cascade" `Quick test_glm_resolve_no_cascade;
      test_case "text cascade default" `Quick test_glm_resolve_text_cascade_default;
      test_case "text cascade filter" `Quick test_glm_resolve_text_cascade_filter;
      test_case "image cascade" `Quick test_glm_resolve_image_cascade;
      test_case "runtime modality support" `Quick test_glm_runtime_modality_support;
    ]);
    ("validate_chain", [
      test_case "valid" `Quick test_validate_valid;
      test_case "invalid" `Quick test_validate_invalid;
      test_case "missing output" `Quick test_validate_missing_output;
    ]);
    ("masc", [
      test_case "enabled default" `Quick test_masc_enabled_default;
      test_case "enabled true" `Quick test_masc_enabled_true;
      test_case "agent custom" `Quick test_masc_agent_custom;
      test_case "heartbeat default" `Quick test_masc_heartbeat_default;
      test_case "heartbeat custom" `Quick test_masc_heartbeat_custom;
    ]);
  ]

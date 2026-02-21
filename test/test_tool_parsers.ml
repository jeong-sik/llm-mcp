(** Test Tool_parsers - Pure function unit tests

    Independent tests for Tool_parsers module.
    These tests do NOT require Lwt or Eio - pure OCaml only.
*)

open Alcotest
open Types

let with_env name value f =
  let old = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match old with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f

(* {1Parse Gemini Args Tests} *)

let test_parse_gemini_defaults () =
  let json = `Assoc [("prompt", `String "Hello world")] in
  match Tool_parsers.parse_gemini_args json with
  | Gemini g ->
      check string "default model" "gemini-3.1-pro-preview" g.model;
      check bool "default yolo" false g.yolo;
      check int "default timeout" 300 g.timeout;
      check bool "default stream" true g.stream;  (* default is true *)
      check string "prompt" "Hello world" g.prompt
  | _ -> fail "Expected Gemini variant"

let test_parse_gemini_custom_values () =
  let json = `Assoc [
    ("prompt", `String "Test prompt");
    ("model", `String "gemini-2.5-flash");
    ("thinking_level", `String "low");
    ("yolo", `Bool true);
    ("timeout", `Int 60);
    ("stream", `Bool true);
  ] in
  match Tool_parsers.parse_gemini_args json with
  | Gemini g ->
      check string "custom model" "gemini-2.5-flash" g.model;
      check bool "yolo true" true g.yolo;
      check int "custom timeout" 60 g.timeout;
      check bool "stream true" true g.stream;
      (match g.thinking_level with
       | Low -> ()
       | High -> fail "Expected Low thinking level")
  | _ -> fail "Expected Gemini variant"

let test_parse_gemini_thinking_levels () =
  (* Test "high" thinking level *)
  let json_high = `Assoc [
    ("prompt", `String "test");
    ("thinking_level", `String "high");
  ] in
  (match Tool_parsers.parse_gemini_args json_high with
   | Gemini g ->
       (match g.thinking_level with
        | High -> ()
        | Low -> fail "Expected High thinking level")
   | _ -> fail "Expected Gemini variant");

  (* Test "low" thinking level *)
  let json_low = `Assoc [
    ("prompt", `String "test");
    ("thinking_level", `String "low");
  ] in
  (match Tool_parsers.parse_gemini_args json_low with
   | Gemini g ->
       (match g.thinking_level with
       | Low -> ()
       | High -> fail "Expected Low thinking level")
   | _ -> fail "Expected Gemini variant")

let test_parse_gemini_use_cli_default_prefers_api_key () =
  let json = `Assoc [("prompt", `String "Hello world")] in

  with_env "GEMINI_API_KEY" "dummy-key" (fun () ->
    with_env "GOOGLE_AI_API_KEY" "" (fun () ->
      match Tool_parsers.parse_gemini_args json with
      | Gemini g ->
          check bool "use_cli defaults to false when GEMINI_API_KEY is set" false g.use_cli
      | _ -> fail "Expected Gemini variant"));

  let json_explicit = `Assoc [("prompt", `String "Hello world"); ("use_cli", `Bool true)] in
  with_env "GEMINI_API_KEY" "dummy-key" (fun () ->
    match Tool_parsers.parse_gemini_args json_explicit with
    | Gemini g ->
        check bool "explicit use_cli=true is respected even when API key is set" true g.use_cli
    | _ -> fail "Expected Gemini variant");

  (* Alias support: GOOGLE_AI_API_KEY should behave the same as GEMINI_API_KEY *)
  with_env "GEMINI_API_KEY" "" (fun () ->
    with_env "GOOGLE_AI_API_KEY" "dummy-key" (fun () ->
      match Tool_parsers.parse_gemini_args json with
      | Gemini g ->
          check bool "use_cli defaults to false when GOOGLE_AI_API_KEY is set" false g.use_cli
      | _ -> fail "Expected Gemini variant"))

(* {1Parse Gemini List Args Tests} *)

let test_parse_gemini_list_defaults () =
  let json = `Assoc [] in
  match Tool_parsers.parse_gemini_list_args json with
  | GeminiList g ->
      check (option string) "default filter" None g.filter;
      check bool "default include_all" false g.include_all
  | _ -> fail "Expected GeminiList variant"

let test_parse_gemini_list_custom_values () =
  let json = `Assoc [
    ("filter", `String "flash");
    ("include_all", `Bool true);
  ] in
  match Tool_parsers.parse_gemini_list_args json with
  | GeminiList g ->
      check (option string) "custom filter" (Some "flash") g.filter;
      check bool "include_all true" true g.include_all
  | _ -> fail "Expected GeminiList variant"

(* {1Parse Chain Run Args Tests} *)

let test_parse_chain_run_chain_id_only () =
  let json = `Assoc [
    ("chain_id", `String "simple-test");
    ("input", `Assoc [("foo", `Int 1)]);
  ] in
  match Tool_parsers.parse_chain_run_args json with
  | ChainRun r ->
      check (option string) "chain none" None (Option.map (fun _ -> "present") r.chain);
      check (option string) "mermaid none" None r.mermaid;
      check (option string) "chain_id" (Some "simple-test") r.chain_id;
      check (option string) "input serialized" (Some "{\"foo\":1}") r.input;
      check bool "trace default false" false r.trace;
      check bool "checkpoint default false" false r.checkpoint_enabled;
      check (option int) "timeout default none" None r.timeout
  | _ -> fail "Expected ChainRun variant"

(* {1Parse Claude Args Tests} *)

let test_parse_claude_defaults () =
  let json = `Assoc [("prompt", `String "Hello Claude")] in
  match Tool_parsers.parse_claude_args json with
  | Claude c ->
      check string "default model" "sonnet" c.model;  (* changed from opus - sonnet is cost-effective default *)
      check bool "default long_context" false c.long_context;  (* default: false to avoid API charges *)
      check int "default timeout" 300 c.timeout;
      check bool "default stream" true c.stream;  (* default is true *)
      check string "prompt" "Hello Claude" c.prompt
  | _ -> fail "Expected Claude variant"

let test_parse_claude_custom_values () =
  let json = `Assoc [
    ("prompt", `String "Test");
    ("model", `String "sonnet");
    ("long_context", `Bool false);
    ("system_prompt", `String "You are a helpful assistant");
    ("output_format", `String "json");
    ("timeout", `Int 120);
    ("stream", `Bool true);
  ] in
  match Tool_parsers.parse_claude_args json with
  | Claude c ->
      check string "custom model" "sonnet" c.model;
      check bool "long_context false" false c.long_context;
      check (option string) "system prompt" (Some "You are a helpful assistant") c.system_prompt;
      check int "custom timeout" 120 c.timeout;
      check bool "stream true" true c.stream;
      (match c.output_format with
       | Json -> ()
       | Text | StreamJson -> fail "Expected Json output format")
  | _ -> fail "Expected Claude variant"

let test_parse_claude_output_formats () =
  let test_format format_str expected_name =
    let json = `Assoc [
      ("prompt", `String "test");
      ("output_format", `String format_str);
    ] in
    match Tool_parsers.parse_claude_args json with
    | Claude c ->
        let format_name = match c.output_format with
          | Text -> "text"
          | Json -> "json"
          | StreamJson -> "stream-json"
        in
        check string ("format " ^ format_str) expected_name format_name
    | _ -> fail "Expected Claude variant"
  in
  test_format "text" "text";
  test_format "json" "json";
  test_format "stream-json" "stream-json"

(* {1Parse Codex Args Tests} *)

let test_parse_codex_defaults () =
  let json = `Assoc [("prompt", `String "Hello Codex")] in
  match Tool_parsers.parse_codex_args json with
  | Codex c ->
      check string "default model" "gpt-5.2" c.model;  (* default is gpt-5.2 *)
      check int "default timeout" 300 c.timeout;
      check bool "default stream" true c.stream;  (* default is true *)
      check string "prompt" "Hello Codex" c.prompt;
      (* Check sandbox default *)
      (match c.sandbox with
       | WorkspaceWrite -> ()
       | ReadOnly | DangerFullAccess -> fail "Expected WorkspaceWrite sandbox")
  | _ -> fail "Expected Codex variant"

let test_parse_codex_reasoning_efforts () =
  let test_effort effort_str =
    let json = `Assoc [
      ("prompt", `String "test");
      ("reasoning_effort", `String effort_str);
    ] in
    match Tool_parsers.parse_codex_args json with
    | Codex c ->
        let effort_name = match c.reasoning_effort with
          | RLow -> "low"
          | RMedium -> "medium"
          | RHigh -> "high"
          | RXhigh -> "xhigh"
        in
        check string ("effort " ^ effort_str) effort_str effort_name
    | _ -> fail "Expected Codex variant"
  in
  test_effort "low";
  test_effort "medium";
  test_effort "high";
  test_effort "xhigh"

let test_parse_codex_sandbox_modes () =
  let test_sandbox sandbox_str expected =
    let json = `Assoc [
      ("prompt", `String "test");
      ("sandbox", `String sandbox_str);
    ] in
    match Tool_parsers.parse_codex_args json with
    | Codex c ->
        let sandbox_name = match c.sandbox with
          | ReadOnly -> "read-only"
          | WorkspaceWrite -> "workspace-write"
          | DangerFullAccess -> "danger-full-access"
        in
        check string ("sandbox " ^ sandbox_str) expected sandbox_name
    | _ -> fail "Expected Codex variant"
  in
  test_sandbox "read-only" "read-only";
  test_sandbox "workspace-write" "workspace-write";
  test_sandbox "danger-full-access" "danger-full-access"

(* {1Parse Ollama Args Tests} *)

let test_parse_ollama_defaults () =
  let json = `Assoc [("prompt", `String "Hello Ollama")] in
  match Tool_parsers.parse_ollama_args json with
  | Ollama o ->
      check string "default model" "devstral" o.model;
      check (option string) "default system_prompt" None o.system_prompt;
      check int "default timeout" 300 o.timeout;
      check bool "default stream" true o.stream;  (* default is true *)
      check string "prompt" "Hello Ollama" o.prompt
  | _ -> fail "Expected Ollama variant"

let test_parse_ollama_custom_values () =
  let json = `Assoc [
    ("prompt", `String "Test");
    ("model", `String "qwen3-coder:30b");
    ("system_prompt", `String "You are a coding expert");
    ("temperature", `Float 0.8);
    ("timeout", `Int 600);
    ("stream", `Bool true);
  ] in
  match Tool_parsers.parse_ollama_args json with
  | Ollama o ->
      check string "custom model" "qwen3-coder:30b" o.model;
      check (option string) "system prompt" (Some "You are a coding expert") o.system_prompt;
      check int "custom timeout" 600 o.timeout;
      check bool "stream true" true o.stream
  | _ -> fail "Expected Ollama variant"

(* {1Budget Mode Tests} *)

let test_budget_mode_true () =
  let json = `Assoc [("budget_mode", `Bool true)] in
  check bool "budget_mode true" true (Tool_parsers.budget_mode_value json)

let test_budget_mode_false () =
  let json = `Assoc [("budget_mode", `Bool false)] in
  check bool "budget_mode false" false (Tool_parsers.budget_mode_value json)

let test_budget_mode_default () =
  let json = `Assoc [] in
  (* Default depends on LLM_MCP_BUDGET_MODE env var, but without it should be false *)
  let _ = Tool_parsers.budget_mode_value json in
  (* Just check it doesn't crash *)
  ()

(* {1Build Command Tests} *)

let test_build_gemini_cmd () =
  let args = Gemini {
    prompt = "Hello";
    model = "gemini-2.5-flash";
    thinking_level = Low;
    yolo = false;
    output_format = Text;
    timeout = 60;
    stream = false;
    use_cli = true;
    fallback_to_api = true;
  } in
  match Tool_parsers.build_gemini_cmd args with
  | Ok cmd_list ->
      check bool "cmd starts with gemini" true (List.hd cmd_list = "gemini");
      check bool "has model flag" true (List.mem "-m" cmd_list);
      check bool "has model value" true (List.mem "gemini-2.5-flash" cmd_list);
      check bool "has prompt flag" true (List.mem "-p" cmd_list)
  | Error e -> fail ("build_gemini_cmd failed: " ^ e)

let test_build_gemini_cmd_with_yolo () =
  let args = Gemini {
    prompt = "Test";
    model = "gemini-3.1-pro-preview";
    thinking_level = High;
    yolo = true;
    output_format = Text;
    timeout = 300;
    stream = false;
    use_cli = true;
    fallback_to_api = true;
  } in
  match Tool_parsers.build_gemini_cmd args with
  | Ok cmd_list ->
      check bool "has yolo flag" true (List.mem "--yolo" cmd_list)
  | Error e -> fail ("build_gemini_cmd failed: " ^ e)

let test_build_claude_cmd () =
  let args = Claude {
    prompt = "Hello";
    model = "sonnet";
    long_context = false;
    system_prompt = None;
    output_format = Text;
    allowed_tools = [];
    working_directory = "";
    timeout = 60;
    stream = false;
    use_cli = true;
    fallback_to_api = true;
    api_key = None;
  } in
  match Tool_parsers.build_claude_cmd args with
  | Ok cmd_list ->
      (* First element is wrapper script path ending with claude-wrapper.sh *)
      let first = List.hd cmd_list in
      check bool "cmd starts with wrapper" true
        (String.ends_with ~suffix:"claude-wrapper.sh" first);
      check bool "has print flag" true (List.mem "-p" cmd_list);
      check bool "has model flag" true (List.mem "--model" cmd_list)
  | Error e -> fail ("build_claude_cmd failed: " ^ e)

let test_build_claude_cmd_with_long_context () =
  Unix.putenv "ANTHROPIC_API_KEY" "test-key";
  let args = Claude {
    prompt = "Test";
    model = "opus";
    long_context = true;
    system_prompt = Some "Be helpful";
    output_format = Json;
    allowed_tools = ["Read"; "Write"];
    working_directory = "/tmp";
    timeout = 300;
    stream = false;
    use_cli = true;
    fallback_to_api = true;
    api_key = None;
  } in
  match Tool_parsers.build_claude_cmd args with
  | Ok cmd_list ->
      (* long_context is implemented via --betas flag *)
      check bool "has betas flag" true (List.mem "--betas" cmd_list);
      check bool "has output format" true (List.mem "--output-format" cmd_list);
      check bool "has json format" true (List.mem "json" cmd_list)
  | Error e -> fail ("build_claude_cmd failed: " ^ e)

let test_build_codex_cmd () =
  let args = Codex {
    prompt = "Hello";
    model = "gpt-5.2";
    reasoning_effort = RHigh;
    sandbox = WorkspaceWrite;
    working_directory = None;
    timeout = 60;
    stream = false;
    use_cli = true;
    fallback_to_api = true;
  } in
  match Tool_parsers.build_codex_cmd args with
  | Ok cmd_list ->
      check bool "cmd starts with codex" true (List.hd cmd_list = "codex");
      check bool "has exec" true (List.mem "exec" cmd_list)
  | Error e -> fail ("build_codex_cmd failed: " ^ e)

let test_build_ollama_curl_cmd () =
  let args = Ollama {
    prompt = "Hello";
    model = "devstral";
    system_prompt = None;
    temperature = 0.7;
    timeout = 60;
    stream = false;
    tools = None;
  } in
  match Tool_parsers.build_ollama_curl_cmd args with
  | Ok cmd_list ->
      check bool "cmd starts with curl" true (List.hd cmd_list = "curl");
      check bool "has silent flag" true (List.mem "-s" cmd_list);
      check bool "has data flag" true (List.mem "-d" cmd_list);
      check bool "has localhost url" true
        (List.exists (fun s -> String.length s >= 21 &&
          String.sub s 0 21 = "http://localhost:1143") cmd_list)
  | Error e -> fail ("build_ollama_curl_cmd failed: " ^ e)

(* {1Thinking Prompt Prefix Test} *)

let test_thinking_prompt_prefix () =
  (* High thinking level should have a non-empty prefix *)
  let prefix_high = Tool_parsers.thinking_prompt_prefix High in
  check bool "High prefix is non-empty" true (String.length prefix_high > 0);
  (* Low thinking level should have empty prefix *)
  let prefix_low = Tool_parsers.thinking_prompt_prefix Low in
  check bool "Low prefix is empty" true (String.length prefix_low = 0)

(* {1Clean Codex Output Tests} *)

let test_clean_codex_output_simple () =
  let output = Tool_parsers.clean_codex_output "Hello world" in
  check string "simple output unchanged" "Hello world" output

let test_clean_codex_output_with_prefix () =
  (* When input has "codex" on first line, function skips that line and takes rest.
     For single-line input containing "codex", result is empty, so original is returned. *)
  let input = "codex> Some output\nActual content here" in
  let output = Tool_parsers.clean_codex_output input in
  check string "line after codex marker returned" "Actual content here" output

let test_clean_codex_output_multiline () =
  let input = "codex> Line 1\nLine 2\ncodex> Line 3" in
  let output = Tool_parsers.clean_codex_output input in
  check bool "multiline processed" true (String.length output > 0)

(* {1Tool Schema Conversion Tests} *)

let test_tool_schema_to_ollama_tool () =
  let schema : tool_schema = {
    name = "test_tool";
    description = "A test tool";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("arg1", `Assoc [("type", `String "string")]);
      ]);
    ];
  } in
  let ollama_tool = Tool_parsers.tool_schema_to_ollama_tool schema in
  match ollama_tool with
  | `Assoc items ->
      check bool "has type" true (List.mem_assoc "type" items);
      check bool "has function" true (List.mem_assoc "function" items)
  | _ -> fail "Expected Assoc"

(* {1Test Suite} *)

let () =
  Alcotest.run "Tool_parsers" [
    "parse_gemini_args", [
      test_case "defaults" `Quick test_parse_gemini_defaults;
      test_case "custom values" `Quick test_parse_gemini_custom_values;
      test_case "thinking levels" `Quick test_parse_gemini_thinking_levels;
      test_case "use_cli default prefers api key" `Quick test_parse_gemini_use_cli_default_prefers_api_key;
    ];

    "parse_gemini_list_args", [
      test_case "defaults" `Quick test_parse_gemini_list_defaults;
      test_case "custom values" `Quick test_parse_gemini_list_custom_values;
    ];

    "parse_chain_run_args", [
      test_case "chain_id only" `Quick test_parse_chain_run_chain_id_only;
    ];

    "parse_claude_args", [
      test_case "defaults" `Quick test_parse_claude_defaults;
      test_case "custom values" `Quick test_parse_claude_custom_values;
      test_case "output formats" `Quick test_parse_claude_output_formats;
    ];

    "parse_codex_args", [
      test_case "defaults" `Quick test_parse_codex_defaults;
      test_case "reasoning efforts" `Quick test_parse_codex_reasoning_efforts;
      test_case "sandbox modes" `Quick test_parse_codex_sandbox_modes;
    ];

    "parse_ollama_args", [
      test_case "defaults" `Quick test_parse_ollama_defaults;
      test_case "custom values" `Quick test_parse_ollama_custom_values;
    ];

    "budget_mode", [
      test_case "true" `Quick test_budget_mode_true;
      test_case "false" `Quick test_budget_mode_false;
      test_case "default" `Quick test_budget_mode_default;
    ];

    "build_gemini_cmd", [
      test_case "basic" `Quick test_build_gemini_cmd;
      test_case "with yolo" `Quick test_build_gemini_cmd_with_yolo;
    ];

    "build_claude_cmd", [
      test_case "basic" `Quick test_build_claude_cmd;
      test_case "with long_context" `Quick test_build_claude_cmd_with_long_context;
    ];

    "build_codex_cmd", [
      test_case "basic" `Quick test_build_codex_cmd;
    ];

    "build_ollama_curl_cmd", [
      test_case "basic" `Quick test_build_ollama_curl_cmd;
    ];

    "thinking_prompt_prefix", [
      test_case "format" `Quick test_thinking_prompt_prefix;
    ];

    "clean_codex_output", [
      test_case "simple" `Quick test_clean_codex_output_simple;
      test_case "with prefix" `Quick test_clean_codex_output_with_prefix;
      test_case "multiline" `Quick test_clean_codex_output_multiline;
    ];

    "tool_schema_conversion", [
      test_case "to ollama tool" `Quick test_tool_schema_to_ollama_tool;
    ];

    (* {1Resolve model aliases} *)

    "resolve_claude_model", [
      test_case "claude alias" `Quick (fun () ->
        check string "claude" "claude-3-5-sonnet-20241022" (Tool_parsers.resolve_claude_model "claude"));
      test_case "sonnet alias" `Quick (fun () ->
        check string "sonnet" "claude-3-5-sonnet-20241022" (Tool_parsers.resolve_claude_model "sonnet"));
      test_case "haiku alias" `Quick (fun () ->
        check string "haiku" "claude-3-5-haiku-20241022" (Tool_parsers.resolve_claude_model "haiku"));
      test_case "opus alias" `Quick (fun () ->
        check string "opus" "claude-opus-4-20250514" (Tool_parsers.resolve_claude_model "opus"));
      test_case "opus-4 alias" `Quick (fun () ->
        check string "opus-4" "claude-opus-4-20250514" (Tool_parsers.resolve_claude_model "opus-4"));
      test_case "passthrough" `Quick (fun () ->
        check string "custom" "custom-model" (Tool_parsers.resolve_claude_model "custom-model"));
      test_case "case insensitive" `Quick (fun () ->
        check string "SONNET" "claude-3-5-sonnet-20241022" (Tool_parsers.resolve_claude_model "SONNET"));
    ];

    "resolve_gemini_model", [
      test_case "gemini alias" `Quick (fun () ->
        check string "gemini" "gemini-3.1-pro-preview" (Tool_parsers.resolve_gemini_model "gemini"));
      test_case "pro alias" `Quick (fun () ->
        check string "pro" "gemini-2.5-pro" (Tool_parsers.resolve_gemini_model "pro"));
      test_case "flash alias" `Quick (fun () ->
        check string "flash" "gemini-2.5-flash" (Tool_parsers.resolve_gemini_model "flash"));
      test_case "flash-lite alias" `Quick (fun () ->
        check string "flash-lite" "gemini-2.5-flash-lite" (Tool_parsers.resolve_gemini_model "flash-lite"));
      test_case "3-pro alias" `Quick (fun () ->
        check string "3-pro" "gemini-3.1-pro-preview" (Tool_parsers.resolve_gemini_model "3-pro"));
      test_case "3-flash alias" `Quick (fun () ->
        check string "3-flash" "gemini-3-flash-preview" (Tool_parsers.resolve_gemini_model "3-flash"));
      test_case "passthrough" `Quick (fun () ->
        check string "custom" "some-model" (Tool_parsers.resolve_gemini_model "some-model"));
    ];

    (* {1GLM args parsing} *)

    "parse_glm_args", [
      test_case "defaults" `Quick (fun () ->
        let json = `Assoc [("prompt", `String "Hello GLM")] in
        match Tool_parsers.parse_glm_args json with
        | Glm g ->
          check string "default model" "glm-5" g.model;
          check string "default modality" "text" g.modality;
          check bool "default cascade" false g.cascade;
          check (option int) "default min_context" None g.min_context_tokens;
          check (option (list string)) "default cascade_models" None g.cascade_models;
          check (option string) "no system_prompt" None g.system_prompt;
          check bool "default thinking" false g.thinking;
          check bool "default do_sample" true g.do_sample;
          check bool "default web_search" false g.web_search;
          check int "tools empty" 0 (List.length g.tools);
          check string "prompt" "Hello GLM" g.prompt
        | _ -> fail "Expected Glm variant");
      test_case "custom values" `Quick (fun () ->
        let json = `Assoc [
          ("prompt", `String "Test");
          ("model", `String "glm-4v-plus-0111");
          ("system_prompt", `String "Be helpful");
          ("temperature", `Float 0.5);
          ("max_tokens", `Int 1024);
          ("thinking", `Bool true);
          ("do_sample", `Bool false);
          ("web_search", `Bool true);
          ("timeout", `Int 60);
          ("stream", `Bool false);
          ("modality", `String "image");
          ("cascade", `Bool true);
          ("cascade_models", `List [`String "glm-image"; `String "cogview-4-250304"]);
          ("min_context_tokens", `Int 150000);
        ] in
        match Tool_parsers.parse_glm_args json with
        | Glm g ->
          check string "model" "glm-4v-plus-0111" g.model;
          check string "modality" "image" g.modality;
          check bool "cascade true" true g.cascade;
          check (option int) "min_context_tokens" (Some 150000) g.min_context_tokens;
          check (option (list string)) "cascade models"
            (Some ["glm-image"; "cogview-4-250304"])
            g.cascade_models;
          check (option string) "system" (Some "Be helpful") g.system_prompt;
          check bool "thinking" true g.thinking;
          check bool "do_sample" false g.do_sample;
          check bool "web_search" true g.web_search;
          check int "timeout" 60 g.timeout;
          check bool "stream" false g.stream
        | _ -> fail "Expected Glm variant");
    ];

    "parse_glm_ocr_args", [
      test_case "defaults" `Quick (fun () ->
        let json = `Assoc [("file", `String "https://example.com/sample.png")] in
        match Tool_parsers.parse_glm_ocr_args json with
        | GlmOcr g ->
          check string "file" "https://example.com/sample.png" g.file;
          check string "default model" "glm-ocr" g.model;
          check int "default timeout" 120 g.timeout;
          check (option string) "api key none" None g.api_key
        | _ -> fail "Expected GlmOcr variant");
      test_case "custom values" `Quick (fun () ->
        let json = `Assoc [
          ("file", `String "https://example.com/doc.pdf");
          ("model", `String "glm-ocr");
          ("timeout", `Int 45);
          ("api_key", `String "zai-test-key");
        ] in
        match Tool_parsers.parse_glm_ocr_args json with
        | GlmOcr g ->
          check string "file" "https://example.com/doc.pdf" g.file;
          check string "model" "glm-ocr" g.model;
          check int "timeout" 45 g.timeout;
          check (option string) "api key" (Some "zai-test-key") g.api_key
        | _ -> fail "Expected GlmOcr variant");
    ];

    "parse_glm_image_args", [
      test_case "defaults" `Quick (fun () ->
        let json = `Assoc [("prompt", `String "blue square")] in
        match Tool_parsers.parse_glm_image_args json with
        | GlmImage g ->
          check string "prompt" "blue square" g.prompt;
          check string "default model" "glm-image" g.model;
          check string "default quality" "hd" g.quality;
          check string "default size" "1280x1280" g.size;
          check int "default timeout" 120 g.timeout;
          check (option string) "api key none" None g.api_key
        | _ -> fail "Expected GlmImage variant");
      test_case "custom values" `Quick (fun () ->
        let json = `Assoc [
          ("prompt", `String "robot mascot");
          ("model", `String "cogview-4-250304");
          ("quality", `String "standard");
          ("size", `String "1024x1024");
          ("timeout", `Int 45);
          ("api_key", `String "zai-test-key");
        ] in
        match Tool_parsers.parse_glm_image_args json with
        | GlmImage g ->
          check string "prompt" "robot mascot" g.prompt;
          check string "model" "cogview-4-250304" g.model;
          check string "quality" "standard" g.quality;
          check string "size" "1024x1024" g.size;
          check int "timeout" 45 g.timeout;
          check (option string) "api key" (Some "zai-test-key") g.api_key
        | _ -> fail "Expected GlmImage variant");
    ];

    "parse_glm_video_args", [
      test_case "defaults" `Quick (fun () ->
        let json = `Assoc [("prompt", `String "short loop animation")] in
        match Tool_parsers.parse_glm_video_args json with
        | GlmVideo g ->
          check string "prompt" "short loop animation" g.prompt;
          check string "default model" "viduq1-text" g.model;
          check string "default quality" "quality" g.quality;
          check bool "default with_audio" true g.with_audio;
          check string "default size" "1920x1080" g.size;
          check int "default fps" 30 g.fps;
          check int "default duration" 5 g.duration;
          check (option string) "default image_url" None g.image_url;
          check int "default timeout" 120 g.timeout
        | _ -> fail "Expected GlmVideo variant");
      test_case "custom values" `Quick (fun () ->
        let json = `Assoc [
          ("prompt", `String "camera pan over city");
          ("model", `String "vidu2-image");
          ("quality", `String "speed");
          ("with_audio", `Bool false);
          ("size", `String "1280x720");
          ("fps", `Int 60);
          ("duration", `Int 10);
          ("image_url", `String "https://example.com/frame.png");
          ("timeout", `Int 90);
          ("api_key", `String "zai-video-key");
        ] in
        match Tool_parsers.parse_glm_video_args json with
        | GlmVideo g ->
          check string "model" "vidu2-image" g.model;
          check string "quality" "speed" g.quality;
          check bool "with_audio" false g.with_audio;
          check string "size" "1280x720" g.size;
          check int "fps" 60 g.fps;
          check int "duration" 10 g.duration;
          check (option string) "image_url" (Some "https://example.com/frame.png") g.image_url;
          check int "timeout" 90 g.timeout;
          check (option string) "api key" (Some "zai-video-key") g.api_key
        | _ -> fail "Expected GlmVideo variant");
    ];

    "parse_glm_stt_args", [
      test_case "defaults" `Quick (fun () ->
        let json = `Assoc [("file_path", `String "/tmp/sample.wav")] in
        match Tool_parsers.parse_glm_stt_args json with
        | GlmStt g ->
          check string "default model" "glm-asr-2512" g.model;
          check (option string) "file_path" (Some "/tmp/sample.wav") g.file_path;
          check (option string) "file_base64 none" None g.file_base64;
          check (option string) "prompt none" None g.prompt;
          check (list string) "hotwords empty" [] g.hotwords;
          check bool "stream default" false g.stream;
          check int "timeout default" 120 g.timeout;
          check (option string) "api key none" None g.api_key
        | _ -> fail "Expected GlmStt variant");
      test_case "custom values" `Quick (fun () ->
        let json = `Assoc [
          ("model", `String "glm-asr-2512");
          ("file_base64", `String "UklGRiQAAABXQVZF");
          ("prompt", `String "회의록 용어");
          ("hotwords", `List [`String "Kidsnote"; `String "MASC"]);
          ("stream", `Bool true);
          ("timeout", `Int 80);
          ("api_key", `String "zai-stt-key");
        ] in
        match Tool_parsers.parse_glm_stt_args json with
        | GlmStt g ->
          check (option string) "file_path none" None g.file_path;
          check (option string) "file_base64" (Some "UklGRiQAAABXQVZF") g.file_base64;
          check (option string) "prompt" (Some "회의록 용어") g.prompt;
          check (list string) "hotwords" ["Kidsnote"; "MASC"] g.hotwords;
          check bool "stream" true g.stream;
          check int "timeout" 80 g.timeout;
          check (option string) "api key" (Some "zai-stt-key") g.api_key
        | _ -> fail "Expected GlmStt variant");
    ];

    (* {1GLM translate args} *)

    "parse_glm_translate_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [
          ("text", `String "Hello");
          ("source_lang", `String "en");
          ("target_lang", `String "ko");
        ] in
        match Tool_parsers.parse_glm_translate_args json with
        | GlmTranslate g ->
          check string "text" "Hello" g.text;
          check string "source" "en" g.source_lang;
          check string "target" "ko" g.target_lang;
          check string "default model" "glm-5" g.model;
          check int "default timeout" 120 g.timeout
        | _ -> fail "Expected GlmTranslate variant");
      test_case "custom strategy" `Quick (fun () ->
        let json = `Assoc [
          ("text", `String "code");
          ("source_lang", `String "ko");
          ("target_lang", `String "en");
          ("strategy", `String "technical");
          ("model", `String "glm-4");
          ("timeout", `Int 60);
        ] in
        match Tool_parsers.parse_glm_translate_args json with
        | GlmTranslate g ->
          check string "model" "glm-4" g.model;
          check int "timeout" 60 g.timeout
        | _ -> fail "Expected GlmTranslate variant");
    ];

    (* {1GLM tool parsing} *)

    "parse_glm_tool", [
      test_case "web_search" `Quick (fun () ->
        let json = `Assoc [
          ("type", `String "web_search");
          ("web_search", `Assoc [
            ("enable", `Bool true);
            ("search_result", `Bool false);
          ]);
        ] in
        let tool = Tool_parsers.parse_glm_tool json in
        check bool "web search type" true (tool.tool_type = Types.GlmWebSearch);
        (match tool.web_search_config with
         | Some (enable, search_result) ->
           check bool "enable" true enable;
           check bool "search_result" false search_result
         | None -> fail "expected web_search_config"));
      test_case "function" `Quick (fun () ->
        let json = `Assoc [
          ("type", `String "function");
          ("function", `Assoc [
            ("name", `String "get_weather");
            ("description", `String "Get weather info");
            ("parameters", `Assoc [
              ("type", `String "object");
              ("properties", `Assoc [
                ("city", `Assoc [
                  ("type", `String "string");
                  ("description", `String "City name");
                ]);
                ("count", `Assoc [
                  ("type", `String "integer");
                ]);
                ("temp", `Assoc [
                  ("type", `String "number");
                ]);
                ("active", `Assoc [
                  ("type", `String "boolean");
                ]);
                ("tags", `Assoc [
                  ("type", `String "array");
                ]);
                ("data", `Assoc [
                  ("type", `String "object");
                ]);
              ]);
              ("required", `List [`String "city"]);
            ]);
          ]);
        ] in
        let tool = Tool_parsers.parse_glm_tool json in
        check bool "function type" true (tool.tool_type = Types.GlmFunction);
        (match tool.function_schema with
         | Some schema ->
           check string "func_name" "get_weather" schema.func_name;
           check string "desc" "Get weather info" schema.func_description;
           check int "params" 6 (List.length schema.func_parameters)
         | None -> fail "expected function_schema"));
      test_case "code_interpreter" `Quick (fun () ->
        let json = `Assoc [
          ("type", `String "code_interpreter");
          ("code_interpreter", `Assoc [
            ("sandbox", `String "none");
          ]);
        ] in
        let tool = Tool_parsers.parse_glm_tool json in
        check bool "code interpreter type" true (tool.tool_type = Types.GlmCodeInterpreter);
        check (option string) "sandbox" (Some "none") tool.code_interpreter_config);
    ];

    (* {1GLM tools array parsing} *)

    "parse_glm_tools", [
      test_case "null tools" `Quick (fun () ->
        let json = `Assoc [] in
        let tools = Tool_parsers.parse_glm_tools json in
        check int "empty" 0 (List.length tools));
      test_case "list" `Quick (fun () ->
        let json = `Assoc [
          ("tools", `List [
            `Assoc [("type", `String "web_search");
                    ("web_search", `Assoc [("enable", `Bool true); ("search_result", `Bool true)])];
          ]);
        ] in
        let tools = Tool_parsers.parse_glm_tools json in
        check int "one tool" 1 (List.length tools));
      test_case "non-list" `Quick (fun () ->
        let json = `Assoc [("tools", `String "invalid")] in
        let tools = Tool_parsers.parse_glm_tools json in
        check int "empty for non-list" 0 (List.length tools));
    ];

    (* {1Stream delta args} *)

    "parse_stream_delta_args", [
      test_case "set enabled" `Quick (fun () ->
        let json = `Assoc [("enabled", `Bool true)] in
        match Tool_parsers.parse_set_stream_delta_args json with
        | SetStreamDelta { enabled } -> check bool "enabled" true enabled
        | _ -> fail "Expected SetStreamDelta");
      test_case "set disabled" `Quick (fun () ->
        let json = `Assoc [("enabled", `Bool false)] in
        match Tool_parsers.parse_set_stream_delta_args json with
        | SetStreamDelta { enabled } -> check bool "disabled" false enabled
        | _ -> fail "Expected SetStreamDelta");
      test_case "set default" `Quick (fun () ->
        let json = `Assoc [] in
        match Tool_parsers.parse_set_stream_delta_args json with
        | SetStreamDelta { enabled } -> check bool "default true" true enabled
        | _ -> fail "Expected SetStreamDelta");
      test_case "get" `Quick (fun () ->
        let json = `Assoc [] in
        match Tool_parsers.parse_get_stream_delta_args json with
        | GetStreamDelta -> ()
        | _ -> fail "Expected GetStreamDelta");
    ];

    (* {1Chain validate args} *)

    "parse_chain_validate_args", [
      test_case "mermaid only" `Quick (fun () ->
        let json = `Assoc [("mermaid", `String "graph LR\n a --> b")] in
        match Tool_parsers.parse_chain_validate_args json with
        | ChainValidate { chain; mermaid; strict } ->
          check bool "no chain" true (chain = None);
          check (option string) "mermaid" (Some "graph LR\n a --> b") mermaid;
          check bool "strict default" true strict
        | _ -> fail "Expected ChainValidate");
      test_case "chain json" `Quick (fun () ->
        let chain_json = `Assoc [("id", `String "test")] in
        let json = `Assoc [("chain", chain_json); ("strict", `Bool false)] in
        match Tool_parsers.parse_chain_validate_args json with
        | ChainValidate { chain; strict; _ } ->
          check bool "has chain" true (chain <> None);
          check bool "not strict" false strict
        | _ -> fail "Expected ChainValidate");
    ];

    (* {1Chain to_mermaid args} *)

    "parse_chain_to_mermaid_args", [
      test_case "basic" `Quick (fun () ->
        let chain_json = `Assoc [("id", `String "c1")] in
        let json = `Assoc [("chain", chain_json)] in
        match Tool_parsers.parse_chain_to_mermaid_args json with
        | ChainToMermaid { chain } ->
          check bool "has chain" true (chain <> `Null)
        | _ -> fail "Expected ChainToMermaid");
    ];

    (* {1Chain visualize args} *)

    "parse_chain_visualize_args", [
      test_case "basic" `Quick (fun () ->
        let chain_json = `Assoc [("id", `String "c1")] in
        let json = `Assoc [("chain", chain_json)] in
        match Tool_parsers.parse_chain_visualize_args json with
        | ChainVisualize { chain } ->
          check bool "has chain" true (chain <> `Null)
        | _ -> fail "Expected ChainVisualize");
    ];

    (* {1Chain convert args} *)

    "parse_chain_convert_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [
          ("from", `String "json");
          ("to", `String "mermaid");
          ("input", `Assoc [("id", `String "c1")]);
        ] in
        match Tool_parsers.parse_chain_convert_args json with
        | ChainConvert { from_format; to_format; pretty; _ } ->
          check string "from" "json" from_format;
          check string "to" "mermaid" to_format;
          check bool "pretty default" true pretty
        | _ -> fail "Expected ChainConvert");
      test_case "not pretty" `Quick (fun () ->
        let json = `Assoc [
          ("from", `String "mermaid");
          ("to", `String "json");
          ("input", `String "graph LR");
          ("pretty", `Bool false);
        ] in
        match Tool_parsers.parse_chain_convert_args json with
        | ChainConvert { pretty; _ } ->
          check bool "not pretty" false pretty
        | _ -> fail "Expected ChainConvert");
    ];

    (* {1Chain orchestrate args} *)

    "parse_chain_orchestrate_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [
          ("goal", `String "review code");
        ] in
        match Tool_parsers.parse_chain_orchestrate_args json with
        | ChainOrchestrate { goal; chain; tasks; chain_id; max_replans; timeout;
                             trace; verify_on_complete; orchestrator_model } ->
          check string "goal" "review code" goal;
          check bool "no chain" true (chain = None);
          check bool "no tasks" true (tasks = None);
          check (option string) "no chain_id" None chain_id;
          check int "max_replans default" 3 max_replans;
          check int "timeout default" 600 timeout;
          check bool "trace default" false trace;
          check bool "verify default" true verify_on_complete;
          check string "orchestrator default" "gemini" orchestrator_model
        | _ -> fail "Expected ChainOrchestrate");
      test_case "custom" `Quick (fun () ->
        let json = `Assoc [
          ("goal", `String "migrate");
          ("chain_id", `String "code-migration");
          ("max_replans", `Int 5);
          ("timeout", `Int 300);
          ("trace", `Bool true);
          ("verify_on_complete", `Bool false);
          ("orchestrator_model", `String "claude");
        ] in
        match Tool_parsers.parse_chain_orchestrate_args json with
        | ChainOrchestrate { goal; chain_id; max_replans; timeout; trace;
                             verify_on_complete; orchestrator_model; _ } ->
          check string "goal" "migrate" goal;
          check (option string) "chain_id" (Some "code-migration") chain_id;
          check int "max_replans" 5 max_replans;
          check int "timeout" 300 timeout;
          check bool "trace" true trace;
          check bool "no verify" false verify_on_complete;
          check string "orchestrator" "claude" orchestrator_model
        | _ -> fail "Expected ChainOrchestrate");
    ];

    (* {1GH PR diff args} *)

    "parse_gh_pr_diff_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [
          ("repo", `String "owner/repo");
          ("pr_number", `Int 123);
        ] in
        match Tool_parsers.parse_gh_pr_diff_args json with
        | GhPrDiff { repo; pr_number } ->
          check string "repo" "owner/repo" repo;
          check int "pr" 123 pr_number
        | _ -> fail "Expected GhPrDiff");
    ];

    (* {1Slack post args} *)

    "parse_slack_post_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [
          ("channel", `String "C123");
          ("text", `String "Hello");
        ] in
        match Tool_parsers.parse_slack_post_args json with
        | SlackPost { channel; text; thread_ts } ->
          check string "channel" "C123" channel;
          check string "text" "Hello" text;
          check (option string) "no thread" None thread_ts
        | _ -> fail "Expected SlackPost");
      test_case "with thread" `Quick (fun () ->
        let json = `Assoc [
          ("channel", `String "C456");
          ("text", `String "Reply");
          ("thread_ts", `String "1234567890.123456");
        ] in
        match Tool_parsers.parse_slack_post_args json with
        | SlackPost { thread_ts; _ } ->
          check (option string) "thread" (Some "1234567890.123456") thread_ts
        | _ -> fail "Expected SlackPost");
    ];

    (* {1Chain checkpoints args} *)

    "parse_chain_checkpoints_args", [
      test_case "defaults" `Quick (fun () ->
        let json = `Assoc [] in
        match Tool_parsers.parse_chain_checkpoints_args json with
        | ChainCheckpoints { chain_id; max_age_hours; cleanup } ->
          check (option string) "no chain_id" None chain_id;
          check (option int) "no max_age" None max_age_hours;
          check bool "no cleanup" false cleanup
        | _ -> fail "Expected ChainCheckpoints");
      test_case "custom" `Quick (fun () ->
        let json = `Assoc [
          ("chain_id", `String "my-chain");
          ("max_age_hours", `Int 24);
          ("cleanup", `Bool true);
        ] in
        match Tool_parsers.parse_chain_checkpoints_args json with
        | ChainCheckpoints { chain_id; max_age_hours; cleanup } ->
          check (option string) "chain_id" (Some "my-chain") chain_id;
          check (option int) "max_age" (Some 24) max_age_hours;
          check bool "cleanup" true cleanup
        | _ -> fail "Expected ChainCheckpoints");
    ];

    (* {1Chain resume args} *)

    "parse_chain_resume_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [("run_id", `String "abc-123")] in
        match Tool_parsers.parse_chain_resume_args json with
        | ChainResume { run_id; trace } ->
          check string "run_id" "abc-123" run_id;
          check bool "trace default" false trace
        | _ -> fail "Expected ChainResume");
      test_case "with trace" `Quick (fun () ->
        let json = `Assoc [("run_id", `String "xyz"); ("trace", `Bool true)] in
        match Tool_parsers.parse_chain_resume_args json with
        | ChainResume { run_id; trace } ->
          check string "run_id" "xyz" run_id;
          check bool "trace" true trace
        | _ -> fail "Expected ChainResume");
    ];

    (* {1Prompt register args} *)

    "parse_prompt_register_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "my-prompt");
          ("template", `String "Hello {{name}}");
        ] in
        match Tool_parsers.parse_prompt_register_args json with
        | PromptRegister { id; template; version } ->
          check string "id" "my-prompt" id;
          check string "template" "Hello {{name}}" template;
          check (option string) "no version" None version
        | _ -> fail "Expected PromptRegister");
      test_case "with version" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "v2-prompt");
          ("template", `String "t");
          ("version", `String "2.0.0");
        ] in
        match Tool_parsers.parse_prompt_register_args json with
        | PromptRegister { version; _ } ->
          check (option string) "version" (Some "2.0.0") version
        | _ -> fail "Expected PromptRegister");
    ];

    (* {1Prompt get args} *)

    "parse_prompt_get_args", [
      test_case "basic" `Quick (fun () ->
        let json = `Assoc [("id", `String "my-prompt")] in
        match Tool_parsers.parse_prompt_get_args json with
        | PromptGet { id; version } ->
          check string "id" "my-prompt" id;
          check (option string) "no version" None version
        | _ -> fail "Expected PromptGet");
      test_case "with version" `Quick (fun () ->
        let json = `Assoc [("id", `String "p"); ("version", `String "1.0.0")] in
        match Tool_parsers.parse_prompt_get_args json with
        | PromptGet { id; version } ->
          check string "id" "p" id;
          check (option string) "version" (Some "1.0.0") version
        | _ -> fail "Expected PromptGet");
    ];

    (* {1Ollama list args} *)

    "parse_ollama_list_args", [
      test_case "any input" `Quick (fun () ->
        let json = `Assoc [("extra", `String "ignored")] in
        match Tool_parsers.parse_ollama_list_args json with
        | OllamaList -> ()
        | _ -> fail "Expected OllamaList");
    ];

    (* {1Build ollama cmd with tools} *)

    "build_ollama_curl_cmd_tools", [
      test_case "with tools" `Quick (fun () ->
        let tool : Types.tool_schema = {
          name = "get_weather"; description = "Get weather";
          input_schema = `Assoc [("type", `String "object")];
        } in
        let args = Ollama {
          prompt = "What is the weather?";
          model = "devstral";
          system_prompt = Some "You are helpful";
          temperature = 0.7;
          timeout = 60;
          stream = false;
          tools = Some [tool];
        } in
        match Tool_parsers.build_ollama_curl_cmd args with
        | Ok cmd_list ->
          check bool "cmd starts with curl" true (List.hd cmd_list = "curl");
          (* tools branch uses /api/chat endpoint *)
          let url = List.find (fun s -> String.length s > 4 && String.sub s 0 4 = "http") cmd_list in
          check bool "chat endpoint" true (String.length url > 0 &&
            let suffix = "/api/chat" in
            let slen = String.length suffix in
            String.length url >= slen &&
            String.sub url (String.length url - slen) slen = suffix)
        | Error e -> fail ("build failed: " ^ e));
      test_case "streaming" `Quick (fun () ->
        let args = Ollama {
          prompt = "Hi"; model = "devstral"; system_prompt = None;
          temperature = 0.7; timeout = 60; stream = true; tools = None;
        } in
        match Tool_parsers.build_ollama_curl_cmd args with
        | Ok cmd_list ->
          check bool "has -sN flag" true (List.mem "-sN" cmd_list)
        | Error e -> fail ("build failed: " ^ e));
      test_case "with system_prompt no tools" `Quick (fun () ->
        let args = Ollama {
          prompt = "Hi"; model = "devstral";
          system_prompt = Some "Be helpful";
          temperature = 0.5; timeout = 60; stream = false; tools = None;
        } in
        match Tool_parsers.build_ollama_curl_cmd args with
        | Ok cmd_list ->
          (* No tools = /api/generate endpoint with system in JSON *)
          let data = List.nth cmd_list (List.length cmd_list - 1) in
          check bool "has system field" true
            (let n = "\"system\"" in
             let nlen = String.length n in
             let dlen = String.length data in
             if nlen > dlen then false
             else let rec c i = if i > dlen - nlen then false
               else if String.sub data i nlen = n then true else c (i+1)
             in c 0)
        | Error e -> fail ("build failed: " ^ e));
    ];

    (* {1Tool calls to JSON} *)

    "tool_calls_to_json", [
      test_case "basic" `Quick (fun () ->
        let calls : Ollama_parser.tool_call list = [
          { name = "get_weather"; arguments = "{\"city\":\"Seoul\"}" };
          { name = "search"; arguments = "{\"q\":\"test\"}" };
        ] in
        let json_str = Tool_parsers.tool_calls_to_json calls in
        check bool "is array" true (String.length json_str > 2 && String.get json_str 0 = '[');
        check bool "has get_weather" true
          (let n = "get_weather" in
           let nlen = String.length n in
           let slen = String.length json_str in
           let rec c i = if i > slen - nlen then false
             else if String.sub json_str i nlen = n then true else c (i+1)
           in c 0));
      test_case "empty" `Quick (fun () ->
        let json_str = Tool_parsers.tool_calls_to_json [] in
        check string "empty array" "[]" json_str);
    ];

    (* {1Clean codex output edge cases} *)

    "clean_codex_output_extra", [
      test_case "empty string" `Quick (fun () ->
        let output = Tool_parsers.clean_codex_output "" in
        check string "empty preserved" "" output);
      test_case "no codex marker" `Quick (fun () ->
        let output = Tool_parsers.clean_codex_output "just some text\nline 2" in
        check string "unchanged" "just some text\nline 2" output);
      test_case "tokens used line filtered" `Quick (fun () ->
        let input = "codex\nActual output\n50 tokens used in total" in
        let output = Tool_parsers.clean_codex_output input in
        check string "tokens line removed" "Actual output" output);
      test_case "multiple codex markers" `Quick (fun () ->
        let input = "codex\nfirst\ncodex\nsecond content" in
        let output = Tool_parsers.clean_codex_output input in
        check string "after last marker" "second content" output);
      test_case "codex in middle" `Quick (fun () ->
        let input = "some text with codex mention\nreal content" in
        let output = Tool_parsers.clean_codex_output input in
        check string "after codex mention" "real content" output);
    ];

    (* {1Parse ollama args with tools} *)

    "parse_ollama_args_tools", [
      test_case "with tools list" `Quick (fun () ->
        let json = `Assoc [
          ("prompt", `String "test");
          ("tools", `List [
            `Assoc [
              ("name", `String "my_tool");
              ("description", `String "A tool");
              ("input_schema", `Assoc [("type", `String "object")]);
            ];
          ]);
        ] in
        match Tool_parsers.parse_ollama_args json with
        | Ollama { tools; _ } ->
          (match tools with
           | Some tl -> check int "one tool" 1 (List.length tl)
           | None -> fail "expected tools")
        | _ -> fail "Expected Ollama");
      test_case "tools null" `Quick (fun () ->
        let json = `Assoc [
          ("prompt", `String "test");
          ("tools", `Null);
        ] in
        match Tool_parsers.parse_ollama_args json with
        | Ollama { tools; _ } ->
          check bool "no tools" true (tools = None)
        | _ -> fail "Expected Ollama");
      test_case "tools invalid type" `Quick (fun () ->
        let json = `Assoc [
          ("prompt", `String "test");
          ("tools", `String "invalid");
        ] in
        match Tool_parsers.parse_ollama_args json with
        | Ollama { tools; _ } ->
          check bool "no tools" true (tools = None)
        | _ -> fail "Expected Ollama");
    ];

    (* {1Default env var functions} *)

    "default_env_funcs", [
      test_case "default_me_root" `Quick (fun () ->
        let root = Tool_parsers.default_me_root () in
        check bool "ends with me" true
          (let suf = "me" in
           let slen = String.length suf in
           String.length root >= slen &&
           String.sub root (String.length root - slen) slen = suf));
      test_case "find_repo_root" `Quick (fun () ->
        let root = Tool_parsers.find_repo_root "/nonexistent/path" in
        check bool "returns string" true (String.length root > 0));
    ];

    (* {1Chain run with different input formats} *)

    "parse_chain_run_args_extra", [
      test_case "with mermaid" `Quick (fun () ->
        let json = `Assoc [
          ("mermaid", `String "graph LR\n a --> b");
          ("trace", `Bool true);
          ("checkpoint_enabled", `Bool true);
          ("timeout", `Int 60);
        ] in
        match Tool_parsers.parse_chain_run_args json with
        | ChainRun { mermaid; trace; checkpoint_enabled; timeout; _ } ->
          check (option string) "mermaid" (Some "graph LR\n a --> b") mermaid;
          check bool "trace" true trace;
          check bool "checkpoint" true checkpoint_enabled;
          check (option int) "timeout" (Some 60) timeout
        | _ -> fail "Expected ChainRun");
      test_case "string input" `Quick (fun () ->
        let json = `Assoc [
          ("input", `String "plain text input");
        ] in
        match Tool_parsers.parse_chain_run_args json with
        | ChainRun { input; _ } ->
          check (option string) "string input" (Some "plain text input") input
        | _ -> fail "Expected ChainRun");
      test_case "null input" `Quick (fun () ->
        let json = `Assoc [] in
        match Tool_parsers.parse_chain_run_args json with
        | ChainRun { input; chain; mermaid; chain_id; _ } ->
          check (option string) "no input" None input;
          check bool "no chain" true (chain = None);
          check (option string) "no mermaid" None mermaid;
          check (option string) "no chain_id" None chain_id
        | _ -> fail "Expected ChainRun");
    ];
  ]

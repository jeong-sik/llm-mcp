(** Test Tool_parsers - Pure function unit tests

    Independent tests for Tool_parsers module.
    These tests do NOT require Lwt or Eio - pure OCaml only.
*)

open Alcotest
open Types

(** {1 Parse Gemini Args Tests} *)

let test_parse_gemini_defaults () =
  let json = `Assoc [("prompt", `String "Hello world")] in
  match Tool_parsers.parse_gemini_args json with
  | Gemini g ->
      check string "default model" "gemini-3-pro-preview" g.model;
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

(** {1 Parse Gemini List Args Tests} *)

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

(** {1 Parse Claude Args Tests} *)

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

(** {1 Parse Codex Args Tests} *)

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

(** {1 Parse Ollama Args Tests} *)

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

(** {1 Budget Mode Tests} *)

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

(** {1 Build Command Tests} *)

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
    model = "gemini-3-pro-preview";
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

(** {1 Thinking Prompt Prefix Test} *)

let test_thinking_prompt_prefix () =
  (* High thinking level should have a non-empty prefix *)
  let prefix_high = Tool_parsers.thinking_prompt_prefix High in
  check bool "High prefix is non-empty" true (String.length prefix_high > 0);
  (* Low thinking level should have empty prefix *)
  let prefix_low = Tool_parsers.thinking_prompt_prefix Low in
  check bool "Low prefix is empty" true (String.length prefix_low = 0)

(** {1 Clean Codex Output Tests} *)

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

(** {1 Tool Schema Conversion Tests} *)

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

(** {1 Test Suite} *)

let () =
  Alcotest.run "Tool_parsers" [
    "parse_gemini_args", [
      test_case "defaults" `Quick test_parse_gemini_defaults;
      test_case "custom values" `Quick test_parse_gemini_custom_values;
      test_case "thinking levels" `Quick test_parse_gemini_thinking_levels;
    ];

    "parse_gemini_list_args", [
      test_case "defaults" `Quick test_parse_gemini_list_defaults;
      test_case "custom values" `Quick test_parse_gemini_list_custom_values;
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
  ]

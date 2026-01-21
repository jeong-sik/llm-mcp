(** Tests for Tools module *)

open Alcotest
open Llm_mcp.Types
open Llm_mcp.Tools

let unwrap_cmd label = function
  | Ok cmd -> cmd
  | Error e -> failf "%s failed: %s" label e

(** Test Gemini argument parsing *)
let test_parse_gemini_args () =
  let json = `Assoc [
    ("prompt", `String "Hello, Gemini!");
    ("model", `String "gemini-3-pro-preview");
    ("thinking_level", `String "high");
    ("yolo", `Bool true);
    ("timeout", `Int 120);
    ("stream", `Bool true);
  ] in
  match parse_gemini_args json with
  | Gemini { prompt; model; thinking_level; yolo; timeout; stream } ->
      check string "prompt" "Hello, Gemini!" prompt;
      check string "model" "gemini-3-pro-preview" model;
      check string "thinking_level" "high" (string_of_thinking_level thinking_level);
      check bool "yolo" true yolo;
      check int "timeout" 120 timeout;
      check bool "stream" true stream
  | _ -> fail "Expected Gemini args"

(** Test Gemini argument parsing with defaults *)
let test_parse_gemini_args_defaults () =
  let json = `Assoc [
    ("prompt", `String "Test prompt");
  ] in
  match parse_gemini_args json with
  | Gemini { prompt; model; thinking_level; yolo; timeout; stream } ->
      check string "prompt" "Test prompt" prompt;
      check string "model default" "gemini-3-pro-preview" model;
      check string "thinking_level default" "high" (string_of_thinking_level thinking_level);
      check bool "yolo default" false yolo;
      check int "timeout default" 300 timeout;
      (* stream defaults to true for SSE keepalive - prevents timeout on cold start *)
      check bool "stream default" true stream
  | _ -> fail "Expected Gemini args"

(** Test Gemini argument parsing with budget mode defaults *)
let test_parse_gemini_args_budget_mode () =
  let json = `Assoc [
    ("prompt", `String "Test prompt");
    ("budget_mode", `Bool true);
  ] in
  match parse_gemini_args json with
  | Gemini { thinking_level; _ } ->
      check string "thinking_level budget" "low" (string_of_thinking_level thinking_level)
  | _ -> fail "Expected Gemini args"

(** Test Claude argument parsing *)
let test_parse_claude_args () =
  let json = `Assoc [
    ("prompt", `String "Hello, Claude!");
    ("model", `String "sonnet");
    ("long_context", `Bool false);
    ("system_prompt", `String "You are helpful.");
    ("output_format", `String "json");
    ("allowed_tools", `List [`String "Read"; `String "Write"]);
    ("working_directory", `String "/home/user");
    ("timeout", `Int 60);
    ("stream", `Bool true);
  ] in
  match parse_claude_args json with
  | Claude { prompt; model; long_context; system_prompt; output_format; allowed_tools; working_directory; timeout; stream } ->
      check string "prompt" "Hello, Claude!" prompt;
      check string "model" "sonnet" model;
      check bool "long_context" false long_context;
      check (option string) "system_prompt" (Some "You are helpful.") system_prompt;
      check string "output_format" "json" (string_of_output_format output_format);
      check (list string) "allowed_tools" ["Read"; "Write"] allowed_tools;
      check string "working_directory" "/home/user" working_directory;
      check int "timeout" 60 timeout;
      check bool "stream" true stream
  | _ -> fail "Expected Claude args"

(** Test Claude argument parsing with defaults *)
let test_parse_claude_args_defaults () =
  let json = `Assoc [
    ("prompt", `String "Test prompt");
  ] in
  let expected_wd = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  match parse_claude_args json with
  | Claude { prompt; model; long_context; system_prompt; output_format; allowed_tools; working_directory; timeout; stream } ->
      check string "prompt" "Test prompt" prompt;
      check string "model default" "opus" model;
      check bool "long_context default" false long_context;  (* default: false to avoid API charges *)
      check (option string) "system_prompt default" None system_prompt;
      check string "output_format default" "text" (string_of_output_format output_format);
      check (list string) "allowed_tools default" [] allowed_tools;
      check string "working_directory default" expected_wd working_directory;
      check int "timeout default" 300 timeout;
      (* stream defaults to true for SSE keepalive *)
      check bool "stream default" true stream
  | _ -> fail "Expected Claude args"

(** Test Claude argument parsing with budget mode defaults *)
let test_parse_claude_args_budget_mode () =
  let json = `Assoc [
    ("prompt", `String "Test prompt");
    ("budget_mode", `Bool true);
  ] in
  match parse_claude_args json with
  | Claude { long_context; _ } ->
      check bool "long_context budget" false long_context
  | _ -> fail "Expected Claude args"

(** Test Codex argument parsing *)
let test_parse_codex_args () =
  let json = `Assoc [
    ("prompt", `String "Hello, Codex!");
    ("model", `String "gpt-5.2");
    ("reasoning_effort", `String "xhigh");
    ("sandbox", `String "danger-full-access");
    ("working_directory", `String "/workspace");
    ("timeout", `Int 180);
    ("stream", `Bool true);
  ] in
  match parse_codex_args json with
  | Codex { prompt; model; reasoning_effort; sandbox; working_directory; timeout; stream } ->
      check string "prompt" "Hello, Codex!" prompt;
      check string "model" "gpt-5.2" model;
      check string "reasoning_effort" "xhigh" (string_of_reasoning_effort reasoning_effort);
      check string "sandbox" "danger-full-access" (string_of_sandbox_policy sandbox);
      check (option string) "working_directory" (Some "/workspace") working_directory;
      check int "timeout" 180 timeout;
      check bool "stream" true stream
  | _ -> fail "Expected Codex args"

(** Test Codex argument parsing with defaults *)
let test_parse_codex_args_defaults () =
  let json = `Assoc [
    ("prompt", `String "Test prompt");
  ] in
  match parse_codex_args json with
  | Codex { prompt; model; reasoning_effort; sandbox; working_directory; timeout; stream } ->
      check string "prompt" "Test prompt" prompt;
      check string "model default" "gpt-5.2" model;
      check string "reasoning_effort default" "xhigh" (string_of_reasoning_effort reasoning_effort);
      check string "sandbox default" "workspace-write" (string_of_sandbox_policy sandbox);
      check (option string) "working_directory default" None working_directory;
      check int "timeout default" 300 timeout;
      (* stream defaults to true for SSE keepalive *)
      check bool "stream default" true stream
  | _ -> fail "Expected Codex args"

(** Test Codex argument parsing with budget mode defaults *)
let test_parse_codex_args_budget_mode () =
  let json = `Assoc [
    ("prompt", `String "Test prompt");
    ("budget_mode", `Bool true);
  ] in
  match parse_codex_args json with
  | Codex { reasoning_effort; _ } ->
      check string "reasoning_effort budget" "medium" (string_of_reasoning_effort reasoning_effort)
  | _ -> fail "Expected Codex args"

(** Test Gemini CLI command building *)
let test_build_gemini_cmd () =
  let args = Gemini {
    prompt = "Test prompt";
    model = "gemini-3-pro";
    thinking_level = High;
    yolo = false;
    timeout = 300;
    stream = false;
  } in
  let cmd = unwrap_cmd "build_gemini_cmd" (build_gemini_cmd args) in
  (* Check essential parts - prompt may be enhanced with thinking instructions *)
  check bool "contains gemini" true (List.mem "gemini" cmd);
  check bool "contains -m" true (List.mem "-m" cmd);
  check bool "contains model" true (List.mem "gemini-3-pro" cmd);
  check bool "no yolo flag" false (List.mem "--yolo" cmd)

let test_build_gemini_cmd_yolo () =
  let args = Gemini {
    prompt = "Test prompt";
    model = "gemini-3-pro";
    thinking_level = High;
    yolo = true;
    timeout = 300;
    stream = false;
  } in
  let cmd = unwrap_cmd "build_gemini_cmd" (build_gemini_cmd args) in
  check bool "contains gemini" true (List.mem "gemini" cmd);
  check bool "contains -m" true (List.mem "-m" cmd);
  check bool "contains model" true (List.mem "gemini-3-pro" cmd);
  check bool "has yolo flag" true (List.mem "--yolo" cmd)

(** Test Claude CLI command building *)
let test_build_claude_cmd () =
  let args = Claude {
    prompt = "Test prompt";
    model = "opus";
    long_context = false;
    system_prompt = None;
    output_format = Text;
    allowed_tools = [];
    working_directory = "/tmp";
    timeout = 300;
    stream = false;
  } in
  let cmd = unwrap_cmd "build_claude_cmd" (build_claude_cmd args) in
  (* First element is the wrapper script path, second is -p, etc. *)
  check bool "is non-empty" true (List.length cmd > 0);
  check bool "contains -p" true (List.mem "-p" cmd);
  check bool "contains --model" true (List.mem "--model" cmd);
  check bool "contains opus" true (List.mem "opus" cmd);
  check bool "no betas (long_context=false)" false (List.mem "--betas" cmd)

let test_build_claude_cmd_long_context () =
  let args = Claude {
    prompt = "Test prompt";
    model = "opus";
    long_context = true;
    system_prompt = Some "Be helpful";
    output_format = Json;
    allowed_tools = ["Read"; "Write"];
    working_directory = "/tmp";
    timeout = 300;
    stream = false;
  } in
  let cmd = unwrap_cmd "build_claude_cmd" (build_claude_cmd args) in
  check bool "contains --betas (long_context)" true (List.mem "--betas" cmd);
  check bool "contains --system-prompt" true (List.mem "--system-prompt" cmd);
  check bool "contains --output-format" true (List.mem "--output-format" cmd);
  check bool "contains json" true (List.mem "json" cmd);
  check bool "contains --allowed-tools" true (List.mem "--allowed-tools" cmd);
  check bool "contains Read" true (List.mem "Read" cmd)

(** Test Codex CLI command building *)
let test_build_codex_cmd () =
  let args = Codex {
    prompt = "Test prompt";
    model = "gpt-5.2";
    reasoning_effort = RXhigh;
    sandbox = WorkspaceWrite;
    working_directory = None;
    timeout = 300;
    stream = false;
  } in
  let cmd = unwrap_cmd "build_codex_cmd" (build_codex_cmd args) in
  check bool "contains codex" true (List.mem "codex" cmd);
  check bool "contains exec" true (List.mem "exec" cmd);
  check bool "contains -m" true (List.mem "-m" cmd);
  check bool "contains gpt-5.2" true (List.mem "gpt-5.2" cmd);
  check bool "contains --sandbox" true (List.mem "--sandbox" cmd);
  check bool "contains workspace-write" true (List.mem "workspace-write" cmd);
  check bool "contains --full-auto" true (List.mem "--full-auto" cmd)

let test_build_codex_cmd_with_cwd () =
  let args = Codex {
    prompt = "Test prompt";
    model = "gpt-5.2";
    reasoning_effort = RHigh;
    sandbox = DangerFullAccess;
    working_directory = Some "/workspace";
    timeout = 300;
    stream = false;
  } in
  let cmd = unwrap_cmd "build_codex_cmd" (build_codex_cmd args) in
  check bool "contains -C" true (List.mem "-C" cmd);
  check bool "contains /workspace" true (List.mem "/workspace" cmd)

(** Test clean_codex_output *)
let test_clean_codex_output () =
  let raw_output = "codex version 1.0.0\nsome text\nmore text\ntokens used: 100" in
  let cleaned = clean_codex_output raw_output in
  check string "cleaned output" "some text\nmore text" cleaned

(** Test clean_codex_output with OpenAI Codex format (v0.79.0+) *)
let test_clean_codex_output_openai_format () =
  let raw_output = "OpenAI Codex v0.79.0 (research preview)\n--------\nworkdir: /tmp\nmodel: gpt-5.2\n--------\nuser\nOK\nmcp: ready\ncodex\nHello World\ntokens used\n22,986" in
  let cleaned = clean_codex_output raw_output in
  (* Should capture content between "codex" line and "tokens used" line *)
  check string "OpenAI Codex format cleaned" "Hello World" cleaned

let test_clean_codex_output_empty () =
  let raw_output = "no codex header here" in
  let cleaned = clean_codex_output raw_output in
  check string "returns original if no header" "no codex header here" cleaned

(** Test exponential_backoff calculation: base_delay * 2^attempt *)
let test_exponential_backoff () =
  (* Formula: base_delay * 2^attempt *)
  let epsilon = 0.001 in
  let check_float msg expected actual =
    if abs_float (expected -. actual) > epsilon then
      fail (Printf.sprintf "%s: expected %f, got %f" msg expected actual)
  in
  check_float "attempt 0" 1.0 (exponential_backoff ~base_delay:1.0 0);   (* 1.0 * 2^0 = 1.0 *)
  check_float "attempt 1" 2.0 (exponential_backoff ~base_delay:1.0 1);   (* 1.0 * 2^1 = 2.0 *)
  check_float "attempt 2" 4.0 (exponential_backoff ~base_delay:1.0 2);   (* 1.0 * 2^2 = 4.0 *)
  check_float "attempt 3" 8.0 (exponential_backoff ~base_delay:1.0 3);   (* 1.0 * 2^3 = 8.0 *)
  check_float "base 0.5, attempt 2" 2.0 (exponential_backoff ~base_delay:0.5 2);  (* 0.5 * 2^2 = 2.0 *)
  check_float "base 2.0, attempt 1" 4.0 (exponential_backoff ~base_delay:2.0 1)   (* 2.0 * 2^1 = 4.0 *)

let () =
  run "Tools" [
    "parse_gemini_args", [
      test_case "full args" `Quick test_parse_gemini_args;
      test_case "defaults" `Quick test_parse_gemini_args_defaults;
      test_case "budget defaults" `Quick test_parse_gemini_args_budget_mode;
    ];
    "parse_claude_args", [
      test_case "full args" `Quick test_parse_claude_args;
      test_case "defaults" `Quick test_parse_claude_args_defaults;
      test_case "budget defaults" `Quick test_parse_claude_args_budget_mode;
    ];
    "parse_codex_args", [
      test_case "full args" `Quick test_parse_codex_args;
      test_case "defaults" `Quick test_parse_codex_args_defaults;
      test_case "budget defaults" `Quick test_parse_codex_args_budget_mode;
    ];
    "build_gemini_cmd", [
      test_case "basic" `Quick test_build_gemini_cmd;
      test_case "yolo mode" `Quick test_build_gemini_cmd_yolo;
    ];
    "build_claude_cmd", [
      test_case "basic" `Quick test_build_claude_cmd;
      test_case "long_context + options" `Quick test_build_claude_cmd_long_context;
    ];
    "build_codex_cmd", [
      test_case "basic" `Quick test_build_codex_cmd;
      test_case "with working_directory" `Quick test_build_codex_cmd_with_cwd;
    ];
    "clean_codex_output", [
      test_case "removes header and footer" `Quick test_clean_codex_output;
      test_case "OpenAI Codex v0.79.0+ format" `Quick test_clean_codex_output_openai_format;
      test_case "returns original if no header" `Quick test_clean_codex_output_empty;
    ];
    "exponential_backoff", [
      test_case "basic calculation" `Quick test_exponential_backoff;
    ];
  ]

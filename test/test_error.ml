(** Tests for Error module - Centralized error types

    Pure function tests:
    - is_recoverable: retry logic validation
    - to_string: human-readable error messages
    - severity_of_error: severity classification
    - string_of_severity: severity to string
    - JSON roundtrip: all error types
*)

open Alcotest
open Error

(** {1 is_recoverable Tests} *)

let test_gemini_function_call_sync_recoverable () =
  let err = Llm (GeminiError GeminiFunctionCallSync) in
  check bool "function call sync" true (is_recoverable err)

let test_gemini_rate_limit_recoverable () =
  let err = Llm (GeminiError GeminiRateLimit) in
  check bool "gemini rate limit" true (is_recoverable err)

let test_claude_rate_limit_recoverable () =
  let err = Llm (ClaudeError ClaudeRateLimit) in
  check bool "claude rate limit" true (is_recoverable err)

let test_codex_rate_limit_recoverable () =
  let err = Llm (CodexError CodexRateLimit) in
  check bool "codex rate limit" true (is_recoverable err)

let test_ollama_timeout_recoverable () =
  let err = Llm (OllamaError OllamaTimeout) in
  check bool "ollama timeout" true (is_recoverable err)

let test_process_timeout_recoverable () =
  let err = Process (ProcessTimeout 30) in
  check bool "process timeout" true (is_recoverable err)

let test_network_error_recoverable () =
  let err = Io (NetworkError "connection refused") in
  check bool "network error" true (is_recoverable err)

let test_auth_not_recoverable () =
  let err = Llm (GeminiError GeminiAuth) in
  check bool "auth not recoverable" false (is_recoverable err)

let test_context_too_long_not_recoverable () =
  let err = Llm (ClaudeError ClaudeContextTooLong) in
  check bool "context not recoverable" false (is_recoverable err)

let test_internal_not_recoverable () =
  let err = Internal "unexpected" in
  check bool "internal not recoverable" false (is_recoverable err)

(** {1 to_string Tests} *)

let test_gemini_errors_to_string () =
  let cases = [
    (GeminiError GeminiFunctionCallSync, "recoverable");
    (GeminiError GeminiContextTooLong, "context");
    (GeminiError GeminiRateLimit, "rate limit");
    (GeminiError GeminiAuth, "authentication");
    (GeminiError (GeminiUnknown "test"), "test");
  ] in
  List.iter (fun (e, keyword) ->
    let msg = to_string (Llm e) in
    check bool (Printf.sprintf "gemini %s" keyword)
      true (Common.contains ~substring:keyword msg)
  ) cases

let test_claude_errors_to_string () =
  let cases = [
    (ClaudeError ClaudeContextTooLong, "context");
    (ClaudeError ClaudeRateLimit, "rate limit");
    (ClaudeError ClaudeAuth, "authentication");
    (ClaudeError ClaudeTimeout, "timed out");
    (ClaudeError (ClaudeUnknown "msg"), "msg");
  ] in
  List.iter (fun (e, keyword) ->
    let msg = to_string (Llm e) in
    check bool (Printf.sprintf "claude %s" keyword)
      true (Common.contains ~substring:keyword msg)
  ) cases

let test_codex_errors_to_string () =
  let msg = to_string (Llm (CodexError CodexSandboxViolation)) in
  check bool "sandbox" true (Common.contains ~substring:"sandbox" msg)

let test_ollama_errors_to_string () =
  let msg = to_string (Llm (OllamaError (OllamaModelNotFound "llama3"))) in
  check bool "model name" true (Common.contains ~substring:"llama3" msg)

let test_chain_errors_to_string () =
  let cases = [
    (ChainParseError "invalid", "parse");
    (ChainCompileError "failed", "compile");
    (ChainExecutionError "runtime", "execution");
    (ChainTimeoutError 5000, "5000");
    (ChainCycleDetected, "cycle");
    (ChainNodeNotFound "node_1", "node_1");
    (ChainValidationError "schema", "validation");
  ] in
  List.iter (fun (e, keyword) ->
    let msg = to_string (Chain e) in
    check bool (Printf.sprintf "chain %s" keyword)
      true (Common.contains ~substring:keyword msg)
  ) cases

let test_mcp_errors_to_string () =
  let cases = [
    (McpParseError "json", "parse");
    (McpMethodNotFound "unknown", "unknown");
    (McpInvalidParams "bad", "invalid");
    (McpAuthError "token", "auth");
    (McpInternalError "crash", "internal");
  ] in
  List.iter (fun (e, keyword) ->
    let msg = to_string (Mcp e) in
    check bool (Printf.sprintf "mcp %s" keyword)
      true (Common.contains ~substring:keyword msg)
  ) cases

let test_process_errors_to_string () =
  let err = Process (ProcessExitCode (1, "error output")) in
  let msg = to_string err in
  check bool "exit code" true (Common.contains ~substring:"1" msg);
  check bool "stderr" true (Common.contains ~substring:"error output" msg)

let test_io_errors_to_string () =
  let cases = [
    (NetworkError "refused", "refused");
    (FileNotFound "/path", "/path");
    (PermissionDenied "/etc", "/etc");
    (JsonParseError "syntax", "JSON");
    (EncodingError "utf8", "Encoding");
  ] in
  List.iter (fun (e, keyword) ->
    let msg = to_string (Io e) in
    check bool (Printf.sprintf "io %s" keyword)
      true (Common.contains ~substring:keyword msg)
  ) cases

let test_internal_to_string () =
  let msg = to_string (Internal "unexpected crash") in
  check bool "internal" true (Common.contains ~substring:"Internal" msg);
  check bool "message" true (Common.contains ~substring:"unexpected crash" msg)

(** {1 severity Tests} *)

let test_gemini_function_sync_warning () =
  let err = Llm (GeminiError GeminiFunctionCallSync) in
  check bool "warning" true (severity_of_error err = Warning)

let test_gemini_rate_limit_warning () =
  let err = Llm (GeminiError GeminiRateLimit) in
  check bool "warning" true (severity_of_error err = Warning)

let test_chain_parse_warning () =
  let err = Chain (ChainParseError "bad") in
  check bool "warning" true (severity_of_error err = Warning)

let test_mcp_method_not_found_warning () =
  let err = Mcp (McpMethodNotFound "foo") in
  check bool "warning" true (severity_of_error err = Warning)

let test_process_timeout_warning () =
  let err = Process (ProcessTimeout 10) in
  check bool "warning" true (severity_of_error err = Warning)

let test_internal_critical () =
  let err = Internal "crash" in
  check bool "critical" true (severity_of_error err = Critical)

let test_chain_execution_error () =
  let err = Chain (ChainExecutionError "fail") in
  check bool "error" true (severity_of_error err = Error)

let test_io_error_severity () =
  let err = Io (FileNotFound "/path") in
  check bool "error" true (severity_of_error err = Error)

(** {1 string_of_severity Tests} *)

let test_all_severities () =
  check string "debug" "DEBUG" (string_of_severity Debug);
  check string "info" "INFO" (string_of_severity Info);
  check string "warning" "WARN" (string_of_severity Warning);
  check string "error" "ERROR" (string_of_severity Error);
  check string "critical" "CRITICAL" (string_of_severity Critical)

(** {1 Result Helpers Tests} *)

let test_fail_helper () =
  let result = fail (Internal "test") in
  match result with
  | Error (Internal "test") -> ()
  | _ -> Alcotest.fail "expected Internal error"

let test_ok_helper () =
  let result = ok 42 in
  match result with
  | Ok 42 -> ()
  | _ -> Alcotest.fail "expected Ok 42"

let test_to_string_result_ok () =
  let result = to_string_result (Ok "value") in
  match result with
  | Ok "value" -> ()
  | _ -> Alcotest.fail "expected Ok"

let test_to_string_result_error () =
  let result = to_string_result (Error (Internal "msg")) in
  match result with
  | Error s -> check bool "has msg" true (Common.contains ~substring:"msg" s)
  | _ -> Alcotest.fail "expected Error"

let test_of_string () =
  let err = of_string "legacy error" in
  match err with
  | Internal "legacy error" -> ()
  | _ -> Alcotest.fail "expected Internal"

(** {1 JSON Roundtrip Tests} *)

let test_llm_error_json_roundtrip () =
  let errors = [
    Llm (GeminiError GeminiFunctionCallSync);
    Llm (GeminiError (GeminiUnknown "test"));
    Llm (ClaudeError ClaudeTimeout);
    Llm (CodexError CodexSandboxViolation);
    Llm (OllamaError (OllamaModelNotFound "model"));
  ] in
  List.iter (fun err ->
    let json = to_yojson err in
    match of_yojson json with
    | Ok err2 -> check string "roundtrip" (to_string err) (to_string err2)
    | Error e -> Alcotest.fail (Printf.sprintf "roundtrip failed: %s" e)
  ) errors

let test_chain_error_json_roundtrip () =
  let errors = [
    Chain (ChainParseError "bad");
    Chain ChainCycleDetected;
    Chain (ChainTimeoutError 5000);
    Chain (ChainNodeNotFound "node");
  ] in
  List.iter (fun err ->
    let json = to_yojson err in
    match of_yojson json with
    | Ok err2 -> check string "roundtrip" (to_string err) (to_string err2)
    | Error e -> Alcotest.fail (Printf.sprintf "roundtrip failed: %s" e)
  ) errors

let test_mcp_error_json_roundtrip () =
  let err = Mcp (McpMethodNotFound "test_method") in
  let json = to_yojson err in
  match of_yojson json with
  | Ok err2 -> check string "roundtrip" (to_string err) (to_string err2)
  | Error e -> Alcotest.fail e

let test_io_error_json_roundtrip () =
  let errors = [
    Io (NetworkError "refused");
    Io (FileNotFound "/path");
    Io (PermissionDenied "/etc");
    Io (JsonParseError "syntax");
    Io (EncodingError "utf8");
  ] in
  List.iter (fun err ->
    let json = to_yojson err in
    match of_yojson json with
    | Ok err2 -> check string "roundtrip" (to_string err) (to_string err2)
    | Error e -> Alcotest.fail e
  ) errors

let test_process_error_json_roundtrip () =
  let errors = [
    Process (ProcessTimeout 30);
    Process (ProcessExitCode (1, "stderr"));
    Process (ProcessSpawnError "failed");
    Process ProcessKilled;
  ] in
  List.iter (fun err ->
    let json = to_yojson err in
    match of_yojson json with
    | Ok err2 -> check string "roundtrip" (to_string err) (to_string err2)
    | Error e -> Alcotest.fail e
  ) errors

(** {1 Test Suite} *)

let recoverable_tests = [
  test_case "gemini function sync" `Quick test_gemini_function_call_sync_recoverable;
  test_case "gemini rate limit" `Quick test_gemini_rate_limit_recoverable;
  test_case "claude rate limit" `Quick test_claude_rate_limit_recoverable;
  test_case "codex rate limit" `Quick test_codex_rate_limit_recoverable;
  test_case "ollama timeout" `Quick test_ollama_timeout_recoverable;
  test_case "process timeout" `Quick test_process_timeout_recoverable;
  test_case "network error" `Quick test_network_error_recoverable;
  test_case "auth not recoverable" `Quick test_auth_not_recoverable;
  test_case "context not recoverable" `Quick test_context_too_long_not_recoverable;
  test_case "internal not recoverable" `Quick test_internal_not_recoverable;
]

let to_string_tests = [
  test_case "gemini errors" `Quick test_gemini_errors_to_string;
  test_case "claude errors" `Quick test_claude_errors_to_string;
  test_case "codex errors" `Quick test_codex_errors_to_string;
  test_case "ollama errors" `Quick test_ollama_errors_to_string;
  test_case "chain errors" `Quick test_chain_errors_to_string;
  test_case "mcp errors" `Quick test_mcp_errors_to_string;
  test_case "process errors" `Quick test_process_errors_to_string;
  test_case "io errors" `Quick test_io_errors_to_string;
  test_case "internal" `Quick test_internal_to_string;
]

let severity_tests = [
  test_case "gemini function sync warning" `Quick test_gemini_function_sync_warning;
  test_case "gemini rate limit warning" `Quick test_gemini_rate_limit_warning;
  test_case "chain parse warning" `Quick test_chain_parse_warning;
  test_case "mcp method not found warning" `Quick test_mcp_method_not_found_warning;
  test_case "process timeout warning" `Quick test_process_timeout_warning;
  test_case "internal critical" `Quick test_internal_critical;
  test_case "chain execution error" `Quick test_chain_execution_error;
  test_case "io error severity" `Quick test_io_error_severity;
  test_case "all severities" `Quick test_all_severities;
]

let result_tests = [
  test_case "fail helper" `Quick test_fail_helper;
  test_case "ok helper" `Quick test_ok_helper;
  test_case "to_string_result ok" `Quick test_to_string_result_ok;
  test_case "to_string_result error" `Quick test_to_string_result_error;
  test_case "of_string" `Quick test_of_string;
]

let json_tests = [
  test_case "llm errors" `Quick test_llm_error_json_roundtrip;
  test_case "chain errors" `Quick test_chain_error_json_roundtrip;
  test_case "mcp errors" `Quick test_mcp_error_json_roundtrip;
  test_case "io errors" `Quick test_io_error_json_roundtrip;
  test_case "process errors" `Quick test_process_error_json_roundtrip;
]

let () =
  run "error" [
    ("is_recoverable", recoverable_tests);
    ("to_string", to_string_tests);
    ("severity", severity_tests);
    ("result_helpers", result_tests);
    ("json_roundtrip", json_tests);
  ]

(** Tests for Types module *)

open Alcotest
open Types

(** Test thinking_level conversion *)
let test_thinking_level_of_string () =
  check string "low" "low" (string_of_thinking_level (thinking_level_of_string "low"));
  check string "high" "high" (string_of_thinking_level (thinking_level_of_string "high"));
  check string "unknown defaults to high" "high"
    (string_of_thinking_level (thinking_level_of_string "unknown"))

(** Test output_format conversion *)
let test_output_format_of_string () =
  check string "text" "text" (string_of_output_format (output_format_of_string "text"));
  check string "json" "json" (string_of_output_format (output_format_of_string "json"));
  check string "stream-json" "stream-json"
    (string_of_output_format (output_format_of_string "stream-json"));
  check string "unknown defaults to text" "text"
    (string_of_output_format (output_format_of_string "invalid"))

(** Test reasoning_effort conversion *)
let test_reasoning_effort_of_string () =
  check string "low" "low" (string_of_reasoning_effort (reasoning_effort_of_string "low"));
  check string "medium" "medium" (string_of_reasoning_effort (reasoning_effort_of_string "medium"));
  check string "high" "high" (string_of_reasoning_effort (reasoning_effort_of_string "high"));
  check string "xhigh" "xhigh" (string_of_reasoning_effort (reasoning_effort_of_string "xhigh"));
  check string "unknown defaults to xhigh" "xhigh"
    (string_of_reasoning_effort (reasoning_effort_of_string "unknown"))

(** Test sandbox_policy conversion *)
let test_sandbox_policy_of_string () =
  check string "read-only" "read-only"
    (string_of_sandbox_policy (sandbox_policy_of_string "read-only"));
  check string "workspace-write" "workspace-write"
    (string_of_sandbox_policy (sandbox_policy_of_string "workspace-write"));
  check string "danger-full-access" "danger-full-access"
    (string_of_sandbox_policy (sandbox_policy_of_string "danger-full-access"));
  check string "unknown defaults to workspace-write" "workspace-write"
    (string_of_sandbox_policy (sandbox_policy_of_string "unknown"))

(** Test tool_result JSON serialization *)
let test_tool_result_to_yojson () =
  let result = {
    model = "gemini (gemini-3-pro)";
    returncode = 0;
    response = "Hello, world!";
    extra = [("key", "value")];
  } in
  let json = tool_result_to_yojson result in
  let open Yojson.Safe.Util in
  check string "model" "gemini (gemini-3-pro)" (json |> member "model" |> to_string);
  check int "returncode" 0 (json |> member "returncode" |> to_int);
  check string "response" "Hello, world!" (json |> member "response" |> to_string);
  (* extra fields are merged at top level, not nested *)
  check string "extra key" "value" (json |> member "key" |> to_string)

(** Test all_schemas contains expected tools *)
let test_all_schemas () =
  check int "should have 22 schemas" 22 (List.length all_schemas);  (* gemini, claude, codex, ollama, ollama_list, glm, chain_{run,validate,convert,list,checkpoints,resume,to_mermaid,visualize,orchestrate}, prompt_{register,list,get}, gh_pr_diff, slack_post, set_stream_delta, get_stream_delta *)
  let names = List.map (fun (s : tool_schema) -> s.name) all_schemas in
  check bool "gemini exists" true (List.mem "gemini" names);
  check bool "claude-cli exists" true (List.mem "claude-cli" names);
  check bool "codex exists" true (List.mem "codex" names);
  check bool "ollama exists" true (List.mem "ollama" names);
  check bool "ollama_list exists" true (List.mem "ollama_list" names)

(** Test gemini_schema structure *)
let test_gemini_schema () =
  check string "name" "gemini" gemini_schema.name;
  check bool "description not empty" true (String.length gemini_schema.description > 0);
  let open Yojson.Safe.Util in
  let props = gemini_schema.input_schema |> member "properties" in
  check bool "has prompt" true (props |> member "prompt" |> to_option Fun.id <> None);
  check bool "has model" true (props |> member "model" |> to_option Fun.id <> None);
  check bool "has thinking_level" true (props |> member "thinking_level" |> to_option Fun.id <> None)

(** Test claude_schema structure *)
let test_claude_schema () =
  check string "name" "claude-cli" claude_schema.name;
  let open Yojson.Safe.Util in
  let props = claude_schema.input_schema |> member "properties" in
  check bool "has prompt" true (props |> member "prompt" |> to_option Fun.id <> None);
  check bool "has long_context" true (props |> member "long_context" |> to_option Fun.id <> None);
  check bool "has allowed_tools" true (props |> member "allowed_tools" |> to_option Fun.id <> None)

(** Test codex_schema structure *)
let test_codex_schema () =
  check string "name" "codex" codex_schema.name;
  let open Yojson.Safe.Util in
  let props = codex_schema.input_schema |> member "properties" in
  check bool "has prompt" true (props |> member "prompt" |> to_option Fun.id <> None);
  check bool "has reasoning_effort" true
    (props |> member "reasoning_effort" |> to_option Fun.id <> None);
  check bool "has sandbox" true (props |> member "sandbox" |> to_option Fun.id <> None)

(** Test common_prefix_length *)
let test_common_prefix_length () =
  check int "identical" 5 (common_prefix_length "hello" "hello");
  check int "prefix match" 6 (common_prefix_length "hello world" "hello there");  (* "hello " = 6 chars *)
  check int "no match" 0 (common_prefix_length "abc" "xyz");
  check int "partial" 3 (common_prefix_length "abc123" "abc456");
  check int "empty first" 0 (common_prefix_length "" "hello");
  check int "empty second" 0 (common_prefix_length "hello" "");
  check int "both empty" 0 (common_prefix_length "" "")

(** Test compute_delta: Full case *)
let test_compute_delta_full () =
  (* Empty old content -> Full *)
  let delta = compute_delta ~old_content:"" ~new_content:"hello" in
  (match delta with
   | Full s -> check string "full from empty" "hello" s
   | _ -> fail "expected Full");
  (* No common prefix -> Full *)
  let delta2 = compute_delta ~old_content:"abc" ~new_content:"xyz" in
  (match delta2 with
   | Full s -> check string "full no prefix" "xyz" s
   | _ -> fail "expected Full")

(** Test compute_delta: Append case *)
let test_compute_delta_append () =
  let delta = compute_delta ~old_content:"hello" ~new_content:"hello world" in
  (match delta with
   | Append s -> check string "append suffix" " world" s
   | _ -> fail "expected Append")

(** Test compute_delta: Replace case *)
let test_compute_delta_replace () =
  (* Replace from middle - common prefix "hello " *)
  let delta = compute_delta ~old_content:"hello world" ~new_content:"hello there" in
  (match delta with
   | Replace (pos, s) ->
       check int "replace position" 6 pos;
       check string "replace content" "there" s
   | Full _ -> () (* Also acceptable if encoding is smaller *)
   | _ -> fail "expected Replace or Full")

(** Test compute_delta: Same content *)
let test_compute_delta_same () =
  let delta = compute_delta ~old_content:"same" ~new_content:"same" in
  (match delta with
   | Full s -> check string "same content" "same" s
   | _ -> fail "expected Full for same content")

(** Test delta encoding/decoding roundtrip *)
let test_delta_roundtrip () =
  let test_delta d =
    let encoded = encode_delta d in
    match decode_delta encoded with
    | Some decoded ->
        let re_encoded = encode_delta decoded in
        check string "roundtrip" encoded re_encoded
    | None -> fail "decode failed"
  in
  test_delta (Full "hello world");
  test_delta (Append " suffix");
  test_delta (Replace (10, "new content"))

(** Test apply_delta *)
let test_apply_delta () =
  check string "Full" "new" (apply_delta "old" (Full "new"));
  check string "Append" "hello world" (apply_delta "hello" (Append " world"));
  check string "Replace" "hello there" (apply_delta "hello world" (Replace (6, "there")));
  check string "Replace at end" "hello there" (apply_delta "hello" (Replace (100, " there")))

(** Test str_contains helper function *)
let test_str_contains () =
  check bool "substring present" true (str_contains ~substring:"hello" "hello world");
  check bool "substring at start" true (str_contains ~substring:"hello" "hello");
  check bool "substring at end" true (str_contains ~substring:"world" "hello world");
  check bool "substring absent" false (str_contains ~substring:"foo" "hello world");
  check bool "empty substring" true (str_contains ~substring:"" "hello");
  check bool "empty string" false (str_contains ~substring:"hello" "");
  check bool "case sensitive" false (str_contains ~substring:"HELLO" "hello world")

(** Test classify_gemini_error - FunctionCallSyncError *)
let test_classify_function_call_sync_error () =
  (* The actual error message from Gemini API *)
  let error1 = "Error: 400 INVALID_ARGUMENT. Please ensure that the number of function response parts is equal to the number of function call parts" in
  let error2 = "function call parts mismatch" in
  let error3 = "INVALID_ARGUMENT: function mismatch" in
  (match classify_gemini_error error1 with
   | Some FunctionCallSyncError -> ()
   | _ -> fail "expected FunctionCallSyncError for error1");
  (match classify_gemini_error error2 with
   | Some FunctionCallSyncError -> ()
   | _ -> fail "expected FunctionCallSyncError for error2");
  (match classify_gemini_error error3 with
   | Some FunctionCallSyncError -> ()
   | _ -> fail "expected FunctionCallSyncError for error3")

(** Test classify_gemini_error - ContextTooLongError *)
let test_classify_context_too_long_error () =
  let error1 = "context is too long" in
  let error2 = "Context exceeds maximum length" in
  (match classify_gemini_error error1 with
   | Some ContextTooLongError -> ()
   | _ -> fail "expected ContextTooLongError for error1");
  (match classify_gemini_error error2 with
   | Some ContextTooLongError -> ()
   | _ -> fail "expected ContextTooLongError for error2")

(** Test classify_gemini_error - RateLimitError *)
let test_classify_rate_limit_error () =
  let error1 = "rate limit exceeded" in
  let error2 = "quota exhausted" in
  let error3 = "RESOURCE_EXHAUSTED" in
  (match classify_gemini_error error1 with
   | Some RateLimitError -> ()
   | _ -> fail "expected RateLimitError for error1");
  (match classify_gemini_error error2 with
   | Some RateLimitError -> ()
   | _ -> fail "expected RateLimitError for error2");
  (match classify_gemini_error error3 with
   | Some RateLimitError -> ()
   | _ -> fail "expected RateLimitError for error3")

(** Test classify_gemini_error - AuthenticationError *)
let test_classify_authentication_error () =
  let error1 = "authentication failed" in
  let error2 = "unauthorized access" in
  let error3 = "invalid api key" in
  (match classify_gemini_error error1 with
   | Some AuthenticationError -> ()
   | _ -> fail "expected AuthenticationError for error1");
  (match classify_gemini_error error2 with
   | Some AuthenticationError -> ()
   | _ -> fail "expected AuthenticationError for error2");
  (match classify_gemini_error error3 with
   | Some AuthenticationError -> ()
   | _ -> fail "expected AuthenticationError for error3")

(** Test classify_gemini_error - UnknownGeminiError and None *)
let test_classify_unknown_and_none () =
  (* Generic 400 error patterns -> UnknownGeminiError *)
  let error1 = "Error: 400 Bad Request" in  (* "error:" + "400" *)
  let error2 = "Error 400 occurred" in      (* "error 400" *)
  let error3 = "400 Bad Request" in         (* "400 bad request" *)
  (match classify_gemini_error error1 with
   | Some (UnknownGeminiError _) -> ()
   | _ -> fail "expected UnknownGeminiError for 'Error: 400'");
  (match classify_gemini_error error2 with
   | Some (UnknownGeminiError _) -> ()
   | _ -> fail "expected UnknownGeminiError for 'Error 400'");
  (match classify_gemini_error error3 with
   | Some (UnknownGeminiError _) -> ()
   | _ -> fail "expected UnknownGeminiError for '400 Bad Request'");
  (* Normal response -> None *)
  let success = "Hello! How can I help you today?" in
  (match classify_gemini_error success with
   | None -> ()
   | Some e -> fail (Printf.sprintf "expected None for success, got %s" (string_of_gemini_error e)));
  (* Empty string -> None *)
  (match classify_gemini_error "" with
   | None -> ()
   | _ -> fail "expected None for empty string")

(** Test false positive prevention - normal responses should NOT be classified as errors *)
let test_classify_no_false_positives () =
  (* Response about HTTP errors should NOT trigger false positive *)
  let explanation1 = "An HTTP 400 error means the request was malformed" in
  let explanation2 = "The server returned error code 500, not 400" in
  let explanation3 = "Common errors include 400, 401, 403, and 404" in
  (match classify_gemini_error explanation1 with
   | None -> ()
   | Some e -> fail (Printf.sprintf "false positive on explanation1: %s" (string_of_gemini_error e)));
  (match classify_gemini_error explanation2 with
   | None -> ()
   | Some e -> fail (Printf.sprintf "false positive on explanation2: %s" (string_of_gemini_error e)));
  (match classify_gemini_error explanation3 with
   | None -> ()
   | Some e -> fail (Printf.sprintf "false positive on explanation3: %s" (string_of_gemini_error e)))

(** Test string_of_gemini_error *)
let test_string_of_gemini_error () =
  check string "FunctionCallSyncError" "FunctionCallSyncError"
    (string_of_gemini_error FunctionCallSyncError);
  check string "ContextTooLongError" "ContextTooLongError"
    (string_of_gemini_error ContextTooLongError);
  check string "RateLimitError" "RateLimitError"
    (string_of_gemini_error RateLimitError);
  check string "AuthenticationError" "AuthenticationError"
    (string_of_gemini_error AuthenticationError);
  check string "UnknownGeminiError" "UnknownGeminiError(test msg)"
    (string_of_gemini_error (UnknownGeminiError "test msg"))

(** Test is_recoverable_gemini_error *)
let test_is_recoverable_gemini_error () =
  check bool "FunctionCallSyncError is recoverable" true
    (is_recoverable_gemini_error FunctionCallSyncError);
  check bool "RateLimitError is recoverable" true
    (is_recoverable_gemini_error RateLimitError);
  check bool "ContextTooLongError is not recoverable" false
    (is_recoverable_gemini_error ContextTooLongError);
  check bool "AuthenticationError is not recoverable" false
    (is_recoverable_gemini_error AuthenticationError);
  check bool "UnknownGeminiError is not recoverable" false
    (is_recoverable_gemini_error (UnknownGeminiError "test"))

let () =
  run "Types" [
    "thinking_level", [
      test_case "of_string roundtrip" `Quick test_thinking_level_of_string;
    ];
    "delta", [
      test_case "common_prefix_length" `Quick test_common_prefix_length;
      test_case "compute_delta Full" `Quick test_compute_delta_full;
      test_case "compute_delta Append" `Quick test_compute_delta_append;
      test_case "compute_delta Replace" `Quick test_compute_delta_replace;
      test_case "compute_delta Same" `Quick test_compute_delta_same;
      test_case "encode/decode roundtrip" `Quick test_delta_roundtrip;
      test_case "apply_delta" `Quick test_apply_delta;
    ];
    "output_format", [
      test_case "of_string roundtrip" `Quick test_output_format_of_string;
    ];
    "reasoning_effort", [
      test_case "of_string roundtrip" `Quick test_reasoning_effort_of_string;
    ];
    "sandbox_policy", [
      test_case "of_string roundtrip" `Quick test_sandbox_policy_of_string;
    ];
    "tool_result", [
      test_case "to_yojson" `Quick test_tool_result_to_yojson;
    ];
    "schemas", [
      test_case "all_schemas count" `Quick test_all_schemas;
      test_case "gemini_schema" `Quick test_gemini_schema;
      test_case "claude_schema" `Quick test_claude_schema;
      test_case "codex_schema" `Quick test_codex_schema;
    ];
    "gemini_error", [
      test_case "str_contains" `Quick test_str_contains;
      test_case "classify FunctionCallSyncError" `Quick test_classify_function_call_sync_error;
      test_case "classify ContextTooLongError" `Quick test_classify_context_too_long_error;
      test_case "classify RateLimitError" `Quick test_classify_rate_limit_error;
      test_case "classify AuthenticationError" `Quick test_classify_authentication_error;
      test_case "classify Unknown and None" `Quick test_classify_unknown_and_none;
      test_case "no false positives" `Quick test_classify_no_false_positives;
      test_case "string_of_gemini_error" `Quick test_string_of_gemini_error;
      test_case "is_recoverable_gemini_error" `Quick test_is_recoverable_gemini_error;
    ];
  ]

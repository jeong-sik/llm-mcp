(** Tests for Ollama_parser module - Ollama response parsing

    Pure function tests:
    - parse_chunk: streaming chunk parsing
    - parse_response: non-streaming response
    - parse_chat_response: chat API with thinking support
    - parse_tool_calls: tool call extraction
    - parse_chat_result: categorized results
    - parse_chat_chunk: streaming chat chunks
*)

open Alcotest
open Ollama_parser

(** {1 parse_chunk Tests} *)

let test_parse_chunk_normal () =
  let json = {|{"response":"hello","done":false}|} in
  match parse_chunk json with
  | Ok (token, done_) ->
      check string "token" "hello" token;
      check bool "not done" false done_
  | Error e -> fail e

let test_parse_chunk_done () =
  let json = {|{"response":"","done":true}|} in
  match parse_chunk json with
  | Ok (_, done_) -> check bool "done" true done_
  | Error e -> fail e

let test_parse_chunk_error () =
  let json = {|{"error":"model not found"}|} in
  match parse_chunk json with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "has error msg" true (Common.contains ~substring:"model not found" e)

let test_parse_chunk_invalid_json () =
  let json = "{not valid json" in
  match parse_chunk json with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "has parse error" true (Common.contains ~substring:"parse" e)

(** {1 parse_response Tests} *)

let test_parse_response_success () =
  let json = {|{"response":"Hello, world!"}|} in
  match parse_response json with
  | Ok resp -> check string "response" "Hello, world!" resp
  | Error e -> fail e

let test_parse_response_error () =
  let json = {|{"error":"context length exceeded"}|} in
  match parse_response json with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "has error" true (Common.contains ~substring:"context" e)

let test_parse_response_invalid () =
  let json = "not json" in
  match parse_response json with
  | Ok _ -> fail "expected error"
  | Error _ -> ()  (* expected *)

(** {1 parse_chat_response Tests} *)

let test_parse_chat_response_simple () =
  let json = {|{"message":{"role":"assistant","content":"Hello!"}}|} in
  match parse_chat_response json with
  | Ok (content, thinking, _) ->
      check string "content" "Hello!" content;
      check bool "no thinking" true (Option.is_none thinking)
  | Error e -> fail e

let test_parse_chat_response_with_thinking () =
  let json = {|{"message":{"role":"assistant","content":"Answer","thinking":"Let me think..."}}|} in
  match parse_chat_response json with
  | Ok (content, thinking, _) ->
      check string "content" "Answer" content;
      (match thinking with
       | Some t -> check string "thinking" "Let me think..." t
       | None -> fail "expected thinking")
  | Error e -> fail e

let test_parse_chat_response_error () =
  let json = {|{"error":"rate limit"}|} in
  match parse_chat_response json with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "has rate limit" true (Common.contains ~substring:"rate limit" e)

(** {1 parse_tool_calls Tests} *)

let test_parse_tool_calls_null () =
  let calls = parse_tool_calls `Null in
  check int "empty" 0 (List.length calls)

let test_parse_tool_calls_empty () =
  let calls = parse_tool_calls (`List []) in
  check int "empty list" 0 (List.length calls)

let test_parse_tool_calls_single () =
  let json = `List [
    `Assoc [
      ("function", `Assoc [
        ("name", `String "get_weather");
        ("arguments", `String {|{"city":"Tokyo"}|})
      ])
    ]
  ] in
  let calls = parse_tool_calls json in
  check int "one call" 1 (List.length calls);
  let call = List.hd calls in
  check string "name" "get_weather" call.name;
  check bool "has tokyo" true (Common.contains ~substring:"Tokyo" call.arguments)

let test_parse_tool_calls_object_args () =
  let json = `List [
    `Assoc [
      ("function", `Assoc [
        ("name", `String "search");
        ("arguments", `Assoc [("query", `String "test")])
      ])
    ]
  ] in
  let calls = parse_tool_calls json in
  check int "one call" 1 (List.length calls);
  let call = List.hd calls in
  check string "name" "search" call.name;
  check bool "has query" true (Common.contains ~substring:"query" call.arguments)

let test_parse_tool_calls_multiple () =
  let json = `List [
    `Assoc [("function", `Assoc [("name", `String "func1"); ("arguments", `String "{}")])];
    `Assoc [("function", `Assoc [("name", `String "func2"); ("arguments", `String "{}")])]
  ] in
  let calls = parse_tool_calls json in
  check int "two calls" 2 (List.length calls);
  check string "first" "func1" (List.nth calls 0).name;
  check string "second" "func2" (List.nth calls 1).name

(** {1 parse_chat_result Tests} *)

let test_parse_chat_result_text_only () =
  let json = {|{"message":{"content":"Just text"}}|} in
  match parse_chat_result json with
  | Ok (TextResponse (text, _)) -> check string "text" "Just text" text
  | Ok _ -> fail "expected TextResponse"
  | Error e -> fail e

let test_parse_chat_result_tool_calls_only () =
  let json = {|{"message":{"content":"","tool_calls":[{"function":{"name":"fn","arguments":"{}"}}]}}|} in
  match parse_chat_result json with
  | Ok (ToolCalls (calls, _)) -> check int "one call" 1 (List.length calls)
  | Ok _ -> fail "expected ToolCalls"
  | Error e -> fail e

let test_parse_chat_result_text_with_tools () =
  let json = {|{"message":{"content":"Here's the result","tool_calls":[{"function":{"name":"calc","arguments":"{}"}}]}}|} in
  match parse_chat_result json with
  | Ok (TextWithTools (text, calls, _)) ->
      check string "text" "Here's the result" text;
      check int "one call" 1 (List.length calls)
  | Ok _ -> fail "expected TextWithTools"
  | Error e -> fail e

let test_parse_chat_result_with_thinking () =
  let json = {|{"message":{"content":"42","thinking":"Computing..."}}|} in
  match parse_chat_result json with
  | Ok (TextResponse (text, thinking)) ->
      check string "text" "42" text;
      (match thinking with
       | Some t -> check string "thinking" "Computing..." t
       | None -> fail "expected thinking")
  | Ok _ -> fail "expected TextResponse"
  | Error e -> fail e

let test_parse_chat_result_error () =
  let json = {|{"error":"timeout"}|} in
  match parse_chat_result json with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "has timeout" true (Common.contains ~substring:"timeout" e)

(** {1 parse_chat_chunk Tests} *)

let test_parse_chat_chunk_streaming () =
  let json = {|{"message":{"content":"tok"},"done":false}|} in
  match parse_chat_chunk json with
  | Ok (content, tool_calls, done_) ->
      check string "content" "tok" content;
      check int "no tools" 0 (List.length tool_calls);
      check bool "not done" false done_
  | Error e -> fail e

let test_parse_chat_chunk_final () =
  let json = {|{"message":{"content":"","tool_calls":[{"function":{"name":"done_fn","arguments":"{}"}}]},"done":true}|} in
  match parse_chat_chunk json with
  | Ok (content, tool_calls, done_) ->
      check string "empty content" "" content;
      check int "one tool" 1 (List.length tool_calls);
      check bool "done" true done_
  | Error e -> fail e

let test_parse_chat_chunk_error () =
  let json = {|{"error":"connection reset"}|} in
  match parse_chat_chunk json with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "has connection" true (Common.contains ~substring:"connection" e)

(** {1 Test Suite} *)

let chunk_tests = [
  test_case "normal" `Quick test_parse_chunk_normal;
  test_case "done" `Quick test_parse_chunk_done;
  test_case "error" `Quick test_parse_chunk_error;
  test_case "invalid json" `Quick test_parse_chunk_invalid_json;
]

let response_tests = [
  test_case "success" `Quick test_parse_response_success;
  test_case "error" `Quick test_parse_response_error;
  test_case "invalid" `Quick test_parse_response_invalid;
]

let chat_response_tests = [
  test_case "simple" `Quick test_parse_chat_response_simple;
  test_case "with thinking" `Quick test_parse_chat_response_with_thinking;
  test_case "error" `Quick test_parse_chat_response_error;
]

let tool_calls_tests = [
  test_case "null" `Quick test_parse_tool_calls_null;
  test_case "empty" `Quick test_parse_tool_calls_empty;
  test_case "single" `Quick test_parse_tool_calls_single;
  test_case "object args" `Quick test_parse_tool_calls_object_args;
  test_case "multiple" `Quick test_parse_tool_calls_multiple;
]

let chat_result_tests = [
  test_case "text only" `Quick test_parse_chat_result_text_only;
  test_case "tool calls only" `Quick test_parse_chat_result_tool_calls_only;
  test_case "text with tools" `Quick test_parse_chat_result_text_with_tools;
  test_case "with thinking" `Quick test_parse_chat_result_with_thinking;
  test_case "error" `Quick test_parse_chat_result_error;
]

let chat_chunk_tests = [
  test_case "streaming" `Quick test_parse_chat_chunk_streaming;
  test_case "final" `Quick test_parse_chat_chunk_final;
  test_case "error" `Quick test_parse_chat_chunk_error;
]

let () =
  run "ollama_parser" [
    ("parse_chunk", chunk_tests);
    ("parse_response", response_tests);
    ("parse_chat_response", chat_response_tests);
    ("parse_tool_calls", tool_calls_tests);
    ("parse_chat_result", chat_result_tests);
    ("parse_chat_chunk", chat_chunk_tests);
  ]

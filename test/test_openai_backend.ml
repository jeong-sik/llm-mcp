(** OpenAI Backend Tests

    Tests for the OpenAI API backend implementation.
    Note: Actual API tests require a valid OPENAI_API_KEY.
*)

open Alcotest
module Types = Agent_core.Types
module Openai = Agent_core.Openai_backend
module State = Agent_core.Default_state
module Agent_loop_functor = Agent_core.Agent_loop_functor

open Types

(** {1 Mock Tools for Testing} *)

module Test_Tools = struct
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    match tc.name with
    | "get_weather" -> Lwt.return (Result.Ok (ToolSuccess "Sunny, 22°C"))
    | _ -> Lwt.return (Result.Error ("Unknown tool: " ^ tc.name))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.id }

  let available_tools () = ["get_weather"]
end

(** {1 Type Compatibility Tests} *)

(* Verify Openai_backend satisfies LLM_BACKEND signature *)
module Openai_Loop = Agent_loop_functor.Make(Openai)(Test_Tools)(State)

let test_signature_compatibility () =
  let _ = Openai.name in
  let _ = Openai.default_config in
  check string "backend name" "openai" Openai.name

let test_config_creation () =
  let config = Openai.{
    api_key = "sk-test-key";
    model = "gpt-4-turbo";
    temperature = 0.5;
    base_url = "https://api.openai.com/v1";
    timeout_ms = Some 30_000;
    max_tokens = Some 4096;
    organization = Some "org-123";
  } in
  check string "model" "gpt-4-turbo" config.model;
  check (float 0.01) "temperature" 0.5 config.temperature;
  check (option int) "max_tokens" (Some 4096) config.max_tokens

let test_default_config () =
  let config = Openai.default_config in
  check string "default model" "gpt-4" config.model;
  check string "default base_url" "https://api.openai.com/v1" config.base_url;
  check (float 0.01) "default temperature" 0.7 config.temperature;
  check (option int) "default timeout" (Some 60_000) config.timeout_ms

let test_config_from_env () =
  let config = Openai.config_from_env ~model:"gpt-3.5-turbo" () in
  check string "model override" "gpt-3.5-turbo" config.model

(** {1 Response Parsing Tests} *)

let test_parse_simple_response () =
  let json = {|{
    "id": "chatcmpl-123",
    "object": "chat.completion",
    "created": 1677652288,
    "model": "gpt-4-0613",
    "choices": [{
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "Hello! How can I help you today?"
      },
      "finish_reason": "stop"
    }],
    "usage": {
      "prompt_tokens": 9,
      "completion_tokens": 12,
      "total_tokens": 21
    }
  }|} in
  match Openai.parse_response json with
  | Result.Error e -> fail (Printf.sprintf "Parse failed: %s" e)
  | Result.Ok resp ->
    check string "content" "Hello! How can I help you today?" (Openai.extract_content resp);
    check bool "is_final" true (Openai.is_final resp);
    check (option pass) "no tool calls" None (Openai.parse_tool_calls resp)

let test_parse_tool_call_response () =
  let json = {|{
    "id": "chatcmpl-456",
    "model": "gpt-4-0613",
    "choices": [{
      "index": 0,
      "message": {
        "role": "assistant",
        "content": null,
        "tool_calls": [{
          "id": "call_abc123",
          "type": "function",
          "function": {
            "name": "get_weather",
            "arguments": "{\"location\": \"Seoul\"}"
          }
        }]
      },
      "finish_reason": "tool_calls"
    }]
  }|} in
  match Openai.parse_response json with
  | Result.Error e -> fail (Printf.sprintf "Parse failed: %s" e)
  | Result.Ok resp ->
    check bool "is_final" false (Openai.is_final resp);
    match Openai.parse_tool_calls resp with
    | None -> fail "expected tool calls"
    | Some calls ->
      check int "tool call count" 1 (List.length calls);
      let tc = List.hd calls in
      check string "tool name" "get_weather" tc.name;
      check string "tool id" "call_abc123" tc.id

let test_parse_error_response () =
  let json = {|{
    "error": {
      "message": "Invalid API key",
      "type": "invalid_request_error",
      "code": "invalid_api_key"
    }
  }|} in
  match Openai.parse_response json with
  | Result.Ok _ -> fail "should have failed"
  | Result.Error msg ->
    check bool "error contains message" true (String.length msg > 0)

let test_parse_empty_choices () =
  let json = {|{
    "id": "chatcmpl-789",
    "model": "gpt-4",
    "choices": []
  }|} in
  match Openai.parse_response json with
  | Result.Ok _ -> fail "should have failed"
  | Result.Error msg ->
    check bool "error mentions choices" true (String.length msg > 0)

let test_parse_invalid_json () =
  let invalid = "not valid json {" in
  match Openai.parse_response invalid with
  | Result.Ok _ -> fail "should have failed"
  | Result.Error msg ->
    check bool "error message exists" true (String.length msg > 0)

(** {1 Utility Tests} *)

let test_has_api_key () =
  let config_with_key = Openai.{ default_config with api_key = "sk-test" } in
  let config_without = Openai.{ default_config with api_key = "" } in
  check bool "has key" true (Openai.has_api_key config_with_key);
  check bool "no key" false (Openai.has_api_key config_without)

(** {1 Error Handling Tests} *)

let test_connection_error () =
  let config = Openai.{
    default_config with
    base_url = "http://localhost:99999";  (* Invalid port *)
    api_key = "test-key";
  } in
  let result = Lwt_main.run (
    Openai.call
      ~config
      ~messages:[{ role = User; content = "test"; tool_calls = None; name = None }]
      ~tools:[]
  ) in
  match result with
  | Result.Error _ -> ()  (* Expected *)
  | Result.Ok _ -> fail "expected connection error"

(** {1 Test Runner} *)

let () =
  run "OpenAI Backend" [
    "Signature", [
      "backend implements LLM_BACKEND", `Quick, test_signature_compatibility;
      "config creation", `Quick, test_config_creation;
      "default config", `Quick, test_default_config;
      "config from env", `Quick, test_config_from_env;
    ];
    "Response Parsing", [
      "simple response", `Quick, test_parse_simple_response;
      "tool call response", `Quick, test_parse_tool_call_response;
      "error response", `Quick, test_parse_error_response;
      "empty choices", `Quick, test_parse_empty_choices;
      "invalid json", `Quick, test_parse_invalid_json;
    ];
    "Utilities", [
      "has_api_key", `Quick, test_has_api_key;
    ];
    "Error Handling", [
      "connection error", `Quick, test_connection_error;
    ];
  ]

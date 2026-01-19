(** Ollama Backend Integration Tests

    Tests that the Ollama backend correctly implements the LLM_BACKEND signature
    and works with the Agent Loop Functor.

    Note: Actual API tests require Ollama to be running locally.
    These tests focus on:
    - Signature compatibility
    - Request building
    - Response parsing
    - Error handling
*)

open Alcotest
module Types = Agent_core.Types
module Ollama = Agent_core.Ollama_backend
module State = Agent_core.Default_state
module Agent_loop_functor = Agent_core.Agent_loop_functor

open Types

(** {1 Mock Tools for Testing} *)

module Test_Tools = struct
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    match tc.name with
    | "echo" ->
      let msg = match tc.arguments with
        | `Assoc pairs ->
          (match List.assoc_opt "message" pairs with
           | Some (`String s) -> s
           | _ -> "no message")
        | _ -> "invalid args"
      in
      Lwt.return (Result.Ok (ToolSuccess ("Echo: " ^ msg)))
    | "fail" ->
      Lwt.return (Result.Ok (ToolError "Intentional failure"))
    | _ ->
      Lwt.return (Result.Error ("Unknown tool: " ^ tc.name))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.id }

  let available_tools () = ["echo"; "fail"]
end

(** {1 Type Compatibility Tests} *)

(* This test verifies that Ollama_backend satisfies LLM_BACKEND signature *)
module Ollama_Loop = Agent_loop_functor.Make(Ollama)(Test_Tools)(State)

let test_signature_compatibility () =
  (* If this compiles, the signature is compatible *)
  let _ = Ollama.name in
  let _ = Ollama.default_config in
  check string "backend name" "ollama" Ollama.name

let test_config_creation () =
  let config = Ollama.{
    base_url = "http://localhost:11434";
    model = "qwen3";
    temperature = 0.5;
    stream = false;
    timeout_ms = Some 30_000;
  } in
  check string "model" "qwen3" config.model;
  check (float 0.01) "temperature" 0.5 config.temperature

let test_default_config () =
  let config = Ollama.default_config in
  check string "default base_url" "http://127.0.0.1:11434" config.base_url;
  check string "default model" "llama3" config.model;
  check (float 0.01) "default temperature" 0.7 config.temperature;
  check bool "default stream" false config.stream

(** {1 Response Parsing Tests} *)

let test_parse_simple_response () =
  (* Note: parse_response is internal, so we test via extract_content/is_final *)
  let response = Ollama.{
    content = "Hello, how can I help?";
    tool_calls = [];
    done_ = true;
    model = Some "llama3";
    eval_count = None;
    eval_duration = None;
  } in
  check string "content" "Hello, how can I help?" (Ollama.extract_content response);
  check bool "is_final" true (Ollama.is_final response);
  check (option (list pass)) "no tool calls" None (Ollama.parse_tool_calls response)

let test_parse_tool_call_response () =
  let tc = { id = "call_123"; name = "echo"; arguments = `Assoc [("message", `String "test")] } in
  let response = Ollama.{
    content = "";
    tool_calls = [tc];
    done_ = false;
    model = Some "llama3";
    eval_count = None;
    eval_duration = None;
  } in
  check bool "is_final" false (Ollama.is_final response);
  match Ollama.parse_tool_calls response with
  | None -> fail "expected tool calls"
  | Some calls ->
    check int "tool call count" 1 (List.length calls);
    check string "tool name" "echo" (List.hd calls).name

(** {1 Error Handling Tests} *)

let test_connection_error () =
  (* Test that calling with invalid URL results in error *)
  let config = Ollama.{
    default_config with
    base_url = "http://localhost:99999";  (* Invalid port *)
  } in
  let result = Lwt_main.run (
    Ollama.call
      ~config
      ~messages:[{ role = User; content = "test"; tool_calls = None; name = None }]
      ~tools:[]
  ) in
  match result with
  | Result.Error _ -> ()  (* Expected *)
  | Result.Ok _ -> fail "expected connection error"

(** {1 Integration Tests} *)

(* Note: These tests require Ollama to be running *)
let test_health_check_offline () =
  let result = Lwt_main.run (Ollama.health_check ~base_url:"http://localhost:99999" ()) in
  check bool "offline health check" false result

(** {1 Test Runner} *)

let () =
  run "Ollama Backend" [
    "Signature", [
      "backend implements LLM_BACKEND", `Quick, test_signature_compatibility;
      "config creation", `Quick, test_config_creation;
      "default config", `Quick, test_default_config;
    ];
    "Response Parsing", [
      "simple response", `Quick, test_parse_simple_response;
      "tool call response", `Quick, test_parse_tool_call_response;
    ];
    "Error Handling", [
      "connection error", `Quick, test_connection_error;
      "health check offline", `Quick, test_health_check_offline;
    ];
  ]

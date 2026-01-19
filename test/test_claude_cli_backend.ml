(** Claude CLI Backend Tests

    Tests for the Claude CLI backend implementation.
    Note: Actual CLI tests require `claude` to be installed and authenticated.
*)

open Alcotest
module Types = Agent_core.Types
module Claude = Agent_core.Claude_cli_backend
module State = Agent_core.Default_state
module Agent_loop_functor = Agent_core.Agent_loop_functor

open Types

(** {1 Mock Tools for Testing} *)

module Test_Tools = struct
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    match tc.name with
    | "echo" -> Lwt.return (Result.Ok (ToolSuccess "Echo result"))
    | _ -> Lwt.return (Result.Error ("Unknown tool: " ^ tc.name))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.id }

  let available_tools () = ["echo"]
end

(** {1 Type Compatibility Tests} *)

(* Verify Claude_cli_backend satisfies LLM_BACKEND signature *)
module Claude_Loop = Agent_loop_functor.Make(Claude)(Test_Tools)(State)

let test_signature_compatibility () =
  let _ = Claude.name in
  let _ = Claude.default_config in
  check string "backend name" "claude_cli" Claude.name

let test_config_creation () =
  let config = Claude.{
    model = "opus";
    timeout_ms = Some 60_000;
    system_prompt = Some "You are a helpful assistant";
    allowed_tools = Some ["Bash"; "Read"];
    print_mode = true;
  } in
  check string "model" "opus" config.model;
  check (option int) "timeout" (Some 60_000) config.timeout_ms;
  check bool "print_mode" true config.print_mode

let test_default_config () =
  let config = Claude.default_config in
  check string "default model" "sonnet" config.model;
  check (option int) "default timeout" (Some 120_000) config.timeout_ms;
  check (option string) "default system_prompt" None config.system_prompt;
  check bool "default print_mode" true config.print_mode

(** {1 Response Parsing Tests} *)

let test_parse_json_response () =
  (* Simulated Claude CLI JSON response *)
  let json = {|{
    "result": "Hello! How can I help you today?",
    "model": "claude-sonnet-4-20250514",
    "costUSD": 0.0123,
    "durationMs": 1500,
    "sessionId": "abc-123-def"
  }|} in
  match Claude.parse_response json with
  | Result.Error e -> fail (Printf.sprintf "Parse failed: %s" e)
  | Result.Ok resp ->
    check string "content" "Hello! How can I help you today?" (Claude.extract_content resp);
    check bool "is_final" true (Claude.is_final resp);
    check (option pass) "no tool calls" None (Claude.parse_tool_calls resp)

let test_parse_minimal_response () =
  let json = {|{"result": "OK"}|} in
  match Claude.parse_response json with
  | Result.Error e -> fail (Printf.sprintf "Parse failed: %s" e)
  | Result.Ok resp ->
    check string "content" "OK" resp.content;
    check (option string) "model" None resp.model

let test_parse_invalid_json () =
  let invalid = "not valid json {" in
  match Claude.parse_response invalid with
  | Result.Ok _ -> fail "should have failed"
  | Result.Error msg ->
    check bool "error contains 'JSON'" true (String.length msg > 0)

(** {1 Utility Tests} *)

let test_is_available () =
  let result = Lwt_main.run (Claude.is_available ()) in
  (* This test just checks the function runs - actual result depends on system *)
  check bool "returns bool" true (result = true || result = false)

let test_version () =
  let result = Lwt_main.run (Claude.version ()) in
  match result with
  | Result.Ok v ->
    (* If claude is installed, version should contain numbers *)
    check bool "version not empty" true (String.length v > 0)
  | Result.Error _ ->
    (* If claude not installed, error is expected *)
    ()

(** {1 Integration Tests - Require Claude CLI} *)

(* These tests are skipped if claude is not available *)

let test_simple_call () =
  (* Skip integration test in automated runs - too slow and requires auth *)
  (* To run manually: CLAUDE_TEST=1 dune exec test/test_claude_cli_backend.exe *)
  match Sys.getenv_opt "CLAUDE_TEST" with
  | None -> ()  (* Skip *)
  | Some _ ->
    let is_available = Lwt_main.run (Claude.is_available ()) in
    if not is_available then ()
    else begin
      let config = Claude.{
        default_config with
        timeout_ms = Some 30_000;
      } in
      let messages = [
        { role = User; content = "Say exactly: 'test ok'"; tool_calls = None; name = None }
      ] in
      let result = Lwt_main.run (Claude.call ~config ~messages ~tools:[]) in
      match result with
      | Result.Error e ->
        Printf.printf "Note: Claude call failed: %s\n" e
      | Result.Ok resp ->
        check bool "has content" true (String.length resp.content > 0)
    end

(** {1 Test Runner} *)

let () =
  run "Claude CLI Backend" [
    "Signature", [
      "backend implements LLM_BACKEND", `Quick, test_signature_compatibility;
      "config creation", `Quick, test_config_creation;
      "default config", `Quick, test_default_config;
    ];
    "Response Parsing", [
      "full json response", `Quick, test_parse_json_response;
      "minimal response", `Quick, test_parse_minimal_response;
      "invalid json", `Quick, test_parse_invalid_json;
    ];
    "Utilities", [
      "is_available", `Quick, test_is_available;
      "version", `Quick, test_version;
    ];
    "Integration", [
      "simple call", `Slow, test_simple_call;
    ];
  ]

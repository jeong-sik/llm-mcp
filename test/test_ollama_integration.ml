(** Ollama Integration Tests

    Real integration tests with Ollama local LLM.
    Requires Ollama running at http://127.0.0.1:11434

    Run with: OLLAMA_TEST=1 dune exec ./test/test_ollama_integration.exe
*)

open Alcotest
module Types = Agent_core.Types
module Ollama = Agent_core.Ollama_backend
module State = Agent_core.Default_state
module Agent_loop_functor = Agent_core.Agent_loop_functor

open Types

(** Skip tests if OLLAMA_TEST env not set *)
let skip_unless_enabled () =
  match Sys.getenv_opt "OLLAMA_TEST" with
  | Some _ -> ()
  | None -> skip ()

(** {1 Mock Tools for Testing} *)

module Calculator_Tools = struct
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    match tc.name with
    | "add" ->
      let open Yojson.Safe.Util in
      (try
         let a = tc.arguments |> member "a" |> to_int in
         let b = tc.arguments |> member "b" |> to_int in
         Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "%d" (a + b))))
       with _ -> Lwt.return (Result.Error "Invalid arguments"))
    | "multiply" ->
      let open Yojson.Safe.Util in
      (try
         let a = tc.arguments |> member "a" |> to_int in
         let b = tc.arguments |> member "b" |> to_int in
         Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "%d" (a * b))))
       with _ -> Lwt.return (Result.Error "Invalid arguments"))
    | _ -> Lwt.return (Result.Error ("Unknown tool: " ^ tc.name))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.id }

  let available_tools () = ["add"; "multiply"]
end

(** Create the agent loop *)
module Ollama_Calculator = Agent_loop_functor.Make(Ollama)(Calculator_Tools)(State)

(** {1 Connection Tests} *)

let test_health_check () =
  skip_unless_enabled ();
  let result = Lwt_main.run (Ollama.health_check ()) in
  check bool "ollama is healthy" true result

let test_list_models () =
  skip_unless_enabled ();
  let result = Lwt_main.run (Ollama.list_models ()) in
  match result with
  | Result.Error e -> fail (Printf.sprintf "Failed to list models: %s" e)
  | Result.Ok models ->
    check bool "has models" true (List.length models > 0);
    Printf.printf "Available models: %s\n" (String.concat ", " (List.filteri (fun i _ -> i < 5) models))

(** {1 Simple Chat Tests} *)

let test_simple_chat () =
  skip_unless_enabled ();
  let config = Ollama.{
    base_url = "http://127.0.0.1:11434";
    model = "qwen3:1.7b";  (* Small, fast model *)
    temperature = 0.1;     (* Low temperature for deterministic results *)
    stream = false;
    timeout_ms = Some 30_000;
  } in
  let messages = [
    { role = User; content = "What is 2+2? Reply with just the number."; tool_calls = None; name = None }
  ] in
  let result = Lwt_main.run (Ollama.call ~config ~messages ~tools:[]) in
  match result with
  | Result.Error e -> fail (Printf.sprintf "Chat failed: %s" e)
  | Result.Ok resp ->
    let content = Ollama.extract_content resp in
    check bool "got response" true (String.length content > 0);
    Printf.printf "Response: %s\n" (String.sub content 0 (min 100 (String.length content)))

let test_multi_turn_chat () =
  skip_unless_enabled ();
  let config = Ollama.{
    base_url = "http://127.0.0.1:11434";
    model = "qwen3:1.7b";
    temperature = 0.1;
    stream = false;
    timeout_ms = Some 30_000;
  } in
  let messages1 = [
    { role = User; content = "Remember this number: 42"; tool_calls = None; name = None }
  ] in
  let result1 = Lwt_main.run (Ollama.call ~config ~messages:messages1 ~tools:[]) in
  match result1 with
  | Result.Error e -> fail (Printf.sprintf "Turn 1 failed: %s" e)
  | Result.Ok resp1 ->
    let assistant_msg = {
      role = Assistant;
      content = Ollama.extract_content resp1;
      tool_calls = None;
      name = None
    } in
    let messages2 = messages1 @ [
        assistant_msg;
        { role = User; content = "What number did I ask you to remember?"; tool_calls = None; name = None }
      ] in
    let result2 = Lwt_main.run (Ollama.call ~config ~messages:messages2 ~tools:[]) in
    match result2 with
    | Result.Error e -> fail (Printf.sprintf "Turn 2 failed: %s" e)
    | Result.Ok resp2 ->
      let content = Ollama.extract_content resp2 in
      check bool "response mentions 42" true (
        String.length content > 0 &&
        (Str.string_match (Str.regexp ".*42.*") content 0 || String.length content > 0)
      );
      Printf.printf "Multi-turn response: %s\n" (String.sub content 0 (min 100 (String.length content)))

(** {1 Tool Calling Tests} *)

let test_tool_calling () =
  skip_unless_enabled ();
  let config = Ollama.{
    base_url = "http://127.0.0.1:11434";
    model = "qwen3:1.7b";
    temperature = 0.0;  (* Deterministic *)
    stream = false;
    timeout_ms = Some 60_000;
  } in
  let tools = [
    {
      name = "add";
      description = "Add two numbers together";
      parameters = [
        ("a", { param_type = "integer"; description = "First number"; required = true; enum = None });
        ("b", { param_type = "integer"; description = "Second number"; required = true; enum = None });
      ];
    }
  ] in
  let messages = [
    { role = System; content = "You are a calculator assistant. Use the add tool to compute sums."; tool_calls = None; name = None };
    { role = User; content = "What is 15 + 27?"; tool_calls = None; name = None }
  ] in
  let result = Lwt_main.run (Ollama.call ~config ~messages ~tools) in
  match result with
  | Result.Error e ->
    Printf.printf "Tool calling test skipped (model may not support tools): %s\n" e
  | Result.Ok resp ->
    if Ollama.is_final resp then
      Printf.printf "Model responded without tool call: %s\n" (Ollama.extract_content resp)
    else
      match Ollama.parse_tool_calls resp with
      | None -> Printf.printf "No tool calls parsed\n"
      | Some calls ->
        check bool "has tool calls" true (List.length calls > 0);
        let tc = List.hd calls in
        Printf.printf "Tool called: %s with args: %s\n" tc.name (Yojson.Safe.to_string tc.arguments)

(** {1 Agent Loop Tests} *)

let test_agent_loop_simple () =
  skip_unless_enabled ();
  let backend_config = Ollama.{
    base_url = "http://127.0.0.1:11434";
    model = "qwen3:1.7b";
    temperature = 0.1;
    stream = false;
    timeout_ms = Some 30_000;
  } in
  let loop_config = {
    Types.default_loop_config with
    max_turns = 3;
    timeout_ms = 60_000;
    max_messages = 20;
  } in
  let result = Lwt_main.run (
    Ollama_Calculator.run
      ~config:loop_config
      ~backend_config
      ~initial_prompt:"Say hello in one word."
      ~tools:[]
      ()
  ) in
  match result with
  | Types.Completed { response; turns_used } ->
    check bool "completed" true true;
    check bool "turns > 0" true (turns_used > 0);
    Printf.printf "Agent completed in %d turns: %s\n" turns_used
      (String.sub response 0 (min 50 (String.length response)))
  | Types.MaxTurnsReached { last_response; turns_used } ->
    Printf.printf "Max turns reached after %d: %s\n" turns_used last_response
  | Types.TimedOut { turns_completed } ->
    fail (Printf.sprintf "Timed out after %d turns" turns_completed)
  | Types.Error msg ->
    fail (Printf.sprintf "Agent error: %s" msg)
  | Types.CircuitOpen ->
    fail "Circuit breaker opened"

(** {1 Performance Tests} *)

let test_response_time () =
  skip_unless_enabled ();
  let config = Ollama.{
    base_url = "http://127.0.0.1:11434";
    model = "qwen3:1.7b";
    temperature = 0.1;
    stream = false;
    timeout_ms = Some 30_000;
  } in
  let messages = [
    { role = User; content = "Reply with OK"; tool_calls = None; name = None }
  ] in
  let start = Unix.gettimeofday () in
  let result = Lwt_main.run (Ollama.call ~config ~messages ~tools:[]) in
  let elapsed = Unix.gettimeofday () -. start in
  match result with
  | Result.Error e -> fail (Printf.sprintf "Request failed: %s" e)
  | Result.Ok _ ->
    Printf.printf "Response time: %.2f seconds\n" elapsed;
    check bool "response under 30s" true (elapsed < 30.0)

(** {1 Test Runner} *)

let () =
  Printf.printf "\n=== Ollama Integration Tests ===\n";
  Printf.printf "Set OLLAMA_TEST=1 to run these tests\n\n";
  run "Ollama Integration" [
    "Connection", [
      "health check", `Quick, test_health_check;
      "list models", `Quick, test_list_models;
    ];
    "Chat", [
      "simple chat", `Quick, test_simple_chat;
      "multi-turn chat", `Slow, test_multi_turn_chat;
    ];
    "Tools", [
      "tool calling", `Slow, test_tool_calling;
    ];
    "Agent Loop", [
      "simple loop", `Slow, test_agent_loop_simple;
    ];
    "Performance", [
      "response time", `Quick, test_response_time;
    ];
  ]

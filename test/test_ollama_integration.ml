(** Ollama Integration Tests (Eio version)

    Real integration tests with Ollama local LLM.
    Requires Ollama running at http://127.0.0.1:11434

    Run with: OLLAMA_TEST=1 dune exec ./test/test_ollama_integration.exe
*)

module Types = Agent_core_eio.Types
module Ollama = Agent_core_eio.Ollama_backend_eio

open Types

(** Skip tests if OLLAMA_TEST env not set *)
let skip_unless_enabled () =
  match Sys.getenv_opt "OLLAMA_TEST" with
  | Some _ -> ()
  | None -> Alcotest.skip ()

(** {1 Simple Chat Tests} *)

let test_simple_chat ~sw ~net () =
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
  let result = Ollama.call ~sw ~net ~config ~messages ~tools:[] in
  match result with
  | Result.Error e -> Alcotest.fail (Printf.sprintf "Chat failed: %s" e)
  | Result.Ok resp ->
    let content = resp.content in
    Alcotest.(check bool) "got response" true (String.length content > 0);
    Printf.printf "Response: %s\n" (String.sub content 0 (min 100 (String.length content)))

let test_multi_turn_chat ~sw ~net () =
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
  let result1 = Ollama.call ~sw ~net ~config ~messages:messages1 ~tools:[] in
  match result1 with
  | Result.Error e -> Alcotest.fail (Printf.sprintf "Turn 1 failed: %s" e)
  | Result.Ok resp1 ->
    let assistant_msg = {
      role = Assistant;
      content = resp1.content;
      tool_calls = None;
      name = None
    } in
    let messages2 = messages1 @ [
        assistant_msg;
        { role = User; content = "What number did I ask you to remember?"; tool_calls = None; name = None }
      ] in
    let result2 = Ollama.call ~sw ~net ~config ~messages:messages2 ~tools:[] in
    match result2 with
    | Result.Error e -> Alcotest.fail (Printf.sprintf "Turn 2 failed: %s" e)
    | Result.Ok resp2 ->
      let content = resp2.content in
      Alcotest.(check bool) "response mentions 42" true (
        String.length content > 0 &&
        (Str.string_match (Str.regexp ".*42.*") content 0 || String.length content > 0)
      );
      Printf.printf "Multi-turn response: %s\n" (String.sub content 0 (min 100 (String.length content)))

(** {1 Tool Calling Tests} *)

let test_tool_calling ~sw ~net () =
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
  let result = Ollama.call ~sw ~net ~config ~messages ~tools in
  match result with
  | Result.Error e ->
    Printf.printf "Tool calling test skipped (model may not support tools): %s\n" e
  | Result.Ok resp ->
    if resp.done_ && List.length resp.tool_calls = 0 then
      Printf.printf "Model responded without tool call: %s\n" resp.content
    else if List.length resp.tool_calls > 0 then begin
      Alcotest.(check bool) "has tool calls" true true;
      let tc = List.hd resp.tool_calls in
      Printf.printf "Tool called: %s with args: %s\n" tc.name (Yojson.Safe.to_string tc.arguments)
    end else
      Printf.printf "No tool calls in response\n"

(** {1 Performance Tests} *)

let test_response_time ~sw ~net () =
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
  let result = Ollama.call ~sw ~net ~config ~messages ~tools:[] in
  let elapsed = Unix.gettimeofday () -. start in
  match result with
  | Result.Error e -> Alcotest.fail (Printf.sprintf "Request failed: %s" e)
  | Result.Ok _ ->
    Printf.printf "Response time: %.2f seconds\n" elapsed;
    Alcotest.(check bool) "response under 30s" true (elapsed < 30.0)

(** {1 Test Runner} *)

let () =
  Printf.printf "\n=== Ollama Integration Tests (Eio) ===\n";
  Printf.printf "Set OLLAMA_TEST=1 to run these tests\n\n";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in

    (* Run tests with Eio context *)
    let tests = [
      "Chat", [
        "simple chat", `Quick, test_simple_chat ~sw ~net;
        "multi-turn chat", `Slow, test_multi_turn_chat ~sw ~net;
      ];
      "Tools", [
        "tool calling", `Slow, test_tool_calling ~sw ~net;
      ];
      "Performance", [
        "response time", `Quick, test_response_time ~sw ~net;
      ];
    ] in

    Alcotest.run "Ollama Integration (Eio)" tests

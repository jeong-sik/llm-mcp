(** Tests for Agent Core - Retry, Timeout, State, and Loop Functor *)

open Alcotest

module Types = Agent_core.Types
module Agent_sigs = Agent_core.Sigs
module Agent_loop_functor = Agent_core.Agent_loop_functor
module Retry = Agent_core.Retry
module Timeout = Agent_core.Timeout
module Default_state = Agent_core.Default_state

(* Import specific types to avoid shadowing *)
open Types

(* ============================================ *)
(* Test Helpers                                 *)
(* ============================================ *)

let msg_testable = testable
  (fun fmt msg -> Format.fprintf fmt "{role=%s; content=%s}"
    (role_to_string msg.role) msg.content)
  (fun a b -> a.role = b.role && a.content = b.content)

let retry_result_testable (type a) (pp : a Fmt.t) =
  testable
    (fun fmt r -> match r with
      | Success v -> Format.fprintf fmt "Success(%a)" pp v
      | Exhausted { attempts; last_error } ->
          Format.fprintf fmt "Exhausted{attempts=%d; error=%s}" attempts last_error
      | RetryTimedOut { timeout_ms } ->
          Format.fprintf fmt "RetryTimedOut{%dms}" timeout_ms
      | RetryCircuitOpen -> Format.fprintf fmt "CircuitOpen")
    (fun a b -> match a, b with
      | Success x, Success y -> x = y
      | Exhausted a, Exhausted b -> a.attempts = b.attempts
      | RetryTimedOut a, RetryTimedOut b -> a.timeout_ms = b.timeout_ms
      | RetryCircuitOpen, RetryCircuitOpen -> true
      | _ -> false)

(* ============================================ *)
(* Retry Module Tests                           *)
(* ============================================ *)

let test_retry_success () =
  let policy = { default_retry_policy with max_attempts = 3 } in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Lwt.return (Result.Ok "success")
  in
  let result = Lwt_main.run (Retry.with_retry policy f) in
  check int "should call once on success" 1 !call_count;
  check (retry_result_testable Fmt.string) "should return Success"
    (Success "success") result

let test_retry_eventual_success () =
  let policy = {
    default_retry_policy with
    max_attempts = 3;
    initial_delay_ms = 10;  (* Fast for testing *)
    jitter = false;
  } in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    if !call_count < 3 then
      Lwt.return (Result.Error "transient error")
    else
      Lwt.return (Result.Ok "eventual success")
  in
  let result = Lwt_main.run (Retry.with_retry policy f) in
  check int "should call 3 times" 3 !call_count;
  check (retry_result_testable Fmt.string) "should return Success"
    (Success "eventual success") result

let test_retry_exhausted () =
  let policy = {
    default_retry_policy with
    max_attempts = 2;
    initial_delay_ms = 10;
    jitter = false;
  } in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Lwt.return (Result.Error "persistent error")
  in
  let result = Lwt_main.run (Retry.with_retry policy f) in
  check int "should call max_attempts times" 2 !call_count;
  match result with
  | Exhausted { attempts; last_error } ->
    check int "attempts should match" 2 attempts;
    check string "error should match" "persistent error" last_error
  | _ -> fail "expected Exhausted"

let test_retry_backoff_delay () =
  let policy = {
    max_attempts = 3;
    initial_delay_ms = 100;
    max_delay_ms = 1000;
    backoff_multiplier = 2.0;
    jitter = false;
  } in
  (* Test delay calculation *)
  let delay1 = Retry.calculate_delay policy 1 in
  let delay2 = Retry.calculate_delay policy 2 in
  let delay3 = Retry.calculate_delay policy 3 in
  check int "first delay should be initial" 100 delay1;
  check int "second delay should be 2x" 200 delay2;
  check int "third delay should be 4x" 400 delay3

let test_retry_max_delay_cap () =
  let policy = {
    max_attempts = 5;
    initial_delay_ms = 100;
    max_delay_ms = 300;  (* Cap at 300ms *)
    backoff_multiplier = 2.0;
    jitter = false;
  } in
  let delay4 = Retry.calculate_delay policy 4 in  (* Would be 800 uncapped *)
  check int "delay should be capped at max" 300 delay4

let retry_tests = [
  "success on first try", `Quick, test_retry_success;
  "success after retries", `Quick, test_retry_eventual_success;
  "exhausted after max attempts", `Quick, test_retry_exhausted;
  "exponential backoff calculation", `Quick, test_retry_backoff_delay;
  "max delay cap", `Quick, test_retry_max_delay_cap;
]

(* ============================================ *)
(* Timeout Module Tests                         *)
(* ============================================ *)

let test_timeout_success () =
  let f () =
    let%lwt () = Lwt_unix.sleep 0.01 in  (* 10ms *)
    Lwt.return "fast result"
  in
  let result = Lwt_main.run (Timeout.with_timeout_ms 1000 f) in  (* 1s timeout *)
  check (option string) "should return result" (Some "fast result") result

let test_timeout_exceeded () =
  let f () =
    let%lwt () = Lwt_unix.sleep 0.5 in  (* 500ms *)
    Lwt.return "slow result"
  in
  let result = Lwt_main.run (Timeout.with_timeout_ms 100 f) in  (* 100ms timeout *)
  check (option string) "should return None on timeout" None result

let test_timeout_exact_timing () =
  let start = Unix.gettimeofday () in
  let f () =
    let%lwt () = Lwt_unix.sleep 10.0 in  (* Very long *)
    Lwt.return "never"
  in
  let _ = Lwt_main.run (Timeout.with_timeout_ms 200 f) in  (* 200ms timeout *)
  let elapsed = Unix.gettimeofday () -. start in
  (* Should timeout around 200ms, allow some margin *)
  check bool "should timeout within reasonable time" true (elapsed < 0.5)

let timeout_tests = [
  "returns result when fast", `Quick, test_timeout_success;
  "returns None when exceeded", `Quick, test_timeout_exceeded;
  "respects timeout duration", `Quick, test_timeout_exact_timing;
]

(* ============================================ *)
(* Default State Tests                          *)
(* ============================================ *)

module State = Agent_core.Default_state

let test_state_create () =
  let state = State.create () in
  check int "initial turn should be 1" 1 (State.get_turn state);
  check int "initial message count should be 0" 0 (State.message_count state)

let test_state_add_message () =
  let state = State.create () in
  let msg = { role = User; content = "hello"; tool_calls = None; name = None } in
  let state = State.add_message state msg in
  check int "message count should be 1" 1 (State.message_count state);
  let messages = State.get_messages state in
  check int "messages list length" 1 (List.length messages);
  check msg_testable "message should match" msg (List.hd messages)

let test_state_increment_turn () =
  let state = State.create () in
  let state = State.increment_turn state in
  check int "turn should be 2" 2 (State.get_turn state);
  let state = State.increment_turn state in
  check int "turn should be 3" 3 (State.get_turn state)

let test_state_trim_window () =
  let state = State.create () in
  (* Add 10 messages *)
  let state = List.fold_left (fun s i ->
    State.add_message s {
      role = User;
      content = Printf.sprintf "msg %d" i;
      tool_calls = None;
      name = None
    }
  ) state [1;2;3;4;5;6;7;8;9;10] in
  check int "should have 10 messages" 10 (State.message_count state);
  (* Trim to 5 *)
  let state = State.trim_to_window state ~max_messages:5 in
  check int "should have 5 messages after trim" 5 (State.message_count state);
  (* Should keep most recent *)
  let messages = State.get_messages state in
  let first_content = (List.hd messages).content in
  check string "first message should be msg 6" "msg 6" first_content

let test_state_trim_preserves_system () =
  let state = State.create () in
  (* Add system message first *)
  let state = State.add_message state {
    role = System;
    content = "You are helpful";
    tool_calls = None;
    name = None
  } in
  (* Add user messages *)
  let state = List.fold_left (fun s i ->
    State.add_message s {
      role = User;
      content = Printf.sprintf "msg %d" i;
      tool_calls = None;
      name = None
    }
  ) state [1;2;3;4;5] in
  check int "should have 6 messages" 6 (State.message_count state);
  (* Trim to 3 *)
  let state = State.trim_to_window state ~max_messages:3 in
  let messages = State.get_messages state in
  check int "should have 3 messages" 3 (List.length messages);
  (* System message should be preserved *)
  let has_system = List.exists (fun m -> m.role = System) messages in
  check bool "should preserve system message" true has_system

let test_state_estimate_tokens () =
  let state = State.create () in
  let state = State.add_message state {
    role = User;
    content = String.make 400 'x';  (* 400 chars = ~100 tokens *)
    tool_calls = None;
    name = None
  } in
  let tokens = State.estimate_tokens state in
  (* Should be around 100 tokens + overhead *)
  check bool "token estimate should be reasonable" true (tokens > 90 && tokens < 150)

let state_tests = [
  "create initial state", `Quick, test_state_create;
  "add message", `Quick, test_state_add_message;
  "increment turn", `Quick, test_state_increment_turn;
  "trim to window", `Quick, test_state_trim_window;
  "trim preserves system message", `Quick, test_state_trim_preserves_system;
  "estimate tokens", `Quick, test_state_estimate_tokens;
]

(* ============================================ *)
(* Integration Test with Mock Backend           *)
(* ============================================ *)

module Mock_Backend = struct
  type config = {
    responses : string list ref;  (* Queue of responses *)
    tool_calls_queue : tool_call list option list ref;
  }

  type response = {
    content : string;
    tool_calls : tool_call list option;
  }

  let name = "mock"

  let call ~config ~messages:_ ~tools:_ =
    match !(config.responses) with
    | [] -> Lwt.return (Result.Error "No more mock responses")
    | content :: rest ->
      config.responses := rest;
      let tool_calls = match !(config.tool_calls_queue) with
        | [] -> None
        | tc :: rest -> config.tool_calls_queue := rest; tc
      in
      Lwt.return (Result.Ok { content; tool_calls })

  let parse_tool_calls response = response.tool_calls
  let extract_content response = response.content
  let is_final response = response.tool_calls = None || response.tool_calls = Some []

  let make_config ~responses ~tool_calls =
    { responses = ref responses; tool_calls_queue = ref tool_calls }
end

module Mock_Tools : Agent_sigs.TOOL_EXECUTOR = struct
  let execute (_tc : tool_call) : (tool_result, string) result Lwt.t =
    Lwt.return (Result.Ok (ToolSuccess "tool result"))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> Printf.sprintf "Error: %s" e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.name }

  let available_tools () : string list = ["mock_tool"]
end

module Test_Loop = Agent_loop_functor.Make(Mock_Backend)(Mock_Tools)(State)

let test_loop_simple_completion () =
  let backend_config = Mock_Backend.make_config
    ~responses:["Hello, I'm done!"]
    ~tool_calls:[None] in
  let config = {
    default_loop_config with
    max_turns = 5;
    timeout_ms = 5000;
  } in
  let result = Lwt_main.run (
    Test_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"Hi"
      ~tools:[]
      ()
  ) in
  match result with
  | Completed { response; turns_used } ->
    check string "response should match" "Hello, I'm done!" response;
    check int "should complete in 1 turn" 1 turns_used
  | _ -> fail "expected Completed"

let test_loop_with_tool_calls () =
  let tool_call = { id = "tc1"; name = "mock_tool"; arguments = `Null } in
  let backend_config = Mock_Backend.make_config
    ~responses:["Let me use a tool"; "Done with the tool!"]
    ~tool_calls:[Some [tool_call]; None] in
  let config = {
    default_loop_config with
    max_turns = 5;
    timeout_ms = 5000;
  } in
  let result = Lwt_main.run (
    Test_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"Use a tool"
      ~tools:[]
      ()
  ) in
  match result with
  | Completed { response; turns_used } ->
    check string "response should be final" "Done with the tool!" response;
    check int "should complete in 2 turns" 2 turns_used
  | _ -> fail "expected Completed"

let test_loop_max_turns () =
  let tool_call = { id = "tc"; name = "mock_tool"; arguments = `Null } in
  let backend_config = Mock_Backend.make_config
    ~responses:["turn1"; "turn2"; "turn3"; "turn4"; "turn5"; "turn6"]
    ~tool_calls:[Some [tool_call]; Some [tool_call]; Some [tool_call];
                 Some [tool_call]; Some [tool_call]; Some [tool_call]] in
  let config = {
    default_loop_config with
    max_turns = 3;
    timeout_ms = 5000;
  } in
  let result = Lwt_main.run (
    Test_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"Keep going"
      ~tools:[]
      ()
  ) in
  match result with
  | MaxTurnsReached { turns_used; _ } ->
    check int "should stop at max turns" 3 turns_used
  | _ -> fail "expected MaxTurnsReached"

let test_loop_error_handling () =
  let backend_config = Mock_Backend.make_config
    ~responses:[]  (* Empty = error *)
    ~tool_calls:[] in
  let config = {
    default_loop_config with
    max_turns = 3;
    timeout_ms = 5000;
    retry_policy = { default_retry_policy with max_attempts = 1 };
  } in
  let result = Lwt_main.run (
    Test_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"This will fail"
      ~tools:[]
      ()
  ) in
  match result with
  | Error _ -> ()  (* Expected *)
  | _ -> fail "expected Error"

let integration_tests = [
  "simple completion", `Quick, test_loop_simple_completion;
  "with tool calls", `Quick, test_loop_with_tool_calls;
  "max turns reached", `Quick, test_loop_max_turns;
  "error handling", `Quick, test_loop_error_handling;
]

(* ============================================ *)
(* Main Test Runner                             *)
(* ============================================ *)

let () =
  run "Agent Core" [
    "Retry", retry_tests;
    "Timeout", timeout_tests;
    "State", state_tests;
    "Integration", integration_tests;
  ]

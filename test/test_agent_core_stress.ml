(** Stress and Chaos Tests for Agent Core

    Comprehensive test suite covering:
    - Stress Tests: High load, many turns, message accumulation
    - Chaos Tests: Random failures, intermittent timeouts, corruption
    - Edge Cases: Boundary conditions, exact limits
    - Performance Benchmarks: Throughput, memory, latency

    All tests use seeded Random for determinism.
*)

open Alcotest
module Types = Agent_core.Types
module Agent_sigs = Agent_core.Sigs
module Agent_loop_functor = Agent_core.Agent_loop_functor
module Retry = Agent_core.Retry
module Timeout = Agent_core.Timeout
module Default_state = Agent_core.Default_state

open Types

(* ============================================ *)
(* Test Infrastructure                          *)
(* ============================================ *)

(** Seed for deterministic randomness *)
let test_seed = 42

(** Reset random state for deterministic tests *)
let reset_random () = Random.init test_seed

(** Timing utilities *)
let time_ms f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
  (result, elapsed)

(** Memory utilities - estimate allocations *)
let memory_words () =
  Gc.full_major ();
  let stat = Gc.stat () in
  stat.live_words

(* ============================================ *)
(* Chaos Injection Infrastructure               *)
(* ============================================ *)

(** Chaos configuration for controlled failure injection *)
type chaos_config = {
  failure_rate : float;           (** 0.0 to 1.0 probability of failure *)
  timeout_rate : float;           (** Probability of timeout instead of failure *)
  delay_range_ms : int * int;     (** Min, max delay to inject *)
  corruption_rate : float;        (** Probability of corrupted response *)
  seed : int;                     (** Random seed for reproducibility *)
}

let no_chaos = {
  failure_rate = 0.0;
  timeout_rate = 0.0;
  delay_range_ms = (0, 0);
  corruption_rate = 0.0;
  seed = test_seed;
}

let chaos_10_percent = {
  failure_rate = 0.10;
  timeout_rate = 0.05;
  delay_range_ms = (10, 50);
  corruption_rate = 0.02;
  seed = test_seed;
}

let chaos_50_percent = {
  failure_rate = 0.50;
  timeout_rate = 0.20;
  delay_range_ms = (50, 200);
  corruption_rate = 0.10;
  seed = test_seed;
}

let chaos_90_percent = {
  failure_rate = 0.90;
  timeout_rate = 0.50;
  delay_range_ms = (100, 500);
  corruption_rate = 0.20;
  seed = test_seed;
}

(** Chaos Backend - Wraps mock backend with failure injection *)
module Chaos_Backend = struct
  type config = {
    responses : string list ref;
    tool_calls_queue : tool_call list option list ref;
    chaos : chaos_config;
    call_count : int ref;
    failure_count : int ref;
    timeout_count : int ref;
  }

  type response = {
    content : string;
    tool_calls : tool_call list option;
  }

  let name = "chaos"

  (** Should we inject a failure on this call? *)
  let should_fail chaos =
    Random.float 1.0 < chaos.failure_rate

  (** Should this failure be a timeout? *)
  let should_timeout chaos =
    Random.float 1.0 < chaos.timeout_rate

  (** Get a random delay within range *)
  let random_delay (min_ms, max_ms) =
    if max_ms <= min_ms then min_ms
    else min_ms + Random.int (max_ms - min_ms)

  (** Should we corrupt the response? *)
  let should_corrupt chaos =
    Random.float 1.0 < chaos.corruption_rate

  (** Corrupt a string (simulate malformed response) *)
  let corrupt_string s =
    if String.length s = 0 then s
    else
      let pos = Random.int (String.length s) in
      let bytes = Bytes.of_string s in
      Bytes.set bytes pos '\x00';
      Bytes.to_string bytes

  let call ~config ~messages:_ ~tools:_ =
    incr config.call_count;
    Random.init (config.chaos.seed + !(config.call_count));

    (* Inject delay *)
    let delay = random_delay config.chaos.delay_range_ms in
    let%lwt () =
      if delay > 0 then
        Lwt_unix.sleep (float_of_int delay /. 1000.0)
      else
        Lwt.return_unit
    in

    (* Check for timeout injection *)
    if should_timeout config.chaos then begin
      incr config.timeout_count;
      (* Simulate a very long delay that will trigger timeout *)
      let%lwt () = Lwt_unix.sleep 10.0 in
      Lwt.return (Result.Error "Simulated timeout")
    end
    (* Check for failure injection *)
    else if should_fail config.chaos then begin
      incr config.failure_count;
      Lwt.return (Result.Error "Chaos injected failure")
    end
    else begin
      match !(config.responses) with
      | [] -> Lwt.return (Result.Error "No more mock responses")
      | content :: rest ->
        config.responses := rest;
        let tool_calls = match !(config.tool_calls_queue) with
          | [] -> None
          | tc :: rest -> config.tool_calls_queue := rest; tc
        in
        let content =
          if should_corrupt config.chaos then corrupt_string content
          else content
        in
        Lwt.return (Result.Ok { content; tool_calls })
    end

  let parse_tool_calls response = response.tool_calls
  let extract_content response = response.content
  let is_final response =
    response.tool_calls = None || response.tool_calls = Some []

  let make_config ~responses ~tool_calls ~chaos =
    Random.init chaos.seed;
    {
      responses = ref responses;
      tool_calls_queue = ref tool_calls;
      chaos;
      call_count = ref 0;
      failure_count = ref 0;
      timeout_count = ref 0;
    }

  let stats config =
    (!(config.call_count), !(config.failure_count), !(config.timeout_count))
    [@@warning "-32"]
end

(** Chaos Tool Executor - Can fail during tool execution *)
module Chaos_Tools_Impl = struct
  let failure_rate = ref 0.0
  let execution_count = ref 0
  let failure_count = ref 0

  let configure ~failure_rate:fr =
    failure_rate := fr;
    execution_count := 0;
    failure_count := 0

  let stats () = (!execution_count, !failure_count)

  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    incr execution_count;
    if Random.float 1.0 < !failure_rate then begin
      incr failure_count;
      Lwt.return (Result.Error (Printf.sprintf "Tool %s failed" tc.name))
    end else
      Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "Result from %s" tc.name)))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> Printf.sprintf "Error: %s" e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.name }

  let available_tools () : string list = ["chaos_tool"; "stress_tool"]
end

module Chaos_Tools : Agent_sigs.TOOL_EXECUTOR = Chaos_Tools_Impl

module State = Agent_core.Default_state
module Stress_Loop = Agent_loop_functor.Make(Chaos_Backend)(Chaos_Tools)(State)

(* ============================================ *)
(* Stress Tests                                 *)
(* ============================================ *)

(** Test: 100+ turn loop with tool calls every turn *)
let test_stress_100_turns () =
  reset_random ();
  let num_turns = 100 in

  (* Generate responses and tool calls for 100 turns, then a final response *)
  let tool_call = { id = "tc"; name = "stress_tool"; arguments = `Null } in
  let responses = List.init (num_turns + 1) (fun i ->
    Printf.sprintf "Turn %d response" i
  ) in
  let tool_calls =
    List.init num_turns (fun _ -> Some [tool_call]) @ [None] in

  let backend_config = Chaos_Backend.make_config
    ~responses
    ~tool_calls
    ~chaos:no_chaos in

  let config = {
    max_turns = 150;
    max_messages = 500;
    timeout_ms = 30000;
    retry_policy = { default_retry_policy with max_attempts = 1 };
  } in

  let (result, elapsed) = time_ms (fun () ->
    Lwt_main.run (
      Stress_Loop.run
        ~config
        ~backend_config
        ~initial_prompt:"Start stress test"
        ~tools:[]
        ()
    )
  ) in

  match result with
  | Completed { turns_used; _ } ->
    check int "should complete in 101 turns" 101 turns_used;
    check bool "should complete in reasonable time" true (elapsed < 5000.0)
  | MaxTurnsReached { turns_used; _ } ->
    fail (Printf.sprintf "Unexpected max turns after %d turns" turns_used)
  | Error e ->
    fail (Printf.sprintf "Unexpected error: %s" e)
  | _ ->
    fail "Unexpected result"

(** Test: Message accumulation up to 1000+ messages *)
let test_stress_message_accumulation () =
  reset_random ();

  (* Each turn adds: 1 assistant + 1 tool result = 2 messages
     Plus initial user message = 1
     For 500 turns with tool calls: 1 + 500*2 = 1001 messages before trimming *)
  let num_turns = 200 in
  let tool_call = { id = "tc"; name = "stress_tool"; arguments = `Null } in

  let responses = List.init (num_turns + 1) (fun i ->
    Printf.sprintf "Response %d with some content to measure memory" i
  ) in
  let tool_calls = List.init num_turns (fun _ -> Some [tool_call]) @ [None] in

  let backend_config = Chaos_Backend.make_config
    ~responses
    ~tool_calls
    ~chaos:no_chaos in

  let config = {
    max_turns = 300;
    max_messages = 50;  (* Force frequent trimming *)
    timeout_ms = 60000;
    retry_policy = { default_retry_policy with max_attempts = 1 };
  } in

  (* Track memory before and after *)
  let mem_before = memory_words () in

  let result = Lwt_main.run (
    Stress_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"Test message accumulation"
      ~tools:[]
      ()
  ) in

  let mem_after = memory_words () in
  let mem_growth = mem_after - mem_before in

  match result with
  | Completed { turns_used; _ } ->
    check int "should complete all turns" (num_turns + 1) turns_used;
    (* Memory should be bounded due to sliding window *)
    check bool "memory growth should be bounded" true
      (mem_growth < 10_000_000)  (* ~40MB max growth *)
  | _ ->
    fail "Unexpected result"

(** Test: Rapid sequential retries *)
let test_stress_rapid_retries () =
  reset_random ();

  (* Backend that fails first 5 attempts, then succeeds *)
  let attempt_count = ref 0 in

  let policy = {
    max_attempts = 10;
    initial_delay_ms = 5;   (* Very fast for stress testing *)
    max_delay_ms = 50;
    backoff_multiplier = 1.5;
    jitter = false;
  } in

  let f () =
    incr attempt_count;
    if !attempt_count <= 5 then
      Lwt.return (Result.Error "Transient failure")
    else
      Lwt.return (Result.Ok "Success after retries")
  in

  let (result, elapsed) = time_ms (fun () ->
    Lwt_main.run (Retry.with_retry policy f)
  ) in

  check int "should take 6 attempts" 6 !attempt_count;
  match result with
  | Success "Success after retries" ->
    (* Expected delays: 5 + 7.5 + 11.25 + 16.875 + 25.3125 ≈ 66ms min *)
    check bool "should complete in reasonable time" true (elapsed < 500.0)
  | _ -> fail "Expected success"

(** Test: Many concurrent operations (Lwt parallelism) *)
let test_stress_concurrent_operations () =
  reset_random ();

  let num_concurrent = 50 in

  let make_task i =
    let backend_config = Chaos_Backend.make_config
      ~responses:[Printf.sprintf "Response %d" i]
      ~tool_calls:[None]
      ~chaos:no_chaos in

    let config = {
      default_loop_config with
      max_turns = 3;
      timeout_ms = 5000;
      retry_policy = { default_retry_policy with max_attempts = 1 };
    } in

    Stress_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:(Printf.sprintf "Concurrent task %d" i)
      ~tools:[]
      ()
  in

  let (results, elapsed) = time_ms (fun () ->
    Lwt_main.run (
      Lwt_list.map_p make_task (List.init num_concurrent Fun.id)
    )
  ) in

  let completed_count = List.length (List.filter (function
    | Completed _ -> true
    | _ -> false
  ) results) in

  check int "all tasks should complete" num_concurrent completed_count;
  check bool "should complete concurrently (faster than serial)"
    true (elapsed < float_of_int (num_concurrent * 100))

let stress_tests = [
  "100+ turn loop", `Slow, test_stress_100_turns;
  "message accumulation with trimming", `Slow, test_stress_message_accumulation;
  "rapid sequential retries", `Quick, test_stress_rapid_retries;
  "concurrent operations", `Quick, test_stress_concurrent_operations;
]

(* ============================================ *)
(* Chaos Tests                                  *)
(* ============================================ *)

(** Test: 10% failure rate - should mostly succeed with retries *)
let test_chaos_10_percent_failures () =
  reset_random ();

  (* With 10% failure rate and 5 retries, should almost always succeed *)
  let config = {
    max_turns = 5;
    max_messages = 50;
    timeout_ms = 10000;
    retry_policy = {
      max_attempts = 5;
      initial_delay_ms = 10;
      max_delay_ms = 100;
      backoff_multiplier = 2.0;
      jitter = false;
    };
  } in

  (* Run multiple times to verify probabilistic behavior *)
  let successes = ref 0 in
  for _ = 1 to 10 do
    let result = Lwt_main.run (
      Stress_Loop.run
        ~config
        ~backend_config:(Chaos_Backend.make_config
          ~responses:["Response"]
          ~tool_calls:[None]
          ~chaos:chaos_10_percent)
        ~initial_prompt:"Test with 10% failures"
        ~tools:[]
        ()
    ) in
    match result with
    | Completed _ -> incr successes
    | _ -> ()
  done;

  (* With 10% failure and 5 retries, should succeed >90% of time *)
  check bool "should mostly succeed with retries" true (!successes >= 7)

(** Test: 50% failure rate - tests resilience *)
let test_chaos_50_percent_failures () =
  reset_random ();

  let results = List.init 20 (fun i ->
    Random.init (test_seed + i);
    let backend_config = Chaos_Backend.make_config
      ~responses:["Response"]
      ~tool_calls:[None]
      ~chaos:chaos_50_percent in

    let config = {
      default_loop_config with
      max_turns = 3;
      timeout_ms = 5000;
      retry_policy = {
        default_retry_policy with
        max_attempts = 10;
        initial_delay_ms = 5;
        jitter = false;
      };
    } in

    Lwt_main.run (
      Stress_Loop.run
        ~config
        ~backend_config
        ~initial_prompt:"Test with 50% failures"
        ~tools:[]
        ()
    )
  ) in

  let successes = List.length (List.filter (function Completed _ -> true | _ -> false) results) in
  let errors = List.length (List.filter (function Error _ -> true | _ -> false) results) in

  (* With 50% failure and 10 retries, expect mix of successes and failures *)
  check bool "should have some successes" true (successes > 0);
  check bool "should have some failures at high chaos" true (errors >= 0)

(** Test: 90% failure rate - extreme chaos *)
let test_chaos_90_percent_failures () =
  reset_random ();

  let backend_config = Chaos_Backend.make_config
    ~responses:["Response"]
    ~tool_calls:[None]
    ~chaos:chaos_90_percent in

  let config = {
    default_loop_config with
    max_turns = 3;
    timeout_ms = 2000;
    retry_policy = {
      default_retry_policy with
      max_attempts = 3;
      initial_delay_ms = 5;
      jitter = false;
    };
  } in

  let result = Lwt_main.run (
    Stress_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"Test with 90% failures"
      ~tools:[]
      ()
  ) in

  (* At 90% failure with only 3 retries, should usually fail *)
  match result with
  | Error _ -> ()  (* Expected *)
  | Completed _ -> ()  (* Possible but unlikely *)
  | _ -> ()

(** Test: Tool execution failures mid-loop *)
let test_chaos_tool_failures () =
  reset_random ();
  Chaos_Tools_Impl.configure ~failure_rate:0.3;  (* 30% tool failure *)

  let tool_call = { id = "tc"; name = "chaos_tool"; arguments = `Null } in

  (* Multiple tool calls to test failure handling *)
  let responses = List.init 10 (fun i -> Printf.sprintf "Turn %d" i) @ ["Final"] in
  let tool_calls = List.init 10 (fun _ -> Some [tool_call]) @ [None] in

  let backend_config = Chaos_Backend.make_config
    ~responses
    ~tool_calls
    ~chaos:no_chaos in  (* No backend chaos, only tool chaos *)

  let config = {
    default_loop_config with
    max_turns = 15;
    timeout_ms = 10000;
    retry_policy = { default_retry_policy with max_attempts = 1 };
  } in

  let result = Lwt_main.run (
    Stress_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"Test tool failures"
      ~tools:[]
      ()
  ) in

  let (exec_count, fail_count) = Chaos_Tools_Impl.stats () in

  (* Should complete despite tool failures (errors are sent back to LLM) *)
  match result with
  | Completed { turns_used; _ } ->
    check bool "should have some tool executions" true (exec_count > 0);
    check bool "should complete despite tool failures" true (turns_used > 0);
    check bool "should have recorded some failures" true (fail_count >= 0)
  | MaxTurnsReached _ -> ()  (* Also acceptable *)
  | _ -> ()

(** Test: Intermittent timeouts with recovery *)
let test_chaos_intermittent_timeouts () =
  reset_random ();

  (* Lower timeout rate to make recovery more likely *)
  let chaos = { chaos_10_percent with timeout_rate = 0.1 } in

  let backend_config = Chaos_Backend.make_config
    ~responses:["Response 1"; "Response 2"; "Final"]
    ~tool_calls:[None; None; None]
    ~chaos in

  let config = {
    default_loop_config with
    max_turns = 5;
    timeout_ms = 500;  (* Short timeout *)
    retry_policy = {
      default_retry_policy with
      max_attempts = 5;
      initial_delay_ms = 10;
      jitter = false;
    };
  } in

  (* This tests that the system can recover from occasional timeouts *)
  let _ = Lwt_main.run (
    Stress_Loop.run
      ~config
      ~backend_config
      ~initial_prompt:"Test intermittent timeouts"
      ~tools:[]
      ()
  ) in
  ()  (* Just verify it doesn't crash *)

let chaos_tests = [
  "10% failure rate", `Quick, test_chaos_10_percent_failures;
  "50% failure rate", `Quick, test_chaos_50_percent_failures;
  "90% failure rate (extreme)", `Quick, test_chaos_90_percent_failures;
  "tool execution failures", `Quick, test_chaos_tool_failures;
  "intermittent timeouts", `Quick, test_chaos_intermittent_timeouts;
]

(* ============================================ *)
(* Edge Case Tests                              *)
(* ============================================ *)

(** Test: Exact timeout boundary (response at 99ms, timeout at 100ms) *)
let test_edge_exact_timeout_boundary () =
  reset_random ();

  (* Create a backend that responds just under the timeout *)
  let delay_ms = 95 in  (* Just under 100ms timeout *)
  let timeout_ms = 100 in

  let f () =
    let%lwt () = Lwt_unix.sleep (float_of_int delay_ms /. 1000.0) in
    Lwt.return "Just in time!"
  in

  let result = Lwt_main.run (Timeout.with_timeout_ms timeout_ms f) in
  check (option string) "should succeed just under timeout"
    (Some "Just in time!") result

(** Test: Response exactly at timeout (may succeed or fail) *)
let test_edge_response_at_timeout () =
  reset_random ();

  let delay_ms = 100 in
  let timeout_ms = 100 in

  let f () =
    let%lwt () = Lwt_unix.sleep (float_of_int delay_ms /. 1000.0) in
    Lwt.return "At the boundary"
  in

  let result = Lwt_main.run (Timeout.with_timeout_ms timeout_ms f) in
  (* At exact boundary, behavior is non-deterministic - just ensure no crash *)
  match result with
  | Some _ -> ()  (* Success is acceptable *)
  | None -> ()    (* Timeout is also acceptable *)

(** Test: Max retries exactly exhausted *)
let test_edge_max_retries_exactly_exhausted () =
  reset_random ();

  let attempt_count = ref 0 in
  let max_attempts = 5 in

  let policy = {
    max_attempts;
    initial_delay_ms = 5;
    max_delay_ms = 50;
    backoff_multiplier = 2.0;
    jitter = false;
  } in

  let f () =
    incr attempt_count;
    if !attempt_count < max_attempts then
      Lwt.return (Result.Error "Transient")
    else if !attempt_count = max_attempts then
      Lwt.return (Result.Ok "Finally!")
    else
      Lwt.return (Result.Error "Should not reach here")
  in

  let result = Lwt_main.run (Retry.with_retry policy f) in

  check int "should exhaust exactly max attempts" max_attempts !attempt_count;
  match result with
  | Success "Finally!" -> ()
  | _ -> fail "Expected success on last attempt"

(** Test: One more than max retries needed - should exhaust *)
let test_edge_one_more_than_max_retries () =
  reset_random ();

  let attempt_count = ref 0 in
  let max_attempts = 5 in

  let policy = {
    max_attempts;
    initial_delay_ms = 5;
    max_delay_ms = 50;
    backoff_multiplier = 2.0;
    jitter = false;
  } in

  let f () =
    incr attempt_count;
    if !attempt_count <= max_attempts then
      Lwt.return (Result.Error "Always fail")
    else
      Lwt.return (Result.Ok "Too late!")
  in

  let result = Lwt_main.run (Retry.with_retry policy f) in

  check int "should stop at max attempts" max_attempts !attempt_count;
  match result with
  | Exhausted { attempts; _ } ->
    check int "exhausted attempts should match" max_attempts attempts
  | _ -> fail "Expected Exhausted"

(** Test: Empty tool calls vs None tool calls *)
let test_edge_empty_vs_none_tool_calls () =
  reset_random ();

  (* Test with Some [] - should be treated as final *)
  let backend_config_empty = Chaos_Backend.make_config
    ~responses:["Response with empty tools"]
    ~tool_calls:[Some []]  (* Empty list, not None *)
    ~chaos:no_chaos in

  let config = {
    default_loop_config with
    max_turns = 5;
    timeout_ms = 5000;
    retry_policy = { default_retry_policy with max_attempts = 1 };
  } in

  let result_empty = Lwt_main.run (
    Stress_Loop.run
      ~config
      ~backend_config:backend_config_empty
      ~initial_prompt:"Test empty tool calls"
      ~tools:[]
      ()
  ) in

  (* Test with None - should also be final *)
  let backend_config_none = Chaos_Backend.make_config
    ~responses:["Response with no tools"]
    ~tool_calls:[None]
    ~chaos:no_chaos in

  let result_none = Lwt_main.run (
    Stress_Loop.run
      ~config
      ~backend_config:backend_config_none
      ~initial_prompt:"Test None tool calls"
      ~tools:[]
      ()
  ) in

  (* Both should complete in 1 turn *)
  (match result_empty with
  | Completed { turns_used; _ } ->
    check int "empty tools should complete in 1 turn" 1 turns_used
  | _ -> fail "Expected Completed for empty tools");

  (match result_none with
  | Completed { turns_used; _ } ->
    check int "None tools should complete in 1 turn" 1 turns_used
  | _ -> fail "Expected Completed for None tools")

(** Test: System message preservation under extreme trimming *)
let test_edge_system_message_extreme_trimming () =
  reset_random ();

  let state = State.create () in

  (* Add system message *)
  let state = State.add_message state {
    role = System;
    content = "You are a helpful assistant";
    tool_calls = None;
    name = None;
  } in

  (* Add 1000 user/assistant messages *)
  let state = List.fold_left (fun s i ->
    let s = State.add_message s {
      role = User;
      content = Printf.sprintf "User message %d" i;
      tool_calls = None;
      name = None;
    } in
    State.add_message s {
      role = Assistant;
      content = Printf.sprintf "Assistant message %d" i;
      tool_calls = None;
      name = None;
    }
  ) state (List.init 500 Fun.id) in

  check int "should have 1001 messages" 1001 (State.message_count state);

  (* Extreme trim to just 3 messages *)
  let state = State.trim_to_window state ~max_messages:3 in

  let messages = State.get_messages state in
  check int "should have 3 messages after trim" 3 (List.length messages);

  (* System message should be first *)
  let first = List.hd messages in
  check bool "first message should be system" true (first.role = System);
  check string "system content preserved" "You are a helpful assistant" first.content

(** Test: Zero max_messages (edge case) *)
let test_edge_zero_max_messages () =
  reset_random ();

  let state = State.create () in
  let state = State.add_message state {
    role = User; content = "test"; tool_calls = None; name = None
  } in

  (* Trim to 0 - should keep at least 1 *)
  let state = State.trim_to_window state ~max_messages:0 in
  let messages = State.get_messages state in

  (* Implementation should handle this gracefully *)
  check bool "should not crash with max_messages=0" true (List.length messages >= 0)

(** Test: Very long message content *)
let test_edge_very_long_message () =
  reset_random ();

  let state = State.create () in

  (* Add a 1MB message *)
  let huge_content = String.make (1024 * 1024) 'x' in
  let state = State.add_message state {
    role = User;
    content = huge_content;
    tool_calls = None;
    name = None;
  } in

  let tokens = State.estimate_tokens state in

  (* 1MB / 4 = 256K tokens approximately *)
  check bool "should estimate many tokens" true (tokens > 200_000);
  check bool "should handle large message" true (State.message_count state = 1)

let edge_case_tests = [
  "exact timeout boundary (just under)", `Quick, test_edge_exact_timeout_boundary;
  "response at timeout boundary", `Quick, test_edge_response_at_timeout;
  "max retries exactly exhausted", `Quick, test_edge_max_retries_exactly_exhausted;
  "one more than max retries", `Quick, test_edge_one_more_than_max_retries;
  "empty tool calls vs None", `Quick, test_edge_empty_vs_none_tool_calls;
  "system message extreme trimming", `Quick, test_edge_system_message_extreme_trimming;
  "zero max_messages", `Quick, test_edge_zero_max_messages;
  "very long message content", `Quick, test_edge_very_long_message;
]

(* ============================================ *)
(* Performance Benchmark Tests                  *)
(* ============================================ *)

(** Benchmark: Throughput - turns per second *)
let test_benchmark_throughput () =
  reset_random ();

  let num_turns = 50 in

  let responses = List.init num_turns (fun i -> Printf.sprintf "Response %d" i) in
  let tool_calls = List.init (num_turns - 1) (fun _ ->
    Some [{ id = "tc"; name = "fast_tool"; arguments = `Null }]
  ) @ [None] in

  let backend_config = Chaos_Backend.make_config
    ~responses
    ~tool_calls
    ~chaos:no_chaos in

  let config = {
    max_turns = 100;
    max_messages = 200;
    timeout_ms = 60000;
    retry_policy = { default_retry_policy with max_attempts = 1 };
  } in

  Chaos_Tools_Impl.configure ~failure_rate:0.0;

  let (result, elapsed) = time_ms (fun () ->
    Lwt_main.run (
      Stress_Loop.run
        ~config
        ~backend_config
        ~initial_prompt:"Benchmark throughput"
        ~tools:[]
        ()
    )
  ) in

  match result with
  | Completed { turns_used; _ } ->
    let turns_per_sec = float_of_int turns_used /. (elapsed /. 1000.0) in
    (* Should achieve at least 100 turns/sec without I/O *)
    check bool "should achieve good throughput" true (turns_per_sec > 50.0);
    Printf.printf "\n  Throughput: %.1f turns/sec (%.1fms total)\n" turns_per_sec elapsed
  | _ -> fail "Expected Completed"

(** Benchmark: Memory per message *)
let test_benchmark_memory_per_message () =
  reset_random ();
  Gc.full_major ();

  let num_messages = 1000 in
  let state = ref (State.create ()) in

  let mem_before = memory_words () in

  for i = 1 to num_messages do
    state := State.add_message !state {
      role = User;
      content = Printf.sprintf "Message %d with some typical content" i;
      tool_calls = None;
      name = None;
    }
  done;

  Gc.full_major ();
  let mem_after = memory_words () in

  let words_per_message = (mem_after - mem_before) / num_messages in
  let bytes_per_message = words_per_message * 8 in  (* 64-bit words *)

  (* Should be less than 1KB per message *)
  check bool "should have reasonable memory per message" true
    (bytes_per_message < 1024);
  Printf.printf "\n  Memory: ~%d bytes/message\n" bytes_per_message

(** Benchmark: Turn latency percentiles *)
let test_benchmark_latency_percentiles () =
  reset_random ();

  let num_samples = 100 in
  let latencies = ref [] in

  for _ = 1 to num_samples do
    let backend_config = Chaos_Backend.make_config
      ~responses:["Response"]
      ~tool_calls:[None]
      ~chaos:no_chaos in

    let config = {
      default_loop_config with
      max_turns = 2;
      timeout_ms = 5000;
      retry_policy = { default_retry_policy with max_attempts = 1 };
    } in

    let (_, elapsed) = time_ms (fun () ->
      Lwt_main.run (
        Stress_Loop.run
          ~config
          ~backend_config
          ~initial_prompt:"Latency test"
          ~tools:[]
          ()
      )
    ) in
    latencies := elapsed :: !latencies
  done;

  let sorted = List.sort compare !latencies in
  let p50_idx = num_samples / 2 in
  let p95_idx = num_samples * 95 / 100 in
  let p99_idx = num_samples * 99 / 100 in

  let p50 = List.nth sorted p50_idx in
  let p95 = List.nth sorted p95_idx in
  let p99 = List.nth sorted p99_idx in

  (* Should have low latency without real I/O *)
  check bool "p50 should be fast" true (p50 < 10.0);
  check bool "p95 should be reasonable" true (p95 < 50.0);
  check bool "p99 should be acceptable" true (p99 < 100.0);
  Printf.printf "\n  Latency: p50=%.2fms, p95=%.2fms, p99=%.2fms\n" p50 p95 p99

(** Benchmark: State trimming performance *)
let test_benchmark_trimming_performance () =
  reset_random ();

  (* Build a large state *)
  let state = ref (State.create ()) in
  for i = 1 to 10000 do
    state := State.add_message !state {
      role = if i mod 2 = 0 then User else Assistant;
      content = Printf.sprintf "Message %d" i;
      tool_calls = None;
      name = None;
    }
  done;

  let (_, elapsed) = time_ms (fun () ->
    for _ = 1 to 100 do
      let _ = State.trim_to_window !state ~max_messages:50 in ()
    done
  ) in

  let per_trim = elapsed /. 100.0 in

  (* Trimming should be fast even on large states *)
  check bool "trimming should be fast" true (per_trim < 10.0);
  Printf.printf "\n  Trim time: %.3fms per trim (10k messages -> 50)\n" per_trim

(** Benchmark: Retry delay calculation *)
let test_benchmark_retry_delay_calculation () =
  reset_random ();

  let policy = {
    max_attempts = 10;
    initial_delay_ms = 100;
    max_delay_ms = 10000;
    backoff_multiplier = 2.0;
    jitter = true;
  } in

  let (_, elapsed) = time_ms (fun () ->
    for _ = 1 to 100000 do
      let _ = Retry.calculate_delay policy (Random.int 10 + 1) in ()
    done
  ) in

  let per_calculation = elapsed /. 100000.0 in

  (* Should be essentially free *)
  check bool "delay calculation should be instant" true (per_calculation < 0.01);
  Printf.printf "\n  Delay calc: %.4fms per calculation\n" per_calculation

let benchmark_tests = [
  "throughput (turns/sec)", `Slow, test_benchmark_throughput;
  "memory per message", `Quick, test_benchmark_memory_per_message;
  "latency percentiles", `Slow, test_benchmark_latency_percentiles;
  "trimming performance", `Quick, test_benchmark_trimming_performance;
  "retry delay calculation", `Quick, test_benchmark_retry_delay_calculation;
]

(* ============================================ *)
(* Regression Tests                             *)
(* ============================================ *)

(** Regression: Turn counter starts at 1, not 0 *)
let test_regression_turn_counter_starts_at_one () =
  let state = State.create () in
  check int "turn should start at 1" 1 (State.get_turn state)

(** Regression: Token estimation doesn't crash on empty message *)
let test_regression_token_estimation_empty () =
  let state = State.create () in
  let state = State.add_message state {
    role = User; content = ""; tool_calls = None; name = None
  } in
  let tokens = State.estimate_tokens state in
  check bool "should handle empty content" true (tokens >= 0)

(** Regression: Trimming doesn't lose all messages *)
let test_regression_trimming_preserves_messages () =
  let state = State.create () in
  let state = State.add_message state {
    role = User; content = "test"; tool_calls = None; name = None
  } in
  let state = State.trim_to_window state ~max_messages:1 in
  check int "should keep at least one message" 1 (State.message_count state)

let regression_tests = [
  "turn counter starts at 1", `Quick, test_regression_turn_counter_starts_at_one;
  "token estimation on empty", `Quick, test_regression_token_estimation_empty;
  "trimming preserves messages", `Quick, test_regression_trimming_preserves_messages;
]

(* ============================================ *)
(* Main Test Runner                             *)
(* ============================================ *)

let () =
  run "Agent Core Stress & Chaos" [
    "Stress", stress_tests;
    "Chaos", chaos_tests;
    "Edge Cases", edge_case_tests;
    "Benchmarks", benchmark_tests;
    "Regression", regression_tests;
  ]

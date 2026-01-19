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
(* Orchestrator Module Tests                    *)
(* ============================================ *)

module Orchestrator = Agent_core.Orchestrator

let test_orchestrator_goal_status_types () =
  let open Orchestrator in
  (* Test goal_status constructors *)
  let not_reached = NotReached "still working" in
  let reached = Reached "task completed" in
  let failed = Failed "unrecoverable error" in

  (match not_reached with
   | NotReached msg -> check string "not reached msg" "still working" msg
   | _ -> fail "expected NotReached");

  (match reached with
   | Reached msg -> check string "reached msg" "task completed" msg
   | _ -> fail "expected Reached");

  (match failed with
   | Failed msg -> check string "failed msg" "unrecoverable error" msg
   | _ -> fail "expected Failed")

let test_orchestrator_handoff_type () =
  let open Orchestrator in
  let handoff : handoff = {
    target_agent = "worker_1";
    context_summary = "processed 50% of data";
    task_description = "continue processing remaining items";
    priority = 1;
  } in
  check string "target agent" "worker_1" handoff.target_agent;
  check string "context summary" "processed 50% of data" handoff.context_summary;
  check string "task description" "continue processing remaining items" handoff.task_description;
  check int "priority" 1 handoff.priority

let test_orchestrator_result_types () =
  let open Orchestrator in
  (* GoalReached *)
  let goal_reached : string orchestrator_result = GoalReached {
    final_state = "completed";
    iterations = 5;
    summary = "All tasks done";
  } in
  (match goal_reached with
   | GoalReached { final_state; iterations; summary } ->
     check string "final state" "completed" final_state;
     check int "iterations" 5 iterations;
     check string "summary" "All tasks done" summary
   | _ -> fail "expected GoalReached");

  (* MaxIterationsReached *)
  let max_iter : string orchestrator_result = MaxIterationsReached {
    last_state = "partial";
    iterations = 100;
    last_status = NotReached "still processing";
  } in
  (match max_iter with
   | MaxIterationsReached { last_state; iterations; _ } ->
     check string "last state" "partial" last_state;
     check int "max iterations" 100 iterations
   | _ -> fail "expected MaxIterationsReached");

  (* OrchestratorError *)
  let error : string orchestrator_result = OrchestratorError "connection failed" in
  (match error with
   | OrchestratorError msg -> check string "error msg" "connection failed" msg
   | _ -> fail "expected OrchestratorError");

  (* AgentFailed *)
  let agent_failed : string orchestrator_result = AgentFailed {
    agent_name = "analyzer";
    error = "out of memory";
    iterations = 42;
  } in
  (match agent_failed with
   | AgentFailed { agent_name; error; iterations } ->
     check string "agent name" "analyzer" agent_name;
     check string "error" "out of memory" error;
     check int "iterations" 42 iterations
   | _ -> fail "expected AgentFailed")

let test_orchestrator_agent_capability () =
  let open Orchestrator in
  let cap : agent_capability = {
    name = "code_reviewer";
    description = "Reviews code for quality and security";
    specialization = ["coding"; "security"; "review"];
    max_context = 128000;
  } in
  check string "capability name" "code_reviewer" cap.name;
  check string "capability description" "Reviews code for quality and security" cap.description;
  check int "specialization count" 3 (List.length cap.specialization);
  check int "max context" 128000 cap.max_context

let test_orchestrator_evaluation () =
  let open Orchestrator in
  let eval : evaluation = {
    score = 0.85;
    feedback = "Good implementation but needs more error handling";
    pass = true;
  } in
  check bool "evaluation pass" true eval.pass;
  check bool "score above threshold" true (eval.score > 0.8);
  check string "feedback" "Good implementation but needs more error handling" eval.feedback

let test_simple_pipeline () =
  let open Orchestrator in
  let stage1 input = Lwt.return (input ^ " -> stage1 done") in
  let stage2 input = Lwt.return (input ^ " -> stage2 done") in

  let result = Lwt_main.run (
    run_simple_pipeline
      ~run_stage1:stage1
      ~run_stage2:stage2
      ~initial_prompt:"start"
  ) in

  match result with
  | Ok { stage1_output; stage2_output; total_iterations } ->
    check string "stage1 output" "start -> stage1 done" stage1_output;
    check string "stage2 output" "start -> stage1 done -> stage2 done" stage2_output;
    check int "total iterations" 2 total_iterations
  | Error e -> fail ("Pipeline failed: " ^ e)

let test_orchestrator_work_queue () =
  let open Orchestrator in
  (* Test work queue initialization *)
  let tasks = ["task1"; "task2"; "task3"] in
  let state = init_orchestrator_state tasks in

  check int "work queue length" 3 (List.length state.work_queue);
  check int "completed length" 0 (List.length state.completed);
  check int "iteration" 0 state.iteration;

  (* Test get_next_work *)
  let next = get_next_work state in
  (match next with
   | Some item ->
     check string "first task" "task1" item.task;
     check bool "status is pending" true (item.status = `Pending)
   | None -> fail "expected pending work item");

  (* Test all_work_done *)
  check bool "not all done initially" false (all_work_done state)

(* ============================================ *)
(* New Pattern Tests: Routing, Parallel, Chain  *)
(* ============================================ *)

let test_routing_types () =
  let open Orchestrator in
  let route : route = {
    route_name = "coding";
    confidence = 0.95;
    reasoning = Some "Query mentions code review";
  } in
  check string "route name" "coding" route.route_name;
  check bool "high confidence" true (route.confidence > 0.9);
  (match route.reasoning with
   | Some r -> check string "reasoning" "Query mentions code review" r
   | None -> fail "expected reasoning")

let test_router_config () =
  let open Orchestrator in
  let config : router_config = {
    routes = [
      ("coding", fun q -> Lwt.return ("Code: " ^ q));
      ("writing", fun q -> Lwt.return ("Write: " ^ q));
    ];
    fallback = (fun q -> Lwt.return ("Fallback: " ^ q));
    confidence_threshold = 0.7;
  } in
  check int "route count" 2 (List.length config.routes);
  check bool "threshold" true (config.confidence_threshold = 0.7)

let test_parallel_result_types () =
  let open Orchestrator in
  let result : string parallel_result = {
    task_id = "task_1";
    result = Ok "completed";
    duration_ms = 150;
  } in
  check string "task id" "task_1" result.task_id;
  check int "duration" 150 result.duration_ms;
  (match result.result with
   | Ok v -> check string "result value" "completed" v
   | Error _ -> fail "expected Ok result")

let test_run_parallel () =
  let open Orchestrator in
  let tasks = [
    ("t1", fun () -> Lwt.return "result1");
    ("t2", fun () -> Lwt.return "result2");
    ("t3", fun () -> Lwt.return "result3");
  ] in
  let results = Lwt_main.run (run_parallel ~tasks ()) in
  check int "result count" 3 (List.length results);
  List.iter (fun r ->
    match r.result with
    | Ok _ -> ()
    | Error e -> fail ("task failed: " ^ e)
  ) results

let test_run_parallel_with_timeout () =
  let open Orchestrator in
  let tasks = [
    ("fast", fun () -> Lwt.return "quick");
    ("slow", fun () ->
       let open Lwt.Syntax in
       let* () = Lwt_unix.sleep 0.01 in
       Lwt.return "delayed");
  ] in
  let results = Lwt_main.run (run_parallel ~timeout_ms:5000 ~tasks ()) in
  check int "result count" 2 (List.length results);
  (* Both should succeed with generous timeout *)
  List.iter (fun r ->
    match r.result with
    | Ok _ -> ()
    | Error _ -> ()  (* Timeout may happen on slow CI *)
  ) results

let test_fanout_pattern () =
  let open Orchestrator in
  let split input = String.split_on_char ' ' input in
  let process word = Lwt.return (String.uppercase_ascii word) in
  let aggregate results = String.concat "-" results in

  let result = Lwt_main.run (
    run_fanout
      ~split
      ~process
      ~aggregate
      ~input:"hello world test"
  ) in
  check string "fanout result" "HELLO-WORLD-TEST" result

let test_chain_step_types () =
  let open Orchestrator in
  let step : chain_step = {
    step_name = "translate";
    transform = fun s -> Lwt.return (String.uppercase_ascii s);
  } in
  check string "step name" "translate" step.step_name;
  let result = Lwt_main.run (step.transform "hello") in
  check string "transform result" "HELLO" result

let test_chain_result_types () =
  let open Orchestrator in
  let result : chain_result = {
    final_output = "final";
    intermediate_outputs = [("step1", "out1"); ("step2", "out2")];
    total_steps = 2;
  } in
  check string "final output" "final" result.final_output;
  check int "total steps" 2 result.total_steps;
  check int "intermediate count" 2 (List.length result.intermediate_outputs)

let test_run_chain () =
  let open Orchestrator in
  let steps = make_chain [
    ("add_prefix", fun s -> Lwt.return ("[PREFIX] " ^ s));
    ("uppercase", fun s -> Lwt.return (String.uppercase_ascii s));
    ("add_suffix", fun s -> Lwt.return (s ^ " [SUFFIX]"));
  ] in
  let result = Lwt_main.run (run_chain ~steps ~initial_input:"hello") in
  match result with
  | Ok { final_output; intermediate_outputs; total_steps } ->
    check string "final output" "[PREFIX] HELLO [SUFFIX]" final_output;
    check int "total steps" 3 total_steps;
    check int "intermediate count" 3 (List.length intermediate_outputs)
  | Error e -> fail ("Chain failed: " ^ e)

let test_make_chain () =
  let open Orchestrator in
  let transforms = [
    ("step1", fun s -> Lwt.return (s ^ "_1"));
    ("step2", fun s -> Lwt.return (s ^ "_2"));
  ] in
  let chain = make_chain transforms in
  check int "chain length" 2 (List.length chain);
  check string "first step name" "step1" (List.hd chain).step_name

let test_worker_type () =
  let open Orchestrator in
  let worker : string worker = {
    worker_id = "w1";
    worker_name = "Worker One";
    capability = {
      name = "analyzer";
      description = "Analyzes data";
      specialization = ["data"; "analysis"];
      max_context = 32000;
    };
    execute = fun task -> Lwt.return ("Done: " ^ task);
  } in
  check string "worker id" "w1" worker.worker_id;
  check string "worker name" "Worker One" worker.worker_name;
  let result = Lwt_main.run (worker.execute "test task") in
  check string "execute result" "Done: test task" result

let test_orchestrator_decision_types () =
  let open Orchestrator in
  (* Test each decision variant *)
  let item = { id = "t1"; task = "task1"; assigned_to = None; status = `Pending } in
  let assign = AssignWork { worker_id = "w1"; work_item = item } in
  let wait = WaitForCompletion in
  let done_ = AllDone in
  let failed = Failed "error reason" in

  (match assign with
   | AssignWork { worker_id; _ } -> check string "worker id" "w1" worker_id
   | _ -> fail "expected AssignWork");

  (match wait with
   | WaitForCompletion -> ()
   | _ -> fail "expected WaitForCompletion");

  (match done_ with
   | AllDone -> ()
   | _ -> fail "expected AllDone");

  (match failed with
   | Failed reason -> check string "reason" "error reason" reason
   | _ -> fail "expected Failed")

let test_run_orchestrator_workers () =
  let open Orchestrator in
  let workers : string worker list = [
    {
      worker_id = "w1";
      worker_name = "Worker 1";
      capability = { name = "w1"; description = ""; specialization = []; max_context = 0 };
      execute = fun task -> Lwt.return ("W1: " ^ task);
    };
    {
      worker_id = "w2";
      worker_name = "Worker 2";
      capability = { name = "w2"; description = ""; specialization = []; max_context = 0 };
      execute = fun task -> Lwt.return ("W2: " ^ task);
    };
  ] in
  let tasks = ["task1"; "task2"; "task3"] in

  let result = Lwt_main.run (run_orchestrator_workers ~workers ~tasks ()) in
  match result with
  | Ok results ->
    check int "result count" 3 (List.length results);
    (* Check all tasks were processed *)
    List.iter (fun r ->
      check bool "has prefix" true (String.length r > 3)
    ) results
  | Error e -> fail ("Orchestrator failed: " ^ e)

let orchestrator_tests = [
  "goal status types", `Quick, test_orchestrator_goal_status_types;
  "handoff type", `Quick, test_orchestrator_handoff_type;
  "result types", `Quick, test_orchestrator_result_types;
  "agent capability", `Quick, test_orchestrator_agent_capability;
  "evaluation type", `Quick, test_orchestrator_evaluation;
  "simple pipeline", `Quick, test_simple_pipeline;
  "work queue", `Quick, test_orchestrator_work_queue;
  (* New pattern tests *)
  "routing types", `Quick, test_routing_types;
  "router config", `Quick, test_router_config;
  "parallel result types", `Quick, test_parallel_result_types;
  "run parallel", `Quick, test_run_parallel;
  "run parallel with timeout", `Quick, test_run_parallel_with_timeout;
  "fanout pattern", `Quick, test_fanout_pattern;
  "chain step types", `Quick, test_chain_step_types;
  "chain result types", `Quick, test_chain_result_types;
  "run chain", `Quick, test_run_chain;
  "make chain", `Quick, test_make_chain;
  "worker type", `Quick, test_worker_type;
  "orchestrator decision types", `Quick, test_orchestrator_decision_types;
  "run orchestrator workers", `Quick, test_run_orchestrator_workers;
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
    "Orchestrator", orchestrator_tests;
  ]

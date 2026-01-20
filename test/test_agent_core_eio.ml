(** Tests for Agent Core Eio

    Unit tests for retry, timeout, and agent loop functionality.
    Uses Eio_main.run for effect handling.
*)

module AC = Agent_core_eio
module Types = Agent_core_eio.Types

(** {1 Test Helpers} *)

let run_eio f =
  Eio_main.run @@ fun env ->
  f (Eio.Stdenv.clock env)

let retry_classify _ = AC.Retry.Retry

(** {1 Retry Tests} *)

let test_retry_success () =
  run_eio @@ fun clock ->
  let policy = { AC.default_retry_policy with Types.max_attempts = 3 } in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Result.Ok "success"
  in
  match AC.Retry.with_retry_eio ~clock ~policy ~op_name:"test_retry_success" ~classify:retry_classify f with
  | `Ok result ->
    Alcotest.(check string) "result" "success" result;
    Alcotest.(check int) "called once" 1 !call_count
  | _ ->
    Alcotest.fail "expected Ok"

let test_retry_eventual_success () =
  run_eio @@ fun clock ->
  let policy = Types.{
    max_attempts = 5;
    initial_delay_ms = 10;
    max_delay_ms = 100;
    backoff_multiplier = 2.0;
    jitter = false;
  } in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    if !call_count < 3 then Result.Error "not yet"
    else Result.Ok "finally"
  in
  match AC.Retry.with_retry_eio ~clock ~policy ~op_name:"test_retry_eventual_success" ~classify:retry_classify f with
  | `Ok result ->
    Alcotest.(check string) "result" "finally" result;
    Alcotest.(check int) "called 3 times" 3 !call_count
  | _ ->
    Alcotest.fail "expected Ok"

let test_retry_exhausted () =
  run_eio @@ fun clock ->
  let policy = Types.{
    max_attempts = 3;
    initial_delay_ms = 10;
    max_delay_ms = 50;
    backoff_multiplier = 2.0;
    jitter = false;
  } in
  let f () = Result.Error "always fails" in
  match AC.Retry.with_retry_eio ~clock ~policy ~op_name:"test_retry_exhausted" ~classify:retry_classify f with
  | `Error _ -> ()
  | _ ->
    Alcotest.fail "expected Error"

let test_retry_with_jitter () =
  run_eio @@ fun clock ->
  let policy = Types.{
    max_attempts = 3;
    initial_delay_ms = 100;
    max_delay_ms = 1000;
    backoff_multiplier = 2.0;
    jitter = true;
  } in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    if !call_count < 2 then Result.Error "retry"
    else Result.Ok "done"
  in
  match AC.Retry.with_retry_eio ~clock ~policy ~op_name:"test_retry_with_jitter" ~classify:retry_classify f with
  | `Ok _ ->
    Alcotest.(check int) "called twice" 2 !call_count
  | _ ->
    Alcotest.fail "expected Ok"

(** {1 Timeout Tests} *)

let test_timeout_success () =
  run_eio @@ fun clock ->
  let f () = "quick result" in
  match AC.Timeout.with_timeout_ms ~clock ~timeout_ms:1000 f with
  | Some result ->
    Alcotest.(check string) "result" "quick result" result
  | None ->
    Alcotest.fail "expected Some result"

let test_timeout_expires () =
  run_eio @@ fun clock ->
  let f () =
    Eio.Time.sleep clock 2.0;
    "should not reach"
  in
  match AC.Timeout.with_timeout_ms ~clock ~timeout_ms:100 f with
  | None -> ()  (* Expected *)
  | Some _ ->
    Alcotest.fail "expected timeout"

let test_timeout_result_success () =
  run_eio @@ fun clock ->
  match AC.Timeout.with_timeout_result ~clock ~timeout_sec:1.0 (fun () -> 42) with
  | Ok result ->
    Alcotest.(check int) "result" 42 result
  | Error _ ->
    Alcotest.fail "expected Ok"

let test_timeout_result_timeout () =
  run_eio @@ fun clock ->
  let f () =
    Eio.Time.sleep clock 2.0;
    "unreachable"
  in
  match AC.Timeout.with_timeout_result ~clock ~timeout_sec:0.1 f with
  | Error msg ->
    Alcotest.(check string) "error" "timeout" msg
  | Ok _ ->
    Alcotest.fail "expected Error"

let test_deadline_success () =
  run_eio @@ fun clock ->
  let now = Eio.Time.now clock in
  let deadline = now +. 1.0 in
  match AC.Timeout.with_deadline ~clock ~deadline (fun () -> "done") with
  | Some result ->
    Alcotest.(check string) "result" "done" result
  | None ->
    Alcotest.fail "expected Some"

let test_deadline_already_passed () =
  run_eio @@ fun clock ->
  let now = Eio.Time.now clock in
  let deadline = now -. 1.0 in
  match AC.Timeout.with_deadline ~clock ~deadline (fun () -> "should not run") with
  | None -> ()  (* Expected: deadline already passed *)
  | Some _ ->
    Alcotest.fail "expected None for passed deadline"

(** {1 Types Tests} *)

let test_role_variants () =
  let open Types in
  Alcotest.(check bool) "User role" true (User = User);
  Alcotest.(check bool) "Assistant role" true (Assistant = Assistant);
  Alcotest.(check bool) "Tool role" true (Tool = Tool);
  Alcotest.(check bool) "System role" true (System = System);
  Alcotest.(check bool) "Different roles" false (User = Assistant)

let test_tool_result_variants () =
  let open Types in
  let success = ToolSuccess "result" in
  let error = ToolError "error" in
  (match success with
  | ToolSuccess s -> Alcotest.(check string) "success content" "result" s
  | ToolError _ -> Alcotest.fail "expected ToolSuccess");
  match error with
  | ToolError e -> Alcotest.(check string) "error content" "error" e
  | ToolSuccess _ -> Alcotest.fail "expected ToolError"

let test_default_config () =
  let config = AC.default_loop_config in
  Alcotest.(check int) "max_turns" 10 config.Types.max_turns;
  Alcotest.(check int) "timeout_ms" 60_000 config.Types.timeout_ms;
  Alcotest.(check int) "max_messages" 50 config.Types.max_messages

(** {1 Test Runner} *)

let retry_tests = [
  "success on first try", `Quick, test_retry_success;
  "eventual success", `Quick, test_retry_eventual_success;
  "exhausted after max attempts", `Quick, test_retry_exhausted;
  "with jitter enabled", `Quick, test_retry_with_jitter;
]

let timeout_tests = [
  "completes before timeout", `Quick, test_timeout_success;
  "timeout expires", `Quick, test_timeout_expires;
  "result type success", `Quick, test_timeout_result_success;
  "result type timeout", `Quick, test_timeout_result_timeout;
  "deadline success", `Quick, test_deadline_success;
  "deadline already passed", `Quick, test_deadline_already_passed;
]

let types_tests = [
  "role variants", `Quick, test_role_variants;
  "tool_result variants", `Quick, test_tool_result_variants;
  "default config values", `Quick, test_default_config;
]

let () =
  Alcotest.run "Agent Core Eio" [
    "Retry", retry_tests;
    "Timeout", timeout_tests;
    "Types", types_tests;
  ]
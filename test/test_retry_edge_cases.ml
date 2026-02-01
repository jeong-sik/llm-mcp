(** Edge Case Tests for Chain Retry *)

open Alcotest

(* Zero delay policy *)
let test_zero_delay () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let policy = Chain_retry.{ 
    max_attempts = 3;
    base_delay_ms = 0;
    max_delay_ms = 0;
    exponential_base = 2.0;
    jitter = false;
  } in
  let count = ref 0 in
  let result = Chain_retry.execute_with_retry ~clock ~policy (fun () ->
    incr count;
    if !count < 3 then Error (Error.Io (Error.NetworkError "retry"))
    else Ok "done"
  ) in
  check int "3 attempts" 3 !count;
  check int "no delay" 0 result.total_delay_ms

(* Single attempt policy *)
let test_single_attempt () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let count = ref 0 in
  let result = Chain_retry.execute_with_retry ~clock ~policy:Chain_retry.no_retry_policy (fun () ->
    incr count;
    Error (Error.Io (Error.NetworkError "fail"))
  ) in
  check int "1 attempt" 1 !count;
  check bool "failed" true (Result.is_error result.value)

(* Always succeed *)
let test_always_succeed () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let count = ref 0 in
  let result = Chain_retry.execute_with_retry ~clock ~policy:Chain_retry.default_policy (fun () ->
    incr count;
    Ok "immediate"
  ) in
  check int "1 attempt" 1 !count;
  check int "0 errors" 0 (List.length result.errors)

(* All errors same type *)
let test_consistent_errors () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let policy = Chain_retry.{ default_policy with max_attempts = 3; base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_retry ~clock ~policy (fun () ->
    Error (Error.Llm (Error.GeminiError Error.GeminiRateLimit))
  ) in
  check int "3 errors" 3 (List.length result.errors);
  List.iter (fun e ->
    check bool "same error type" true (Error.is_recoverable e)
  ) result.errors

(* Mixed recoverable and non-recoverable *)
let test_mixed_errors () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  let count = ref 0 in
  let result = Chain_retry.execute_with_retry ~clock ~policy (fun () ->
    incr count;
    if !count = 1 then Error (Error.Llm (Error.GeminiError Error.GeminiRateLimit))
    else Error (Error.Llm (Error.GeminiError Error.GeminiAuth))  (* non-recoverable *)
  ) in
  check int "2 attempts" 2 !count;
  check bool "failed" true (Result.is_error result.value)

(* Circuit breaker edge: exactly at threshold *)
let test_breaker_at_threshold () =
  let breaker = Chain_executor_retry.create_breaker ~failure_threshold:3 () in
  Chain_executor_retry.circuit_failure breaker;
  Chain_executor_retry.circuit_failure breaker;
  check bool "still closed at 2" true (Chain_executor_retry.circuit_allows breaker);
  Chain_executor_retry.circuit_failure breaker;
  check bool "open at 3" false (Chain_executor_retry.circuit_allows breaker)

(* Circuit breaker: success resets count *)
let test_breaker_success_resets () =
  let breaker = Chain_executor_retry.create_breaker ~failure_threshold:3 () in
  Chain_executor_retry.circuit_failure breaker;
  Chain_executor_retry.circuit_failure breaker;
  Chain_executor_retry.circuit_success breaker;
  Chain_executor_retry.circuit_failure breaker;
  Chain_executor_retry.circuit_failure breaker;
  check bool "still closed" true (Chain_executor_retry.circuit_allows breaker)

(* Empty batch *)
let test_empty_batch () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let results = Chain_executor_retry.execute_batch_with_retry ~clock [] in
  check int "empty" 0 (List.length results)

(* Error message edge cases *)
let test_error_message_edge () =
  let cases = [
    ("", false);
    ("   ", false);
    ("TIMEOUT", true);  (* case insensitive *)
    ("rate limit", true);  (* space, not underscore *)
    ("Connection timeout: 30s", true);
  ] in
  List.iter (fun (msg, expected) ->
    check bool msg expected (Chain_executor_retry.is_recoverable_message msg)
  ) cases

let () =
  run "Retry Edge Cases" [
    "policy", [
      test_case "zero_delay" `Quick test_zero_delay;
      test_case "single_attempt" `Quick test_single_attempt;
      test_case "always_succeed" `Quick test_always_succeed;
    ];
    "errors", [
      test_case "consistent" `Quick test_consistent_errors;
      test_case "mixed" `Quick test_mixed_errors;
      test_case "message_edge" `Quick test_error_message_edge;
    ];
    "breaker", [
      test_case "at_threshold" `Quick test_breaker_at_threshold;
      test_case "success_resets" `Quick test_breaker_success_resets;
    ];
    "batch", [
      test_case "empty" `Quick test_empty_batch;
    ];
  ]

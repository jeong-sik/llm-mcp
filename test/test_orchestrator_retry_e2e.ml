(** E2E Test: Chain Orchestrator with Retry Integration

    Tests that retry logic is actually invoked during chain execution.
*)

open Alcotest

(** Test that recoverable errors trigger retry in orchestrator context *)
let test_retry_invoked_on_rate_limit () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  
  (* Simulate an LLM call that fails twice then succeeds *)
  let mock_exec () =
    incr call_count;
    if !call_count < 3 then
      Error (Error.Llm (Error.GeminiError Error.GeminiRateLimit))
    else
      Ok "success after retry"
  in
  
  let result = Chain_executor_retry.execute_llm_with_retry 
    ~clock ~provider:"test" mock_exec in
  
  check int "retried correctly" 3 !call_count;
  check int "attempts recorded" 3 result.Chain_retry.attempts;
  match result.Chain_retry.value with
  | Ok v -> check string "success" "success after retry" v
  | Error _ -> fail "expected success"

(** Test circuit breaker integration *)
let test_circuit_breaker_e2e () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let breaker = Chain_executor_retry.create_breaker ~failure_threshold:2 () in
  
  (* First call succeeds *)
  let r1 = Chain_executor_retry.execute_with_breaker 
    ~clock ~breaker ~node_id:"node-1" (fun () -> Ok "ok") in
  check bool "first succeeds" true (Result.is_ok r1);
  
  (* Next two fail, should open circuit *)
  let _ = Chain_executor_retry.execute_with_breaker 
    ~clock ~breaker ~node_id:"node-2" (fun () -> Error "fail1") in
  let _ = Chain_executor_retry.execute_with_breaker 
    ~clock ~breaker ~node_id:"node-3" (fun () -> Error "fail2") in
  
  (* Circuit should be open now *)
  let r4 = Chain_executor_retry.execute_with_breaker 
    ~clock ~breaker ~node_id:"node-4" (fun () -> Ok "should not run") in
  check bool "circuit open" true (Result.is_error r4)

(** Test error classification for real error messages *)
let test_real_error_classification () =
  let cases = [
    ("Error: 429 Too Many Requests", true);
    ("connection reset by peer", true);
    ("ETIMEDOUT after 30000ms", true);
    ("Rate limit exceeded. Please retry after 60 seconds.", true);
    ("Invalid API key", false);
    ("JSON parse error: unexpected token", false);
    ("Model not found: gpt-99", false);
  ] in
  List.iter (fun (msg, expected) ->
    let is_rec = Chain_executor_retry.is_recoverable_message msg in
    check bool msg expected is_rec
  ) cases

(** Test batch execution with mixed results *)
let test_batch_with_failures () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  
  let nodes = [
    ("success-1", fun () -> Ok "result-1");
    ("fail-recoverable", fun () -> Error "connection timeout");
    ("success-2", fun () -> Ok "result-2");
    ("fail-permanent", fun () -> Error "invalid syntax");
  ] in
  
  let results = Chain_executor_retry.execute_batch_with_retry ~clock nodes in
  
  check int "all processed" 4 (List.length results);
  
  let successes = List.filter (fun (_, r) -> Result.is_ok r) results in
  check int "2 successes" 2 (List.length successes)

(** Test that non-recoverable errors don't retry *)
let test_no_retry_for_auth_errors () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  
  let mock_exec () =
    incr call_count;
    Error (Error.Llm (Error.GeminiError Error.GeminiAuth))
  in
  
  let result = Chain_executor_retry.execute_llm_with_retry 
    ~clock ~provider:"test" mock_exec in
  
  check int "no retry for auth" 1 !call_count;
  check bool "failed" true (Result.is_error result.Chain_retry.value)

let () =
  run "Orchestrator Retry E2E" [
    "integration", [
      test_case "retry_on_rate_limit" `Quick test_retry_invoked_on_rate_limit;
      test_case "circuit_breaker" `Quick test_circuit_breaker_e2e;
      test_case "error_classification" `Quick test_real_error_classification;
      test_case "batch_with_failures" `Quick test_batch_with_failures;
      test_case "no_retry_auth" `Quick test_no_retry_for_auth_errors;
    ];
  ]

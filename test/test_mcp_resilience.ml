(** MCP Resilience Tests - Circuit Breaker, Retry, Timeout

    Tests external MCP error handling patterns:
    1. Circuit breaker state transitions
    2. Retry with exponential backoff
    3. Timeout handling
    4. Error classification (retriable vs permanent)

    Phase 4.2 of MCP Unify project.
*)

open Alcotest

module Resilience = Mcp_resilience

(** {1 Circuit Breaker Tests} *)

let test_circuit_breaker_closed_allows_requests () =
  Eio_main.run @@ fun _ ->
  let cb = Resilience.create_circuit_breaker ~name:"test_cb" ~failure_threshold:3 () in
  check bool "closed circuit allows" true (Resilience.circuit_allows cb);
  check bool "still allows" true (Resilience.circuit_allows cb)

let test_circuit_breaker_opens_after_failures () =
  Eio_main.run @@ fun _ ->
  let cb = Resilience.create_circuit_breaker ~name:"test_open" ~failure_threshold:3 () in

  (* Record 3 failures to trigger open *)
  Resilience.circuit_record_failure cb;
  check bool "1 failure - still closed" true (Resilience.circuit_allows cb);

  Resilience.circuit_record_failure cb;
  check bool "2 failures - still closed" true (Resilience.circuit_allows cb);

  Resilience.circuit_record_failure cb;
  check bool "3 failures - now open" false (Resilience.circuit_allows cb)

let test_circuit_breaker_success_resets_count () =
  Eio_main.run @@ fun _ ->
  let cb = Resilience.create_circuit_breaker ~name:"test_reset" ~failure_threshold:3 () in

  (* 2 failures *)
  Resilience.circuit_record_failure cb;
  Resilience.circuit_record_failure cb;

  (* Success should reset *)
  Resilience.circuit_record_success cb;

  (* 2 more failures should NOT open (reset) *)
  Resilience.circuit_record_failure cb;
  Resilience.circuit_record_failure cb;
  check bool "reset by success" true (Resilience.circuit_allows cb)

let test_circuit_breaker_half_open_recovery () =
  Eio_main.run @@ fun _ ->
  let cb = Resilience.create_circuit_breaker
    ~name:"test_half"
    ~failure_threshold:2
    ~success_threshold:2
    ~timeout_ms:10  (* Very short timeout for test *)
    () in

  (* Open the circuit *)
  Resilience.circuit_record_failure cb;
  Resilience.circuit_record_failure cb;
  check bool "circuit open" false (Resilience.circuit_allows cb);

  (* Wait for timeout to transition to HalfOpen *)
  Unix.sleepf 0.05;  (* 50ms > 10ms timeout for stability *)
  check bool "half-open allows probe" true (Resilience.circuit_allows cb);

  (* Success in half-open *)
  Resilience.circuit_record_success cb;
  Resilience.circuit_record_success cb;

  (* Should be closed now *)
  check bool "closed after recovery" true (Resilience.circuit_allows cb)

(** {1 Retry Tests} *)

let test_retry_success_on_first_attempt () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in

  let result = Resilience.with_retry_eio
    ~clock
    ~policy:Resilience.default_policy
    ~op_name:"test_first"
    ~classify:(fun _ -> Resilience.Retry)
    (fun () ->
      incr attempts;
      Resilience.Ok "success")
  in

  check int "single attempt" 1 !attempts;
  (match result with
   | Resilience.Ok v -> check string "result" "success" v
   | _ -> fail "expected Ok")

let test_retry_success_after_failures () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in

  let result = Resilience.with_retry_eio
    ~clock
    ~policy:{Resilience.default_policy with max_attempts = 5; initial_delay_ms = 1}
    ~op_name:"test_retry"
    ~classify:(fun _ -> Resilience.Retry)
    (fun () ->
      incr attempts;
      if !attempts < 3 then Resilience.Error "transient"
      else Resilience.Ok "recovered")
  in

  check int "3 attempts" 3 !attempts;
  (match result with
   | Resilience.Ok v -> check string "result" "recovered" v
   | _ -> fail "expected Ok")

let test_retry_permanent_failure_no_retry () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in

  let result = Resilience.with_retry_eio
    ~clock
    ~policy:Resilience.default_policy
    ~op_name:"test_permanent"
    ~classify:(fun _ -> Resilience.Fail "permanent error")
    (fun () ->
      incr attempts;
      Resilience.Error "bad request")
  in

  check int "single attempt" 1 !attempts;
  (match result with
   | Resilience.Error msg -> check string "error" "permanent error" msg
   | _ -> fail "expected Error")

let test_retry_max_attempts_exceeded () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in

  let result = Resilience.with_retry_eio
    ~clock
    ~policy:{Resilience.default_policy with max_attempts = 3; initial_delay_ms = 1}
    ~op_name:"test_max"
    ~classify:(fun _ -> Resilience.Retry)
    (fun () ->
      incr attempts;
      Resilience.Error "always fails")
  in

  check int "3 attempts" 3 !attempts;
  (match result with
   | Resilience.Error _ -> ()
   | _ -> fail "expected Error")

let test_retry_with_circuit_breaker () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let cb = Resilience.create_circuit_breaker ~name:"test_cb_retry" ~failure_threshold:2 () in
  let attempts = ref 0 in

  (* First call - fails and opens circuit *)
  let _ = Resilience.with_retry_eio
    ~clock
    ~policy:{Resilience.default_policy with max_attempts = 3; initial_delay_ms = 1}
    ~circuit_breaker:(Some cb)
    ~op_name:"test_cb1"
    ~classify:(fun _ -> Resilience.Retry)
    (fun () ->
      incr attempts;
      Resilience.Error "fail")
  in

  (* Circuit should be open now, second call should be rejected immediately *)
  attempts := 0;
  let result = Resilience.with_retry_eio
    ~clock
    ~policy:Resilience.default_policy
    ~circuit_breaker:(Some cb)
    ~op_name:"test_cb2"
    ~classify:(fun _ -> Resilience.Retry)
    (fun () ->
      incr attempts;
      Resilience.Ok "should not run")
  in

  check int "no attempts (circuit open)" 0 !attempts;
  (match result with
   | Resilience.CircuitOpen -> ()
   | _ -> fail "expected CircuitOpen")

(** {1 Timeout Tests} *)

let test_timeout_success_within_limit () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  let result = Resilience.with_timeout_eio ~clock ~timeout_ms:1000
    (fun () -> "fast")
  in

  (match result with
   | Resilience.Ok v -> check string "result" "fast" v
   | _ -> fail "expected Ok")

let test_timeout_exceeded () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  let result = Resilience.with_timeout_eio ~clock ~timeout_ms:10
    (fun () ->
      Eio.Time.sleep clock 0.2;  (* 200ms > 10ms timeout for stability *)
      "slow")
  in

  (match result with
   | Resilience.Error msg -> check string "timeout error" "Timeout" msg
   | _ -> fail "expected timeout Error")

(** {1 Backoff Calculation Tests} *)

let test_backoff_exponential_growth () =
  let policy = { Resilience.default_policy with
    initial_delay_ms = 100;
    backoff_multiplier = 2.0;
    max_delay_ms = 10000;
    jitter = false;
  } in

  let d1 = Resilience.calculate_delay policy 1 in
  let d2 = Resilience.calculate_delay policy 2 in
  let d3 = Resilience.calculate_delay policy 3 in
  let d4 = Resilience.calculate_delay policy 4 in

  check (float 0.1) "attempt 1" 100.0 d1;
  check (float 0.1) "attempt 2" 200.0 d2;
  check (float 0.1) "attempt 3" 400.0 d3;
  check (float 0.1) "attempt 4" 800.0 d4

let test_backoff_max_cap () =
  let policy = { Resilience.default_policy with
    initial_delay_ms = 1000;
    backoff_multiplier = 10.0;
    max_delay_ms = 5000;
    jitter = false;
  } in

  let d1 = Resilience.calculate_delay policy 1 in
  let d2 = Resilience.calculate_delay policy 2 in
  let d3 = Resilience.calculate_delay policy 3 in

  check (float 0.1) "attempt 1" 1000.0 d1;
  check (float 0.1) "attempt 2 (capped)" 5000.0 d2;
  check (float 0.1) "attempt 3 (capped)" 5000.0 d3

let test_backoff_jitter_variance () =
  let policy = { Resilience.default_policy with
    initial_delay_ms = 1000;
    backoff_multiplier = 1.0;
    jitter = true;
  } in

  (* Run multiple times to check jitter produces different values *)
  let delays = List.init 10 (fun _ -> Resilience.calculate_delay policy 1) in
  let unique = List.sort_uniq compare delays in

  (* With jitter, we expect at least some variation *)
  check bool "jitter produces variance" true (List.length unique > 1)

(** {1 Concurrent Stress Tests} *)

let test_circuit_breaker_concurrent_access () =
  Eio_main.run @@ fun _ ->
  let cb = Resilience.create_circuit_breaker ~name:"test_concurrent" ~failure_threshold:100 () in
  let success_count = Atomic.make 0 in
  let failure_count = Atomic.make 0 in

  let worker () =
    for _ = 1 to 50 do
      if Random.bool () then begin
        Resilience.circuit_record_success cb;
        Atomic.incr success_count
      end else begin
        Resilience.circuit_record_failure cb;
        Atomic.incr failure_count
      end;
      ignore (Resilience.circuit_allows cb)
    done
  in

  Eio.Fiber.all [worker; worker; worker; worker];

  let total = Atomic.get success_count + Atomic.get failure_count in
  check int "all operations completed" 200 total;
  Printf.printf "Successes: %d, Failures: %d\n%!"
    (Atomic.get success_count) (Atomic.get failure_count)

(** {1 Test Suite} *)

let () =
  run "MCP_Resilience" [
    "circuit_breaker", [
      test_case "closed allows requests" `Quick test_circuit_breaker_closed_allows_requests;
      test_case "opens after failures" `Quick test_circuit_breaker_opens_after_failures;
      test_case "success resets count" `Quick test_circuit_breaker_success_resets_count;
      test_case "half-open recovery" `Quick test_circuit_breaker_half_open_recovery;
      test_case "concurrent access" `Slow test_circuit_breaker_concurrent_access;
    ];
    "retry", [
      test_case "success on first attempt" `Quick test_retry_success_on_first_attempt;
      test_case "success after failures" `Quick test_retry_success_after_failures;
      test_case "permanent failure no retry" `Quick test_retry_permanent_failure_no_retry;
      test_case "max attempts exceeded" `Quick test_retry_max_attempts_exceeded;
      test_case "with circuit breaker" `Quick test_retry_with_circuit_breaker;
    ];
    "timeout", [
      test_case "success within limit" `Quick test_timeout_success_within_limit;
      test_case "timeout exceeded" `Slow test_timeout_exceeded;
    ];
    "backoff", [
      test_case "exponential growth" `Quick test_backoff_exponential_growth;
      test_case "max cap" `Quick test_backoff_max_cap;
      test_case "jitter variance" `Quick test_backoff_jitter_variance;
    ];
  ]

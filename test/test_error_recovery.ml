(** Integration Test: Error Recovery Scenarios

    Tests chain execution resilience under various error conditions.
    Verifies retry logic, fallback behavior, and error propagation.

    @since 0.4.0
*)

open Alcotest

(** {1 Test Helpers} *)

let make_recoverable_error () =
  Error.Llm (Error.GeminiError Error.GeminiRateLimit)

let _make_non_recoverable_error () =
  Error.Chain (Error.ChainCycleDetected)

let _make_timeout_error ms =
  Error.Chain (Error.ChainTimeoutError ms)

let make_network_error msg =
  Error.Io (Error.NetworkError msg)

(** {1 Basic Error Classification Tests} *)

let test_error_recoverable_classification () =
  let cases = [
    (Error.Llm (Error.GeminiError Error.GeminiRateLimit), true, "Gemini rate limit");
    (Error.Llm (Error.GeminiError Error.GeminiFunctionCallSync), true, "Gemini function sync");
    (Error.Llm (Error.ClaudeError Error.ClaudeRateLimit), true, "Claude rate limit");
    (Error.Llm (Error.OllamaError Error.OllamaTimeout), true, "Ollama timeout");
    (Error.Process (Error.ProcessTimeout 30), true, "Process timeout");
    (Error.Io (Error.NetworkError "conn refused"), true, "Network error");
    (Error.Chain Error.ChainCycleDetected, false, "Chain cycle");
    (Error.Llm (Error.GeminiError Error.GeminiAuth), false, "Auth error");
    (Error.Internal "bug", false, "Internal error");
  ] in
  List.iter (fun (err, expected, desc) ->
    check bool desc expected (Error.is_recoverable err)
  ) cases

let test_error_severity () =
  let cases = [
    (Error.Llm (Error.GeminiError Error.GeminiRateLimit), Error.Warning);
    (Error.Chain (Error.ChainParseError "syntax"), Error.Warning);
    (Error.Internal "critical bug", Error.Critical);
  ] in
  List.iter (fun (err, expected) ->
    let actual = Error.severity_of_error err in
    check bool (Error.to_string err) true (actual = expected)
  ) cases

(** {1 Retry Behavior Tests} *)

let test_retry_on_rate_limit () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    if !attempts < 3 then Error (make_recoverable_error ())
    else Ok "recovered"
  in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_retry ~clock ~policy f in
  
  check int "retried correct times" 3 !attempts;
  match result.value with
  | Ok v -> check string "recovered" "recovered" v
  | Error _ -> fail "should have recovered"

let test_no_retry_on_auth_error () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    Error (Error.Llm (Error.GeminiError Error.GeminiAuth))
  in
  let result = Chain_retry.execute_with_retry ~clock ~policy:Chain_retry.default_policy f in
  
  check int "no retry on auth" 1 !attempts;
  match result.value with
  | Ok _ -> fail "should have failed"
  | Error _ -> ()

let test_retry_exhaustion () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    Error (make_network_error "connection refused")
  in
  let policy = Chain_retry.{ 
    max_attempts = 4;
    base_delay_ms = 10;
    max_delay_ms = 100;
    exponential_base = 2.0;
    jitter = false;
  } in
  let result = Chain_retry.execute_with_retry ~clock ~policy f in
  
  check int "exhausted all attempts" 4 !attempts;
  check int "all errors recorded" 4 (List.length result.errors);
  match result.value with
  | Ok _ -> fail "should have failed"
  | Error e ->
      check bool "last error is network" true 
        (match e with Error.Io (Error.NetworkError _) -> true | _ -> false)

(** {1 Timeout Behavior Tests} *)

let test_timeout_interrupts_retry () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    Eio.Time.sleep clock 0.05;  (* 50ms per attempt *)
    Error (make_recoverable_error ())
  in
  let policy = Chain_retry.{ default_policy with max_attempts = 100; base_delay_ms = 50 } in
  let result = Chain_retry.execute_with_timeout ~clock ~policy ~timeout_ms:120 f in
  
  check bool "didn't exhaust attempts" true (!attempts < 100);
  match result.value with
  | Ok _ -> fail "should have timed out"
  | Error e ->
      check bool "timeout error" true
        (match e with Error.Chain (Error.ChainTimeoutError _) -> true | _ -> false)

(** {1 Cascading Error Tests} *)

let test_error_context_preserved () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let error_sequence = [
    make_network_error "attempt 1";
    make_recoverable_error ();
    make_network_error "attempt 3";
  ] in
  let idx = ref 0 in
  let f () =
    let err = List.nth error_sequence !idx in
    incr idx;
    Error err
  in
  let policy = Chain_retry.{ default_policy with max_attempts = 3; base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_retry ~clock ~policy f in
  
  check int "all errors preserved" 3 (List.length result.errors);
  (* Verify error order *)
  let first = List.hd result.errors in
  check bool "first error is network" true
    (match first with Error.Io (Error.NetworkError msg) -> msg = "attempt 1" | _ -> false)

(** {1 Provider-Specific Error Tests} *)

let test_gemini_function_call_sync_recovery () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    if !attempts < 2 then
      Error (Error.Llm (Error.GeminiError Error.GeminiFunctionCallSync))
    else
      Ok "function call succeeded"
  in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_retry ~clock ~policy f in
  
  check int "retried once" 2 !attempts;
  match result.value with
  | Ok v -> check string "success" "function call succeeded" v
  | Error _ -> fail "should have recovered from function call sync"

let test_ollama_not_running_not_recoverable () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    Error (Error.Llm (Error.OllamaError Error.OllamaNotRunning))
  in
  let result = Chain_retry.execute_with_retry ~clock ~policy:Chain_retry.default_policy f in
  
  check int "no retry" 1 !attempts;
  match result.value with
  | Ok _ -> fail "should have failed"
  | Error _ -> ()

(** {1 Custom Retry Filter Tests} *)

let test_custom_retry_filter () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    if !attempts < 3 then
      Error (Error.Llm (Error.GeminiError (Error.GeminiUnknown "custom error")))
    else
      Ok "success"
  in
  let should_retry = function
    | Error.Llm (Error.GeminiError (Error.GeminiUnknown _)) -> true
    | _ -> false
  in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_filter ~clock ~policy ~should_retry f in
  
  check int "retried with custom filter" 3 !attempts;
  match result.value with
  | Ok v -> check string "success" "success" v
  | Error _ -> fail "should have recovered"

(** {1 Retry Context Accumulation Tests} *)

let test_retry_context_accumulates () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let ctx = Chain_retry.create_retry_context () in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  
  (* Operation 1: 2 attempts *)
  let count1 = ref 0 in
  let _ = Chain_retry.execute_with_context ~clock ~policy ~ctx (fun () ->
    incr count1;
    if !count1 < 2 then Error (make_recoverable_error ()) else Ok "op1"
  ) in
  
  (* Operation 2: 3 attempts *)
  let count2 = ref 0 in
  let _ = Chain_retry.execute_with_context ~clock ~policy ~ctx (fun () ->
    incr count2;
    if !count2 < 3 then Error (make_recoverable_error ()) else Ok "op2"
  ) in
  
  check int "total attempts" 5 ctx.total_attempts;  (* 2 + 3 *)
  check int "total retries" 3 ctx.total_retries;    (* 1 + 2 *)
  check bool "delay accumulated" true (ctx.total_delay_ms > 0)

let () =
  run "Error Recovery" [
    "classification", [
      test_case "recoverable" `Quick test_error_recoverable_classification;
      test_case "severity" `Quick test_error_severity;
    ];
    "retry_behavior", [
      test_case "rate_limit" `Quick test_retry_on_rate_limit;
      test_case "no_retry_auth" `Quick test_no_retry_on_auth_error;
      test_case "exhaustion" `Quick test_retry_exhaustion;
    ];
    "timeout", [
      test_case "interrupts_retry" `Quick test_timeout_interrupts_retry;
    ];
    "cascading", [
      test_case "context_preserved" `Quick test_error_context_preserved;
    ];
    "providers", [
      test_case "gemini_function_sync" `Quick test_gemini_function_call_sync_recovery;
      test_case "ollama_not_running" `Quick test_ollama_not_running_not_recoverable;
    ];
    "custom", [
      test_case "filter" `Quick test_custom_retry_filter;
      test_case "context_accumulates" `Quick test_retry_context_accumulates;
    ];
  ]

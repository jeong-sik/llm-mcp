(** Tests for Chain_retry module *)

open Alcotest

let test_calculate_delay () =
  let policy = Chain_retry.{ default_policy with jitter = false } in
  
  check int "attempt 0 = no delay" 0 (Chain_retry.calculate_delay policy 0);
  check int "attempt 1 = base delay" 1000 (Chain_retry.calculate_delay policy 1);
  check int "attempt 2 = 2x base" 2000 (Chain_retry.calculate_delay policy 2);
  check int "attempt 3 = 4x base" 4000 (Chain_retry.calculate_delay policy 3)

let test_calculate_delay_capped () =
  let policy = Chain_retry.{ 
    default_policy with 
    jitter = false; 
    max_delay_ms = 3000 
  } in
  
  check int "capped at max" 3000 (Chain_retry.calculate_delay policy 10)

let test_calculate_delay_with_jitter () =
  let policy = Chain_retry.{ default_policy with jitter = true } in
  let delays = List.init 10 (fun _ -> Chain_retry.calculate_delay policy 1) in
  let all_same = List.for_all (fun d -> d = List.hd delays) delays in
  check bool "delays vary with jitter" false all_same

let test_no_retry_policy () =
  let policy = Chain_retry.no_retry_policy in
  check int "max_attempts = 1" 1 policy.max_attempts

let test_execute_with_retry_success () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Ok "success"
  in
  let result = Chain_retry.execute_with_retry ~clock ~policy:Chain_retry.default_policy f in
  check int "called once" 1 !call_count;
  check int "one attempt" 1 result.attempts;
  check int "no delay" 0 result.total_delay_ms;
  check (list pass) "no errors" [] result.errors;
  match result.value with
  | Ok v -> check string "value" "success" v
  | Error _ -> fail "expected success"

let test_execute_with_retry_recoverable_error () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    if !call_count < 3 then
      Error (Error.Llm (Error.GeminiError Error.GeminiRateLimit))
    else
      Ok "success after retry"
  in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_retry ~clock ~policy f in
  check int "called 3 times" 3 !call_count;
  check int "3 attempts" 3 result.attempts;
  check bool "some delay" true (result.total_delay_ms > 0);
  check int "2 errors recorded" 2 (List.length result.errors);
  match result.value with
  | Ok v -> check string "value" "success after retry" v
  | Error _ -> fail "expected success after retry"

let test_execute_with_retry_non_recoverable () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Error (Error.Chain (Error.ChainCycleDetected))  (* Non-recoverable *)
  in
  let result = Chain_retry.execute_with_retry ~clock ~policy:Chain_retry.default_policy f in
  check int "called once (no retry)" 1 !call_count;
  check int "1 attempt" 1 result.attempts;
  match result.value with
  | Ok _ -> fail "expected error"
  | Error _ -> ()

let test_execute_with_retry_max_attempts () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Error (Error.Io (Error.NetworkError "connection failed"))
  in
  let policy = Chain_retry.{ default_policy with max_attempts = 3; base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_retry ~clock ~policy f in
  check int "called max times" 3 !call_count;
  check int "max attempts" 3 result.attempts;
  match result.value with
  | Ok _ -> fail "expected error"
  | Error _ -> check int "all errors recorded" 3 (List.length result.errors)

let test_execute_with_timeout () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let f () =
    incr attempts;
    (* Each attempt returns a recoverable error *)
    Error (Error.Llm (Error.GeminiError Error.GeminiRateLimit))
  in
  let policy = Chain_retry.{ default_policy with max_attempts = 100; base_delay_ms = 50; jitter = false } in
  (* Very short timeout - should cut off retries *)
  let result = Chain_retry.execute_with_timeout ~clock ~policy ~timeout_ms:80 f in
  (* Should have been cut off before exhausting all 100 attempts *)
  check bool "didn't exhaust attempts" true (!attempts < 100);
  match result.value with
  | Ok _ -> fail "expected timeout or failure"
  | Error _ -> () (* Either timeout or last error is fine *)

let test_execute_with_filter () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    if !call_count < 3 then
      Error (Error.Io (Error.NetworkError "transient"))
    else
      Ok "success"
  in
  let should_retry = function
    | Error.Io (Error.NetworkError _) -> true
    | _ -> false
  in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  let result = Chain_retry.execute_with_filter ~clock ~policy ~should_retry f in
  check int "retried with filter" 3 !call_count;
  match result.value with
  | Ok v -> check string "value" "success" v
  | Error _ -> fail "expected success"

let test_retry_context () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let ctx = Chain_retry.create_retry_context () in
  
  (* First operation: succeeds immediately *)
  let f1 () = Ok "first" in
  let _ = Chain_retry.execute_with_context ~clock ~policy:Chain_retry.default_policy ~ctx f1 in
  
  (* Second operation: retries twice *)
  let call_count = ref 0 in
  let f2 () =
    incr call_count;
    if !call_count < 2 then
      Error (Error.Llm (Error.GeminiError Error.GeminiRateLimit))
    else
      Ok "second"
  in
  let policy = Chain_retry.{ default_policy with base_delay_ms = 10; jitter = false } in
  let _ = Chain_retry.execute_with_context ~clock ~policy ~ctx f2 in
  
  check int "total attempts" 3 ctx.total_attempts;  (* 1 + 2 *)
  check int "total retries" 1 ctx.total_retries;    (* 0 + 1 *)
  check bool "has error counts" true (Hashtbl.length ctx.error_counts > 0)

let () =
  run "Chain_retry" [
    "delay", [
      test_case "calculate_delay" `Quick test_calculate_delay;
      test_case "calculate_delay_capped" `Quick test_calculate_delay_capped;
      test_case "calculate_delay_with_jitter" `Quick test_calculate_delay_with_jitter;
      test_case "no_retry_policy" `Quick test_no_retry_policy;
    ];
    "execution", [
      test_case "success" `Quick test_execute_with_retry_success;
      test_case "recoverable_error" `Quick test_execute_with_retry_recoverable_error;
      test_case "non_recoverable" `Quick test_execute_with_retry_non_recoverable;
      test_case "max_attempts" `Quick test_execute_with_retry_max_attempts;
      test_case "with_timeout" `Quick test_execute_with_timeout;
      test_case "with_filter" `Quick test_execute_with_filter;
      test_case "retry_context" `Quick test_retry_context;
    ];
  ]

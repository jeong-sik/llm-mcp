(** Unit tests for Retry module *)

open Alcotest

let test_calculate_delay () =
  let config = Retry.default_config in
  (* First attempt: should be initial_delay_ms *)
  let delay1 = Retry.calculate_delay ~config ~attempt:1 in
  check bool "delay1 near initial" true (delay1 >= 750 && delay1 <= 1250);  (* 1000ms ± 25% jitter *)

  (* Second attempt: should be ~2x *)
  let delay2 = Retry.calculate_delay ~config ~attempt:2 in
  check bool "delay2 near 2x" true (delay2 >= 1500 && delay2 <= 2500);

  (* Third attempt: should be ~4x *)
  let delay3 = Retry.calculate_delay ~config ~attempt:3 in
  check bool "delay3 near 4x" true (delay3 >= 3000 && delay3 <= 5000)

let test_calculate_delay_no_jitter () =
  let config = { Retry.default_config with jitter = false } in
  let delay1 = Retry.calculate_delay ~config ~attempt:1 in
  check int "delay1 exact" 1000 delay1;
  let delay2 = Retry.calculate_delay ~config ~attempt:2 in
  check int "delay2 exact" 2000 delay2;
  let delay3 = Retry.calculate_delay ~config ~attempt:3 in
  check int "delay3 exact" 4000 delay3

let test_calculate_delay_max_cap () =
  let config = { Retry.default_config with jitter = false; max_delay_ms = 5000 } in
  (* 1000 * 2^10 = 1024000, but should be capped at 5000 *)
  let delay = Retry.calculate_delay ~config ~attempt:10 in
  check int "delay capped" 5000 delay

let test_is_rate_limit_error () =
  check bool "429" true (Retry.is_rate_limit_error "HTTP 429 Too Many Requests");
  check bool "rate_limit" true (Retry.is_rate_limit_error {|{"error": "rate_limit_exceeded"}|});
  check bool "quota" true (Retry.is_rate_limit_error "Error: quota exceeded");
  check bool "normal" false (Retry.is_rate_limit_error "Hello world")

let test_is_temporary_error () =
  check bool "503" true (Retry.is_temporary_error "HTTP 503 Service Unavailable");
  check bool "502" true (Retry.is_temporary_error "Bad Gateway");
  check bool "500" true (Retry.is_temporary_error "HTTP 500 Internal Server Error");  (* 500s are also retryable *)
  check bool "normal" false (Retry.is_temporary_error "OK")

let test_is_connection_error () =
  check bool "refused" true (Retry.is_connection_error "Connection refused");
  check bool "dns" true (Retry.is_connection_error "Could not resolve host");
  check bool "normal" false (Retry.is_connection_error "Success")

let test_is_retryable_error () =
  check bool "rate_limit" true (Retry.is_retryable_error "rate_limit");
  check bool "503" true (Retry.is_retryable_error "503");
  check bool "connection" true (Retry.is_retryable_error "ECONNREFUSED");
  check bool "auth" false (Retry.is_retryable_error "401 Unauthorized");
  check bool "normal" false (Retry.is_retryable_error "Success")

let test_parse_retry_after () =
  (* GLM style *)
  check (option int) "glm style" (Some 5000)
    (Retry.parse_retry_after {|{"error": {"retry_after": 5}}|});
  (* Missing *)
  check (option int) "missing" None
    (Retry.parse_retry_after {|{"status": "ok"}|});
  (* Invalid JSON *)
  check (option int) "invalid" None
    (Retry.parse_retry_after "not json")

let test_to_result () =
  let success : int Retry.retry_result = Retry.Success 42 in
  check (result int string) "success" (Ok 42) (Retry.to_result success);

  let failure : int Retry.retry_result = Retry.AllAttemptsFailed {
    attempts = 3;
    last_error = "error";
    errors = ["e1"; "e2"; "e3"];
  } in
  check (result int string) "failure" (Error "error") (Retry.to_result failure)

let test_map () =
  let success : int Retry.retry_result = Retry.Success 10 in
  let mapped = Retry.map (fun x -> x * 2) success in
  check (result int string) "mapped success" (Ok 20) (Retry.to_result mapped);

  let failure : int Retry.retry_result = Retry.AllAttemptsFailed {
    attempts = 1;
    last_error = "err";
    errors = ["err"];
  } in
  let mapped_fail = Retry.map (fun x -> x * 2) failure in
  check (result int string) "mapped failure" (Error "err") (Retry.to_result mapped_fail)

let () =
  run "Retry" [
    "calculate_delay", [
      test_case "with jitter" `Quick test_calculate_delay;
      test_case "no jitter" `Quick test_calculate_delay_no_jitter;
      test_case "max cap" `Quick test_calculate_delay_max_cap;
    ];
    "error_detection", [
      test_case "rate limit" `Quick test_is_rate_limit_error;
      test_case "temporary" `Quick test_is_temporary_error;
      test_case "connection" `Quick test_is_connection_error;
      test_case "retryable" `Quick test_is_retryable_error;
    ];
    "utilities", [
      test_case "parse_retry_after" `Quick test_parse_retry_after;
      test_case "to_result" `Quick test_to_result;
      test_case "map" `Quick test_map;
    ];
  ]

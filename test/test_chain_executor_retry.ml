(** Tests for Chain_executor_retry module *)

open Alcotest

let test_is_recoverable_message () =
  let cases = [
    ("Connection timeout after 30s", true);
    ("Rate limit exceeded (429)", true);
    ("connection refused", true);
    ("Service temporarily unavailable", true);
    ("503 Service Unavailable", true);
    ("ETIMEDOUT", true);
    ("Invalid JSON format", false);
    ("Missing required parameter", false);
    ("Authentication failed", false);
    ("Syntax error in query", false);
  ] in
  List.iter (fun (msg, expected) ->
    check bool msg expected (Chain_executor_retry.is_recoverable_message msg)
  ) cases

let test_classify_error () =
  let recoverable = Chain_executor_retry.classify_error "Connection timeout" in
  check bool "timeout is recoverable" true (Error.is_recoverable recoverable);
  
  let non_recoverable = Chain_executor_retry.classify_error "Invalid syntax" in
  check bool "syntax error not recoverable" false (Error.is_recoverable non_recoverable)

let test_execute_with_retry_success () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Ok "success"
  in
  let result = Chain_executor_retry.execute_with_retry ~clock ~node_id:"test-node" f in
  check int "called once" 1 !call_count;
  match result with
  | Ok v -> check string "value" "success" v
  | Error _ -> fail "expected success"

let test_execute_with_retry_recoverable () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    if !call_count < 3 then
      Error "Connection timeout"  (* Recoverable *)
    else
      Ok "recovered"
  in
  let policy = Chain_retry.{ 
    Chain_executor_retry.default_node_policy with 
    base_delay_ms = 10; 
    jitter = false 
  } in
  let result = Chain_executor_retry.execute_with_retry ~clock ~policy ~node_id:"retry-node" f in
  check int "retried" 3 !call_count;
  match result with
  | Ok v -> check string "value" "recovered" v
  | Error _ -> fail "expected recovery"

let test_execute_with_retry_non_recoverable () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Error "Invalid syntax error"  (* Not recoverable *)
  in
  let result = Chain_executor_retry.execute_with_retry ~clock ~node_id:"no-retry-node" f in
  check int "no retry" 1 !call_count;
  match result with
  | Ok _ -> fail "expected error"
  | Error _ -> ()

let test_execute_llm_with_retry () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    if !call_count < 2 then
      Error (Error.Llm (Error.GeminiError Error.GeminiRateLimit))
    else
      Ok "llm response"
  in
  let result = Chain_executor_retry.execute_llm_with_retry ~clock ~provider:"gemini" f in
  check int "retried" 2 !call_count;
  match result.value with
  | Ok v -> check string "value" "llm response" v
  | Error _ -> fail "expected success"

let test_execute_tool_with_retry () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let f () =
    incr call_count;
    Ok "tool result"
  in
  let result = Chain_executor_retry.execute_tool_with_retry ~clock ~tool_name:"test_tool" f in
  check int "called once" 1 !call_count;
  match result.value with
  | Ok v -> check string "value" "tool result" v
  | Error _ -> fail "expected success"

let test_execute_batch_with_retry () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let nodes = [
    ("node-1", fun () -> Ok "result-1");
    ("node-2", fun () -> Ok "result-2");
    ("node-3", fun () -> Error "some error");
  ] in
  let results = Chain_executor_retry.execute_batch_with_retry ~clock nodes in
  check int "all nodes processed" 3 (List.length results);
  
  let (_, r1) = List.nth results 0 in
  let (_, r2) = List.nth results 1 in
  let (_, r3) = List.nth results 2 in
  
  check bool "node-1 success" true (Result.is_ok r1);
  check bool "node-2 success" true (Result.is_ok r2);
  check bool "node-3 error" true (Result.is_error r3)

(* Circuit breaker tests *)
let test_circuit_breaker_closed () =
  Eio_main.run @@ fun _env ->
  let breaker = Chain_executor_retry.create_breaker () in
  check bool "initially allows" true (Chain_executor_retry.circuit_allows breaker)

let test_circuit_breaker_opens () =
  Eio_main.run @@ fun _env ->
  let breaker = Chain_executor_retry.create_breaker ~failure_threshold:3 () in
  
  (* Record failures *)
  Chain_executor_retry.circuit_failure breaker;
  Chain_executor_retry.circuit_failure breaker;
  check bool "still allows after 2 failures" true (Chain_executor_retry.circuit_allows breaker);
  
  Chain_executor_retry.circuit_failure breaker;
  check bool "opens after 3 failures" false (Chain_executor_retry.circuit_allows breaker)

let test_circuit_breaker_resets () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let breaker = Chain_executor_retry.create_breaker ~failure_threshold:2 ~reset_timeout:0.01 () in
  
  (* Open the circuit *)
  Chain_executor_retry.circuit_failure breaker;
  Chain_executor_retry.circuit_failure breaker;
  check bool "circuit open" false (Chain_executor_retry.circuit_allows breaker);
  
  (* Wait for reset *)
  Eio.Time.sleep clock 0.02;
  check bool "circuit half-open" true (Chain_executor_retry.circuit_allows breaker);
  
  (* Success resets to closed *)
  Chain_executor_retry.circuit_success breaker;
  check bool "circuit closed" true (Chain_executor_retry.circuit_allows breaker)

let test_execute_with_breaker () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let breaker = Chain_executor_retry.create_breaker ~failure_threshold:10 () in
  
  let result = Chain_executor_retry.execute_with_breaker 
    ~clock ~breaker ~node_id:"breaker-node" 
    (fun () -> Ok "success") in
  
  match result with
  | Ok v -> check string "value" "success" v
  | Error _ -> fail "expected success"

let test_execute_with_breaker_open () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let breaker = Chain_executor_retry.create_breaker ~failure_threshold:1 () in
  
  (* Open the circuit *)
  Chain_executor_retry.circuit_failure breaker;
  
  let result = Chain_executor_retry.execute_with_breaker 
    ~clock ~breaker ~node_id:"blocked-node" 
    (fun () -> Ok "should not run") in
  
  match result with
  | Ok _ -> fail "expected circuit open error"
  | Error msg -> check bool "circuit open message" true 
      (String.length msg > 0)

let () =
  run "Chain_executor_retry" [
    "classification", [
      test_case "is_recoverable_message" `Quick test_is_recoverable_message;
      test_case "classify_error" `Quick test_classify_error;
    ];
    "retry", [
      test_case "success" `Quick test_execute_with_retry_success;
      test_case "recoverable" `Quick test_execute_with_retry_recoverable;
      test_case "non_recoverable" `Quick test_execute_with_retry_non_recoverable;
      test_case "llm_retry" `Quick test_execute_llm_with_retry;
      test_case "tool_retry" `Quick test_execute_tool_with_retry;
      test_case "batch" `Quick test_execute_batch_with_retry;
    ];
    "circuit_breaker", [
      test_case "closed" `Quick test_circuit_breaker_closed;
      test_case "opens" `Quick test_circuit_breaker_opens;
      test_case "resets" `Quick test_circuit_breaker_resets;
      test_case "execute_with_breaker" `Quick test_execute_with_breaker;
      test_case "execute_blocked" `Quick test_execute_with_breaker_open;
    ];
  ]

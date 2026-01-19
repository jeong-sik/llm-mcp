(** Tests for Lwt Bridge

    Tests for Lwt/Eio interoperability.
    Uses Eio_main.run with Lwt_eio integration.
*)

module Bridge = Agent_core_bridge.Lwt_bridge

(** {1 Test Helpers} **)

let run_with_lwt f =
  Eio_main.run @@ fun env ->
  Bridge.with_lwt env f

(** {1 Basic Interop Tests} **)

let test_run_lwt_simple () =
  run_with_lwt @@ fun () ->
  let result = Bridge.run_lwt (Lwt.return 42) in
  Alcotest.(check int) "simple lwt" 42 result

let test_run_lwt_fn () =
  run_with_lwt @@ fun () ->
  let result = Bridge.run_lwt_fn (fun () -> Lwt.return "hello") in
  Alcotest.(check string) "lwt fn" "hello" result

let test_run_lwt_result_ok () =
  run_with_lwt @@ fun () ->
  let promise = Lwt.return (Ok 100) in
  match Bridge.run_lwt_result promise with
  | Ok v -> Alcotest.(check int) "result ok" 100 v
  | Error _ -> Alcotest.fail "expected Ok"

let test_run_lwt_result_error () =
  run_with_lwt @@ fun () ->
  let promise = Lwt.return (Error "test error") in
  match Bridge.run_lwt_result promise with
  | Error e -> Alcotest.(check string) "result error" "test error" e
  | Ok _ -> Alcotest.fail "expected Error"

let test_run_lwt_safe_success () =
  run_with_lwt @@ fun () ->
  match Bridge.run_lwt_safe (Lwt.return "safe") with
  | Ok v -> Alcotest.(check string) "safe success" "safe" v
  | Error _ -> Alcotest.fail "expected Ok"

let test_run_lwt_safe_exception () =
  run_with_lwt @@ fun () ->
  let promise = Lwt.fail (Failure "boom") in
  match Bridge.run_lwt_safe promise with
  | Error e ->
    (* Exception message contains "boom" somewhere *)
    let contains_boom = try
      let _ = Str.search_forward (Str.regexp_string "boom") e 0 in true
    with Not_found -> false
    in
    Alcotest.(check bool) "contains boom" true contains_boom
  | Ok _ -> Alcotest.fail "expected Error"

let test_run_lwt_all () =
  run_with_lwt @@ fun () ->
  let promises = [Lwt.return 1; Lwt.return 2; Lwt.return 3] in
  let results = Bridge.run_lwt_all promises in
  Alcotest.(check (list int)) "parallel results" [1; 2; 3] results

(** {1 Bidirectional Tests} **)

let test_run_eio_from_lwt () =
  run_with_lwt @@ fun () ->
  (* Run Lwt code that calls back into Eio *)
  let result = Bridge.run_lwt_fn @@ fun () ->
    let open Lwt.Syntax in
    let* r = Bridge.run_eio (fun () -> "from eio") in
    Lwt.return r
  in
  Alcotest.(check string) "eio from lwt" "from eio" result

let test_nested_interop () =
  run_with_lwt @@ fun () ->
  (* Eio -> Lwt -> Eio -> Lwt *)
  let result = Bridge.run_lwt_fn @@ fun () ->
    let open Lwt.Syntax in
    let* eio_result = Bridge.run_eio (fun () ->
      (* Back in Eio, but we can call Lwt again *)
      "nested"
    ) in
    Lwt.return (eio_result ^ "_done")
  in
  Alcotest.(check string) "nested" "nested_done" result

(** {1 Async Behavior Tests} **)

let test_lwt_pause () =
  run_with_lwt @@ fun () ->
  let result = Bridge.run_lwt_fn @@ fun () ->
    let open Lwt.Syntax in
    let* () = Lwt.pause () in
    Lwt.return "after pause"
  in
  Alcotest.(check string) "after pause" "after pause" result

let test_lwt_sleep () =
  run_with_lwt @@ fun () ->
  let start = Unix.gettimeofday () in
  let result = Bridge.run_lwt_fn @@ fun () ->
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.01 in  (* 10ms *)
    Lwt.return "after sleep"
  in
  let elapsed = Unix.gettimeofday () -. start in
  Alcotest.(check string) "after sleep" "after sleep" result;
  Alcotest.(check bool) "elapsed > 10ms" true (elapsed >= 0.01)

(** {1 Test Runner} **)

let basic_tests = [
  "run_lwt simple", `Quick, test_run_lwt_simple;
  "run_lwt_fn", `Quick, test_run_lwt_fn;
  "run_lwt_result ok", `Quick, test_run_lwt_result_ok;
  "run_lwt_result error", `Quick, test_run_lwt_result_error;
  "run_lwt_safe success", `Quick, test_run_lwt_safe_success;
  "run_lwt_safe exception", `Quick, test_run_lwt_safe_exception;
  "run_lwt_all", `Quick, test_run_lwt_all;
]

let bidirectional_tests = [
  "run_eio from lwt", `Quick, test_run_eio_from_lwt;
  "nested interop", `Quick, test_nested_interop;
]

let async_tests = [
  "lwt pause", `Quick, test_lwt_pause;
  "lwt sleep", `Quick, test_lwt_sleep;
]

let () =
  Alcotest.run "Lwt Bridge" [
    "Basic Interop", basic_tests;
    "Bidirectional", bidirectional_tests;
    "Async Behavior", async_tests;
  ]

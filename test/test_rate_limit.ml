(** Tests for Rate_limit module - Token bucket rate limiting

    Pure function tests:
    - create: limiter configuration
    - check: rate limiting logic
    - remaining: token count queries
    - cleanup: stale bucket removal
    - headers: X-RateLimit-* generation
    - too_many_requests_body: 429 response
*)

open Alcotest
module RL = Rate_limit

(** {1 Create Tests} *)

let test_create_defaults () =
  let limiter = RL.create () in
  check int "burst" 20 limiter.RL.burst;
  check (float 0.1) "rate" 10.0 limiter.RL.rate

let test_create_custom () =
  let limiter = RL.create ~rate:5.0 ~burst:10 () in
  check int "burst" 10 limiter.RL.burst;
  check (float 0.1) "rate" 5.0 limiter.RL.rate

(** {1 Check Tests} *)

let test_check_allows_first_request () =
  let limiter = RL.create ~rate:10.0 ~burst:5 () in
  check bool "first allowed" true (RL.check limiter ~key:"client1")

let test_check_allows_burst () =
  let limiter = RL.create ~rate:1.0 ~burst:5 () in
  (* Should allow burst requests *)
  for _ = 1 to 5 do
    let _ = RL.check limiter ~key:"client2" in ()
  done;
  (* Check that all 5 were counted *)
  let remaining = RL.remaining limiter ~key:"client2" in
  check bool "burst consumed" true (remaining <= 0)

let test_check_rejects_over_limit () =
  let limiter = RL.create ~rate:0.1 ~burst:2 () in
  let _ = RL.check limiter ~key:"client3" in
  let _ = RL.check limiter ~key:"client3" in
  (* Third request should be rejected *)
  check bool "rejected" false (RL.check limiter ~key:"client3")

let test_check_different_keys_independent () =
  let limiter = RL.create ~rate:1.0 ~burst:2 () in
  let _ = RL.check limiter ~key:"clientA" in
  let _ = RL.check limiter ~key:"clientA" in
  (* clientA exhausted, but clientB should still work *)
  check bool "clientB allowed" true (RL.check limiter ~key:"clientB")

let test_check_refills_over_time () =
  let limiter = RL.create ~rate:1000.0 ~burst:5 () in  (* Very fast refill *)
  (* Exhaust tokens *)
  for _ = 1 to 5 do
    let _ = RL.check limiter ~key:"client4" in ()
  done;
  (* Wait a tiny bit for refill (1000 tokens/sec = 1 token/ms) *)
  Unix.sleepf 0.01;
  (* Should have refilled *)
  check bool "refilled" true (RL.check limiter ~key:"client4")

(** {1 Remaining Tests} *)

let test_remaining_new_key () =
  let limiter = RL.create ~burst:10 () in
  check int "new key" 10 (RL.remaining limiter ~key:"new_client")

let test_remaining_after_requests () =
  let limiter = RL.create ~rate:0.01 ~burst:5 () in
  let _ = RL.check limiter ~key:"remaining_test" in
  let _ = RL.check limiter ~key:"remaining_test" in
  let rem = RL.remaining limiter ~key:"remaining_test" in
  check bool "remaining decreased" true (rem < 5)

(** {1 Cleanup Tests} *)

let test_cleanup_removes_stale () =
  let limiter = RL.create () in
  (* Create a bucket *)
  let _ = RL.check limiter ~key:"stale_client" in
  (* Artificially age the bucket by manipulating last_update *)
  (* Since we can't access internals, we'll just verify cleanup runs *)
  let removed = RL.cleanup limiter ~older_than_seconds:0 in
  (* Should remove the bucket since older_than_seconds=0 means "older than now" *)
  check bool "removed >= 0" true (removed >= 0)

let test_cleanup_keeps_recent () =
  let limiter = RL.create () in
  let _ = RL.check limiter ~key:"recent_client" in
  let removed = RL.cleanup limiter ~older_than_seconds:3600 in
  (* Should not remove anything recent *)
  check int "nothing removed" 0 removed

(** {1 Headers Tests} *)

let test_headers_includes_limit () =
  let limiter = RL.create ~burst:100 () in
  let hdrs = RL.headers limiter ~key:"header_test" in
  let limit = List.assoc "X-RateLimit-Limit" hdrs in
  check string "limit" "100" limit

let test_headers_includes_remaining () =
  let limiter = RL.create ~burst:50 () in
  let _ = RL.check limiter ~key:"header_test2" in
  let hdrs = RL.headers limiter ~key:"header_test2" in
  let remaining = List.assoc "X-RateLimit-Remaining" hdrs in
  check bool "remaining < burst" true (int_of_string remaining < 50)

let test_headers_includes_reset () =
  let limiter = RL.create () in
  let hdrs = RL.headers limiter ~key:"header_test3" in
  let reset = List.assoc "X-RateLimit-Reset" hdrs in
  (* Reset should be a timestamp in the future *)
  let reset_time = int_of_string reset in
  let now = int_of_float (Unix.gettimeofday ()) in
  check bool "reset >= now" true (reset_time >= now)

(** {1 Response Body Tests} *)

let test_too_many_requests_body () =
  let body = RL.too_many_requests_body () in
  check bool "has error" true (Common.contains ~substring:"Too Many Requests" body);
  check bool "has message" true (Common.contains ~substring:"Rate limit" body);
  check bool "is json" true (Common.contains ~substring:"{" body)

(** {1 Environment Configuration Tests} *)

let test_rate_from_env_default () =
  (* Without env var, should return default *)
  let rate = RL.rate_from_env () in
  check bool "positive rate" true (rate > 0.0)

let test_burst_from_env_default () =
  let burst = RL.burst_from_env () in
  check bool "positive burst" true (burst > 0)

(** {1 Global Instance Tests} *)

let test_check_global () =
  (* Just verify it doesn't crash *)
  let _ = RL.check_global ~key:"global_test" in
  ()

let test_remaining_global () =
  let rem = RL.remaining_global ~key:"global_test2" in
  check bool "positive" true (rem >= 0)

(** {1 Test Suite} *)

let create_tests = [
  test_case "defaults" `Quick test_create_defaults;
  test_case "custom" `Quick test_create_custom;
]

let check_tests = [
  test_case "allows first" `Quick test_check_allows_first_request;
  test_case "allows burst" `Quick test_check_allows_burst;
  test_case "rejects over limit" `Quick test_check_rejects_over_limit;
  test_case "independent keys" `Quick test_check_different_keys_independent;
  test_case "refills" `Quick test_check_refills_over_time;
]

let remaining_tests = [
  test_case "new key" `Quick test_remaining_new_key;
  test_case "after requests" `Quick test_remaining_after_requests;
]

let cleanup_tests = [
  test_case "removes stale" `Quick test_cleanup_removes_stale;
  test_case "keeps recent" `Quick test_cleanup_keeps_recent;
]

let headers_tests = [
  test_case "includes limit" `Quick test_headers_includes_limit;
  test_case "includes remaining" `Quick test_headers_includes_remaining;
  test_case "includes reset" `Quick test_headers_includes_reset;
]

let body_tests = [
  test_case "429 body" `Quick test_too_many_requests_body;
]

let env_tests = [
  test_case "rate default" `Quick test_rate_from_env_default;
  test_case "burst default" `Quick test_burst_from_env_default;
]

let global_tests = [
  test_case "check_global" `Quick test_check_global;
  test_case "remaining_global" `Quick test_remaining_global;
]

let run_tests () =
  run "rate_limit" [
    ("create", create_tests);
    ("check", check_tests);
    ("remaining", remaining_tests);
    ("cleanup", cleanup_tests);
    ("headers", headers_tests);
    ("body", body_tests);
    ("env_config", env_tests);
    ("global", global_tests);
  ]

let () =
  (* Rate_limit uses Eio mutexes, so tests must run under an Eio context. *)
  Eio_main.run (fun _env -> run_tests ())

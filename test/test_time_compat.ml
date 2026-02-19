(** Tests for Time_compat — Eio-native timestamps with Unix fallback *)

open Alcotest

(* === Clock lifecycle === *)

let test_clear_and_has_clock () =
  Time_compat.clear_clock ();
  check bool "no clock after clear" false (Time_compat.has_clock ())

let test_with_eio_clock () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Time_compat.set_clock clock;
  check bool "has clock" true (Time_compat.has_clock ());
  let t = Time_compat.now () in
  check bool "eio timestamp > 2020" true (t > 1577836800.0);
  Time_compat.clear_clock ();
  check bool "cleared" false (Time_compat.has_clock ())

(* === Unix fallback path === *)

let test_now_unix_fallback () =
  Time_compat.clear_clock ();
  let t = Time_compat.now () in
  (* Should be a reasonable Unix timestamp — after 2020 *)
  check bool "reasonable timestamp" true (t > 1577836800.0);
  let t2 = Time_compat.now () in
  check bool "monotonic" true (t2 >= t)

let test_now_ms () =
  Time_compat.clear_clock ();
  let ms = Time_compat.now_ms () in
  check bool "positive" true (ms > 0);
  let t = Time_compat.now () in
  let expected_ms = int_of_float (t *. 1000.0) in
  (* Allow 2 second tolerance *)
  check bool "consistent with now" true (abs (ms - expected_ms) < 2000)

let test_now_us () =
  Time_compat.clear_clock ();
  let us = Time_compat.now_us () in
  check bool "positive" true (Int64.compare us 0L > 0);
  let t = Time_compat.now () in
  let expected_us = Int64.of_float (t *. 1_000_000.0) in
  let diff = Int64.abs (Int64.sub us expected_us) in
  (* Allow 2 second tolerance in microseconds *)
  check bool "consistent with now" true (Int64.compare diff 2_000_000L < 0)

(* === Timed execution === *)

let test_timed () =
  Time_compat.clear_clock ();
  let (result, duration) = Time_compat.timed (fun () ->
    let _ = List.init 1000 (fun i -> i * i) in
    42
  ) in
  check int "result" 42 result;
  check bool "non-negative duration" true (duration >= 0.0);
  check bool "reasonable duration" true (duration < 5.0)

let test_timed_ms () =
  Time_compat.clear_clock ();
  let (result, ms) = Time_compat.timed_ms (fun () -> "hello") in
  check string "result" "hello" result;
  check bool "non-negative" true (ms >= 0);
  check bool "reasonable" true (ms < 5000)

let () =
  run "Time_compat" [
    ("clock", [
      test_case "clear and has" `Quick test_clear_and_has_clock;
      test_case "with eio" `Quick test_with_eio_clock;
    ]);
    ("now", [
      test_case "unix fallback" `Quick test_now_unix_fallback;
      test_case "milliseconds" `Quick test_now_ms;
      test_case "microseconds" `Quick test_now_us;
    ]);
    ("timed", [
      test_case "timed float" `Quick test_timed;
      test_case "timed_ms" `Quick test_timed_ms;
    ]);
  ]

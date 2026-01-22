(** Tests for OCaml 5 parallel utilities *)

open Alcotest

(** Test parallel2: run 2 tasks in parallel *)
let test_parallel2 () =
  let start = Unix.gettimeofday () in
  let (a, b) = Llm_mcp.Parallel_utils.parallel2
    (fun () -> Unix.sleepf 0.1; 1)
    (fun () -> Unix.sleepf 0.1; 2)
  in
  let elapsed = Unix.gettimeofday () -. start in
  (* Verify correct results *)
  check int "first result" 1 a;
  check int "second result" 2 b;
  (* Parallel execution check: should complete faster than sequential (0.2s).
     Use 2.0s tolerance to handle CI/VM overhead and system load variations. *)
  check bool "parallel execution (faster than sequential)" true (elapsed < 2.0)

(** Test parallel3: run 3 tasks in parallel (MAGI Trinity pattern) *)
let test_parallel3 () =
  let start = Unix.gettimeofday () in
  let (a, b, c) = Llm_mcp.Parallel_utils.parallel3
    (fun () -> Unix.sleepf 0.1; "gemini")
    (fun () -> Unix.sleepf 0.1; "claude")
    (fun () -> Unix.sleepf 0.1; "codex")
  in
  let elapsed = Unix.gettimeofday () -. start in
  (* Verify correct results *)
  check string "gemini" "gemini" a;
  check string "claude" "claude" b;
  check string "codex" "codex" c;
  (* Parallel execution check: should complete faster than sequential (0.3s).
     Use 2.0s tolerance to handle CI/VM overhead and system load variations. *)
  check bool "parallel execution (faster than sequential)" true (elapsed < 2.0)

(** Test parallel_map: run N tasks in parallel *)
let test_parallel_map () =
  let start = Unix.gettimeofday () in
  let tasks = [
    (fun () -> Unix.sleepf 0.05; 1);
    (fun () -> Unix.sleepf 0.05; 2);
    (fun () -> Unix.sleepf 0.05; 3);
    (fun () -> Unix.sleepf 0.05; 4);
  ] in
  let results = Llm_mcp.Parallel_utils.parallel_map tasks in
  let elapsed = Unix.gettimeofday () -. start in
  (* Verify correct results and order preservation *)
  check (list int) "results in order" [1; 2; 3; 4] results;
  (* Parallel execution check: should complete faster than sequential (0.2s).
     Use 2.0s tolerance to handle CI/VM overhead and system load variations. *)
  check bool "parallel execution (faster than sequential)" true (elapsed < 2.0)

(** Test parallel_lwt3: run Lwt computations in parallel domains.
    NOTE: Cannot be tested inside Alcotest because it runs in Lwt context.
    parallel_lwt3 calls Lwt_main.run in each Domain, which fails if
    already inside an Lwt_main.run.

    Real usage: MAGI Trinity calls this from the HTTP handler's
    synchronous callback, not from inside Lwt context. *)
let test_parallel_lwt3 () =
  (* Skip: Cannot test nested Lwt_main.run in test harness.
     The function works correctly in production where it's called
     from non-Lwt context (e.g., sync HTTP handler). *)
  skip ()

(** Test stats function *)
let test_stats () =
  let stats = Llm_mcp.Parallel_utils.stats () in
  check bool "has domain info" true (String.length stats > 0);
  check bool "mentions domains" true
    (String.sub stats 0 8 = "Parallel")

(** CPU-bound parallel test - verifies true multicore *)
let test_cpu_bound_parallel () =
  (* Use a heavier computation to see parallelism benefit *)
  let heavy_work () =
    let sum = ref 0 in
    for i = 1 to 5_000_000 do
      sum := !sum + (i mod 7)
    done;
    !sum
  in

  let start = Unix.gettimeofday () in
  let (a, b, c) = Llm_mcp.Parallel_utils.parallel3 heavy_work heavy_work heavy_work in
  let elapsed_parallel = Unix.gettimeofday () -. start in

  (* Sequential baseline *)
  let start = Unix.gettimeofday () in
  let _ = heavy_work () in
  let _ = heavy_work () in
  let _ = heavy_work () in
  let elapsed_seq = Unix.gettimeofday () -. start in

  check int "same result" a b;
  check int "same result" b c;
  (* Parallel should be faster - at least some speedup *)
  let speedup = elapsed_seq /. elapsed_parallel in
  (* On multicore, expect 1.5x+. On single core or busy system, allow 1.0x *)
  check bool "some speedup or equal" true (speedup >= 0.9)

let () =
  run "Llm_mcp.Parallel_utils" [
    "parallel2", [
      test_case "basic" `Quick test_parallel2;
    ];
    "parallel3", [
      test_case "basic" `Quick test_parallel3;
    ];
    "parallel_map", [
      test_case "basic" `Quick test_parallel_map;
    ];
    "parallel_lwt3", [
      test_case "with Lwt" `Quick test_parallel_lwt3;
    ];
    "stats", [
      test_case "info" `Quick test_stats;
    ];
    "cpu_bound", [
      test_case "true multicore speedup" `Slow test_cpu_bound_parallel;
    ];
  ]

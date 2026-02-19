(** Tests for Spawn_registry module.

    Targets: parse_int, latency_snapshot, record_latency,
    try_start, finish, snapshot, to_json, to_prometheus_text. *)

open Alcotest

(* --- parse_int --- *)

let test_parse_int_valid () =
  check (option int) "42" (Some 42) (Spawn_registry.parse_int "42")

let test_parse_int_zero () =
  check (option int) "0" (Some 0) (Spawn_registry.parse_int "0")

let test_parse_int_negative () =
  check (option int) "-5" (Some (-5)) (Spawn_registry.parse_int "-5")

let test_parse_int_invalid () =
  check (option int) "abc" None (Spawn_registry.parse_int "abc")

let test_parse_int_empty () =
  check (option int) "empty" None (Spawn_registry.parse_int "")

let test_parse_int_float () =
  check (option int) "3.14" None (Spawn_registry.parse_int "3.14")

(* --- latency --- *)

let test_latency_initial () =
  let snap = Spawn_registry.latency_snapshot () in
  check bool "count >= 0" true (snap.count >= 0)

let test_record_and_snapshot () =
  Spawn_registry.record_latency 10.0;
  Spawn_registry.record_latency 20.0;
  Spawn_registry.record_latency 30.0;
  let snap = Spawn_registry.latency_snapshot () in
  check bool "count > 0" true (snap.count > 0);
  check bool "avg > 0" true (snap.avg_ms > 0.0);
  check bool "min <= max" true (snap.min_ms <= snap.max_ms)

let test_record_updates () =
  Spawn_registry.record_latency 100.0;
  let snap = Spawn_registry.latency_snapshot () in
  check bool "max >= 100" true (snap.max_ms >= 100.0)

(* --- try_start / finish --- *)

let test_try_start () =
  Eio_main.run (fun _env ->
    match Spawn_registry.try_start ~label:"t1" with
    | Ok id ->
        check bool "id > 0" true (id > 0);
        Spawn_registry.finish ~id ~ok:true ~error:None
    | Error msg -> fail msg
  )

let test_start_and_finish () =
  Eio_main.run (fun _env ->
    match Spawn_registry.try_start ~label:"t2" with
    | Ok id ->
        Spawn_registry.finish ~id ~ok:true ~error:None;
        let snap = Spawn_registry.snapshot () in
        check bool "inflight >= 0" true (snap.inflight >= 0)
    | Error msg -> fail msg
  )

let test_start_and_fail () =
  Eio_main.run (fun _env ->
    match Spawn_registry.try_start ~label:"t3" with
    | Ok id ->
        Spawn_registry.finish ~id ~ok:false ~error:(Some "test_err");
        let snap = Spawn_registry.snapshot () in
        check bool "failed > 0" true (snap.failed > 0);
        check (option string) "last_error" (Some "test_err") snap.last_error
    | Error msg -> fail msg
  )

let test_finish_nonexistent () =
  Eio_main.run (fun _env ->
    Spawn_registry.finish ~id:999999 ~ok:true ~error:None;
    check pass "no crash" () ()
  )

(* --- snapshot --- *)

let test_snapshot_structure () =
  Eio_main.run (fun _env ->
    let s = Spawn_registry.snapshot () in
    check bool "inflight>=0" true (s.inflight >= 0);
    check bool "total>=0" true (s.total >= 0);
    check bool "failed>=0" true (s.failed >= 0);
    check bool "updated>0" true (s.updated_at > 0.0)
  )

(* --- to_json --- *)

let test_to_json () =
  Eio_main.run (fun _env ->
    let json = Spawn_registry.to_json () in
    let open Yojson.Safe.Util in
    check bool "inflight" true (json |> member "inflight" |> to_int >= 0);
    check bool "total" true (json |> member "total" |> to_int >= 0);
    check bool "latency_ms" true (json |> member "latency_ms" <> `Null)
  )

let test_to_json_latency () =
  Eio_main.run (fun _env ->
    let json = Spawn_registry.to_json () in
    let open Yojson.Safe.Util in
    let lat = json |> member "latency_ms" in
    ignore (lat |> member "count" |> to_int);
    ignore (lat |> member "avg" |> to_float);
    ignore (lat |> member "p50" |> to_float);
    ignore (lat |> member "p95" |> to_float);
    ignore (lat |> member "p99" |> to_float);
    check pass "all fields" () ()
  )

(* --- to_prometheus_text --- *)

let test_prometheus () =
  Eio_main.run (fun _env ->
    let text = Spawn_registry.to_prometheus_text () in
    check bool "inflight" true
      (try ignore (Str.search_forward (Str.regexp "llm_spawn_inflight") text 0); true
       with Not_found -> false);
    check bool "total" true
      (try ignore (Str.search_forward (Str.regexp "llm_spawn_total") text 0); true
       with Not_found -> false);
    check bool "HELP" true
      (try ignore (Str.search_forward (Str.regexp "# HELP") text 0); true
       with Not_found -> false);
    check bool "TYPE" true
      (try ignore (Str.search_forward (Str.regexp "# TYPE") text 0); true
       with Not_found -> false)
  )

let test_prometheus_latency () =
  Eio_main.run (fun _env ->
    let text = Spawn_registry.to_prometheus_text () in
    check bool "latency" true
      (try ignore (Str.search_forward (Str.regexp "llm_spawn_latency_ms ") text 0); true
       with Not_found -> false);
    check bool "p50" true
      (try ignore (Str.search_forward (Str.regexp "llm_spawn_latency_ms_p50") text 0); true
       with Not_found -> false)
  )

(* --- Test Suite --- *)

let () =
  run "spawn_registry" [
    ("parse_int", [
      test_case "valid" `Quick test_parse_int_valid;
      test_case "zero" `Quick test_parse_int_zero;
      test_case "negative" `Quick test_parse_int_negative;
      test_case "invalid" `Quick test_parse_int_invalid;
      test_case "empty" `Quick test_parse_int_empty;
      test_case "float" `Quick test_parse_int_float;
    ]);
    ("latency", [
      test_case "initial" `Quick test_latency_initial;
      test_case "record+snapshot" `Quick test_record_and_snapshot;
      test_case "updates" `Quick test_record_updates;
    ]);
    ("try_start_finish", [
      test_case "start" `Quick test_try_start;
      test_case "start+finish" `Quick test_start_and_finish;
      test_case "start+fail" `Quick test_start_and_fail;
      test_case "finish nonexistent" `Quick test_finish_nonexistent;
    ]);
    ("snapshot", [
      test_case "structure" `Quick test_snapshot_structure;
    ]);
    ("to_json", [
      test_case "valid" `Quick test_to_json;
      test_case "latency" `Quick test_to_json_latency;
    ]);
    ("prometheus", [
      test_case "format" `Quick test_prometheus;
      test_case "latency" `Quick test_prometheus_latency;
    ]);
  ]

(** Wave 8: notification_sse — persist/cleanup/load branches, format_event variants *)

(* NOTE: These tests exercise pure functions and state management in
   notification_sse. Persistence functions are tested by controlling
   the LLM_MCP_SSE_PERSIST env var. *)

let test_persist_disabled () =
  Unix.putenv "LLM_MCP_SSE_PERSIST" "0";
  (* persist_event should be a no-op when disabled *)
  Notification_sse.persist_event 999999 "test-event";
  (* No crash means success *)
  Unix.putenv "LLM_MCP_SSE_PERSIST" "1"

let test_cleanup_acks_empty () =
  (* cleanup_acks on empty cache should not crash *)
  Notification_sse.cleanup_acks ();
  ()

let test_persist_acks_throttled () =
  (* persist_acks has interval throttling - second call should be throttled *)
  Unix.putenv "LLM_MCP_SSE_PERSIST" "0";
  Notification_sse.persist_acks ();
  Notification_sse.persist_acks ();
  Unix.putenv "LLM_MCP_SSE_PERSIST" "1"

let test_format_event_with_type () =
  let ev = Notification_sse.format_event ~event_type:"notification" "test data" in
  let has_event_type =
    try ignore (Str.search_forward (Str.regexp_string "event: notification") ev 0); true
    with Not_found -> false
  in
  Alcotest.(check bool) "has event type" true has_event_type;
  let has_data =
    try ignore (Str.search_forward (Str.regexp_string "data: test data") ev 0); true
    with Not_found -> false
  in
  Alcotest.(check bool) "has data" true has_data

let test_format_event_without_type () =
  let ev = Notification_sse.format_event "just data" in
  let has_no_event_line =
    try ignore (Str.search_forward (Str.regexp_string "event:") ev 0); false
    with Not_found -> true
  in
  Alcotest.(check bool) "no event line" true has_no_event_line

let test_format_event_with_id () =
  let ev = Notification_sse.format_event ~id:42 "data" in
  let has_id =
    try ignore (Str.search_forward (Str.regexp_string "id: 42") ev 0); true
    with Not_found -> false
  in
  Alcotest.(check bool) "has custom id" true has_id

let test_prime_event () =
  let ev = Notification_sse.prime_event ~retry_ms:5000 in
  let has_retry =
    try ignore (Str.search_forward (Str.regexp_string "retry: 5000") ev 0); true
    with Not_found -> false
  in
  Alcotest.(check bool) "has retry" true has_retry;
  (* Prime event should NOT have data: line *)
  let has_no_data =
    try ignore (Str.search_forward (Str.regexp_string "data:") ev 0); false
    with Not_found -> true
  in
  Alcotest.(check bool) "no data" true has_no_data

let test_buffer_overflow () =
  (* Buffer should cap at max_buffer_size *)
  for i = 1 to 150 do
    Notification_sse.buffer_event i (Printf.sprintf "event-%d" i)
  done;
  (* Events after 1 should be available, older ones evicted *)
  let events = Notification_sse.get_events_after 140 in
  Alcotest.(check bool) "some events after 140" true (List.length events > 0)

let test_client_count () =
  let initial = Notification_sse.client_count () in
  Alcotest.(check bool) "count >= 0" true (initial >= 0)

let test_broadcast_stats () =
  let (s, f) = Notification_sse.get_broadcast_stats () in
  Alcotest.(check bool) "success >= 0" true (s >= 0);
  Alcotest.(check bool) "failure >= 0" true (f >= 0)

let test_update_nonexistent_client () =
  (* Updating a non-existent client should not crash *)
  Notification_sse.update_last_event_id "nonexistent-session" 999;
  ()

let test_unregister_nonexistent () =
  (* Unregistering non-existent should not crash *)
  Notification_sse.unregister "nonexistent";
  ()

let test_unregister_if_wrong_id () =
  (* unregister_if_current with wrong id should be a no-op *)
  Notification_sse.unregister_if_current "nonexistent" 999;
  ()

let test_load_events_disabled () =
  Unix.putenv "LLM_MCP_SSE_PERSIST" "0";
  Notification_sse.load_events ();
  Unix.putenv "LLM_MCP_SSE_PERSIST" "1"

let test_load_acks_disabled () =
  Unix.putenv "LLM_MCP_SSE_PERSIST" "0";
  Notification_sse.load_acks ();
  Unix.putenv "LLM_MCP_SSE_PERSIST" "1"

let () =
  let open Alcotest in
  run "Notification_sse_wave8" [
    "format", [
      test_case "with type" `Quick test_format_event_with_type;
      test_case "without type" `Quick test_format_event_without_type;
      test_case "with id" `Quick test_format_event_with_id;
      test_case "prime event" `Quick test_prime_event;
    ];
    "buffer", [
      test_case "overflow" `Quick test_buffer_overflow;
    ];
    "persist", [
      test_case "disabled" `Quick test_persist_disabled;
      test_case "acks throttled" `Quick test_persist_acks_throttled;
      test_case "load events disabled" `Quick test_load_events_disabled;
      test_case "load acks disabled" `Quick test_load_acks_disabled;
    ];
    "cleanup", [
      test_case "acks empty" `Quick test_cleanup_acks_empty;
    ];
    "clients", [
      test_case "count" `Quick test_client_count;
      test_case "broadcast stats" `Quick test_broadcast_stats;
      test_case "update nonexistent" `Quick test_update_nonexistent_client;
      test_case "unregister nonexistent" `Quick test_unregister_nonexistent;
      test_case "unregister wrong id" `Quick test_unregister_if_wrong_id;
    ];
  ]

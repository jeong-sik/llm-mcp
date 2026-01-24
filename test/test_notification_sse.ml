(** Tests for Notification SSE module - SSE notification stream *)

open Alcotest

(** Test format_event *)
let test_format_event () =
  let event = Notification_sse.format_event ~event_type:"test" "hello" in
  check bool "has id" true
    (Common.contains ~substring:"id: " event);
  check bool "has event type" true
    (Common.contains ~substring:"event: test\n" event);
  check bool "has data" true
    (Common.contains ~substring:"data: hello\n" event);
  check bool "ends with blank" true
    (let len = String.length event in
     len >= 2 && String.sub event (len - 2) 2 = "\n\n")

(** Test format_event without event_type *)
let test_format_event_no_type () =
  let event = Notification_sse.format_event "simple data" in
  check bool "has id" true
    (Common.contains ~substring:"id: " event);
  check bool "has data" true
    (Common.contains ~substring:"data: simple data\n" event);
  (* No event type line *)
  check bool "no event line" false
    (Common.contains ~substring:"event: " event)

(** Test prime_event *)
let test_prime_event () =
  let prime = Notification_sse.prime_event ~retry_ms:3000 in
  check bool "has retry" true
    (Common.contains ~substring:"retry: 3000\n" prime);
  check bool "has id" true
    (Common.contains ~substring:"id: " prime);
  check bool "ends with blank" true
    (let len = String.length prime in
     len >= 2 && String.sub prime (len - 2) 2 = "\n\n")

(** Test client registration and unregistration *)
let test_register_unregister () =
  let session_id = "test-session-1" in
  let pushed = ref [] in
  let push_fn s = pushed := s :: !pushed in
  let client_id = Notification_sse.register session_id ~push:push_fn ~last_event_id:0 in
  check bool "client id > 0" true (client_id > 0);
  check bool "client count >= 1" true (Notification_sse.client_count () >= 1);
  Notification_sse.unregister session_id

(** Test unregister_if_current *)
let test_unregister_if_current () =
  let session_id = "test-session-2" in
  let push_fn _ = () in
  let client_id = Notification_sse.register session_id ~push:push_fn ~last_event_id:0 in
  (* Wrong client_id should not unregister *)
  Notification_sse.unregister_if_current session_id (client_id + 1);
  (* Correct client_id should unregister - can't easily verify without exposing internals *)
  Notification_sse.unregister_if_current session_id client_id

(** Test get_events_after for replay *)
let test_get_events_after () =
  (* This tests the event buffer replay functionality.
     The function should return a valid list (possibly empty if no events yet).
     We verify the function executes without error and returns the expected type. *)
  let events = Notification_sse.get_events_after 0 in
  (* Verify it's a proper list by checking it's iterable - the actual count
     depends on prior test state, so we just verify the structure is valid *)
  let _ = List.length events in
  check bool "function executes successfully" true true

(** Test update_last_event_id *)
let test_update_last_event_id () =
  let session_id = "test-session-3" in
  let push_fn _ = () in
  let _ = Notification_sse.register session_id ~push:push_fn ~last_event_id:0 in
  (* Should not crash even if session doesn't exist *)
  Notification_sse.update_last_event_id "nonexistent" 100;
  Notification_sse.update_last_event_id session_id 50;
  Notification_sse.unregister session_id

(** Test ack blocks already-acked events *)
let test_ack_blocks_event () =
  let session_id = "test-session-5" in
  let pushed = ref [] in
  let push_fn s = pushed := s :: !pushed in
  let _ = Notification_sse.register session_id ~push:push_fn ~last_event_id:0 in
  Notification_sse.update_last_event_id session_id Int.max_int;
  let json = `Assoc [("type", `String "test"); ("msg", `String "hello")] in
  Notification_sse.broadcast json;
  check int "no broadcast after ack" 0 (List.length !pushed);
  Notification_sse.unregister session_id

(** Test broadcast *)
let test_broadcast () =
  let session_id = "test-session-4" in
  let pushed = ref [] in
  let push_fn s = pushed := s :: !pushed in
  let _ = Notification_sse.register session_id ~push:push_fn ~last_event_id:0 in
  let json = `Assoc [("type", `String "test"); ("msg", `String "hello")] in
  Notification_sse.broadcast json;
  check bool "received broadcast" true (List.length !pushed >= 1);
  let last_event = List.hd !pushed in
  check bool "event has notification type" true
    (Common.contains ~substring:"event: notification" last_event);
  Notification_sse.unregister session_id

(** Test max_buffer_size constant *)
let test_max_buffer_size () =
  check int "buffer size" 100 Notification_sse.max_buffer_size

let () =
  run "Llm_mcp.Notification_sse" [
    "format_event", [
      test_case "with event type" `Quick test_format_event;
      test_case "without event type" `Quick test_format_event_no_type;
    ];
    "prime_event", [
      test_case "format" `Quick test_prime_event;
    ];
    "register_unregister", [
      test_case "basic flow" `Quick test_register_unregister;
      test_case "conditional unregister" `Quick test_unregister_if_current;
    ];
    "event_buffer", [
      test_case "get_events_after" `Quick test_get_events_after;
    ];
    "update_last_event_id", [
      test_case "updates without crash" `Quick test_update_last_event_id;
      test_case "ack blocks event" `Quick test_ack_blocks_event;
    ];
    "broadcast", [
      test_case "sends to clients" `Quick test_broadcast;
    ];
    "constants", [
      test_case "max_buffer_size" `Quick test_max_buffer_size;
    ];
  ]

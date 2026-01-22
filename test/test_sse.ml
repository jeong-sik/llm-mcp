(** Tests for Server-Sent Events (SSE) module *)

open Alcotest

(** Test stream creation *)
let test_create_stream () =
  let stream = Sse.create_stream () in
  check bool "stream_id starts with s" true
    (stream.stream_id.[0] = 's');
  check int "event counter starts at 0" 0 stream.event_counter

(** Test unique stream IDs *)
let test_unique_stream_ids () =
  let s1 = Sse.create_stream () in
  let s2 = Sse.create_stream () in
  check bool "different stream ids" true (s1.stream_id <> s2.stream_id)

(** Test event ID generation *)
let test_next_event_id () =
  let stream = Sse.create_stream () in
  let id1 = Sse.next_event_id stream in
  let id2 = Sse.next_event_id stream in
  check bool "id1 ends with :1" true (Str.string_match (Str.regexp ".*:1$") id1 0);
  check bool "id2 ends with :2" true (Str.string_match (Str.regexp ".*:2$") id2 0);
  check int "counter incremented" 2 stream.event_counter

(** Test event formatting - simple *)
let test_format_event_simple () =
  let event : Sse.event = {
    id = Some "event-1";
    event_type = None;
    data = "hello";
    retry = None;
  } in
  let formatted = Sse.format_event event in
  check bool "has id line" true
    (Common.contains ~substring:"id: event-1\n" formatted);
  check bool "has data line" true
    (Common.contains ~substring:"data: hello\n" formatted);
  check bool "ends with blank line" true
    (let len = String.length formatted in
     len >= 2 && String.sub formatted (len - 2) 2 = "\n\n")

(** Test event formatting - with event type *)
let test_format_event_with_type () =
  let event : Sse.event = {
    id = Some "event-2";
    event_type = Some "notification";
    data = "test data";
    retry = None;
  } in
  let formatted = Sse.format_event event in
  check bool "has event type" true
    (Common.contains ~substring:"event: notification\n" formatted)

(** Test event formatting - with retry *)
let test_format_event_with_retry () =
  let event : Sse.event = {
    id = None;
    event_type = None;
    data = "retry test";
    retry = Some 5000;
  } in
  let formatted = Sse.format_event event in
  check bool "has retry" true
    (Common.contains ~substring:"retry: 5000\n" formatted)

(** Test multiline data *)
let test_format_event_multiline () =
  let event : Sse.event = {
    id = None;
    event_type = None;
    data = "line1\nline2\nline3";
    retry = None;
  } in
  let formatted = Sse.format_event event in
  check bool "has data: line1" true
    (Common.contains ~substring:"data: line1\n" formatted);
  check bool "has data: line2" true
    (Common.contains ~substring:"data: line2\n" formatted);
  check bool "has data: line3" true
    (Common.contains ~substring:"data: line3\n" formatted)

(** Test prime event *)
let test_prime_event () =
  let stream = Sse.create_stream () in
  let prime = Sse.prime_event stream in
  check bool "has retry" true
    (Common.contains ~substring:"retry: 5000\n" prime);
  check bool "has id" true
    (Common.contains ~substring:"id: " prime);
  check bool "ends with blank" true
    (let len = String.length prime in
     len >= 2 && String.sub prime (len - 2) 2 = "\n\n")

(** Test json_event *)
let test_json_event () =
  let stream = Sse.create_stream () in
  let json = `Assoc [("key", `String "value")] in
  let event = Sse.json_event stream json in
  check bool "has id" true
    (Common.contains ~substring:"id: " event);
  check bool "has json data" true
    (Common.contains ~substring:{|"key":"value"|} event)

(** Test progress_event *)
let test_progress_event () =
  let stream = Sse.create_stream () in
  let event = Sse.progress_event stream ~progress:0.5 ~message:"halfway" in
  check bool "has event type" true
    (Common.contains ~substring:"event: progress\n" event);
  check bool "has progress value" true
    (Common.contains ~substring:"0.5" event);
  check bool "has message" true
    (Common.contains ~substring:"halfway" event)

(** Test content type *)
let test_content_type () =
  check string "SSE content type" "text/event-stream" Sse.content_type

(** Test headers *)
let test_headers () =
  let headers = Sse.headers in
  check bool "has content-type" true
    (List.exists (fun (k, v) ->
      k = "Content-Type" && v = "text/event-stream") headers);
  check bool "has cache-control" true
    (List.exists (fun (k, _) -> k = "Cache-Control") headers);
  check bool "has connection" true
    (List.exists (fun (k, _) -> k = "Connection") headers)

let () =
  run "Llm_mcp.Sse" [
    "create_stream", [
      test_case "initial state" `Quick test_create_stream;
      test_case "unique ids" `Quick test_unique_stream_ids;
    ];
    "next_event_id", [
      test_case "incrementing" `Quick test_next_event_id;
    ];
    "format_event", [
      test_case "simple" `Quick test_format_event_simple;
      test_case "with event type" `Quick test_format_event_with_type;
      test_case "with retry" `Quick test_format_event_with_retry;
      test_case "multiline" `Quick test_format_event_multiline;
    ];
    "prime_event", [
      test_case "format" `Quick test_prime_event;
    ];
    "json_event", [
      test_case "json encoding" `Quick test_json_event;
    ];
    "progress_event", [
      test_case "progress format" `Quick test_progress_event;
    ];
    "constants", [
      test_case "content_type" `Quick test_content_type;
      test_case "headers" `Quick test_headers;
    ];
  ]

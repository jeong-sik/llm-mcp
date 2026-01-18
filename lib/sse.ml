(** Server-Sent Events (SSE) for MCP Streamable HTTP *)

(** SSE Event *)
type event = {
  id : string option;
  event_type : string option;
  data : string;
  retry : int option;
}

(** Stream state for resumability *)
type stream_state = {
  stream_id : string;
  mutable event_counter : int;
}

(** Create new stream state.
    Uses timestamp + random for uniqueness across server restarts.
    OCaml 5.x Random is auto-initialized, but we add timestamp for extra safety. *)
let create_stream () =
  let ts = int_of_float (Unix.gettimeofday () *. 1000.) mod 1000000 in
  let stream_id = Printf.sprintf "s%d-%d" ts (Random.int 10000) in
  { stream_id; event_counter = 0 }

(** Generate event ID for resumability *)
let next_event_id stream =
  stream.event_counter <- stream.event_counter + 1;
  Printf.sprintf "%s:%d" stream.stream_id stream.event_counter

(** Format SSE event according to spec *)
let format_event event =
  let buf = Buffer.create 256 in
  (* Event type (optional) *)
  (match event.event_type with
  | Some t -> Buffer.add_string buf (Printf.sprintf "event: %s\n" t)
  | None -> ());
  (* Event ID (for resumability) *)
  (match event.id with
  | Some id -> Buffer.add_string buf (Printf.sprintf "id: %s\n" id)
  | None -> ());
  (* Retry interval (optional) *)
  (match event.retry with
  | Some ms -> Buffer.add_string buf (Printf.sprintf "retry: %d\n" ms)
  | None -> ());
  (* Data lines (can be multiple for multi-line data) *)
  let lines = String.split_on_char '\n' event.data in
  List.iter (fun line ->
    Buffer.add_string buf (Printf.sprintf "data: %s\n" line)
  ) lines;
  (* Empty line to end event *)
  Buffer.add_string buf "\n";
  Buffer.contents buf

(** Create initial "prime" event per spec: id + retry, no data payload. *)
let prime_event stream =
  let id = next_event_id stream in
  Printf.sprintf "retry: %d\nid: %s\n\n" 5000 id

(** Create JSON-RPC response event *)
let json_event stream json =
  let id = next_event_id stream in
  let data = Yojson.Safe.to_string json in
  format_event { id = Some id; event_type = None; data; retry = None }

(** Create progress event (for long-running operations) *)
let progress_event stream ~progress ~message =
  let id = next_event_id stream in
  let data = Yojson.Safe.to_string (`Assoc [
    ("progress", `Float progress);
    ("message", `String message);
  ]) in
  format_event { id = Some id; event_type = Some "progress"; data; retry = None }

(** SSE content type header *)
let content_type = "text/event-stream"

(** SSE headers *)
let headers = [
  ("Content-Type", content_type);
  ("Cache-Control", "no-cache");
  ("Connection", "keep-alive");
  ("X-Accel-Buffering", "no");  (* Disable nginx buffering *)
]

(** Server-Sent Events (SSE) notification stream for MCP Streamable HTTP.

    This module is used by `GET /mcp` to keep a long-lived SSE connection for
    server→client notifications, with basic resumability via Last-Event-ID. *)

(** Connected SSE client. *)
type client = {
  id : int;
  push : string -> unit;
  mutable last_sent_id : int;
  mutable last_acked_id : int;
}

(** Client registry: session_id -> client. *)
let clients : (string, client) Hashtbl.t = Hashtbl.create 16

(** Monotonic client id to avoid unregister race on reconnect. *)
let client_id_counter = ref 0

(** Global event counter for resumability. *)
let event_counter = ref 0

(** Event buffer for replay (stores (event_id, raw_sse_event)). *)
let max_buffer_size = 100
let event_buffer : (int * string) Queue.t = Queue.create ()

let buffer_event event_id event_str =
  if Queue.length event_buffer >= max_buffer_size then
    ignore (Queue.pop event_buffer);
  Queue.push (event_id, event_str) event_buffer

let get_events_after last_id =
  Queue.fold
    (fun acc (event_id, ev) -> if event_id > last_id then ev :: acc else acc)
    []
    event_buffer
  |> List.rev

let format_event ?id ?event_type data =
  incr event_counter;
  let event_id = match id with Some i -> i | None -> !event_counter in
  let id_line = Printf.sprintf "id: %d\n" event_id in
  let event_line =
    match event_type with
    | Some t -> Printf.sprintf "event: %s\n" t
    | None -> ""
  in
  Printf.sprintf "%s%sdata: %s\n\n" id_line event_line data

(** Priming event recommended by spec: id + retry, no data payload. *)
let prime_event ~retry_ms =
  incr event_counter;
  let event_id = !event_counter in
  Printf.sprintf "retry: %d\nid: %d\n\n" retry_ms event_id

let register session_id ~push ~last_event_id =
  incr client_id_counter;
  let client = { id = !client_id_counter; push; last_sent_id = last_event_id; last_acked_id = last_event_id } in
  Hashtbl.replace clients session_id client;
  client.id

let unregister session_id = Hashtbl.remove clients session_id

let unregister_if_current session_id client_id =
  match Hashtbl.find_opt clients session_id with
  | Some client when client.id = client_id -> Hashtbl.remove clients session_id
  | _ -> ()

let client_count () = Hashtbl.length clients

let update_last_event_id session_id event_id =
  match Hashtbl.find_opt clients session_id with
  | Some client -> client.last_acked_id <- event_id
  | None -> ()

let broadcast json =
  let data = Yojson.Safe.to_string json in
  let current_event_id = !event_counter + 1 in
  let event = format_event ~id:current_event_id ~event_type:"notification" data in
  buffer_event current_event_id event;
  Hashtbl.iter
    (fun _session_id client ->
      if current_event_id > client.last_acked_id then (
        try
          client.push event;
          client.last_sent_id <- current_event_id
        with _ -> ()))
    clients

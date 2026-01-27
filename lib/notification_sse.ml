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

let persist_enabled () =
  match Sys.getenv_opt "LLM_MCP_SSE_PERSIST" with
  | Some "0" | Some "false" | Some "no" -> false
  | _ -> true

let events_path () =
  Common.me_path ["logs"; "llm_mcp_sse_events.jsonl"]

let acks_path () =
  Common.me_path ["logs"; "llm_mcp_sse_acks.json"]

let event_max_bytes () =
  match Sys.getenv_opt "LLM_MCP_SSE_EVENT_MAX_BYTES" with
  | Some v -> (try int_of_string v with _ -> 1024 * 1024)
  | None -> 1024 * 1024

let ack_ttl_sec () =
  match Sys.getenv_opt "LLM_MCP_SSE_ACK_TTL_SEC" with
  | Some v -> (try int_of_string v with _ -> 86400)
  | None -> 86400

let ack_persist_interval_sec () =
  match Sys.getenv_opt "LLM_MCP_SSE_ACK_PERSIST_MIN_SEC" with
  | Some v -> (try float_of_string v with _ -> 1.0)
  | None -> 1.0

let last_ack_persist_ts = ref 0.0

let ack_cache : (string, int * float) Hashtbl.t = Hashtbl.create 64

let ensure_dir_for path =
  Common.ensure_dir (Filename.dirname path)

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

let persist_event event_id event_str =
  if not (persist_enabled ()) then ()
  else
    let path = events_path () in
    ensure_dir_for path;
    let line =
      Yojson.Safe.to_string (`Assoc [
        ("id", `Int event_id);
        ("event", `String event_str);
      ]) ^ "\n"
    in
    let oc =
      open_out_gen [Open_creat; Open_append; Open_wronly] 0o644 path
    in
    output_string oc line;
    close_out_noerr oc;
    (try
       let max_bytes = event_max_bytes () in
       let st = Unix.stat path in
       if st.Unix.st_size > max_bytes then begin
         let lines = Common.read_lines_tail ~max_bytes ~max_lines:max_buffer_size path in
         let oc_trunc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o644 path in
         List.iter (fun l -> output_string oc_trunc (l ^ "\n")) lines;
         close_out_noerr oc_trunc
       end
     with exn ->
       (* Log truncation errors for debugging - non-critical *)
       Printf.eprintf "[SSE] Event log truncation error: %s\n%!" (Printexc.to_string exn))

let cleanup_acks () =
  let ttl = float_of_int (ack_ttl_sec ()) in
  let now = Unix.gettimeofday () in
  Hashtbl.filter_map_inplace
    (fun _sid (last_id, updated_at) ->
      if now -. updated_at > ttl then None else Some (last_id, updated_at))
    ack_cache

let persist_acks () =
  if not (persist_enabled ()) then ()
  else
    let now = Unix.gettimeofday () in
    if now -. !last_ack_persist_ts < ack_persist_interval_sec () then ()
    else begin
      last_ack_persist_ts := now;
      cleanup_acks ();
      let entries =
        Hashtbl.fold (fun sid (last_id, updated_at) acc ->
          `Assoc [
            ("session_id", `String sid);
            ("last_acked_id", `Int last_id);
            ("updated_at", `Float updated_at);
          ] :: acc
        ) ack_cache []
      in
      let json = `Assoc [("acks", `List entries)] in
      let path = acks_path () in
      ensure_dir_for path;
      ignore (Common.write_json path json)
    end

let load_acks () =
  if not (persist_enabled ()) then ()
  else
    let path = acks_path () in
    match Common.read_json_opt path with
    | None -> ()
    | Some json ->
        let open Yojson.Safe.Util in
        let add_entry sid last_id updated_at =
          Hashtbl.replace ack_cache sid (last_id, updated_at)
        in
        (match json |> member "acks" with
         | `List items ->
             List.iter (fun item ->
               match item with
               | `Assoc _ ->
                   let sid = item |> member "session_id" |> to_string_option in
                   let last_id = item |> member "last_acked_id" |> to_int_option in
                   let updated_at = item |> member "updated_at" |> to_float_option in
                   (match sid, last_id, updated_at with
                    | Some s, Some i, Some ts -> add_entry s i ts
                    | _ -> ())
               | _ -> ()) items
         | _ -> ());
        cleanup_acks ()

let load_events () =
  if not (persist_enabled ()) then ()
  else
    let path = events_path () in
    let lines = Common.read_lines_tail ~max_bytes:(event_max_bytes ()) ~max_lines:max_buffer_size path in
    List.iter (fun line ->
      let line = String.trim line in
      if line <> "" then
        try
          let json = Yojson.Safe.from_string line in
          let open Yojson.Safe.Util in
          let id = json |> member "id" |> to_int_option in
          let ev = json |> member "event" |> to_string_option in
          match id, ev with
          | Some i, Some s ->
              buffer_event i s;
              if i > !event_counter then event_counter := i
          | _ -> ()
        with exn ->
          (* Log parse errors but continue loading other events *)
          Printf.eprintf "[SSE] Failed to parse event line: %s (error: %s)\n%!"
            (String.sub line 0 (min 100 (String.length line)))
            (Printexc.to_string exn)
    ) lines

let () =
  load_acks ();
  load_events ()

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
  let persisted_ack =
    match Hashtbl.find_opt ack_cache session_id with
    | Some (last_id, _updated_at) -> last_id
    | None -> last_event_id
  in
  let last_acked_id = max last_event_id persisted_ack in
  let client = { id = !client_id_counter; push; last_sent_id = last_event_id; last_acked_id } in
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
  | Some client ->
      client.last_acked_id <- event_id;
      Hashtbl.replace ack_cache session_id (event_id, Unix.gettimeofday ());
      persist_acks ()
  | None -> ()

(** Track broadcast failures for monitoring *)
let broadcast_failure_count = ref 0
let broadcast_success_count = ref 0

let broadcast json =
  let data = Yojson.Safe.to_string json in
  let current_event_id = !event_counter + 1 in
  let event = format_event ~id:current_event_id ~event_type:"notification" data in
  buffer_event current_event_id event;
  persist_event current_event_id event;
  Hashtbl.iter
    (fun session_id client ->
      if current_event_id > client.last_acked_id then (
        try
          client.push event;
          client.last_sent_id <- current_event_id;
          incr broadcast_success_count
        with exn ->
          (* CRITICAL: Log broadcast failures - messages may be lost *)
          incr broadcast_failure_count;
          Printf.eprintf "[SSE] ⚠️ Broadcast failed to session %s (client_id=%d): %s\n%!"
            session_id client.id (Printexc.to_string exn)))
    clients

let get_broadcast_stats () =
  (!broadcast_success_count, !broadcast_failure_count)

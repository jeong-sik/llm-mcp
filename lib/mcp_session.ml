(** MCP Session Management with protocol defaults *)

(** Default protocol version (current spec) *)
let protocol_version = "2025-11-25"

(** Session state *)
type session = {
  id : string;
  created_at : float;
  mutable last_activity : float;
  mutable initialized : bool;
  mutable negotiated_protocol : string;  (** Protocol version negotiated during initialize *)
}

(** Session store (in-memory for now) *)
let sessions : (string, session) Hashtbl.t = Hashtbl.create 16

(** Generate cryptographically secure session ID *)
let generate_session_id () =
  (* Use /dev/urandom for secure random bytes *)
  let bytes =
    In_channel.with_open_bin "/dev/urandom" (fun ic ->
      let buf = Bytes.create 16 in
      really_input ic buf 0 16;
      buf)
  in
  (* Convert to hex string - visible ASCII only *)
  let hex = Buffer.create 32 in
  Bytes.iter (fun b -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code b))) bytes;
  Buffer.contents hex

(** Validate session ID per MCP spec: visible ASCII only (0x21-0x7E) *)
let is_valid_session_id id =
  String.length id > 0 &&
  String.for_all
    (fun c ->
      let code = Char.code c in
      code >= 0x21 && code <= 0x7E)
    id

(** Create new session *)
let create_session ?id ?(protocol=protocol_version) () =
  let id =
    match id with
    | Some id when is_valid_session_id id -> id
    | Some _ -> generate_session_id ()
    | None -> generate_session_id ()
  in
  let now = Unix.gettimeofday () in
  let session = {
    id;
    created_at = now;
    last_activity = now;
    initialized = false;
    negotiated_protocol = protocol;
  } in
  Hashtbl.replace sessions id session;
  session

(** Update session's negotiated protocol *)
let set_negotiated_protocol session version =
  session.negotiated_protocol <- version

(** Get session by ID *)
let get_session id =
  match Hashtbl.find_opt sessions id with
  | Some session ->
      session.last_activity <- Unix.gettimeofday ();
      Some session
  | None -> None

(** Mark session as initialized *)
let mark_initialized session =
  session.initialized <- true

(** Delete session *)
let delete_session id =
  Hashtbl.remove sessions id

(** Session timeout in seconds (30 minutes) *)
let session_timeout = 1800.0

(** Clean expired sessions *)
let cleanup_expired () =
  let now = Unix.gettimeofday () in
  let expired = Hashtbl.fold (fun id session acc ->
    if now -. session.last_activity > session_timeout then id :: acc
    else acc
  ) sessions [] in
  List.iter delete_session expired

(** Check if session is valid *)
let is_valid_session id =
  match get_session id with
  | Some session ->
      let now = Unix.gettimeofday () in
      now -. session.last_activity <= session_timeout
  | None -> false

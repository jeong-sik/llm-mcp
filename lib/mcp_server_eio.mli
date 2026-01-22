(** MCP Server implementation using Eio for async I/O

    This module provides a multi-tenant MCP server with:
    - Thread-safe session management via Eio.Mutex
    - Bearer token authentication
    - JSON-RPC 2.0 protocol handling
*)

(** {1 Session Management} *)

(** Session data structure - exposed for testing *)
type session = {
  id: string;
  protocol_version: string;
  created_at: float;
  last_accessed: float;
}

(** Thread-safe session storage (abstract) *)
type session_store

(** Create a new empty session store *)
val create_session_store : unit -> session_store

(** Generate a unique session ID *)
val generate_session_id : unit -> string

(** Get session by ID from store *)
val get_session : session_store -> string -> session option

(** Store or update session *)
val put_session : session_store -> session -> unit

(** Update session last accessed time *)
val touch_session : session_store -> string -> unit

(** Remove session from store *)
val remove_session : session_store -> string -> unit

(** Clean up stale sessions (not accessed in 1 hour) *)
val cleanup_stale_sessions : session_store -> unit


(** {1 Protocol Constants} *)

(** Supported MCP protocol versions *)
val supported_protocol_versions : string list

(** {1 Response Helpers} *)

(** Create a JSON-RPC error response *)
val make_error : id:Yojson.Safe.t -> int -> string -> Yojson.Safe.t

(** Health check response (JSON string) *)
val health_response : unit -> string


(** {1 Authentication} *)

(** Check authorization header against LLM_MCP_API_KEY env var.
    Returns [Ok ()] if authorized, [Error message] if not.
    If LLM_MCP_API_KEY is not set, all requests are allowed (dev mode). *)
val auth_middleware : (string * string) list -> (unit, string) result


(** {1 Header Utilities} *)

(** Extract session ID from params or X-Session-Id header *)
val extract_session_id : Yojson.Safe.t option -> (string * string) list -> string option


(** {1 Server} *)

module Http = Http_server_eio

(** Handle a JSON-RPC request (used by tests and local dispatch). *)
val handle_request :
  sw:Eio.Switch.t ->
  proc_mgr:'pm Eio.Process.mgr ->
  clock:'clock Eio.Time.clock ->
  store:session_store ->
  headers:(string * string) list ->
  string ->
  string option * Yojson.Safe.t

(** Run the Eio HTTP server.
    Requires an Eio switch and environment with net, clock, and process_mgr. *)
val run : sw:Eio.Switch.t ->
          env:< net : 'net Eio.Net.t;
                clock : 'clock Eio.Time.clock;
                process_mgr : 'pm Eio.Process.mgr; .. > ->
          ?config:Http.config -> unit -> unit

(** Start server with Eio runtime.
    This is the main entry point that sets up the Eio environment. *)
val start : ?port:int -> unit -> unit

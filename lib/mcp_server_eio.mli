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

(** Run the Eio HTTP server.
    Requires an Eio switch and environment with net, clock, and process_mgr. *)
val run : sw:Eio.Switch.t ->
          env:< net : _ Eio.Net.t; clock : _ Eio.Time.clock; process_mgr : _ Eio.Process.mgr; .. > ->
          ?config:Http.config -> unit -> unit

(** Start server with Eio runtime.
    This is the main entry point that sets up the Eio environment. *)
val start : ?port:int -> unit -> unit

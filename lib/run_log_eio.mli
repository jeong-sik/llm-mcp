(** LLM-MCP run log (JSONL) - Pure Eio Version

    Lightweight observability without logging sensitive data.
    Uses Eio.Mutex and Eio.Path for async-safe file operations.
*)

open Types

(** {1 Path Configuration} *)

val log_path : unit -> string
(** Get the log file path (from env or default) *)

(** {1 Recording} *)

val record :
  fs:_ Eio.Path.t ->
  tool:string ->
  streamed:bool ->
  prompt_chars:int ->
  duration_ms:int ->
  tool_result ->
  unit
(** Record a tool execution to the run log.
    @param fs Eio filesystem capability *)

(** {1 Reading} *)

val read_events : unit -> Yojson.Safe.t list
(** Read all events from the log file *)

val read_recent : since_ts:int -> limit:int -> Yojson.Safe.t list
(** Read recent events since a timestamp, limited to [limit] entries *)

(** {1 Statistics} *)

val stats : since_ts:int -> until_ts:int -> Yojson.Safe.t
(** Compute statistics over events in a time range.
    If [until_ts] is 0, includes all events since [since_ts]. *)

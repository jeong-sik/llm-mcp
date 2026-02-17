(** Streaming JSON parser for LLM responses with incomplete data support. *)

type t

(** [create ()] creates a new streaming JSON parser. *)
val create : unit -> t

(** [feed parser chunk] accumulates text and attempts parsing.

    Returns [Some json] if valid JSON is obtained, [None] otherwise.
*)
val feed : t -> string -> Yojson.Safe.t option

(** [finalize parser] returns best-effort result. *)
val finalize : t -> Yojson.Safe.t

(** [is_field_complete parser field] checks if a field was parsed successfully. *)
val is_field_complete : t -> string -> bool

(** [reset parser] clears parser state for reuse. *)
val reset : t -> unit

(** [parse_streaming_json ?default text] one-shot parsing of potentially incomplete JSON. *)
val parse_streaming_json : ?default:Yojson.Safe.t -> string -> Yojson.Safe.t

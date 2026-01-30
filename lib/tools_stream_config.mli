(** Tools Stream Config - Stream Delta Configuration *)

(** Check if stream delta is enabled *)
val enabled : unit -> bool

(** Set stream delta enabled/disabled at runtime *)
val set : bool -> bool

(** Get current stream delta status *)
val get : unit -> bool

(** Get the source of current configuration: "runtime" or "environment" *)
val source : unit -> string

(** Maximum events to buffer before flushing *)
val max_events : unit -> int

(** Maximum characters per delta chunk *)
val max_chars : unit -> int

(** Generate unique stream ID for a model *)
val generate_id : string -> string

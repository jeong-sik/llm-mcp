(** Checkpoint Store - Persistence for long-running chain executions

    Enables checkpoint/resume for chains that may be interrupted or need
    to continue across sessions. Checkpoints are stored as JSON files
    in ~/.cache/llm-mcp/checkpoints/
*)

(** Token usage tracking *)
type token_usage = Chain_category.token_usage

(** A single checkpoint capturing execution state *)
type checkpoint = {
  run_id: string;           (** Unique identifier for this execution run *)
  chain_id: string;         (** Chain definition identifier *)
  node_id: string;          (** Last completed node ID *)
  outputs: (string * string) list;  (** Node outputs accumulated so far *)
  traces: Chain_types.trace_entry list;  (** Execution traces *)
  timestamp: float;         (** Unix timestamp when checkpoint was created *)
  total_tokens: token_usage option;  (** Accumulated token usage *)
}

(** Checkpoint store configuration *)
type checkpoint_store = {
  base_dir: string;  (** Base directory for checkpoint files *)
}

(** Default checkpoint directory (~/.cache/llm-mcp/checkpoints) *)
val default_base_dir : unit -> string

(** Create a new checkpoint store with optional custom base directory *)
val create : ?base_dir:string -> unit -> checkpoint_store

(** Generate a unique run ID for a new execution *)
val generate_run_id : unit -> string

(** Save a checkpoint to disk (uses standard file I/O) *)
val save : checkpoint_store -> checkpoint -> (unit, string) result

(** Save a checkpoint to disk using Eio file operations *)
val save_eio : fs:_ Eio.Path.t -> checkpoint_store -> checkpoint -> (unit, string) result

(** Load a checkpoint by run_id (uses standard file I/O) *)
val load : checkpoint_store -> run_id:string -> (checkpoint, string) result

(** Load a checkpoint by run_id using Eio file operations *)
val load_eio : fs:_ Eio.Path.t -> checkpoint_store -> run_id:string -> (checkpoint, string) result

(** List all checkpoints for a specific chain (newest first) *)
val list_checkpoints : checkpoint_store -> chain_id:string -> checkpoint list

(** List all checkpoints (newest first) *)
val list_all : checkpoint_store -> checkpoint list

(** Delete a specific checkpoint *)
val delete : checkpoint_store -> run_id:string -> unit

(** Cleanup checkpoints older than max_age_hours, returns count deleted *)
val cleanup_old : checkpoint_store -> max_age_hours:int -> int

(** Create a checkpoint from execution state *)
val make_checkpoint :
  run_id:string ->
  chain_id:string ->
  node_id:string ->
  outputs:(string * string) list ->
  traces:Chain_types.trace_entry list ->
  ?total_tokens:token_usage ->
  unit ->
  checkpoint

(** Convert checkpoint to JSON *)
val checkpoint_to_json : checkpoint -> Yojson.Safe.t

(** Parse checkpoint from JSON *)
val checkpoint_of_json : Yojson.Safe.t -> (checkpoint, string) result

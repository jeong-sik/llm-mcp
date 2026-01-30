(** Tools Stream Config - Stream Delta Configuration

    Runtime configuration for streaming token deltas.
    Mostly pure with one mutable ref for runtime toggle.
*)

(** Runtime toggle for stream delta (can be changed without restart) *)
let stream_delta_override : bool option ref = ref None

(** Check if stream delta is enabled *)
let enabled () =
  match !stream_delta_override with
  | Some v -> v  (* Runtime override takes precedence *)
  | None ->
      match Sys.getenv_opt "LLM_MCP_STREAM_DELTA" with
      | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
      | _ -> false

(** Set stream delta enabled/disabled at runtime *)
let set enabled_val =
  stream_delta_override := Some enabled_val;
  enabled_val

(** Get current stream delta status *)
let get () = enabled ()

(** Get the source of current configuration: "runtime" or "environment" *)
let source () =
  match !stream_delta_override with
  | Some _ -> "runtime"
  | None -> "environment"

(** Maximum events to buffer before flushing *)
let max_events () =
  Safe_parse.env_int ~var:"LLM_MCP_STREAM_DELTA_MAX_EVENTS" ~default:2000

(** Maximum characters per delta chunk *)
let max_chars () =
  Safe_parse.env_int ~var:"LLM_MCP_STREAM_DELTA_MAX_CHARS" ~default:200

(** Generate unique stream ID for a model *)
let generate_id model_name =
  Printf.sprintf "stream-%s-%d" model_name (int_of_float (Unix.gettimeofday () *. 1000.0))

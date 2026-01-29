(** Chain Trace Types - Execution Tracing Types *)

(** Trace event types for execution logging *)
type trace_event =
  | NodeStart of { node_type : string; attempt : int }
  | NodeComplete of { duration_ms : int; success : bool; node_type : string; attempt : int }
  | NodeError of { message : string; error_class : string option; node_type : string; attempt : int }
  | ChainStart of { chain_id : string; mermaid_dsl : string option }
  | ChainComplete of { chain_id : string; success : bool }

(** Internal trace entry for execution *)
type internal_trace = {
  timestamp : float;
  node_id : string;
  event : trace_event;
}

(** Execution phase for node status tracking *)
type exec_phase =
  | Planned
  | Running
  | Completed
  | Failed
  | Skipped

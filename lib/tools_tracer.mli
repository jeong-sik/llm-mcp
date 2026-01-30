(** Tools Tracer - Langfuse Tracing Helpers for LLM Tools *)

(** Extract model name from tool_args for tracing *)
val get_model_name : Types.tool_args -> string

(** Extract input/prompt from tool_args for tracing *)
val get_input : Types.tool_args -> string

(** Get tool name for logging (dot-separated format) *)
val get_tool_name : Types.tool_args -> string

(** Classify error type from tool result *)
val classify_error : Types.tool_result -> string option

(** Check if result was streamed *)
val was_streamed : Types.tool_result -> bool

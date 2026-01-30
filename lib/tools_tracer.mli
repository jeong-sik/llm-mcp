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

(** Get API key from environment, returns empty string if not set *)
val get_api_key : string -> string

(** Check if API key is set, returns Some error_result if missing *)
val require_api_key : env_var:string -> model:string -> extra:(string * string) list -> Types.tool_result option

(** Create success result *)
val success_result : model:string -> extra:(string * string) list -> string -> Types.tool_result

(** Create timeout error result *)
val timeout_result : model:string -> extra:(string * string) list -> int -> Types.tool_result

(** Create process error result *)
val process_error_result : model:string -> extra:(string * string) list -> string -> Types.tool_result

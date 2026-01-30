(** Core types for LLM-MCP - Foundation types used across all domains *)

(** Thinking level for Gemini 3 *)
type thinking_level = Low | High [@@deriving yojson]

val thinking_level_of_string : string -> thinking_level
val string_of_thinking_level : thinking_level -> string

(** Reasoning effort for Codex/GPT-5.2 *)
type reasoning_effort = RLow | RMedium | RHigh | RXhigh [@@deriving yojson]

val reasoning_effort_of_string : string -> reasoning_effort
val string_of_reasoning_effort : reasoning_effort -> string

(** Sandbox policy for Codex *)
type sandbox_policy = ReadOnly | WorkspaceWrite | DangerFullAccess [@@deriving yojson]

val sandbox_policy_of_string : string -> sandbox_policy
val string_of_sandbox_policy : sandbox_policy -> string

(** Output format for Claude CLI *)
type output_format = Text | Json | StreamJson [@@deriving yojson]

val output_format_of_string : string -> output_format
val string_of_output_format : output_format -> string

(** MCP Tool schema *)
type tool_schema = {
  name : string;
  description : string;
  input_schema : Yojson.Safe.t;
}

(** Tool execution result *)
type tool_result = {
  model : string;
  returncode : int;
  response : string;
  extra : (string * string) list;
}

val tool_result_to_yojson : tool_result -> Yojson.Safe.t

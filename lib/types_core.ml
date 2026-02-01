(** Core types for LLM-MCP - Foundation types used across all domains *)

[@@@warning "-32"]

(** Thinking level for Gemini 3 *)
type thinking_level = Low | High [@@deriving yojson]

let thinking_level_of_string = function
  | "low" -> Low
  | "high" | _ -> High

let string_of_thinking_level = function
  | Low -> "low"
  | High -> "high"

(** Reasoning effort for Codex/GPT-5.2 *)
type reasoning_effort = RLow | RMedium | RHigh | RXhigh [@@deriving yojson]

let reasoning_effort_of_string = function
  | "low" -> RLow
  | "medium" -> RMedium
  | "high" -> RHigh
  | "xhigh" | _ -> RXhigh

let string_of_reasoning_effort = function
  | RLow -> "low"
  | RMedium -> "medium"
  | RHigh -> "high"
  | RXhigh -> "xhigh"

(** Sandbox policy for Codex *)
type sandbox_policy = ReadOnly | WorkspaceWrite | DangerFullAccess [@@deriving yojson]

let sandbox_policy_of_string = function
  | "read-only" -> ReadOnly
  | "danger-full-access" -> DangerFullAccess
  | "workspace-write" | _ -> WorkspaceWrite

let string_of_sandbox_policy = function
  | ReadOnly -> "read-only"
  | WorkspaceWrite -> "workspace-write"
  | DangerFullAccess -> "danger-full-access"

(** Output format for Claude CLI *)
type output_format = Text | Json | StreamJson [@@deriving yojson]

let output_format_of_string = function
  | "json" -> Json
  | "stream-json" -> StreamJson
  | "text" | _ -> Text

let string_of_output_format = function
  | Text -> "text"
  | Json -> "json"
  | StreamJson -> "stream-json"

(** Response format for tool output - Standardized on Verbose (JSON) *)
type response_format =
  | Verbose     (* Full JSON, human-readable *)
  | Compact     (* DSL (Deprecated, maps to Verbose) *)
  | Binary      (* msgpack (Deprecated, maps to Verbose) *)
  | Base85      (* base85 (Deprecated, maps to Verbose) *)
  | Compressed  (* zlib (Deprecated, maps to Verbose) *)
  | ZstdDict    (* zstd+dict (Deprecated, maps to Verbose) *)
  | Auto        (* Adaptive (Deprecated, maps to Verbose) *)

let response_format_of_string = function
  | "compact" | "dsl" -> Compact
  | "binary" | "msgpack" -> Binary
  | "base85" | "ascii85" -> Base85
  | "compressed" | "zlib" -> Compressed
  | "zstd" | "zstd-dict" | "dict" -> ZstdDict
  | "auto" | "adaptive" -> Auto
  | "verbose" | "json" | _ -> Verbose

let string_of_response_format = function
  | Verbose -> "verbose"
  | Compact -> "compact"
  | Binary -> "binary"
  | Base85 -> "base85"
  | Compressed -> "compressed"
  | ZstdDict -> "zstd-dict"
  | Auto -> "auto"

(** MCP Tool schema - defined early for use in tool_args *)
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
  extra : (string * string) list;  (* Additional metadata *)
}

let tool_result_to_yojson { model; returncode; response; extra } =
  let base = [
    ("model", `String model);
    ("returncode", `Int returncode);
    ("response", `String response);
  ] in
  let extra_json = List.map (fun (k, v) -> (k, `String v)) extra in
  `Assoc (base @ extra_json)

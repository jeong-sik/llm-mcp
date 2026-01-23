(** Agent Core Types - Shared type definitions for Agent Loop abstraction

    These types are used across all LLM backends (Ollama, Claude, Codex)
    and provide a unified interface for the Agent Loop Functor.

    Note: This file is Lwt-free and can be shared between Lwt and Eio implementations.
*)

(** {1 Message Types} *)

(** Role in conversation *)
type role =
  | User
  | Assistant
  | Tool
  | System
[@@deriving show, eq]

(** Tool call request from LLM *)
type tool_call = {
  id : string;
  name : string;
  arguments : Yojson.Safe.t;
}
[@@deriving show]

(** Conversation message *)
type message = {
  role : role;
  content : string;
  tool_calls : tool_call list option;
  name : string option;  (** For tool response messages *)
}

(** {1 Tool Types} *)

(** Tool parameter schema *)
type param_schema = {
  param_type : string;
  description : string;
  required : bool;
  enum : string list option;
}

(** Tool definition *)
type tool = {
  name : string;
  description : string;
  parameters : (string * param_schema) list;
}

(** Tool execution result *)
type tool_result =
  | ToolSuccess of string
  | ToolError of string

(** {1 Loop Control Types} *)

(** Alias to shared Resilience types *)
type retry_policy = Mcp_resilience.retry_policy = {
  max_attempts: int;
  initial_delay_ms: int;
  max_delay_ms: int;
  backoff_multiplier: float;
  jitter: bool;
}

let default_retry_policy = Mcp_resilience.default_policy

(** Loop configuration *)
type loop_config = {
  max_turns : int;
  max_messages : int;  (** Sliding window size for memory management *)
  retry_policy : retry_policy;
  timeout_ms : int;
}

let default_loop_config = {
  max_turns = 10;
  max_messages = 50;
  retry_policy = default_retry_policy;
  timeout_ms = 60_000;
}

(** Loop result *)
type loop_result =
  | Completed of { response : string; turns_used : int }
  | MaxTurnsReached of { last_response : string; turns_used : int }
  | Error of string
  | TimedOut of { turns_completed : int }
  | CircuitOpen

(** {1 Retry Result Types} *)

type 'a retry_result = 'a Mcp_resilience.retry_result =
  | Ok of 'a
  | Error of string
  | CircuitOpen
  | TimedOut

(** {1 Helper Functions} *)

let role_to_string = function
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"
  | System -> "system"

let string_to_role = function
  | "user" -> User
  | "assistant" -> Assistant
  | "tool" -> Tool
  | "system" -> System
  | _ -> User

let message_to_json msg =
  let base = [
    ("role", `String (role_to_string msg.role));
    ("content", `String msg.content);
  ] in
  let with_name = match msg.name with
    | Some n -> base @ [("name", `String n)]
    | None -> base
  in
  `Assoc with_name

let tool_call_to_json tc =
  `Assoc [
    ("id", `String tc.id);
    ("name", `String tc.name);
    ("arguments", tc.arguments);
  ]

let estimate_tokens_of_message msg =
  let content_tokens = String.length msg.content / 4 in
  let name_tokens = match msg.name with Some n -> String.length n / 4 | None -> 0 in
  content_tokens + name_tokens + 10

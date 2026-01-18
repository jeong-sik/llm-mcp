(** Agent Core Types - Shared type definitions for Agent Loop abstraction

    These types are used across all LLM backends (Ollama, Claude, Codex)
    and provide a unified interface for the Agent Loop Functor.
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

(** Retry policy - matches masc-mcp/lib/retry.ml *)
type retry_policy = {
  max_attempts : int;
  initial_delay_ms : int;
  max_delay_ms : int;
  backoff_multiplier : float;
  jitter : bool;
}

let default_retry_policy = {
  max_attempts = 3;
  initial_delay_ms = 100;
  max_delay_ms = 10000;
  backoff_multiplier = 2.0;
  jitter = true;
}

(** Loop configuration *)
type loop_config = {
  max_turns : int;
  max_messages : int;  (** Sliding window size for memory management *)
  retry_policy : retry_policy;
  timeout_ms : int;
}

let default_loop_config = {
  max_turns = 10;
  max_messages = 50;  (** Keep last 50 messages *)
  retry_policy = default_retry_policy;
  timeout_ms = 60_000;  (** 60 seconds default timeout *)
}

(** Loop result *)
type loop_result =
  | Completed of { response : string; turns_used : int }
  | MaxTurnsReached of { last_response : string; turns_used : int }
  | Error of string
  | TimedOut of { turns_completed : int }
  | CircuitOpen  (** Too many failures, circuit breaker tripped *)

(** {1 Retry Result Types} *)

type 'a retry_result =
  | Success of 'a
  | Exhausted of { attempts : int; last_error : string }
  | RetryTimedOut of { timeout_ms : int }
  | RetryCircuitOpen

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

(** Estimate token count (rough approximation: 4 chars = 1 token) *)
let estimate_tokens_of_message msg =
  let content_tokens = String.length msg.content / 4 in
  let name_tokens = match msg.name with Some n -> String.length n / 4 | None -> 0 in
  content_tokens + name_tokens + 10  (* overhead for role, structure *)

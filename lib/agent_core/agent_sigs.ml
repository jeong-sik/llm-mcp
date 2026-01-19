(** Agent Core Signatures - Module types for Agent Loop abstraction

    Defines the interfaces that LLM backends, tool executors, and state
    managers must implement to work with the Agent Loop Functor.

    Design Philosophy:
    - Separation of concerns: Backend, Tools, State are independent
    - Composable: Mix and match implementations
    - Testable: Easy to mock each component
*)

open Agent_types

(** {1 LLM Backend Interface}

    Abstracts the LLM provider (Ollama, Claude CLI, Codex, etc.)
*)
module type LLM_BACKEND = sig
  (** Backend-specific configuration *)
  type config

  (** Raw response from the LLM *)
  type response

  (** Human-readable name for logging *)
  val name : string

  (** Call the LLM with messages and available tools.

      @param config Backend configuration (URL, model, temperature, etc.)
      @param messages Conversation history
      @param tools Available tools for function calling
      @return Either a response or an error string
  *)
  val call :
    config:config ->
    messages:message list ->
    tools:tool list ->
    (response, string) result Lwt.t

  (** Parse tool calls from the response, if any *)
  val parse_tool_calls : response -> tool_call list option

  (** Extract the text content from the response *)
  val extract_content : response -> string

  (** Check if the response indicates the conversation is complete
      (no more tool calls needed) *)
  val is_final : response -> bool
end

(** {1 Tool Executor Interface}

    Handles execution of tool calls from the LLM.
*)
module type TOOL_EXECUTOR = sig
  (** Execute a tool call.

      @param tool_call The tool call to execute
      @return Either a result or an error string
  *)
  val execute : tool_call -> (tool_result, string) result Lwt.t

  (** Convert a tool call and its result into a message for the conversation *)
  val to_message : tool_call -> tool_result -> message

  (** List available tool names *)
  val available_tools : unit -> string list
end

(** {1 State Manager Interface}

    Manages conversation state with memory efficiency.
*)
module type STATE_MANAGER = sig
  (** Conversation state *)
  type t

  (** Create a new empty state *)
  val create : unit -> t

  (** Add a message to the conversation *)
  val add_message : t -> message -> t

  (** Add multiple messages at once *)
  val add_messages : t -> message list -> t

  (** Get all messages in the conversation *)
  val get_messages : t -> message list

  (** Get the current turn number *)
  val get_turn : t -> int

  (** Increment the turn counter *)
  val increment_turn : t -> t

  (** {2 Memory Management}

      These functions help prevent memory issues in long-running loops.
  *)

  (** Trim messages to keep only the most recent ones.

      @param max_messages Maximum number of messages to keep
      @return New state with trimmed messages

      Note: Always preserves the system message if present.
  *)
  val trim_to_window : t -> max_messages:int -> t

  (** Estimate total token count of all messages *)
  val estimate_tokens : t -> int

  (** Get message count *)
  val message_count : t -> int
end

(** {1 Callback Types}

    For monitoring and logging during loop execution.
*)

(** Called before each turn *)
type on_turn_start = int -> unit Lwt.t

(** Called after each turn with the assistant's response *)
type on_turn_end = int -> message -> unit Lwt.t

(** Called when a tool is executed *)
type on_tool_call = tool_call -> tool_result -> unit Lwt.t

(** Called when an error occurs (for logging/monitoring) *)
type on_error = string -> unit Lwt.t

(** Callback bundle *)
type callbacks = {
  on_turn_start : on_turn_start option;
  on_turn_end : on_turn_end option;
  on_tool_call : on_tool_call option;
  on_error : on_error option;
}

let no_callbacks = {
  on_turn_start = None;
  on_turn_end = None;
  on_tool_call = None;
  on_error = None;
}

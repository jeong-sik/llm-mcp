(** Agent Core Eio Signatures - Module types for Effect-based Agent Loop

    Key difference from Lwt version:
    - No Lwt.t wrapper - direct style programming
    - Uses Eio's effect-based concurrency
    - Same interface semantics, cleaner syntax
*)

open Agent_types

(** {1 LLM Backend Interface - Eio Version}

    Abstracts the LLM provider (Ollama, Claude CLI, Codex, etc.)
    Returns results directly .t wrapped.
*)
module type LLM_BACKEND = sig
  (** Backend-specific configuration *)
  type config

  (** Raw response from the LLM *)
  type response

  (** Human-readable name for logging *)
  val name : string

  (** Call the LLM with messages and available tools.

      Direct style - blocks until complete (Eio handles suspension).
      @param config Backend configuration (URL, model, temperature, etc.)
      @param messages Conversation history
      @param tools Available tools for function calling
      @return Either a response or an error string
  *)
  val call :
    config:config ->
    messages:message list ->
    tools:tool list ->
    (response, string) result

  (** Parse tool calls from the response, if any *)
  val parse_tool_calls : response -> tool_call list option

  (** Extract the text content from the response *)
  val extract_content : response -> string

  (** Check if the response indicates the conversation is complete *)
  val is_final : response -> bool
end

(** {1 Tool Executor Interface - Eio Version} *)
module type TOOL_EXECUTOR = sig
  (** Execute a tool call.

      Direct style - may perform IO via Eio effects.
  *)
  val execute : tool_call -> (tool_result, string) result

  (** Convert a tool call and its result into a message *)
  val to_message : tool_call -> tool_result -> message

  (** List available tool names *)
  val available_tools : unit -> string list
end

(** {1 State Manager Interface}

    Same as Lwt version - no IO involved.
*)
module type STATE_MANAGER = sig
  type t

  val create : unit -> t
  val add_message : t -> message -> t
  val add_messages : t -> message list -> t
  val get_messages : t -> message list
  val get_turn_count : t -> int
  val increment_turn : t -> t
  val apply_sliding_window : t -> max_messages:int -> t
  val estimate_tokens : t -> int
end

(** {1 Callbacks for Loop Events} *)

type callbacks = {
  on_turn_start : int -> unit;
  on_turn_end : int -> string -> unit;
  on_tool_call : tool_call -> unit;
  on_tool_result : tool_call -> tool_result -> unit;
  on_error : string -> unit;
}

let no_callbacks = {
  on_turn_start = (fun _ -> ());
  on_turn_end = (fun _ _ -> ());
  on_tool_call = (fun _ -> ());
  on_tool_result = (fun _ _ -> ());
  on_error = (fun _ -> ());
}

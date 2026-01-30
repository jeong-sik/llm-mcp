(** Tools Ollama Agentic - Pure helpers for Ollama agentic execution *)

(** Ollama API endpoint *)
val base_url : string

(** Conversation message type for agentic loop *)
type agent_message = {
  role : string;
  content : string;
  tool_calls : Ollama_parser.tool_call list option;
  name : string option;
}

(** Convert message to JSON for Ollama API *)
val agent_message_to_json : agent_message -> Yojson.Safe.t

(** Build Ollama chat request *)
val build_chat_request :
  model:string ->
  temperature:float ->
  tools:Yojson.Safe.t list ->
  agent_message list ->
  Yojson.Safe.t

(** Parse chat response to extract content and tool calls *)
val parse_chat_response : string -> (string * Ollama_parser.tool_call list, string) result

(** Create assistant message from model response *)
val make_assistant_message : content:string -> tool_calls:Ollama_parser.tool_call list -> agent_message

(** Create tool result message *)
val make_tool_message : name:string -> content:string -> agent_message

(** Create user message *)
val make_user_message : string -> agent_message

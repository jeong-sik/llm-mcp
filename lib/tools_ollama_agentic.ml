(** Tools Ollama Agentic - Pure helpers for Ollama agentic execution

    Pure functions for Ollama chat API and agentic loop.
    No Eio dependencies.
*)

(** Ollama API endpoint *)
let base_url = "http://127.0.0.1:11434"

(** Conversation message type for agentic loop *)
type agent_message = {
  role : string;
  content : string;
  tool_calls : Ollama_parser.tool_call list option;
  name : string option;
}

(** Convert message to JSON for Ollama API *)
let agent_message_to_json msg =
  let base = [
    ("role", `String msg.role);
    ("content", `String msg.content);
  ] in
  let with_name = match msg.name with
    | Some n -> base @ [("name", `String n)]
    | None -> base
  in
  `Assoc with_name

(** Build Ollama chat request *)
let build_chat_request ~model ~temperature ~tools messages =
  `Assoc [
    ("model", `String model);
    ("messages", `List (List.map agent_message_to_json messages));
    ("stream", `Bool false);
    ("options", `Assoc [
      ("temperature", `Float temperature);
    ]);
    ("tools", `List tools);
  ]

(** Parse chat response to extract content and tool calls *)
let parse_chat_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    let message = json |> member "message" in
    let content = message |> member "content" |> to_string_option |> Option.value ~default:"" in
    let tool_calls =
      match message |> member "tool_calls" with
      | `List calls ->
          List.filter_map (fun call ->
            let name = call |> member "function" |> member "name" |> to_string_option in
            let args = call |> member "function" |> member "arguments" in
            match name with
            | Some n -> Some Ollama_parser.{ name = n; arguments = Yojson.Safe.to_string args }
            | None -> None
          ) calls
      | _ -> []
    in
    Ok (content, tool_calls)
  with exn ->
    Error (Printf.sprintf "Parse error: %s" (Printexc.to_string exn))

(** Create assistant message from model response *)
let make_assistant_message ~content ~tool_calls =
  { role = "assistant"; content; tool_calls = Some tool_calls; name = None }

(** Create tool result message *)
let make_tool_message ~name ~content =
  { role = "tool"; content; tool_calls = None; name = Some name }

(** Create user message *)
let make_user_message content =
  { role = "user"; content; tool_calls = None; name = None }

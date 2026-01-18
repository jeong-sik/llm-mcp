(** Ollama response parsing utilities *)

(** Parse a single streaming chunk from Ollama.
    Returns (token, done) or error. *)
let parse_chunk json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    match json |> member "error" |> to_string_option with
    | Some err -> Error err
    | None ->
        let response = json |> member "response" |> to_string in
        let done_ = json |> member "done" |> to_bool in
        Ok (response, done_)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | _ -> Error (Printf.sprintf "Failed to parse chunk: %s" json_str)

(** Parse a complete Ollama response (non-streaming).
    Returns the response text or error. *)
let parse_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    (* Check for error first *)
    match json |> member "error" |> to_string_option with
    | Some err -> Error err
    | None ->
        let response = json |> member "response" |> to_string in
        Ok response
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | _ -> Error (Printf.sprintf "Failed to parse response: %s" json_str)

(** Structured tool call from Ollama response *)
type tool_call = {
  name : string;
  arguments : string;  (* JSON string of arguments *)
}

(** Parse chat response from /api/chat endpoint.
    Returns the assistant message content or error. *)
let parse_chat_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    match json |> member "error" |> to_string_option with
    | Some err -> Error err
    | None ->
        let message = json |> member "message" in
        let content = message |> member "content" |> to_string_option |> Option.value ~default:"" in
        let tool_calls = message |> member "tool_calls" in
        Ok (content, tool_calls)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | _ -> Error (Printf.sprintf "Failed to parse chat response: %s" json_str)

(** Parse tool_calls JSON array into structured list.
    Ollama format: [{"function": {"name": "...", "arguments": "{...}"}}] *)
let parse_tool_calls (tool_calls_json : Yojson.Safe.t) : tool_call list =
  let open Yojson.Safe.Util in
  match tool_calls_json with
  | `Null -> []
  | `List calls ->
      List.filter_map (fun call_json ->
        try
          let func = call_json |> member "function" in
          let name = func |> member "name" |> to_string in
          let arguments = func |> member "arguments" in
          (* arguments can be string or object *)
          let args_str = match arguments with
            | `String s -> s
            | json -> Yojson.Safe.to_string json
          in
          Some { name; arguments = args_str }
        with _ -> None
      ) calls
  | _ -> []

(** Chat response result type *)
type chat_result =
  | TextResponse of string
  | ToolCalls of tool_call list
  | TextWithTools of string * tool_call list

(** Parse chat response and categorize the result *)
let parse_chat_result json_str : (chat_result, string) result =
  match parse_chat_response json_str with
  | Error e -> Error e
  | Ok (content, tool_calls_json) ->
      let tool_calls = parse_tool_calls tool_calls_json in
      match (content, tool_calls) with
      | ("", []) -> Ok (TextResponse "")
      | (text, []) -> Ok (TextResponse text)
      | ("", calls) -> Ok (ToolCalls calls)
      | (text, calls) -> Ok (TextWithTools (text, calls))

(** Parse streaming chunk from /api/chat endpoint.
    Chat streaming returns: {"message": {"content": "token"}, "done": false}
    Final chunk: {"message": {"content": "", "tool_calls": [...]}, "done": true} *)
let parse_chat_chunk json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    match json |> member "error" |> to_string_option with
    | Some err -> Error err
    | None ->
        let message = json |> member "message" in
        let content = message |> member "content" |> to_string_option |> Option.value ~default:"" in
        let done_ = json |> member "done" |> to_bool in
        let tool_calls = if done_ then parse_tool_calls (message |> member "tool_calls") else [] in
        Ok (content, tool_calls, done_)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | _ -> Error (Printf.sprintf "Failed to parse chat chunk: %s" json_str)

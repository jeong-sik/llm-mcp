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
        let content = message |> member "content" |> to_string in
        let tool_calls = message |> member "tool_calls" in
        Ok (content, tool_calls)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | _ -> Error (Printf.sprintf "Failed to parse chat response: %s" json_str)

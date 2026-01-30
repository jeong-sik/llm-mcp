(** Tools MCP Parse - Pure MCP Response Parsing

    Pure functions for parsing MCP protocol responses.
    Handles both SSE stream format and plain JSON-RPC.
    No Eio dependencies.
*)

(** Parse a JSON-RPC result, extracting text content or raw result *)
let parse_json_result json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    let result = json |> member "result" in
    let error = json |> member "error" in
    if error <> `Null then
      let msg = Safe_parse.json_string ~context:"mcp:error" ~default:"Unknown error" error "message" in
      Some (Printf.sprintf "Error: %s" msg)
    else
      let content = result |> member "content" in
      (match content with
       | `List items ->
           let texts = List.filter_map (fun item ->
             match item |> member "type" |> to_string_option with
             | Some "text" -> item |> member "text" |> to_string_option
             | _ -> None
           ) items in
           Some (String.concat "\n" texts)
       | _ ->
           (match result with
            | `Assoc _ | `List _ ->
                Some (Yojson.Safe.to_string result)
            | _ ->
                let result_str = result |> to_string_option in
                Some (Option.value result_str ~default:json_str)))
  with _ -> None

(** Extract SSE data lines from response body *)
let extract_sse_data_lines body =
  let lines = String.split_on_char '\n' body in
  List.filter_map (fun l ->
    if String.length l > 5 && String.sub l 0 5 = "data:" then
      Some (String.sub l 5 (String.length l - 5) |> String.trim)
    else None
  ) lines

(** Build JSON-RPC tool call request *)
let build_tool_call_request ~tool_name ~arguments : string =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "tools/call");
    ("params", `Assoc [
      ("name", `String tool_name);
      ("arguments", arguments);
    ]);
  ] |> Yojson.Safe.to_string

(** Parse MCP HTTP response (streamable-http SSE or plain JSON-RPC) *)
let parse_http_response (body : string) : string option =
  let data_lines = extract_sse_data_lines body in
  let rec find_last_json = function
    | [] -> None
    | h :: t ->
        (match parse_json_result h with
         | Some v -> Some v
         | None -> find_last_json t)
  in
  match find_last_json (List.rev data_lines) with
  | Some v -> Some v
  | None -> parse_json_result body

(** MCP Client for external tool calls

    Enables llm-mcp to call external MCP servers when Ollama
    requests tools that are not built-in.

    Protocol: JSON-RPC 2.0 over HTTP
    Default endpoints:
    - llm-mcp internal tools: self-execution
    - masc-mcp: http://127.0.0.1:8935
    - Other MCP: configurable via tool arguments
*)

(** Request ID counter *)
let request_id = ref 0

let next_id () =
  incr request_id;
  !request_id

(** Build JSON-RPC 2.0 request *)
let build_jsonrpc_request method_name params =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int (next_id ()));
    ("method", `String method_name);
    ("params", params);
  ]

(** Call MCP server via HTTP POST *)
let call_mcp_server ~url ~method_name ~params =
  let open Lwt.Syntax in
  let open Cohttp in
  let open Cohttp_lwt_unix in

  let uri = Uri.of_string (url ^ "/mcp") in
  let body = build_jsonrpc_request method_name params |> Yojson.Safe.to_string in
  let headers = Header.init_with "Content-Type" "application/json" in

  try%lwt
    let* (resp, body) = Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri in
    let* body_str = Cohttp_lwt.Body.to_string body in

    let status = Response.status resp in
    if Code.is_success (Code.code_of_status status) then begin
      let open Yojson.Safe.Util in
      try
        let json = Yojson.Safe.from_string body_str in
        let result = json |> member "result" in
        let error = json |> member "error" in
        if error <> `Null then
          let msg = try error |> member "message" |> to_string with _ -> "Unknown error" in
          Lwt.return (Error msg)
        else
          Lwt.return (Ok result)
      with e ->
        Lwt.return (Error (Printf.sprintf "Parse error: %s" (Printexc.to_string e)))
    end else
      Lwt.return (Error (Printf.sprintf "HTTP error %d: %s"
                          (Code.code_of_status status) body_str))
  with e ->
    Lwt.return (Error (Printf.sprintf "Connection error: %s" (Printexc.to_string e)))

(** Call tools/call on an MCP server *)
let call_tool ~url ~tool_name ~arguments =
  let params = `Assoc [
    ("name", `String tool_name);
    ("arguments", arguments);
  ] in
  call_mcp_server ~url ~method_name:"tools/call" ~params

(** List available tools from MCP server *)
let list_tools ~url =
  call_mcp_server ~url ~method_name:"tools/list" ~params:`Null

(** Default MASC-MCP endpoint *)
let masc_mcp_url = "http://127.0.0.1:8935"

(** Call MASC tool *)
let call_masc ~tool_name ~arguments =
  call_tool ~url:masc_mcp_url ~tool_name ~arguments

(** Format tool result for Ollama chat *)
let format_tool_result tool_name result =
  match result with
  | Ok json ->
      `Assoc [
        ("role", `String "tool");
        ("content", `String (Yojson.Safe.to_string json));
        ("name", `String tool_name);
      ]
  | Error msg ->
      `Assoc [
        ("role", `String "tool");
        ("content", `String (Printf.sprintf "Error: %s" msg));
        ("name", `String tool_name);
      ]

(** MCP Protocol Server - Eio Direct-Style Implementation

    Pure Eio implementation for MCP 2025-11-25.
    Uses httpun-eio for HTTP, Lwt_eio bridge for tool execution.

    Key differences from Lwt version:
    - Direct-style (no monadic bind)
    - Structured concurrency with Eio.Switch
    - Fiber-based parallelism
*)

open Printf

module P = Mcp_protocol.Protocol
module R = Mcp_protocol.Resources
module Http = Http_server_eio

(** {1 JSON-RPC Types} *)

(** JSON-RPC 2.0 request *)
type jsonrpc_request = {
  jsonrpc : string;
  id : Yojson.Safe.t option;
  method_ : string;
  params : Yojson.Safe.t option;
}

(** Parse JSON-RPC request from JSON *)
let jsonrpc_request_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let jsonrpc = json |> member "jsonrpc" |> to_string in
    let id_json = json |> member "id" in
    let id = match id_json with `Null -> None | x -> Some x in
    let method_ = json |> member "method" |> to_string in
    let params_json = json |> member "params" in
    let params = match params_json with `Null -> None | x -> Some x in
    Ok { jsonrpc; id; method_; params }
  with exn ->
    Error (Printexc.to_string exn)

(** {1 Session Management} *)

type session = {
  id: string;
  protocol_version: string;
  created_at: float;
}

let generate_session_id () =
  sprintf "eio-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.0))

(** {1 JSON-RPC Response Helpers} *)

let make_response ~id result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ]

let make_error ~id code message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
  ]

(** {1 Server Info} *)

let server_info = {
  P.name = "llm-mcp-eio";
  version = "0.4.0";
}

let capabilities = {
  P.tools = true;
  resources = true;
}

(** {1 Request Handlers} *)

(** Handle initialize request *)
let handle_initialize ~session_opt id params =
  ignore session_opt;
  let protocol_version = match params with
    | Some (`Assoc fields) ->
        (match List.assoc_opt "protocolVersion" fields with
         | Some (`String v) -> v
         | _ -> P.protocol_version)
    | _ -> P.protocol_version
  in
  let session = {
    id = generate_session_id ();
    protocol_version;
    created_at = Unix.gettimeofday ();
  } in
  let result = `Assoc [
    ("protocolVersion", `String session.protocol_version);
    ("serverInfo", P.server_info_to_json server_info);
    ("capabilities", P.capabilities_to_json capabilities);
  ] in
  (session, make_response ~id result)

(** Handle tools/list request *)
let handle_list_tools id =
  let tools = List.map (fun (schema : Types.tool_schema) ->
    P.tool_to_json {
      P.name = schema.name;
      description = schema.description;
      input_schema = schema.input_schema;
    }
  ) Types.all_schemas in
  make_response ~id (`Assoc [("tools", `List tools)])

(** Handle tools/call request - uses Lwt_eio bridge for now *)
let handle_call_tool id params =
  let open Yojson.Safe.Util in
  let name = params |> member "name" |> to_string in
  let arguments = params |> member "arguments" in

  (* Parse arguments based on tool *)
  let args : Types.tool_args = match name with
    | "gemini" -> Tools.parse_gemini_args arguments
    | "claude-cli" -> Tools.parse_claude_args arguments
    | "codex" -> Tools.parse_codex_args arguments
    | "ollama" -> Tools.parse_ollama_args arguments
    | "ollama-list" -> Tools.parse_ollama_list_args arguments
    | _ -> failwith (sprintf "Unknown tool: %s" name)
  in

  (* Execute via Lwt_eio bridge *)
  let result =
    try
      Lwt_eio.Promise.await_lwt (Tools.execute args)
    with exn ->
      { Types.model = "error";
        returncode = 1;
        response = Printexc.to_string exn;
        extra = [];
      }
  in

  (* Format response - returncode 0 = success *)
  let is_error = result.Types.returncode <> 0 in
  let content =
    `Assoc [
      ("type", `String "text");
      ("text", `String result.response);
    ]
  in
  make_response ~id (`Assoc [
    ("content", `List [content]);
    ("isError", `Bool is_error);
  ])

(** Resources (static for now) *)
let resources : P.resource list = [
  { P.uri = "llm-mcp://info";
    name = "Server Info";
    description = "LLM-MCP server information";
    mime_type = "application/json"; }
]

let resource_templates = []

(** Handle resources/read request *)
let handle_read_resource id params =
  let open Yojson.Safe.Util in
  let uri = match params with
    | Some p -> p |> member "uri" |> to_string
    | None -> ""
  in
  match uri with
  | "llm-mcp://info" ->
      let info = `Assoc [
        ("server", `String "llm-mcp-eio");
        ("version", `String "0.4.0");
        ("runtime", `String "Eio");
        ("tools", `Int (List.length Types.all_schemas));
      ] in
      make_response ~id (`Assoc [
        ("contents", `List [
          `Assoc [
            ("uri", `String uri);
            ("mimeType", `String "application/json");
            ("text", `String (Yojson.Safe.to_string info));
          ]
        ]);
      ])
  | _ ->
      make_error ~id (-32602) (sprintf "Unknown resource: %s" uri)

(** {1 Request Dispatch} *)

(** Check if request is valid JSON-RPC 2.0 *)
let is_jsonrpc_v2 json =
  match Yojson.Safe.Util.(json |> member "jsonrpc") with
  | `String "2.0" -> true
  | _ -> false

(** Main request handler - Direct-style, no Lwt *)
let handle_request ~session_opt request_str =
  try
    let json = Yojson.Safe.from_string request_str in

    if not (is_jsonrpc_v2 json) then
      (session_opt, make_error ~id:`Null (-32600) "Invalid Request: jsonrpc must be 2.0")
    else
      match jsonrpc_request_of_yojson json with
      | Error msg ->
          (session_opt, make_error ~id:`Null (-32600) ("Invalid Request: " ^ msg))
      | Ok req ->
          let id = match req.id with Some id -> id | None -> `Null in

          (* Notifications (no id) get no response *)
          if req.id = None then
            (session_opt, `Null)
          else
            match req.method_ with
            | "initialize" ->
                let (session, response) = handle_initialize ~session_opt id req.params in
                (Some session, response)
            | "initialized" ->
                (session_opt, make_response ~id `Null)
            | "tools/list" ->
                (session_opt, handle_list_tools id)
            | "tools/call" ->
                (match req.params with
                 | Some params ->
                     (session_opt, handle_call_tool id params)
                 | None ->
                     (session_opt, make_error ~id (-32602) "Missing params"))
            | "resources/list" ->
                let result = R.list_result resources in
                (session_opt, make_response ~id result)
            | "resources/templates/list" ->
                let result = R.templates_list_result resource_templates in
                (session_opt, make_response ~id result)
            | "resources/read" ->
                (session_opt, handle_read_resource id req.params)
            | method_ ->
                (session_opt, make_error ~id (-32601) ("Method not found: " ^ method_))
  with
  | Yojson.Json_error msg ->
      (session_opt, make_error ~id:`Null (-32700) ("Parse error: " ^ msg))
  | exn ->
      (session_opt, make_error ~id:`Null (-32603) ("Internal error: " ^ Printexc.to_string exn))

(** {1 HTTP Server} *)

(** Handle HTTP request - httpun callback style *)
let handle_http ~session reqd =
  let request = Httpun.Reqd.request reqd in
  let path = Http.Request.path request in
  let meth = Http.Request.meth request in

  match (meth, path) with
  (* Health check *)
  | (`GET, "/health") ->
      let body = Yojson.Safe.to_string (`Assoc [
        ("status", `String "ok");
        ("server", `String "llm-mcp-eio");
        ("transport", `String "http");
        ("runtime", `String "Eio");
      ]) in
      Http.Response.json body reqd

  (* CORS preflight *)
  | (`OPTIONS, _) ->
      Http.Response.cors_preflight reqd

  (* MCP JSON-RPC endpoint *)
  | (`POST, "/mcp" | `POST, "/") ->
      Http.Request.read_body_async reqd (fun body_str ->
        let (_new_session, response) = handle_request ~session_opt:session body_str in
        let response_str = Yojson.Safe.to_string response in
        Http.Response.json response_str reqd
      )

  (* Not found *)
  | _ ->
      Http.Response.not_found reqd

(** {1 Server Entry Points} *)

(** Run the Eio HTTP server *)
let run ~sw ~env ?(config = Http.default_config) () =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, config.port) in
  let socket = Eio.Net.listen net ~sw ~backlog:config.max_connections addr in

  eprintf "[llm-mcp-eio] Starting on http://%s:%d\n%!" config.host config.port;
  eprintf "[llm-mcp-eio] Available tools: %d\n%!" (List.length Types.all_schemas);

  (* Accept loop *)
  let session = ref None in

  (* request_handler: sockaddr -> Gluten.Reqd.t -> unit *)
  let request_handler _client_addr (gluten_reqd : Httpun.Reqd.t Gluten.Reqd.t) =
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    handle_http ~session:!session reqd
  in

  let rec accept_loop () =
    let client_socket, client_addr = Eio.Net.accept ~sw socket in

    Eio.Fiber.fork ~sw (fun () ->
      Httpun_eio.Server.create_connection_handler
        ~sw
        ~request_handler
        ~error_handler:Http.error_handler
        client_addr
        client_socket
    );

    (* Small yield to allow other fibers *)
    Eio.Time.sleep clock 0.0;
    accept_loop ()
  in

  accept_loop ()

(** Start server with Eio runtime *)
let start ?(port = 8932) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = { Http.default_config with port } in
  run ~sw ~env ~config ()

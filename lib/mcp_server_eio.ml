(** MCP Protocol Server - Eio Direct-Style Implementation

    Pure Eio implementation for MCP 2025-11-25.
    Uses httpun-eio for HTTP, Tools_eio for direct-style tool execution.

    Key differences from Lwt version:
    - Direct-style (no monadic bind)
    - Structured concurrency with Eio.Switch
    - Fiber-based parallelism
    - No Lwt_eio bridge needed
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
  last_accessed: float;
}

(** Thread-safe session storage *)
type session_store = {
  sessions: (string, session) Hashtbl.t;
  mutex: Eio.Mutex.t;
}

let create_session_store () = {
  sessions = Hashtbl.create 16;
  mutex = Eio.Mutex.create ();
}

let generate_session_id () =
  sprintf "eio-%d-%d-%s"
    (Unix.getpid ())
    (int_of_float (Unix.gettimeofday () *. 1000.0))
    (String.sub (Digest.to_hex (Digest.string (string_of_float (Random.float 1.0)))) 0 8)

(** Get session by ID from store *)
let get_session store session_id =
  Eio.Mutex.use_ro store.mutex (fun () ->
    Hashtbl.find_opt store.sessions session_id
  )

(** Store or update session *)
let put_session store session =
  Eio.Mutex.use_rw ~protect:true store.mutex (fun () ->
    Hashtbl.replace store.sessions session.id session
  )

(** Update session last accessed time *)
let touch_session store session_id =
  Eio.Mutex.use_rw ~protect:true store.mutex (fun () ->
    match Hashtbl.find_opt store.sessions session_id with
    | Some session ->
        let updated = { session with last_accessed = Unix.gettimeofday () } in
        Hashtbl.replace store.sessions session.id updated
    | None -> ()
  )

(** Remove session from store *)
let remove_session store session_id =
  Eio.Mutex.use_rw ~protect:true store.mutex (fun () ->
    Hashtbl.remove store.sessions session_id
  )

(** Clean up stale sessions (not accessed in 1 hour) *)
let cleanup_stale_sessions store =
  let now = Unix.gettimeofday () in
  let max_age = 3600.0 in (* 1 hour *)
  Eio.Mutex.use_rw ~protect:true store.mutex (fun () ->
    let to_remove = ref [] in
    Hashtbl.iter (fun id session ->
      if now -. session.last_accessed > max_age then
        to_remove := id :: !to_remove
    ) store.sessions;
    List.iter (Hashtbl.remove store.sessions) !to_remove;
    let removed = List.length !to_remove in
    if removed > 0 then
      eprintf "[session-cleanup] Removed %d stale session(s)\n%!" removed
  )

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
  version = "0.2.0";
}

let capabilities = {
  P.tools = true;
  resources = true;
}

(** {1 Request Handlers} *)

(** Extract session ID from request headers or params *)
let extract_session_id params headers =
  (* Try to get session ID from X-Session-Id header *)
  let header_session =
    List.find_map (fun (name, value) ->
      if String.lowercase_ascii name = "x-session-id" then Some value else None
    ) headers
  in
  match header_session with
  | Some sid -> Some sid
  | None ->
      (* Fallback: check params for sessionId field (for clients that send it) *)
      match params with
      | Some (`Assoc fields) ->
          (match List.assoc_opt "sessionId" fields with
           | Some (`String sid) -> Some sid
           | _ -> None)
      | _ -> None

(** Handle initialize request *)
let handle_initialize ~store id params =
  let protocol_version = match params with
    | Some (`Assoc fields) ->
        (match List.assoc_opt "protocolVersion" fields with
         | Some (`String v) -> v
         | _ -> P.protocol_version)
    | _ -> P.protocol_version
  in
  let now = Unix.gettimeofday () in
  let session = {
    id = generate_session_id ();
    protocol_version;
    created_at = now;
    last_accessed = now;
  } in
  (* Store the new session *)
  put_session store session;
  eprintf "[session] Created new session: %s\n%!" session.id;

  let result = `Assoc [
    ("protocolVersion", `String session.protocol_version);
    ("serverInfo", P.server_info_to_json server_info);
    ("capabilities", P.capabilities_to_json capabilities);
    ("sessionId", `String session.id); (* Return session ID to client *)
  ] in
  (Some session.id, make_response ~id result)

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

(** Handle tools/call request - direct Eio execution *)
let handle_call_tool ~sw ~proc_mgr ~clock id params =
  let open Yojson.Safe.Util in
  let name = params |> member "name" |> to_string in
  let arguments = params |> member "arguments" in

  (* Parse arguments based on tool *)
  let args : Types.tool_args = match name with
    | "gemini" -> Tools_eio.parse_gemini_args arguments
    | "claude-cli" -> Tools_eio.parse_claude_args arguments
    | "codex" -> Tools_eio.parse_codex_args arguments
    | "ollama" -> Tools_eio.parse_ollama_args arguments
    | "ollama-list" -> Tools_eio.parse_ollama_list_args arguments
    | "chain.run" -> Tools_eio.parse_chain_run_args arguments
    | "chain.validate" -> Tools_eio.parse_chain_validate_args arguments
    | _ -> failwith (sprintf "Unknown tool: %s" name)
  in

  (* Execute via direct Eio call *)
  let result =
    try
      Tools_eio.execute ~sw ~proc_mgr ~clock args
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
        ("version", `String "0.2.0");
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
let handle_request ~sw ~proc_mgr ~clock ~store ~headers request_str =
  try
    let json = Yojson.Safe.from_string request_str in

    if not (is_jsonrpc_v2 json) then
      (None, make_error ~id:`Null (-32600) "Invalid Request: jsonrpc must be 2.0")
    else
      match jsonrpc_request_of_yojson json with
      | Error msg ->
          (None, make_error ~id:`Null (-32600) ("Invalid Request: " ^ msg))
      | Ok req ->
          let id = match req.id with Some id -> id | None -> `Null in

          (* Extract session ID from headers or params *)
          let session_id_opt = extract_session_id req.params headers in

          (* Touch session if it exists (update last_accessed) *)
          (match session_id_opt with
           | Some sid -> touch_session store sid
           | None -> ());

          (* Notifications (no id) get no response *)
          if req.id = None then
            (None, `Null)
          else
            match req.method_ with
            | "initialize" ->
                handle_initialize ~store id req.params
            | "initialized" ->
                (session_id_opt, make_response ~id `Null)
            | "tools/list" ->
                (session_id_opt, handle_list_tools id)
            | "tools/call" ->
                (match req.params with
                 | Some params ->
                     (* Verify session exists for tools/call *)
                     (match session_id_opt with
                      | Some sid ->
                          (match get_session store sid with
                           | Some _ ->
                               (session_id_opt, handle_call_tool ~sw ~proc_mgr ~clock id params)
                           | None ->
                               eprintf "[session] Session %s not found, rejecting tools/call\n%!" sid;
                               (None, make_error ~id (-32000) "Session not found. Please call initialize first."))
                      | None ->
                          eprintf "[session] No session ID provided for tools/call\n%!";
                          (None, make_error ~id (-32000) "Session required. Please call initialize first."))
                 | None ->
                     (session_id_opt, make_error ~id (-32602) "Missing params"))
            | "resources/list" ->
                let result = R.list_result resources in
                (session_id_opt, make_response ~id result)
            | "resources/templates/list" ->
                let result = R.templates_list_result resource_templates in
                (session_id_opt, make_response ~id result)
            | "resources/read" ->
                (session_id_opt, handle_read_resource id req.params)
            | method_ ->
                (session_id_opt, make_error ~id (-32601) ("Method not found: " ^ method_))
  with
  | Yojson.Json_error msg ->
      (None, make_error ~id:`Null (-32700) ("Parse error: " ^ msg))
  | exn ->
      (None, make_error ~id:`Null (-32603) ("Internal error: " ^ Printexc.to_string exn))

(** {1 Authentication Middleware} *)

(** Extract Bearer token from Authorization header *)
let extract_bearer_token headers =
  let auth_header = List.find_opt (fun (name, _value) ->
    String.lowercase_ascii name = "authorization"
  ) headers in
  match auth_header with
  | None -> None
  | Some (_name, value) ->
      (* Check if it starts with "Bearer " *)
      let prefix = "Bearer " in
      let prefix_len = String.length prefix in
      if String.length value > prefix_len &&
         String.sub value 0 prefix_len = prefix then
        Some (String.sub value prefix_len (String.length value - prefix_len))
      else
        None

(** Check if request is authenticated

    Returns Ok () if:
    - LLM_MCP_API_KEY env var is not set (development mode)
    - Valid Bearer token matches LLM_MCP_API_KEY

    Returns Error msg if:
    - LLM_MCP_API_KEY is set but Authorization header is missing
    - Bearer token doesn't match expected value
*)
let auth_middleware headers =
  match Sys.getenv_opt "LLM_MCP_API_KEY" with
  | None | Some "" ->
      (* Development mode - no auth required (empty string = unset) *)
      Ok ()
  | Some expected_token ->
      (* Production mode - check Bearer token *)
      match extract_bearer_token headers with
      | None ->
          eprintf "[auth] Missing or invalid Authorization header\n%!";
          Error "Unauthorized: Missing or invalid Authorization header"
      | Some token ->
          if String.equal token expected_token then
            Ok ()
          else begin
            eprintf "[auth] Invalid Bearer token provided\n%!";
            Error "Unauthorized: Invalid Bearer token"
          end

(** Send 401 Unauthorized response *)
let send_unauthorized reqd message =
  let headers = Httpun.Headers.of_list [
    ("content-type", "application/json");
    ("access-control-allow-origin", "*");
  ] in
  let body = Yojson.Safe.to_string (`Assoc [
    ("error", `String message);
  ]) in
  let response = Httpun.Response.create ~headers `Unauthorized in
  Httpun.Reqd.respond_with_string reqd response body

(** {1 HTTP Server} *)

(** Extract headers from httpun request *)
let extract_headers request =
  Httpun.Headers.to_list request.Httpun.Request.headers

(** Handle HTTP request - httpun callback style *)
let handle_http ~sw ~proc_mgr ~clock ~store reqd =
  let request = Httpun.Reqd.request reqd in
  let path = Http.Request.path request in
  let meth = Http.Request.meth request in
  let headers = extract_headers request in

  match (meth, path) with
  (* Health check - no auth required *)
  | (`GET, "/health") ->
      let session_count =
        Eio.Mutex.use_ro store.mutex (fun () ->
          Hashtbl.length store.sessions
        )
      in
      let body = Yojson.Safe.to_string (`Assoc [
        ("status", `String "ok");
        ("server", `String "llm-mcp-eio");
        ("transport", `String "http");
        ("runtime", `String "Eio");
        ("sessions", `Int session_count);
      ]) in
      Http.Response.json body reqd

  (* CORS preflight - no auth required *)
  | (`OPTIONS, _) ->
      Http.Response.cors_preflight reqd

  (* All other endpoints require authentication *)
  | _ ->
      match auth_middleware headers with
      | Error msg ->
          send_unauthorized reqd msg

      | Ok () ->
          (* Authentication passed - route to endpoint *)
          match (meth, path) with
          (* Session stats endpoint *)
          | (`GET, "/sessions") ->
              let sessions_json =
                Eio.Mutex.use_ro store.mutex (fun () ->
                  let sessions_list = Hashtbl.fold (fun _id session acc ->
                    let session_json = `Assoc [
                      ("id", `String session.id);
                      ("protocol_version", `String session.protocol_version);
                      ("created_at", `Float session.created_at);
                      ("last_accessed", `Float session.last_accessed);
                      ("age_seconds", `Float (Unix.gettimeofday () -. session.created_at));
                      ("idle_seconds", `Float (Unix.gettimeofday () -. session.last_accessed));
                    ] in
                    session_json :: acc
                  ) store.sessions [] in
                  `List sessions_list
                )
              in
              let body = Yojson.Safe.to_string (`Assoc [
                ("count", `Int (List.length (match sessions_json with `List l -> l | _ -> [])));
                ("sessions", sessions_json);
              ]) in
              Http.Response.json body reqd

          (* MCP JSON-RPC endpoint *)
          | (`POST, "/mcp" | `POST, "/") ->
              Http.Request.read_body_async reqd (fun body_str ->
                let (_session_id_opt, response) = handle_request ~sw ~proc_mgr ~clock ~store ~headers body_str in
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
  let proc_mgr = Eio.Stdenv.process_mgr env in

  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, config.port) in
  let socket = Eio.Net.listen net ~sw ~backlog:config.max_connections addr in

  eprintf "[llm-mcp-eio] Starting on http://%s:%d\n%!" config.host config.port;
  eprintf "[llm-mcp-eio] Available tools: %d\n%!" (List.length Types.all_schemas);
  eprintf "[llm-mcp-eio] Multi-tenancy: Enabled (per-connection sessions)\n%!";
  (match Sys.getenv_opt "LLM_MCP_API_KEY" with
   | None -> eprintf "[llm-mcp-eio] Authentication: Disabled (development mode)\n%!"
   | Some _ -> eprintf "[llm-mcp-eio] Authentication: Enabled (Bearer token required)\n%!");

  (* Create session store for multi-tenancy *)
  let store = create_session_store () in

  (* Periodic cleanup fiber for stale sessions *)
  Eio.Fiber.fork ~sw (fun () ->
    let rec cleanup_loop () =
      Eio.Time.sleep clock 300.0; (* Clean up every 5 minutes *)
      cleanup_stale_sessions store;
      cleanup_loop ()
    in
    cleanup_loop ()
  );

  (* request_handler: sockaddr -> Gluten.Reqd.t -> unit *)
  let request_handler _client_addr (gluten_reqd : Httpun.Reqd.t Gluten.Reqd.t) =
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    handle_http ~sw ~proc_mgr ~clock ~store reqd
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

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

(** Supported MCP protocol versions *)
let supported_protocol_versions = [
  "2024-11-05";
  "2025-03-26";
  "2025-11-25";
]

(** Health check response *)
let health_response () =
  Yojson.Safe.to_string (`Assoc [
    ("status", `String "ok");
    ("server", `String "llm-mcp");
    ("version", `String Version.version);
    ("transport", `String "http");
    ("language", `String "ocaml");
  ])

(** {1 Server Info} *)

let server_info = {
  P.name = "llm-mcp-eio";
  version = Version.version;
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
      match String.lowercase_ascii name with
      | "x-session-id" | "mcp-session-id" -> Some value
      | _ -> None
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
           | _ ->
               (match List.assoc_opt "session_id" fields with
                | Some (`String sid) -> Some sid
                | _ -> None))
      | _ -> None

(** Extract last event id from headers or params (for SSE ack). *)
let extract_last_event_id params headers =
  let parse_int_opt value =
    try Some (int_of_string value) with _ -> None
  in
  let header_last_event =
    List.find_map (fun (name, value) ->
      if String.lowercase_ascii name = "last-event-id" then
        parse_int_opt value
      else
        None
    ) headers
  in
  match header_last_event with
  | Some id -> Some id
  | None ->
      match params with
      | Some (`Assoc fields) ->
          (match List.assoc_opt "last_event_id" fields with
           | Some (`Int id) -> Some id
           | Some (`String value) -> parse_int_opt value
           | _ ->
               (match List.assoc_opt "lastEventId" fields with
                | Some (`Int id) -> Some id
                | Some (`String value) -> parse_int_opt value
                | _ -> None))
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
    ("instructions", `String "LLM-MCP provides multi-LLM access (MAGI Trinity). \
      Tools: gemini (CASPER/strategy), claude-cli (BALTHASAR/values), codex (MELCHIOR/code), ollama (local). \
      Use response_format='compact' for 64% token savings. \
      For MAGI consensus, call 2+ LLMs and compare results. \
      Chain Engine: Use chain.run for multi-LLM workflows (Mermaid DSL supported).");
  ] in
  (Some session.id, make_response ~id result)

(** Handle tools/list request *)
let handle_list_tools id =
  let tools = List.map (fun (schema : Types.tool_schema) ->
    `Assoc [
      ("name", `String schema.name);
      ("description", `String schema.description);
      ("inputSchema", schema.input_schema);
    ]
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
    | "ollama_list" -> Tools_eio.parse_ollama_list_args arguments
    | "glm" -> Tools_eio.parse_glm_args arguments
    | "chain.run" -> Tools_eio.parse_chain_run_args arguments
    | "chain.validate" -> Tools_eio.parse_chain_validate_args arguments
    | "chain.list" -> Types.ChainList
    | "chain.to_mermaid" -> Tools_eio.parse_chain_to_mermaid_args arguments
    | "chain.visualize" -> Tools_eio.parse_chain_visualize_args arguments
    | "chain.convert" -> Tools_eio.parse_chain_convert_args arguments
    | "chain.orchestrate" -> Tools_eio.parse_chain_orchestrate_args arguments
    | "chain.checkpoints" -> Tools_eio.parse_chain_checkpoints_args arguments
    | "chain.resume" -> Tools_eio.parse_chain_resume_args arguments
    | "prompt.register" -> Tools_eio.parse_prompt_register_args arguments
    | "prompt.list" -> Types.PromptList
    | "prompt.get" -> Tools_eio.parse_prompt_get_args arguments
    | "gh_pr_diff" -> Tools_eio.parse_gh_pr_diff_args arguments
    | "slack_post" -> Tools_eio.parse_slack_post_args arguments
    | "set_stream_delta" -> Tools_eio.parse_set_stream_delta_args arguments
    | "get_stream_delta" -> Tools_eio.parse_get_stream_delta_args arguments
    | _ -> failwith (sprintf "Unknown tool: %s" name)
  in

  (* Execute via direct Eio call with Langfuse tracing *)
  let result =
    try
      Tools_eio.execute_with_tracing ~sw ~proc_mgr ~clock args
    with exn ->
      let bt = Printexc.get_backtrace () in
      { Types.model = "error";
        returncode = 1;
        response = Printf.sprintf "%s\nBacktrace:\n%s" (Printexc.to_string exn) bt;
        extra = [];
      }
  in

  (* Format response - returncode 0 = success *)
  let is_error = result.Types.returncode <> 0 in

  (* Build response text - include extra fields like tool_calls, thinking *)
  let response_text =
    let extra_json = match result.Types.extra with
      | [] -> ""
      | extras ->
          let json_str = Yojson.Safe.to_string (`Assoc (List.map (fun (k, v) -> (k, `String v)) extras)) in
          if result.response = "" then json_str
          else "\n\n[Extra]\n" ^ json_str
    in
    result.response ^ extra_json
  in

  let content =
    `Assoc [
      ("type", `String "text");
      ("text", `String response_text);
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
    description = Some "LLM-MCP server information";
    mime_type = Some "application/json"; }
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
        ("version", `String Version.version);
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

          (* Notification ack can arrive without id; update SSE state then return Null. *)
          if String.equal req.method_ "notifications/ack" then begin
            let last_event_id_opt = extract_last_event_id req.params headers in
            (match session_id_opt, last_event_id_opt with
             | Some sid, Some last_id ->
                 Notification_sse.update_last_event_id sid last_id
             | _ -> ());
            (session_id_opt, `Null)
          end else if req.id = None then
            (* Notifications (no id) get no response *)
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
                     (* Get or create session for tools/call *)
                     let effective_session_id = match session_id_opt with
                       | Some sid ->
                           (match get_session store sid with
                            | Some _ ->
                                (* Session exists, use it *)
                                Some sid
                            | None ->
                                (* Session expired/not found, auto-create new one *)
                                let now = Unix.gettimeofday () in
                                let new_session = {
                                  id = generate_session_id ();
                                  protocol_version = P.protocol_version;
                                  created_at = now;
                                  last_accessed = now;
                                } in
                                put_session store new_session;
                                eprintf "[session] Old session %s not found, auto-created: %s\n%!" sid new_session.id;
                                Some new_session.id)
                       | None ->
                           (* No session ID provided, allow for compatibility *)
                           eprintf "[session] No session ID, allowing tools/call anyway\n%!";
                           None
                     in
                     (effective_session_id, handle_call_tool ~sw ~proc_mgr ~clock id params)
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
  let starts_with ~prefix s =
    let p = String.length prefix in
    String.length s >= p && String.sub s 0 p = prefix
  in

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

  (* Readiness probe - for Kubernetes *)
  | (`GET, "/ready") ->
      (* Check if server can handle requests *)
      let ready = true in (* TODO: Add deeper checks if needed *)
      if ready then
        Http.Response.json ~status:`OK
          {|{"status":"ready"}|} reqd
      else
        Http.Response.json ~status:`Service_unavailable
          {|{"status":"not_ready"}|} reqd

  (* Prometheus metrics endpoint *)
  | (`GET, "/metrics") ->
      let session_count =
        Eio.Mutex.use_ro store.mutex (fun () ->
          Hashtbl.length store.sessions
        )
      in
      Metrics.set_active_sessions session_count;
      let body = Metrics.to_prometheus_text () in
      let headers = Httpun.Headers.of_list [
        ("content-type", "text/plain; version=0.0.4; charset=utf-8");
        ("content-length", string_of_int (String.length body));
      ] in
      let response = Httpun.Response.create ~headers `OK in
      Httpun.Reqd.respond_with_string reqd response body

  (* Chain viewer UI + run history - no auth required *)
  | (`GET, "/chain/view") ->
      Http.Response.html Chain_view_page.html reqd

  | (`GET, "/chain/runs") ->
      let body = Chain_run_store.list_runs_json () |> Yojson.Safe.to_string in
      Http.Response.json body reqd

  | (`GET, path) when starts_with ~prefix:"/chain/runs/" path ->
      let prefix_len = String.length "/chain/runs/" in
      let run_id = String.sub path prefix_len (String.length path - prefix_len) in
      (match Chain_run_store.get_run_json ~run_id with
       | Some json ->
           Http.Response.json (Yojson.Safe.to_string (`Assoc [("run", json)])) reqd
       | None ->
           Http.Response.not_found reqd)

  (* CORS preflight - no auth required *)
  | (`OPTIONS, _) ->
      Http.Response.cors_preflight reqd

  (* All other endpoints require authentication *)
  | _ ->
      match auth_middleware headers with
      | Error msg ->
          send_unauthorized reqd msg

      | Ok () ->
          (* Rate limiting - use client IP or session ID as key *)
          let rate_key =
            extract_session_id None headers
            |> Option.value ~default:"anonymous"
          in
          if not (Rate_limit.check_global ~key:rate_key) then begin
            Metrics.record_error ~error_type:"rate_limit" ();
            let headers = Httpun.Headers.of_list [
              ("content-type", "application/json");
              ("retry-after", "1");
            ] in
            let response = Httpun.Response.create ~headers `Too_many_requests in
            Httpun.Reqd.respond_with_string reqd response
              (Rate_limit.too_many_requests_body ())
          end
          else
          (* Authentication and rate limit passed - route to endpoint *)
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

          (* SSE streaming endpoint for MCP Streamable HTTP *)
          | (`GET, "/sse") ->
              let session_id =
                extract_session_id None headers
                |> Option.value ~default:(generate_session_id ())
              in
              let protocol_version = List.assoc_opt "mcp-protocol-version" headers
                |> Option.value ~default:"2024-11-05" in
              let last_event_id =
                extract_last_event_id None headers |> Option.value ~default:0
              in
              Http.Response.sse_stream ~session_id ~protocol_version reqd ~on_write:(fun body ->
                let push_event ev =
                  try
                    Httpun.Body.Writer.write_string body ev;
                    Httpun.Body.Writer.flush body ignore
                  with exn ->
                    Printf.eprintf "[MCP] SSE push failed to session %s: %s\n%!"
                      session_id (Printexc.to_string exn)
                in
                let notif_client_id =
                  Notification_sse.register session_id ~push:push_event ~last_event_id
                in
                (* Replay missed events if client provides Last-Event-Id *)
                if last_event_id > 0 then
                  Notification_sse.get_events_after last_event_id
                  |> List.iter push_event;
                (* Register for shutdown notifications *)
                let client_id = Http.register_sse_client body in
                (* Send initial connection event *)
                Http.send_sse_event body ~event:"open" ~data:(sprintf
                  {|{"session_id":"%s","protocol_version":"%s","message":"SSE connection established"}|}
                  session_id protocol_version);
                (* Keep connection alive with heartbeats until closed *)
                let rec heartbeat_loop n =
                  Eio.Time.sleep clock 30.0;  (* 30s heartbeat interval *)
                  if Http.sse_client_count () > 0 then begin
                    Http.send_sse_event body ~event:"heartbeat"
                      ~data:(sprintf {|{"timestamp":%f}|} (Unix.gettimeofday ()));
                    heartbeat_loop (n + 1)
                  end
                in
                (* Run heartbeat in background fiber *)
                Eio.Fiber.fork ~sw (fun () ->
                  try heartbeat_loop 1 with exn ->
                    Printf.eprintf "[MCP] Heartbeat loop error for session %s: %s\n%!"
                      session_id (Printexc.to_string exn));
                (* The connection stays open - httpun manages the lifecycle *)
                (* Unregister when connection closes (on_eof from client) *)
                Eio.Fiber.fork ~sw (fun () ->
                  try
                    Eio.Time.sleep clock 3600.0;  (* 1h max connection time *)
                    (try Http.unregister_sse_client client_id with exn ->
                       Printf.eprintf "[MCP] SSE unregister error for session %s: %s\n%!"
                         session_id (Printexc.to_string exn));
                    (try Notification_sse.unregister_if_current session_id notif_client_id with exn ->
                       Printf.eprintf "[MCP] Notification unregister error for session %s: %s\n%!"
                         session_id (Printexc.to_string exn));
                    (try Httpun.Body.Writer.close body with exn ->
                       Printf.eprintf "[MCP] Body close error for session %s: %s\n%!"
                         session_id (Printexc.to_string exn))
                  with exn ->
                    Printf.eprintf "[MCP] SSE cleanup fiber error for session %s: %s\n%!"
                      session_id (Printexc.to_string exn)
                )
              )

          (* Chain stats endpoint for monitoring *)
          | (`GET, "/chain/stats") ->
              let stats = Chain_stats.compute () in
              let body = Yojson.Safe.to_string (Chain_stats.to_json stats) in
              Http.Response.json body reqd

          (* MCP JSON-RPC endpoint *)
          | (`POST, "/mcp" | `POST, "/") ->
              Http.Request.read_body_async reqd (fun body_str ->
                let (_session_id_opt, response) = handle_request ~sw ~proc_mgr ~clock ~store ~headers body_str in
                (* Notifications return `Null - respond with 202 Accepted per MCP Streamable HTTP spec *)
                match response with
                | `Null -> Http.Response.accepted reqd
                | _ ->
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

  (* Periodic cleanup fiber for stale sessions - prevents memory leaks *)
  Eio.Fiber.fork ~sw (fun () ->
    let rec cleanup_loop () =
      Eio.Time.sleep clock 60.0; (* Clean up every 1 minute *)
      (* Cleanup must never crash the server switch. *)
      (try
         cleanup_stale_sessions store;
         Mcp_session.cleanup_expired ()  (* Also clean global session store *)
       with exn ->
         eprintf "[cleanup] loop error: %s\n%!" (Printexc.to_string exn));
      cleanup_loop ()
    in
    try cleanup_loop () with exn ->
      eprintf "[cleanup] fatal loop error: %s\n%!" (Printexc.to_string exn)
  );

  (* request_handler: sockaddr -> Gluten.Reqd.t -> unit *)
  let request_handler _client_addr (gluten_reqd : Httpun.Reqd.t Gluten.Reqd.t) =
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    handle_http ~sw ~proc_mgr ~clock ~store reqd
  in

  let initial_backoff_s = 0.05 in
  let max_backoff_s = 1.0 in
  let backoff_s = ref initial_backoff_s in
  let reset_backoff () = backoff_s := initial_backoff_s in
  let bump_backoff () = backoff_s := min max_backoff_s (!backoff_s *. 2.0) in
  let rec accept_loop () =
    (try
       let client_socket, client_addr = Eio.Net.accept ~sw socket in
       reset_backoff ();
       Eio.Fiber.fork ~sw (fun () ->
         try
           Httpun_eio.Server.create_connection_handler
             ~sw
             ~request_handler
             ~error_handler:Http.error_handler
             client_addr
             client_socket
         with exn ->
           eprintf "[accept] connection handler error: %s\n%!"
             (Printexc.to_string exn));

       (* Small yield to allow other fibers *)
       Eio.Time.sleep clock 0.0
     with exn ->
       let delay = !backoff_s in
       eprintf "[accept] error: %s (backoff %.2fs)\n%!"
         (Printexc.to_string exn) delay;
       Eio.Time.sleep clock delay;
       bump_backoff ());
    accept_loop ()
  in

  accept_loop ()

(** Start server with Eio runtime *)
let start ?(port = 8932) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = { Http.default_config with port } in
  run ~sw ~env ~config ()

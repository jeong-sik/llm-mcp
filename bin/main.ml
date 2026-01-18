(** LLM-MCP CLI Entrypoint

    Usage:
      llm-mcp              # Run in HTTP mode (default)
      llm-mcp --stdio      # Run in stdio mode (legacy)
      llm-mcp --port 8932  # Run in HTTP mode (custom port)
      llm-mcp --agent ollama --prompt "Hello"  # CLI agent mode

    MCP Protocol: 2025-11-25 with legacy compatibility
*)

open Cmdliner
open Llm_mcp

(** Run in stdio mode *)
let run_stdio () =
  Lwt_main.run (Mcp_server.run_stdio ())

(** MCP Protocol Versions (legacy + current) *)
let mcp_protocol_versions = Mcp_server.supported_protocol_versions
let mcp_protocol_version_default = Mcp_session.protocol_version

(** Debug logging toggle (LLM_MCP_DEBUG=1 or MCP_DEBUG=1) *)
let debug_enabled =
  match Sys.getenv_opt "LLM_MCP_DEBUG" with
  | Some "1" -> true
  | _ ->
      (match Sys.getenv_opt "MCP_DEBUG" with
      | Some "1" -> true
      | _ -> false)

let log_debug fmt =
  if debug_enabled then Printf.eprintf fmt else Printf.ifprintf stderr fmt

(** Keep debug logs single-line even if headers contain CR/LF. *)
let sanitize_header value =
  String.map (fun c -> if c = '\n' || c = '\r' then ' ' else c) value

let header_value req name =
  match Cohttp.Header.get (Cohttp.Request.headers req) name with
  | Some v -> sanitize_header v
  | None -> "-"

let log_request_debug ~label req session_id protocol_version =
  if debug_enabled then
    let path = Uri.path (Cohttp.Request.uri req) in
    let session_header = header_value req "mcp-session-id" in
    let protocol_header = header_value req "mcp-protocol-version" in
    log_debug
      "LLM_MCP_DEBUG: %s %s session=%s header-session=%s protocol=%s header-protocol=%s accept=%s content-type=%s last-event-id=%s user-agent=%s origin=%s\n%!"
      label
      path
      session_id
      session_header
      protocol_version
      protocol_header
      (header_value req "accept")
      (header_value req "content-type")
      (header_value req "last-event-id")
      (header_value req "user-agent")
      (header_value req "origin")

(** Safe string prefix check (OCaml 4.13+ has String.starts_with) *)
let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

(** Allowed localhost origins for DNS rebinding protection *)
let allowed_origins = [
  "http://localhost";
  "https://localhost";
  "http://127.0.0.1";
  "https://127.0.0.1";
]

(** Validate Origin header for DNS rebinding protection (spec MUST for browsers) *)
let validate_origin req =
  match Cohttp.Header.get (Cohttp.Request.headers req) "origin" with
  | None -> true
  | Some origin -> List.exists (fun prefix -> starts_with ~prefix origin) allowed_origins

(** Get Mcp-Session-Id from headers *)
let get_session_id_header req =
  Cohttp.Header.get (Cohttp.Request.headers req) "mcp-session-id"

(** Get Last-Event-ID from headers for resumability *)
let get_last_event_id req =
  match Cohttp.Header.get (Cohttp.Request.headers req) "last-event-id" with
  | Some id -> (try Some (int_of_string id) with _ -> None)
  | None -> None

(** Get MCP-Protocol-Version from headers *)
let get_protocol_version req =
  match Cohttp.Header.get (Cohttp.Request.headers req) "mcp-protocol-version" with
  | Some v -> v
  | None -> mcp_protocol_version_default

let get_protocol_version_opt req =
  Cohttp.Header.get (Cohttp.Request.headers req) "mcp-protocol-version"

(** Validate MCP-Protocol-Version *)
let is_valid_protocol_version version =
  List.mem version mcp_protocol_versions

(** Check if client accepts SSE *)
let wants_sse req =
  let headers = Cohttp.Request.headers req in
  Mcp_protocol.Http_negotiation.accepts_sse_header
    (Cohttp.Header.get headers "accept")

(** Check if client accepts MCP Streamable HTTP (JSON + SSE) *)
let accepts_streamable_mcp req =
  let headers = Cohttp.Request.headers req in
  Mcp_protocol.Http_negotiation.accepts_streamable_mcp
    (Cohttp.Header.get headers "accept")

(** Common MCP headers for all responses *)
let mcp_headers session_id protocol_version =
  [
    ("Mcp-Session-Id", session_id);
    ("Mcp-Protocol-Version", protocol_version);
  ]

(** CORS headers for cross-origin requests *)
let cors_headers () =
  [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Expose-Headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ]

(** CORS preflight response headers *)
let cors_preflight_headers () =
  [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS");
    ("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-ID, Accept, Origin");
    ("Access-Control-Expose-Headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ]

(** JSON headers without Mcp-Session-Id (e.g. early validation failures) *)
let json_headers_no_session protocol_version =
  [("Content-Type", "application/json"); ("Mcp-Protocol-Version", protocol_version)]
  @ cors_headers ()

(** JSON-RPC error response *)
let json_rpc_error code message =
  Yojson.Safe.to_string (Mcp_server.make_error ~id:`Null code message)

let jsonrpc_error_code = function
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | Some (`Assoc err_fields) -> (
          match List.assoc_opt "code" err_fields with
          | Some (`Int code) -> Some code
          | _ -> None)
      | _ -> None)
  | _ -> None

let is_http_error_response = function
  | Mcp_server.JsonResponse (`Assoc fields) ->
      let id_is_null =
        match List.assoc_opt "id" fields with
        | Some `Null -> true
        | _ -> false
      in
      let code = jsonrpc_error_code (`Assoc fields) in
      id_is_null && (code = Some (-32700) || code = Some (-32600))
  | _ -> false

(** Get or create a server-owned session for Streamable HTTP *)
let get_or_create_session ~protocol_version req =
  Mcp_session.cleanup_expired ();
  match get_session_id_header req with
  | Some session_id when Mcp_session.is_valid_session_id session_id -> (
      match Mcp_session.get_session session_id with
      | Some session -> session
      | None -> Mcp_session.create_session ~id:session_id ~protocol:protocol_version ())
  | Some _ -> Mcp_session.create_session ~protocol:protocol_version ()
  | None -> Mcp_session.create_session ~protocol:protocol_version ()

(** SSE retry interval in milliseconds *)
let sse_retry_ms = 5000

(** Run in HTTP mode using cohttp-lwt-unix *)
let run_http ~host ~port () =
  let open Lwt.Syntax in
  let open Cohttp_lwt_unix in

  Printf.eprintf "🐫 llm-mcp (OCaml) MCP 2025-11-25 server\n";
  Printf.eprintf "   HTTP: http://%s:%d\n" host port;
  Printf.eprintf "   MCP:  http://%s:%d/mcp\n" host port;
  Printf.eprintf "   SSE:  GET /mcp (notifications), POST /mcp (optional)\n%!";

  let handle_get_mcp req =
    let open Lwt.Syntax in
    let header_protocol_version = get_protocol_version_opt req in
    let protocol_version = get_protocol_version req in
    let session = get_or_create_session ~protocol_version req in
    let protocol_version_for_headers =
      match header_protocol_version with
      | Some v -> v
      | None -> session.Mcp_session.negotiated_protocol
    in
    log_request_debug ~label:"GET /mcp" req session.id protocol_version_for_headers;
    let last_event_id = get_last_event_id req in
    let stream, push_to_stream = Lwt_stream.create () in

    (* Register SSE client (one per session; replaces older connection) *)
    let client_id =
      Notification_sse.register session.id
        ~push:(fun s -> push_to_stream (Some s))
        ~last_event_id:(Option.value last_event_id ~default:0)
    in

    (* Replay missed events if Last-Event-ID provided (spec MUST) *)
    let _replayed_count =
      match last_event_id with
      | Some last_id ->
          let missed_events = Notification_sse.get_events_after last_id in
          List.iter (fun ev -> push_to_stream (Some ev)) missed_events;
          List.length missed_events
      | None -> 0
    in

    (* Send SSE priming event first (spec SHOULD) *)
    push_to_stream (Some (Notification_sse.prime_event ~retry_ms:sse_retry_ms));

    (* Cleanup on disconnect *)
    Lwt.async (fun () ->
      let* () = Lwt_stream.closed stream in
      Notification_sse.unregister_if_current session.id client_id;
      Lwt.return_unit
    );

    let headers =
      Cohttp.Header.of_list
        (Sse.headers @ mcp_headers session.id protocol_version_for_headers @ cors_headers ())
    in
    let body = Cohttp_lwt.Body.of_stream stream in
    Server.respond ~status:`OK ~headers ~body ()
  in

  let callback _conn req body =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let meth = Cohttp.Request.meth req in
    let protocol_version = get_protocol_version req in
    let is_mcp_path = path = "/mcp" || path = "/" in

    if is_mcp_path && not (validate_origin req) then
      let headers = Cohttp.Header.of_list (json_headers_no_session mcp_protocol_version_default) in
      Server.respond_string ~status:`Forbidden ~headers
        ~body:(json_rpc_error (-32600) "Invalid origin") ()
    else if is_mcp_path && meth <> `OPTIONS &&
            not (is_valid_protocol_version protocol_version) then
      let headers = Cohttp.Header.of_list (json_headers_no_session mcp_protocol_version_default) in
      Server.respond_string ~status:`Bad_request ~headers
        ~body:(json_rpc_error (-32600) "Unsupported protocol version") ()
    else match (meth, path) with
    | `GET, "/health" ->
        let body = Mcp_server.health_response () in
        let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
        Server.respond_string ~status:`OK ~headers ~body ()

    | `GET, "/mcp" ->
        handle_get_mcp req

    | `POST, "/" | `POST, "/mcp" ->
        if not (accepts_streamable_mcp req) then
          let session_hint =
            match get_session_id_header req with
            | Some id -> id
            | None -> "-"
          in
          log_request_debug ~label:"POST /mcp invalid-accept" req session_hint protocol_version;
          let headers = Cohttp.Header.of_list (json_headers_no_session mcp_protocol_version_default) in
          Server.respond_string ~status:`Bad_request ~headers
            ~body:(json_rpc_error (-32600) "Invalid Accept header: must include application/json and text/event-stream") ()
        else
        let header_protocol_version = get_protocol_version_opt req in
        let session = get_or_create_session ~protocol_version req in
        let protocol_version_for_headers =
          match header_protocol_version with
          | Some v -> v
          | None -> session.Mcp_session.negotiated_protocol
        in
        log_request_debug ~label:"POST /mcp" req session.id protocol_version_for_headers;
        let wants_stream = wants_sse req in
        let* body_str = Cohttp_lwt.Body.to_string body in
        let* (_session_opt, response) =
          Mcp_server.handle_request ~session_opt:(Some session) ~wants_stream body_str
        in
        let base_headers = mcp_headers session.id protocol_version_for_headers @ cors_headers () in
        (match response with
        | Mcp_server.NoResponse ->
            let headers = Cohttp.Header.of_list base_headers in
            Server.respond_string ~status:`Accepted ~headers ~body:"" ()
        | Mcp_server.JsonResponse json when is_http_error_response response ->
            let headers =
              Cohttp.Header.of_list (("Content-Type", "application/json") :: base_headers)
            in
            Server.respond_string ~status:`Bad_request ~headers
              ~body:(Yojson.Safe.to_string json) ()
        | Mcp_server.JsonResponse json ->
            if wants_stream then
              let stream = Sse.create_stream () in
              let headers =
                Cohttp.Header.of_list (base_headers @ Sse.headers)
              in
              let body = (Sse.prime_event stream) ^ (Sse.json_event stream json) in
              Server.respond_string ~status:`OK ~headers ~body ()
            else
              let body = Yojson.Safe.to_string json in
              let headers = Cohttp.Header.of_list (("Content-Type", "application/json") :: base_headers) in
              Server.respond_string ~status:`OK ~headers ~body ()
        | Mcp_server.SseStream (stream, generator) ->
            (* SSE streaming response - wait for full result then send *)
            let headers =
              Cohttp.Header.of_list (base_headers @ Sse.headers)
            in
            (* Send prime event + result *)
            let* result_event = generator () in
            let body = (Sse.prime_event stream) ^ result_event in
            Server.respond_string ~status:`OK ~headers ~body ()
        | Mcp_server.SseTokenStream (stream, event_stream, starter) ->
            (* Real-time SSE streaming - emit events as tokens arrive *)
            let headers =
              Cohttp.Header.of_list (base_headers @ Sse.headers)
            in
            (* Start the streaming generator in background *)
            Lwt.async starter;
            (* Prepend prime event to the stream *)
            let prime = Sse.prime_event stream in
            let full_stream = Lwt_stream.append
              (Lwt_stream.of_list [prime])
              event_stream
            in
            let body = Cohttp_lwt.Body.of_stream full_stream in
            Server.respond ~status:`OK ~headers ~body ())

    | `DELETE, "/mcp" ->
        (* Session termination per MCP 2025-11-25 spec *)
        (match get_session_id_header req with
        | Some session_id when Mcp_session.is_valid_session_id session_id ->
            let protocol_version_for_headers =
              match get_protocol_version_opt req with
              | Some v -> v
              | None ->
                  (match Mcp_session.get_session session_id with
                  | Some session -> session.Mcp_session.negotiated_protocol
                  | None -> mcp_protocol_version_default)
            in
            log_request_debug ~label:"DELETE /mcp" req session_id protocol_version_for_headers;
            Notification_sse.unregister session_id;
            Mcp_session.delete_session session_id;
            let headers =
              Cohttp.Header.of_list (mcp_headers session_id protocol_version_for_headers @ cors_headers ())
            in
            Server.respond_string ~status:`No_content ~headers ~body:"" ()
        | _ ->
            let headers = Cohttp.Header.of_list (json_headers_no_session protocol_version) in
            Server.respond_string ~status:`Bad_request ~headers
              ~body:(json_rpc_error (-32600) "Mcp-Session-Id required") ())

    | `OPTIONS, _ ->
        let headers = Cohttp.Header.of_list (cors_preflight_headers ()) in
        Server.respond_string ~status:`OK ~headers ~body:"" ()

    | _ ->
        let body = `Assoc [
          ("error", `String "Not found");
          ("path", `String path);
        ] |> Yojson.Safe.to_string in
        let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
        Server.respond_string ~status:`Not_found ~headers ~body ()
  in

  let server = Server.make ~callback () in
  Lwt_main.run (Server.create ~mode:(`TCP (`Port port)) server)

(** CLI argument parsing *)
let host_arg =
  let doc = "Host to bind (default: 127.0.0.1)" in
  Arg.(value & opt string "127.0.0.1" & info ["host"] ~doc)

let port_arg =
  let doc = "HTTP port (default: 8932)" in
  Arg.(value & opt int 8932 & info ["port"; "p"] ~docv:"PORT" ~doc)

let stdio_flag =
  let doc = "Run in stdio mode (legacy; no SSE)." in
  Arg.(value & flag & info ["stdio"] ~doc)

let agent_arg =
  let doc = "Run in CLI agent mode. Supported: ollama, adam, seele, gemini, claude-cli, codex" in
  Arg.(value & opt (some string) None & info ["agent"] ~docv:"AGENT" ~doc)

let prompt_arg =
  let doc = "Prompt for CLI agent mode (required with --agent)" in
  Arg.(value & opt (some string) None & info ["prompt"] ~docv:"PROMPT" ~doc)

let model_arg =
  let doc = "Model override for CLI agent mode (optional)" in
  Arg.(value & opt (some string) None & info ["model"; "m"] ~docv:"MODEL" ~doc)

let timeout_arg =
  let doc = "Timeout in seconds for CLI agent mode (default: 300)" in
  Arg.(value & opt int 300 & info ["timeout"; "t"] ~docv:"SECONDS" ~doc)

(** Run in CLI agent mode - execute tool directly and print result *)
let run_agent ~agent ~prompt ~model_opt ~timeout =
  let open Llm_mcp.Types in
  let args = match String.lowercase_ascii agent with
    | "ollama" ->
        let model = Option.value model_opt ~default:"devstral" in
        Ollama { prompt; model; system_prompt = None; temperature = 0.7; timeout; stream = false }
    | "gemini" ->
        let model = Option.value model_opt ~default:"gemini-3-pro-preview" in
        Gemini { prompt; model; thinking_level = High; yolo = false; timeout; stream = false }
    | "claude-cli" | "claude" ->
        let model = Option.value model_opt ~default:"opus" in
        Claude { prompt; model; ultrathink = true; system_prompt = None;
                 output_format = Text; allowed_tools = [];
                 working_directory = Sys.getcwd (); timeout; stream = false }
    | "codex" ->
        let model = Option.value model_opt ~default:"gpt-5.2" in
        Codex { prompt; model; reasoning_effort = RXhigh;
                sandbox = WorkspaceWrite; working_directory = None; timeout; stream = false }
    | _ ->
        Printf.eprintf "Unknown agent: %s\nSupported: ollama, gemini, claude-cli, codex\n" agent;
        exit 1
  in
  let result = Lwt_main.run (Llm_mcp.Tools.execute args) in
  (* Print response to stdout *)
  print_endline result.response;
  (* Exit with tool's return code *)
  exit result.returncode

let main stdio host port agent_opt prompt_opt model_opt timeout =
  match agent_opt, prompt_opt with
  | Some agent, Some prompt ->
      (* CLI agent mode *)
      run_agent ~agent ~prompt ~model_opt ~timeout
  | Some _, None ->
      Printf.eprintf "Error: --prompt is required with --agent\n";
      exit 1
  | None, Some _ ->
      Printf.eprintf "Error: --agent is required with --prompt\n";
      exit 1
  | None, None ->
      (* Server mode *)
      if stdio then
        run_stdio ()
      else
        run_http ~host ~port ()

let cmd =
  let doc = "Multi-LLM MCP Server (OCaml) - MCP 2025-11-25" in
  let info = Cmd.info "llm-mcp" ~version:"2.0.1" ~doc in
  Cmd.v info Term.(const main $ stdio_flag $ host_arg $ port_arg $ agent_arg $ prompt_arg $ model_arg $ timeout_arg)

let () = exit (Cmd.eval cmd)

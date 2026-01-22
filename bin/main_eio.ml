(** LLM-MCP Server - Eio/httpun-eio Implementation

    Pure Eio HTTP server using httpun-eio for MCP 2025-11-25.

    Architecture:
    - Server: httpun-eio (Eio native, Effect-based)
    - MCP Handler: Mcp_server_eio (pure Eio, chain.run enabled)
    - SSE: Native Eio streaming
*)

open Printf
open Llm_mcp

(** ============== Configuration ============== *)

type config = {
  port: int;
  host: string;
  max_connections: int;
}

let default_config = {
  port = 8932;
  host = "127.0.0.1";
  max_connections = 128;
}

(** ============== Dashboard HTML ============== *)

let dashboard_html = {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>🐫 Chain Engine Dashboard</title>
  <script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #1a1a2e; color: #eee; padding: 20px; }
    .header { display: flex; align-items: center; gap: 15px; margin-bottom: 20px; }
    .header h1 { font-size: 24px; }
    .status { display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #666; }
    .status.connected { background: #4ade80; animation: pulse 2s infinite; }
    @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
    .main-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-bottom: 20px; }
    .stats-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 15px; }
    .card { background: #16213e; border-radius: 12px; padding: 20px; }
    .card-label { font-size: 12px; color: #888; text-transform: uppercase; margin-bottom: 5px; }
    .card-value { font-size: 28px; font-weight: bold; color: #4ade80; }
    .card-value.warn { color: #fbbf24; }
    .mermaid-container { background: #16213e; border-radius: 12px; padding: 20px; min-height: 300px; }
    .mermaid-container h2 { font-size: 16px; margin-bottom: 15px; color: #888; }
    .mermaid-container .mermaid { background: #1a1a2e; padding: 15px; border-radius: 8px; }
    .mermaid-container .no-chain { color: #666; font-style: italic; text-align: center; padding: 50px; }
    .events { background: #16213e; border-radius: 12px; padding: 20px; max-height: 400px; overflow-y: auto; }
    .events h2 { font-size: 16px; margin-bottom: 15px; color: #888; }
    .event { display: flex; align-items: center; gap: 10px; padding: 10px; border-radius: 8px; margin-bottom: 8px; background: #1a1a2e; font-family: monospace; font-size: 13px; }
    .event-icon { font-size: 16px; }
    .event-type { color: #60a5fa; min-width: 120px; }
    .event-id { color: #a78bfa; min-width: 100px; }
    .event-detail { color: #888; }
    .event.chain_start { border-left: 3px solid #4ade80; }
    .event.chain_complete { border-left: 3px solid #22d3ee; }
    .event.node_start { border-left: 3px solid #fbbf24; }
    .event.node_complete { border-left: 3px solid #a78bfa; }
    .event.chain_error { border-left: 3px solid #f87171; background: #2d1f1f; }
    @media (max-width: 900px) { .main-grid { grid-template-columns: 1fr; } }
  </style>
</head>
<body>
  <div class="header">
    <span class="status" id="status"></span>
    <h1>🐫 Chain Engine Dashboard</h1>
  </div>

  <div class="main-grid">
    <div>
      <div class="stats-grid">
        <div class="card">
          <div class="card-label">Total Chains</div>
          <div class="card-value" id="total-chains">-</div>
        </div>
        <div class="card">
          <div class="card-label">Success Rate</div>
          <div class="card-value" id="success-rate">-</div>
        </div>
        <div class="card">
          <div class="card-label">Avg Duration</div>
          <div class="card-value" id="avg-duration">-</div>
        </div>
        <div class="card">
          <div class="card-label">Total Nodes</div>
          <div class="card-value" id="total-nodes">-</div>
        </div>
      </div>
    </div>
    <div class="mermaid-container">
      <h2>🔄 Current Chain</h2>
      <div id="mermaid-graph"><div class="no-chain">Waiting for chain execution...</div></div>
    </div>
  </div>

  <div class="events">
    <h2>Live Events</h2>
    <div id="event-list"></div>
  </div>

  <script>
    mermaid.initialize({ startOnLoad: false, theme: 'dark', themeVariables: { primaryColor: '#16213e', primaryTextColor: '#eee', lineColor: '#4ade80' } });

    const icons = { chain_start: '▶', chain_complete: '✓', node_start: '→', node_complete: '●', chain_error: '✗', ping: '♥' };
    const eventList = document.getElementById('event-list');
    const statusEl = document.getElementById('status');
    const mermaidEl = document.getElementById('mermaid-graph');
    let eventSource;
    let currentMermaid = null;
    let nodeStates = {};  // node_id -> 'pending' | 'running' | 'complete' | 'error'

    const STATE_COLORS = { pending: '#666', running: '#fbbf24', complete: '#4ade80', error: '#f87171' };
    let renderCounter = 0;

    async function renderMermaid(dsl) {
      currentMermaid = dsl;
      nodeStates = {};
      try {
        const { svg } = await mermaid.render('mermaid-svg-' + (++renderCounter), dsl);
        mermaidEl.innerHTML = svg;
      } catch (e) { console.error('Mermaid render error:', e); }
    }

    function updateNodeStyle(nodeId, state) {
      // Direct SVG manipulation - no re-render needed
      const svg = mermaidEl.querySelector('svg');
      if (!svg) return;
      // Mermaid generates nodes with id like "flowchart-nodeId-0" or class containing node id
      const node = svg.querySelector('[id*="' + nodeId + '"] rect, [id*="' + nodeId + '"] polygon, [id*="' + nodeId + '"] circle, .node#' + nodeId + ' rect');
      if (node) {
        node.style.fill = STATE_COLORS[state];
        node.style.transition = 'fill 0.3s ease';
      }
    }

    function updateNodeState(nodeId, state) {
      nodeStates[nodeId] = state;
      updateNodeStyle(nodeId, state);
    }

    function connect() {
      eventSource = new EventSource('/chain/events');
      eventSource.onopen = () => statusEl.classList.add('connected');
      eventSource.onerror = () => {
        statusEl.classList.remove('connected');
        nodeStates = {};
        currentMermaid = null;
        setTimeout(connect, 3000);
      };

      eventSource.addEventListener('chain_start', e => {
        const data = JSON.parse(e.data);
        if (data.mermaid_dsl) renderMermaid(data.mermaid_dsl);
        addEvent('chain_start', data);
      });

      eventSource.addEventListener('node_start', e => {
        const data = JSON.parse(e.data);
        updateNodeState(data.node_id, 'running');
        addEvent('node_start', data);
      });

      eventSource.addEventListener('node_complete', e => {
        const data = JSON.parse(e.data);
        updateNodeState(data.node_id, 'complete');
        addEvent('node_complete', data);
      });

      eventSource.addEventListener('chain_complete', e => {
        const data = JSON.parse(e.data);
        addEvent('chain_complete', data);
      });

      eventSource.addEventListener('chain_error', e => {
        const data = JSON.parse(e.data);
        if (data.node_id) updateNodeState(data.node_id, 'error');
        addEvent('chain_error', data);
      });
    }

    function addEvent(type, data) {
      const div = document.createElement('div');
      div.className = 'event ' + type;
      const id = data.chain_id || data.node_id || '';
      const detail = data.duration_ms ? (data.duration_ms / 1000).toFixed(1) + 's' : (data.message || '');
      div.innerHTML = '<span class="event-icon">' + icons[type] + '</span><span class="event-type">' + type + '</span><span class="event-id">' + id + '</span><span class="event-detail">' + detail + '</span>';
      eventList.insertBefore(div, eventList.firstChild);
      if (eventList.children.length > 50) eventList.removeChild(eventList.lastChild);
    }

    function fetchStats() {
      fetch('/chain/stats').then(r => r.json()).then(s => {
        document.getElementById('total-chains').textContent = s.total_chains;
        document.getElementById('success-rate').textContent = (s.success_rate * 100).toFixed(0) + '%';
        document.getElementById('avg-duration').textContent = (s.avg_duration_ms / 1000).toFixed(1) + 's';
        document.getElementById('total-nodes').textContent = s.total_nodes;
        document.getElementById('success-rate').classList.toggle('warn', s.success_rate < 0.9);
      }).catch(() => {});
    }

    connect();
    fetchStats();
    setInterval(fetchStats, 5000);
  </script>
</body>
</html>|}

(** ============== MCP Protocol Constants ============== *)

let mcp_protocol_versions = Mcp_server.supported_protocol_versions
let mcp_protocol_version_default = Mcp_session.protocol_version

(** ============== Debug Logging ============== *)

let debug_enabled =
  match Sys.getenv_opt "LLM_MCP_DEBUG" with
  | Some "1" -> true
  | _ -> (match Sys.getenv_opt "MCP_DEBUG" with Some "1" -> true | _ -> false)

let log_debug fmt =
  if debug_enabled then eprintf fmt else Printf.ifprintf stderr fmt

(** ============== Request Helpers ============== *)

let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let allowed_origins = [
  "http://localhost"; "https://localhost";
  "http://127.0.0.1"; "https://127.0.0.1";
]

let validate_origin headers =
  match Httpun.Headers.get headers "origin" with
  | None -> true
  | Some origin -> List.exists (fun prefix -> starts_with ~prefix origin) allowed_origins

let get_header headers name = Httpun.Headers.get headers name
let get_header_or headers name default =
  match get_header headers name with Some v -> v | None -> default

let get_session_id_header headers = get_header headers "mcp-session-id"
let get_protocol_version headers =
  get_header_or headers "mcp-protocol-version" mcp_protocol_version_default

let is_valid_protocol_version version =
  List.mem version mcp_protocol_versions

let wants_sse headers =
  Mcp_protocol.Http_negotiation.accepts_sse_header (get_header headers "accept")

let accepts_streamable_mcp headers =
  Mcp_protocol.Http_negotiation.accepts_streamable_mcp (get_header headers "accept")

let get_last_event_id headers =
  match get_header headers "last-event-id" with
  | Some id -> (try Some (int_of_string id) with Failure _ -> None)
  | None -> None

(** ============== Session Management ============== *)

let get_or_create_session ~protocol_version headers =
  Mcp_session.cleanup_expired ();
  match get_session_id_header headers with
  | Some session_id when Mcp_session.is_valid_session_id session_id -> (
      match Mcp_session.get_session session_id with
      | Some session -> session
      | None -> Mcp_session.create_session ~id:session_id ~protocol:protocol_version ())
  | Some _ -> Mcp_session.create_session ~protocol:protocol_version ()
  | None -> Mcp_session.create_session ~protocol:protocol_version ()

(** ============== Response Helpers ============== *)

module Response = struct
  let cors_headers = [
    ("access-control-allow-origin", "*");
    ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ]

  let mcp_headers session_id protocol_version = [
    ("mcp-session-id", session_id);
    ("mcp-protocol-version", protocol_version);
  ]

  let json_with_session ?(status = `OK) ~session_id ~protocol_version body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ mcp_headers session_id protocol_version @ cors_headers) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ cors_headers) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let html ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/html; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ cors_headers) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let not_found reqd = text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list [
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, DELETE, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-ID, Accept, Origin");
      ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
      ("access-control-max-age", "86400");
      ("content-length", "0");
    ] in
    let response = Httpun.Response.create ~headers `No_content in
    Httpun.Reqd.respond_with_string reqd response ""

  let sse_stream ~session_id ~protocol_version reqd ~on_write =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/event-stream");
      ("cache-control", "no-cache");
      ("connection", "keep-alive");
      ("x-accel-buffering", "no");
    ] @ mcp_headers session_id protocol_version @ cors_headers) in
    let response = Httpun.Response.create ~headers `OK in
    let body = Httpun.Reqd.respond_with_streaming reqd response in
    on_write body
end

module Request = struct
  let read_body_async reqd callback =
    let body = Httpun.Reqd.request_body reqd in
    let chunks = ref [] in
    let rec read_loop () =
      Httpun.Body.Reader.schedule_read body
        ~on_eof:(fun () ->
          callback (String.concat "" (List.rev !chunks)))
        ~on_read:(fun buffer ~off ~len ->
          let chunk = Bigstringaf.substring buffer ~off ~len in
          chunks := chunk :: !chunks;
          read_loop ())
    in
    read_loop ()

  let path (request : Httpun.Request.t) =
    request.target |> String.split_on_char '?' |> List.hd

  let meth (request : Httpun.Request.t) = request.meth
  let headers (request : Httpun.Request.t) = request.headers
end

(** ============== SSE Client Registry ============== *)

type sse_client = {
  body: Httpun.Body.Writer.t;
  mutable connected: bool;
}

let sse_clients : (int, sse_client) Hashtbl.t = Hashtbl.create 64
let sse_client_counter = ref 0

let register_sse_client body =
  incr sse_client_counter;
  let id = !sse_client_counter in
  let client = { body; connected = true } in
  Hashtbl.add sse_clients id client;
  id

let unregister_sse_client id =
  (match Hashtbl.find_opt sse_clients id with
   | Some c -> c.connected <- false
   | None -> ());
  Hashtbl.remove sse_clients id

let sse_client_count () = Hashtbl.length sse_clients

let broadcast_sse_shutdown reason =
  let data = sprintf
    {|{"jsonrpc":"2.0","method":"notifications/shutdown","params":{"reason":"%s","message":"Server is shutting down, please reconnect"}}|}
    reason
  in
  let msg = sprintf "event: notification\ndata: %s\n\n" data in
  Hashtbl.iter (fun _ client ->
    if client.connected then
      try
        Httpun.Body.Writer.write_string client.body msg;
        Httpun.Body.Writer.flush client.body ignore
      with
      | Eio.Io _ -> client.connected <- false  (* Mark disconnected on I/O error *)
      | _ -> ()  (* Ignore other errors during shutdown broadcast *)
  ) sse_clients

let send_sse_event body ~event ~data =
  let msg = sprintf "event: %s\ndata: %s\n\n" event data in
  Httpun.Body.Writer.write_string body msg;
  Httpun.Body.Writer.flush body ignore

let[@warning "-32"] send_sse_event_with_id body ~id ~event ~data =
  let msg = sprintf "id: %d\nevent: %s\ndata: %s\n\n" id event data in
  Httpun.Body.Writer.write_string body msg;
  Httpun.Body.Writer.flush body ignore

(** ============== JSON-RPC Helpers ============== *)

let json_rpc_error code message =
  Yojson.Safe.to_string (Mcp_server.make_error ~id:`Null code message)

(** ============== HTTP Handlers ============== *)

let health_handler _request reqd =
  let body = Mcp_server.health_response () in
  Response.json body reqd

let handle_get_mcp ~clock headers reqd =
  let protocol_version = get_protocol_version headers in
  let session = get_or_create_session ~protocol_version headers in
  let last_event_id = get_last_event_id headers in

  log_debug "LLM_MCP_DEBUG: GET /mcp session=%s protocol=%s\n%!" session.id protocol_version;

  Response.sse_stream ~session_id:session.id ~protocol_version reqd ~on_write:(fun body ->
    (* Register for shutdown notification *)
    let client_id = register_sse_client body in

    (* Also register with Notification_sse for MCP notifications *)
    let _sse_client_id = Notification_sse.register session.id
      ~push:(fun s ->
        try
          Httpun.Body.Writer.write_string body s;
          Httpun.Body.Writer.flush body ignore
        with Eio.Io _ | Invalid_argument _ -> ())  (* Connection closed *)
      ~last_event_id:(Option.value last_event_id ~default:0)
    in

    (* Replay missed events if Last-Event-ID provided *)
    (match last_event_id with
    | Some last_id ->
        let missed = Notification_sse.get_events_after last_id in
        List.iter (fun ev ->
          Httpun.Body.Writer.write_string body ev;
          Httpun.Body.Writer.flush body ignore
        ) missed
    | None -> ());

    (* Send SSE priming event *)
    let prime = Notification_sse.prime_event ~retry_ms:5000 in
    Httpun.Body.Writer.write_string body prime;
    Httpun.Body.Writer.flush body ignore;

    (* Keep connection alive with periodic pings *)
    let rec ping_loop () =
      try
        Eio.Time.sleep clock 15.0;
        let timestamp = string_of_float (Unix.gettimeofday ()) in
        send_sse_event body ~event:"ping" ~data:timestamp;
        ping_loop ()
      with
      | Eio.Io _ | Invalid_argument _ | Eio.Cancel.Cancelled _ ->
          (* Connection closed, cancelled, or body invalid - cleanup *)
          unregister_sse_client client_id;
          Notification_sse.unregister session.id;
          (try Httpun.Body.Writer.close body with Invalid_argument _ -> ())
    in
    ping_loop ()
  )

(** Handle GET /chain/events - SSE stream for real-time chain progress *)
let handle_chain_events ~clock reqd =
  (* Create a simple SSE stream without MCP session *)
  let headers = Httpun.Headers.of_list [
    ("content-type", "text/event-stream");
    ("cache-control", "no-cache");
    ("connection", "keep-alive");
    ("x-accel-buffering", "no");
    ("access-control-allow-origin", "*");
  ] in
  let response = Httpun.Response.create ~headers `OK in
  let body = Httpun.Reqd.respond_with_streaming reqd response in

  (* Send initial priming event *)
  let prime = sprintf "retry: 5000\nid: 0\n\n" in
  Httpun.Body.Writer.write_string body prime;
  Httpun.Body.Writer.flush body ignore;

  (* Subscribe to chain telemetry events *)
  let subscription = Chain_telemetry.subscribe (fun event ->
    let event_type, json_data = match event with
      | Chain_telemetry.ChainStart payload ->
          ("chain_start", `Assoc [
            ("chain_id", `String payload.Chain_telemetry.start_chain_id);
            ("nodes", `Int payload.start_nodes);
            ("timestamp", `Float payload.start_timestamp);
            ("mermaid_dsl", match payload.start_mermaid_dsl with Some m -> `String m | None -> `Null);
          ])
      | Chain_telemetry.NodeStart payload ->
          ("node_start", `Assoc [
            ("node_id", `String payload.Chain_telemetry.node_start_id);
            ("node_type", `String payload.node_start_type);
            ("parent", match payload.node_parent with Some p -> `String p | None -> `Null);
          ])
      | Chain_telemetry.NodeComplete payload ->
          ("node_complete", `Assoc [
            ("node_id", `String payload.Chain_telemetry.node_complete_id);
            ("duration_ms", `Int payload.node_duration_ms);
            ("tokens", `Int payload.node_tokens.Chain_category.total_tokens);
            ("confidence", `Float payload.node_confidence);
          ])
      | Chain_telemetry.ChainComplete payload ->
          ("chain_complete", `Assoc [
            ("chain_id", `String payload.Chain_telemetry.complete_chain_id);
            ("duration_ms", `Int payload.complete_duration_ms);
            ("tokens", `Int payload.complete_tokens.Chain_category.total_tokens);
            ("nodes_executed", `Int payload.nodes_executed);
            ("nodes_skipped", `Int payload.nodes_skipped);
          ])
      | Chain_telemetry.Error payload ->
          ("chain_error", `Assoc [
            ("node_id", `String payload.Chain_telemetry.error_node_id);
            ("message", `String payload.error_message);
            ("retries", `Int payload.error_retries);
            ("timestamp", `Float payload.error_timestamp);
          ])
    in
    let data = Yojson.Safe.to_string json_data in
    try
      send_sse_event body ~event:event_type ~data
    with Eio.Io _ | Invalid_argument _ -> ()  (* Connection closed *)
  ) in

  (* Keep connection alive with periodic pings until client disconnects *)
  let rec ping_loop () =
    try
      Eio.Time.sleep clock 15.0;
      let timestamp = string_of_float (Unix.gettimeofday ()) in
      send_sse_event body ~event:"ping" ~data:timestamp;
      ping_loop ()
    with
    | Eio.Io _ | Invalid_argument _ | Eio.Cancel.Cancelled _ ->
        (* Client disconnected, clean up *)
        Chain_telemetry.unsubscribe subscription;
        (try Httpun.Body.Writer.close body with Invalid_argument _ -> ())
  in
  ping_loop ()

let handle_post_mcp ~sw ~clock ~proc_mgr ~store headers reqd =
  let protocol_version = get_protocol_version headers in

  if not (accepts_streamable_mcp headers) then begin
    let body = json_rpc_error (-32600)
      "Invalid Accept header: must include application/json and text/event-stream" in
    Response.json ~status:`Bad_request body reqd
  end else begin
    let wants_stream = wants_sse headers in
    let session_id_from_header = get_session_id_header headers in

    log_debug "LLM_MCP_DEBUG: POST /mcp (Eio) protocol=%s stream=%b session_header=%s\n%!"
      protocol_version wants_stream
      (Option.value session_id_from_header ~default:"<none>");

    Request.read_body_async reqd (fun body_str ->
      (* Convert Httpun.Headers to (string * string) list for Mcp_server_eio *)
      let headers_list = Httpun.Headers.to_list headers in

      (* Call the Eio-native handler *)
      let (new_session_id_opt, json_response) =
        Mcp_server_eio.handle_request
          ~sw ~proc_mgr ~clock ~store
          ~headers:headers_list
          body_str
      in

      (* Determine session ID: use new one if created, otherwise from header *)
      let session_id = match new_session_id_opt with
        | Some id -> id
        | None -> Option.value session_id_from_header ~default:"unknown"
      in

      (* Check if response is an error *)
      let is_error = match json_response with
        | `Assoc fields ->
            (match List.assoc_opt "error" fields with Some _ -> true | None -> false)
        | _ -> false
      in

      (* Notifications return `Null - respond with 202 Accepted per MCP Streamable HTTP spec *)
      if json_response = `Null then begin
        let resp_headers = Httpun.Headers.of_list ([
          ("content-length", "0");
        ] @ Response.cors_headers) in
        let resp = Httpun.Response.create ~headers:resp_headers `Accepted in
        Httpun.Reqd.respond_with_string reqd resp ""
      end
      else if is_error then
        Response.json_with_session ~status:`Bad_request
          ~session_id ~protocol_version
          (Yojson.Safe.to_string json_response) reqd
      else if wants_stream then begin
        (* SSE response with single message event *)
        let stream = Sse.create_stream () in
        let body = (Sse.prime_event stream) ^ (Sse.json_event stream json_response) in
        let resp_headers = Httpun.Headers.of_list ([
          ("content-type", "text/event-stream");
          ("content-length", string_of_int (String.length body));
        ] @ Response.mcp_headers session_id protocol_version @ Response.cors_headers) in
        let resp = Httpun.Response.create ~headers:resp_headers `OK in
        Httpun.Reqd.respond_with_string reqd resp body
      end else
        Response.json_with_session ~session_id ~protocol_version
          (Yojson.Safe.to_string json_response) reqd
    )
  end

let handle_delete_mcp headers reqd =
  let protocol_version = get_protocol_version headers in
  match get_session_id_header headers with
  | Some session_id when Mcp_session.is_valid_session_id session_id ->
      log_debug "LLM_MCP_DEBUG: DELETE /mcp session=%s\n%!" session_id;
      Notification_sse.unregister session_id;
      Mcp_session.delete_session session_id;
      let hdrs = Httpun.Headers.of_list (
        Response.mcp_headers session_id protocol_version @ Response.cors_headers
      ) in
      let resp = Httpun.Response.create ~headers:hdrs `No_content in
      Httpun.Reqd.respond_with_string reqd resp ""
  | _ ->
      Response.json ~status:`Bad_request
        (json_rpc_error (-32600) "Mcp-Session-Id required") reqd

(** ============== Router ============== *)

let route_request ~sw ~clock ~proc_mgr ~store request reqd =
  let path = Request.path request in
  let meth = Request.meth request in
  let headers = Request.headers request in
  let is_mcp_path = path = "/mcp" || path = "/" in
  let protocol_version = get_protocol_version headers in

  (* Origin validation for MCP paths *)
  if is_mcp_path && meth <> `OPTIONS && not (validate_origin headers) then
    Response.json ~status:`Forbidden (json_rpc_error (-32600) "Invalid origin") reqd
  (* Protocol version validation *)
  else if is_mcp_path && meth <> `OPTIONS && not (is_valid_protocol_version protocol_version) then
    Response.json ~status:`Bad_request (json_rpc_error (-32600) "Unsupported protocol version") reqd
  else match (meth, path) with
  | `OPTIONS, _ ->
      Response.cors_preflight reqd

  | `GET, "/health" ->
      health_handler request reqd

  | `GET, "/" ->
      Response.text "🐫 llm-mcp (OCaml Eio) MCP 2025-11-25 server" reqd

  (* Chain stats endpoint for monitoring dashboard *)
  | `GET, "/chain/stats" ->
      let stats = Chain_stats.compute () in
      let body = Yojson.Safe.to_string (Chain_stats.to_json stats) in
      Response.json body reqd

  (* Chain status endpoint for currently running chains *)
  | `GET, "/chain/status" ->
      let status = Chain_telemetry.get_running_chains () in
      let body = Yojson.Safe.to_string (`List (List.map (fun (id, started, progress) ->
        `Assoc [
          ("chain_id", `String id);
          ("started_at", `Float started);
          ("progress", `Float progress);
          ("elapsed_sec", `Float (Unix.gettimeofday () -. started));
        ]
      ) status)) in
      Response.json body reqd

  (* Chain events SSE endpoint for real-time progress monitoring *)
  | `GET, "/chain/events" ->
      handle_chain_events ~clock reqd

  (* Dashboard HTML page for monitoring *)
  | `GET, "/dashboard" ->
      Response.html dashboard_html reqd

  | `GET, "/mcp" ->
      handle_get_mcp ~clock headers reqd

  | `POST, "/" | `POST, "/mcp" ->
      handle_post_mcp ~sw ~clock ~proc_mgr ~store headers reqd

  | `DELETE, "/mcp" ->
      handle_delete_mcp headers reqd

  | _ ->
      Response.not_found reqd

(** ============== httpun-eio Server ============== *)

let make_request_handler ~sw ~clock ~proc_mgr ~store =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    route_request ~sw ~clock ~proc_mgr ~store request reqd

let error_handler _client_addr ?request:_ error start_response =
  let response_body = start_response Httpun.Headers.empty in
  let msg = match error with
    | `Exn exn -> Printexc.to_string exn
    | `Bad_request -> "Bad Request"
    | `Bad_gateway -> "Bad Gateway"
    | `Internal_server_error -> "Internal Server Error"
  in
  Httpun.Body.Writer.write_string response_body msg;
  Httpun.Body.Writer.close response_body

(** Graceful shutdown exception *)
exception Shutdown

let run ~sw ~net ~clock ~proc_mgr ~store config =
  let request_handler = make_request_handler ~sw ~clock ~proc_mgr ~store in
  let ip = match Ipaddr.of_string config.host with
    | Ok addr -> Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets addr)
    | Error _ -> Eio.Net.Ipaddr.V4.loopback
  in
  let addr = `Tcp (ip, config.port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr in

  eprintf "🐫 llm-mcp (OCaml Eio) MCP 2025-11-25 server\n";
  eprintf "   HTTP: http://%s:%d\n" config.host config.port;
  eprintf "   MCP:  GET  /mcp -> SSE stream (notifications)\n";
  eprintf "         POST /mcp -> JSON-RPC requests\n";
  eprintf "   Graceful shutdown: SIGTERM/SIGINT supported\n%!";

  let rec accept_loop () =
    let flow, client_addr = Eio.Net.accept ~sw socket in
    Eio.Fiber.fork ~sw (fun () ->
      try
        Httpun_eio.Server.create_connection_handler
          ~sw
          ~request_handler
          ~error_handler
          client_addr
          flow
      with exn ->
        eprintf "[llm-mcp] Connection error: %s\n%!" (Printexc.to_string exn)
    );
    accept_loop ()
  in
  accept_loop ()

(** ============== Entry Point ============== *)

let start_server config =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let store = Mcp_server_eio.create_session_store () in

  (* Graceful shutdown setup *)
  let switch_ref = ref None in
  let shutdown_initiated = ref false in
  let initiate_shutdown signal_name =
    if not !shutdown_initiated then begin
      shutdown_initiated := true;
      eprintf "\n🐫 llm-mcp: Received %s, shutting down gracefully...\n%!" signal_name;

      (* Broadcast shutdown notification to all SSE clients *)
      broadcast_sse_shutdown signal_name;
      eprintf "🐫 llm-mcp: Sent shutdown notification to %d SSE clients\n%!" (sse_client_count ());

      (* Give clients 500ms to receive the notification *)
      Unix.sleepf 0.5;

      match !switch_ref with
      | Some sw -> Eio.Switch.fail sw Shutdown
      | None -> ()
    end
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGTERM"));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGINT"));

  (* Initialize Chain Registry *)
  let chains_dir = "data/chains" in
  if Sys.file_exists chains_dir && Sys.is_directory chains_dir then begin
    eprintf "🐫 llm-mcp: Loading chains from %s...\n%!" chains_dir;
    Chain_registry.init ~persist_dir:chains_dir ()
  end else
    Chain_registry.init ();

  (try
    Eio.Switch.run @@ fun sw ->
    switch_ref := Some sw;
    run ~sw ~net ~clock ~proc_mgr ~store config
  with
  | Shutdown ->
      eprintf "🐫 llm-mcp: Shutdown complete.\n%!"
  | Eio.Cancel.Cancelled _ ->
      eprintf "🐫 llm-mcp: Shutdown complete.\n%!")

(** ============== CLI ============== *)

open Cmdliner

let host_arg =
  let doc = "Host to bind (default: 127.0.0.1)" in
  Arg.(value & opt string "127.0.0.1" & info ["host"] ~doc)

let port_arg =
  let doc = "HTTP port (default: 8932)" in
  Arg.(value & opt int 8932 & info ["port"; "p"] ~docv:"PORT" ~doc)

let main host port =
  (* Enable chain stats collection *)
  Chain_stats.enable ();
  let config = { default_config with host; port } in
  start_server config

let cmd =
  let doc = "LLM-MCP Server (Eio) - MCP 2025-11-25" in
  let info = Cmd.info "llm-mcp" ~version:Version.version ~doc in
  Cmd.v info Term.(const main $ host_arg $ port_arg)

let () = exit (Cmd.eval cmd)

(** Chain Engine Web Dashboard - Real-time Mermaid visualization

    HTTP 서버로 Chain 실행 상태를 Mermaid 다이어그램과 함께 시각화합니다.

    Features:
    - /           : 대시보드 HTML 페이지
    - /api/chains : 등록된 체인 목록
    - /api/stats  : 실행 통계
    - /api/run    : 체인 실행 (POST)
    - /sse        : 실시간 이벤트 스트림

    Usage:
      dune exec chain_dashboard_web -- --port 8080
      open http://localhost:8080

    @author Chain Engine
    @since 2026-01
*)

open Eio

(** {1 Configuration} *)

let default_port = 8080

(** {1 HTTP Response Helpers} *)

let html_content_type = "text/html; charset=utf-8"
let json_content_type = "application/json; charset=utf-8"
let sse_content_type = "text/event-stream"
let _css_content_type = "text/css; charset=utf-8"  (* Reserved for future static CSS serving *)
let _js_content_type = "application/javascript; charset=utf-8"  (* Reserved for future static JS serving *)

(** Build HTTP response *)
let http_response ?(status=200) ?(content_type=html_content_type) body =
  let status_text = match status with
    | 200 -> "OK"
    | 400 -> "Bad Request"
    | 404 -> "Not Found"
    | 500 -> "Internal Server Error"
    | _ -> "Unknown"
  in
  Printf.sprintf
    "HTTP/1.1 %d %s\r\n\
     Content-Type: %s\r\n\
     Content-Length: %d\r\n\
     Access-Control-Allow-Origin: *\r\n\
     Connection: close\r\n\
     \r\n\
     %s"
    status status_text content_type (String.length body) body

(** Build SSE response headers *)
let sse_headers () =
  Printf.sprintf
    "HTTP/1.1 200 OK\r\n\
     Content-Type: %s\r\n\
     Cache-Control: no-cache\r\n\
     Connection: keep-alive\r\n\
     Access-Control-Allow-Origin: *\r\n\
     \r\n"
    sse_content_type

(** {1 HTML Templates} *)

let dashboard_html = {|<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Chain Engine Dashboard</title>
  <script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: 'SF Mono', 'Monaco', 'Menlo', monospace;
      background: #0d1117;
      color: #c9d1d9;
      min-height: 100vh;
    }
    .container {
      max-width: 1400px;
      margin: 0 auto;
      padding: 20px;
    }
    header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 20px 0;
      border-bottom: 1px solid #30363d;
      margin-bottom: 20px;
    }
    h1 {
      color: #58a6ff;
      font-size: 24px;
    }
    .status-badge {
      padding: 6px 12px;
      border-radius: 20px;
      font-size: 12px;
      font-weight: 600;
    }
    .status-connected { background: #238636; }
    .status-disconnected { background: #da3633; }
    .grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 20px;
    }
    @media (max-width: 1000px) {
      .grid { grid-template-columns: 1fr; }
    }
    .panel {
      background: #161b22;
      border: 1px solid #30363d;
      border-radius: 8px;
      padding: 20px;
    }
    .panel-title {
      font-size: 14px;
      color: #8b949e;
      margin-bottom: 16px;
      text-transform: uppercase;
      letter-spacing: 1px;
    }
    .mermaid-container {
      background: #0d1117;
      border-radius: 8px;
      padding: 20px;
      min-height: 300px;
      overflow: auto;
    }
    .mermaid {
      display: flex;
      justify-content: center;
    }
    /* Node highlighting */
    .node-active rect, .node-active circle, .node-active polygon {
      stroke: #f0883e !important;
      stroke-width: 3px !important;
      animation: pulse 1s infinite;
    }
    .node-complete rect, .node-complete circle, .node-complete polygon {
      stroke: #3fb950 !important;
      stroke-width: 2px !important;
    }
    .node-error rect, .node-error circle, .node-error polygon {
      stroke: #f85149 !important;
      stroke-width: 2px !important;
    }
    @keyframes pulse {
      0%, 100% { opacity: 1; }
      50% { opacity: 0.6; }
    }
    .stats-grid {
      display: grid;
      grid-template-columns: repeat(4, 1fr);
      gap: 12px;
    }
    .stat-card {
      background: #0d1117;
      border-radius: 6px;
      padding: 16px;
      text-align: center;
    }
    .stat-value {
      font-size: 28px;
      font-weight: 700;
      color: #58a6ff;
    }
    .stat-label {
      font-size: 11px;
      color: #8b949e;
      margin-top: 4px;
    }
    .chain-list {
      max-height: 300px;
      overflow-y: auto;
    }
    .chain-item {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 12px;
      border-bottom: 1px solid #30363d;
      cursor: pointer;
      transition: background 0.2s;
    }
    .chain-item:hover {
      background: #21262d;
    }
    .chain-name {
      font-weight: 600;
      color: #c9d1d9;
    }
    .chain-nodes {
      font-size: 12px;
      color: #8b949e;
    }
    .btn {
      padding: 8px 16px;
      border-radius: 6px;
      border: none;
      font-size: 12px;
      font-weight: 600;
      cursor: pointer;
      transition: all 0.2s;
    }
    .btn-primary {
      background: #238636;
      color: white;
    }
    .btn-primary:hover {
      background: #2ea043;
    }
    .btn-secondary {
      background: #21262d;
      color: #c9d1d9;
      border: 1px solid #30363d;
    }
    .btn-secondary:hover {
      background: #30363d;
    }
    .log-container {
      max-height: 200px;
      overflow-y: auto;
      font-size: 12px;
      background: #0d1117;
      border-radius: 6px;
      padding: 12px;
    }
    .log-entry {
      padding: 4px 0;
      border-bottom: 1px solid #21262d;
    }
    .log-time {
      color: #8b949e;
      margin-right: 8px;
    }
    .log-info { color: #58a6ff; }
    .log-success { color: #3fb950; }
    .log-error { color: #f85149; }
    .log-warn { color: #d29922; }
    .input-group {
      margin-bottom: 12px;
    }
    .input-group label {
      display: block;
      font-size: 12px;
      color: #8b949e;
      margin-bottom: 4px;
    }
    .input-group textarea {
      width: 100%;
      background: #0d1117;
      border: 1px solid #30363d;
      border-radius: 6px;
      color: #c9d1d9;
      padding: 12px;
      font-family: inherit;
      font-size: 13px;
      resize: vertical;
    }
    .input-group textarea:focus {
      outline: none;
      border-color: #58a6ff;
    }
    .token-bar {
      display: flex;
      align-items: center;
      margin-bottom: 8px;
    }
    .token-label {
      width: 80px;
      font-size: 12px;
      color: #8b949e;
    }
    .token-track {
      flex: 1;
      height: 8px;
      background: #21262d;
      border-radius: 4px;
      overflow: hidden;
    }
    .token-fill {
      height: 100%;
      border-radius: 4px;
      transition: width 0.3s;
    }
    .token-fill.gemini { background: #4285f4; }
    .token-fill.claude { background: #cc785c; }
    .token-fill.codex { background: #10a37f; }
    .token-fill.ollama { background: #a855f7; }
    .token-count {
      width: 80px;
      text-align: right;
      font-size: 12px;
      color: #8b949e;
    }
  </style>
</head>
<body>
  <div class="container">
    <header>
      <h1>⚡ Chain Engine Dashboard</h1>
      <span id="connection-status" class="status-badge status-disconnected">Disconnected</span>
    </header>

    <div class="grid">
      <!-- Left Column: Mermaid Diagram -->
      <div class="panel">
        <div class="panel-title">Chain Visualization</div>
        <div class="mermaid-container">
          <div id="mermaid-diagram" class="mermaid">
            graph TD
              Start([Start]) --> A[No Chain Selected]
              A --> End([End])
          </div>
        </div>
      </div>

      <!-- Right Column: Controls & Stats -->
      <div style="display: flex; flex-direction: column; gap: 20px;">
        <!-- Stats -->
        <div class="panel">
          <div class="panel-title">Statistics</div>
          <div class="stats-grid">
            <div class="stat-card">
              <div class="stat-value" id="stat-chains">0</div>
              <div class="stat-label">CHAINS</div>
            </div>
            <div class="stat-card">
              <div class="stat-value" id="stat-executions">0</div>
              <div class="stat-label">EXECUTIONS</div>
            </div>
            <div class="stat-card">
              <div class="stat-value" id="stat-tokens">0</div>
              <div class="stat-label">TOKENS</div>
            </div>
            <div class="stat-card">
              <div class="stat-value" id="stat-success">0%</div>
              <div class="stat-label">SUCCESS</div>
            </div>
          </div>
        </div>

        <!-- Token Usage -->
        <div class="panel">
          <div class="panel-title">Token Usage by Model</div>
          <div class="token-bar">
            <span class="token-label">Gemini</span>
            <div class="token-track"><div class="token-fill gemini" id="token-gemini" style="width: 0%"></div></div>
            <span class="token-count" id="token-gemini-count">0</span>
          </div>
          <div class="token-bar">
            <span class="token-label">Claude</span>
            <div class="token-track"><div class="token-fill claude" id="token-claude" style="width: 0%"></div></div>
            <span class="token-count" id="token-claude-count">0</span>
          </div>
          <div class="token-bar">
            <span class="token-label">Codex</span>
            <div class="token-track"><div class="token-fill codex" id="token-codex" style="width: 0%"></div></div>
            <span class="token-count" id="token-codex-count">0</span>
          </div>
          <div class="token-bar">
            <span class="token-label">Ollama</span>
            <div class="token-track"><div class="token-fill ollama" id="token-ollama" style="width: 0%"></div></div>
            <span class="token-count" id="token-ollama-count">0</span>
          </div>
        </div>

        <!-- Chain Input -->
        <div class="panel">
          <div class="panel-title">Run Chain</div>
          <div class="input-group">
            <label>Mermaid DSL</label>
            <textarea id="chain-input" rows="6" placeholder="graph TD
  A[LLM:gemini] --> B[LLM:claude]
  B --> C{Quorum:2}
  C --> D[Merge]"></textarea>
          </div>
          <div style="display: flex; gap: 8px;">
            <button class="btn btn-primary" onclick="runChain()">▶ Execute</button>
            <button class="btn btn-secondary" onclick="previewChain()">👁 Preview</button>
            <button class="btn btn-secondary" onclick="loadSample('magi')">MAGI</button>
            <button class="btn btn-secondary" onclick="loadSample('pr')">PR Review</button>
          </div>
        </div>
      </div>
    </div>

    <!-- Event Log -->
    <div class="panel" style="margin-top: 20px;">
      <div class="panel-title">Event Log</div>
      <div class="log-container" id="event-log">
        <div class="log-entry">
          <span class="log-time">[--:--:--]</span>
          <span class="log-info">Waiting for connection...</span>
        </div>
      </div>
    </div>
  </div>

  <script>
    // Mermaid initialization
    mermaid.initialize({
      startOnLoad: true,
      theme: 'dark',
      themeVariables: {
        primaryColor: '#238636',
        primaryTextColor: '#c9d1d9',
        primaryBorderColor: '#30363d',
        lineColor: '#8b949e',
        secondaryColor: '#21262d',
        tertiaryColor: '#161b22'
      }
    });

    // State
    let eventSource = null;
    let currentChain = null;
    let nodeStates = {};

    // SSE Connection
    function connectSSE() {
      if (eventSource) eventSource.close();

      eventSource = new EventSource('/sse');

      eventSource.onopen = () => {
        document.getElementById('connection-status').className = 'status-badge status-connected';
        document.getElementById('connection-status').textContent = 'Connected';
        addLog('Connected to server', 'success');
      };

      eventSource.onerror = () => {
        document.getElementById('connection-status').className = 'status-badge status-disconnected';
        document.getElementById('connection-status').textContent = 'Disconnected';
        addLog('Connection lost, reconnecting...', 'error');
        setTimeout(connectSSE, 3000);
      };

      eventSource.addEventListener('stats', (e) => {
        const stats = JSON.parse(e.data);
        updateStats(stats);
      });

      eventSource.addEventListener('node_start', (e) => {
        const data = JSON.parse(e.data);
        highlightNode(data.node_id, 'active');
        addLog(`Node started: ${data.node_id}`, 'info');
      });

      eventSource.addEventListener('node_complete', (e) => {
        const data = JSON.parse(e.data);
        highlightNode(data.node_id, 'complete');
        addLog(`Node complete: ${data.node_id} (${data.duration_ms}ms)`, 'success');
      });

      eventSource.addEventListener('node_error', (e) => {
        const data = JSON.parse(e.data);
        highlightNode(data.node_id, 'error');
        addLog(`Node error: ${data.node_id} - ${data.error}`, 'error');
      });

      eventSource.addEventListener('chain_complete', (e) => {
        const data = JSON.parse(e.data);
        addLog(`Chain complete: ${data.chain_id} (${data.duration_ms}ms, ${data.tokens} tokens)`, 'success');
        fetchStats();
      });
    }

    // Highlight node in Mermaid diagram
    function highlightNode(nodeId, state) {
      nodeStates[nodeId] = state;

      // Find and update node element
      const nodes = document.querySelectorAll('.node');
      nodes.forEach(node => {
        const text = node.querySelector('span, text')?.textContent || '';
        if (text.includes(nodeId) || node.id?.includes(nodeId)) {
          node.classList.remove('node-active', 'node-complete', 'node-error');
          node.classList.add(`node-${state}`);
        }
      });
    }

    // Update stats display
    function updateStats(stats) {
      document.getElementById('stat-chains').textContent = stats.total_chains || 0;
      document.getElementById('stat-executions').textContent = stats.total_executions || 0;
      document.getElementById('stat-tokens').textContent = formatNumber(stats.total_tokens || 0);
      document.getElementById('stat-success').textContent = `${Math.round((stats.success_rate || 0) * 100)}%`;

      // Update token bars
      const maxTokens = Math.max(
        stats.tokens_gemini || 0,
        stats.tokens_claude || 0,
        stats.tokens_codex || 0,
        stats.tokens_ollama || 0,
        1
      );

      updateTokenBar('gemini', stats.tokens_gemini || 0, maxTokens);
      updateTokenBar('claude', stats.tokens_claude || 0, maxTokens);
      updateTokenBar('codex', stats.tokens_codex || 0, maxTokens);
      updateTokenBar('ollama', stats.tokens_ollama || 0, maxTokens);
    }

    function updateTokenBar(model, count, max) {
      const pct = (count / max) * 100;
      document.getElementById(`token-${model}`).style.width = `${pct}%`;
      document.getElementById(`token-${model}-count`).textContent = formatNumber(count);
    }

    function formatNumber(n) {
      if (n >= 1000000) return `${(n/1000000).toFixed(1)}M`;
      if (n >= 1000) return `${(n/1000).toFixed(1)}K`;
      return n.toString();
    }

    // Add log entry
    function addLog(message, level = 'info') {
      const log = document.getElementById('event-log');
      const time = new Date().toTimeString().split(' ')[0];
      const entry = document.createElement('div');
      entry.className = 'log-entry';
      entry.innerHTML = `<span class="log-time">[${time}]</span><span class="log-${level}">${message}</span>`;
      log.insertBefore(entry, log.firstChild);

      // Limit log entries
      while (log.children.length > 100) {
        log.removeChild(log.lastChild);
      }
    }

    // Preview chain (render Mermaid without execution)
    async function previewChain() {
      const input = document.getElementById('chain-input').value;
      if (!input.trim()) {
        addLog('Please enter a chain definition', 'warn');
        return;
      }

      try {
        const container = document.getElementById('mermaid-diagram');
        container.innerHTML = input;
        container.removeAttribute('data-processed');
        await mermaid.run({ nodes: [container] });
        nodeStates = {};
        addLog('Chain preview rendered', 'info');
      } catch (e) {
        addLog(`Mermaid error: ${e.message}`, 'error');
      }
    }

    // Run chain
    async function runChain() {
      const input = document.getElementById('chain-input').value;
      if (!input.trim()) {
        addLog('Please enter a chain definition', 'warn');
        return;
      }

      // First preview
      await previewChain();

      // Then execute
      try {
        const response = await fetch('/api/run', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ mermaid: input })
        });

        const result = await response.json();

        if (result.error) {
          addLog(`Execution error: ${result.error}`, 'error');
        } else {
          addLog(`Chain submitted: ${result.chain_id}`, 'info');
        }
      } catch (e) {
        addLog(`Request error: ${e.message}`, 'error');
      }
    }

    // Load sample chains
    function loadSample(type) {
      const samples = {
        magi: `graph TD
    Start([Input]) --> C[LLM:gemini<br/>CASPER]
    Start --> B[LLM:claude<br/>BALTHASAR]
    Start --> M[LLM:codex<br/>MELCHIOR]
    C --> Q{Quorum:2}
    B --> Q
    M --> Q
    Q --> End([Consensus])`,
        pr: `graph TD
    Start([PR Code]) --> L[Tool:eslint]
    L --> T[Tool:tsc]
    T --> S[LLM:gemini<br/>Security]
    T --> Q[LLM:claude<br/>Quality]
    S --> V{Quorum:2}
    Q --> V
    V --> End([Verdict])`
      };

      document.getElementById('chain-input').value = samples[type] || '';
      addLog(`Loaded sample: ${type}`, 'info');
    }

    // Fetch stats
    async function fetchStats() {
      try {
        const response = await fetch('/api/stats');
        const stats = await response.json();
        updateStats(stats);
      } catch (e) {
        console.error('Failed to fetch stats:', e);
      }
    }

    // Initialize
    connectSSE();
    fetchStats();
    setInterval(fetchStats, 10000);
  </script>
</body>
</html>|}

(** {1 API Handlers} *)

(** Token accumulator type for stats aggregation *)
type tokens_by_model_acc = {
  gemini: int;
  claude: int;
  codex: int;
  ollama: int;
}

(** Get stats as JSON *)
let get_stats_json () =
  Chain_stats.enable ();
  let stats = Chain_stats.compute () in
  let tokens_by_model =
    List.fold_left (fun acc (model, count) ->
      match String.lowercase_ascii model with
      | m when String.length m >= 6 && String.sub m 0 6 = "gemini" ->
          { acc with gemini = acc.gemini + count }
      | m when String.length m >= 6 && String.sub m 0 6 = "claude" ->
          { acc with claude = acc.claude + count }
      | m when String.length m >= 5 && String.sub m 0 5 = "codex" ->
          { acc with codex = acc.codex + count }
      | _ -> { acc with ollama = acc.ollama + count }
    ) { gemini = 0; claude = 0; codex = 0; ollama = 0 } stats.tokens_by_model
  in
  Printf.sprintf {|{
  "total_chains": %d,
  "total_executions": %d,
  "total_tokens": %d,
  "tokens_gemini": %d,
  "tokens_claude": %d,
  "tokens_codex": %d,
  "tokens_ollama": %d,
  "success_rate": %.2f,
  "estimated_cost_usd": %.2f
}|}
    stats.total_chains
    stats.total_chains
    stats.total_tokens
    tokens_by_model.gemini
    tokens_by_model.claude
    tokens_by_model.codex
    tokens_by_model.ollama
    stats.success_rate
    stats.estimated_cost_usd

(** Get registered chains as JSON *)
let get_chains_json () =
  let chains = Chain_registry.list_all () in
  let chain_items = List.map (fun chain ->
    let id = chain.Chain_types.id in
    let node_count = List.length chain.Chain_types.nodes in
    Printf.sprintf {|{"id": "%s", "nodes": %d}|} id node_count
  ) chains in
  Printf.sprintf "[%s]" (String.concat ", " chain_items)

(** Stub exec_fn for LLM calls - returns placeholder response *)
let stub_exec_fn ~model ~prompt ?tools:_ () =
  (* In real implementation, this would call Tools_eio or similar *)
  let truncated = String.sub prompt 0 (min 50 (String.length prompt)) in
  let response = Printf.sprintf "[%s] Response to: %s..." model truncated in
  Ok response

(** Stub tool_exec for tool calls - returns placeholder response *)
let stub_tool_exec ~name ~args =
  (* In real implementation, this would call Tools_eio.execute *)
  let args_str = Yojson.Safe.to_string args in
  let response = Printf.sprintf "Tool '%s' executed with args: %s" name args_str in
  Ok response

(** Run chain from Mermaid DSL *)
let run_chain_json ~clock ~sw body =
  try
    let json = Yojson.Safe.from_string body in
    (* Use to_string_option to handle null/missing field gracefully *)
    match Yojson.Safe.Util.(json |> member "mermaid" |> to_string_option) with
    | None ->
        {|{"error": "Missing or null 'mermaid' field in request body"}|}
    | Some mermaid ->

    (* Parse Mermaid to chain *)
    match Chain_mermaid_parser.parse_chain mermaid with
    | Error e ->
        Printf.sprintf {|{"error": "Parse error: %s"}|} e
    | Ok chain ->
        let chain_id = chain.Chain_types.id in

        (* Register chain to registry for listing *)
        Chain_registry.register ~description:"Submitted via web dashboard" chain;

        (* Compile chain to execution plan *)
        (match Chain_compiler.compile chain with
        | Error e ->
            Printf.sprintf {|{"error": "Compile error: %s"}|} e
        | Ok plan ->
            (* Execute in background fiber *)
            Fiber.fork ~sw (fun () ->
              let timeout = chain.Chain_types.config.Chain_types.timeout in
              let trace = chain.Chain_types.config.Chain_types.trace in
              let _result = Chain_executor_eio.execute
                ~sw ~clock ~timeout ~trace
                ~exec_fn:stub_exec_fn
                ~tool_exec:stub_tool_exec
                plan in
              ()
            );

            Printf.sprintf {|{"chain_id": "%s", "status": "submitted"}|} chain_id)
  with
  | Yojson.Json_error msg ->
      Printf.sprintf {|{"error": "JSON error: %s"}|} msg
  | e ->
      Printf.sprintf {|{"error": "%s"}|} (Printexc.to_string e)

(** {1 SSE Event Streaming} *)

(** Mutable SSE client list *)
let sse_clients : (_ Flow.sink) list ref = ref []

(** Send SSE event to all clients *)
let broadcast_sse event_type data =
  let msg = Printf.sprintf "event: %s\ndata: %s\n\n" event_type data in
  sse_clients := List.filter (fun sink ->
    try
      Flow.copy_string msg sink;
      true
    with _ -> false
  ) !sse_clients

(** Setup telemetry listener for SSE broadcasting *)
let setup_telemetry_broadcast () =
  let handler = function
    | Chain_telemetry.NodeStart payload ->
        let data = Printf.sprintf {|{"node_id": "%s", "node_type": "%s"}|}
          payload.node_start_id
          payload.node_start_type in
        broadcast_sse "node_start" data
    | Chain_telemetry.NodeComplete payload ->
        let data = Printf.sprintf
          {|{"node_id": "%s", "duration_ms": %d, "tokens": %d}|}
          payload.node_complete_id
          payload.node_duration_ms
          payload.node_tokens.total_tokens in
        broadcast_sse "node_complete" data
    | Chain_telemetry.Error payload ->
        let data = Printf.sprintf {|{"node_id": "%s", "error": "%s"}|}
          payload.error_node_id
          (String.escaped payload.error_message) in
        broadcast_sse "node_error" data
    | Chain_telemetry.ChainComplete payload ->
        let data = Printf.sprintf
          {|{"chain_id": "%s", "duration_ms": %d, "tokens": %d}|}
          payload.complete_chain_id
          payload.complete_duration_ms
          payload.complete_tokens.total_tokens in
        broadcast_sse "chain_complete" data
    | _ -> ()
  in
  ignore (Chain_telemetry.subscribe handler)

(** {1 HTTP Router} *)

(** Parse HTTP request *)
let parse_request data =
  let lines = String.split_on_char '\n' data in
  match lines with
  | [] -> None
  | first_line :: rest ->
      let parts = String.split_on_char ' ' first_line in
      match parts with
      | method_ :: path :: _ ->
          (* Find body after empty line *)
          let rec find_body = function
            | [] -> ""
            | "" :: rest | "\r" :: rest -> String.concat "\n" rest
            | _ :: rest -> find_body rest
          in
          let body = find_body rest in
          Some (method_, path, body)
      | _ -> None

(** Handle HTTP request *)
let handle_request ~clock ~sw method_ path body =
  match method_, path with
  | "GET", "/" ->
      http_response dashboard_html

  | "GET", "/api/stats" ->
      http_response ~content_type:json_content_type (get_stats_json ())

  | "GET", "/api/chains" ->
      http_response ~content_type:json_content_type (get_chains_json ())

  | "POST", "/api/run" ->
      http_response ~content_type:json_content_type (run_chain_json ~clock ~sw body)

  | "GET", "/sse" ->
      (* SSE connection - return headers only, body will be streamed *)
      sse_headers ()

  | "OPTIONS", _ ->
      (* CORS preflight *)
      "HTTP/1.1 204 No Content\r\n\
       Access-Control-Allow-Origin: *\r\n\
       Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n\
       Access-Control-Allow-Headers: Content-Type\r\n\
       \r\n"

  | _ ->
      http_response ~status:404 ~content_type:json_content_type
        {|{"error": "Not found"}|}

(** {1 Server} *)

(** Handle a single connection *)
let handle_connection ~clock ~sw flow _addr =
  let buf = Cstruct.create 8192 in
  match Flow.single_read flow buf with
  | n ->
      let data = Cstruct.to_string ~len:n buf in
      begin match parse_request data with
      | Some (method_, path, body) ->
          let response = handle_request ~clock ~sw method_ path body in
          Flow.copy_string response flow;

          (* For SSE, keep connection open and add to clients *)
          if path = "/sse" then begin
            sse_clients := flow :: !sse_clients;
            (* Send initial stats *)
            let stats = get_stats_json () in
            let msg = Printf.sprintf "event: stats\ndata: %s\n\n" stats in
            Flow.copy_string msg flow;
            (* Keep connection alive - will be cleaned up when client disconnects *)
            Fiber.await_cancel ()
          end
      | None ->
          Flow.copy_string (http_response ~status:400 "Bad Request") flow
      end
  | exception End_of_file -> ()

(** Start the server *)
let run_server ~sw ~clock ~net ~port =
  let addr = `Tcp (Net.Ipaddr.V4.loopback, port) in
  let socket = Net.listen ~sw ~reuse_addr:true ~backlog:128 net addr in

  Printf.printf "Chain Engine Web Dashboard running at http://localhost:%d\n%!" port;
  Printf.printf "Press Ctrl+C to stop\n%!";

  (* Setup telemetry broadcasting *)
  setup_telemetry_broadcast ();

  (* Enable stats collection *)
  Chain_stats.enable ();

  (* Accept connections *)
  while true do
    Net.accept_fork ~sw socket ~on_error:(fun e ->
      Printf.eprintf "Connection error: %s\n%!" (Printexc.to_string e)
    ) (handle_connection ~clock ~sw)
  done

(** {1 CLI} *)

let () =
  let port = ref default_port in
  let specs = [
    ("--port", Arg.Set_int port, "Port to listen on (default: 8080)");
    ("-p", Arg.Set_int port, "Port to listen on (default: 8080)");
  ] in
  Arg.parse specs (fun _ -> ()) "Chain Engine Web Dashboard\n\nUsage: chain_dashboard_web [options]";

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  run_server
    ~sw
    ~clock:(Stdenv.clock env)
    ~net:(Stdenv.net env)
    ~port:!port

(** LLM-MCP Server - Eio/httpun-eio Implementation

    Pure Eio HTTP server using httpun-eio for MCP 2025-11-25.

    Architecture:
    - Server: httpun-eio (Eio native, Effect-based)
    - MCP Handler: Mcp_server_eio (pure Eio, chain.run enabled)
    - SSE: Native Eio streaming
*)

open Printf

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
  <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #1a1a2e; color: #eee; padding: 20px; }
    .header { display: flex; align-items: center; gap: 15px; margin-bottom: 20px; }
    .header h1 { font-size: 24px; }
    .status { display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #666; }
    .status.connected { background: #4ade80; animation: pulse 2s infinite; }
    @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
    .stats-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 15px; margin-bottom: 20px; height: 90px; }
    .card { background: linear-gradient(135deg, #16213e 0%, #1a1a2e 100%); border-radius: 12px; padding: 20px; border: 1px solid #2a2a4e; transition: transform 0.2s, box-shadow 0.2s; }
    .card:hover { transform: translateY(-2px); box-shadow: 0 8px 25px rgba(0,0,0,0.3); }
    .card-label { font-size: 11px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px; }
    .card-value { font-size: 32px; font-weight: bold; color: #4ade80; text-shadow: 0 0 20px rgba(74,222,128,0.3); }
    .card-value.warn { color: #fbbf24; text-shadow: 0 0 20px rgba(251,191,36,0.3); }
    .card-value.error { color: #f87171; text-shadow: 0 0 20px rgba(248,113,113,0.3); }
    .mermaid-container { background: linear-gradient(135deg, #16213e 0%, #1a1a2e 100%); border-radius: 12px; padding: 15px; margin-bottom: 20px; border: 1px solid #2a2a4e; height: 450px; overflow: hidden; }
    .mermaid-container.expanded { position: fixed; top: 20px; left: 20px; right: 20px; bottom: 20px; z-index: 1000; overflow: auto; height: auto; }
    .mermaid-container .header-row { display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; height: 32px; flex-shrink: 0; }
    .mermaid-container h2 { font-size: 12px; color: #4ade80; margin: 0; text-transform: uppercase; letter-spacing: 0.5px; flex-shrink: 0; }
    .mermaid-container .toggle-btn { background: #4ade80; color: #000; border: none; padding: 5px 10px; border-radius: 6px; cursor: pointer; font-size: 10px; font-weight: bold; flex-shrink: 0; }
    .mermaid-container .toggle-btn:hover { background: #22c55e; }
    .chain-tabs { display: flex; gap: 5px; flex: 1; justify-content: center; overflow-x: auto; max-width: 60%; }
    .chain-tab { background: #1a1a2e; color: #888; border: 1px solid #333; padding: 4px 8px; border-radius: 5px; cursor: pointer; font-size: 9px; white-space: nowrap; flex-shrink: 0; }
    .chain-tab:hover { background: #2a2a4e; color: #eee; }
    .chain-tab.active { background: #4ade80; color: #000; border-color: #4ade80; font-weight: bold; }
    .mermaid-container #mermaid-graph { background: #1a1a2e; padding: 10px; border-radius: 8px; height: calc(100% - 50px); overflow: auto; }
    .mermaid-container #mermaid-graph svg { max-width: 100%; height: auto; transform-origin: top left; }
    .mermaid-container #mermaid-graph svg text { font-size: 11px !important; }
    .mermaid-container #mermaid-graph svg .nodeLabel { font-size: 10px !important; max-width: 100px; overflow: hidden; text-overflow: ellipsis; }
    .mermaid-container #mermaid-graph svg .node rect, .mermaid-container #mermaid-graph svg .node polygon { rx: 4; ry: 4; }
    .mermaid-container #mermaid-graph svg .edgeLabel { font-size: 9px !important; }
    .mermaid-container .no-chain { color: #666; font-style: italic; text-align: center; padding: 40px; }
    .mermaid-container .node-legend { display: flex; gap: 20px; margin-top: 15px; font-size: 12px; }
    .mermaid-container .legend-item { display: flex; align-items: center; gap: 6px; }
    .mermaid-container .legend-dot { width: 12px; height: 12px; border-radius: 3px; }
    .mermaid-container .legend-dot.pending { background: #666; }
    .mermaid-container .legend-dot.running { background: #fbbf24; }
    .mermaid-container .legend-dot.complete { background: #4ade80; }
    .mermaid-container .legend-dot.error { background: #f87171; }
    .two-column { display: flex; gap: 20px; margin-bottom: 20px; height: 320px; }
    .two-column > * { flex: 1; min-width: 0; flex-shrink: 0; }
    .events { background: linear-gradient(135deg, #16213e 0%, #1a1a2e 100%); border-radius: 12px; padding: 20px; height: 100%; overflow-y: auto; border: 1px solid #2a2a4e; }
    .events h2 { font-size: 14px; margin-bottom: 15px; color: #4ade80; display: flex; align-items: center; gap: 8px; text-transform: uppercase; letter-spacing: 0.5px; }
    .events .empty-state { color: #555; font-style: italic; text-align: center; padding: 40px 20px; animation: pulse 3s ease-in-out infinite; }
    .events .empty-state .icon { font-size: 40px; margin-bottom: 12px; opacity: 0.6; }
    .event { display: flex; align-items: center; gap: 10px; padding: 12px; border-radius: 8px; margin-bottom: 8px; background: #1a1a2e; font-family: monospace; font-size: 13px; animation: fadeIn 0.3s ease; }
    @keyframes fadeIn { from { opacity: 0; transform: translateY(-10px); } to { opacity: 1; transform: translateY(0); } }
    .event-icon { font-size: 18px; }
    .event-type { color: #60a5fa; min-width: 110px; font-weight: 500; }
    .event-id { color: #a78bfa; min-width: 90px; font-size: 11px; }
    .event-time { color: #555; font-size: 10px; margin-left: auto; }
    .event-detail { color: #888; flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .event.chain_start { border-left: 3px solid #4ade80; background: linear-gradient(90deg, rgba(74,222,128,0.1) 0%, #1a1a2e 100%); }
    .event.chain_complete { border-left: 3px solid #22d3ee; background: linear-gradient(90deg, rgba(34,211,238,0.1) 0%, #1a1a2e 100%); }
    .event.node_start { border-left: 3px solid #fbbf24; }
    .event.node_complete { border-left: 3px solid #a78bfa; }
    .event.chain_error { border-left: 3px solid #f87171; background: linear-gradient(90deg, rgba(248,113,113,0.15) 0%, #1a1a2e 100%); }
    .history-panel { background: linear-gradient(135deg, #16213e 0%, #1a1a2e 100%); border-radius: 12px; padding: 20px; height: 100%; overflow-y: auto; min-width: 0; border: 1px solid #2a2a4e; }
    .history-panel h2 { font-size: 14px; margin-bottom: 15px; color: #60a5fa; display: flex; align-items: center; gap: 10px; text-transform: uppercase; letter-spacing: 0.5px; }
    .history-panel .empty-state { color: #555; font-style: italic; text-align: center; padding: 40px 20px; }
    .history-panel .empty-state .icon { font-size: 32px; margin-bottom: 10px; opacity: 0.5; }
    .history-item { display: flex; justify-content: space-between; align-items: center; padding: 12px; border-radius: 8px; margin-bottom: 8px; background: #1a1a2e; font-family: monospace; font-size: 12px; cursor: pointer; transition: all 0.2s ease; border: 1px solid transparent; }
    .history-item:hover { background: #2a2a4e; transform: translateX(4px); border-color: #333; }
    .history-item.chain_complete { border-left: 3px solid #4ade80; }
    .history-item.chain_error { border-left: 3px solid #f87171; }
    .history-item .chain-id { color: #60a5fa; font-weight: bold; }
    .replay-btn { background: #4f46e5; border: none; color: white; padding: 4px 8px; border-radius: 4px; cursor: pointer; font-size: 10px; transition: background 0.2s; }
    .replay-btn:hover { background: #6366f1; }
    .history-item .duration { color: #4ade80; }
    .history-item .timestamp { color: #666; font-size: 11px; }
    .history-item .tokens { color: #a78bfa; }
    .chart-panel { background: linear-gradient(135deg, #16213e 0%, #1a1a2e 100%); border-radius: 12px; padding: 15px; border: 1px solid #2a2a4e; height: 200px; }
    .chart-panel h2 { font-size: 12px; margin-bottom: 10px; color: #a78bfa; display: flex; align-items: center; gap: 10px; text-transform: uppercase; letter-spacing: 0.5px; }
    .chart-container { height: 120px; position: relative; }
    .chart-loading { display: flex; align-items: center; justify-content: center; height: 100%; color: #666; }
    .chart-loading::after { content: ''; width: 20px; height: 20px; border: 2px solid #333; border-top-color: #4ade80; border-radius: 50%; animation: spin 1s linear infinite; margin-left: 10px; }
    @keyframes spin { to { transform: rotate(360deg); } }
    .chart-error { display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; color: #666; }
    .chart-error .icon { font-size: 32px; margin-bottom: 10px; opacity: 0.5; }
    .chart-empty { display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; color: #555; }
    .chart-empty .icon { font-size: 32px; margin-bottom: 10px; opacity: 0.5; }
    .token-summary { display: flex; gap: 20px; margin-bottom: 15px; font-size: 12px; flex-wrap: wrap; }
    .token-summary .model-stat { display: flex; align-items: center; gap: 5px; padding: 4px 8px; background: #1a1a2e; border-radius: 4px; }
    .token-summary .model-dot { width: 10px; height: 10px; border-radius: 50%; }
    @media (max-width: 900px) { .stats-grid { grid-template-columns: repeat(2, 1fr); } }
    /* Tab Navigation */
    .tab-nav { display: flex; gap: 4px; margin-bottom: 20px; background: #16213e; padding: 4px; border-radius: 8px; width: fit-content; }
    .tab-btn { background: transparent; color: #888; border: none; padding: 10px 20px; border-radius: 6px; cursor: pointer; font-size: 13px; font-weight: 500; transition: all 0.2s; }
    .tab-btn:hover { color: #eee; background: #1a1a2e; }
    .tab-btn.active { background: #4ade80; color: #000; }
    .tab-content { display: none; }
    .tab-content.active { display: block; }
    /* Explorer Tab Styles */
    .explorer-container { background: #16213e; border-radius: 12px; padding: 20px; min-height: 500px; }
    .chain-group { margin-bottom: 16px; border: 1px solid #2a2a4e; border-radius: 8px; overflow: hidden; }
    .chain-group-header { display: flex; justify-content: space-between; align-items: center; padding: 12px 16px; background: #1a1a2e; cursor: pointer; transition: background 0.2s; }
    .chain-group-header:hover { background: #2a2a4e; }
    .chain-group-header .chain-name { font-weight: 600; color: #4ade80; font-size: 14px; }
    .chain-group-header .chain-stats { font-size: 12px; color: #888; }
    .chain-group-header .toggle-icon { color: #888; transition: transform 0.2s; }
    .chain-group.expanded .toggle-icon { transform: rotate(90deg); }
    .chain-runs { display: none; padding: 0; }
    .chain-group.expanded .chain-runs { display: block; }
    .run-item { padding: 12px 16px; border-top: 1px solid #2a2a4e; background: #16213e; }
    .run-item:hover { background: #1a1a2e; }
    .run-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px; }
    .run-header .run-id { font-size: 12px; color: #60a5fa; }
    .run-header .run-time { font-size: 11px; color: #666; }
    .run-header .run-status { font-size: 12px; }
    .run-header .run-status.success { color: #4ade80; }
    .run-header .run-status.error { color: #f87171; }
    .run-nodes { margin-top: 8px; padding-left: 12px; border-left: 2px solid #2a2a4e; }
    .node-item { padding: 6px 0; font-size: 12px; font-family: monospace; }
    .node-item .node-model { color: #a78bfa; }
    .node-item .node-prompt { color: #888; max-width: 400px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .node-item .node-status { margin-left: 8px; }
    .node-detail { margin-top: 8px; padding: 10px; background: #1a1a2e; border-radius: 6px; font-size: 11px; }
    .node-detail-label { color: #4ade80; font-weight: 500; margin-bottom: 4px; }
    .node-detail-content { color: #ccc; white-space: pre-wrap; max-height: 100px; overflow-y: auto; }
    .explorer-empty { text-align: center; padding: 60px 20px; color: #666; }
    .explorer-empty .icon { font-size: 48px; margin-bottom: 16px; opacity: 0.5; }
  </style>
</head>
<body>
  <div class="header">
    <span class="status" id="status"></span>
    <h1>🐫 Chain Engine Dashboard</h1>
  </div>

  <!-- Tab Navigation -->
  <div class="tab-nav">
    <button class="tab-btn active" onclick="switchTab('overview')">📊 Overview</button>
    <button class="tab-btn" onclick="switchTab('explorer')">🔍 Explorer</button>
  </div>

  <!-- Overview Tab -->
  <div id="tab-overview" class="tab-content active">

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
      <div class="card-label">Total Tokens</div>
      <div class="card-value" id="total-tokens">-</div>
    </div>
  </div>

  <div class="mermaid-container" id="mermaid-container">
    <div class="header-row">
      <h2>🔄 Active Chains</h2>
      <div id="chain-tabs" class="chain-tabs"></div>
      <button class="toggle-btn" id="toggle-direction" onclick="toggleDirection()">↔ LR</button>
      <button class="toggle-btn" id="toggle-expand" onclick="toggleExpand()">⛶ Expand</button>
    </div>
    <div id="mermaid-graph"><div class="no-chain">Waiting for chain execution...</div></div>
    <div class="node-legend" id="node-legend" style="display:none;">
      <div class="legend-item"><div class="legend-dot pending"></div> Pending</div>
      <div class="legend-item"><div class="legend-dot running"></div> Running</div>
      <div class="legend-item"><div class="legend-dot complete"></div> Complete</div>
      <div class="legend-item"><div class="legend-dot error"></div> Error</div>
    </div>
  </div>

  <div class="two-column">
    <div class="events">
      <h2>⚡ Live Events <span id="event-count" style="font-size:11px;color:#555;font-weight:normal;"></span></h2>
      <div id="event-list">
        <div class="empty-state">
          <div class="icon">📡</div>
          <div>Waiting for chain events...</div>
          <div style="font-size:11px;margin-top:5px;">Events will appear here in real-time</div>
        </div>
      </div>
    </div>

    <div class="history-panel">
      <h2>📜 History <button class="toggle-btn" onclick="loadHistory()">↻ Refresh</button></h2>
      <div id="history-list">
        <div class="empty-state">
          <div class="icon">📋</div>
          <div>No chain history yet</div>
        </div>
      </div>
    </div>
  </div>

  <div class="chart-panel">
    <h2>📊 Token Usage <button class="toggle-btn" onclick="refreshTokenChart()">↻ Refresh</button></h2>
    <div id="token-summary" class="token-summary"></div>
    <div class="chart-container">
      <canvas id="token-chart"></canvas>
    </div>
  </div>

  </div><!-- End Overview Tab -->

  <!-- Explorer Tab -->
  <div id="tab-explorer" class="tab-content">
    <div class="explorer-container">
      <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;">
        <h2 style="font-size:14px;color:#4ade80;margin:0;">🔍 Chain Explorer</h2>
        <button class="toggle-btn" onclick="refreshExplorer()">↻ Refresh</button>
      </div>
      <div id="explorer-list">
        <div class="explorer-empty">
          <div class="icon">📂</div>
          <div>Loading chain history...</div>
        </div>
      </div>
    </div>
  </div><!-- End Explorer Tab -->

  <script>
    // Global scope functions for onclick handlers
    window.switchChain = null;  // Will be set after DOMContentLoaded
    function toggleExpand() {
      const container = document.getElementById('mermaid-container');
      const btn = document.getElementById('toggle-expand');
      container.classList.toggle('expanded');
      btn.textContent = container.classList.contains('expanded') ? '✕ Close' : '⛶ Expand';
    }

    // Tab switching
    function switchTab(tabId) {
      document.querySelectorAll('.tab-btn').forEach(btn => btn.classList.remove('active'));
      document.querySelectorAll('.tab-content').forEach(content => content.classList.remove('active'));
      document.querySelector('[onclick*=\"' + tabId + '\"]').classList.add('active');
      document.getElementById('tab-' + tabId).classList.add('active');
      if (tabId === 'explorer') refreshExplorer();
    }

    // Explorer: Parse nodes from mermaid DSL
    function parseNodesFromDsl(dsl) {
      if (!dsl) return [];
      const nodes = [];
      const nodeRegex = /([a-zA-Z_][a-zA-Z0-9_]*)\[\"([^\"]+)\"\]/g;
      let match;
      while ((match = nodeRegex.exec(dsl)) !== null) {
        const [_, id, content] = match;
        // Parse: LLM:model 'prompt' or Tool:name {...}
        const llmMatch = content.match(/LLM:([^\s']+)\s*'([^']*)'?/);
        const toolMatch = content.match(/Tool:([^\s{]+)/);
        if (llmMatch) {
          nodes.push({ id, type: 'llm', model: llmMatch[1], prompt: llmMatch[2] || '' });
        } else if (toolMatch) {
          nodes.push({ id, type: 'tool', name: toolMatch[1] });
        } else {
          nodes.push({ id, type: 'unknown', content: content.substring(0, 50) });
        }
      }
      return nodes;
    }

    // Explorer: Render chain groups
    function refreshExplorer() {
      const listEl = document.getElementById('explorer-list');
      listEl.innerHTML = '<div class="explorer-empty"><div class="icon">⏳</div><div>Loading...</div></div>';

      fetch('/chain/history').then(r => r.json()).then(history => {
        // Sort by timestamp ascending
        history.sort((a, b) => (a.timestamp || 0) - (b.timestamp || 0));

        // Collect mermaid_dsl from chain_start events
        const dslMap = {};
        history.forEach(h => {
          if (h.event === 'chain_start' && h.chain_id && h.mermaid_dsl) {
            dslMap[h.chain_id + '_' + h.timestamp] = h.mermaid_dsl;
          }
        });

        // Group by chain_id
        const chainGroups = {};
        history.forEach(h => {
          if (h.event === 'chain_complete' || h.event === 'chain_error') {
            const chainId = h.chain_id || h.node_id || 'unknown';
            if (!chainGroups[chainId]) chainGroups[chainId] = { runs: [], totalTokens: 0 };
            // Find matching DSL (closest timestamp before this complete event)
            let matchedDsl = null;
            Object.keys(dslMap).forEach(key => {
              if (key.startsWith(chainId + '_')) {
                const ts = parseFloat(key.split('_')[1]);
                if (ts <= h.timestamp) matchedDsl = dslMap[key];
              }
            });
            const tokens = h.tokens ? (typeof h.tokens === 'object' ? (h.tokens.total_tokens || 0) : h.tokens) : 0;
            chainGroups[chainId].runs.push({
              ...h,
              parsedNodes: parseNodesFromDsl(matchedDsl),
              mermaidDsl: matchedDsl
            });
            chainGroups[chainId].totalTokens += tokens;
          }
        });

        if (Object.keys(chainGroups).length === 0) {
          listEl.innerHTML = '<div class="explorer-empty"><div class="icon">📂</div><div>No chain history yet</div><div style="font-size:11px;margin-top:8px;color:#555;">Run some chains to see them here</div></div>';
          return;
        }

        // Render groups (sorted by most recent run)
        const sortedChains = Object.entries(chainGroups)
          .sort((a, b) => {
            const aLatest = a[1].runs[a[1].runs.length - 1]?.timestamp || 0;
            const bLatest = b[1].runs[b[1].runs.length - 1]?.timestamp || 0;
            return bLatest - aLatest;
          });

        listEl.innerHTML = sortedChains.map(([chainId, data]) => {
          const runCount = data.runs.length;
          const latestRun = data.runs[data.runs.length - 1];
          const isError = latestRun?.event === 'chain_error';

          const runsHtml = data.runs.slice().reverse().map((run, idx) => {
            const runNum = runCount - idx;
            const date = new Date(run.timestamp * 1000);
            const duration = run.duration_ms ? (run.duration_ms / 1000).toFixed(1) + 's' : '-';
            const tokens = run.tokens ? (typeof run.tokens === 'object' ? (run.tokens.total_tokens || 0) : run.tokens) : 0;
            const isErr = run.event === 'chain_error';
            const nodesExecuted = run.nodes_executed || 0;

            const nodesHtml = run.parsedNodes.length > 0
              ? '<div class=\"run-nodes\">' + run.parsedNodes.map(n => {
                  if (n.type === 'llm') {
                    const shortPrompt = n.prompt.length > 60 ? n.prompt.substring(0, 60) + '...' : n.prompt;
                    return '<div class=\"node-item\"><span class=\"node-model\">🤖 ' + n.model + '</span> <span class=\"node-prompt\">\"' + shortPrompt + '\"</span></div>';
                  } else if (n.type === 'tool') {
                    return '<div class=\"node-item\"><span class=\"node-model\">🔧 ' + n.name + '</span></div>';
                  }
                  return '<div class=\"node-item\"><span class=\"node-model\">❓ ' + n.content + '</span></div>';
                }).join('') + '</div>'
              : (nodesExecuted === 0 ? '<div style=\"font-size:11px;color:#666;margin-top:4px;\">(0 nodes executed)</div>' : '');

            return '<div class=\"run-item\">' +
              '<div class=\"run-header\">' +
                '<span class=\"run-id\">#' + runNum + '</span>' +
                '<span class=\"run-status ' + (isErr ? 'error' : 'success') + '\">' + (isErr ? '❌' : '✅') + ' ' + duration + ' • ' + tokens + ' tok</span>' +
                '<span class=\"run-time\">' + date.toLocaleDateString() + ' ' + date.toLocaleTimeString() + '</span>' +
              '</div>' +
              nodesHtml +
            '</div>';
          }).join('');

          return '<div class=\"chain-group ' + (runCount <= 3 ? 'expanded' : '') + '\" onclick=\"this.classList.toggle(\x27expanded\x27)\">' +
            '<div class=\"chain-group-header\">' +
              '<span class=\"chain-name\">' + (isError ? '❌ ' : '📂 ') + chainId + '</span>' +
              '<span class=\"chain-stats\">' + runCount + ' run' + (runCount > 1 ? 's' : '') + ' • ' + data.totalTokens + ' tok</span>' +
              '<span class=\"toggle-icon\">▶</span>' +
            '</div>' +
            '<div class=\"chain-runs\" onclick=\"event.stopPropagation()\">' + runsHtml + '</div>' +
          '</div>';
        }).join('');
      }).catch(err => {
        listEl.innerHTML = '<div class="explorer-empty"><div class="icon">❌</div><div>Failed to load history</div><div style="font-size:11px;margin-top:8px;color:#f87171;">' + err.message + '</div></div>';
      });
    }

    document.addEventListener('DOMContentLoaded', function() {
    if (typeof mermaid !== 'undefined') {
      mermaid.initialize({
        startOnLoad: false,
        theme: 'dark',
        themeVariables: {
          primaryColor: '#16213e',
          primaryTextColor: '#eee',
          lineColor: '#4ade80',
          fontSize: '12px',
          fontFamily: 'ui-monospace, SFMono-Regular, Consolas, monospace'
        },
        flowchart: {
          nodeSpacing: 30,
          rankSpacing: 40,
          curve: 'basis',
          htmlLabels: true,
          useMaxWidth: true,
          padding: 10,
          wrappingWidth: 150
        }
      });
    }

    const icons = { chain_start: '▶', chain_complete: '✓', node_start: '→', node_complete: '●', chain_error: '✗', ping: '♥' };
    const eventList = document.getElementById('event-list');
    const statusEl = document.getElementById('status');
    const mermaidEl = document.getElementById('mermaid-graph');
    const legendEl = document.getElementById('node-legend');
    let eventSource;
    // Multi-chain support
    let activeChains = {};  // chain_id -> { mermaid, nodeStates, startTime, fullTexts }
    let currentChainId = null;
    let graphDirection = 'LR';  // LR (left-right) or TB (top-bottom)

    const STATE_COLORS = { pending: '#666', running: '#fbbf24', complete: '#4ade80', error: '#f87171' };
    let renderCounter = 0;

    function toggleDirection() {
      graphDirection = graphDirection === 'LR' ? 'TB' : 'LR';
      const btn = document.getElementById('toggle-direction');
      btn.textContent = graphDirection === 'LR' ? '↔ LR' : '↕ TB';
      // Re-render current chain with new direction
      if (currentChainId && activeChains[currentChainId]) {
        const chain = activeChains[currentChainId];
        renderMermaid(chain.mermaid, currentChainId);
        Object.entries(chain.nodeStates).forEach(([nodeId, state]) => {
          updateNodeStyle(nodeId, state);
        });
      }
    }

    // Truncate long text in Mermaid DSL and apply direction
    function truncateMermaidDsl(dsl, maxLen = 25) {
      const fullTexts = {};  // nodeId -> fullText (for tooltip)
      let truncated = dsl.replace(/\["([^"]+)"\]/g, (match, text) => {
        // Extract node id from context (before the bracket)
        const nodeMatch = dsl.substring(0, dsl.indexOf(match)).match(/(\w+)\s*$/);
        const nodeId = nodeMatch ? nodeMatch[1] : 'node';
        fullTexts[nodeId] = text;
        // Shorten LLM model names (ollama:qwen3:1.7b -> qwen3:1.7b)
        let shortText = text.replace(/LLM:ollama:([^']+)/, 'ollama:$1')
                            .replace(/LLM:([a-z]+)\s*'/, '$1: \x27');
        if (shortText.length > maxLen) {
          return '["' + shortText.substring(0, maxLen) + '..."]';
        }
        return '["' + shortText + '"]';
      });
      // Apply graph direction (replace LR/TB/RL/BT with current direction)
      truncated = truncated.replace(/graph\s+(LR|TB|RL|BT)/i, 'graph ' + graphDirection);
      return { dsl: truncated, fullTexts };
    }

    async function renderMermaid(dsl, chainId) {
      const { dsl: truncatedDsl, fullTexts } = truncateMermaidDsl(dsl);

      if (!activeChains[chainId]) {
        activeChains[chainId] = { mermaid: dsl, nodeStates: {}, startTime: Date.now(), fullTexts };
      }
      activeChains[chainId].mermaid = dsl;
      activeChains[chainId].fullTexts = fullTexts;
      currentChainId = chainId;

      updateChainTabs();
      legendEl.style.display = 'flex';

      if (typeof mermaid === 'undefined') {
        mermaidEl.innerHTML = '<pre style="color:#f87171;font-size:12px;">Mermaid not loaded</pre>';
        return;
      }
      try {
        const { svg } = await mermaid.render('mermaid-svg-' + (++renderCounter), truncatedDsl);
        mermaidEl.innerHTML = svg;
        const svgEl = mermaidEl.querySelector('svg');
        if (svgEl) {
          svgEl.style.minWidth = '100%';
          svgEl.style.height = 'auto';
        }
        // Add tooltips for truncated nodes
        addNodeTooltips(fullTexts);
        // Build node ID map for state updates
        if (activeChains[chainId]) {
          activeChains[chainId].nodeIdMap = buildNodeIdMap();
        }
      } catch (e) {
        console.error('Mermaid render error:', e);
        mermaidEl.innerHTML = '<pre style="color:#f87171;font-size:12px;">Render error: ' + e.message + '</pre>';
      }
    }

    function addNodeTooltips(fullTexts) {
      const svg = mermaidEl.querySelector('svg');
      if (!svg) return;
      Object.entries(fullTexts).forEach(([nodeId, text]) => {
        const node = svg.querySelector('[id*="' + nodeId + '"]');
        if (node) {
          const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
          title.textContent = text;
          node.appendChild(title);
        }
      });
    }

    // Build map of Mermaid node IDs to SVG element IDs
    function buildNodeIdMap() {
      const svg = mermaidEl.querySelector('svg');
      if (!svg) return {};
      const nodeMap = {};

      // Pattern 1: flowchart-nodeId-N (most common)
      // Use greedy match that captures everything between 'flowchart-' and the last '-N'
      svg.querySelectorAll('[id^="flowchart-"]').forEach(el => {
        // Match: flowchart-<node_id>-<number>
        // Node IDs can contain hyphens, so capture everything before the last -number
        const match = el.id.match(/^flowchart-(.+)-(\d+)$/);
        if (match) {
          const mermaidId = match[1];
          if (!nodeMap[mermaidId]) nodeMap[mermaidId] = el.id;
        }
      });

      // Pattern 2: node-nodeId (alternative pattern)
      svg.querySelectorAll('g.node[id]').forEach(el => {
        const id = el.getAttribute('id');
        if (id && !id.startsWith('flowchart-')) {
          nodeMap[id] = id;
        }
      });

      // Pattern 3: data-id attribute (newer Mermaid versions)
      svg.querySelectorAll('[data-id]').forEach(el => {
        const dataId = el.getAttribute('data-id');
        if (dataId && !nodeMap[dataId]) {
          nodeMap[dataId] = el.id || dataId;
        }
      });

      // Pattern 4: Aria-label based lookup (Mermaid adds aria-label with node content)
      svg.querySelectorAll('[aria-label]').forEach(el => {
        const label = el.getAttribute('aria-label');
        // Try to find node ID from parent group
        let parent = el.closest('[id^="flowchart-"]');
        if (parent && label) {
          const match = parent.id.match(/^flowchart-(.+)-(\d+)$/);
          if (match) nodeMap[match[1]] = parent.id;
        }
      });

      console.log('Node ID map built:', Object.keys(nodeMap).length, 'nodes:', JSON.stringify(nodeMap));
      return nodeMap;
    }

    function updateChainTabs() {
      const tabsEl = document.getElementById('chain-tabs');
      if (!tabsEl) return;
      const chainIds = Object.keys(activeChains);
      tabsEl.innerHTML = chainIds.map((id, index) => {
        const chain = activeChains[id];
        const isActive = id === currentChainId;
        const elapsed = ((Date.now() - chain.startTime) / 1000).toFixed(0);
        const shortId = id.length > 15 ? id.substring(0, 12) + '...' : id;
        return '<button class="chain-tab' + (isActive ? ' active' : '') + '" onclick="switchChain(\'' + id + '\')" title="' + id + '">' +
          '#' + (index + 1) + ' ' + shortId + ' (' + elapsed + 's)</button>';
      }).join('');
    }

    function switchChain(chainId) {
      if (!activeChains[chainId]) return;
      currentChainId = chainId;
      const chain = activeChains[chainId];
      renderMermaid(chain.mermaid, chainId);
      // Restore node states
      Object.entries(chain.nodeStates).forEach(([nodeId, state]) => {
        updateNodeStyle(nodeId, state);
      });
      updateChainTabs();
    }

    function updateNodeStyle(nodeId, state) {
      const svg = mermaidEl.querySelector('svg');
      if (!svg) { console.log('No SVG found'); return; }

      // Try using nodeIdMap first (most reliable)
      const chain = activeChains[currentChainId];
      const nodeIdMap = chain ? chain.nodeIdMap : null;

      let found = false;

      // Method 1: Use pre-built nodeIdMap
      if (nodeIdMap && nodeIdMap[nodeId]) {
        const svgId = nodeIdMap[nodeId];
        const el = svg.getElementById(svgId);
        if (el) {
          // Apply to rect/polygon inside the group
          const shapes = el.querySelectorAll('rect, polygon, circle, ellipse');
          shapes.forEach(shape => {
            shape.style.fill = STATE_COLORS[state];
            shape.style.transition = 'fill 0.3s ease';
            shape.style.stroke = state === 'running' ? '#fff' : '#333';
            shape.style.strokeWidth = state === 'running' ? '3px' : '1px';
          });
          // Also try direct rect/polygon if it's the element itself
          if (['rect', 'polygon', 'circle', 'ellipse'].includes(el.tagName.toLowerCase())) {
            el.style.fill = STATE_COLORS[state];
            el.style.transition = 'fill 0.3s ease';
          }
          found = shapes.length > 0 || ['rect', 'polygon'].includes(el.tagName.toLowerCase());
          if (found) console.log('✓ Node styled via map:', nodeId, '->', svgId, 'state:', state);
        }
      }

      // Method 2: Fallback to selector-based matching
      if (!found) {
        const normalizedId = nodeId.replace(/[^a-zA-Z0-9_]/g, '').toLowerCase();
        const selectors = [
          '[id*="flowchart-' + nodeId + '-"] rect',
          '[id*="flowchart-' + nodeId + '-"] polygon',
          '[id*="flowchart-' + normalizedId + '-"] rect',
          '[id*="flowchart-' + normalizedId + '-"] polygon',
          'g[id*="' + nodeId + '"] rect',
          'g[id*="' + nodeId + '"] polygon'
        ];

        for (const sel of selectors) {
          try {
            const nodes = svg.querySelectorAll(sel);
            nodes.forEach(node => {
              node.style.fill = STATE_COLORS[state];
              node.style.transition = 'fill 0.3s ease';
              node.style.stroke = state === 'running' ? '#fff' : '#333';
              node.style.strokeWidth = state === 'running' ? '3px' : '1px';
              found = true;
            });
            if (found) {
              console.log('✓ Node styled via selector:', nodeId, 'sel:', sel, 'state:', state);
              break;
            }
          } catch(e) { /* invalid selector */ }
        }
      }

      if (!found) {
        // Debug: list available mappings and SVG IDs
        const mapKeys = nodeIdMap ? Object.keys(nodeIdMap).join(', ') : 'no map';
        const allIds = Array.from(svg.querySelectorAll('[id*="flowchart-"]')).map(el => el.id).slice(0, 5);
        console.log('✗ Node not found:', nodeId, '| Map keys:', mapKeys, '| SVG IDs:', allIds.join(', '));
      }
    }

    function updateNodeState(chainId, nodeId, state) {
      if (!activeChains[chainId]) return;
      activeChains[chainId].nodeStates[nodeId] = state;
      if (chainId === currentChainId) {
        updateNodeStyle(nodeId, state);
      }
    }

    function connect() {
      eventSource = new EventSource('/chain/events');
      eventSource.onopen = () => statusEl.classList.add('connected');
      eventSource.onerror = () => {
        statusEl.classList.remove('connected');
        setTimeout(connect, 3000);
      };

      eventSource.addEventListener('chain_start', e => {
        const data = JSON.parse(e.data);
        const chainId = data.chain_id || 'unknown';
        console.log('chain_start received:', chainId);
        if (data.mermaid_dsl) {
          renderMermaid(data.mermaid_dsl, chainId);
        }
        addEvent('chain_start', data);
      });

      eventSource.addEventListener('node_start', e => {
        const data = JSON.parse(e.data);
        const chainId = data.chain_id || currentChainId;
        console.log('node_start:', data.node_id, 'chain:', chainId);
        updateNodeState(chainId, data.node_id, 'running');
        addEvent('node_start', data);
      });

      eventSource.addEventListener('node_complete', e => {
        const data = JSON.parse(e.data);
        const chainId = data.chain_id || currentChainId;
        updateNodeState(chainId, data.node_id, 'complete');
        addEvent('node_complete', data);
      });

      eventSource.addEventListener('chain_complete', e => {
        const data = JSON.parse(e.data);
        const chainId = data.chain_id || currentChainId;
        // Mark chain as complete but keep in activeChains for viewing
        if (activeChains[chainId]) {
          activeChains[chainId].complete = true;
        }
        updateChainTabs();
        addEvent('chain_complete', data);
      });

      eventSource.addEventListener('chain_error', e => {
        const data = JSON.parse(e.data);
        const chainId = data.chain_id || currentChainId;
        if (data.node_id) updateNodeState(chainId, data.node_id, 'error');
        addEvent('chain_error', data);
      });
    }

    let eventCounter = 0;
    function addEvent(type, data) {
      // Clear empty state on first event
      const emptyState = eventList.querySelector('.empty-state');
      if (emptyState) emptyState.remove();

      eventCounter++;
      const countEl = document.getElementById('event-count');
      if (countEl) countEl.textContent = '(' + eventCounter + ')';

      const div = document.createElement('div');
      div.className = 'event ' + type;
      const id = data.chain_id || data.node_id || '';
      const detail = data.duration_ms ? (data.duration_ms / 1000).toFixed(1) + 's' : (data.message || '');
      const time = new Date().toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit', second: '2-digit' });
      div.innerHTML = '<span class="event-icon">' + icons[type] + '</span><span class="event-type">' + type.replace('_', ' ') + '</span><span class="event-id">' + id + '</span><span class="event-detail">' + detail + '</span><span class="event-time">' + time + '</span>';
      eventList.insertBefore(div, eventList.firstChild);
      if (eventList.children.length > 50) eventList.removeChild(eventList.lastChild);
    }

    function fetchStats() {
      // Try memory stats first, then fallback to history-based stats
      fetch('/chain/stats').then(r => r.json()).then(s => {
        // Only update if we have real data (not all zeros)
        if (s.total_chains > 0 || s.total_tokens > 0) {
          document.getElementById('total-chains').textContent = s.total_chains;
          document.getElementById('success-rate').textContent = (s.success_rate * 100).toFixed(0) + '%';
          document.getElementById('avg-duration').textContent = (s.avg_duration_ms / 1000).toFixed(1) + 's';
          document.getElementById('total-tokens').textContent = s.total_tokens || 0;
          document.getElementById('success-rate').classList.toggle('warn', s.success_rate < 0.9);
        }
      }).catch(() => {});
    }

    // Compute stats from history data (persisted across restarts)
    function computeStatsFromHistory(history) {
      let totalChains = 0, successCount = 0, failCount = 0, totalDuration = 0, totalTokens = 0;
      history.forEach(h => {
        if (h.event === 'chain_complete') {
          totalChains++;
          successCount++;
          if (h.duration_ms) totalDuration += h.duration_ms;
          if (h.tokens) {
            totalTokens += typeof h.tokens === 'object' ?
              (h.tokens.total_tokens || h.tokens.prompt_tokens + h.tokens.completion_tokens || 0) : (h.tokens || 0);
          }
        } else if (h.event === 'chain_error') {
          totalChains++;
          failCount++;
        }
      });
      const successRate = totalChains > 0 ? successCount / totalChains : 1;
      const avgDuration = successCount > 0 ? totalDuration / successCount : 0;
      return { totalChains, successRate, avgDuration, totalTokens };
    }

    // Store mermaid DSL from history for replay
    const historyMermaid = {};

    function loadHistory() {
      fetch('/chain/history').then(r => r.json()).then(history => {
        const listEl = document.getElementById('history-list');
        if (!history || history.length === 0) {
          listEl.innerHTML = '<div class="empty-state"><div class="icon">📋</div><div>No chain history yet</div></div>';
          return;
        }

        // Update stats from history (fallback for when memory stats are empty)
        const stats = computeStatsFromHistory(history);
        const currentChains = document.getElementById('total-chains').textContent;
        if (currentChains === '-' || currentChains === '0') {
          document.getElementById('total-chains').textContent = stats.totalChains;
          document.getElementById('success-rate').textContent = (stats.successRate * 100).toFixed(0) + '%';
          document.getElementById('avg-duration').textContent = (stats.avgDuration / 1000).toFixed(1) + 's';
          document.getElementById('total-tokens').textContent = stats.totalTokens;
          document.getElementById('success-rate').classList.toggle('warn', stats.successRate < 0.9);
        }
        // Store mermaid_dsl from chain_start events
        history.forEach(h => {
          if (h.event === 'chain_start' && h.chain_id && h.mermaid_dsl) {
            historyMermaid[h.chain_id] = h.mermaid_dsl;
          }
        });
        // Sort by timestamp ascending so newest entry wins when deduplicating
        history.sort((a, b) => (a.timestamp || 0) - (b.timestamp || 0));
        // Group by chain_id, show only complete/error events (newest overwrites oldest)
        const chains = {};
        history.forEach(h => {
          if (h.event === 'chain_complete' || h.event === 'chain_error') {
            chains[h.chain_id || h.node_id] = h;
          }
        });
        listEl.innerHTML = Object.values(chains).slice(0, 50).map(h => {
          const date = new Date(h.timestamp * 1000);
          const timeStr = date.toLocaleTimeString();
          const dateStr = date.toLocaleDateString();
          const isError = h.event === 'chain_error';
          const duration = h.duration_ms ? (h.duration_ms / 1000).toFixed(1) + 's' : '-';
          const tokens = h.tokens ? (typeof h.tokens === 'object' ? (h.tokens.total_tokens || h.tokens.prompt_tokens + h.tokens.completion_tokens || 0) : h.tokens) : 0;
          const chainId = h.chain_id || h.node_id || 'unknown';
          const hasMermaid = historyMermaid[chainId] ? true : false;
          return '<div class="history-item ' + h.event + '">' +
            '<div onclick="showHistoryDetail(\'' + chainId + '\')"><span class="chain-id">' + chainId + '</span></div>' +
            '<div class="duration">' + (isError ? '❌' : '✓') + ' ' + duration + '</div>' +
            '<div class="tokens">' + tokens + ' tok</div>' +
            '<div class="timestamp">' + dateStr + ' ' + timeStr + '</div>' +
            (hasMermaid ? '<button class="replay-btn" onclick="event.stopPropagation();replayChain(\'' + chainId + '\')" title="Replay">▶</button>' : '') +
          '</div>';
        }).join('');
      }).catch(() => {
        document.getElementById('history-list').innerHTML = '<div style="color:#f87171;">Failed to load history</div>';
      });
    }

    // Replay a chain from history
    function replayChain(chainId) {
      const mermaid = historyMermaid[chainId];
      if (!mermaid) {
        alert('Cannot replay: Mermaid DSL not found');
        return;
      }
      // Call chain.run with the stored mermaid DSL
      fetch('/mcp', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          jsonrpc: '2.0',
          id: Date.now(),
          method: 'tools/call',
          params: {
            name: 'chain.run',
            arguments: { mermaid: mermaid, input: '{}' }
          }
        })
      }).then(r => r.json()).then(res => {
        console.log('Chain replay started:', res);
      }).catch(err => {
        console.error('Replay failed:', err);
        alert('Replay failed: ' + err.message);
      });
    }

    function showHistoryDetail(chainId) {
      console.log('Show detail for:', chainId);
      // Fetch full history and find the chain_start event with mermaid_dsl
      fetch('/chain/history').then(r => r.json()).then(history => {
        // Find the chain_start event for this chainId
        const chainStart = history.find(h =>
          h.event === 'chain_start' && h.chain_id === chainId
        );
        if (chainStart && chainStart.mermaid_dsl) {
          // Render the Mermaid diagram
          renderMermaid(chainStart.mermaid_dsl, chainId + '_history');
          // Update tabs to show this is a history view
          const tabsEl = document.getElementById('chain-tabs');
          if (tabsEl) {
            tabsEl.innerHTML = '<button class="chain-tab active">📜 ' + chainId + ' (history)</button>' +
              '<button class="chain-tab" onclick="loadHistory()">← Back to History</button>';
          }
          // Find the completion event to show final state
          const chainComplete = history.find(h =>
            (h.event === 'chain_complete' || h.event === 'chain_error') && h.chain_id === chainId
          );
          if (chainComplete) {
            // Show summary in events panel
            const eventsEl = document.getElementById('event-list');
            if (eventsEl) {
              const duration = chainComplete.duration_ms ? (chainComplete.duration_ms / 1000).toFixed(2) + 's' : '-';
              const tokens = chainComplete.tokens ? (typeof chainComplete.tokens === 'object' ? (chainComplete.tokens.total_tokens || chainComplete.tokens.prompt_tokens + chainComplete.tokens.completion_tokens || 0) : chainComplete.tokens) : 0;
              eventsEl.innerHTML = '<div class="event ' + chainComplete.event + '">' +
                '<strong>' + (chainComplete.event === 'chain_error' ? '❌ Error' : '✓ Complete') + '</strong><br>' +
                'Duration: ' + duration + '<br>' +
                'Tokens: ' + tokens + '<br>' +
                (chainComplete.message ? 'Error: ' + chainComplete.message : '') +
              '</div>';
            }
          }
        } else {
          alert('Mermaid diagram not available for this chain');
        }
      }).catch(err => {
        console.error('Failed to load history:', err);
        alert('Failed to load chain details');
      });
    }

    // Token usage chart
    let tokenChart = null;
    const modelColors = {
      gemini: '#4285f4',
      claude: '#f8a500',
      codex: '#10a37f',
      ollama: '#a78bfa',
      unknown: '#666'
    };

    function refreshTokenChart() {
      const summaryEl = document.getElementById('token-summary');
      const chartContainer = document.querySelector('.chart-container');

      // Show loading state
      summaryEl.innerHTML = '<div class="chart-loading">Loading token data</div>';

      fetch('/chain/history').then(r => r.json()).then(history => {
        // Aggregate token usage by model from chain_complete events
        const byModel = {};
        const byHour = {};
        let totalTokens = 0;

        history.forEach(h => {
          if (h.event === 'chain_complete' && h.tokens) {
            const tokens = typeof h.tokens === 'object' ? (h.tokens.total_tokens || (h.tokens.prompt_tokens || 0) + (h.tokens.completion_tokens || 0) || 0) : (h.tokens || 0);
            totalTokens += tokens;
            // Try to extract model from chain_id or use 'unknown'
            let model = 'unknown';
            if (h.chain_id) {
              if (h.chain_id.includes('gemini')) model = 'gemini';
              else if (h.chain_id.includes('claude')) model = 'claude';
              else if (h.chain_id.includes('codex')) model = 'codex';
              else if (h.chain_id.includes('ollama')) model = 'ollama';
            }
            byModel[model] = (byModel[model] || 0) + tokens;

            // Group by hour for time series
            const hour = new Date(h.timestamp * 1000).getHours();
            if (!byHour[hour]) byHour[hour] = {};
            byHour[hour][model] = (byHour[hour][model] || 0) + tokens;
          }
        });

        // Handle empty data
        if (totalTokens === 0) {
          summaryEl.innerHTML = '';
          chartContainer.innerHTML = '<div class="chart-empty"><div class="icon">📊</div><div>No token usage data yet</div><div style="font-size:11px;margin-top:5px;color:#555;">Run some chains to see usage stats</div></div>';
          return;
        }

        // Restore canvas if needed
        if (!document.getElementById('token-chart')) {
          chartContainer.innerHTML = '<canvas id="token-chart"></canvas>';
        }

        // Update summary with total
        summaryEl.innerHTML = '<div class="model-stat" style="font-weight:bold;"><span>Total: ' + totalTokens.toLocaleString() + ' tokens</span></div>' +
          Object.entries(byModel).map(([model, tokens]) =>
            '<div class="model-stat">' +
              '<div class="model-dot" style="background:' + (modelColors[model] || '#666') + '"></div>' +
              '<span>' + model + ': ' + tokens.toLocaleString() + '</span>' +
            '</div>'
          ).join('');

        // Create/update chart
        const ctx = document.getElementById('token-chart').getContext('2d');
        const hours = Array.from({length: 24}, (_, i) => i + 'h');
        const datasets = Object.keys(modelColors).filter(m => byModel[m]).map(model => ({
          label: model,
          data: hours.map((_, i) => (byHour[i] && byHour[i][model]) || 0),
          backgroundColor: modelColors[model],
          borderColor: modelColors[model],
          borderWidth: 1
        }));

        if (tokenChart) {
          tokenChart.data.datasets = datasets;
          tokenChart.update();
        } else {
          tokenChart = new Chart(ctx, {
            type: 'bar',
            data: { labels: hours, datasets: datasets },
            options: {
              responsive: true,
              maintainAspectRatio: false,
              plugins: {
                legend: { display: true, position: 'top', labels: { color: '#888', font: { size: 10 } } }
              },
              scales: {
                x: { stacked: true, ticks: { color: '#666' }, grid: { color: '#333' } },
                y: { stacked: true, ticks: { color: '#666' }, grid: { color: '#333' } }
              }
            }
          });
        }
      }).catch((err) => {
        console.error('Token chart error:', err);
        summaryEl.innerHTML = '';
        chartContainer.innerHTML = '<div class="chart-error"><div class="icon">⚠️</div><div>Failed to load token data</div><div style="font-size:11px;margin-top:5px;"><button class="toggle-btn" onclick="refreshTokenChart()">↻ Retry</button></div></div>';
      });
    }

    connect();
    fetchStats();
    loadHistory();  // Load history on page load
    refreshTokenChart();  // Load token chart
    setInterval(fetchStats, 5000);
    setInterval(updateChainTabs, 1000);  // Update elapsed time
    setInterval(refreshTokenChart, 30000);  // Refresh chart every 30s

    // Expose to global scope for onclick handlers
    window.switchChain = switchChain;
    window.toggleDirection = toggleDirection;
    window.loadHistory = loadHistory;
    window.showHistoryDetail = showHistoryDetail;
    window.replayChain = replayChain;
    window.refreshTokenChart = refreshTokenChart;
    });  // end DOMContentLoaded
  </script>
</body>
</html>|}

(** ============== MCP Protocol Constants ============== *)

let mcp_protocol_versions = Mcp_server_eio.supported_protocol_versions
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

let contains_substring ~sub s =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  loop 0

let truncate_for_log s =
  let max_len = 200 in
  if String.length s <= max_len then s
  else (String.sub s 0 max_len) ^ "..."

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
  match get_header headers "accept" with
  | Some accept ->
      let accept_l = String.lowercase_ascii accept in
      contains_substring ~sub:"text/event-stream" accept_l
  | None -> false

let accepts_streamable_mcp headers =
  match get_header headers "accept" with
  | Some accept -> Mcp_protocol.Http_negotiation.accepts_streamable_mcp accept
  | None -> false

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
      ("content-type", "application/json");
      ("content-length", string_of_int (String.length body));
    ] @ mcp_headers session_id protocol_version @ cors_headers) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json");
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

(** Close all SSE connections gracefully - for shutdown *)
let close_all_sse_connections () =
  let client_ids = Hashtbl.fold (fun k _ acc -> k :: acc) sse_clients [] in
  List.iter (fun id ->
    (match Hashtbl.find_opt sse_clients id with
     | Some client ->
         client.connected <- false;
         (try Httpun.Body.Writer.close client.body with _ -> ())
     | None -> ());
    Hashtbl.remove sse_clients id
  ) client_ids;
  eprintf "🐫 llm-mcp: Closed %d SSE connections\n%!" (List.length client_ids)

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
  Yojson.Safe.to_string (Mcp_server_eio.make_error ~id:`Null code message)

(** ============== HTTP Handlers ============== *)

let health_handler _request reqd =
  let body = Mcp_server_eio.health_response () in
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
    let wants_stream_headers = wants_sse headers in
    let session_id_from_header = get_session_id_header headers in
    let accept_dbg =
      match get_header headers "accept" with
      | Some v -> truncate_for_log v
      | None -> "<none>"
    in

    log_debug "LLM_MCP_DEBUG: POST /mcp (Eio) protocol=%s stream=%b session_header=%s\n%!"
      protocol_version wants_stream_headers
      (Option.value session_id_from_header ~default:"<none>");
    log_debug "LLM_MCP_DEBUG: POST /mcp Accept=%s\n%!" accept_dbg;

    Request.read_body_async reqd (fun body_str ->
      let has_method name json =
        let rec has_method_inner value =
          match value with
          | `Assoc fields -> (
              match List.assoc_opt "method" fields with
              | Some (`String m) -> String.equal m name
              | _ -> false)
          | `List items -> List.exists has_method_inner items
          | _ -> false
        in
        has_method_inner json
      in
      let (has_tools_call, has_tools_list) =
        try
          let json = Yojson.Safe.from_string body_str in
          (has_method "tools/call" json, has_method "tools/list" json)
        with _ -> (false, false)
      in
      (* tools/list must return JSON (non-SSE) *)
      let wants_stream =
        wants_stream_headers && has_tools_call && (not has_tools_list)
      in
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

  (* Chain history endpoint - read past executions from JSONL *)
  | `GET, "/chain/history" ->
      let history_file = match Sys.getenv_opt "LLM_MCP_CHAIN_HISTORY_FILE" with
        | Some path -> path
        | None ->
            match Sys.getenv_opt "CHAIN_HISTORY_FILE" with  (* Legacy fallback *)
            | Some path -> path
            | None -> "data/chain_history.jsonl"
      in
      let records =
        try
          let ic = open_in history_file in
          Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
            let rec read_lines acc =
              match input_line ic with
              | line ->
                  (try read_lines (Yojson.Safe.from_string line :: acc)
                   with _ -> read_lines acc)  (* Skip malformed lines *)
              | exception End_of_file -> List.rev acc
            in
            read_lines []
          )
        with _ -> []  (* File doesn't exist or can't be read *)
      in
      (* Return last 100 records, most recent first *)
      let recent = List.rev records |> fun l ->
        if List.length l > 100 then
          List.filteri (fun i _ -> i < 100) l
        else l
      in
      let body = Yojson.Safe.to_string (`List recent) in
      Response.json body reqd

  (* Prometheus metrics endpoint for monitoring integration *)
  | `GET, "/metrics" ->
      let history_file = match Sys.getenv_opt "LLM_MCP_CHAIN_HISTORY_FILE" with
        | Some path -> path
        | None ->
            match Sys.getenv_opt "CHAIN_HISTORY_FILE" with  (* Legacy fallback *)
            | Some path -> path
            | None -> "data/chain_history.jsonl"
      in
      let records =
        try
          let ic = open_in history_file in
          Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
            let rec read_lines acc =
              match input_line ic with
              | line ->
                  (try read_lines (Yojson.Safe.from_string line :: acc)
                   with _ -> read_lines acc)
              | exception End_of_file -> List.rev acc
            in
            read_lines []
          )
        with _ -> []
      in
      (* Aggregate metrics from history *)
      let success_count = ref 0 in
      let failure_count = ref 0 in
      let total_tokens = ref 0 in
      let gemini_tokens = ref 0 in
      let claude_tokens = ref 0 in
      let codex_tokens = ref 0 in
      let ollama_tokens = ref 0 in
      let total_duration_ms = ref 0 in
      let duration_count = ref 0 in
      List.iter (fun json ->
        let open Yojson.Safe.Util in
        let event = json |> member "event" |> to_string_option in
        let chain_id = json |> member "chain_id" |> to_string_option in
        let tokens =
          match json |> member "tokens" with
          | `Int n -> Some n
          | `Assoc _ as obj ->
              (* Try total_tokens first, then prompt_tokens + completion_tokens *)
              (match obj |> member "total_tokens" |> to_int_option with
               | Some t -> Some t
               | None ->
                   let prompt = obj |> member "prompt_tokens" |> to_int_option |> Option.value ~default:0 in
                   let completion = obj |> member "completion_tokens" |> to_int_option |> Option.value ~default:0 in
                   if prompt + completion > 0 then Some (prompt + completion) else None)
          | _ -> None
        in
        let duration_ms = json |> member "duration_ms" |> to_int_option in
        (match event with
         | Some "chain_complete" ->
             incr success_count;
             (match tokens with Some t -> total_tokens := !total_tokens + t | None -> ());
             (match duration_ms with Some d -> total_duration_ms := !total_duration_ms + d; incr duration_count | None -> ());
             let contains_model model id =
               try ignore (Str.search_forward (Str.regexp_string model) id 0); true
               with Not_found -> false
             in
             (match chain_id with
              | Some id when contains_model "gemini" id ->
                  (match tokens with Some t -> gemini_tokens := !gemini_tokens + t | None -> ())
              | Some id when contains_model "claude" id ->
                  (match tokens with Some t -> claude_tokens := !claude_tokens + t | None -> ())
              | Some id when contains_model "codex" id ->
                  (match tokens with Some t -> codex_tokens := !codex_tokens + t | None -> ())
              | Some id when contains_model "ollama" id ->
                  (match tokens with Some t -> ollama_tokens := !ollama_tokens + t | None -> ())
              | _ -> ())
         | Some "chain_error" -> incr failure_count
         | _ -> ())
      ) records;
      let avg_duration = if !duration_count > 0 then float_of_int !total_duration_ms /. float_of_int !duration_count else 0.0 in
      let metrics = Printf.sprintf {|# HELP chain_executions_total Total chain executions
# TYPE chain_executions_total counter
chain_executions_total{status="success"} %d
chain_executions_total{status="failure"} %d

# HELP chain_tokens_total Total tokens used
# TYPE chain_tokens_total counter
chain_tokens_total{model="gemini"} %d
chain_tokens_total{model="claude"} %d
chain_tokens_total{model="codex"} %d
chain_tokens_total{model="ollama"} %d
chain_tokens_total{model="all"} %d

# HELP chain_duration_avg_ms Average chain execution duration in milliseconds
# TYPE chain_duration_avg_ms gauge
chain_duration_avg_ms %.2f

# HELP chain_duration_total_ms Total chain execution duration in milliseconds
# TYPE chain_duration_total_ms counter
chain_duration_total_ms %d
|}
        !success_count !failure_count
        !gemini_tokens !claude_tokens !codex_tokens !ollama_tokens !total_tokens
        avg_duration !total_duration_ms
      in
      let headers = Httpun.Headers.of_list [
        ("content-type", "text/plain; version=0.0.4; charset=utf-8");
        ("content-length", string_of_int (String.length metrics));
      ] in
      Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers `OK) metrics

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

  let rec accept_loop backoff_s =
    try
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
      accept_loop 0.05
    with exn ->
      eprintf "[llm-mcp] Accept error: %s\n%!" (Printexc.to_string exn);
      (try Eio.Time.sleep clock backoff_s with _ -> ());
      let next_backoff = Float.min 2.0 (backoff_s *. 1.5) in
      accept_loop next_backoff
  in
  accept_loop 0.05

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

      (* Give clients 200ms to receive the notification *)
      Unix.sleepf 0.2;

      (* Gracefully close all SSE connections before Switch.fail *)
      close_all_sse_connections ();

      (* Give connections 200ms to complete close handshake *)
      Unix.sleepf 0.2;

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
  (* Enable backtrace recording for debugging *)
  Printexc.record_backtrace true;
  (* Enable chain stats collection *)
  Chain_stats.enable ();
  let config = { default_config with host; port } in
  start_server config

let cmd =
  let doc = "LLM-MCP Server (Eio) - MCP 2025-11-25" in
  let info = Cmd.info "llm-mcp" ~version:Version.version ~doc in
  Cmd.v info Term.(const main $ host_arg $ port_arg)

let () = exit (Cmd.eval cmd)

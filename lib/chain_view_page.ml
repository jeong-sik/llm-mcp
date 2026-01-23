(** HTML page for chain run visualization *)

let html = {|
<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Chain Viewer</title>
  <style>
    :root {
      --bg: #0f1115;
      --panel: #151924;
      --panel-2: #1b2130;
      --text: #e6e8ef;
      --muted: #9aa3b2;
      --accent: #6aa6ff;
      --ok: #4cc38a;
      --fail: #ff6b6b;
      --warn: #f3c969;
      --border: #2a3247;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      font-family: ui-sans-serif, system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial;
      background: var(--bg);
      color: var(--text);
    }
    #app {
      display: grid;
      grid-template-columns: 260px 1fr 360px;
      height: 100vh;
    }
    .panel {
      border-right: 1px solid var(--border);
      background: var(--panel);
      display: flex;
      flex-direction: column;
      min-width: 0;
    }
    .panel:last-child { border-right: none; }
    .panel-header {
      padding: 12px 14px;
      border-bottom: 1px solid var(--border);
      display: flex;
      align-items: center;
      gap: 8px;
      justify-content: space-between;
      background: var(--panel-2);
    }
    .panel-header h3 {
      margin: 0;
      font-size: 13px;
      font-weight: 600;
      letter-spacing: 0.04em;
      text-transform: uppercase;
      color: var(--muted);
    }
    button {
      background: transparent;
      border: 1px solid var(--border);
      color: var(--text);
      padding: 6px 10px;
      border-radius: 6px;
      cursor: pointer;
    }
    button:hover { border-color: var(--accent); color: var(--accent); }
    #run-list, #node-list {
      overflow: auto;
      padding: 8px;
      display: flex;
      flex-direction: column;
      gap: 6px;
    }
    .run-item, .node-item {
      padding: 8px 10px;
      border: 1px solid var(--border);
      border-radius: 8px;
      background: #121621;
      cursor: pointer;
      display: grid;
      gap: 4px;
    }
    .run-item.active, .node-item.active { border-color: var(--accent); }
    .run-title { font-size: 12px; color: var(--muted); }
    .run-id { font-size: 12px; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
    .badge {
      font-size: 11px;
      padding: 2px 6px;
      border-radius: 6px;
      display: inline-block;
    }
    .badge.ok { background: rgba(76,195,138,0.15); color: var(--ok); border: 1px solid rgba(76,195,138,0.4); }
    .badge.fail { background: rgba(255,107,107,0.15); color: var(--fail); border: 1px solid rgba(255,107,107,0.4); }
    .badge.warn { background: rgba(243,201,105,0.15); color: var(--warn); border: 1px solid rgba(243,201,105,0.4); }
    #main {
      display: grid;
      grid-template-rows: 1fr 260px;
      min-width: 0;
    }
    #graph {
      padding: 12px;
      overflow: auto;
    }
    #graph-fallback {
      padding: 12px;
      color: var(--muted);
      font-size: 12px;
    }
    #detail {
      padding: 12px;
      overflow: auto;
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      font-size: 12px;
      white-space: pre-wrap;
      background: #0e121b;
      border-top: 1px solid var(--border);
    }
    .row {
      display: flex;
      align-items: center;
      gap: 8px;
      flex-wrap: wrap;
      font-size: 12px;
      color: var(--muted);
    }
    .node-title {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
    }
    .node-id {
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      font-size: 12px;
    }
    .node-output {
      font-size: 12px;
      color: var(--muted);
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .empty {
      padding: 12px;
      color: var(--muted);
      font-size: 12px;
    }
  </style>
</head>
<body>
  <div id="app">
    <div class="panel">
      <div class="panel-header">
        <h3>Runs</h3>
        <button id="refresh">Refresh</button>
      </div>
      <div id="run-list"></div>
    </div>

    <div id="main">
      <div class="panel">
        <div class="panel-header">
          <h3>Graph</h3>
          <div id="graph-meta" class="row"></div>
        </div>
        <div id="graph"></div>
        <div id="graph-fallback"></div>
      </div>
      <div class="panel">
        <div class="panel-header">
          <h3>Nodes</h3>
        </div>
        <div id="node-list"></div>
      </div>
    </div>

    <div class="panel">
      <div class="panel-header">
        <h3>Detail</h3>
      </div>
      <div id="detail">Select a node to inspect details.</div>
    </div>
  </div>

  <script type="module">
    import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';

    mermaid.initialize({ startOnLoad: false, theme: 'dark', securityLevel: 'loose' });

    const state = {
      runs: [],
      current: null,
      nodes: new Map(),
      selectedNode: null,
    };

    const el = (id) => document.getElementById(id);

    const fmtMs = (ms) => (ms == null ? '-' : `${ms}ms`);
    const fmtStatus = (s) => s || 'unknown';

    async function fetchJson(path) {
      const res = await fetch(path);
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      return res.json();
    }

    function clearChildren(node) {
      while (node.firstChild) node.removeChild(node.firstChild);
    }

    function renderRuns() {
      const list = el('run-list');
      clearChildren(list);
      if (!state.runs.length) {
        const empty = document.createElement('div');
        empty.className = 'empty';
        empty.textContent = 'No runs yet. Execute chain.run/orchestrate first.';
        list.appendChild(empty);
        return;
      }
      state.runs.forEach((run) => {
        const item = document.createElement('div');
        item.className = 'run-item';
        if (state.current && state.current.run_id === run.run_id) item.classList.add('active');
        item.onclick = () => loadRun(run.run_id);

        const title = document.createElement('div');
        title.className = 'run-title';
        title.textContent = run.chain_id;

        const id = document.createElement('div');
        id.className = 'run-id';
        id.textContent = run.run_id;

        const row = document.createElement('div');
        row.className = 'row';

        const badge = document.createElement('span');
        badge.className = `badge ${run.success ? 'ok' : 'fail'}`;
        badge.textContent = run.success ? 'SUCCESS' : 'FAIL';

        const meta = document.createElement('span');
        meta.textContent = `${run.node_count} nodes · ${run.duration_ms}ms`;

        row.appendChild(badge);
        row.appendChild(meta);

        item.appendChild(title);
        item.appendChild(id);
        item.appendChild(row);
        list.appendChild(item);
      });
    }

    async function renderGraph(mermaidText) {
      const graph = el('graph');
      const fallback = el('graph-fallback');
      graph.innerHTML = '';
      fallback.textContent = '';
      if (!mermaidText) {
        fallback.textContent = 'No graph data.';
        return;
      }
      try {
        const { svg } = await mermaid.render('chain-graph', mermaidText);
        graph.innerHTML = svg;
        bindGraphClicks();
      } catch (err) {
        fallback.textContent = `Mermaid render failed: ${err.message}`;
      }
    }

    function bindGraphClicks() {
      const graph = el('graph');
      const nodes = graph.querySelectorAll('g.node');
      nodes.forEach((nodeEl) => {
        const title = nodeEl.querySelector('title');
        const nodeId = title ? title.textContent.trim() : null;
        if (!nodeId || !state.nodes.has(nodeId)) return;
        nodeEl.style.cursor = 'pointer';
        nodeEl.addEventListener('click', () => selectNode(nodeId));
      });
    }

    function renderNodes() {
      const list = el('node-list');
      clearChildren(list);
      if (!state.current || !state.current.nodes.length) {
        const empty = document.createElement('div');
        empty.className = 'empty';
        empty.textContent = 'No node details available.';
        list.appendChild(empty);
        return;
      }

      state.current.nodes.forEach((node) => {
        const item = document.createElement('div');
        item.className = 'node-item';
        if (state.selectedNode === node.id) item.classList.add('active');
        item.onclick = () => selectNode(node.id);

        const title = document.createElement('div');
        title.className = 'node-title';

        const id = document.createElement('span');
        id.className = 'node-id';
        id.textContent = node.id;

        const badge = document.createElement('span');
        const status = fmtStatus(node.status);
        badge.className = `badge ${status === 'success' ? 'ok' : status === 'failure' ? 'fail' : 'warn'}`;
        badge.textContent = status.toUpperCase();

        title.appendChild(id);
        title.appendChild(badge);

        const meta = document.createElement('div');
        meta.className = 'row';
        meta.textContent = `${node.type} · ${fmtMs(node.duration_ms)}`;

        const output = document.createElement('div');
        output.className = 'node-output';
        output.textContent = node.output_preview || '(no output)';

        item.appendChild(title);
        item.appendChild(meta);
        item.appendChild(output);
        list.appendChild(item);
      });
    }

    function renderDetail(node) {
      const detail = el('detail');
      if (!node) {
        detail.textContent = 'Select a node to inspect details.';
        return;
      }
      detail.textContent = JSON.stringify(node, null, 2);
    }

    function selectNode(nodeId) {
      const node = state.nodes.get(nodeId);
      state.selectedNode = nodeId;
      renderNodes();
      renderDetail(node || null);
    }

    function renderMeta(run) {
      const meta = el('graph-meta');
      if (!run) {
        meta.textContent = '';
        return;
      }
      meta.textContent = `Run: ${run.run_id} · ${run.duration_ms}ms · ${run.nodes.length} nodes`;
    }

    async function loadRun(runId) {
      const data = await fetchJson(`/chain/runs/${runId}`);
      state.current = data.run;
      state.nodes = new Map(state.current.nodes.map((n) => [n.id, n]));
      state.selectedNode = null;
      renderRuns();
      renderMeta(state.current);
      await renderGraph(state.current.mermaid);
      renderNodes();
      renderDetail(null);
    }

    async function loadRuns() {
      const data = await fetchJson('/chain/runs');
      state.runs = data.runs || [];
      renderRuns();

      const urlParams = new URLSearchParams(window.location.search);
      const runId = urlParams.get('run_id');
      if (runId) {
        await loadRun(runId);
        return;
      }
      if (state.runs.length) {
        await loadRun(state.runs[0].run_id);
      }
    }

    document.getElementById('refresh').addEventListener('click', () => {
      loadRuns();
    });

    loadRuns().catch((err) => {
      const list = el('run-list');
      list.innerHTML = '';
      const empty = document.createElement('div');
      empty.className = 'empty';
      empty.textContent = `Failed to load runs: ${err.message}`;
      list.appendChild(empty);
    });
  </script>
</body>
</html>
|}

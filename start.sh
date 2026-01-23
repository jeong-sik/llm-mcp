#!/bin/bash
# Start OCaml llm-mcp server (replaces Python version)
# Usage: ./start.sh [--port PORT]

cd "$(dirname "$0")"
PORT="${1:-8932}"

# Kill existing
pkill -f "main.exe.*--port" 2>/dev/null

# Start OCaml server
echo "🐫 Starting llm-mcp (OCaml) on port $PORT..."
nohup ./_build/default/bin/main.exe --port "$PORT" > /tmp/llm-mcp-ocaml.log 2>&1 &

sleep 1
if curl -s "http://127.0.0.1:$PORT/health" | grep -q '"status":"ok"'; then
    echo "✅ Server running: http://127.0.0.1:$PORT/mcp"
else
    echo "❌ Failed to start. Check /tmp/llm-mcp-ocaml.log"
    exit 1
fi

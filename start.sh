#!/bin/bash
# Start OCaml llm-mcp server (delegates to start-llm-mcp.sh)
# Usage: ./start.sh [PORT]

cd "$(dirname "$0")"
PORT="${1:-}"
if [ -n "$PORT" ]; then
    exec ./start-llm-mcp.sh --port "$PORT"
fi
exec ./start-llm-mcp.sh

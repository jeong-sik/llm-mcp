#!/bin/bash
# Start OCaml llm-mcp server (delegates to start-llm-mcp.sh)

cd "$(dirname "$0")"

# Run on port 8935 (legacy)
exec ./start-llm-mcp.sh --port 8935

#!/bin/bash
# LLM-MCP (OCaml) - Start Script (HTTP default)
# Usage: ./start-llm-mcp.sh [--stdio] [--port PORT]

set -e

# Ensure OCaml environment
if command -v opam >/dev/null 2>&1; then
    eval $(opam env)
elif [ -f "/opt/homebrew/bin/opam" ]; then
    eval $(/opt/homebrew/bin/opam env)
elif [ -f "$HOME/.opam/opam-init/init.sh" ]; then
    . "$HOME/.opam/opam-init/init.sh" > /dev/null 2> /dev/null || true
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Resolve target port/stdout mode from args for safe cleanup
resolve_port() {
    local port="8932"
    while [ "$#" -gt 0 ]; do
        case "$1" in
            --port)
                shift
                port="${1:-$port}"
                ;;
            --port=*)
                port="${1#*=}"
                ;;
            -p)
                shift
                port="${1:-$port}"
                ;;
        esac
        shift || true
    done
    echo "$port"
}

is_stdio_mode() {
    while [ "$#" -gt 0 ]; do
        case "$1" in
            --stdio|--stdio=true|--stdio=1) return 0 ;;
        esac
        shift || true
    done
    return 1
}

is_llm_mcp_cmd() {
    local cmd="$1"
    case "$cmd" in
        *"/features/llm-mcp/"*|*"/llm-mcp/"*|*"llm-mcp"*|*"main.exe"*) return 0 ;;
    esac
    return 1
}

stop_existing_on_port() {
    local port="$1"
    local pids
    pids="$(lsof -nP -iTCP:"$port" -sTCP:LISTEN -t 2>/dev/null || true)"
    if [ -z "$pids" ]; then
        return 0
    fi

    local killed=0
    for pid in $pids; do
        local cmd
        cmd="$(ps -o command= -p "$pid" 2>/dev/null || true)"
        if is_llm_mcp_cmd "$cmd"; then
            echo "Stopping existing llm-mcp on port $port (pid: $pid)..." >&2
            kill "$pid" 2>/dev/null || true
            killed=1
        fi
    done

    if [ "$killed" -eq 1 ]; then
        sleep 0.5
        for pid in $pids; do
            if kill -0 "$pid" 2>/dev/null; then
                local cmd
                cmd="$(ps -o command= -p "$pid" 2>/dev/null || true)"
                if is_llm_mcp_cmd "$cmd"; then
                    kill -9 "$pid" 2>/dev/null || true
                fi
            fi
        done
    else
        echo "Port $port is in use by a non-llm-mcp process; skipping kill." >&2
    fi
}

# Resolve executable path (prefer workspace build dir)
WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/llm-mcp/bin/main.exe"
LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/main.exe"
INSTALLED_EXE="$(command -v llm-mcp || true)"
LLM_EXE=""

if [ -x "$WORKSPACE_EXE" ]; then
    LLM_EXE="$WORKSPACE_EXE"
elif [ -x "$LOCAL_EXE" ]; then
    LLM_EXE="$LOCAL_EXE"
elif [ -n "$INSTALLED_EXE" ]; then
    LLM_EXE="$INSTALLED_EXE"
fi

# Build if needed (requires dune on PATH)
if [ -z "$LLM_EXE" ]; then
    echo "Building LLM-MCP (bin/main.exe only)..." >&2
    if ! command -v dune >/dev/null 2>&1; then
        echo "Error: dune not found. Install dune or build llm-mcp binary first." >&2
        exit 1
    fi
    dune build ./bin/main.exe >&2

    if [ -x "$WORKSPACE_EXE" ]; then
        LLM_EXE="$WORKSPACE_EXE"
    elif [ -x "$LOCAL_EXE" ]; then
        LLM_EXE="$LOCAL_EXE"
    elif command -v llm-mcp >/dev/null 2>&1; then
        LLM_EXE="$(command -v llm-mcp)"
    else
        echo "Error: build succeeded but llm-mcp executable not found." >&2
        exit 1
    fi
fi

PORT="$(resolve_port "$@")"
if ! is_stdio_mode "$@"; then
    stop_existing_on_port "$PORT"
fi

exec "$LLM_EXE" "$@"

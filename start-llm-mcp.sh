#!/bin/bash
# LLM-MCP (OCaml) - Start Script (HTTP default)
# Usage: ./start-llm-mcp.sh [--stdio] [--port PORT] [--host HOST] [--public] [--allow-no-auth]
#        ./start-llm-mcp.sh --masc-hook [--masc-agent NAME] [--masc-heartbeat-sec N]

set -e

# Try to raise the file descriptor limit to avoid EMFILE accept crashes.
# This must never be fatal under launchd.
ULIMIT_NOFILE="${LLM_ULIMIT_NOFILE:-65536}"
ulimit -n "$ULIMIT_NOFILE" >/dev/null 2>&1 || true

# Load API keys from user environment
if [ -f "$HOME/.zshenv" ]; then
    source "$HOME/.zshenv" 2>/dev/null || true
fi

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

# Parse MASC hook flags and strip them from args passed to binary
clean_args=()
masc_hook=""
masc_agent=""
masc_heartbeat=""
public_flag="false"
allow_no_auth_flag="false"
while [ "$#" -gt 0 ]; do
    case "$1" in
        --masc-hook|--masc-hook=true|--masc-hook=1)
            masc_hook="true"
            shift
            ;;
        --masc-hook=false|--masc-hook=0)
            masc_hook="false"
            shift
            ;;
        --masc-agent)
            shift
            masc_agent="${1:-}"
            shift || true
            ;;
        --masc-agent=*)
            masc_agent="${1#*=}"
            shift
            ;;
        --masc-heartbeat-sec)
            shift
            masc_heartbeat="${1:-}"
            shift || true
            ;;
        --masc-heartbeat-sec=*)
            masc_heartbeat="${1#*=}"
            shift
            ;;
        --public)
            public_flag="true"
            clean_args+=("$1")
            shift
            ;;
        --allow-no-auth)
            allow_no_auth_flag="true"
            clean_args+=("$1")
            shift
            ;;
        *)
            clean_args+=("$1")
            shift
            ;;
    esac
done

if [ -n "$masc_hook" ]; then
    export LLM_MCP_MASC_HOOK="$masc_hook"
fi
if [ -n "$masc_agent" ]; then
    export LLM_MCP_MASC_AGENT="$masc_agent"
fi
if [ -n "$masc_heartbeat" ]; then
    export LLM_MCP_MASC_HEARTBEAT_SEC="$masc_heartbeat"
fi

if [ "$public_flag" = "true" ]; then
    export LLM_MCP_PUBLIC=1
fi
if [ "$allow_no_auth_flag" = "true" ]; then
    export LLM_MCP_ALLOW_NO_AUTH=1
fi

set -- "${clean_args[@]}"

is_truthy() {
    case "$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')" in
        1|true|yes) return 0 ;;
    esac
    return 1
}

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

is_launchd_job() {
    [ "${LAUNCH_JOB_IDENTIFIER:-}" = "com.jeong-sik.llm-mcp" ]
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

# Resolve executable path
# Priority: 1. Local build (freshest)  2. Workspace build  3. Installed  4. Release binary  5. Auto-download
RELEASE_BINARY="$SCRIPT_DIR/llm-mcp-macos-arm64"
WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/llm-mcp/bin/main_eio.exe"
LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/main_eio.exe"
INSTALLED_EXE="$(command -v llm-mcp || true)"
LLM_EXE=""

# 1. Local build (always freshest, has latest features)
if [ -x "$LOCAL_EXE" ]; then
    LLM_EXE="$LOCAL_EXE"
# 2. Workspace build
elif [ -x "$WORKSPACE_EXE" ]; then
    LLM_EXE="$WORKSPACE_EXE"
# 3. System-installed
elif [ -n "$INSTALLED_EXE" ]; then
    LLM_EXE="$INSTALLED_EXE"
# 4. Release binary (fallback, may be outdated)
elif [ -x "$RELEASE_BINARY" ]; then
    LLM_EXE="$RELEASE_BINARY"
fi

# 5. Auto-download from GitHub releases if nothing found
if [ -z "$LLM_EXE" ]; then
    echo "No binary found. Downloading from GitHub releases..." >&2
    RELEASE_URL="https://github.com/jeong-sik/llm-mcp/releases/latest/download/llm-mcp-macos-arm64"
    if curl -fsSL -o "$RELEASE_BINARY" "$RELEASE_URL" 2>/dev/null; then
        chmod +x "$RELEASE_BINARY"
        LLM_EXE="$RELEASE_BINARY"
        echo "Downloaded: $RELEASE_BINARY" >&2
    else
        # Fallback: build from source
        echo "Download failed. Building from source..." >&2
        if ! command -v dune >/dev/null 2>&1; then
            echo "Error: dune not found. Install dune or download binary manually." >&2
            exit 1
        fi
        dune build ./bin/main.exe >&2
        if [ -x "$LOCAL_EXE" ]; then
            LLM_EXE="$LOCAL_EXE"
        else
            echo "Error: build failed." >&2
            exit 1
        fi
    fi
fi

PORT="$(resolve_port "$@")"
if ! is_stdio_mode "$@"; then
    if ! is_truthy "${LLM_MCP_ALLOW_NO_AUTH:-}" \
        && [ -z "${LLM_MCP_API_KEY:-}" ] && [ -z "${MCP_API_KEY:-}" ]; then
        echo "Error: LLM_MCP_API_KEY (or MCP_API_KEY) is required. Set --allow-no-auth or LLM_MCP_ALLOW_NO_AUTH=1 to bypass." >&2
        exit 2
    fi
    if ! is_launchd_job && command -v launchctl >/dev/null 2>&1; then
        if launchctl list 2>/dev/null | grep -q "com.jeong-sik.llm-mcp"; then
            echo "launchd com.jeong-sik.llm-mcp is managing llm-mcp; aborting direct start." >&2
            exit 1
        fi
    fi
    stop_existing_on_port "$PORT"
fi

exec "$LLM_EXE" "$@"

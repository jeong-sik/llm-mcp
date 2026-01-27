#!/bin/bash
# Chain DSL Simple Test Suite
# Tests 'simple-test' model behavior

set -e

PORT=${1:-8932}
BASE_URL="http://127.0.0.1:$PORT"
PASSED=0
FAILED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

log_pass() { echo -e "${GREEN}✓ PASS${NC}: $1"; ((PASSED++)); }
log_fail() { echo -e "${RED}✗ FAIL${NC}: $1 - $2"; ((FAILED++)); }

# Run a chain test and check result content
run_test_content() {
    local name="$1"
    local mermaid="$2"
    local expected_content="$3"
    local timeout="${4:-30}"

    local payload=$(jq -n \
                  --arg mermaid "$mermaid" \
                  --arg timeout "$timeout" \
                  '{
                    jsonrpc: "2.0",
                    id: 1,
                    method: "tools/call",
                    params: {
                      name: "chain.run",
                      arguments: {
                        mermaid: $mermaid,
                        timeout: ($timeout | tonumber)
                      }
                    }
                  }')

    local result=$(timeout $timeout curl -s "$BASE_URL/mcp" -X POST \
        -H "Content-Type: application/json" \
        -H "Accept: application/json, text/event-stream" \
        -d "$payload" 2>/dev/null)

    if [ -z "$result" ]; then
        log_fail "$name" "No response or timeout"
        return
    fi

    if echo "$result" | grep -q '"isError":false'; then
        # Extract content (simplified extraction)
        if echo "$result" | grep -F -q "$expected_content"; then
             log_pass "$name"
        else
             log_fail "$name" "Content match failure. Expected '$expected_content' in output."
        fi
    else
        log_fail "$name" "Execution failed (isError=true). Response: $result"
    fi
}

# Check server health
check_health() {
    local health=$(curl -s "$BASE_URL/health" 2>/dev/null)
    if echo "$health" | grep -q '"status":"ok"'; then
        echo -e "${GREEN}Server healthy at $BASE_URL${NC}"
        return 0
    else
        echo -e "${RED}Server not responding at $BASE_URL${NC}"
        return 1
    fi
}

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║       Chain Simple Test Suite                             ║"
echo "║       Testing 'simple-test' model integration             ║"
echo "╚═══════════════════════════════════════════════════════════╝"

if ! check_health; then
    echo "Please start the server: dune exec llm-mcp -- --port $PORT"
    exit 1
fi

# 1. Simple Test Model
run_test_content "1.1 Simple Test Model" \
    "graph LR
    A[LLM:simple-test \"Hello\"]" \
    "[test]Hello"

# 2. Stub vs Simple Test
run_test_content "1.2 Stub Model" \
    "graph LR
    A[LLM:stub \"Hello\"]" \
    "[stub]Hello"

echo ""
echo "Summary: Passed: $PASSED, Failed: $FAILED"

if [ $FAILED -gt 0 ]; then
    exit 1
fi

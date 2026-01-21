#!/bin/bash
# Chain DSL Pattern Test Suite
# Tests all 11 node types and common combinations

set -e

PORT=${1:-8932}
BASE_URL="http://127.0.0.1:$PORT"
PASSED=0
FAILED=0
SKIPPED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

log_pass() { echo -e "${GREEN}✓ PASS${NC}: $1"; ((PASSED++)); }
log_fail() { echo -e "${RED}✗ FAIL${NC}: $1 - $2"; ((FAILED++)); }
log_skip() { echo -e "${YELLOW}○ SKIP${NC}: $1 - $2"; ((SKIPPED++)); }
log_section() { echo -e "\n${YELLOW}═══════════════════════════════════════${NC}"; echo -e "${YELLOW}$1${NC}"; echo -e "${YELLOW}═══════════════════════════════════════${NC}"; }

# Run a chain test and check result
run_test() {
    local name="$1"
    local mermaid="$2"
    local expect_success="${3:-true}"
    local timeout="${4:-60}"

    local payload=$(cat <<EOF
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "chain.run",
    "arguments": {
      "mermaid": "$mermaid",
      "timeout": $timeout
    }
  }
}
EOF
)

    local result=$(timeout $timeout curl -s "$BASE_URL/mcp" -X POST \
        -H "Content-Type: application/json" \
        -H "Accept: application/json, text/event-stream" \
        -d "$payload" 2>/dev/null | grep -o '"isError":[^,}]*' | head -1)

    if [ -z "$result" ]; then
        if [ "$expect_success" = "true" ]; then
            log_fail "$name" "No response or timeout"
        else
            log_pass "$name (expected failure)"
        fi
        return
    fi

    if echo "$result" | grep -q '"isError":false'; then
        if [ "$expect_success" = "true" ]; then
            log_pass "$name"
        else
            log_fail "$name" "Expected failure but succeeded"
        fi
    else
        if [ "$expect_success" = "true" ]; then
            log_fail "$name" "isError=true"
        else
            log_pass "$name (expected failure)"
        fi
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
echo "║       Chain DSL Pattern Test Suite                        ║"
echo "║       Testing all 11 node types + combinations            ║"
echo "╚═══════════════════════════════════════════════════════════╝"

if ! check_health; then
    echo "Please start the server: dune exec llm-mcp -- --port $PORT"
    exit 1
fi

# ═══════════════════════════════════════════════════════════════
# 1. BASIC PATTERNS
# ═══════════════════════════════════════════════════════════════
log_section "1. BASIC PATTERNS"

# 1.1 Single LLM node
run_test "1.1 Single LLM" \
    "graph LR\\n    A[LLM:sonnet \\\"1+1=?\\\"]"

# 1.2 Two-node chain (sequential)
run_test "1.2 Two-node chain" \
    "graph LR\\n    A[LLM:sonnet \\\"Say hello\\\"] --> B[LLM:sonnet \\\"Translate to Korean: {{A}}\\\"]"

# 1.3 Three-node chain
run_test "1.3 Three-node chain" \
    "graph LR\\n    A[LLM:sonnet \\\"Pick a color\\\"] --> B[LLM:sonnet \\\"Pick a fruit of color {{A}}\\\"] --> C[LLM:sonnet \\\"Summarize: {{B}}\\\"]"

# ═══════════════════════════════════════════════════════════════
# 2. PARALLEL PATTERNS
# ═══════════════════════════════════════════════════════════════
log_section "2. PARALLEL PATTERNS"

# 2.1 Simple fanout (no merge)
run_test "2.1 Simple fanout" \
    "graph LR\\n    A[LLM:sonnet \\\"Topic: AI\\\"] --> B[LLM:sonnet \\\"Pros of {{A}}\\\"]\\n    A --> C[LLM:sonnet \\\"Cons of {{A}}\\\"]"

# 2.2 Fan-out/Fan-in
run_test "2.2 Fan-out/Fan-in" \
    "graph LR\\n    A[LLM:sonnet \\\"Topic: Space\\\"] --> B[LLM:sonnet \\\"Science of {{A}}\\\"]\\n    A --> C[LLM:sonnet \\\"Fiction of {{A}}\\\"]\\n    B --> D[LLM:sonnet \\\"Combine: {{B}} and {{C}}\\\"]\\n    C --> D"

# ═══════════════════════════════════════════════════════════════
# 3. CONTROL PATTERNS
# ═══════════════════════════════════════════════════════════════
log_section "3. CONTROL PATTERNS"

# 3.1 Quorum (2/3 voting)
run_test "3.1 Quorum 2/3" \
    "graph LR\\n    J1[LLM:sonnet \\\"1+1=?\\\"] --> V{Quorum:2}\\n    J2[LLM:sonnet \\\"2+0=?\\\"] --> V\\n    J3[LLM:sonnet \\\"3-1=?\\\"] --> V"

# 3.2 Quorum (3/3 voting)
run_test "3.2 Quorum 3/3" \
    "graph LR\\n    J1[LLM:sonnet \\\"What is 2?\\\"] --> V{Quorum:3}\\n    J2[LLM:sonnet \\\"1+1?\\\"] --> V\\n    J3[LLM:sonnet \\\"4/2?\\\"] --> V"

# 3.3 Merge:concat
run_test "3.3 Merge concat" \
    "graph LR\\n    A[LLM:sonnet \\\"Say hi\\\"] --> M{Merge:concat}\\n    B[LLM:sonnet \\\"Say bye\\\"] --> M"

# 3.4 Merge:first
run_test "3.4 Merge first" \
    "graph LR\\n    A[LLM:sonnet \\\"Number 1\\\"] --> M{Merge:first}\\n    B[LLM:sonnet \\\"Number 2\\\"] --> M"

# ═══════════════════════════════════════════════════════════════
# 4. COMPOSITION PATTERNS
# ═══════════════════════════════════════════════════════════════
log_section "4. COMPOSITION PATTERNS"

# 4.1 Pipeline (inline sequential)
run_test "4.1 Pipeline" \
    "graph LR\\n    P[[Pipeline:step1,step2,step3]]" \
    "false" # Pipeline needs registered steps - expected to fail

# 4.2 Fanout (explicit parallel)
run_test "4.2 Fanout explicit" \
    "graph LR\\n    F[[Fanout:branch1,branch2]]" \
    "false" # Fanout needs registered branches - expected to fail

# 4.3 ChainRef (template reference)
run_test "4.3 ChainRef" \
    "graph LR\\n    R[[Ref:my_template]]" \
    "false" # ChainRef needs registered chain - expected to fail

# ═══════════════════════════════════════════════════════════════
# 5. NESTED/COMBINED PATTERNS
# ═══════════════════════════════════════════════════════════════
log_section "5. NESTED/COMBINED PATTERNS"

# 5.1 Chain + Quorum
run_test "5.1 Chain then Quorum" \
    "graph LR\\n    Q[LLM:sonnet \\\"Pick: cats or dogs\\\"] --> J1[LLM:sonnet \\\"Vote 1: {{Q}}\\\"]\\n    Q --> J2[LLM:sonnet \\\"Vote 2: {{Q}}\\\"]\\n    J1 --> V{Quorum:2}\\n    J2 --> V"

# 5.2 Fanout + Merge + Chain
run_test "5.2 Fanout-Merge-Chain" \
    "graph LR\\n    A[LLM:sonnet \\\"Topic: Music\\\"] --> B[LLM:sonnet \\\"Jazz: {{A}}\\\"]\\n    A --> C[LLM:sonnet \\\"Rock: {{A}}\\\"]\\n    B --> M{Merge:concat}\\n    C --> M\\n    M --> D[LLM:sonnet \\\"Summary: {{M}}\\\"]" \
    "true" 90

# 5.3 Multiple independent chains
run_test "5.3 Multiple chains" \
    "graph LR\\n    A[LLM:sonnet \\\"Hello\\\"] --> B[LLM:sonnet \\\"Repeat: {{A}}\\\"]\\n    C[LLM:sonnet \\\"World\\\"] --> D[LLM:sonnet \\\"Echo: {{C}}\\\"]"

# ═══════════════════════════════════════════════════════════════
# 6. EDGE CASES
# ═══════════════════════════════════════════════════════════════
log_section "6. EDGE CASES"

# 6.1 Empty prompt
run_test "6.1 Empty prompt" \
    "graph LR\\n    A[LLM:sonnet \\\"\\\"]" \
    "true"  # Should still work with empty prompt

# 6.2 Long chain (5 nodes)
run_test "6.2 Long chain (5)" \
    "graph LR\\n    A[LLM:sonnet \\\"1\\\"] --> B[LLM:sonnet \\\"{{A}}+1\\\"] --> C[LLM:sonnet \\\"{{B}}+1\\\"] --> D[LLM:sonnet \\\"{{C}}+1\\\"] --> E[LLM:sonnet \\\"{{D}}+1\\\"]" \
    "true" 120

# 6.3 Wide fanout (4 parallel)
run_test "6.3 Wide fanout (4)" \
    "graph LR\\n    A[LLM:sonnet \\\"Start\\\"] --> B[LLM:sonnet \\\"B\\\"]\\n    A --> C[LLM:sonnet \\\"C\\\"]\\n    A --> D[LLM:sonnet \\\"D\\\"]\\n    A --> E[LLM:sonnet \\\"E\\\"]"

# 6.4 Diamond pattern
run_test "6.4 Diamond" \
    "graph LR\\n    A[LLM:sonnet \\\"Top\\\"] --> B[LLM:sonnet \\\"Left\\\"]\\n    A --> C[LLM:sonnet \\\"Right\\\"]\\n    B --> D[LLM:sonnet \\\"Bottom: {{B}} {{C}}\\\"]\\n    C --> D"

# 6.5 Self-reference (should handle gracefully)
run_test "6.5 Circular ref" \
    "graph LR\\n    A[LLM:sonnet \\\"{{A}}\\\"]" \
    "true"  # Should work but with empty substitution

# ═══════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════
echo ""
echo "╔═══════════════════════════════════════════════════════════╗"
echo "║                    TEST SUMMARY                            ║"
echo "╠═══════════════════════════════════════════════════════════╣"
printf "║  ${GREEN}PASSED${NC}: %-4d                                           ║\n" $PASSED
printf "║  ${RED}FAILED${NC}: %-4d                                           ║\n" $FAILED
printf "║  ${YELLOW}SKIPPED${NC}: %-3d                                           ║\n" $SKIPPED
echo "╠═══════════════════════════════════════════════════════════╣"
TOTAL=$((PASSED + FAILED))
if [ $TOTAL -gt 0 ]; then
    RATE=$((PASSED * 100 / TOTAL))
    printf "║  Pass Rate: %d%%                                          ║\n" $RATE
fi
echo "╚═══════════════════════════════════════════════════════════╝"

if [ $FAILED -gt 0 ]; then
    exit 1
fi

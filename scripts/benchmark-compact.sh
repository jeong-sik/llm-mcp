#!/bin/bash
# Compact Protocol v1.3 Benchmark
# Measures actual token/size savings across formats

set -e

PORT="${LLM_MCP_PORT:-8932}"
BASE_URL="http://localhost:$PORT"

# Check server is running
if ! curl -s "$BASE_URL/health" > /dev/null 2>&1; then
    echo "❌ LLM-MCP server not running on port $PORT"
    echo "Start with: cd ~/me/features/llm-mcp && ./start-llm-mcp.sh"
    exit 1
fi

echo "📊 Compact Protocol v1.3 Benchmark"
echo "=================================="
echo ""

# Test prompt
PROMPT="What is 2+2? Answer with just the number."

# Function to make MCP request
call_mcp() {
    local format=$1
    curl -s -X POST "$BASE_URL/mcp" \
        -H "Content-Type: application/json" \
        -d "{
            \"jsonrpc\": \"2.0\",
            \"id\": 1,
            \"method\": \"tools/call\",
            \"params\": {
                \"name\": \"gemini\",
                \"arguments\": {
                    \"prompt\": \"$PROMPT\",
                    \"response_format\": \"$format\",
                    \"thinking_level\": \"low\",
                    \"timeout\": 30
                }
            }
        }"
}

# Function to measure response size
measure() {
    local format=$1
    local label=$2

    echo -n "Testing $label... "

    local start=$(date +%s%N)
    local response=$(call_mcp "$format")
    local end=$(date +%s%N)

    local duration_ms=$(( (end - start) / 1000000 ))

    # Extract result from JSON-RPC response
    local result=$(echo "$response" | jq -r '.result.content[0].text // .result // empty')

    if [[ -z "$result" ]]; then
        echo "❌ Failed"
        return
    fi

    local size=${#result}
    echo "✅ ${size} bytes (${duration_ms}ms)"

    # Store for comparison
    eval "SIZE_$format=$size"
}

echo "Prompt: \"$PROMPT\""
echo ""

# Run benchmarks
measure "verbose" "Verbose (JSON)"
measure "compact" "Compact (DSL)"
measure "binary" "Binary (Base64)"
measure "base85" "Base85"
# Note: compressed only helps for large responses
# measure "compressed" "Compressed (Zlib)"

echo ""
echo "📈 Results"
echo "----------"

BASELINE=${SIZE_verbose:-100}

if [[ -n "$SIZE_compact" ]]; then
    SAVINGS=$(( (BASELINE - SIZE_compact) * 100 / BASELINE ))
    echo "Compact savings: ${SAVINGS}% (${SIZE_verbose} → ${SIZE_compact} bytes)"
fi

if [[ -n "$SIZE_binary" ]]; then
    SAVINGS=$(( (BASELINE - SIZE_binary) * 100 / BASELINE ))
    echo "Binary savings:  ${SAVINGS}% (${SIZE_verbose} → ${SIZE_binary} bytes)"
fi

if [[ -n "$SIZE_base85" ]]; then
    SAVINGS=$(( (BASELINE - SIZE_base85) * 100 / BASELINE ))
    echo "Base85 savings:  ${SAVINGS}% (${SIZE_verbose} → ${SIZE_base85} bytes)"
fi

echo ""
echo "💡 Note: Compact Protocol is most effective for LLM-to-LLM communication"
echo "   where token efficiency matters more than human readability."

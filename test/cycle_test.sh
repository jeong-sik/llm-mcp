#!/bin/bash
# Agent Loop Long Cycle Test
# Usage: ./cycle_test.sh <duration_minutes>

DURATION_MIN=${1:-1}
DURATION_SEC=$((DURATION_MIN * 60))
LOG_FILE="/tmp/cycle_test_${DURATION_MIN}min.log"
RESULT_FILE="/tmp/cycle_test_${DURATION_MIN}min_result.json"

echo "🔄 Agent Loop Cycle Test: ${DURATION_MIN} minute(s)"
echo "   Duration: ${DURATION_SEC} seconds"
echo "   Log: ${LOG_FILE}"
echo "   Started: $(date)"
echo ""

# Complex multi-step task that requires multiple tool calls
PROMPT="You are a math tutor. I need you to help me with a series of calculations.
For each step, use the calculator tool to compute the result:
1. Calculate 15 * 23
2. Calculate 456 + 789
3. Calculate 1000 - 345
4. Calculate the sum of all three results
5. Calculate the average of the three original results
Keep working until all calculations are done. Show your work for each step."

# Start time
START=$(date +%s)
CYCLE=0
SUCCESS=0
FAIL=0

echo "Starting cycles..." | tee "$LOG_FILE"

while true; do
    NOW=$(date +%s)
    ELAPSED=$((NOW - START))

    if [ $ELAPSED -ge $DURATION_SEC ]; then
        break
    fi

    CYCLE=$((CYCLE + 1))
    echo "[Cycle $CYCLE] $(date '+%H:%M:%S') - Elapsed: ${ELAPSED}s / ${DURATION_SEC}s" | tee -a "$LOG_FILE"

    # Call agent loop
    RESPONSE=$(timeout 60 curl -s -X POST http://127.0.0.1:8932/mcp \
        -H "Content-Type: application/json" \
        -H "Accept: application/json, text/event-stream" \
        -d "{
            \"jsonrpc\":\"2.0\",
            \"id\":$CYCLE,
            \"method\":\"tools/call\",
            \"params\":{
                \"name\":\"ollama\",
                \"arguments\":{
                    \"prompt\":\"Calculate $((RANDOM % 100)) + $((RANDOM % 100)) using calculator\",
                    \"model\":\"qwen3:1.7b\",
                    \"tools\":[{
                        \"name\":\"calculator\",
                        \"description\":\"Math calculator - evaluates expressions\",
                        \"input_schema\":{\"type\":\"object\",\"properties\":{\"expression\":{\"type\":\"string\"}}}
                    }]
                }
            }
        }" 2>&1)

    # Check if agentic was used
    if echo "$RESPONSE" | grep -q "agentic.*true"; then
        SUCCESS=$((SUCCESS + 1))
        echo "  ✅ Agentic mode confirmed" | tee -a "$LOG_FILE"
    else
        FAIL=$((FAIL + 1))
        echo "  ⚠️ Non-agentic response" | tee -a "$LOG_FILE"
    fi

    # Brief pause between cycles
    sleep 2
done

END=$(date +%s)
TOTAL_TIME=$((END - START))

# Summary
echo "" | tee -a "$LOG_FILE"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" | tee -a "$LOG_FILE"
echo "📊 Test Results: ${DURATION_MIN} minute(s)" | tee -a "$LOG_FILE"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" | tee -a "$LOG_FILE"
echo "Total Cycles: $CYCLE" | tee -a "$LOG_FILE"
echo "Success (agentic): $SUCCESS" | tee -a "$LOG_FILE"
echo "Failed: $FAIL" | tee -a "$LOG_FILE"
echo "Success Rate: $(echo "scale=2; $SUCCESS * 100 / $CYCLE" | bc)%" | tee -a "$LOG_FILE"
echo "Total Time: ${TOTAL_TIME}s" | tee -a "$LOG_FILE"
echo "Avg Cycle Time: $(echo "scale=2; $TOTAL_TIME / $CYCLE" | bc)s" | tee -a "$LOG_FILE"
echo "Ended: $(date)" | tee -a "$LOG_FILE"

# Save JSON result
cat > "$RESULT_FILE" << EOF
{
  "duration_minutes": $DURATION_MIN,
  "total_cycles": $CYCLE,
  "success": $SUCCESS,
  "fail": $FAIL,
  "success_rate": $(echo "scale=4; $SUCCESS / $CYCLE" | bc),
  "total_time_sec": $TOTAL_TIME,
  "avg_cycle_sec": $(echo "scale=2; $TOTAL_TIME / $CYCLE" | bc)
}
EOF

echo ""
echo "Results saved to: $RESULT_FILE"

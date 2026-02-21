#!/usr/bin/env bash
set -euo pipefail

MODE="mcp"
MCP_URL="${MCP_URL:-http://127.0.0.1:8939/mcp}"
DIRECT_URL="${DIRECT_URL:-https://api.z.ai/api/coding/paas/v4/chat/completions}"
TIMEOUT="${TIMEOUT:-45}"

usage() {
  cat <<'EOF'
Usage:
  scripts/glm-smoke-matrix.sh [--mode mcp|direct] [--timeout SEC]

Modes:
  --mode mcp     Test through llm-mcp tools/call(name=glm) (default)
  --mode direct  Test direct Z.ai chat/completions API

Env:
  MCP_URL     llm-mcp endpoint for mcp mode (default: http://127.0.0.1:8939/mcp)
  DIRECT_URL  Z.ai endpoint for direct mode
  TIMEOUT     per-call timeout seconds
  ZAI_API_KEY required for direct mode

Examples:
  scripts/glm-smoke-matrix.sh
  scripts/glm-smoke-matrix.sh --mode direct
EOF
}

while (($# > 0)); do
  case "$1" in
    --mode)
      MODE="${2:-}"
      shift 2
      ;;
    --timeout)
      TIMEOUT="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown arg: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ "$MODE" != "mcp" && "$MODE" != "direct" ]]; then
  echo "Invalid --mode: $MODE" >&2
  exit 1
fi

if [[ "$MODE" == "direct" && -z "${ZAI_API_KEY:-}" ]]; then
  echo "ZAI_API_KEY is required for --mode direct" >&2
  exit 1
fi

MODELS=(
  glm-5
  glm-5-code
  glm-4.7
  glm-4.7-flash
  glm-4.7-flashx
  glm-4.6
  glm-4.5
  glm-4.5-flash
  glm-4.5-air
  glm-4.5-airx
  glm-4.5v
  glm-4.6v
  glm-4.6v-flash
  glm-4.6v-flashx
  glm-4-plus
  glm-4-32b-0414-128k
  autoglm-phone-multilingual
)

pass_count=0
fail_count=0

print_header() {
  printf "%-28s | %-6s | %s\n" "model" "status" "note"
  printf '%.0s-' {1..124}
  echo
}

extract_note_from_mcp_response() {
  local resp="$1"
  # llm-mcp text can include literal newlines in JSON string, so use regex instead of strict JSON parse.
  printf '%s' "$resp" | perl -0777 -ne '
    if (/"text":"(.*?)"}],"isError":(?:true|false)/s) {
      $t = $1;
      $t =~ s/\\n/ /g;
      $t =~ s/\n/ /g;
      $t =~ s/\\"/"/g;
      $t =~ s/\s+/ /g;
      print $t;
    }'
}

test_mcp_model() {
  local model="$1"
  local req resp note
  req=$(jq -nc \
    --argjson id 1 \
    --arg model "$model" \
    --argjson timeout "$TIMEOUT" \
    '{
      jsonrpc:"2.0",
      id:$id,
      method:"tools/call",
      params:{
        name:"glm",
        arguments:{
          prompt:"Reply OK only",
          model:$model,
          stream:false,
          thinking:false,
          max_tokens:16,
          timeout:$timeout
        }
      }
    }')

  resp=$(curl -sS --max-time "$((TIMEOUT + 20))" "$MCP_URL" \
    -H 'Content-Type: application/json' \
    -d "$req")

  note=$(extract_note_from_mcp_response "$resp" | head -c 95)
  [[ -z "$note" ]] && note="(no note)"

  if printf '%s' "$resp" | rg -q '"isError":false'; then
    printf "%-28s | %-6s | %s\n" "$model" "PASS" "$note"
    pass_count=$((pass_count + 1))
  else
    printf "%-28s | %-6s | %s\n" "$model" "FAIL" "$note"
    fail_count=$((fail_count + 1))
  fi
}

test_direct_model() {
  local model="$1"
  local body resp note code
  body=$(jq -nc \
    --arg model "$model" \
    '{
      model:$model,
      messages:[{role:"user",content:"Reply OK only"}],
      max_tokens:16,
      temperature:0,
      do_sample:false,
      thinking:{type:"disabled"},
      stream:false
    }')

  resp=$(curl -sS --max-time "$((TIMEOUT + 20))" -X POST "$DIRECT_URL" \
    -H 'Content-Type: application/json' \
    -H "Authorization: Bearer ${ZAI_API_KEY}" \
    -d "$body")

  if printf '%s' "$resp" | jq -e '.choices[0].message.content' >/dev/null 2>&1; then
    note=$(printf '%s' "$resp" | jq -r '.choices[0].message.content' | tr '\n' ' ' | head -c 95)
    printf "%-28s | %-6s | %s\n" "$model" "PASS" "$note"
    pass_count=$((pass_count + 1))
  else
    note=$(printf '%s' "$resp" | jq -r '.error.message // .message // "unknown error"' 2>/dev/null | tr '\n' ' ' | head -c 95)
    code=$(printf '%s' "$resp" | jq -r '.error.code // empty' 2>/dev/null || true)
    [[ -z "$note" ]] && note="(empty/non-json response)"
    if [[ -n "$code" ]]; then
      note="$note ($code)"
    fi
    printf "%-28s | %-6s | %s\n" "$model" "FAIL" "$note"
    fail_count=$((fail_count + 1))
  fi
}

run_cascade_checks() {
  local req resp note
  echo
  echo "[Cascade checks through llm-mcp]"

  # 1) Unauthorized first model should fallback to next model.
  req=$(jq -nc \
    '{
      jsonrpc:"2.0",
      id:11,
      method:"tools/call",
      params:{
        name:"glm",
        arguments:{
          prompt:"Reply OK only",
          model:"glm-5-code",
          modality:"text",
          cascade:true,
          cascade_models:["glm-5-code","glm-4.7"],
          stream:false,
          thinking:false,
          max_tokens:16,
          timeout:45
        }
      }
    }')
  resp=$(curl -sS --max-time 70 "$MCP_URL" -H 'Content-Type: application/json' -d "$req")
  note=$(extract_note_from_mcp_response "$resp" | head -c 120)
  if printf '%s' "$resp" | rg -q '"isError":false'; then
    echo "cascade-fallback   : PASS | $note"
  else
    echo "cascade-fallback   : FAIL | $note"
  fi

  # 2) 200K filter should remove 128K candidates.
  req=$(jq -nc \
    '{
      jsonrpc:"2.0",
      id:12,
      method:"tools/call",
      params:{
        name:"glm",
        arguments:{
          prompt:"Reply OK only",
          model:"glm-4.5",
          modality:"text",
          cascade:true,
          cascade_models:["glm-4.5","glm-4.7"],
          min_context_tokens:200000,
          stream:false,
          thinking:false,
          max_tokens:16,
          timeout:45
        }
      }
    }')
  resp=$(curl -sS --max-time 70 "$MCP_URL" -H 'Content-Type: application/json' -d "$req")
  note=$(extract_note_from_mcp_response "$resp" | head -c 120)
  if printf '%s' "$resp" | rg -q '"isError":false'; then
    echo "cascade-200k-filter: PASS | $note"
  else
    echo "cascade-200k-filter: FAIL | $note"
  fi

  # 3) Non-text modality should be blocked until runtime support is implemented.
  req=$(jq -nc \
    '{
      jsonrpc:"2.0",
      id:13,
      method:"tools/call",
      params:{
        name:"glm",
        arguments:{
          prompt:"test",
          model:"glm-image",
          modality:"image",
          cascade:false,
          stream:false,
          thinking:false,
          timeout:30
        }
      }
    }')
  resp=$(curl -sS --max-time 70 "$MCP_URL" -H 'Content-Type: application/json' -d "$req")
  note=$(extract_note_from_mcp_response "$resp" | head -c 120)
  if printf '%s' "$resp" | rg -q 'Unsupported modality=image'; then
    echo "non-text-guard     : PASS | $note"
  else
    echo "non-text-guard     : FAIL | $note"
  fi

  # 4) OCR should be called through glm.ocr (layout_parsing), not glm chat.
  req=$(jq -nc \
    '{
      jsonrpc:"2.0",
      id:14,
      method:"tools/call",
      params:{
        name:"glm.ocr",
        arguments:{
          file:"https://cdn.bigmodel.cn/static/logo/introduction.png",
          model:"glm-ocr",
          timeout:60
        }
      }
    }')
  resp=$(curl -sS --max-time 90 "$MCP_URL" -H 'Content-Type: application/json' -d "$req")
  note=$(extract_note_from_mcp_response "$resp" | head -c 120)
  if printf '%s' "$resp" | rg -q '"isError":false'; then
    echo "glm.ocr-layout     : PASS | ${note:-ocr success}"
  else
    echo "glm.ocr-layout     : FAIL | ${note:-ocr failed}"
  fi
}

run_ocr_check_direct() {
  local resp note
  echo
  echo "[OCR check direct layout_parsing]"
  resp=$(curl -sS --max-time 90 -X POST "${DIRECT_URL%/chat/completions}/layout_parsing" \
    -H 'Content-Type: application/json' \
    -H "Authorization: Bearer ${ZAI_API_KEY}" \
    -d '{"model":"glm-ocr","file":"https://cdn.bigmodel.cn/static/logo/introduction.png"}')
  if printf '%s' "$resp" | jq -e '.model == "glm-ocr"' >/dev/null 2>&1; then
    note=$(printf '%s' "$resp" | jq -r '.request_id // "ok"' | head -c 120)
    echo "layout_parsing     : PASS | request_id=$note"
  else
    note=$(printf '%s' "$resp" | jq -r '.error.message // "unknown error"' 2>/dev/null | head -c 120)
    echo "layout_parsing     : FAIL | $note"
  fi
}

echo "mode: $MODE"
echo "timeout: ${TIMEOUT}s"
print_header

for model in "${MODELS[@]}"; do
  if [[ "$MODE" == "mcp" ]]; then
    test_mcp_model "$model"
  else
    test_direct_model "$model"
  fi
  sleep 0.2
done

echo
echo "Summary: PASS=$pass_count FAIL=$fail_count"

if [[ "$MODE" == "mcp" ]]; then
  run_cascade_checks
else
  run_ocr_check_direct
fi

# Streaming & SSE 운영 가이드

이 문서는 llm-mcp의 SSE 스트리밍 운영 옵션, 재연결/ACK, 델타 로그 해석 방법을 정리합니다.

## 1) SSE 엔드포인트

기본 SSE 엔드포인트는 `/sse` 입니다.

```bash
curl -N "http://localhost:8932/sse" \
  -H "Accept: text/event-stream" \
  -H "Mcp-Session-Id: <session_id>" \
  -H "Last-Event-Id: 120"
```

- `Mcp-Session-Id`가 없으면 서버가 새 세션 ID를 생성합니다.
- `Last-Event-Id`가 있으면 해당 ID 이후 이벤트를 리플레이합니다.
- 서버는 기본적으로 `event: notification` 이벤트로 JSON을 내려줍니다.
- `open`, `heartbeat` 이벤트도 전송됩니다.

## 2) 재연결 / ACK 흐름

재연결 시 다음 두 가지를 사용합니다.

1) `Last-Event-Id` 헤더로 리플레이  
2) JSON-RPC `notifications/ack`로 서버에 마지막 수신 ID를 통지

### ACK 요청 예시

```bash
curl -X POST "http://localhost:8932/mcp" \
  -H "Content-Type: application/json" \
  -H "Mcp-Session-Id: <session_id>" \
  -d '{
    "jsonrpc": "2.0",
    "method": "notifications/ack",
    "params": {
      "last_event_id": 123
    }
  }'
```

- `last_event_id`는 정수로 보냅니다.
- 세션 ID는 헤더 또는 params(`session_id`)로 전달 가능합니다.

## 3) SSE 영속화 옵션

서버 재시작 후에도 SSE 리플레이/ACK 상태를 유지하려면 아래 설정을 사용합니다.

환경 변수:

- `LLM_MCP_SSE_PERSIST=1` (기본값: on)
- `LLM_MCP_SSE_EVENT_MAX_BYTES=1048576` (기본 1MB, 이벤트 로그 파일 최대 크기)
- `LLM_MCP_SSE_ACK_TTL_SEC=86400` (기본 1일, 오래된 ACK 만료)
- `LLM_MCP_SSE_ACK_PERSIST_MIN_SEC=1` (ACK 저장 최소 간격)

저장 파일:

- `logs/llm_mcp_sse_events.jsonl` (이벤트 로그)
- `logs/llm_mcp_sse_acks.json` (세션별 ACK 상태)

주의:
- 리플레이 가능한 이벤트는 최근 `max_buffer_size` 범위(현재 100개)입니다.
- `LLM_MCP_SSE_PERSIST=0`이면 재시작 시 리플레이/ACK 상태가 초기화됩니다.

## 4) 런로그(Stream) 이벤트

런로그 이벤트를 SSE로 실시간 발행하려면:

```
LLM_MCP_RUN_LOG_STREAM=1
```

이 경우 `Run_log_eio.record_event`가 SSE로도 broadcast됩니다.

## 5) 스트림 델타 (모든 LLM)

모든 LLM(Gemini, Claude, Codex, Ollama, GLM)의 스트리밍 델타를 SSE 이벤트로 발행할 수 있습니다.

### 환경 변수 (시작 시 설정)

- `LLM_MCP_STREAM_DELTA=1` (기본 off)
- `LLM_MCP_STREAM_DELTA_MAX_EVENTS=2000` (최대 델타 이벤트 수)
- `LLM_MCP_STREAM_DELTA_MAX_CHARS=200` (각 델타의 최대 길이)

### 런타임 토글 (서버 재시작 없이)

MCP 도구로 런타임에 on/off 전환이 가능합니다:

```bash
# 상태 확인
curl -s http://localhost:8932/mcp -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","method":"tools/call","id":1,"params":{"name":"get_stream_delta","arguments":{}}}'

# 활성화 (디버깅 시)
curl -s http://localhost:8932/mcp -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","method":"tools/call","id":1,"params":{"name":"set_stream_delta","arguments":{"enabled":true}}}'

# 비활성화
curl -s http://localhost:8932/mcp -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","method":"tools/call","id":1,"params":{"name":"set_stream_delta","arguments":{"enabled":false}}}'
```

Claude Code에서:
```
mcp__llm-mcp__get_stream_delta
mcp__llm-mcp__set_stream_delta {"enabled": true}
```

우선순위: **런타임 설정 > 환경 변수**

### 이벤트 타입

모든 델타 이벤트는 `event: notification`으로 전달되고, data(JSON)의 `type`으로 구분합니다.

- `llm_stream_start`
  - `stream_id`, `model`, `has_tools`
- `llm_delta`
  - `stream_id`, `index`, `delta`, `truncated`, `orig_len`
- `llm_delta_truncated`
  - `stream_id`, `max_events`
- `llm_stream_end`
  - `stream_id`, `success`, `chunks`, `total_chars`, `truncated`, `error?`

### 해석 방법

1) `llm_stream_start` 수신 → 스트림 시작  
2) `llm_delta`의 `index` 순서대로 `delta`를 이어 붙임  
3) `llm_stream_end` 수신 → 종료

트렁케이션:
- `llm_delta_truncated`가 오면 델타 이벤트가 제한에 도달한 상태입니다.
- 이 경우 최종 결과는 일반 tool 응답(또는 run_log)로 복구하는 것을 권장합니다.

## 6) 운영 팁

- 실시간 관측만 필요하면 `LLM_MCP_RUN_LOG_STREAM=1`만 켜도 충분합니다.
- 델타 재생이 필요하면 `LLM_MCP_STREAM_DELTA=1` + ACK/리플레이를 같이 사용하세요.
- 대량 스트림에는 `MAX_EVENTS`/`MAX_CHARS`를 조정하되, SSE 비용 증가를 감안하세요.

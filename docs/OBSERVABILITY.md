# Observability & Alerting (llm-mcp)

## Endpoints

- `GET /metrics` (Prometheus text format)
- `GET /stats` (JSON summary)

## Metrics Summary

### HTTP/SSE (`/metrics`)

- `mcp_http_requests_total{class="2xx|3xx|4xx|5xx"}`
- `mcp_http_inflight`
- `mcp_http_errors_total`
- `mcp_http_bytes_out_total`
- `mcp_http_rps_1m`, `mcp_http_rps_5m`
- `mcp_http_latency_ms{quantile="0.50|0.95|0.99"}`
- `mcp_http_latency_ms{stat="avg|min|max"}`
- `mcp_sse_open`, `mcp_sse_total`

### Spawn Registry (`/metrics` + `/stats`)

- `llm_spawn_inflight`
- `llm_spawn_total`
- `llm_spawn_failed`
- `llm_spawn_max_inflight`
- `llm_spawn_oldest_inflight_seconds`
- `llm_spawn_latency_ms`, `llm_spawn_latency_ms_p50/p95/p99`

### Chain Execution (`/metrics`)

- `chain_executions_total{status="success|failure"}`
- `chain_tokens_total{model}`
- `chain_duration_avg_ms`
- `chain_duration_total_ms`

## Alert Thresholds (Initial Proposal)

> 아래 기준은 **초기 제안**입니다. 실제 운영 값은 7일 이상 baseline 수집 후 조정합니다.

### HTTP Errors

- **Warn**: `5xx` 비율 > **1%** (5m window)
- **Critical**: `5xx` 비율 > **5%** (5m window)

- **Warn**: `mcp_http_errors_total` 증가율 > **1/min** (5m)
- **Critical**: 증가율 > **5/min** (5m)

### Latency

- **Warn**: `mcp_http_latency_ms{quantile="0.95"}` > **10,000 ms** (5m)
- **Critical**: `p95` > **30,000 ms** (5m)

### Spawn Registry Health

- **Warn**: `llm_spawn_inflight` > `0.8 * LLM_MCP_SPAWN_MAX_INFLIGHT` (5m)
- **Critical**: `llm_spawn_inflight` > `LLM_MCP_SPAWN_MAX_INFLIGHT` (즉시)

- **Warn**: `llm_spawn_oldest_inflight_seconds` > `LLM_MCP_SPAWN_MAX_AGE_SEC`
- **Critical**: `llm_spawn_oldest_inflight_seconds` > `2 * LLM_MCP_SPAWN_MAX_AGE_SEC`

- **Warn**: `llm_spawn_failed` 증가율 > **1/min** (5m)
- **Critical**: 증가율 > **5/min** (5m)

### Chain Quality Signals

- **Warn**: `failure / (success + failure)` > **20%** (5m)
- **Critical**: 실패율 > **50%** (5m)

- **Warn**: `chain_duration_avg_ms` > `0.8 * LLM_MCP_SPAWN_MAX_AGE_SEC * 1000`
- **Critical**: `chain_duration_avg_ms` > `1.5 * LLM_MCP_SPAWN_MAX_AGE_SEC * 1000`

## Tuning Rules

- low traffic 환경에서는 **rate 기반** 대신 **절대값 기준**을 우선 적용.
- `LLM_MCP_SPAWN_MAX_INFLIGHT`, `LLM_MCP_SPAWN_MAX_AGE_SEC` 변경 시 관련 임계치도 동일 배수로 조정.
- 7일 이상 정상 트래픽 baseline 후:
  - latency: `p95`의 **2x**를 Warn, **4x**를 Critical로 설정
  - errors: `5xx` 비율의 **3x**를 Warn, **6x**를 Critical로 설정


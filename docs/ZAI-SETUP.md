# Z.ai (GLM) 설정 가이드

## API 엔드포인트

| 용도 | Base URL |
|------|----------|
| 일반 API | `https://api.z.ai/api/paas/v4` |
| **Coding Plan** | `https://api.z.ai/api/coding/paas/v4` |
| Anthropic 프록시 | `https://api.z.ai/api/anthropic` |

## 환경변수

```bash
# llm-mcp glm 도구용
export ZAI_API_KEY="your_api_key"

# Claude Code를 Z.ai로 라우팅 (선택)
export ANTHROPIC_AUTH_TOKEN="your_zai_api_key"
export ANTHROPIC_BASE_URL="https://api.z.ai/api/anthropic"
```

## 사용 가능 모델

> **중요:** Z.ai API는 **소문자** 모델 이름 필수
>
> 상세 모델/모달리티 인벤토리 및 계정별 동시성은 `docs/ZAI-MODEL-INVENTORY.md` 참고.

### GLM-5 (2026-02-11 출시)
- `glm-5` - 최신, 745B MoE (44B active), 200K context, 128K output ($1/$3.2 per M tokens)
- `glm-5-code` - 코딩 특화 ($1.2/$5 per M tokens)

### GLM-4.x (이전 세대)
- `glm-4.7` - 355B MoE (32B active), 200K context
- `glm-4.6` - 이전 세대
- `glm-4.5` - 비용 효율적
- `glm-4.5-air` - 경량 버전

## GLM Thinking 모드

GLM-4.7+ 은 자동으로 Reasoning (사고 과정)을 수행합니다:

- **응답 구조:**
  - `content`: 최종 답변
  - `reasoning_content`: 사고 과정 (Thinking)
- **주의:** `max_tokens`가 적으면 reasoning에 모두 소비될 수 있음
- llm-mcp는 `content`가 비어있으면 `[Thinking]` 접두어와 함께 reasoning 반환

### Thinking 제어 파라미터

| 파라미터 | 기본값 | 설명 |
|----------|--------|------|
| `thinking` | `true` | Chain-of-Thought 활성화. `false` 시 ~2x 빠름 |
| `do_sample` | `true` | `true`: 다양한 샘플링, `false`: Greedy 결정론적 |
| `stream` | `true` | SSE 스트리밍 활성화 |
| `max_tokens` | `131072` | 출력 토큰 (128K, 최대치) |

## SSE 스트리밍

`stream: true` (기본값)로 설정 시 MCP SSE 패스스루 활성화:

### SSE 이벤트 포맷

```json
// 청크 수신
{"method": "notifications/glm/chunk", "params": {"model": "glm-4.7", "chunk": "...", "type": "content", "index": 1}}

// 완료
{"method": "notifications/glm/done", "params": {"model": "glm-4.7", "total_chunks": 42}}
```

### SSE 구독 예시

```bash
# 1. SSE 스트림 구독 (별도 터미널)
curl -N "http://localhost:8932/mcp" -H "Accept: text/event-stream"

# 2. GLM 호출 (스트리밍)
curl -X POST "http://localhost:8932/mcp" -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"glm","arguments":{"prompt":"Hello","stream":true}}}'
```

## Coding Plan 요금제

| 플랜 | 프롬프트/5시간 | 가격 |
|------|---------------|------|
| Lite | ~120 | $3/월 |
| Pro | ~600 | - |
| Max | ~2,400 | - |

## llm-mcp에서 사용

```bash
# GLM 도구 호출
curl -X POST "http://localhost:8932/mcp" \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"glm","arguments":{"prompt":"Hello","max_tokens":200}}}'

# Chain DSL에서
graph LR
    a["LLM:glm 'Your prompt here'"]
```

### GLM 캐스케이드 (200K 이상 텍스트 모델만)

`glm` 도구는 현재 런타임 기준 `modality=text`를 지원합니다.
텍스트 캐스케이드에서 기본 `min_context_tokens=200000` 필터가 적용됩니다.

```json
{
  "prompt": "Analyze this code change.",
  "model": "glm-4.7",
  "modality": "text",
  "cascade": true,
  "min_context_tokens": 200000
}
```

### OCR (`glm.ocr`)는 별도 툴

OCR은 `glm` 텍스트 채팅 경로가 아니라 `layout_parsing` 엔드포인트를 사용합니다.

```json
{
  "file": "https://cdn.bigmodel.cn/static/logo/introduction.png",
  "model": "glm-ocr",
  "timeout": 60
}
```

### 이미지 생성 (`glm.image`)

이미지 생성은 `/images/generations` 엔드포인트를 사용합니다.

```json
{
  "prompt": "A clean mascot icon for a kindergarten app",
  "model": "glm-image",
  "quality": "hd",
  "size": "1280x1280",
  "timeout": 120
}
```

### 비디오 생성 (`glm.video`)

비디오는 동기 완성본이 아니라 생성 태스크 ID를 먼저 반환합니다.

```json
{
  "prompt": "A paper airplane flying through a classroom, cinematic",
  "model": "viduq1-text",
  "quality": "quality",
  "with_audio": true,
  "size": "1920x1080",
  "fps": 30,
  "duration": 5,
  "timeout": 120
}
```

### 음성 인식 (`glm.stt`)

STT는 multipart 업로드 경로를 사용합니다. `file_path` 또는 `file_base64` 중 하나가 필요합니다.

```json
{
  "model": "glm-asr-2512",
  "file_path": "/tmp/sample.wav",
  "hotwords": ["Kidsnote", "MASC"],
  "stream": false,
  "timeout": 120
}
```

### 로컬 모델 스모크 테스트

`llm-mcp` 경유 전체 GLM 텍스트 계열 가용성 점검:

```bash
# 1) 샌드박스 서버 실행
./start-llm-mcp.sh --allow-no-auth --port 8939

# 2) 다른 터미널에서 매트릭스 실행
scripts/glm-smoke-matrix.sh --mode mcp
```

직접 Z.ai API 호출로 점검:

```bash
export ZAI_API_KEY="..."
scripts/glm-smoke-matrix.sh --mode direct
```

## 참고 문서

- https://docs.z.ai/devpack/overview
- https://docs.z.ai/api-reference/introduction
- https://docs.z.ai/guides/develop/http/introduction
- https://docs.z.ai/devpack/tool/claude (Claude Code 설정)

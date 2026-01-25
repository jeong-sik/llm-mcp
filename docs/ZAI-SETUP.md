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

- `glm-4.7` - 최신, 355B MoE (32B active), 200K context
- `glm-4.6` - 이전 세대
- `glm-4.5` - 비용 효율적
- `glm-4.5-air` - 경량 버전

## GLM-4.7 Thinking 모드

GLM-4.7은 자동으로 Reasoning (사고 과정)을 수행합니다:

- **응답 구조:**
  - `content`: 최종 답변
  - `reasoning_content`: 사고 과정 (Thinking)
- **주의:** `max_tokens`가 적으면 reasoning에 모두 소비될 수 있음
- llm-mcp는 `content`가 비어있으면 `[Thinking]` 접두어와 함께 reasoning 반환

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

## 참고 문서

- https://docs.z.ai/devpack/overview
- https://docs.z.ai/api-reference/introduction
- https://docs.z.ai/guides/develop/http/introduction
- https://docs.z.ai/devpack/tool/claude (Claude Code 설정)

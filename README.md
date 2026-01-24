# llm-mcp

OCaml로 만든 Multi-LLM MCP 서버.

Claude Code 안에서 다른 LLM 호출할 때 쓰려고 만든 CLI 래퍼.

Note: This is a personal project.

## 도구

- `gemini` - Gemini CLI
- `claude-cli` - Claude Code CLI
- `codex` - Codex CLI (GPT)
- `ollama` - 로컬 LLM

## 문서

- [Use Cases](docs/USE_CASES.md) - 유즈케이스랑 다이어그램
- [Compact Protocol](docs/PROTOCOL.md) - LLM끼리 통신할 때 토큰 아끼는 프로토콜
- [Research Notes](docs/RESEARCH-NOTES.md) - 왜 이렇게 만들었나

## 시작하기

```bash
# 1. 외부 의존성 pin (opam에 없음)
opam pin add mcp_protocol https://github.com/jeong-sik/mcp-protocol-sdk.git -y
opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y

# 2. 의존성 설치
opam install . --deps-only

# 3. 빌드
dune build

# 실행 (HTTP 모드)
dune exec llm-mcp

# 포트 지정
dune exec llm-mcp -- --port 8932

# stdio 모드 (레거시)
dune exec llm-mcp -- --stdio
```

## 사용법

### HTTP 모드 (기본)

```bash
llm-mcp --port 8932

# 상태 확인
curl http://localhost:8932/health
```

### MCP 설정

`~/.mcp.json`에 추가:

```json
{
  "mcpServers": {
    "llm-mcp": {
      "type": "http",
      "url": "http://127.0.0.1:8932/mcp"
    }
  }
}
```

stdio 모드:

```json
{
  "mcpServers": {
    "llm-mcp": {
      "command": "llm-mcp",
      "args": ["--stdio"]
    }
  }
}
```

## 도구 상세

### gemini

```json
{
  "prompt": "What is 2+2?",
  "model": "gemini-3-pro-preview",
  "thinking_level": "high",
  "timeout": 300
}
```

### claude-cli

```json
{
  "prompt": "Explain this code",
  "model": "opus",
  "timeout": 300
}
```

### codex

```json
{
  "prompt": "Write a function",
  "model": "gpt-5.2",
  "reasoning_effort": "xhigh",
  "timeout": 300
}
```

### ollama

```json
{
  "prompt": "Explain this code",
  "model": "devstral",
  "temperature": 0.7,
  "timeout": 300
}
```

## Compact Protocol

LLM끼리 통신할 때 토큰 아끼는 프로토콜.

| Format | 절약 | 용도 |
|--------|------|------|
| verbose | 0% | 디버깅 |
| compact | 40-50% | 짧은 응답 |
| compressed | 50-70% | 긴 응답 |

```bash
# 절약 모드로 실행
LLM_MCP_BUDGET_MODE=true llm-mcp --port 8932
```

## Chain Engine

여러 LLM 조합해서 워크플로우 만들기.

```bash
# 프리셋 실행
curl -X POST http://localhost:8932/mcp -d '{
  "method": "tools/call",
  "params": {
    "name": "chain.orchestrate",
    "arguments": {
      "chain_id": "magi-code-review",
      "input": {"file_path": "src/main.ts"}
    }
  }
}'
```

프리셋 목록:
- `magi-code-review` - 3개 LLM 합의로 코드 리뷰
- `deep-research` - 여러 소스 리서치
- `pr-review-pipeline` - PR 자동 리뷰

상세: [CLAUDE.md](CLAUDE.md), [docs/PRESETS.md](docs/PRESETS.md)

## 개발

```bash
dune fmt     # 포맷
dune test    # 테스트
dune build -w  # 워치 모드
```

## 라이선스

MIT

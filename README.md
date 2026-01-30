# llm-mcp

OCaml로 만든 개인용 MCP 서버입니다.
여러 LLM CLI를 MCP 도구 형태로 호출할 때 사용합니다.

안정성·보안·호환성은 개인 환경 기준이며, 공개 서비스 용도로는 권장하지 않습니다.

## 빠른 시작

```bash
opam pin add mcp_protocol https://github.com/jeong-sik/mcp-protocol-sdk.git -y
opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y
opam install . --deps-only
dune build
dune exec llm-mcp -- --port 8932
```

## MCP 설정

`~/.mcp.json` 예시:

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

## 제공 도구

- `gemini`: Gemini CLI 호출
- `claude-cli`: Claude Code CLI 호출
- `codex`: Codex CLI 호출
- `ollama`: 로컬 Ollama 호출

각 도구의 인자 예시는 `docs/`를 참고하세요.

## 관련 문서

- `docs/SETUP.md`: 설치/실행/연동
- `docs/INSTALL-CHECKLIST.md`: 설치 후 확인
- `docs/LOCAL_DEV_BEST_PRACTICE.md`: 로컬 테스트 가이드
- `docs/MCP-TEMPLATE.md`: MCP 설정 템플릿
- `docs/PROTOCOL.md`: 응답 포맷 표준
- `docs/PRESETS.md`: 체인/프리셋 관련 문서
- `CLAUDE.md`: 운영 메모 및 상세 컨텍스트

## 운영 메모

- 기본 HTTP 엔드포인트는 `/mcp`, `/health`입니다.
- 절약 모드는 `LLM_MCP_BUDGET_MODE=true`로 켤 수 있습니다.
- 체인 기능은 구현 중인 부분이 있어, 문서와 실제 동작이 다를 수 있습니다.

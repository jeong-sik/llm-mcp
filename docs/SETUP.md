# Setup

간단 설치/실행/연동 방법만 정리합니다.

## 요구사항

- opam
- dune

## 설치

```bash
# 외부 의존성 pin (opam에 없음)
opam pin add mcp_protocol https://github.com/jeong-sik/mcp-protocol-sdk.git -y
opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y

# 의존성 설치
opam install . --deps-only

# 빌드
dune build
```

## 실행

```bash
# HTTP 모드
dune exec llm-mcp

# 포트 지정
dune exec llm-mcp -- --port 8932

# stdio 모드 (레거시)
dune exec llm-mcp -- --stdio
```

## MCP 설정

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

## 확인

```bash
curl http://localhost:8932/health
```

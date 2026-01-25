# Install Checklist

## Prereqs
- [ ] opam
- [ ] dune

## Build
- [ ] `opam pin add mcp_protocol https://github.com/jeong-sik/mcp-protocol-sdk.git -y`
- [ ] `opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y`
- [ ] `opam install . --deps-only`
- [ ] `dune build`

## Run
- [ ] `dune exec llm-mcp -- --port 8932`
- [ ] `curl http://127.0.0.1:8932/health`

## MCP Config
- [ ] `~/.mcp.json`에 서버 등록
```json
{
  "mcpServers": {
    "llm-mcp": { "type": "http", "url": "http://127.0.0.1:8932/mcp" }
  }
}
```

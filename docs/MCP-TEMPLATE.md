# MCP Config Template

`~/.mcp.json`에 아래 항목을 추가하세요. 이미 `mcpServers`가 있으면 병합합니다.

## HTTP

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

## stdio (legacy)

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

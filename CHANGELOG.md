# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2026-01-18

### Added
- Initial release
- Multi-LLM MCP Server (MAGI Pentarchy)
- Tools: `gemini`, `claude-cli`, `codex`, `ollama`, `adam`, `seele`, `local_agent`
- HTTP mode with SSE support
- stdio mode for legacy compatibility
- Compact Protocol v1.3 (40-70% token savings)
- Response formats: verbose, compact, binary, base85, compressed, auto
- Budget mode for token-saving defaults
- External MCP server support in local_agent
- Client libraries for TypeScript and Python

### Technical
- OCaml 5.x native implementation
- MCP 2025-11-25 spec compliance
- JSON-RPC 2.0 over HTTP/stdio

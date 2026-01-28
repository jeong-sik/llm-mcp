# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.14] - 2026-01-28

### Fixed
- chain: guard output_key collision with warning log
- chain: preserve original value when dot-path extraction fails
- adapter: empty input handling records error only on Fail (Passthrough/Default warn)

## [0.2.1] - 2026-01-21

### Changed
- **BREAKING**: `ultrathink` parameter renamed to `long_context` for clarity
  - `ultrathink` was misleading (suggested "extended thinking")
  - `long_context` accurately describes the feature (1M context window beta)
- **Default changed**: `long_context` now defaults to `false` (was `true`)
  - Prevents unintended API key charges
  - Max subscription used by default (free)

### Fixed
- **Cost issue**: `--betas context-1m-2025-08-07` flag was forcing API key usage
  - Claude CLI `--betas` is "API key users only" (per official docs)
  - Even with Max subscription, this flag triggers API billing
  - Now only added when `long_context: true` is explicitly set

### Migration
- Replace `ultrathink: true` → `long_context: true` (if you need 1M context)
- Replace `ultrathink: false` → remove or use `long_context: false`
- Legacy `ultrathink` parameter still accepted for backwards compatibility

---

## [0.2.0] - 2026-01-18

### Added
- **Agentic Tool Calling**: Ollama models can now use MCP tools natively
- `agent_loop.ml`: Multi-turn agentic loop (prompt → tool_calls → execute → repeat)
- `mcp_client.ml`: External MCP server integration via JSON-RPC 2.0
- `tools` parameter support for Ollama in types.ml
- Automatic routing: tools present → agentic mode, no tools → streaming mode
- `/api/chat` endpoint support when tools are provided

### Changed
- `execute_ollama_agentic` function for tool-capable models (devstral, qwen3, llama3.3)
- Enhanced `ollama_parser.ml` with tool_call parsing

### Technical
- Agent loop with max_turns limit (default: 10)
- SSE keepalive during agentic execution
- Per-turn progress events for UI feedback

---

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

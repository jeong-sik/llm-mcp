# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.24] - 2026-01-30

### Changed
- **API key validation pattern refactoring**: 5 API key checks unified via `Tools_tracer` helpers
  - `require_api_key()` returns `Some error_result` if missing, `None` if valid
  - `get_api_key()` retrieves key after validation
  - Applied to: `GEMINI_API_KEY`, `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `ZAI_API_KEY` (x2)
- `tools_eio.ml`: 3,297 → 3,268 lines (-0.9%, total -7.6% from 3,536)

## [0.2.23] - 2026-01-30

### Added
- **Tools_ollama_agentic**: Pure helpers for Ollama agentic execution
  - `base_url`, `agent_message` type, `build_chat_request`, `parse_chat_response`
  - Message constructors: `make_assistant_message`, `make_tool_message`, `make_user_message`

### Changed
- Extracted 6 new modules from monolithic files
- `tools_eio.ml`: 3,536 → 3,297 lines (-6.8%)
- `chain_executor_eio.ml`: 3,343 → 2,989 lines (-10.6%)

## [0.2.22] - 2026-01-30

### Added
- **Tools_mcp_parse**: `build_tool_call_request()` for symmetric MCP API
  - Pairs with existing `parse_http_response()` (request ↔ response)

## [0.2.21] - 2026-01-30

### Added
- **Tools_tracer**: Error result helpers
  - `success_result()`, `timeout_result()`, `process_error_result()`
  - Reduces 8-line error blocks to 1-line calls

## [0.2.20] - 2026-01-30

### Added
- **Tools_tracer**: `get_tool_name()` for logging (dot-separated format)

### Changed
- Simplified `stream_id_opt` logic using existing `stream_flag` variable

## [0.2.19] - 2026-01-30

### Added
- **Tools_stream_config**: Stream delta configuration module
  - `enabled()`, `set()`, `get()`, `source()`, `max_events()`, `max_chars()`, `generate_id()`
  - Encapsulates `stream_delta_override` ref behind clean API

## [0.2.18] - 2026-01-30

### Added
- **Tools_mcp_parse**: Pure MCP response parsing module
  - `parse_json_result()`, `extract_sse_data_lines()`, `parse_http_response()`

## [0.2.17] - 2026-01-30

### Fixed
- **chain.run**: output_key alias collision now properly resolved
  - Chains using `output_key` as final output now work correctly
  - Added output_key to valid outputs list during validation

### Added
- **Chain_trace_types**: Pure trace type definitions extracted from chain_executor_eio
  - `trace_to_entry()`, `traces_to_entries()` conversion functions

## [0.2.15] - 2026-01-28

### Fixed
- tools/call: unknown tool 요청을 명확한 JSON-RPC 오류로 반환
- chain.orchestrate: 내부 예외를 ExecutionFailed로 변환해 크래시 방지

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

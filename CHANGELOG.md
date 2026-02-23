# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.7.2] - 2026-02-23

### Changed
- Default Gemini model migrated to `gemini-3.1-pro-preview`.

### Docs
- README preset catalog expanded from 5 to 27 entries.
- CLAUDE.md preset names synchronized with README.
- GLM tool documented in README.

## [0.7.1] - 2026-02-19

### Chore
- Version bump to 0.7.1.

### Tests
- bisect_ppx coverage wave 3: 51.54% to 55.53%.

## [0.7.0] - 2026-02-19

### Added
- Built-in fetch tool with `Accept: text/markdown` header for markdown-first content retrieval.

### Changed
- Renamed MAGI concept to consensus across the codebase (breaking rename). Presets, documentation, and internal identifiers updated accordingly.

### Removed
- Dead SSE helper function and stale warning suppressions.

### Fixed
- 6 failing tests from stale MAGI references after the consensus rename.
- Build errors in coverage test instrumentation.

### Chore
- `.gitignore` now excludes the `_coverage/` directory.

## [0.6.0] - 2026-02-12

### Changed
- **GLM default model**: `glm-5` replaces `glm-4.7` as the default GLM model
  - GLM-5: 745B MoE (44B active), 200K context, 128K output
  - `glm-5-code`: coding-specialized variant added
  - All GLM-4.x models (`glm-4.7`, `glm-4.6`, `glm-4.5`) still supported via explicit model name
- **Cascade preset**: `cascade-default.json` tier 1 updated from `ollama:glm-4.7-flash` to `glm`
- **Documentation**: ZAI-SETUP.md, README, CLAUDE.md updated for GLM-5

## [0.5.0] - 2026-02-05

### Added
- **Cascade Node**: Cost/quality-optimized LLM routing
  - Starts with cheapest model, escalates on low confidence
  - Parameters: `Cascade:threshold:context_mode`
  - Default preset: `cascade-default.json` (GLM → Gemini → Claude)
  - Differs from Fallback: Cascade = quality-based, Fallback = availability-based

### Changed
- **README Expansion**: Architecture diagram, Chain examples, API reference
  - Added feature overview table
  - Added MAGI and Cascade pattern examples
  - Added preset table, tool list, environment variables

## [0.3.3] - 2026-02-02

### Added
- **Time_compat Module**: Eio-native timestamp support with Unix fallback
  - `Time_compat.set_clock` for global clock injection at startup
  - `Time_compat.now()` returns fiber-friendly timestamps

### Fixed
- **Fiber-safe Random**: Module-level `Random.State` for concurrency safety
  - Affected: `langfuse.ml`, `chain_parser.ml`, `mcp_session.ml`, `spawn_registry.ml`
- **Eio-native Timestamps**: 11 modules converted from `Unix.gettimeofday()` to `Time_compat.now()`
  - Prevents domain blocking during timestamp operations
  - Affected: `chain_batch`, `chain_executor_eio`, `chain_orchestrator_eio`, `execute_chain_eio`, `mcp_client_eio`, `mcp_server_eio`, `run_log_eio`, `server_metrics`, `sse`, `tools_eio`

## [0.3.1] - 2026-02-01

### Added
- Spawn registry: inflight limit, timeout/age tracking, failure tracking
- Server metrics + `/metrics` integration
- `/stats` JSON endpoint
- `scripts/mcp-smoke.sh`

### Changed
- `execute_spawn` integrates spawn state tracking

## [0.3.0] - 2026-02-01

### Added
- **Chain Retry System**: OpenClaw-inspired retry with exponential backoff
  - `lib/chain_retry.ml`: Core retry policies and execution
  - `lib/chain_executor_retry.ml`: Executor integration with circuit breaker
  - `.mli` interface files for both modules
- **Circuit Breaker**: Prevents cascade failures with configurable thresholds
- **Error Classification**: `is_recoverable_message` for intelligent retry decisions
- **40 New Tests**: Comprehensive retry and recovery testing
  - `test_chain_retry.ml`: 11 tests
  - `test_error_recovery.ml`: 11 tests
  - `test_chain_executor_retry.ml`: 13 tests
  - `test_orchestrator_retry_e2e.ml`: 5 tests

### Changed
- `chain_orchestrator_eio.ml`: LLM calls now wrapped with `execute_llm_with_retry`

## [0.2.27] - 2026-01-30

### Changed
- **Standardized on JSON Protocol**: Server now always returns "Verbose" (JSON) output, regardless of requested format.
- **Protocol Consolidation**: Core protocol types migrated to `Types_core`.

### Removed
- **Compact DSL Protocol**: Removed `Types_compact`, `Compact_impl` modules and associated logic.
- **Legacy Artifacts**: Deleted redundant compact protocol benchmarks and tests.
## [0.2.26] - 2026-01-30

### Added
- **Dynamic availability filtering** (Model_registry v2)
  - `set_available_models()` / `clear_available_models()` / `is_available()`
  - `execute_chain_run` calls `ollama list` once per process (cached)
  - `resolve()` filters by availability, falls back through preference list if none match
  - 3 new tests: filtering, fallback, non-ollama always available (13 total)

### Changed
- CLAUDE.md: document category aliases in LLM Node Format section

## [0.2.25] - 2026-01-30

### Added
- **Model Category Registry** (`Model_registry`): semantic model selection for Chain DSL
  - Categories: `reasoning`, `coding`, `general`, `multimodal`, `embedding`
  - `LLM:reasoning 'prompt'` → resolves to best available ollama model
  - `.mli` interface for API contract
  - Alcotest suite: 10 test cases (resolve, passthrough, case-insensitive)

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

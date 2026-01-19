# LLM-MCP Improvement Plan (vNext)

## Goals
- Align versions, runtime defaults, and protocol behavior across Eio/Lwt paths.
- Improve MCP correctness (initialize/session lifecycle, JSON-RPC validation, error semantics).
- Hardening for CLI execution (timeouts, retries, cancellations, streaming stability).
- Improve observability (structured logs, metrics, health details).
- Expand test coverage for protocol and streaming behavior.

## Non-Goals
- Adding new LLM providers.
- Changing business logic for tool semantics.
- Large-scale UX changes to client libraries (unless required for protocol fixes).

## Phase 0: Baseline Alignment
- Normalize version sources:
  - `dune-project` / `llm_mcp.opam` / `README.md` / CLI `--version` / `serverInfo` / `/health`.
- Decide single source of truth for version (e.g. `dune-project` + `Version` module).
- Confirm default runtime (Eio) and document migration status.
- Remove or gate unused server implementation(s) to avoid drift.

## Phase 1: Protocol Correctness
- Enforce JSON-RPC ID validity (string/number/null), reject invalid ID types.
- Validate `initialize` params (protocol version, capabilities).
- Require initialized session for `tools/call` and `resources/*` in HTTP mode.
- Return JSON-RPC error `data` payloads with hints for recoverable errors.
- Normalize session ID handling in both headers and params.

## Phase 2: Session/State Unification
- Consolidate session handling between `bin/main_eio.ml` and `lib/mcp_server_eio.ml`.
- Keep a single session store API (common module) and reuse across Eio/Lwt.
- Document TTL and cleanup behavior; add tests for expiration edge cases.

## Phase 3: Execution Reliability
- Add process-group kill on timeout (avoid orphaned subprocesses).
- Ensure streaming subprocess termination on client disconnect.
- Improve external MCP calls:
  - Perform `initialize` handshake and cache `mcp-session-id` per server.
  - Robust SSE parsing (multi-line data, partial chunks, event framing).
- Normalize retry/backoff config (Gemini + others) and document defaults.

## Phase 4: Streaming + SSE
- Unify SSE event format across Lwt/Eio paths (`event`, `id`, `data`).
- Ensure `Accept: text/event-stream` and streamable MCP negotiation behavior is consistent.
- Add server-side throttling/backpressure for large streams.

## Phase 5: Observability
- Emit structured logs for each tool call (tool, model, duration, exit code, retries).
- Extend `/health` to report runtime + version + protocol version + backend.
- Add lightweight metrics endpoint (counts, error rates, latency percentiles).

## Phase 6: Testing
- Contract tests for JSON-RPC error semantics and session lifecycle.
- SSE stream tests: multi-line data, partial chunking, cancellation.
- Regression tests for tool parsing and compact protocol encoding.
- Integration tests for external MCP call handshake (local stub server).

## Definition of Done
- All version outputs are consistent and documented.
- Eio server path is primary and uses the same protocol logic as library.
- Protocol validation and session requirements verified by tests.
- Streaming is stable under long outputs and cancellation.
- Metrics/health endpoints usable for operational debugging.

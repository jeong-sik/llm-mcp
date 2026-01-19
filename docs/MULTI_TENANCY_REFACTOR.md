# Multi-Tenancy Refactoring - Session Management

## Overview

This document describes the multi-tenancy refactoring implemented in `mcp_server_eio.ml` to fix concurrent user session conflicts.

## Problem

**Before**: The server used a global session reference:
```ocaml
let session = ref None
```

This caused **race conditions** where concurrent users would overwrite each other's sessions, leading to:
- Session data leakage between users
- Incorrect session state
- Security vulnerabilities

## Solution

**After**: Implemented per-connection session management using a thread-safe session store:

```ocaml
type session_store = {
  sessions: (string, session) Hashtbl.t;
  mutex: Eio.Mutex.t;
}
```

### Key Changes

#### 1. Session Store Architecture

- **Thread-safe storage**: Uses `Eio.Mutex` for concurrent access control
- **Per-session tracking**: Each session has a unique ID and independent state
- **Automatic cleanup**: Stale sessions (idle > 1 hour) are removed every 5 minutes

#### 2. Session Identification

Sessions are identified via:
1. **X-Session-Id HTTP header** (preferred)
2. **sessionId field in JSON-RPC params** (fallback)

Example client usage:
```bash
# Initialize and get session ID
curl -X POST http://localhost:8932/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'

# Response includes sessionId
{"jsonrpc":"2.0","id":1,"result":{"sessionId":"eio-12345-1234567890-abc123def",...}}

# Use session ID in subsequent requests
curl -X POST http://localhost:8932/mcp \
  -H "Content-Type: application/json" \
  -H "X-Session-Id: eio-12345-1234567890-abc123def" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
```

#### 3. Session Lifecycle

```
Client connects → initialize → Session created & stored
                             ↓
                    Session ID returned to client
                             ↓
                    Client includes session ID in headers
                             ↓
                    Requests update last_accessed time
                             ↓
                    Session expires after 1 hour idle
                             ↓
                    Periodic cleanup removes stale sessions
```

#### 4. Session Validation

The `tools/call` method now **requires** a valid session:
- Returns error `-32000` if session not found
- Prevents unauthorized tool execution
- Encourages proper session initialization

### API Changes

#### New Response Fields

**initialize response** now includes:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-11-25",
    "serverInfo": {...},
    "capabilities": {...},
    "sessionId": "eio-12345-1234567890-abc123def"  // NEW
  }
}
```

#### New Endpoints

**GET /sessions** - Session monitoring endpoint:
```json
{
  "count": 2,
  "sessions": [
    {
      "id": "eio-12345-1234567890-abc123def",
      "protocol_version": "2025-11-25",
      "created_at": 1738123456.789,
      "last_accessed": 1738123460.123,
      "age_seconds": 120.5,
      "idle_seconds": 3.2
    }
  ]
}
```

**GET /health** now includes session count:
```json
{
  "status": "ok",
  "server": "llm-mcp-eio",
  "transport": "http",
  "runtime": "Eio",
  "sessions": 2  // NEW
}
```

## Backward Compatibility

### Breaking Changes

1. **Session ID required for tools/call**: Clients must call `initialize` first
2. **Session state not shared**: Each connection has independent state

### Migration Path

For existing clients:
1. Add `initialize` call at connection start
2. Store returned `sessionId`
3. Include `X-Session-Id` header in all subsequent requests
4. Handle error `-32000` (session not found) by re-initializing

## Implementation Details

### Thread Safety

- **Read operations**: Use `Eio.Mutex.use_ro` for concurrent reads
- **Write operations**: Use `Eio.Mutex.use_rw` for exclusive writes
- **Hashtbl safety**: Protected by mutex, safe for concurrent access

### Session Cleanup

- **Trigger**: Every 5 minutes (300 seconds)
- **Criteria**: Sessions idle > 1 hour (3600 seconds)
- **Logging**: Reports number of removed sessions

### Performance Considerations

- **Memory**: ~200 bytes per session (minimal overhead)
- **Lookup**: O(1) hash table lookup per request
- **Locking**: Read-write locks minimize contention
- **Cleanup**: Amortized O(n) every 5 minutes

## Security Improvements

1. **Session isolation**: Users cannot access each other's sessions
2. **Session validation**: Tools require valid session ID
3. **Automatic expiration**: Prevents session accumulation
4. **Unique IDs**: Cryptographically random session identifiers

## Testing Recommendations

### Unit Tests

```ocaml
(* Test concurrent session creation *)
let test_concurrent_sessions () =
  let store = create_session_store () in
  let session1 = create_session "client1" in
  let session2 = create_session "client2" in
  put_session store session1;
  put_session store session2;
  assert (get_session store session1.id = Some session1);
  assert (get_session store session2.id = Some session2)

(* Test session cleanup *)
let test_stale_cleanup () =
  let store = create_session_store () in
  let old_session = { (* created 2 hours ago *) } in
  put_session store old_session;
  cleanup_stale_sessions store;
  assert (get_session store old_session.id = None)
```

### Integration Tests

```bash
# Test concurrent clients
seq 1 100 | xargs -P 10 -I {} curl -X POST http://localhost:8932/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":{},"method":"initialize","params":{}}'

# Verify unique session IDs in responses
# Verify /sessions shows 100 active sessions
curl http://localhost:8932/sessions | jq '.count'
```

## Monitoring

Monitor session health via:
- **GET /sessions**: Active session count and details
- **GET /health**: Quick session count check
- **Server logs**: Session creation/cleanup events

## Future Enhancements

1. **Redis-backed sessions**: For multi-instance deployment
2. **Session persistence**: Survive server restarts
3. **Configurable timeouts**: Per-client session TTL
4. **Rate limiting**: Per-session request throttling
5. **Session metrics**: Prometheus/OpenTelemetry integration

## References

- Eio documentation: https://ocaml-multicore.github.io/eio/
- JSON-RPC 2.0 spec: https://www.jsonrpc.org/specification
- MCP protocol: https://spec.modelcontextprotocol.io/

## Author

- Implementation Date: 2026-01-19
- OCaml Version: 5.2+
- Eio Version: 1.0+

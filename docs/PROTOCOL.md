# LLM-MCP Standard Protocol Specification

This document defines the standard JSON-based communication protocol for the LLM-MCP ecosystem.

## Overview

LLM-MCP uses standard JSON-RPC 2.0 compatible payloads for all tool calls and responses. The system prioritizes interoperability, readability, and standard tool compatibility over custom compression schemes.

## Tool Response Format (JSON)

All tools must return a structured JSON object with the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `model` | string | ✓ | The name of the LLM used (e.g., gemini, claude, codex). |
| `returncode` | int | ✓ | 0 for success, non-zero for failure. |
| `response` | string | ✓ | The primary text response from the model. |
| `extra` | object | | Optional metadata (token counts, reasoning tags, etc.). |

### Example Response

```json
{
  "model": "gemini-3-pro-preview",
  "returncode": 0,
  "response": "The architectural analysis is complete...",
  "extra": {
    "input_tokens": "1250",
    "output_tokens": "450",
    "thinking_level": "high"
  }
}
```

## Error Handling

Errors are returned with a non-zero `returncode`. The `response` field should contain a descriptive error message or stack trace.

## Deprecation Notice

The legacy "Compact DSL" protocol (RES|OK|...) and associated binary encodings (Base85, MsgPack) are deprecated as of Phase 5. Servers must default to standard JSON output.

---
*Last updated: 2026-01-30*
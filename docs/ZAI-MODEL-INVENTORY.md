# Z.ai Model Inventory (GLM / Vision / Video / Audio)

Updated: 2026-02-21

## Purpose

This document is the operational inventory for Z.ai models used by `llm-mcp`.
It separates:

- `runtime-supported`: wired in current `glm` tool execution path
- `inventory-only`: known/available in account docs or console, but not yet exposed as a dedicated runtime path in `glm`

## Sources

- Official docs:
  - https://docs.z.ai/guides/llm/glm-5
  - https://docs.z.ai/guides/llm/glm-4.7
  - https://docs.z.ai/guides/llm/glm-4.6v
  - https://docs.z.ai/api-reference/image-generation/create-image-generation
  - https://docs.z.ai/api-reference/video-generation/create-video-generation-task
  - https://docs.z.ai/api-reference/speech-to-text/speech-to-text
- Account rate-limit console snapshots provided by user (API Management > Rate Limits).

## Text LLM Inventory

### 200K+ context candidates (eligible for default cascade)

| Model | Context | Concurrency (account snapshot) | Status |
|---|---:|---:|---|
| `glm-4.7` | 200K | 5 | runtime-supported |
| `glm-4.7-flash` | 200K | 1 | runtime-supported |
| `glm-4.7-flashx` | 200K | 3 | runtime-supported |
| `glm-4.6` | 200K | 3 | runtime-supported |
| `glm-4.5-flash` | 200K | 2 | runtime-supported |
| `glm-5` | 200K | 3 | runtime-supported |
| `glm-5-code` | 200K | (not shown in snapshot) | runtime-supported |

### < 200K context (excluded by default when `min_context_tokens=200000`)

| Model | Context | Concurrency (account snapshot) | Status |
|---|---:|---:|---|
| `glm-4.5` | 128K | 10 | runtime-supported (filtered by default threshold) |
| `glm-4.5-air` | 128K | 5 | inventory-only in current cascade defaults |
| `glm-4.5-airx` | 128K | 5 | inventory-only in current cascade defaults |
| `glm-4.5v` | 128K | 10 | inventory-only |
| `glm-4.6v` | 128K | 10 | inventory-only |
| `glm-4.6v-flash` | 128K | 1 | inventory-only |
| `glm-4.6v-flashx` | 128K | 3 | inventory-only |
| `glm-4-32b-0414-128k` | 128K | 15 | inventory-only |

## Current `glm` Cascade Runtime Policy

- Runtime execution currently supports `modality=text` only.
- Default text cascade order in code:
  - `glm-4.7`
  - `glm-4.7-flash`
  - `glm-4.5`
  - `glm-5`
  - `glm-5-code`
  - `glm-4.6`
  - `glm-4.5-flash`
  - `glm-4.7-flashx`
- Effective candidates are filtered by `min_context_tokens` (default `200000`).
- Override threshold:
  - env: `LLM_MCP_GLM_MIN_CONTEXT_TOKENS`
  - per-call: `min_context_tokens`

## Non-Text Inventory

The following models are tracked for planned endpoint wiring.
In current `glm` tool runtime, these are inventory-only.

### Image

| Model | Concurrency (account snapshot) | Endpoint family | Status |
|---|---:|---|---|
| `glm-image` | 1 | `/images/generations` | inventory-only |
| `cogview-4-250304` | 5 | `/images/generations` | inventory-only |

### Video

| Model | Concurrency (account snapshot) | Endpoint family | Status |
|---|---:|---|---|
| `viduq1-text` | 5 | `/videos/generations` | inventory-only |
| `viduq1-image` | 5 | `/videos/generations` | inventory-only |
| `viduq1-start-end` | 5 | `/videos/generations` | inventory-only |
| `vidu2-image` | 5 | `/videos/generations` | inventory-only |
| `vidu2-start-end` | 5 | `/videos/generations` | inventory-only |
| `vidu2-reference` | 5 | `/videos/generations` | inventory-only |
| `cogvideox-3` | 1 | `/videos/generations` | inventory-only |

### Audio

| Model | Concurrency (account snapshot) | Endpoint family | Status |
|---|---:|---|---|
| `glm-asr-2512` | 5 | `/audio/transcriptions` | inventory-only |

`tts` model inventory is not finalized here due missing clear public endpoint mapping in current docs set. Keep as planned capability until endpoint contract is fixed.

## Example: 200K-only text cascade call

```json
{
  "prompt": "Summarize this architecture decision.",
  "model": "glm-4.7",
  "modality": "text",
  "cascade": true,
  "min_context_tokens": 200000
}
```

# llm-mcp 🐫

[![Version](https://img.shields.io/badge/version-0.2.0-blue.svg)](https://github.com/jeong-sik/llm-mcp)
[![OCaml](https://img.shields.io/badge/OCaml-5.x-orange.svg)](https://ocaml.org/)
[![MCP](https://img.shields.io/badge/MCP-2025--11--25-blue.svg)](https://spec.modelcontextprotocol.io/)
[![Status](https://img.shields.io/badge/status-Production%20Ready-green.svg)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Multi-LLM MCP Server written in OCaml (native binary).

> **Note**: Python 버전은 별도 아카이브로 이동됨 (이 저장소에는 OCaml 버전만 유지)

**MAGI Trinity (삼두정치)**: Unified MCP wrapper for multiple LLM CLIs:

| # | 멤버 | 역할 | 도구 | 모델 |
|---|------|------|------|------|
| 1 | 🔬 **MELCHIOR** | 과학자 | `codex` | GPT-5.2 |
| 2 | 🪞 **BALTHASAR** | 거울 | `claude-cli` | Opus 4.5 |
| 3 | 🎯 **CASPER** | 전략가 | `gemini` | Gemini 3 Pro |

+ `ollama` - 범용 로컬 LLM (devstral, deepseek-r1, qwen3 등)
+ 💡 **페르소나**: `system_prompt` 파라미터로 커스텀 역할 설정 가능

## Why OCaml?

| Feature | Python | OCaml |
|---------|--------|-------|
| Type Safety | Runtime errors | **Compile-time verification** ✨ |
| Performance | Interpreted | **Native binary** |
| Deployment | pip, venv, uvicorn | **Single binary** |
| Pattern Matching | `match` statement | **Exhaustive** |
| Code Size | ~480 lines | ~400 lines |

## Quick Start

> Requires OCaml >= 5.4.0 (recommended: `opam switch create . 5.4.0`)

```bash
# Install dependencies
opam install . --deps-only

# Build
dune build

# Run (HTTP mode, default)
dune exec llm-mcp

# Run (HTTP mode, custom port)
dune exec llm-mcp -- --port 8932

# Run (stdio mode, legacy)
dune exec llm-mcp -- --stdio

# Install globally
dune install
```

## Usage

### HTTP mode (default)

```bash
llm-mcp --port 8932

# Health check
curl http://localhost:8932/health
```

### stdio mode (legacy)

```bash
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | llm-mcp --stdio | jq
```

### HTTP mode (example call)

```bash
# Start server
llm-mcp --port 8932

# Health check
curl http://localhost:8932/health

# Call tool
curl -X POST http://localhost:8932/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "gemini",
      "arguments": {
        "prompt": "Hello, world!",
        "model": "gemini-3-pro-preview"
      }
    }
  }'
```

## MCP Configuration

Add to `~/.mcp.json` (stdio):

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

Or for HTTP mode (recommended):

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

## Tools

### Token-saving mode

- Use `"budget_mode": true` to apply token-saving defaults.
- Or set `LLM_MCP_BUDGET_MODE=1` to enable budget defaults when parameters are omitted.

Budget defaults:
- Gemini: `thinking_level = "low"`
- Claude: `ultrathink = false`
- Codex: `reasoning_effort = "medium"`
- **Response format**: `compact` (when budget_mode=true)

### Compact Protocol v1.3 🚀

LLM-to-LLM 통신 최적화를 위한 다중 포맷 응답 시스템. MAGI 멀티-에이전트 협업 시 토큰 비용을 **최대 70%** 절감합니다.

> 📖 **상세 스펙**: [`docs/PROTOCOL.md`](docs/PROTOCOL.md)

#### Response Formats

| Format | Prefix | Overhead | Best For |
|--------|--------|----------|----------|
| Verbose | `{` | 0% (baseline) | 디버깅, 사람이 읽을 때 |
| Compact DSL | `RES\|` | **-40~50%** ✨ | 짧은 응답 (<50 bytes) |
| Binary | `M` | +33% | Base64 호환성 우선 |
| Base85 | `A` | +25% | 중간 응답 (50-500 bytes) |
| Compressed | `Z` | **-50~70%** ✨ | 긴 응답 (>500 bytes) |
| Auto | - | varies | 자동 최적 선택 |

#### Usage

```bash
# 1. Parameter (per-request)
curl -X POST http://localhost:8932/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "method": "tools/call",
    "params": {
      "name": "codex",
      "arguments": {
        "prompt": "Hello",
        "response_format": "compact"
      }
    }
  }'

# 2. Environment variable (server default)
LLM_MCP_BUDGET_MODE=true llm-mcp --port 8932
```

#### Response Examples

**Verbose (JSON)**:
```json
{"model":"codex","returncode":0,"response":"Hello"}
```

**Compact DSL** (`RES|`):
```
RES|OK|X5|0|Hello
```

**Base85** (`A`):
```
A{base85_encoded_msgpack}
```

**Compressed** (`Z`):
```
Z{zlib_compressed_base85}
```

#### Streaming Delta Protocol

스트리밍 응답용 델타 업데이트:

```
D|F|Hello           # Full: 전체 교체
D|+|, world!        # Append: 끝에 추가
D|R|7|World         # Replace: 위치 7부터 교체
```

#### Client Libraries

| Language | Location | Tests | Example |
|----------|----------|-------|---------|
| TypeScript | `clients/typescript/` | 18 | [`example-usage.ts`](clients/typescript/example-usage.ts) |
| Python | `clients/python/` | 20 | [`example_usage.py`](clients/python/example_usage.py) |

```typescript
// TypeScript
import { decode, decodeBase85 } from './compact-decoder';
const response = decode(serverOutput);
// Run example: npx tsx example-usage.ts
```

```python
# Python
from compact_decoder import decode, decode_base85
response = decode(server_output)
# Run example: python example_usage.py
```

#### When to Use

| Scenario | Recommended Format |
|----------|-------------------|
| Human debugging | `verbose` |
| MAGI consensus voting | `compact` ✨ |
| Large code responses | `compressed` ✨ |
| Default (budget_mode=true) | `auto` |

### gemini
Run Gemini CLI (CASPER in MAGI)

> **Note**: Gemini CLI는 `thinking_level`을 직접 지원하지 않음 ([Issue #6693](https://github.com/google-gemini/gemini-cli/issues/6693) 참조).
> `thinking_level: "high"` 설정 시 prompt engineering workaround 적용:
> *"Think step by step carefully, considering multiple perspectives and edge cases before answering."*

```json
{
  "prompt": "What is 2+2?",
  "model": "gemini-3-pro-preview",
  "thinking_level": "high",
  "budget_mode": false,
  "yolo": false,
  "timeout": 300
}
```

### claude-cli
Run Claude Code CLI (BALTHASAR in MAGI)

```json
{
  "prompt": "Explain this code",
  "model": "opus",
  "ultrathink": true,
  "budget_mode": false,
  "system_prompt": null,
  "output_format": "text",
  "allowed_tools": [],
  "working_directory": "/tmp",
  "timeout": 300
}
```

### codex
Run OpenAI Codex CLI (MELCHIOR in MAGI)

```json
{
  "prompt": "Write a function",
  "model": "gpt-5.2",
  "reasoning_effort": "xhigh",
  "budget_mode": false,
  "sandbox": "workspace-write",
  "working_directory": null,
  "timeout": 300
}
```

#### Codex CLI Direct Usage (Non-MCP)

MCP 없이 직접 Codex CLI를 호출할 때의 올바른 문법:

```bash
# ✅ CORRECT: codex exec 사용 (non-interactive)
echo 'Review this code...' | codex exec -c 'model="gpt-5.2"' -

# ✅ CORRECT: 프롬프트를 인자로 직접 전달
codex exec -c 'model="gpt-5.2"' "Explain this function"

# ❌ WRONG: -p는 profile 옵션 (prompt 아님!)
codex -p "prompt"  # Error: config profile not found

# ❌ WRONG: --json 옵션 없음
codex --json ...   # Error: unexpected argument

# ❌ WRONG: -a auto 없음
codex -a auto ...  # Error: invalid value (possible: untrusted, on-failure, on-request, never)
```

**주요 옵션**:
| 옵션 | 설명 | 예시 |
|------|------|------|
| `-c 'model="..."'` | 모델 선택 | `-c 'model="gpt-5.2"'` |
| `-a never` | 승인 정책 | 자동 실행 |
| `exec` | Non-interactive 모드 | `codex exec ...` |
| `-` (stdin) | 파이프 입력 | `echo "..." \| codex exec -` |

**Code Review 전용**:
```bash
# Built-in review subcommand
codex exec review

# 또는 커스텀 프롬프트
echo 'Review for security issues...' | codex exec -c 'model="gpt-5.2"' -
```

### ollama
Run local LLM via Ollama (completely free, no API key)

```json
{
  "prompt": "Explain this code",
  "model": "devstral",
  "system_prompt": null,
  "temperature": 0.7,
  "timeout": 300
}
```

#### Ollama Model Tiers

> ⚠️ **MCP Timeout**: Claude Code has **60 second hard limit**. Choose models accordingly!

| Tier | Cold Start | VRAM | Models | MCP Compatible |
|------|------------|------|--------|----------------|
| **Tier 1 - Fast** ⚡ | <10s | <8GB | `qwen3:1.7b`, `llama3.2`, `exaone3.5` | ✅ Recommended |
| **Tier 2 - Medium** | 10-30s | 8-20GB | `devstral`, `mistral-small-24b` | ✅ Usually OK |
| **Tier 3 - Heavy** 🐢 | >60s | >40GB | `atom-80b`, `glm4-32k` (84GB) | ❌ Pre-warm required |

**Pre-warm heavy models** (background, before MCP call):
```bash
# Warm up glm4-32k in background
curl http://localhost:11434/api/generate -d '{"model": "glm4-32k:latest", "prompt": "hi", "stream": false}' &

# Check loaded models
curl http://localhost:11434/api/ps | jq '.models[].name'
```

## Development

```bash
# Format code
dune fmt

# Run tests
dune test

# Build in watch mode
dune build -w
```

## License

MIT

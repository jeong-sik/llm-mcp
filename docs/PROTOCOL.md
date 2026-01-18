# Compact Protocol v2.0 Specification

LLM-MCP 양방향 압축 스펙. 클라이언트 개발자를 위한 가이드.

## Overview

Compact Protocol v2.0은 **양방향(입력+출력) 압축**을 지원하는 LLM API 통신 최적화 시스템입니다.

### 핵심 기능

| Feature | Description | Reduction |
|---------|-------------|-----------|
| 출력 압축 | DSL/Base85/Zlib 다중 포맷 | 40-70% |
| 도구 캐싱 | Tool definition hash 참조 | 98% |
| 프롬프트 캐싱 | System prompt hash 참조 | 97% |
| 델타 인코딩 | 대화 컨텍스트 증분 전송 | 48% |
| **전체 (10턴)** | 모든 최적화 적용 시 | **94%** |

---

## Part 1: Response Encoding (출력 압축)

Compact Protocol은 LLM 응답의 토큰 효율성을 높이기 위한 다중 포맷 시스템입니다.

| Format | Prefix | Overhead | Best For |
|--------|--------|----------|----------|
| Verbose | `{` | 0% (baseline) | 디버깅, 사람이 읽을 때 |
| Compact DSL | `RES\|` | -40~50% | 짧은 응답 (<50 bytes) |
| Binary (Base64) | `M` | +33% | 호환성 우선 |
| Base85 | `A` | +25% | 중간 응답 (50-500 bytes) |
| Compressed | `Z` | -50~70% | 긴 응답 (>500 bytes) |

## Format Detection

```
응답 시작 문자로 포맷 감지:
  '{' → Verbose JSON
  'RES|' → Compact DSL
  'M' → Base64 MessagePack
  'A' → Base85 MessagePack
  'Z' → Zlib + Base85
```

## 1. Verbose (JSON)

표준 JSON 포맷. 가장 읽기 쉽지만 가장 큼.

```json
{
  "model": "gemini",
  "returncode": 0,
  "response": "Hello, world!",
  "reasoning_effort": "medium",
  "ultrathink": false
}
```

### Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `model` | string | ✓ | LLM 이름 (gemini, claude, codex, etc.) |
| `returncode` | int | ✓ | 0=성공, 1=에러 |
| `response` | string | ✓ | LLM 응답 본문 |
| `reasoning_effort` | string | | low/medium/high |
| `ultrathink` | bool | | Extended thinking 사용 여부 |

## 2. Compact DSL

파이프 구분자 기반 DSL. 짧은 응답에 최적.

```
RES|{STATUS}|{MODEL}|{TOKENS}|{RESULT}
```

### Example

```
RES|OK|G3|150|Hello, world!
```

### Status Codes

| Code | Meaning |
|------|---------|
| `OK` | 성공 |
| `ERR` | 에러 |
| `PART` | 부분 응답 |
| `STREAM` | 스트리밍 중 |

### Model Codes

| Code | Model |
|------|-------|
| `G3` | Gemini |
| `C4` | Claude |
| `X5` | Codex |
| `OL` | Ollama |
| `AD` | ADAM |
| `SE` | SEELE |

### Escaping

결과에 `|`가 포함되면 그대로 유지. 파싱 시 처음 4개 필드 분리 후 나머지는 결과로 취급.

```
RES|OK|G3|0|a|b|c  →  result = "a|b|c"
```

## 3. Binary (Base64 MessagePack)

MessagePack으로 직렬화 후 Base64 인코딩.

```
M{base64_encoded_msgpack}
```

### MessagePack Structure

모든 버전은 첫 번째 요소로 버전 번호를 포함합니다.

**v1 (Legacy - String Encoding)**
```
[1, "OK", "G3", tokens, response]
```
- Status/Model이 문자열로 인코딩됨
- 가장 호환성 높음, 사이즈 가장 큼

**v2 (Integer Encoding)**
```
[2, status_int, model_int, tokens, response]
```
- Status/Model이 정수로 인코딩됨
- v1 대비 ~4 bytes 절약

**v3 (Compact - Default)**
```
[3, status_int, model_int, response]           // tokens=0 생략
[3, status_int, model_int, tokens, response]   // tokens>0
```
- tokens=0일 때 생략하여 추가 1 byte 절약
- 기본 인코딩 버전

**Integer Mappings:**
- `status_int`: 0=OK, 1=ERR, 2=PART, 3=STREAM
- `model_int`: 0=G3, 1=C4, 2=X5, 3=OL, 4=AD, 5=SE

### Example

```
MgqAAKRoZWxsbw==
```

## 4. Base85 (ASCII85 MessagePack)

Base64 대신 Base85 인코딩 사용. 25% 오버헤드 (Base64의 33% 대비).

```
A{base85_encoded_msgpack}
```

### Custom Alphabet

⚠️ **중요**: 표준 ASCII85가 아닌 커스텀 알파벳 사용!

```
0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-:+=^!/*?&<>()[]{}@%$#
```

### Encoding Rules

1. 4 bytes → 5 chars
2. 남은 바이트 (1-3) → (2-4) chars
3. Big-endian byte order

### Decoding Algorithm

```python
def decode_base85(encoded: str) -> bytes:
    ALPHABET = "0123456789ABC...@%$#"  # 85 chars
    REVERSE = {c: i for i, c in enumerate(ALPHABET)}

    result = []
    i = 0

    while i + 5 <= len(encoded):
        # 5 chars → 4 bytes
        value = sum(REVERSE[encoded[i+j]] * (85 ** (4-j)) for j in range(5))
        result.extend([
            (value >> 24) & 0xFF,
            (value >> 16) & 0xFF,
            (value >> 8) & 0xFF,
            value & 0xFF,
        ])
        i += 5

    # Handle remaining (2-4 chars → 1-3 bytes)
    remaining = len(encoded) - i
    if remaining > 0:
        padded = [84] * 5  # Pad with highest value
        for j in range(remaining):
            padded[j] = REVERSE[encoded[i+j]]
        value = sum(padded[j] * (85 ** (4-j)) for j in range(5))
        for j in range(remaining - 1):
            result.append((value >> (24 - 8*j)) & 0xFF)

    return bytes(result)
```

## 5. Compressed (Zlib + Base85)

Zlib으로 압축 후 Base85 인코딩. 큰 응답에 최적.

```
Z{base85_encoded_zlib_compressed_msgpack}
```

### Compression

- Algorithm: Zlib (deflate)
- Level: 4 (balance between speed and ratio)
- Input: MessagePack bytes
- Output: Compressed bytes → Base85

### Decompression Flow

```
Z{data}
  │
  └─ Strip 'Z' prefix
       │
       └─ Base85 decode
            │
            └─ Zlib decompress
                 │
                 └─ MessagePack decode
                      │
                      └─ CompactResponse
```

## 6. Auto (Adaptive)

응답 크기에 따라 자동 선택.

| Response Size | Selected Format |
|--------------|-----------------|
| < 50 bytes | Compact DSL |
| 50-500 bytes | Base85 |
| > 500 bytes | Compressed |

## Error Types

v1.3에서 도입된 구조화된 에러 타입:

```ocaml
type decode_error =
  | InvalidBase85Char of char * int    (* char, position *)
  | InvalidBase85Length of int         (* actual length *)
  | DecompressionFailed of string      (* underlying error *)
  | InvalidMsgpack of string           (* parse error *)
  | InvalidFormat of string            (* unrecognized format *)
  | MissingField of string             (* required field name *)
```

## Client Libraries

| Language | Location | Tests |
|----------|----------|-------|
| TypeScript | `clients/typescript/compact-decoder.ts` | 13 |
| Python | `clients/python/compact_decoder.py` | 15 |

### Usage (TypeScript)

```typescript
import { decode, decodeBase85 } from './compact-decoder';

// Auto-detect and decode
const response = decode(serverOutput);
console.log(response.result);

// Low-level Base85
const bytes = decodeBase85('VPa.s');  // → "abcd"
```

### Usage (Python)

```python
from compact_decoder import decode, decode_base85

# Auto-detect and decode
response = decode(server_output)
print(response.result)

# Low-level Base85
data = decode_base85('VPa.s')  # → b"abcd"
```

## Streaming Delta Protocol

스트리밍 응답용 델타 프로토콜 (D prefix):

```
D|{OP}|{DATA}
```

### Operations

| Op | Name | Description |
|----|------|-------------|
| `F` | Full | 전체 교체 |
| `+` | Append | 끝에 추가 |
| `R` | Replace | 위치 지정 교체 |
| `Z` | Compressed | Zlib 압축된 델타 |

### Examples

```
D|F|Hello           # Full: content = "Hello"
D|+|, world!        # Append: content = "Hello, world!"
D|R|7|World         # Replace at 7: content = "Hello, World!"
D|Z|{compressed}    # Compressed delta
```

---

## Part 2: Request Encoding (입력 압축)

### Tool Definition Caching

도구 정의는 한 번만 전송하고 이후 해시 참조로 재사용합니다.

```
첫 호출:  TOOL|def|{"name":"search","schema":{...}}
이후:    TOOL|ref|t_abc12345
```

### System Prompt Caching

시스템 프롬프트도 동일하게 캐싱합니다.

```
첫 호출:  SYS|full|You are a helpful assistant...
이후:    SYS|ref|s_xyz78901
```

### Conversation Delta Encoding

대화 컨텍스트는 델타 연산으로 증분 전송합니다.

| Op | Prefix | Description |
|----|--------|-------------|
| Full | `D\|F\|` | 전체 교체 |
| Append | `D\|+\|` | 끝에 추가 |
| Replace | `D\|R\|pos\|` | 위치 지정 교체 |
| Compressed | `D\|Z\|` | Zlib 압축 델타 |

### Compact Request Format

```
REQ|{version}|{tool}|{system}|{args}|{deltas...}
```

예시:
```
REQ|1|t_abc12345|s_xyz78901|{"query":"hello"}|D|+|User: Hi
```

---

## Version History

| Version | Changes |
|---------|---------|
| v1.0 | Initial: Verbose, Compact DSL |
| v1.1 | Added Binary (Base64 MessagePack) |
| v1.2 | Added Base85, legacy prefixes (MPK:, A85:) |
| v1.3 | Shortened prefixes (M, A, Z), Auto format, structured errors |
| v1.3.1 | MessagePack v1/v2/v3 encoding support |
| **v2.0** | **Bidirectional compression: tool/prompt caching, delta encoding** |

---

*Last updated: 2026-01-12*

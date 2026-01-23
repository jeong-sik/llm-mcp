# Compact Protocol v4: LLM-to-LLM Communication Standard

> **목적**: LLM ↔ LLM ↔ LLM 통신에서 토큰 효율 극대화
> **압축률**: 68-75% (Zstd + Dictionary)
> **구현**: OCaml Native (`features/llm-mcp/`)

---

## 1. Format Prefixes

모든 메시지는 **1-byte prefix**로 포맷을 식별합니다:

| Prefix | Format | 용도 | 압축률 |
|--------|--------|------|--------|
| `M` | MessagePack + Base64 | 바이너리 안전, 레거시 호환 | 40-50% |
| `A` | MessagePack + Base85 | Base64보다 25% 효율적 | 45-55% |
| `Z` | Gzip + Base85 | 중간 크기 페이로드 | 55-65% |
| `S` | **Zstd + Base85** | 대용량, 최고 압축 | **68-75%** |
| `D` | Zstd + Dictionary + Base85 | 반복 패턴 최적화 | **75-85%** |
| `RES\|` | DSL (Domain-Specific) | 소형 응답, 사람 가독성 | - |
| `{` | JSON Verbose | 디버깅, 호환성 | 0% |

---

## 2. Auto Format Selection

크기 기반 자동 선택 로직:

```
payload_size < 50 bytes   → DSL (RES|...)
payload_size < 256 bytes  → Base85 (A prefix)
payload_size < 512 bytes  → Zstd (S prefix)
payload_size >= 512 bytes → Zstd (S prefix)
dictionary available      → Dictionary Zstd (D prefix)
```

---

## 3. DSL Format (소형 응답용)

**구조**: `RES|{status}|{model}|{tokens}|{result}`

```
RES|OK|C4|150|The answer is 42.
RES|ERR|G3|0|API rate limit exceeded
RES|OK|O4|2500|def fibonacci(n): ...
```

**Status Codes**:
- `OK` - 성공
- `ERR` - 오류
- `PARTIAL` - 부분 응답
- `STREAM` - 스트리밍 중

**Model Codes**:
- `C4` = Claude Opus 4
- `C45` = Claude Opus 4.5
- `CS` = Claude Sonnet
- `CH` = Claude Haiku
- `G3` = Gemini 2.5 Pro
- `G4` = GPT-4o
- `G5` = GPT-5
- `O4` = Ollama (local)

---

## 4. Binary Formats (S/D prefix)

### Encoding Flow
```
response → MessagePack → Zstd compress → Base85 encode → "S" + encoded
```

### Decoding Flow
```
"S" + encoded → Base85 decode → Zstd decompress → MessagePack → response
```

### MessagePack Schema
```json
{
  "v": 1,           // version
  "s": "OK",        // status
  "m": "C45",       // model code
  "t": 1500,        // tokens
  "r": "..."        // result (main content)
}
```

---

## 5. LLM 간 통신 예시

### Agent A → Agent B (코드 전송)
```
# Agent A sends:
S<base85-encoded-zstd-compressed-msgpack>

# Decoded content:
{
  "v": 1,
  "s": "OK",
  "m": "C45",
  "t": 2500,
  "r": "def process_data(items):\n    return [x * 2 for x in items]\n\n# Usage:\nresult = process_data([1, 2, 3])"
}
```

### MAGI Trinity Pipeline
```
[Claude/BALTHASAR] ──S──▶ [Gemini/CASPER] ──S──▶ [Codex/MELCHIOR]
     분석 결과              검증 결과              최종 판정
```

---

## 6. Implementation

### OCaml (Native)
```ocaml
(* Encode *)
let encoded = Types.format_tool_result ~format:Auto result

(* Decode *)
match Types.decode_formatted_response encoded with
| Ok response -> process response
| Error e -> handle_error e
```

### Python (Reference)
```python
def decode_compact(data: str) -> dict:
    prefix = data[0]
    rest = data[1:]

    if prefix == 'S':
        decoded = base85_decode(rest)
        decompressed = zstd.decompress(decoded)
        return msgpack.unpackb(decompressed)
    elif prefix == 'R':  # RES|...
        parts = data.split('|')
        return {
            'status': parts[1],
            'model': parts[2],
            'tokens': int(parts[3]),
            'result': '|'.join(parts[4:])
        }
    # ... other formats
```

---

## 7. Best Practices

### DO ✅
- 500+ bytes → `S` (Zstd) 사용
- 반복 패턴 → `D` (Dictionary) 사용
- 디버깅 시 → JSON Verbose 사용
- 소형 응답 → DSL 사용

### DON'T ❌
- 소형 페이로드에 Zstd 사용 (오버헤드)
- Base64 사용 (Base85가 25% 효율적)
- 압축 없이 대용량 전송

---

## 8. Error Handling

```ocaml
type decode_error =
  | Empty_response
  | Invalid_prefix of char
  | Base85_decode_failed of string
  | Zstd_decompress_failed of string
  | Msgpack_parse_failed of string
  | Dictionary_not_found
```

**Fallback 전략**: 디코딩 실패 시 원본 문자열 그대로 처리

---

## 9. Performance Benchmarks

| Payload | Raw Size | S Format | Compression |
|---------|----------|----------|-------------|
| Tiny (50B) | 50 | 45 | 10% |
| Small (200B) | 200 | 120 | 40% |
| Medium (1KB) | 1024 | 380 | 63% |
| Large (5KB) | 5120 | 1600 | 69% |
| XLarge (20KB) | 20480 | 5900 | 71% |

**속도**: ~500 MB/s (Native Zstd)

---

## 10. Related Files

- `features/llm-mcp/lib/types.ml` - Core types & encode/decode
- `features/llm-mcp/lib/format_selector.ml` - Auto format selection
- `features/llm-mcp/lib/dictionary.ml` - Trained dictionary management
- `features/llm-mcp/test/test_binaries.ml` - E2E tests

---

## 11. Bidirectional Communication

### 현재 상태

| 방향 | 지원 | 함수 |
|------|------|------|
| **Output** (LLM → LLM) | ✅ 구현됨 | `format_tool_result`, `decode_formatted_response` |
| **Input** (LLM ← LLM) | ⚠️ 부분적 | 수동 파싱 필요 |

### Output (응답 압축) - 구현 완료
```
Claude ──[S-compressed]──▶ Gemini
                           decode_formatted_response()
```

### Input (요청 압축) - TODO
```
Claude ◀──[S-compressed]── Gemini (요청)
format_tool_call()
```

**Input 압축이 유용한 경우**:
- 대용량 코드/데이터를 컨텍스트로 전달
- Multi-turn 대화 히스토리 압축
- Tool arguments에 긴 텍스트 포함

### 양방향 예시 (목표)
```
[Agent A]                      [Agent B]
    │                              │
    ├──S(request)─────────────────▶│
    │                              ├── decode_request()
    │                              ├── process()
    │◀─────────────────S(response)─┤
    ├── decode_response()          │
```

### Input 압축 구현 계획

```ocaml
(* TODO: Input encoder *)
val format_tool_call : tool_call -> string
val decode_tool_call : string -> (tool_call, error) result

type tool_call = {
  name: string;
  arguments: Yojson.Safe.t;
  context: string option;  (* 압축 대상 *)
}
```

**우선순위**: Output 압축만으로도 68%+ 절감 → Input은 Phase 2

---

*Compact Protocol v4 - Making LLM communication efficient* 🚀

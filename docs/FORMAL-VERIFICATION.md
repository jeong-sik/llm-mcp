# Formal Verification Scope (Phase 1-2)

This document defines the formal-ish invariants we verify for:

1) Protocol codec (Compact/Verbose/Base85/Zstd formats)
2) Consensus and validation logic (Meta/Quorum/Pipeline policies)

Non-goals:
- External CLI reliability (codex/gemini/claude/ollama)
- LLM output quality or truthfulness
- Network/IO behavior outside the codec boundaries

---

## 1) Protocol Codec Invariants

We treat `format_tool_result` + `decode_formatted_response` as the codec boundary.
The invariants below must hold for all supported formats.

### Invariants
- Round-trip preservation:
  - `status`, `model`, `tokens`, `result` are preserved after decode.
- DSL correctness:
  - Results containing `|` must decode without loss.
- Auto selection:
  - `< auto_compact_threshold` => Compact DSL (`RES|...`)
  - `[auto_compact_threshold, auto_base85_threshold)` => Base85 (`A...`)
  - `>= auto_base85_threshold` => Zstd (`S...`)
- Unknown prefix and empty payload are rejected.

### Covered By
- `test/test_binaries.ml` (E2E codec roundtrip + special cases)
- `test/test_msgpack_size.ml` (msgpack v1-v3 roundtrip)
- `test/test_protocol_invariants.ml` (status/tokens/model preservation)

---

## 2) Consensus/Validation Logic Invariants

We treat validators as deterministic functions for these guarantees.

### Invariants
- Determinism:
  - For the same input, verdict is stable across runs.
- Majority boundary (strict):
  - `pass_count > n/2` is required to pass.
- Weighted threshold:
  - Weighted score must be `>= 0.5 * total_weight`.
- Monotonic pass (majority):
  - Adding a passing validator does not flip `pass -> fail`.

### Covered By
- `test/test_presets.ml` (Meta/Quorum invariants)

---

## Notes

- These invariants only apply at the codec and validator layers.
- For full-system reliability, use runtime checks and integration tests.

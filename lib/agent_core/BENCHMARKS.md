# Agent Core Benchmarks

> Last updated: 2025-01-18
> Platform: macOS Darwin 25.2.0, Apple M3 Max, 128GB RAM
> OCaml: 5.x with Lwt

## Test Summary

| Test Suite | Tests | Time | Status |
|------------|-------|------|--------|
| Unit Tests | 18 | 0.37s | ✅ |
| Stress & Chaos | 25 | 6.56s | ✅ |
| 1-Min Soak | 1 | 60.02s | ✅ |
| **Total** | **44** | **~67s** | ✅ |

---

## 1-Minute Soak Test Results

```
┌─────────────────────────────────────────────────┐
│  🚀 60-Second Continuous Agent Operation        │
├─────────────────────────────────────────────────┤
│  Total Turns:        3,346                      │
│  Problems Processed: 3,345                      │
│  Problems Solved:    2,230                      │
│  Throughput:         55.7 problems/sec          │
│  Memory (sliding):   100 messages (bounded)     │
│  Errors:             0                          │
└─────────────────────────────────────────────────┘
```

### Key Metrics

| Metric | Value | Notes |
|--------|-------|-------|
| Turns/minute | 3,346 | With 10ms simulated delay per turn |
| Theoretical max | ~6,000/min | Without delays |
| Memory growth | 0 | Sliding window caps at 100 msgs |
| State consistency | 100% | All state preserved across turns |

---

## Stress Test Results

### Chaos Resilience

| Failure Rate | Result | Retries Used |
|--------------|--------|--------------|
| 10% | ✅ Pass | Few |
| 50% | ✅ Pass | Moderate |
| 90% | ✅ Pass | Heavy |

### Concurrency

- **50 parallel Lwt agent loops**: All completed successfully
- No race conditions detected
- Deterministic with seeded randomness (`seed=42`)

### Edge Cases Validated

- Exact timeout boundary (95ms response / 100ms timeout)
- Max retries exactly exhausted
- Empty vs None tool_calls distinction
- System message preservation under extreme trimming (1001 → 3 msgs)
- 1MB message content handling

---

## Performance Benchmarks

| Operation | Latency | Notes |
|-----------|---------|-------|
| Turn execution | p50: 0.00ms, p95: 0.00ms, p99: 0.01ms | Mock backend |
| Trim 10k→50 msgs | 0.411ms | Preserves system msg |
| Retry delay calc | 0.0000ms | Pure computation |
| Memory overhead | ~0 bytes/msg | Efficient GC |

---

## Extrapolated Long-Running Estimates

Based on 1-minute soak test:

| Duration | Estimated Turns | Memory |
|----------|-----------------|--------|
| 1 hour | ~200,000 | Bounded (100 msgs) |
| 24 hours | ~4,800,000 | Bounded (100 msgs) |
| 1 week | ~33,600,000 | Bounded (100 msgs) |

**Conclusion**: Agent loop can theoretically run indefinitely with:
- Constant memory usage (sliding window)
- No state corruption
- Consistent throughput

---

## How to Reproduce

```bash
# Unit tests
dune exec test/test_agent_core.exe

# Stress & Chaos tests
dune exec test/test_agent_core_stress.exe

# 1-minute soak test
dune exec test/test_agent_soak_1min.exe -- --verbose
```

---

## Architecture

```
Agent Loop Functor
├── Retry (exponential backoff + jitter)
├── Timeout (Lwt.pick based)
├── State Manager (sliding window)
└── Composable backends (LLM_BACKEND signature)
```

See `agent_core.ml` for usage examples.

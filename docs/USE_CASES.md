# LLM-MCP Use Cases & Architecture

> Eio 기반 에이전트 시스템의 유즈케이스 다이어그램

## Status

| Pattern | 구현 | 테스트 | 모듈 |
|---------|:----:|:------:|------|
| 1. Backend Selection | ✅ | ✅ | `*_backend_eio.ml` |
| 2. Pipeline | ✅ | ✅ | `Presets.Pipeline` |
| 3. Quorum | ✅ | ✅ | `Presets.Quorum` |
| 4. Gate | ✅ | ✅ | `Presets.Gate` |
| 5. Layered | ✅ | ✅ | `Presets.Layered` |
| 6. Diamond | ✅ | ✅ | `Presets.Diamond` |
| 7. Goal-Driven | ✅ | ✅ | `Validation_stack` |

---

## 1. LLM Backend Selection

```
                    ┌─────────────────────────────────────────┐
                    │           Application Layer              │
                    └──────────────────┬──────────────────────┘
                                       │
                ┌──────────────────────┼──────────────────────┐
                │                      │                      │
                ▼                      ▼                      ▼
       ┌────────────────┐    ┌────────────────┐    ┌────────────────┐
       │ Ollama Backend │    │ OpenAI Backend │    │Claude CLI      │
       │     (Eio)      │    │     (Eio)      │    │Backend (Eio)   │
       └───────┬────────┘    └───────┬────────┘    └───────┬────────┘
               │ HTTP                │ HTTPS               │ Process
               ▼                     ▼                     ▼
       ┌────────────────┐    ┌────────────────┐    ┌────────────────┐
       │  Ollama Server │    │   OpenAI API   │    │  Claude CLI    │
       │  llama3, qwen3 │    │  gpt-4, gpt-4o │    │ sonnet, opus   │
       └────────────────┘    └────────────────┘    └────────────────┘
```

**Use Cases**:
- Ollama: 개발/오프라인/프라이버시
- OpenAI: 프로덕션/고품질
- Claude CLI: Claude Code 통합

---

## 2. Pipeline (순차 검증)

```
   PR Submitted
        │
        ▼
┌───────────────┐  Pass   ┌───────────────┐  Pass   ┌───────────────┐
│   Syntax      │ ──────▶ │   Type        │ ──────▶ │   Security    │
│   Check       │         │   Safety      │         │   Scan        │
└───────────────┘         └───────────────┘         └───────────────┘
        │ Fail                   │ Fail                   │ Fail
        ▼                        ▼                        ▼
   ┌─────────┐              ┌─────────┐              ┌─────────┐
   │ REJECT  │              │ REJECT  │              │ REJECT  │
   └─────────┘              └─────────┘              └─────────┘
```

```ocaml
let pipeline = Presets.Pipeline.create ~sw [
  (module Syntax_validator);
  (module Type_safety_validator);
  (module Security_validator);
]
```

---

## 3. Quorum (다수결 합의)

```
                         User Query
                              │
               ┌──────────────┼──────────────┐
               ▼              ▼              ▼
        ┌───────────┐  ┌───────────┐  ┌───────────┐
        │  Gemini   │  │  Claude   │  │   GPT-4   │
        │ (CASPER)  │  │(BALTHASAR)│  │(MELCHIOR) │
        └─────┬─────┘  └─────┬─────┘  └─────┬─────┘
              │              │              │
              └──────────────┼──────────────┘
                             ▼
                    ┌─────────────────┐
                    │ Quorum: 2/3     │
                    │ PASS → APPROVED │
                    └─────────────────┘
```

```ocaml
let quorum = Presets.Quorum.create ~sw ~required:2 [
  (module Gemini_validator);
  (module Claude_validator);
  (module GPT4_validator);
]
```

---

## 4. Gate (조건부 실행)

```
                    Request
                       │
                       ▼
              ┌─────────────────┐
              │  Gate Check     │
              │  feature_flag?  │
              └────────┬────────┘
                       │
          ┌────────────┴────────────┐
          ▼ Enabled                 ▼ Disabled
   ┌─────────────┐           ┌─────────────┐
   │  Execute    │           │    Skip     │
   │  Validator  │           │ (Pass-thru) │
   └─────────────┘           └─────────────┘
```

```ocaml
let gated = Presets.Gate.feature_flag
  ~name:"experimental"
  ~flag:"experimental_review"
  (module AI_review_validator)
```

---

## 5. Layered (Fast → Slow)

```
   Input
     │
     ▼
┌──────────────────────────────────────────┐
│  Layer 1 (< 10ms): Format, Required      │
└────────────────────┬─────────────────────┘
                     │ Pass
                     ▼
┌──────────────────────────────────────────┐
│  Layer 2 (< 100ms): DB, Business Rules   │
└────────────────────┬─────────────────────┘
                     │ Pass
                     ▼
┌──────────────────────────────────────────┐
│  Layer 3 (< 10s): External API, LLM      │
└────────────────────┬─────────────────────┘
                     │ Pass
                     ▼
               ┌───────────┐
               │ VALIDATED │
               └───────────┘

⚡ Early rejection saves expensive computation
```

```ocaml
let layered = Presets.Layered.create ~sw [
  [(module Format); (module Required)];  (* Fast *)
  [(module DB_check); (module Rules)];   (* Medium *)
  [(module External); (module LLM)];     (* Slow *)
]
```

---

## 6. Diamond (병렬 → 합류)

```
              Document
                  │
                  ▼
           ┌──────────┐
           │  SPLIT   │
           └────┬─────┘
                │
     ┌──────────┼──────────┐
     ▼          ▼          ▼
┌─────────┐┌─────────┐┌─────────┐
│ Grammar ││ Factual ││  Style  │
│ Score:95││Score:87 ││Score:92 │
└────┬────┘└────┬────┘└────┬────┘
     │          │          │
     └──────────┼──────────┘
                ▼
          ┌──────────┐
          │  MERGE   │
          │ Avg: 91.3│
          └──────────┘
```

```ocaml
let diamond = Presets.Diamond.create ~sw
  ~merge:weighted_average
  [(module Grammar); (module Factual); (module Style)]
```

---

## 7. Goal-Driven Loop

```
           ┌─────────────────┐
           │   START GOAL    │
           │ "90% coverage"  │
           └────────┬────────┘
                    │
    ┌───────────────┴───────────────┐
    │                               │
    ▼                               │
┌─────────────┐                     │
│ AGENT LOOP  │◀────────────────────┤
│ 1. Read     │                     │
│ 2. Generate │                     │
│ 3. Execute  │                     │
└──────┬──────┘                     │
       │                            │
       ▼                            │
┌─────────────┐                     │
│  VALIDATOR  │                     │
│ coverage≥90?│                     │
└──────┬──────┘                     │
       │                            │
   Yes │    No                      │
       ▼    ▼                       │
┌──────┐ ┌─────────────┐           │
│ DONE │ │ INTERVENOR  │───────────┘
└──────┘ │ (AI/Human)  │
         └─────────────┘
```

```ocaml
let goal = Validation_stack.Goal.threshold
  ~metric:(module Coverage_metric)
  ~threshold:0.90

let intervenor = Validation_stack.Make_ai_intervenor(struct
  let stall_threshold = 120
  let retry_limit = 5
end)
```

---

## Quick Reference

| Pattern | Use Case | 테스트 |
|---------|----------|:------:|
| Pipeline | CI/CD, 순차 검증 | ✅ |
| Fanout | 병렬 처리 | ✅ |
| Quorum | 다수결 (MAGI) | ✅ |
| Gate | Feature Flag | ✅ |
| Layered | Fast→Slow 최적화 | ✅ |
| Diamond | Split→Merge | ✅ |
| Weighted | 가중치 투표 | ✅ |
| Circuit Breaker | 장애 차단 | - |
| Saga | 분산 트랜잭션 | - |

---

*llm-mcp v2.1.0*

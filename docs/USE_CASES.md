# LLM-MCP Use Cases & Architecture

> Realistic use case diagrams for the Eio-based agent system

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
                         │                     │                     │
                         │ HTTP                │ HTTPS/TLS           │ Process
                         │ localhost:11434     │ api.openai.com      │ Spawn
                         ▼                     ▼                     ▼
                 ┌────────────────┐    ┌────────────────┐    ┌────────────────┐
                 │  Ollama Server │    │   OpenAI API   │    │  Claude CLI    │
                 │  (Local LLM)   │    │   (Cloud)      │    │  (Installed)   │
                 │                │    │                │    │                │
                 │ llama3, qwen3  │    │ gpt-4, gpt-4o  │    │ sonnet, opus   │
                 │ devstral, etc  │    │ gpt-3.5-turbo  │    │ haiku          │
                 └────────────────┘    └────────────────┘    └────────────────┘
                        │                      │                      │
                        └──────────────────────┼──────────────────────┘
                                               │
                    ┌──────────────────────────┴──────────────────────────┐
                    │                    Use Cases                         │
                    ├──────────────────────────────────────────────────────┤
                    │ • Ollama: Development, offline, privacy-sensitive    │
                    │ • OpenAI: Production, high quality, tool calling     │
                    │ • Claude CLI: Existing Claude Code integration       │
                    └──────────────────────────────────────────────────────┘
```

## 2. Code Review Pipeline

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        Code Review Pipeline                                  │
│                     (Validator Preset: Pipeline)                            │
└─────────────────────────────────────────────────────────────────────────────┘

   PR Submitted
        │
        ▼
┌───────────────┐     Pass      ┌───────────────┐     Pass      ┌───────────────┐
│   Syntax      │ ───────────▶  │   Type        │ ───────────▶  │   Security    │
│   Check       │               │   Safety      │               │   Scan        │
│               │               │               │               │               │
│ (AST parse)   │               │ (type errors) │               │ (vuln check)  │
└───────────────┘               └───────────────┘               └───────────────┘
        │                              │                               │
        │ Fail                         │ Fail                          │ Fail
        ▼                              ▼                               ▼
   ┌─────────┐                   ┌─────────┐                     ┌─────────┐
   │ REJECT  │                   │ REJECT  │                     │ REJECT  │
   │ + Error │                   │ + Types │                     │ + CVE   │
   └─────────┘                   └─────────┘                     └─────────┘

                                       │ All Pass
                                       ▼
                              ┌───────────────┐
                              │   APPROVED    │
                              │   for Merge   │
                              └───────────────┘

OCaml Code:
```ocaml
let pipeline = Presets.Pipeline.create ~name:"code-review" [
  (module Syntax_validator);
  (module Type_safety_validator);
  (module Security_validator);
]
```

## 3. Multi-LLM Consensus (Quorum)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    Multi-LLM Consensus System                                │
│                   (Validator Preset: Quorum)                                │
└─────────────────────────────────────────────────────────────────────────────┘

                              User Query
                                  │
                                  ▼
                         ┌───────────────┐
                         │  Orchestrator │
                         │   (Fanout)    │
                         └───────┬───────┘
                                 │
              ┌──────────────────┼──────────────────┐
              │                  │                  │
              ▼                  ▼                  ▼
      ┌───────────────┐  ┌───────────────┐  ┌───────────────┐
      │    Gemini     │  │    Claude     │  │    GPT-4      │
      │   (CASPER)    │  │  (BALTHASAR)  │  │  (MELCHIOR)   │
      │               │  │               │  │               │
      │  Strategic    │  │   Balanced    │  │   Technical   │
      │  Analysis     │  │   Judgment    │  │   Precision   │
      └───────┬───────┘  └───────┬───────┘  └───────┬───────┘
              │                  │                  │
              │ Response A       │ Response B       │ Response C
              │                  │                  │
              └──────────────────┼──────────────────┘
                                 │
                                 ▼
                        ┌───────────────┐
                        │    Quorum     │
                        │   Consensus   │
                        │               │
                        │ Required: 2/3 │
                        └───────┬───────┘
                                │
              ┌─────────────────┼─────────────────┐
              │                 │                 │
              ▼                 ▼                 ▼
        ┌──────────┐     ┌──────────┐     ┌──────────┐
        │ 3/3 Pass │     │ 2/3 Pass │     │ 1/3 Pass │
        │ ──────── │     │ ──────── │     │ ──────── │
        │ APPROVED │     │ APPROVED │     │ REJECTED │
        │ (Strong) │     │ (Weak)   │     │          │
        └──────────┘     └──────────┘     └──────────┘

OCaml Code:
```ocaml
let quorum = Presets.Quorum.create
  ~name:"magi-trinity"
  ~required:2  (* 2 out of 3 must pass *)
  ~policy:`Majority
  [
    (module Gemini_validator);   (* CASPER *)
    (module Claude_validator);   (* BALTHASAR *)
    (module GPT4_validator);     (* MELCHIOR *)
  ]
```

## 4. Feature Flag Gate

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                       Feature Flag Gate Pattern                              │
│                     (Validator Preset: Gate)                                │
└─────────────────────────────────────────────────────────────────────────────┘

                              Request
                                 │
                                 ▼
                       ┌─────────────────┐
                       │   Gate Check    │
                       │                 │
                       │ experimental_   │
                       │ code_review?    │
                       └────────┬────────┘
                                │
               ┌────────────────┴────────────────┐
               │                                 │
               ▼ Enabled                         ▼ Disabled
      ┌─────────────────┐               ┌─────────────────┐
      │   AI-Powered    │               │   Skip Gate     │
      │   Code Review   │               │   (Pass-through)│
      │                 │               │                 │
      │ • LLM Analysis  │               │ Return          │
      │ • Pattern Check │               │ NotApplicable   │
      │ • Suggestions   │               │                 │
      └────────┬────────┘               └────────┬────────┘
               │                                 │
               └─────────────┬───────────────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │  Continue with  │
                    │  other checks   │
                    └─────────────────┘

OCaml Code:
```ocaml
let gated_review = Presets.Gate.feature_flag
  ~name:"experimental-review"
  ~flag:"experimental_code_review"
  (module AI_code_review_validator)
```

## 5. Layered Validation (Fast → Slow)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                      Layered Validation Pattern                              │
│                   (Validator Preset: Layered)                               │
└─────────────────────────────────────────────────────────────────────────────┘

                              Input Data
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  Layer 1: Fast Checks (< 10ms)                                              │
│  ───────────────────────────────                                            │
│  • Format validation                                                         │
│  • Required fields                                                           │
│  • Length limits                                                             │
└──────────────────────────────────┬──────────────────────────────────────────┘
                                   │ Pass
                                   ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  Layer 2: Medium Checks (< 100ms)                                           │
│  ────────────────────────────────                                           │
│  • Database lookups                                                          │
│  • Business rules                                                            │
│  • Cross-field validation                                                    │
└──────────────────────────────────┬──────────────────────────────────────────┘
                                   │ Pass
                                   ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  Layer 3: Slow Checks (< 10s)                                               │
│  ────────────────────────────                                               │
│  • External API calls                                                        │
│  • ML model inference                                                        │
│  • LLM validation                                                            │
└──────────────────────────────────┬──────────────────────────────────────────┘
                                   │ Pass
                                   ▼
                          ┌───────────────┐
                          │   VALIDATED   │
                          └───────────────┘

         ⚡ Early rejection at fast layers saves expensive computation

OCaml Code:
```ocaml
let layered = Presets.Layered.create ~name:"api-validation" [
  (* Layer 1: Fast *)
  [ (module Format_validator); (module Required_fields) ];
  (* Layer 2: Medium *)
  [ (module Database_check); (module Business_rules) ];
  (* Layer 3: Slow *)
  [ (module External_api); (module LLM_validator) ];
]
```

## 6. Diamond Pattern (Parallel → Merge)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Diamond Pattern                                      │
│                  (Split → Parallel → Merge)                                 │
└─────────────────────────────────────────────────────────────────────────────┘

                              Document
                                  │
                                  ▼
                         ┌───────────────┐
                         │    SPLIT      │
                         │   (Fanout)    │
                         └───────┬───────┘
                                 │
              ┌──────────────────┼──────────────────┐
              │                  │                  │
              ▼                  ▼                  ▼
      ┌───────────────┐  ┌───────────────┐  ┌───────────────┐
      │   Grammar     │  │   Factual     │  │   Style       │
      │   Check       │  │   Accuracy    │  │   Analysis    │
      │               │  │               │  │               │
      │   (Parallel)  │  │   (Parallel)  │  │   (Parallel)  │
      └───────┬───────┘  └───────┬───────┘  └───────┬───────┘
              │                  │                  │
              │ Score: 95       │ Score: 87       │ Score: 92
              │                  │                  │
              └──────────────────┼──────────────────┘
                                 │
                                 ▼
                        ┌───────────────┐
                        │    MERGE      │
                        │   (Reduce)    │
                        │               │
                        │ Weighted Avg  │
                        │ = 91.3        │
                        └───────┬───────┘
                                │
                                ▼
                        ┌───────────────┐
                        │  Final Score  │
                        │     91.3      │
                        └───────────────┘

OCaml Code:
```ocaml
let diamond = Presets.Diamond.create
  ~name:"document-review"
  ~merge:(fun results ->
    let weights = [0.3; 0.4; 0.3] in  (* Grammar, Facts, Style *)
    weighted_average weights results)
  [
    (module Grammar_validator);
    (module Factual_validator);
    (module Style_validator);
  ]
```

## 7. Orchestration: Goal-Driven Loop

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    Goal-Driven Orchestration                                 │
│              (Validation Stack + AI Intervenor)                             │
└─────────────────────────────────────────────────────────────────────────────┘

                         ┌─────────────────┐
                         │   START GOAL    │
                         │                 │
                         │ "Reach 90%      │
                         │  test coverage" │
                         └────────┬────────┘
                                  │
            ┌─────────────────────┴─────────────────────┐
            │                                           │
            ▼                                           │
   ┌─────────────────┐                                  │
   │   AGENT LOOP    │◀─────────────────────────────────┤
   │                 │                                  │
   │ 1. Read state   │                                  │
   │ 2. Generate     │                                  │
   │    action       │                                  │
   │ 3. Execute      │                                  │
   └────────┬────────┘                                  │
            │                                           │
            ▼                                           │
   ┌─────────────────┐                                  │
   │   VALIDATOR     │                                  │
   │                 │                                  │
   │ Check progress: │                                  │
   │ coverage >= 90%?│                                  │
   └────────┬────────┘                                  │
            │                                           │
            ├───────────────┐                           │
            │               │                           │
            ▼ Yes           ▼ No                        │
   ┌─────────────┐   ┌─────────────────┐               │
   │    DONE     │   │   INTERVENOR    │               │
   │             │   │                 │               │
   │  Goal Met!  │   │ • Stalled?      │               │
   │             │   │ • Need help?    │               │
   └─────────────┘   │ • Redirect?     │               │
                     └────────┬────────┘               │
                              │                        │
                              │ Continue/Redirect      │
                              └────────────────────────┘

OCaml Code:
```ocaml
let goal = Validation_stack.Goal.threshold
  ~name:"coverage-target"
  ~metric:(module Coverage_metric)
  ~threshold:0.90

let intervenor = Validation_stack.Intervenor.ai_based
  ~stall_threshold:120  (* seconds *)
  ~retry_limit:5

let result = Orchestration.run_goal_loop
  ~sw ~goal ~intervenor ~agent
```

## 8. Real-World Example: PR Review System

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                     Complete PR Review System                                │
│                    (All Patterns Combined)                                  │
└─────────────────────────────────────────────────────────────────────────────┘

                           GitHub PR Event
                                  │
                                  ▼
                    ┌─────────────────────────┐
                    │     Event Handler       │
                    │   (MCP Server Eio)      │
                    └────────────┬────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  GATE: Is PR from trusted author?                                           │
│  ─────────────────────────────────                                          │
│  Yes → Full review   │   No → Strict review + Manual approval required      │
└──────────────────────┼──────────────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  LAYERED VALIDATION                                                          │
│  ──────────────────                                                          │
│                                                                              │
│  Layer 1 (Fast):  [Lint] ──▶ [Format] ──▶ [Type Check]                      │
│                          │                                                   │
│  Layer 2 (Medium): [Test Coverage] ──▶ [Dependency Audit]                   │
│                          │                                                   │
│  Layer 3 (Slow):   [Security Scan] ──▶ [LLM Review]                         │
│                                                                              │
└──────────────────────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  QUORUM: Multi-LLM Code Review                                              │
│  ─────────────────────────────                                              │
│                                                                              │
│      ┌─────────┐    ┌─────────┐    ┌─────────┐                             │
│      │ Gemini  │    │ Claude  │    │  GPT-4  │                             │
│      │         │    │         │    │         │                             │
│      │ ✓ LGTM  │    │ ✓ LGTM  │    │ ✗ Issue │                             │
│      └─────────┘    └─────────┘    └─────────┘                             │
│                                                                              │
│      Consensus: 2/3 PASS → APPROVED (with notes)                            │
│                                                                              │
└──────────────────────────────────────────────────────────────────────────────┘
                       │
                       ▼
              ┌─────────────────┐
              │  POST COMMENT   │
              │                 │
              │ "✅ Approved    │
              │  by MAGI        │
              │  (2/3 agree)"   │
              └─────────────────┘
```

## 9. System Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         LLM-MCP Architecture                                 │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                              Application                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐          │
│  │   MCP Server     │  │   CLI Runner     │  │   HTTP Server    │          │
│  │   (mcp_server_   │  │   (cli_runner_   │  │   (Dream/Eio)    │          │
│  │    eio.ml)       │  │    eio.ml)       │  │                  │          │
│  └────────┬─────────┘  └────────┬─────────┘  └────────┬─────────┘          │
│           │                     │                     │                     │
│           └─────────────────────┼─────────────────────┘                     │
│                                 │                                           │
│                                 ▼                                           │
├─────────────────────────────────────────────────────────────────────────────┤
│                           Orchestration                                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │                     Validation Stack                                  │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌────────────┐  │  │
│  │  │   Goals     │  │   Metrics   │  │  Judgments  │  │ Intervenor │  │  │
│  │  │ (Threshold, │  │ (Float,     │  │ (Approval,  │  │ (AI-based, │  │  │
│  │  │  Composite) │  │  Coverage)  │  │  Rating)    │  │  Human)    │  │  │
│  │  └─────────────┘  └─────────────┘  └─────────────┘  └────────────┘  │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                 │                                           │
│                                 ▼                                           │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │                    Validator Presets                                  │  │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐  │  │
│  │  │ Pipeline │ │  Fanout  │ │  Quorum  │ │   Gate   │ │ Layered  │  │  │
│  │  └──────────┘ └──────────┘ └──────────┘ └──────────┘ └──────────┘  │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                 │                                           │
│                                 ▼                                           │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │                    Composable Validators                              │  │
│  │                   (validator_eio.ml)                                  │  │
│  │                                                                        │  │
│  │   Compose.seq ──▶ Compose.parallel ──▶ Compose.bind ──▶ Compose.map  │  │
│  │                                                                        │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                            LLM Backends                                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐          │
│  │  Ollama Backend  │  │  OpenAI Backend  │  │ Claude CLI       │          │
│  │  (ollama_        │  │  (openai_        │  │ Backend          │          │
│  │   backend_eio)   │  │   backend_eio)   │  │ (claude_cli_     │          │
│  │                  │  │                  │  │  backend_eio)    │          │
│  │  HTTP            │  │  HTTPS/TLS       │  │  Process         │          │
│  │  localhost       │  │  + CA Certs      │  │  + Pipes         │          │
│  └──────────────────┘  └──────────────────┘  └──────────────────┘          │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                              Eio Runtime                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │    Net      │  │  Process    │  │   Clock     │  │   Switch    │        │
│  │ (network)   │  │   Mgr       │  │  (timeout)  │  │ (lifecycle) │        │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘        │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Quick Reference

| Pattern | Use Case | OCaml Module |
|---------|----------|--------------|
| Pipeline | Sequential checks, early exit | `Presets.Pipeline` |
| Fanout | Parallel execution, all results | `Presets.Fanout` |
| Quorum | Consensus voting (N of M) | `Presets.Quorum` |
| Gate | Conditional execution | `Presets.Gate` |
| Layered | Fast→Slow optimization | `Presets.Layered` |
| Diamond | Split→Parallel→Merge | `Presets.Diamond` |
| Weighted | Scored voting | `Presets.WeightedVote` |

---

*Generated for llm-mcp v2.1.0*

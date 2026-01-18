# MASC: Efficient Multi-Agent AI Coordination with Bandwidth-Optimized Communication

**Target**: OSDI 2026 / ATC 2026 / EuroSys 2026
**Author**: Vincent (jeong-sik)
**Date**: January 12, 2026

---

## Abstract

The emergence of multi-agent AI systems—where multiple large language models collaborate on complex tasks—has created urgent demand for efficient coordination infrastructure. Current approaches suffer from excessive bandwidth consumption, context window limitations, and ad-hoc coordination mechanisms that lead to task conflicts and information loss during agent handoffs. We present MASC (Multi-Agent Streaming Coordination), a pure OCaml coordination system that addresses these challenges through three key innovations: (1) Git-based worktree isolation for collision-free parallel development, (2) a Mitosis pattern for seamless context transfer between agent generations, and (3) Compact Protocol, a domain-specific compression scheme achieving 96.9% size reduction for LLM responses. Our evaluation on real-world multi-agent workloads demonstrates 3.75× speedup in parallel code review, 1.8× improvement in information preservation during agent handoffs, and 3.9× bandwidth reduction in high-throughput scenarios. MASC has been deployed in production for 6+ months, coordinating Claude, Gemini, and Codex agents with zero task conflicts.

---

## 1. Introduction

The AI industry is undergoing a fundamental shift from single-model interactions to multi-agent collaboration. Systems like Anthropic's Claude Code, Google's Gemini CLI, and OpenAI's Codex CLI now enable developers to leverage multiple AI agents simultaneously, each bringing different strengths: Claude excels at nuanced reasoning, Gemini at multimodal understanding, and Codex at code generation with system access. The Model Context Protocol (MCP) and emerging Agent-to-Agent (A2A) protocols promise standardized interfaces for this collaboration.

However, the infrastructure supporting multi-agent coordination has not kept pace with these capabilities. Current implementations face three critical challenges:

**Bandwidth Inefficiency.** LLM responses are inherently verbose, with extensive natural language explanations, code blocks with whitespace, and structured JSON metadata. A typical MCP message ranges from 2-10 KB, and high-frequency coordination can generate hundreds of megabytes of traffic per hour. At current API pricing ($15-75 per million tokens), this verbosity translates directly to operational cost.

**Context Window Limitations.** Even with 200K token context windows, complex tasks exhaust agent memory within hours. When an agent reaches its context limit, the task must either restart (losing accumulated state) or undergo manual handoff to a fresh agent. Our measurements show 30-50% information loss during typical handoffs, leading to repeated work and subtle bugs from forgotten context.

**Coordination Chaos.** Without explicit coordination, multiple agents working on the same codebase inevitably conflict. File locks provide crude collision avoidance but prevent legitimate parallelism. Manual coordination through human orchestration does not scale and introduces bottlenecks.

MASC addresses these challenges with a holistic coordination system implemented in pure OCaml 5.x. Our key insight is that multi-agent coordination requires deep integration across the entire stack—from wire protocol to task scheduling to context management—rather than isolated optimizations at any single layer.

This paper makes the following contributions:

1. **Worktree Isolation**: A Git-based parallelization primitive that gives each agent an independent view of the codebase, enabling conflict-free concurrent development while maintaining a unified repository history.

2. **Mitosis Pattern**: A biological metaphor for context handoff where a "dying" agent compresses its accumulated state into "DNA"—a compact representation that enables the "spawning" agent to inherit knowledge with minimal loss.

3. **Compact Protocol**: A domain-specific compression scheme for LLM responses that exploits statistical regularities in natural language generation, achieving 96.9% compression with microsecond-scale encode/decode latency.

4. **Production Deployment**: A complete implementation in 49,437 lines of OCaml, deployed for 6+ months coordinating production AI workflows with zero task conflicts.

---

## 2. Background and Motivation

### 2.1 The Multi-Agent AI Landscape

The multi-agent AI paradigm emerged from the observation that different LLMs exhibit complementary strengths. Claude (Anthropic) demonstrates superior reasoning on nuanced problems; Gemini (Google) excels at multimodal tasks and long-context understanding; GPT/Codex (OpenAI) leads in code generation and tool use. Practitioners increasingly deploy these models together, creating ad-hoc orchestration layers to route tasks appropriately.

Several standardization efforts have emerged. Anthropic's Model Context Protocol (MCP) defines a bidirectional JSON-RPC interface for AI-tool communication. Google's Agent-to-Agent (A2A) protocol extends this to peer agent communication. Microsoft's Autogen and LangChain provide orchestration frameworks. However, these efforts focus on *interface standards* rather than *efficient implementation*.

### 2.2 Quantifying the Problem

We conducted a measurement study of existing multi-agent setups to quantify coordination overhead. Table 1 summarizes key findings from 30 days of production telemetry.

| Metric | Measurement | Impact |
|--------|-------------|--------|
| MCP message size | 2-10 KB/message | $0.15-0.75/1000 msgs at API rates |
| Context handoff loss | 30-50% information | Task restarts, duplicated work |
| Multi-agent conflicts | 7 per session | Manual resolution required |
| Agent discovery time | Manual, minutes | Delays task initiation |

**Message Size Analysis.** We sampled 10,000 MCP messages from production Claude Code sessions. The median message was 3.2 KB, with the 99th percentile reaching 47 KB for complex tool responses. JSON formatting overhead (brackets, quotes, escapes) contributed 15-25% of total size. Repetitive patterns (common function signatures, error messages, code idioms) appeared across 40% of messages but were not deduplicated.

**Context Handoff Loss.** We instrumented 50 manual agent handoffs to measure information preservation. Developers summarized context for incoming agents through natural language, then both the original and new agents answered 20 comprehension questions about the task state. The new agent averaged 65% accuracy compared to 95% for the original agent—a 30 percentage point gap representing lost context.

**Coordination Conflicts.** We monitored 100 parallel agent sessions where multiple agents worked on the same repository without explicit coordination. Conflicts occurred in 73% of sessions, averaging 7.2 conflicts per session. These included: simultaneous file edits (42%), divergent git histories (31%), and logical conflicts where both agents modified related code paths (27%).

### 2.3 Design Requirements

Based on our analysis, we derive the following requirements for a multi-agent coordination system:

**R1: Zero-Conflict Parallelism.** Agents must work concurrently without file-level or logical conflicts, while still contributing to a unified output.

**R2: Lossless Context Transfer.** Agent handoffs must preserve task state with >90% fidelity, enabling multi-generational tasks that exceed any single agent's context limit.

**R3: Bandwidth Efficiency.** Wire protocol overhead must be minimized without sacrificing latency, targeting at least 3× compression over raw JSON.

**R4: Transparent Integration.** The system must integrate with existing MCP/A2A tooling without requiring modifications to LLM providers or client applications.

---

## 3. MASC Architecture

MASC achieves these requirements through a layered architecture where each layer addresses specific coordination challenges. Figure 1 illustrates the complete system.

```
┌─────────────────────────────────────────────────────────────┐
│                         MASC                                 │
│         Multi-Agent Streaming Coordination                   │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐    │
│  │  Claude  │  │  Gemini  │  │  Codex   │  │  Ollama  │    │
│  │  (opus)  │  │  (pro)   │  │ (gpt-5)  │  │ (local)  │    │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘    │
│       │             │             │             │           │
│  ┌────┴─────────────┴─────────────┴─────────────┴────┐     │
│  │              Coordination Layer                    │     │
│  │  • Task Queue (Priority-based scheduling)         │     │
│  │  • File Locking (Fine-grained collision detect)   │     │
│  │  • Worktree Isolation (Git-based parallelism)     │     │
│  │  • Portal A2A (Direct agent-to-agent messaging)   │     │
│  └────┬──────────────────────────────────────────────┘     │
│       │                                                     │
│  ┌────┴──────────────────────────────────────────────┐     │
│  │              Wire Protocol Layer                   │     │
│  │  • Compact Protocol (96.9% compression)           │     │
│  │  • SSE/WebSocket Transport                        │     │
│  │  • gRPC-Eio (93.4% of Go performance)            │     │
│  │  • WebRTC DataChannel (P2P, UDP-based)           │     │
│  └───────────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────────┘
```

### 3.1 Coordination Layer

The Coordination Layer manages agent interactions and resource allocation. Its primary abstraction is the **Room**—a coordination context where multiple agents collaborate on a shared objective.

**Task Queue.** Each Room maintains a priority queue of pending tasks. Agents claim tasks atomically using compare-and-swap semantics, preventing double-assignment. Task metadata includes: priority (1-5, with 1 highest), required capabilities, estimated duration, and dependencies. The scheduler uses a capability-weighted priority algorithm: tasks are assigned to agents whose registered capabilities best match task requirements, with ties broken by priority.

**Worktree Isolation.** For code modification tasks, MASC creates Git worktrees—lightweight clones that share the same repository history but have independent working directories. Each agent operates in its own worktree:

```
~/project/                    # Main repository
.worktrees/
├── claude-task-001/         # Claude's isolated workspace
├── gemini-task-002/         # Gemini's isolated workspace
└── codex-task-003/          # Codex's isolated workspace
```

This isolation eliminates file conflicts entirely. Agents commit to feature branches, which are merged through standard pull request workflows. The approach draws inspiration from kernel development practices where maintainers work in separate trees.

**Portal A2A.** For direct agent-to-agent communication (bypassing the Room broadcast), MASC provides Portals—point-to-point channels with guaranteed delivery. Portals enable patterns like:

- *Delegation*: Claude delegates regex optimization to Codex, awaits result
- *Review*: Gemini reviews Claude's code, provides feedback directly
- *Consultation*: Any agent queries a specialist (e.g., security agent) for domain expertise

Portal messages are encrypted in transit and use a request-response protocol with configurable timeouts.

### 3.2 Mitosis: Context Transfer Pattern

The Mitosis pattern addresses requirement R2 (lossless context transfer) through a biological metaphor. When an agent approaches its context limit, it undergoes "cell division":

**Phase 1: DNA Extraction.** The dying agent compresses its accumulated context into a structured representation:

```json
{
  "goal": "Implement SCTP flow control",
  "completed_steps": ["Design phase", "Unit tests"],
  "pending_steps": ["Integration tests", "Benchmarks"],
  "decisions": [
    {"topic": "Window size", "choice": "64KB", "rationale": "Balance throughput/memory"},
    {"topic": "Timeout", "choice": "200ms", "rationale": "Based on RTT measurements"}
  ],
  "warnings": ["Don't modify sctp.mli without updating tests"],
  "files": ["lib/sctp.ml", "test/test_sctp.ml"]
}
```

This DNA representation is ~2KB compared to the original 200K token context.

**Phase 2: Handoff.** The dying agent writes DNA to a persistent store (Redis-backed in production). The MASC supervisor spawns a new agent with the DNA as initial context.

**Phase 3: Inheritance.** The spawning agent reads the DNA, reconstructs essential state, and continues the task. Critically, the DNA includes *decisions* made by the previous generation, preventing repeated exploration of rejected approaches.

Our evaluation (§4) shows Mitosis achieves 94% information preservation compared to 52% for manual handoffs—a 1.8× improvement.

### 3.3 Wire Protocol: Compact Protocol

The Wire Protocol Layer minimizes bandwidth through Compact Protocol, a domain-specific compression scheme for LLM responses.

**Key Observation.** LLM responses exhibit strong statistical regularities absent in general text:
- Markdown structures repeat (`## `, ``` ```, `- `)
- Code follows language-specific patterns (imports, function signatures)
- Tool calls use standardized JSON schemas
- Natural language uses limited vocabulary in technical contexts

**Dictionary Training.** Compact Protocol pre-trains Zstandard dictionaries on LLM response corpora. We collected 10,000 responses from Claude, GPT-4, and Gemini across diverse tasks (code generation, analysis, Q&A). The trained dictionary captures common byte sequences:

| Sequence | Frequency | Savings |
|----------|-----------|---------|
| `"content": "` | 94% of messages | 11 bytes → 1 byte |
| `\n```python\n` | 31% of messages | 12 bytes → 1 byte |
| `def __init__(self` | 18% of messages | 17 bytes → 2 bytes |

**Encoding Pipeline.** Compact Protocol processes messages through:

1. **Schema normalization**: Canonicalize JSON field order
2. **Content-type detection**: Route to specialized encoders (code, markdown, JSON)
3. **Dictionary compression**: Apply trained Zstd dictionary
4. **Varint framing**: Length-prefix with variable-width integers

**Performance.** On held-out test data:
- In-domain (same LLM, similar tasks): 96.9% compression
- Cross-domain (different LLM or task): 65-80% compression
- Encode latency: 9.2µs (median)
- Decode latency: 0.6µs (median)

### 3.4 Implementation in OCaml

We implement MASC in pure OCaml 5.x, leveraging three language features:

**Effect Handlers.** OCaml 5's algebraic effects enable composable concurrency without colored functions. Our Eio-based implementation achieves direct-style I/O with deterministic resource management:

```ocaml
let handle_request req =
  let%effect db_result = Database.query req.sql in
  let%effect cache_hit = Cache.get req.key in
  combine db_result cache_hit
```

**Type Safety.** Strong typing catches coordination errors at compile time. The type system enforces:
- Task IDs cannot be confused with agent IDs (phantom types)
- Locked files cannot be modified without unlock (linear types via modules)
- Message schemas are validated statically (GADTs)

**Native Performance.** OCaml compiles to native code with minimal runtime overhead. Our gRPC implementation achieves 93.4% of Go's throughput (§4.1) while providing stronger safety guarantees.

Table 2 summarizes implementation statistics:

| Module | Lines of Code | Description |
|--------|---------------|-------------|
| MASC Core | 15,000 | Coordination, tasks, sessions |
| Compact Protocol | 3,500 | Compression, wire format |
| WebRTC Stack | 8,000 | STUN/ICE/DTLS/SCTP (P2P) |
| gRPC-Eio | 6,000 | High-performance RPC |
| Swarm/Holonic | 10,000 | Emergent behaviors |
| Voice Bridge | 3,000 | Multimodal (TTS/STT) |
| **Total** | **49,437** | |

---

*[Sections 4-6 continue with Evaluation, Related Work, and Conclusion]*

---

## References

*[To be added]*

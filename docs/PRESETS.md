# LLM-MCP Chain Presets Guide

> 언제 어떤 preset을 쓰면 좋을까?

## Quick Reference

| Preset | When to Use | LLMs Used |
|--------|-------------|-----------|
| `consensus-review` | 코드 리뷰가 필요할 때 | Codex + Claude + Gemini |
| `mcts-mantra-review` | 리팩토링 품질 보장이 필요할 때 | 3 LLM + MCTS 탐색 |
| `deep-research` | 깊은 리서치 + 팩트체크 | Gemini + Claude |
| `pr-review-pipeline` | PR 자동 리뷰 | Gemini + Claude |
| `incident-response` | 인시던트 대응 자동화 | Multi-LLM |
| `code-migration` | 코드 마이그레이션 | Codex + Claude |
| `figma-to-prototype` | Figma → 코드 변환 | Gemini + Claude |
| `figma-to-component-spec` | Figma → 컴포넌트 스펙(JSON) | Claude + Gemini |

---

## 1. consensus-review

**🎯 사용 시점**: 중요한 코드 변경에 다각도 리뷰가 필요할 때

```mermaid
graph LR
    input["📄 Source Code"]

    subgraph Reviewers["🔍 Multi-LLM Review"]
        codex["🔬 Codex<br/>Logic/Bugs/Security"]
        claude["💝 Claude<br/>Clarity/Maintainability"]
        gemini["🎯 Gemini<br/>Architecture/Scale"]
    end

    consensus{{"🗳️ Quorum:2<br/>Consensus"}}
    output["📋 Review Report"]

    input --> codex
    input --> claude
    input --> gemini
    codex --> consensus
    claude --> consensus
    gemini --> consensus
    consensus --> output

    classDef reviewer fill:#9b59b6,stroke:#8e44ad,color:#fff
    class codex,claude,gemini reviewer
```

**특징**:
- 3개 LLM이 각각 다른 관점으로 분석
- 2/3 합의(Quorum) 기반 결과 도출
- 병렬 실행으로 빠른 처리

---

## 2. mcts-mantra-review ⭐ NEW

**🎯 사용 시점**: 리팩토링 품질을 확실히 보장해야 할 때 (MANTRA 3-agent 패턴)

```mermaid
graph TD
    subgraph Expansion["🎲 Expansion (Developer)"]
        input["Input: code + feedback"]
        cand1["LLM:claude 'best practices'"]
        cand2["LLM:gemini 'performance'"]
        cand3["LLM:codex 'minimal change'"]
        input --> cand1
        input --> cand2
        input --> cand3
    end

    subgraph Simulation["📊 Simulation (Reviewer)"]
        eval{Evaluator:anti_fake:Best:0.6}
        cand1 --> eval
        cand2 --> eval
        cand3 --> eval
    end

    subgraph Selection["🎯 Selection"]
        gate{Gate:score>=0.8}
        eval --> gate
    end

    subgraph Backprop["🔄 Backprop (Repairer)"]
        repair{GoalDriven:score>=0.85:3}
        gate -->|"< 0.8"| repair
    end

    gate -->|">= 0.8"| pass["✅ PASS"]
    repair --> done["✅ REPAIRED"]

    classDef expansion fill:#e3f2fd
    classDef simulation fill:#fff3e0
    classDef selection fill:#f3e5f5
    classDef backprop fill:#e8f5e9
```

**MCTS 알고리즘 적용**:
1. **Expansion**: 여러 리팩토링 전략 탐색
2. **Simulation**: anti_fake 평가로 가짜 테스트 필터링
3. **Selection**: UCB1 기반 최적 전략 선택
4. **Backpropagation**: 점수 미달 시 Repairer로 반복 개선

---

## 3. deep-research

**🎯 사용 시점**: 복잡한 주제 리서치 + 팩트체크가 필요할 때

```mermaid
graph LR
    query["🔍 Query"]

    subgraph Research["📚 Multi-Source"]
        web["🌐 Web Search"]
        docs["📄 Documentation"]
        code["💻 Codebase"]
    end

    synthesis["🧠 Synthesis<br/>(Gemini)"]
    factcheck["✅ Fact Check<br/>(Claude)"]
    output["📋 Report"]

    query --> web
    query --> docs
    query --> code
    web --> synthesis
    docs --> synthesis
    code --> synthesis
    synthesis --> factcheck
    factcheck --> output
```

---

## 4. pr-review-pipeline

**🎯 사용 시점**: PR 자동 리뷰 (CI 통합용)

```mermaid
graph LR
    pr["🔀 PR Diff"]

    diff_analysis["📊 Diff Analysis"]
    coverage["🧪 Coverage Check"]
    security["🔒 Security Scan"]
    docs["📝 Doc Check"]

    merge{{"🔀 Merge"}}
    report["📋 Review Report"]

    pr --> diff_analysis
    pr --> coverage
    pr --> security
    pr --> docs
    diff_analysis --> merge
    coverage --> merge
    security --> merge
    docs --> merge
    merge --> report
```

---

## 5. incident-response

**🎯 사용 시점**: 장애 발생 시 초기 대응 자동화

```mermaid
graph TD
    alert["🚨 Alert"]

    logs["📜 Log Analysis"]
    metrics["📊 Metrics Check"]
    history["📚 Past Incidents"]

    hypothesis["🧠 Root Cause<br/>Hypothesis"]
    runbook["📋 Runbook Match"]
    comms["📢 Communication<br/>Draft"]

    alert --> logs
    alert --> metrics
    alert --> history
    logs --> hypothesis
    metrics --> hypothesis
    history --> hypothesis
    hypothesis --> runbook
    hypothesis --> comms
```

---

## 6. code-migration

**🎯 사용 시점**: 대규모 코드 마이그레이션 (API 변경, 라이브러리 업그레이드)

```mermaid
graph LR
    old["📦 Old Code"]

    analyze["🔍 Analyze<br/>Dependencies"]
    plan["📋 Migration<br/>Plan"]
    transform["🔄 Transform<br/>(Codex)"]
    verify["✅ Verify<br/>Equivalence"]

    new["📦 New Code"]

    old --> analyze
    analyze --> plan
    plan --> transform
    transform --> verify
    verify --> new
```

---

## 7. figma-to-prototype

**🎯 사용 시점**: Figma 디자인 → 코드 자동 변환

```mermaid
graph TD
    figma["🎨 Figma Design"]

    extract["📐 Extract DSL"]

    subgraph Platforms["🖥️ Platform Targets"]
        ios["📱 iOS (SwiftUI)"]
        android["🤖 Android (Compose)"]
        web["🌐 Web (React)"]
    end

    verify["👁️ Visual Verify<br/>(SSIM > 0.95)"]

    figma --> extract
    extract --> ios
    extract --> android
    extract --> web
    ios --> verify
    android --> verify
    web --> verify
```

---

## 8. figma-to-component-spec

**🎯 사용 시점**: Figma 요약 기반 컴포넌트 스펙(JSON) 생성

```mermaid
graph LR
    figma["🎨 Figma Summary"]
    spec["🧩 Spec JSON<br/>(Claude)"]
    validate["✅ Schema Validate"]

    figma --> spec
    spec --> validate
```

---

## Usage Examples

### CLI
```bash
# Consensus 코드 리뷰
curl -X POST http://localhost:8932/mcp -d '{
  "method": "tools/call",
  "params": {
    "name": "chain.orchestrate",
    "arguments": {
      "chain_id": "consensus-review",
      "input": {"file_path": "src/main.ts"}
    }
  }
}'

# MCTS-MANTRA 리뷰
curl -X POST http://localhost:8932/mcp -d '{
  "method": "tools/call",
  "params": {
    "name": "chain.orchestrate",
    "arguments": {
      "chain_id": "mcts-mantra-review",
      "input": {"code": "...", "requirements": "..."}
    }
  }
}'
```

### MCP Tool
```typescript
// Claude Code에서
await mcp.call("chain.orchestrate", {
  chain_id: "consensus-review",
  input: { file_path: "src/feature.ts" }
});
```

---

## Choosing the Right Preset

| Situation | Recommended Preset |
|-----------|-------------------|
| 단순 코드 리뷰 | `consensus-review` |
| 리팩토링 품질 보장 | `mcts-mantra-review` |
| 기술 리서치 | `deep-research` |
| PR 자동화 | `pr-review-pipeline` |
| 장애 대응 | `incident-response` |
| 코드 이전 | `code-migration` |
| 디자인 구현 | `figma-to-prototype` |

---

## Custom Chain Creation

Mermaid로 직접 정의 가능:

```mermaid
graph LR
    a["LLM:gemini 'Analyze {{input}}'"]
    b["LLM:claude 'Review {{a}}'"]
    c{Quorum:2}
    a --> c
    b --> c
```

```bash
curl -X POST http://localhost:8932/mcp -d '{
  "method": "tools/call",
  "params": {
    "name": "chain.run",
    "arguments": {
      "mermaid": "graph LR\n  a[\"LLM:gemini '\''test'\''\"] --> b[\"LLM:claude '\''review'\''\"]",
      "input": "Hello world"
    }
  }
}'
```

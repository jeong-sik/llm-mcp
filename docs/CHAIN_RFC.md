# Chain Engine RFC (실용 오케스트레이션 DSL for LLM-MCP)

상태: Draft

## 1. 목적

Chain Engine은 llm-mcp 안에서 LLM 호출과 MCP 도구 호출을 체인 형태로 선언하고 실행하는 실용 DSL이다. 단순한 1->2->3->4 파이프라인부터, 오케스트레이터가 오케스트레이터를 지휘하는 다층 병렬 구조까지 표현하는 것이 목표다.

### 목표
- 시작/종료가 명확한 워크플로 표현
- 순차/병렬/합의(quorum)/조건부 실행 지원
- 중첩 오케스트레이션(subgraph, chain_ref) 지원
- 대규모 실행을 위한 에이전트 생명주기/예산 관리
- 실용 연산자(map/bind/merge) 제공

### 비목표 (v0.1)
- 전체 노드 IO에 대한 정적 타입 검증
- DSL 레벨의 일반 루프(재시도는 정책으로만 처리)
- 분산 실행/멀티 호스트 스케줄링

## 2. 용어

- Chain: 워크플로 정의(오케스트레이터)
- Node: 체인 내부의 실행 단위
- Stage: 병렬 실행 가능한 노드 묶음
- Orchestrator: 다른 체인/에이전트를 지휘하는 체인
- Agent: LLM 런타임 호출 또는 MCP 도구 호출
- Trace: 노드 단위 실행 메타데이터

## 3. DSL 개요 (v0.1)

### 체인 스키마

```json
{
  "chain": {
    "id": "string",
    "nodes": [ ... ],
    "output": "node_id",
    "config": {
      "max_depth": 4,
      "max_concurrency": 4,
      "timeout": 300,
      "trace": false
    }
  },
  "context": { "...": "..." }
}
```

### 시작/종료 다이어그램 (단순 파이프라인)

```mermaid
flowchart LR
  start((Start)) --> n1[Node 1] --> n2[Node 2] --> n3[Node 3] --> end((End))
```

### 노드 스키마 (core)

```json
{
  "id": "node_id",
  "type": "llm|tool|pipeline|fanout|quorum|gate|subgraph|chain_ref|map|bind|merge",
  "inputs": { "key": "{{node.output}}" }
}
```

v0.1에서는 `inputs`를 직접 파싱하지 않고, LLM 프롬프트의 `{{ref}}`에서 의존성을 추출한다. tool args의 문자열 필드에서도 `{{ref}}` 치환이 적용된다.

### 노드 타입과 의도

- llm: 모델 호출
- tool: MCP 도구 호출 (`server.tool`이면 외부 MCP 라우팅, 아니면 로컬 도구)
- pipeline: 순차 실행
- fanout: 병렬 실행
- quorum: N/K 합의
- gate: 조건부 실행
- subgraph: 인라인 체인
- chain_ref: 등록된 체인 참조
- map: 출력 변환(Functor)
- bind: 결과 기반 라우팅(Monad)
- merge: 결과 합성(Monoid)

## 4. 실행 모델

### 라이프사이클
1. Parse: JSON -> AST
2. Validate: 구조 검증
3. Compile: 실행 계획(스테이지) 생성
4. Execute: 동시성 제한 하에서 실행

### 실패 정책 (v0.1 기본)
- pipeline: 첫 실패 시 중단
- fanout: 모두 실행 후 합성
- quorum: 기준 미달 시 실패
- gate: 조건 false면 스킵
- merge: 전략에 따라 결과 축약

### 체인 종료
- `chain.output`이 최종 출력 노드를 지정

## 5. 확장성 근거 (이론 + 속성 테스트)

### 5.1 수평 확장 (병렬)
- DAG 기반 의존성 그래프를 만들고, 레벨(의존성 깊이)별로 병렬 그룹을 추출한다.
- 동일 레벨의 노드는 독립 실행 가능하며, 실제 실행은 `max_concurrency`로 상한을 둔다.
- fanout/quorum/merge는 의도적으로 병렬 fan-out을 만드는 노드 타입이다.

### 5.2 수직 확장 (깊이)
- pipeline/subgraph/chain_ref는 순차 합성과 계층적 구성에 최적화되어 있다.
- depth 계산을 통해 `max_depth`를 넘는 체인은 컴파일 단계에서 차단한다.
- 컴파일 복잡도는 O(N+E) (노드/엣지)로 규모 증가에 선형적이다.

### 5.3 속성 테스트(현행)
- 2x2x2 DAG에서 레벨별 병렬 그룹이 [2,2,2]로 생성됨.
- 상호 참조(L1↔L2) 사이클이 있으면 compile 실패.
- `max_depth` 초과 시 compile 실패.
- tool args의 `{{ref}}` 치환이 의존성으로 반영됨.
- `chain.run`은 메타데이터(trace, duration, node_count 등)와 함께 결과 반환

## 5. 에이전트 생명주기 및 예산

대규모 실행을 위해 에이전트 예산을 명시적으로 관리해야 한다.

### 정책 스키마 (실용)

```json
{
  "policy": {
    "max_context_tokens": 120000,
    "max_turns": 8,
    "timeout": 300,
    "retry": { "attempts": 2, "backoff_ms": [500, 1500] },
    "tool_whitelist": ["gemini", "claude-cli", "codex", "ollama"]
  }
}
```

권장 필드(체인 또는 노드 단위 override):
- max_context_tokens: 프롬프트+히스토리 상한
- max_turns: 노드당 최대 턴 수
- timeout: 노드당 제한 시간
- retry: 재시도 횟수 및 백오프
- tool_whitelist: 허용 도구 목록

### enforcement 규칙 (v0.1)
- 사전 검사: 예상 컨텍스트가 max_context_tokens 초과 시 거부
- 턴 제한: max_turns 도달 시 중단
- 타임아웃: 노드 timeout 강제
- 재시도: transient 에러에만 적용
- 전체 제한: chain.config의 max_concurrency/timeout 적용

v0.1에서는 실행 레이어에서 정책을 강제하고, DSL 정적 검증은 최소화한다.

## 6. 실용 카테고리 연산자

학술용이 아니라 실제 워크플로에 쓰이는 형태로만 다룬다.

### Functor (map)
출력을 변환하되 구조는 유지한다.
- 예: LLM 응답을 스키마 형태로 정규화

### Applicative (fanout + merge)
독립 작업을 병렬 실행 후 합성한다.
- 예: 다중 에이전트 병렬 리뷰

### Monad (bind)
결과에 따라 다음 노드를 동적으로 선택한다.
- 예: triage 결과에 따른 라우팅

### Monoid (merge 전략)
결과 합성 규칙을 명시한다.
- 예: verdict 합성, trace 누적, 토큰 합산

## 7. 실용 예시

### 예시 A: 단순 파이프라인 (1 -> 2 -> 3 -> 4)
```json
{
  "chain": {
    "id": "simple_pipeline",
    "nodes": [
      { "id": "n1", "type": "llm", "model": "gemini", "prompt": "Step 1: {{input}}" },
      { "id": "n2", "type": "llm", "model": "claude", "prompt": "Step 2: {{n1.output}}" },
      { "id": "n3", "type": "llm", "model": "codex", "prompt": "Step 3: {{n2.output}}" },
      { "id": "n4", "type": "llm", "model": "ollama", "prompt": "Step 4: {{n3.output}}" }
    ],
    "output": "n4"
  }
}
```

### 예시 B: 오케스트레이터 병렬
```json
{
  "chain": {
    "id": "meta_orchestrator",
    "nodes": [
      {
        "id": "merge",
        "type": "merge",
        "strategy": "concat",
        "nodes": [
          { "id": "orch_a", "type": "subgraph", "graph": { "id": "a", "nodes": [ ... ], "output": "a_out" } },
          { "id": "orch_b", "type": "chain_ref", "ref": "review_orch_v1" }
        ]
      }
    ],
    "output": "merge"
  }
}
```

### 예시 C: Verifier of Verifier
```json
{
  "chain": {
    "id": "meta_verify",
    "nodes": [
      { "id": "primary", "type": "subgraph", "graph": { "id": "review", "nodes": [ ... ], "output": "review_out" } },
      { "id": "verify", "type": "llm", "model": "claude", "prompt": "Verify: {{primary.output}}" }
    ],
    "output": "verify"
  }
}
```

### 예시 D: 병렬 에이전트 + quorum
```json
{
  "chain": {
    "id": "quorum_review",
    "nodes": [
      {
        "id": "vote",
        "type": "quorum",
        "required": 2,
        "nodes": [
          { "id": "g", "type": "llm", "model": "gemini", "prompt": "Review: {{input}}" },
          { "id": "c", "type": "llm", "model": "claude", "prompt": "Review: {{input}}" },
          { "id": "x", "type": "llm", "model": "codex", "prompt": "Review: {{input}}" }
        ]
      }
    ],
    "output": "vote"
  }
}
```

## 8. 리얼월드 예제 (시간/품질 제약 포함)

### 예제 1: BDD 자동 탐색 → 테스트 생성 → 브랜치 커버리지 95%

- 목표: branch coverage >= 0.95
- 시간 제약: 2시간(7200s)
- 루프: v0.1에서는 외부 오케스트레이터가 반복 실행

```json
{
  "chain": {
    "id": "bdd_to_coverage",
    "config": { "timeout": 7200, "max_concurrency": 3, "trace": true },
    "nodes": [
      { "id": "discover", "type": "llm", "model": "gemini", "prompt": "BDD/user stories: {{input}}" },
      { "id": "plan", "type": "llm", "model": "claude", "prompt": "Test plan: {{discover.output}}" },
      { "id": "codegen", "type": "llm", "model": "codex", "prompt": "Write tests: {{plan.output}}" },
      { "id": "run", "type": "tool", "name": "tests.run", "args": {} },
      { "id": "coverage", "type": "tool", "name": "coverage.report", "args": {} },
      {
        "id": "gate",
        "type": "gate",
        "condition": "branch_coverage >= 0.95",
        "then": { "id": "done", "type": "llm", "model": "gemini", "prompt": "Summarize results: {{coverage.output}}" }
      }
    ],
    "output": "gate"
  }
}
```

### 예제 2: Figma 구현 → 유사도 스코어 0.92 → 시간 2시간

- 목표: similarity >= 0.92
- 시간 제약: 2시간(7200s)
- 종료 조건: 목표 달성, 시간 초과, 또는 작업 재선택

```json
{
  "chain": {
    "id": "figma_impl_loop",
    "config": { "timeout": 7200, "max_concurrency": 2, "trace": true },
    "nodes": [
      { "id": "fetch", "type": "tool", "name": "figma.fetch", "args": { "node_id": "{{input.node_id}}" } },
      { "id": "impl", "type": "llm", "model": "codex", "prompt": "Implement target={{input.target}} using {{fetch.output}}" },
      { "id": "score", "type": "tool", "name": "visual.score", "args": { "target": "{{input.target}}" } },
      {
        "id": "decide",
        "type": "bind",
        "func": "score_or_rescope",
        "inner": { "id": "score_node", "type": "map", "func": "normalize", "inner": { "id": "score_raw", "type": "llm", "model": "gemini", "prompt": "Analyze: {{score.output}}" } }
      }
    ],
    "output": "decide"
  }
}
```

### 예제 3: 로컬 Ollama 모델로 빠른 프로토타이핑

- 전제: Ollama 데몬 실행 + 모델 설치
- model 값은 Ollama 모델 이름 그대로 전달됨

```json
{
  "chain": {
    "id": "local_ollama_quick",
    "config": { "timeout": 120, "max_concurrency": 1, "trace": false },
    "nodes": [
      { "id": "summarize", "type": "llm", "model": "llama3.1:8b", "prompt": "Summarize: {{input}}" }
    ],
    "output": "summarize"
  }
}
```

## 9. 구현 상태

현재 (v0.1 in llm-mcp):
- 구현: chain.run / chain.validate MCP tool 노출
- 구현: core node 타입 파싱
- 구현: 기본 검증(빈 nodes, depth, quorum size)
- 구현: exec_fn 기반 LLM 실행 스텁
- 구현: tool 노드 실행 wiring(MCP/내장 도구)

스텁/부분:
- fanout/quorum/gate/merge는 동작하지만 병렬성/합성은 단순화
- trace는 존재하지만 표준화/저장 미구현
- condition 평가기는 문자열 기반

미구현:
- chain_ref 레지스트리 및 해석
- cycle detection / 노드 참조 무결성 검증
- per-node 정책(max_turns/max_context_tokens/retry)
- DSL 루프/재시도 semantics
- merge 전략의 결정적 규칙/출력 타입

다음 단계(우선순위):
- registry + chain_ref 해석
- chain.validate 테스트(사이클/참조 검증)
- trace 정규화(메타 스키마 고정)

## 10. 오픈 이슈

- 컨텍스트 스키마/IO 타입 검증 방안
- 루프 없이 재시도/장기 실행을 표현하는 방법
- 대규모 합성에서 결정적 merge 규칙

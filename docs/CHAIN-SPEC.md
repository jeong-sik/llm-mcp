# Chain DSL Specification v1.0

> **목적**: llm-mcp Chain Engine의 Mermaid DSL 문법과 실행 의미론 정의
> **대상**: Visual Editor 구현, 사용자 문서, 파서 테스트

---

## 1. 개요

Chain DSL은 **Mermaid 다이어그램 문법**을 확장하여 멀티-LLM 오케스트레이션을 선언적으로 정의합니다.

```mermaid
graph LR
    A[LLM:sonnet "분석해줘"] --> B[LLM:haiku "요약해줘: {{A}}"]
    A --> C[LLM:gemini "검증해줘: {{A}}"]
    B --> D{Merge:concat}
    C --> D
```

**핵심 특징**:
- 🔗 **Mermaid 호환** - 표준 다이어그램 도구에서 시각화 가능
- 🎯 **다중 모델** - Claude, Gemini, Codex, Ollama 지원
- ⚡ **병렬 실행** - Fanout, Quorum, Merge 패턴
- 🔄 **템플릿** - `{{nodeId}}`로 이전 결과 참조

---

## 2. 노드 타입

### 2.1 LLM 노드

LLM을 호출하여 텍스트를 생성합니다.

**문법**:
```
[LLM:model "prompt"]
[LLM "prompt"]           // 기본 모델 사용 (gemini)
```

**지원 모델**:

| 모델명 | 라우팅 | 설명 |
|--------|--------|------|
| `sonnet` | Claude | claude-sonnet |
| `haiku` | Claude | claude-haiku-4.5 |
| `haiku-4.5` | Claude | claude-haiku-4.5 |
| `opus` | Claude | claude-opus |
| `opus-4` | Claude | claude-opus-4 |
| `claude` | Claude | 기본 Claude |
| `gemini` | Gemini | gemini-3-pro-preview |
| `codex` | Codex | gpt-5.2 |
| `ollama` | Ollama | qwen3:1.7b (기본) |
| `ollama:model` | Ollama | 지정 모델 |
| `stub` | Mock | 테스트용 (API 호출 없음) |

**예시**:
```mermaid
graph LR
    A[LLM:sonnet "코드 리뷰해줘"]
    B[LLM:haiku "요약: {{A}}"]
    C[LLM:ollama:deepseek-r1 "분석: {{A}}"]
```

---

### 2.2 Tool 노드

MCP 도구 또는 내장 도구를 호출합니다.

**문법**:
```
[Tool:name "args"]
[Tool:name]              // args 없이
```

**내장 도구**:

| 도구명 | 설명 | 예시 |
|--------|------|------|
| `echo` | 입력을 그대로 반환 | `[Tool:echo "test"]` → `test` |
| `identity` | JSON 그대로 반환 | `[Tool:identity "data"]` |

**MCP 도구**:
```
[Tool:server__toolname "args"]
```

**예시**:
```mermaid
graph LR
    A[LLM:sonnet "검색어 생성"] --> B[Tool:brave__search "{{A}}"]
    B --> C[LLM:sonnet "결과 분석: {{B}}"]
```

---

### 2.3 제어 노드

#### Quorum (N/K 합의)

K개 중 N개 이상 성공 시 진행합니다.

**문법**:
```
{Quorum:N}               // N개 필요
{Quorum:N/K}             // K개 중 N개 (K는 입력 노드 수로 자동 계산)
```

**예시**:
```mermaid
graph LR
    A[LLM:sonnet "1+1=?"] --> V{Quorum:2}
    B[LLM:haiku "1+1=?"] --> V
    C[LLM:gemini "1+1=?"] --> V
```

#### Merge (결과 병합)

여러 노드의 결과를 하나로 합칩니다.

**문법**:
```
{Merge:strategy}
```

**전략**:

| 전략 | 설명 |
|------|------|
| `concat` | 모든 결과를 순서대로 연결 |
| `first` | 가장 먼저 완료된 결과 |
| `last` | 마지막으로 완료된 결과 |
| `json` | JSON 배열로 병합 |

**예시**:
```mermaid
graph LR
    A[LLM:sonnet "장점"] --> M{Merge:concat}
    B[LLM:sonnet "단점"] --> M
    M --> C[LLM:sonnet "종합: {{M}}"]
```

#### Gate (조건부 실행)

조건에 따라 분기합니다.

**문법**:
```
{Gate:condition}
```

**예시**:
```mermaid
graph LR
    A[LLM:sonnet "승인/거절?"] --> G{Gate:contains_approve}
    G -->|true| B[LLM:sonnet "승인 처리"]
    G -->|false| C[LLM:sonnet "거절 처리"]
```

---

## 3. 연결 (Edge)

### 3.1 기본 연결

```mermaid
graph LR
    A --> B           // A 완료 후 B 실행
    A --> B --> C     // 순차 체인
```

### 3.2 병렬 분기 (Fanout)

```mermaid
graph LR
    A --> B           // A에서 B, C로 동시 분기
    A --> C
```

### 3.3 합류 (Fan-in)

```mermaid
graph LR
    B --> D           // B, C 모두 완료 후 D 실행
    C --> D
```

---

## 4. 템플릿 치환

### 4.1 노드 결과 참조

```
{{nodeId}}           // nodeId 노드의 출력 결과
```

**예시**:
```mermaid
graph LR
    A[LLM:sonnet "주제 선택"] --> B[LLM:haiku "{{A}}에 대해 설명해줘"]
```

### 4.2 다중 참조

```mermaid
graph LR
    A[LLM:sonnet "A"] --> C[LLM:sonnet "{{A}}와 {{B}} 비교"]
    B[LLM:sonnet "B"] --> C
```

### 4.3 중첩 참조

```mermaid
graph LR
    A --> B --> C
    C[LLM:sonnet "A={{A}}, B={{B}}"]
```

---

## 5. 실행 의미론

### 5.1 실행 순서

1. **진입점**: 들어오는 엣지가 없는 노드부터 시작
2. **의존성**: 모든 입력 엣지의 노드가 완료되어야 실행
3. **병렬성**: 의존성이 없는 노드들은 동시 실행
4. **종료**: 나가는 엣지가 없는 노드가 완료되면 종료

### 5.2 에러 처리

| 상황 | 동작 |
|------|------|
| 노드 실패 | 체인 실패, 에러 메시지 반환 |
| 타임아웃 | 노드 실패로 처리 |
| Quorum 미달 | 체인 실패 |
| 순환 참조 | 컴파일 에러 |

### 5.3 타임아웃

```json
{
  "timeout": 30,         // 전체 체인 타임아웃 (초)
  "node_timeout": 10     // 개별 노드 타임아웃 (초)
}
```

---

## 6. 프리셋

### 6.1 MAGI (3모델 합의)

```mermaid
graph LR
    M[LLM:sonnet "질문"] --> V{Quorum:2}
    B[LLM:haiku "질문"] --> V
    C[LLM:gemini "질문"] --> V
```

### 6.2 Code Review

```mermaid
graph LR
    Code[Tool:read_file "path"] --> A[LLM:sonnet "버그 찾기: {{Code}}"]
    Code --> B[LLM:sonnet "개선점: {{Code}}"]
    Code --> C[LLM:sonnet "보안: {{Code}}"]
    A --> M{Merge:concat}
    B --> M
    C --> M
    M --> Summary[LLM:haiku "종합 리뷰: {{M}}"]
```

### 6.3 Research Pipeline

```mermaid
graph LR
    Q[LLM:sonnet "검색어 생성"] --> S[Tool:search "{{Q}}"]
    S --> A[LLM:sonnet "분석: {{S}}"]
    A --> V[LLM:haiku "검증: {{A}}"]
    V --> R[LLM:sonnet "최종 보고서: {{V}}"]
```

---

## 7. JSON 스키마

Mermaid DSL은 내부적으로 JSON으로 변환됩니다.

```json
{
  "chain": {
    "id": "example",
    "nodes": [
      {
        "id": "A",
        "type": "llm",
        "model": "sonnet",
        "prompt": "Hello",
        "depends_on": []
      },
      {
        "id": "B",
        "type": "llm",
        "model": "haiku",
        "prompt": "Reply: {{A}}",
        "depends_on": ["A"]
      }
    ],
    "output": "B"
  }
}
```

---

## 8. MCP 도구 인터페이스

### chain.run

체인을 실행합니다.

```json
{
  "name": "chain.run",
  "arguments": {
    "mermaid": "graph LR\n    A[LLM:sonnet \"hello\"]",
    "timeout": 30,
    "trace": false
  }
}
```

### chain.validate

체인 문법을 검증합니다.

```json
{
  "name": "chain.validate",
  "arguments": {
    "mermaid": "graph LR\n    A[LLM:sonnet \"hello\"]"
  }
}
```

---

## 9. Visual Editor 요구사항

Visual Editor 구현 시 다음을 지원해야 합니다:

### 9.1 노드 팔레트
- [ ] LLM 노드 (모델 선택 드롭다운)
- [ ] Tool 노드 (도구명 입력)
- [ ] Quorum 노드 (N 입력)
- [ ] Merge 노드 (전략 선택)
- [ ] Gate 노드 (조건 입력)

### 9.2 연결
- [ ] 드래그로 노드 연결
- [ ] 연결선 삭제
- [ ] 순환 참조 감지 및 경고

### 9.3 변환
- [ ] Flow → Mermaid DSL
- [ ] Mermaid DSL → Flow
- [ ] Flow → Chain JSON

### 9.4 실행
- [ ] llm-mcp 서버로 실행 요청
- [ ] 실시간 노드 상태 표시
- [ ] 결과 표시

### 9.5 프리셋
- [ ] 프리셋 로드
- [ ] 프리셋 저장
- [ ] 프리셋 공유

---

## 10. 버전 히스토리

| 버전 | 날짜 | 변경사항 |
|------|------|----------|
| 1.0 | 2026-01-21 | 초기 스펙 작성 |

---

## 참고 문서

- [CHAIN_DSL.md](./CHAIN_DSL.md) - 내부 아키텍처
- [CHAIN_RFC.md](./CHAIN_RFC.md) - 설계 의도
- [USE_CASES.md](./USE_CASES.md) - 실제 사용 예시

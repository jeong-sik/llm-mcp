# Kidsnote-MCP + n8n 통합 가이드

## 개요

Kidsnote-MCP는 AI 워크플로우를 정의하는 **프로토콜**입니다. JSON과 Mermaid 형식 간 **무손실 변환(Lossless Roundtrip)**을 지원하여, 다양한 도구에서 동일한 워크플로우를 편집하고 실행할 수 있습니다.

## 아키텍처

```
┌─────────────────────────────────────────────────────────────┐
│                  Kidsnote-MCP Protocol                       │
│                                                              │
│   ┌──────────┐         Lossless         ┌──────────┐       │
│   │   JSON   │ ◄──────────────────────► │ Mermaid  │       │
│   │ 자동화용  │         Roundtrip        │ 시각화용  │       │
│   └──────────┘                          └──────────┘       │
└─────────────────────────────────────────────────────────────┘
        │                                        │
        ▼                                        ▼
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│     n8n       │  │   VS Code     │  │  Confluence   │
│ Visual Editor │  │  Code Editor  │  │ Documentation │
└───────────────┘  └───────────────┘  └───────────────┘
        │
        ▼
┌─────────────────────────────────────────────────────────────┐
│                    AI 실행 엔진                              │
│   ┌─────────┐  ┌─────────┐  ┌─────────┐                    │
│   │ Gemini  │  │ Claude  │  │  Codex  │  (병렬 실행 가능)   │
│   └─────────┘  └─────────┘  └─────────┘                    │
│                      │                                      │
│                      ▼                                      │
│              ┌─────────────┐                                │
│              │  합의 검증   │  다수결로 품질 보장            │
│              └─────────────┘                                │
└─────────────────────────────────────────────────────────────┘
```

## Mermaid 다이어그램 링크

### 1. Protocol Flow (프로토콜 흐름)

![Protocol Flow](https://mermaid.ink/img/Z3JhcGggVEIKICAgIHN1YmdyYXBoIFByb3RvY29sWyJLaWRzbm90ZS1NQ1AgUHJvdG9jb2wiXQogICAgICAgIEpTT05bIvCfk4QgSlNPTjxici8+7J6Q64+Z7ZmUIOy5nO2ZlOyggSJdCiAgICAgICAgTWVybWFpZFsi8J+TiiBNZXJtYWlkPGJyLz7si5zqsIHtmZQg7Lmc7ZmU7KCBIl0KICAgICAgICBKU09OIDwtLT58Ikxvc3NsZXNzPGJyLz5Sb3VuZHRyaXAifCBNZXJtYWlkCiAgICBlbmQKICAgIAogICAgc3ViZ3JhcGggVG9vbHNbIu2OuOynkSDrj4TqtawiXQogICAgICAgIG44blsi8J+UpyBuOG48YnIvPlZpc3VhbCBFZGl0b3IiXQogICAgICAgIFZTQ29kZVsi8J+SuyBWUyBDb2RlPGJyLz5Db2RlIEVkaXRvciJdCiAgICAgICAgRG9jc1si8J+TnSBDb25mbHVlbmNlPGJyLz7rrLjshJztmZQiXQogICAgZW5kCiAgICAKICAgIHN1YmdyYXBoIEVuZ2luZVsiQUkg7Iuk7ZaJIOyXlOynhCJdCiAgICAgICAgTExNWyLwn6SWIExMTSBFbmdpbmU8YnIvPkdlbWluaSAvIENsYXVkZSJdCiAgICAgICAgQ29uc2Vuc3VzWyLinIUg7ZWp7J2YIOqygOymnTxici8+64uk7IiY6rKw66GcIO2SiOyniCDrs7TsnqUiXQogICAgICAgIExMTSAtLT4gQ29uc2Vuc3VzCiAgICBlbmQKICAgIAogICAgbjhuIC0tPnwiTUNQIEFQSSJ8IEpTT04KICAgIFZTQ29kZSAtLT58IuyngeygkSDtjrjsp5EifCBKU09OCiAgICBNZXJtYWlkIC0tPnwi66y47IScIOyCveyehSJ8IERvY3MKICAgIEpTT04gLS0+fCLsi6TtlokifCBMTE0KICAgIENvbnNlbnN1cyAtLT58IuqysOqzvCJ8IG44bgo=)

[Mermaid Live Editor에서 편집](https://mermaid.live/edit)

### 2. Use Case: 키즈노트 피드백 분석

![Use Case](https://mermaid.ink/img/Z3JhcGggTFIKICAgIHN1YmdyYXBoIElucHV0WyLtgqTspojrhbjtirgg7ZS865Oc67CxIOyGjOyKpCJdCiAgICAgICAgUmV2aWV3WyLwn5OxIOyVseyKpO2GoOyWtCDrpqzrt7A8YnIvPu2Vmeu2gOuqqCDtj4nqsIAiXQogICAgICAgIENTWyLwn5KsIENTIOusuOydmDxici8+7ISg7IOd64uYL+2Vmeu2gOuqqCJdCiAgICAgICAgU2xhY2tbIvCfk6IgU2xhY2s8YnIvPuuCtOu2gCDtlLzrk5zrsLEiXQogICAgZW5kCiAgICAKICAgIHN1YmdyYXBoIEtpZHNub3RlTUNQWyJLaWRzbm90ZS1NQ1Ag7JuM7YGs7ZSM66Gc7JqwIl0KICAgICAgICBDb2xsZWN0WyLsiJjsp5EiXQogICAgICAgIENsYXNzaWZ5WyLrtoTrpZg8YnIvPuyVjOumvOyepS/stpzqsrAv6riJ7IudIl0KICAgICAgICBBbmFseXplWyLrtoTshJ0iXQogICAgICAgIFByaW9yaXRpemVbIuyasOyEoOyInOychCJdCiAgICAgICAgQ29sbGVjdCAtLT4gQ2xhc3NpZnkgLS0+IEFuYWx5emUgLS0+IFByaW9yaXRpemUKICAgIGVuZAogICAgCiAgICBzdWJncmFwaCBPdXRwdXRbIuqysOqzvOusvCJdCiAgICAgICAgUmVwb3J0WyLwn5OKIOyduOyCrOydtO2KuCDrpqztj6ztirgiXQogICAgICAgIEpJUkFbIvCfjqsgSklSQSDti7DsvJMiXQogICAgICAgIENvbmZsdWVuY2VbIvCfk50g6riw7ZqNIOusuOyEnCJdCiAgICBlbmQKICAgIAogICAgUmV2aWV3ICYgQ1MgJiBTbGFjayAtLT4gQ29sbGVjdAogICAgUHJpb3JpdGl6ZSAtLT4gUmVwb3J0ICYgSklSQSAmIENvbmZsdWVuY2UK)

## Kidsnote-MCP 예시

### 키즈노트 피드백 분석 워크플로우

```json
{
  "id": "kidsnote_feedback_analysis",
  "nodes": [
    {
      "id": "collect",
      "type": "fanout",
      "branches": [
        { "type": "tool", "name": "fetch_appstore_reviews" },
        { "type": "tool", "name": "fetch_cs_tickets" },
        { "type": "tool", "name": "fetch_slack_feedback" }
      ]
    },
    {
      "id": "classify",
      "type": "llm",
      "model": "gemini",
      "prompt": "다음 키즈노트 피드백을 카테고리별로 분류하세요:\n- 알림장 관련\n- 출결 관련\n- 급식/식단 관련\n- 앱 성능\n- 기타\n\n피드백:\n{{collect.output}}",
      "input_mapping": [["data", "collect"]]
    },
    {
      "id": "analyze",
      "type": "llm",
      "model": "claude",
      "prompt": "분류된 피드백에서 핵심 인사이트를 도출하세요. 선생님/학부모 관점을 구분하여 분석:\n{{classify.output}}",
      "input_mapping": [["classified", "classify"]]
    },
    {
      "id": "prioritize",
      "type": "llm",
      "model": "gemini",
      "prompt": "인사이트를 중요도와 긴급도 기준으로 우선순위를 매기세요:\n{{analyze.output}}",
      "input_mapping": [["insights", "analyze"]]
    }
  ],
  "output": "prioritize",
  "config": {
    "timeout": 300,
    "trace": true,
    "max_depth": 2
  }
}
```

### 동일한 워크플로우 - Mermaid 형식

```mermaid
graph LR
    %% @chain {"id":"kidsnote_feedback_analysis","output":"prioritize","timeout":300,"trace":true,"max_depth":2}
    %% @node:classify {"input_mapping":[["data","collect"]]}
    %% @node:analyze {"input_mapping":[["classified","classify"]]}
    %% @node:prioritize {"input_mapping":[["insights","analyze"]]}

    collect["Fanout: appstore_reviews, cs_tickets, slack_feedback"]
    classify["LLM:gemini '알림장/출결/급식 분류'"]
    analyze["LLM:claude '선생님/학부모 인사이트'"]
    prioritize["LLM:gemini '우선순위 결정'"]

    collect --> classify --> analyze --> prioritize
```

## n8n 연동 방법

### 1. n8n에서 Kidsnote-MCP 호출 (MCP Client Tool)

```
n8n Workflow:
┌──────────────┐    ┌─────────────────┐    ┌──────────────┐
│ Schedule     │ -> │ MCP Client Tool │ -> │ Slack Post   │
│ (매일 09:00) │    │ kidsnote.run    │    │ #feedback    │
└──────────────┘    └─────────────────┘    └──────────────┘
```

**MCP Client Tool 설정:**
- Endpoint: `http://kidsnote-mcp:8932/mcp`
- Method: `tools/call`
- Tool: `chain.run`
- Arguments: Kidsnote-MCP JSON

### 2. Kidsnote-MCP에서 n8n 호출 (Webhook)

```json
{
  "id": "kidsnote_with_n8n",
  "nodes": [
    { "id": "analyze", "type": "llm", "model": "gemini", "prompt": "알림장 분석: {{input}}" },
    {
      "id": "n8n_process",
      "type": "webhook",
      "url": "https://n8n.kidsnote.com/webhook/process",
      "method": "POST",
      "body": "{{analyze.output}}"
    },
    { "id": "summary", "type": "llm", "model": "claude",
      "prompt": "n8n 처리 결과 요약: {{n8n_process.output}}" }
  ],
  "output": "summary"
}
```

## 키즈노트 전사 도입 유즈케이스

### 기획 (PM)
| 유즈케이스 | Kidsnote-MCP 역할 | n8n 역할 |
|-----------|-------------------|---------|
| 학부모 피드백 클러스터링 | 분류/분석/우선순위 | 앱스토어 리뷰 수집, 결과 Slack 공유 |
| 알림장 기능 PRD 초안 | 문서 생성 | Confluence 자동 연동 |
| 출결 시스템 기술검토 | 분석/공수산정 | JIRA Epic 생성 |

### 개발 (Engineering)
| 유즈케이스 | Kidsnote-MCP 역할 | n8n 역할 |
|-----------|-------------------|---------|
| 키즈노트 서버 모니터링 | 로그 분석, 원인 추론 | Sentry 알림 트리거 |
| 원아 데이터 자연어 쿼리 | 쿼리 생성/실행 | 결과 전달 |
| 기술 부채 리스트업 | 코드 분석 | 주간 리포트 생성 |

### QA
| 유즈케이스 | Kidsnote-MCP 역할 | n8n 역할 |
|-----------|-------------------|---------|
| 알림장 테스트 케이스 작성 | 시나리오 생성 | 기획서 입력, TestRail 저장 |
| CS 이슈 등급 분류 | 분석/분류 | JIRA 티켓 자동 생성 |

### 디자인
| 유즈케이스 | Kidsnote-MCP 역할 | n8n 역할 |
|-----------|-------------------|---------|
| 키즈노트 앱 피그마 일관성 검수 | 분석/검증 | Figma API 연동 |
| 다국어 레이아웃 검증 (한/영/일) | 시뮬레이션 | 결과 리포트 |

### 운영 (Operations)
| 유즈케이스 | Kidsnote-MCP 역할 | n8n 역할 |
|-----------|-------------------|---------|
| 학부모 CS 자동 분류 | 카테고리 분류/긴급도 판단 | Zendesk 연동 |
| 선생님 문의 우선순위화 | 분석/우선순위 | Slack #cs-urgent 알림 |

## 핵심 가치

1. **JSON ↔ Mermaid 무손실 변환**: 개발자는 JSON, 비개발자는 Mermaid로 동일한 워크플로우 관리
2. **n8n 시각적 편집**: 드래그앤드롭으로 AI 워크플로우 구성 (비개발자 친화적)
3. **다중 LLM 지원**: Gemini, Claude, Codex 등 병렬 실행 및 합의 검증
4. **확장성**: 400+ n8n 통합과 결합 (Slack, JIRA, Confluence, Figma...)

## 다음 단계

1. [ ] Kidsnote-MCP SSE 엔드포인트 추가 (n8n MCP Client 연결용)
2. [ ] n8n Custom Node 개발 (Kidsnote-MCP 전용)
3. [ ] 파일럿 워크플로우 3개 구축
   - 학부모 피드백 분석
   - 알림장 CS 자동 분류
   - 주간 기술 부채 리포트
4. [ ] 전사 교육 및 롤아웃

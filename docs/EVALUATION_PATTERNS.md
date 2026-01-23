# Evaluation Patterns - Quantitative Assessment DSL

> Chain Engine 정량적 평가 패턴 Use Cases

기존 Quorum (N/K 투표) 패턴을 보완하는 숫자 기반 정량적 평가 패턴.

## Overview

| Pattern | Purpose | Key Metric | Use Case |
|---------|---------|------------|----------|
| **Threshold** | 단일 값 임계값 평가 | `value >= threshold` | Coverage, Latency, Confidence |
| **Evaluator** | 복수 출력 비교 평가 | `max(scores)` | A/B Test, Quality Gate |
| **GoalDriven** | 목표 달성까지 반복 | `metric >= goal` | Iterative Improvement |

---

## 1. Threshold (임계값 평가)

숫자 값이 특정 기준을 넘는지 평가하고, 결과에 따라 분기 처리.

### Node Type Definition

```ocaml
| Threshold of {
    metric : string;        (* Metric name or expression *)
    operator : [ `Gte | `Gt | `Lte | `Lt | `Eq ];
    value : float;          (* Threshold value *)
    then_node : node;       (* Execute if condition met *)
    else_node : node option;(* Execute if condition not met *)
  }
```

### Use Case 1.1: Code Coverage Gate

테스트 커버리지가 80% 이상이면 통과, 미만이면 리포트 생성.

```json
{
  "id": "coverage_gate",
  "nodes": [
    {
      "id": "run_tests",
      "type": "tool",
      "name": "pytest",
      "args": { "coverage": true, "output": "json" }
    },
    {
      "id": "coverage_check",
      "type": "threshold",
      "metric": "{{run_tests.output.coverage_percent}}",
      "operator": ">=",
      "value": 80.0,
      "then": {
        "id": "pass",
        "type": "llm",
        "model": "gemini",
        "prompt": "Coverage {{run_tests.output.coverage_percent}}% passed. Generate summary."
      },
      "else": {
        "id": "fail_report",
        "type": "llm",
        "model": "claude",
        "prompt": "Coverage {{run_tests.output.coverage_percent}}% < 80%. Suggest improvements for: {{run_tests.output.uncovered_files}}"
      }
    }
  ],
  "output": "coverage_check"
}
```

**Expected Behavior**:
- `coverage_percent >= 80.0` → `pass` 노드 실행 → 성공 요약 생성
- `coverage_percent < 80.0` → `fail_report` 노드 실행 → 개선 제안 생성

**Failure Handling**:
- 메트릭 추출 실패 시 → `else` 노드 실행 (안전 폴백)
- `else` 노드 없을 때 실패 시 → chain 실패 반환

---

### Use Case 1.2: Confidence Score Retry

LLM 응답의 confidence가 낮으면 재시도.

```json
{
  "id": "confident_answer",
  "nodes": [
    {
      "id": "initial_answer",
      "type": "llm",
      "model": "gemini",
      "prompt": "Answer with confidence score (0-1): {{input}}"
    },
    {
      "id": "confidence_gate",
      "type": "threshold",
      "metric": "{{initial_answer.output.confidence}}",
      "operator": "<",
      "value": 0.5,
      "then": {
        "id": "retry_with_opus",
        "type": "llm",
        "model": "claude",
        "prompt": "The previous answer had low confidence. Provide a more thorough answer: {{input}}"
      }
    },
    {
      "id": "final",
      "type": "gate",
      "condition": "retry_with_opus.executed",
      "then": { "id": "use_retry", "type": "map", "func": "identity", "inner": { "id": "_", "type": "chain_ref", "ref": "retry_with_opus" } },
      "else": { "id": "use_initial", "type": "map", "func": "identity", "inner": { "id": "_", "type": "chain_ref", "ref": "initial_answer" } }
    }
  ],
  "output": "final"
}
```

**Expected Behavior**:
- `confidence >= 0.5` → 초기 응답 사용
- `confidence < 0.5` → claude로 재시도 후 해당 응답 사용

**Failure Handling**:
- confidence 파싱 실패 → 기본값 0.0 적용 (재시도 트리거)
- 재시도도 실패 → chain 에러 반환

---

### Use Case 1.3: Latency Fallback

응답 지연이 3초 초과 시 더 빠른 모델로 폴백.

```json
{
  "id": "latency_aware_query",
  "nodes": [
    {
      "id": "primary",
      "type": "llm",
      "model": "claude",
      "prompt": "{{input}}",
      "timeout": 5
    },
    {
      "id": "latency_check",
      "type": "threshold",
      "metric": "{{primary.duration_ms}}",
      "operator": ">",
      "value": 3000,
      "then": {
        "id": "fallback_fast",
        "type": "llm",
        "model": "gemini",
        "prompt": "Quick response needed: {{input}}"
      }
    }
  ],
  "output": "latency_check",
  "config": { "trace": true }
}
```

**Expected Behavior**:
- `duration_ms <= 3000` → primary 응답 사용
- `duration_ms > 3000` → gemini 폴백 실행 후 해당 응답 사용

**Failure Handling**:
- timeout 발생 시 → `duration_ms = timeout * 1000` 간주 → 폴백 트리거

---

### Use Case 1.4: Visual Similarity (SSIM)

Figma-to-Code 변환 결과의 시각적 유사도가 95% 이상이면 통과.

```json
{
  "id": "figma_verify",
  "nodes": [
    {
      "id": "render_html",
      "type": "tool",
      "name": "playwright_screenshot",
      "args": { "url": "{{html_preview_url}}" }
    },
    {
      "id": "compare",
      "type": "tool",
      "name": "figma_image_similarity",
      "args": {
        "image_a": "{{figma_render_path}}",
        "image_b": "{{render_html.screenshot_path}}"
      }
    },
    {
      "id": "ssim_gate",
      "type": "threshold",
      "metric": "{{compare.ssim}}",
      "operator": ">=",
      "value": 0.95,
      "then": {
        "id": "approve",
        "type": "llm",
        "model": "gemini",
        "prompt": "SSIM {{compare.ssim}} >= 0.95. Approve with summary."
      },
      "else": {
        "id": "refine",
        "type": "llm",
        "model": "codex",
        "prompt": "SSIM {{compare.ssim}} < 0.95. Analyze diff regions: {{compare.diff_regions}} and suggest CSS fixes."
      }
    }
  ],
  "output": "ssim_gate"
}
```

**Expected Behavior**:
- `ssim >= 0.95` → 승인 및 요약 생성
- `ssim < 0.95` → 차이 분석 및 CSS 수정 제안

**Failure Handling**:
- 이미지 비교 실패 → `ssim = 0.0` 간주 → `else` 노드 실행

---

## 2. Evaluator (출력 비교 평가)

여러 LLM 출력을 점수화하여 최선의 결과 선택.

### Node Type Definition

```ocaml
| Evaluator of {
    nodes : node list;           (* Candidates to evaluate *)
    scorer : scorer_config;      (* How to score each output *)
    selector : [ `Max | `Min | `Threshold of float ];
    reject_below : float option; (* Quality gate *)
  }

and scorer_config =
  | LlmScorer of {
      model : string;
      prompt : string;  (* Must output numeric score *)
    }
  | MetricScorer of string  (* Built-in metric function *)
  | CustomScorer of string  (* Custom scoring function name *)
```

### Use Case 2.1: Best of N Selection

3개 LLM 응답 중 가장 높은 품질 점수의 응답 선택.

```json
{
  "id": "best_of_three",
  "nodes": [
    {
      "id": "evaluate",
      "type": "evaluator",
      "nodes": [
        { "id": "gemini_answer", "type": "llm", "model": "gemini", "prompt": "{{input}}" },
        { "id": "claude_answer", "type": "llm", "model": "claude", "prompt": "{{input}}" },
        { "id": "codex_answer", "type": "llm", "model": "codex", "prompt": "{{input}}" }
      ],
      "scorer": {
        "type": "llm",
        "model": "claude",
        "prompt": "Rate the following answer on a scale of 0-100 for accuracy, completeness, and clarity. Output ONLY the numeric score.\n\nQuestion: {{input}}\nAnswer: {{candidate.output}}\n\nScore:"
      },
      "selector": "max"
    }
  ],
  "output": "evaluate"
}
```

**Expected Behavior**:
1. 3개 LLM 병렬 실행
2. 각 응답에 대해 scorer LLM 실행 (점수 추출)
3. 최고 점수 응답 반환
4. `result.metadata`에 모든 점수 기록: `{"scores": {"gemini": 85, "claude": 92, "codex": 78}}`

**Failure Handling**:
- 점수 파싱 실패 → 해당 후보 제외 (score = -1)
- 모든 후보 실패 → chain 에러 반환
- 동점 시 → 첫 번째 후보 선택

---

### Use Case 2.2: A/B Prompt Testing

두 가지 프롬프트 전략 비교 평가.

```json
{
  "id": "ab_prompt_test",
  "nodes": [
    {
      "id": "ab_eval",
      "type": "evaluator",
      "nodes": [
        {
          "id": "prompt_a",
          "type": "llm",
          "model": "gemini",
          "prompt": "You are a helpful assistant. Answer: {{input}}"
        },
        {
          "id": "prompt_b",
          "type": "llm",
          "model": "gemini",
          "prompt": "You are an expert analyst. Provide detailed analysis: {{input}}"
        }
      ],
      "scorer": {
        "type": "llm",
        "model": "claude",
        "prompt": "Evaluate this response for user satisfaction (0-100). Consider helpfulness, clarity, and completeness.\n\nUser question: {{input}}\nResponse: {{candidate.output}}\n\nSatisfaction score:"
      },
      "selector": "max"
    }
  ],
  "output": "ab_eval",
  "config": { "trace": true }
}
```

**Expected Behavior**:
- 동일 모델, 다른 프롬프트로 응답 생성
- 각 응답 점수 평가 후 더 높은 점수 선택
- trace에 A/B 결과 기록 (프롬프트 최적화 데이터)

**Failure Handling**:
- 한쪽 프롬프트 실패 → 다른쪽 자동 선택
- 둘 다 실패 → chain 에러 반환

---

### Use Case 2.3: Quality Gate with Rejection

점수가 기준 미달이면 전체 거부.

```json
{
  "id": "quality_gate_review",
  "nodes": [
    {
      "id": "code_review",
      "type": "evaluator",
      "nodes": [
        {
          "id": "review",
          "type": "llm",
          "model": "claude",
          "prompt": "Review this code for bugs, security issues, and best practices:\n{{code}}"
        }
      ],
      "scorer": {
        "type": "llm",
        "model": "gemini",
        "prompt": "Rate this code review quality (0-100). Consider: thoroughness, actionability, accuracy.\n\nReview:\n{{candidate.output}}\n\nQuality score:"
      },
      "selector": "max",
      "reject_below": 70.0
    },
    {
      "id": "fallback",
      "type": "gate",
      "condition": "{{code_review.rejected}}",
      "then": {
        "id": "human_escalation",
        "type": "llm",
        "model": "claude",
        "prompt": "The automated review scored below quality threshold. Please flag for human review: {{code}}"
      }
    }
  ],
  "output": "fallback"
}
```

**Expected Behavior**:
- `score >= 70.0` → 리뷰 결과 반환
- `score < 70.0` → `rejected: true` 설정 → human escalation 트리거

**Failure Handling**:
- reject 시 `else` 없으면 → `{rejected: true, reason: "Score X below threshold 70"}` 반환
- 후속 노드에서 `{{prev.rejected}}` 조건 사용 가능

---

### Use Case 2.4: Multi-Metric Scoring

여러 평가 기준의 가중 평균.

```json
{
  "id": "multi_metric_eval",
  "nodes": [
    {
      "id": "generate",
      "type": "llm",
      "model": "claude",
      "prompt": "Write a technical blog post about: {{topic}}"
    },
    {
      "id": "multi_score",
      "type": "evaluator",
      "nodes": [
        { "id": "content", "type": "chain_ref", "ref": "generate" }
      ],
      "scorer": {
        "type": "composite",
        "metrics": [
          {
            "name": "accuracy",
            "weight": 0.4,
            "scorer": {
              "type": "llm",
              "model": "gemini",
              "prompt": "Rate technical accuracy (0-100): {{candidate.output}}"
            }
          },
          {
            "name": "readability",
            "weight": 0.3,
            "scorer": {
              "type": "metric",
              "name": "flesch_kincaid_score"
            }
          },
          {
            "name": "engagement",
            "weight": 0.3,
            "scorer": {
              "type": "llm",
              "model": "claude",
              "prompt": "Rate engagement level (0-100): {{candidate.output}}"
            }
          }
        ]
      },
      "selector": { "type": "threshold", "value": 75.0 },
      "reject_below": 60.0
    }
  ],
  "output": "multi_score"
}
```

**Expected Behavior**:
- 3가지 메트릭 각각 평가 (병렬)
- 가중 평균 계산: `0.4*accuracy + 0.3*readability + 0.3*engagement`
- `score >= 75.0` → pass, `60.0 <= score < 75.0` → pass with warning, `score < 60.0` → reject

**Failure Handling**:
- 개별 메트릭 실패 → 해당 가중치 제외하고 나머지로 계산
- 모든 메트릭 실패 → chain 에러

---

## 3. GoalDriven (목표 달성 루프)

목표 메트릭에 도달할 때까지 반복 실행.

### Node Type Definition

```ocaml
| GoalDriven of {
    goal_metric : string;        (* Target metric expression *)
    goal_value : float;          (* Target value to achieve *)
    goal_operator : [ `Gte | `Gt | `Lte | `Lt ];
    max_iterations : int;        (* Safety limit *)
    action_node : node;          (* Node to execute each iteration *)
    measure_node : node;         (* Node to measure progress *)
    on_stall : node option;      (* Execute if no progress after N iterations *)
    stall_threshold : int;       (* Iterations without progress before stall *)
  }
```

### Use Case 3.1: Test Coverage Improvement

테스트 커버리지 90% 달성까지 테스트 생성 반복.

```json
{
  "id": "coverage_goal",
  "nodes": [
    {
      "id": "achieve_coverage",
      "type": "goal_driven",
      "goal_metric": "coverage_percent",
      "goal_value": 90.0,
      "goal_operator": ">=",
      "max_iterations": 10,
      "action_node": {
        "id": "generate_tests",
        "type": "pipeline",
        "nodes": [
          {
            "id": "analyze",
            "type": "llm",
            "model": "claude",
            "prompt": "Analyze uncovered code and generate test cases:\n\nUncovered files: {{measure.uncovered_files}}\nCurrent coverage: {{measure.coverage_percent}}%\n\nGenerate pytest test code:"
          },
          {
            "id": "write_tests",
            "type": "tool",
            "name": "file_write",
            "args": {
              "path": "tests/generated_test_{{iteration}}.py",
              "content": "{{analyze.output}}"
            }
          }
        ]
      },
      "measure_node": {
        "id": "measure",
        "type": "tool",
        "name": "pytest",
        "args": { "coverage": true, "output": "json" }
      },
      "on_stall": {
        "id": "human_help",
        "type": "llm",
        "model": "claude",
        "prompt": "Coverage stuck at {{measure.coverage_percent}}% after {{stall_count}} attempts. Analyze blockers and suggest manual intervention."
      },
      "stall_threshold": 3
    }
  ],
  "output": "achieve_coverage"
}
```

**Expected Behavior**:
1. 초기 coverage 측정
2. `coverage < 90%` 동안 반복:
   - `action_node` 실행 (테스트 생성)
   - `measure_node` 실행 (coverage 재측정)
   - 진행 상황 추적
3. 3회 연속 progress 없으면 → `on_stall` 실행
4. `coverage >= 90%` 또는 `max_iterations` 도달 시 종료

**Result Structure**:
```json
{
  "output": "Coverage achieved: 91.2%",
  "iterations": 4,
  "progress_history": [72.0, 78.5, 84.2, 91.2],
  "goal_achieved": true
}
```

**Failure Handling**:
- `max_iterations` 도달 → `{goal_achieved: false, final_value: X, iterations: max}` 반환
- action 실패 → 해당 iteration skip, 다음 iteration 진행
- measure 실패 → chain 에러 (메트릭 없이 진행 불가)

---

### Use Case 3.2: Translation Quality (BLEU)

BLEU 점수 0.8 이상 달성까지 번역 개선 반복.

```json
{
  "id": "translation_quality",
  "nodes": [
    {
      "id": "improve_translation",
      "type": "goal_driven",
      "goal_metric": "bleu_score",
      "goal_value": 0.8,
      "goal_operator": ">=",
      "max_iterations": 5,
      "action_node": {
        "id": "refine",
        "type": "llm",
        "model": "claude",
        "prompt": "Improve this translation to better match the reference style and meaning.\n\nOriginal: {{source_text}}\nCurrent translation: {{current_translation}}\nReference style: {{reference_sample}}\nPrevious BLEU: {{measure.bleu_score}}\n\nImproved translation:"
      },
      "measure_node": {
        "id": "measure",
        "type": "tool",
        "name": "bleu_score",
        "args": {
          "candidate": "{{refine.output}}",
          "reference": "{{reference_translation}}"
        }
      },
      "stall_threshold": 2
    }
  ],
  "output": "improve_translation"
}
```

**Expected Behavior**:
1. 초기 번역 → BLEU 측정
2. `bleu < 0.8` 동안 개선 반복
3. 이전 BLEU 점수를 컨텍스트로 제공하여 점진적 개선 유도
4. 목표 달성 또는 max iterations 도달 시 최선의 번역 반환

**Failure Handling**:
- BLEU 하락 시 → 이전 최고점 번역 유지
- stall → 현재 최선 결과와 함께 경고 반환

---

### Use Case 3.3: Build Success Loop

빌드 성공할 때까지 코드 수정 반복.

```json
{
  "id": "fix_build",
  "nodes": [
    {
      "id": "build_loop",
      "type": "goal_driven",
      "goal_metric": "build_success",
      "goal_value": 1.0,
      "goal_operator": ">=",
      "max_iterations": 5,
      "action_node": {
        "id": "fix_errors",
        "type": "pipeline",
        "nodes": [
          {
            "id": "analyze_errors",
            "type": "llm",
            "model": "codex",
            "prompt": "Analyze build errors and generate fixes:\n\nBuild output:\n{{measure.build_output}}\n\nError count: {{measure.error_count}}\n\nGenerate code patches:"
          },
          {
            "id": "apply_patches",
            "type": "tool",
            "name": "git_apply",
            "args": { "patch": "{{analyze_errors.output}}" }
          }
        ]
      },
      "measure_node": {
        "id": "measure",
        "type": "pipeline",
        "nodes": [
          {
            "id": "build",
            "type": "tool",
            "name": "dune_build",
            "args": { "capture_output": true }
          },
          {
            "id": "parse",
            "type": "map",
            "func": "parse_build_result",
            "inner": { "id": "_", "type": "chain_ref", "ref": "build" }
          }
        ]
      },
      "on_stall": {
        "id": "rollback",
        "type": "pipeline",
        "nodes": [
          {
            "id": "revert",
            "type": "tool",
            "name": "git_reset",
            "args": { "mode": "hard", "ref": "HEAD~{{iteration}}" }
          },
          {
            "id": "report",
            "type": "llm",
            "model": "claude",
            "prompt": "Build fix failed after {{iteration}} attempts. Changes reverted. Summary of attempted fixes: {{progress_history}}"
          }
        ]
      },
      "stall_threshold": 2
    }
  ],
  "output": "build_loop"
}
```

**Expected Behavior**:
1. 빌드 실행 → 결과 파싱 (`build_success: 0` or `1`)
2. 실패 시 에러 분석 → 패치 생성 → 적용
3. 재빌드 후 측정
4. 성공 또는 max iterations 도달 시 종료
5. stall 시 변경사항 롤백

**Failure Handling**:
- patch 적용 실패 → 해당 iteration skip
- stall 발생 → git reset으로 안전하게 복구
- 최종 실패 → 원본 상태로 복구 + 실패 보고서

---

### Use Case 3.4: Response Quality Iteration

응답 품질 점수 85점 달성까지 자기 개선 반복.

```json
{
  "id": "self_improve",
  "nodes": [
    {
      "id": "quality_loop",
      "type": "goal_driven",
      "goal_metric": "quality_score",
      "goal_value": 85.0,
      "goal_operator": ">=",
      "max_iterations": 3,
      "action_node": {
        "id": "improve",
        "type": "llm",
        "model": "claude",
        "prompt": "Improve this response based on the feedback.\n\nOriginal question: {{input}}\nCurrent response: {{current_response}}\nQuality score: {{measure.quality_score}}/100\nFeedback: {{measure.feedback}}\n\nImproved response:"
      },
      "measure_node": {
        "id": "measure",
        "type": "llm",
        "model": "gemini",
        "prompt": "Evaluate this response. Output JSON with quality_score (0-100) and feedback.\n\nQuestion: {{input}}\nResponse: {{improve.output}}\n\n{\"quality_score\": <number>, \"feedback\": \"<text>\"}"
      },
      "stall_threshold": 2
    }
  ],
  "output": "quality_loop"
}
```

**Expected Behavior**:
- 응답 생성 → 품질 평가 → 피드백 기반 개선 → 재평가 반복
- 자기 개선 루프 (Evaluator-Optimizer 패턴)
- 품질 목표 달성 시 최종 응답 반환

**Failure Handling**:
- 품질 하락 시 → 이전 최고 응답 유지
- JSON 파싱 실패 → `quality_score: 0` 간주

---

## Comparison with Existing Patterns

| Pattern | Quorum | Threshold | Evaluator | GoalDriven |
|---------|--------|-----------|-----------|------------|
| **Input** | N responses | 1 metric | N responses | Metric series |
| **Evaluation** | Boolean (pass/fail) | Numeric comparison | Scoring | Metric vs goal |
| **Output** | Consensus | Branch | Best candidate | Final state |
| **Loop** | No | No | No | Yes |
| **Use Case** | Voting | Gating | Selection | Iteration |

## Implementation Priority

1. **Threshold** (Low complexity)
   - Extends existing Gate pattern
   - Numeric comparison vs boolean
   - ~200 LOC estimated

2. **Evaluator** (Medium complexity)
   - New scorer abstraction needed
   - Parallel scoring + aggregation
   - ~500 LOC estimated

3. **GoalDriven** (High complexity)
   - Loop primitive (new for DSL)
   - State management across iterations
   - Stall detection + recovery
   - ~800 LOC estimated

## Related Modules

- `chain_types.ml` - Type definitions
- `chain_executor_eio.ml` - Eio-based execution
- `validation_stack.ml` - Existing validator patterns
- `chain_metrics.ml` - (New) Metric extraction utilities

---

*llm-mcp Chain Engine v2.2.0 - Evaluation Patterns RFC*

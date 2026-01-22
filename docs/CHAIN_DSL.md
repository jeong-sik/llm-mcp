# Chain Engine - Orchestration DSL

A composable DSL for orchestrating multi-LLM workflows in llm-mcp.

## Philosophy: Composition & Decomposition

The Chain Engine applies category theory principles to create mathematically sound compositions:

```
┌─────────────────────────────────────────────────────────────┐
│                 Composition & Decomposition                  │
├─────────────────────────────────────────────────────────────┤
│  Decomposition              Composition                      │
│  ─────────────────────     ─────────────────────            │
│  Complex workflows          Small nodes                      │
│         │                        │                           │
│         ▼ Functor.map            ▼ Monad.bind               │
│         ▼ Profunctor.dimap       ▼ Kleisli.(>=>)            │
│         │                        │                           │
│         ▼                        ▼                           │
│  Small nodes        ←────────→    Complex pipelines          │
│                   Monoid.concat                              │
└─────────────────────────────────────────────────────────────┘
```

**Core Laws**:
- **Identity**: `id >> f = f = f >> id`
- **Associativity**: `(a >> b) >> c = a >> (b >> c)`
- **Functor**: `map id = id`, `map (f . g) = map f . map g`

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      MCP Layer                               │
│  chain.run, chain.validate, chain.register                  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Chain Engine                               │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │ DSL Parser  │  │  Compiler   │  │  Executor   │         │
│  │ JSON→AST    │  │ AST→Plan    │  │ Eio Fibers  │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│              Validator Layer (Existing)                      │
│  Compose (sequence, parallel, quorum, bind)                 │
│  Meta-Validator, Presets (Pipeline, Fanout, Diamond...)     │
└─────────────────────────────────────────────────────────────┘
```

## Node Types

| Type | Description | Internal Mapping |
|------|-------------|------------------|
| `llm` | LLM call | gemini/claude/codex/ollama tool |
| `tool` | MCP tool call | tools_eio.execute |
| `pipeline` | Sequential execution | Validator_presets.Pipeline |
| `fanout` | Parallel execution | Validator_presets.Fanout |
| `quorum` | N/K consensus | Validator_presets.Quorum |
| `gate` | Conditional execution | Validator_presets.Gate |
| `subgraph` | Inline sub-chain | Recursive execution |
| `chain_ref` | Reference to registered chain | Registry lookup → recursive |
| `map` | Output transformation | Functor.map |
| `bind` | Dynamic routing | Monad.bind |
| `merge` | Combine parallel results | Monoid.concat |
| `threshold` | Quality gate | Conditional execution |
| `goal_driven` | Iterative improvement | Loop until goal |
| `evaluator` | Score and select best | Multi-candidate selection |
| `retry` | Retry on failure | Backoff strategies |
| `fallback` | Try alternatives | Primary → fallbacks |
| `race` | Parallel race | First result wins |
| `adapter` | Data transformation | Inter-node refinement |
| `cache` | Result caching | TTL-based memoization |
| `batch` | List batch processing | Parallel/sequential batches |

## DSL Syntax

### Basic Structure

```json
{
  "id": "chain_id",
  "nodes": [
    { "id": "node_id", "type": "...", ... }
  ],
  "output": "final_node_id",
  "config": {
    "max_depth": 4,
    "max_concurrency": 3,
    "timeout": 300,
    "trace": true
  }
}
```

### LLM Node

```json
{
  "id": "summarize",
  "type": "llm",
  "model": "gemini",
  "system": "You are a professional summarizer. Be concise and accurate.",
  "prompt": "Summarize: {{input}}",
  "timeout": 30
}
```

**Fields:**
- `model`: Model identifier (gemini, claude, codex, ollama:*)
- `system` (optional): System instruction for role/persona definition
- `prompt`: User prompt with `{{var}}` placeholders
- `timeout` (optional): Timeout in seconds

### Tool Node

```json
{
  "id": "lint",
  "type": "tool",
  "name": "eslint",
  "args": { "fix": true }
}
```

### Pipeline Node (Sequential)

```json
{
  "id": "pipeline",
  "type": "pipeline",
  "nodes": [
    { "id": "step1", "type": "llm", "model": "gemini", "prompt": "..." },
    { "id": "step2", "type": "llm", "model": "claude", "prompt": "{{step1.output}}" }
  ]
}
```

### Fanout Node (Parallel)

```json
{
  "id": "parallel_review",
  "type": "fanout",
  "branches": [
    { "id": "security", "type": "llm", "model": "gemini", "prompt": "Security: {{input}}" },
    { "id": "quality", "type": "llm", "model": "claude", "prompt": "Quality: {{input}}" }
  ]
}
```

### Quorum Node (Consensus)

```json
{
  "id": "consensus",
  "type": "quorum",
  "required": 2,
  "nodes": [
    { "id": "casper", "type": "llm", "model": "gemini", "prompt": "..." },
    { "id": "balthasar", "type": "llm", "model": "claude", "prompt": "..." },
    { "id": "melchior", "type": "llm", "model": "codex", "prompt": "..." }
  ]
}
```

### Gate Node (Conditional)

```json
{
  "id": "conditional",
  "type": "gate",
  "condition": "is_complex",
  "then": { "id": "deep", "type": "llm", "model": "opus", "prompt": "..." },
  "else": { "id": "fast", "type": "llm", "model": "haiku", "prompt": "..." }
}
```

### Subgraph Node (Nested Chain)

```json
{
  "id": "nested",
  "type": "subgraph",
  "graph": {
    "id": "inner",
    "nodes": [
      { "id": "inner_step", "type": "llm", "model": "gemini", "prompt": "..." }
    ],
    "output": "inner_step"
  }
}
```

### Chain Reference

```json
{
  "id": "reuse",
  "type": "chain_ref",
  "ref": "previously_registered_chain_id"
}
```

### Input Mapping

Explicit input mapping allows precise control over data flow:

```json
{
  "id": "consumer",
  "type": "llm",
  "model": "claude",
  "prompt": "Process: {{data}}",
  "input_mapping": [["data", "{{producer.output}}"]]
}
```

Without explicit mapping, references like `{{producer.output}}` are automatically extracted from the prompt.

## Merge Strategies

For combining results from parallel nodes:

| Strategy | Description |
|----------|-------------|
| `first` | Use first result |
| `last` | Use last result |
| `concat` | Concatenate all results |
| `weighted_average` | Weighted combination |
| `custom:<func>` | Custom merge function |

```json
{
  "id": "merge",
  "type": "merge",
  "strategy": "weighted_average",
  "nodes": [...]
}
```

### Resilience Nodes

The Chain Engine provides three resilience patterns for building fault-tolerant workflows:

#### Retry Node

Retry a node on failure with configurable backoff strategies:

```json
{
  "id": "retrier",
  "type": "retry",
  "max_attempts": 3,
  "backoff": "exponential:2.0",
  "retry_on": ["timeout", "rate_limit"],
  "node": {
    "id": "api_call",
    "type": "llm",
    "model": "gemini",
    "prompt": "Process: {{input}}"
  }
}
```

**Backoff Strategies:**
| Strategy | Format | Description |
|----------|--------|-------------|
| Exponential | `exponential:2.0` | Delay = base × 2^attempt |
| Constant | `constant:1.5` | Fixed 1.5s delay |
| Linear | `linear:0.5` | Delay = base × attempt |

#### Fallback Node

Try primary node first, then fallbacks in order on failure:

```json
{
  "id": "resilient_api",
  "type": "fallback",
  "primary": {
    "id": "fast_llm",
    "type": "llm",
    "model": "gemini",
    "prompt": "Quick answer: {{input}}"
  },
  "fallbacks": [
    {
      "id": "accurate_llm",
      "type": "llm",
      "model": "claude",
      "prompt": "Detailed answer: {{input}}"
    },
    {
      "id": "local_llm",
      "type": "llm",
      "model": "ollama:qwen",
      "prompt": "Fallback answer: {{input}}"
    }
  ]
}
```

#### Race Node

Execute nodes in parallel, return first successful result:

```json
{
  "id": "fastest_response",
  "type": "race",
  "timeout": 5.0,
  "nodes": [
    {
      "id": "gemini_fast",
      "type": "llm",
      "model": "gemini",
      "prompt": "Answer quickly: {{input}}"
    },
    {
      "id": "claude_fast",
      "type": "llm",
      "model": "claude",
      "prompt": "Answer quickly: {{input}}"
    }
  ]
}
```

### Adapter Node (Data Transformation)

Transform data between nodes with various strategies:

```json
{
  "id": "extract_result",
  "type": "adapter",
  "input_ref": "{{api_response.output}}",
  "transform": {
    "type": "extract",
    "path": "data.result.content"
  },
  "on_error": "passthrough"
}
```

**Transform Types:**

| Type | Description | Example |
|------|-------------|---------|
| `extract` | Extract JSON field | `{"type": "extract", "path": "data.items[0]"}` |
| `template` | Apply template | `{"type": "template", "template": "Result: {{value}}"}` |
| `summarize` | Summarize with token limit | `{"type": "summarize", "max_tokens": 100}` |
| `truncate` | Truncate characters | `{"type": "truncate", "max_chars": 500}` |
| `jsonpath` | JSONPath query | `{"type": "jsonpath", "path": "$.data"}` |
| `regex` | Regex replace | `{"type": "regex", "pattern": "\\s+", "replacement": " "}` |
| `validate_schema` | Schema validation | `{"type": "validate_schema", "schema": "response"}` |
| `parse_json` | Parse string as JSON | `"parse_json"` |
| `stringify` | Convert to string | `"stringify"` |
| `chain` | Chain transforms | `{"type": "chain", "transforms": [...]}` |
| `conditional` | Conditional transform | `{"type": "conditional", "condition": "...", "on_true": ..., "on_false": ...}` |
| `custom` | Custom function | `{"type": "custom", "func": "myTransform"}` |

**Error Handling:**
- `fail`: Stop pipeline on error (default)
- `passthrough`: Pass original input on error
- `{"default": "fallback value"}`: Use default value on error

**Example: Pipeline with Adapters**

```json
{
  "id": "refined_pipeline",
  "type": "pipeline",
  "nodes": [
    {
      "id": "analyze",
      "type": "llm",
      "model": "gemini",
      "system": "You are a JSON analyzer. Always return valid JSON.",
      "prompt": "Analyze this text and return JSON: {{input}}"
    },
    {
      "id": "parse_output",
      "type": "adapter",
      "input_ref": "{{analyze.output}}",
      "transform": "parse_json",
      "on_error": {"default": "{}"}
    },
    {
      "id": "extract_summary",
      "type": "adapter",
      "input_ref": "{{parse_output.output}}",
      "transform": {"type": "extract", "path": "summary"},
      "on_error": "passthrough"
    },
    {
      "id": "format_result",
      "type": "adapter",
      "input_ref": "{{extract_summary.output}}",
      "transform": {"type": "template", "template": "## Summary\n\n{{value}}"}
    }
  ]
}
```

### Cache Node (Result Caching)

Cache expensive operations to avoid re-execution:

```json
{
  "id": "cached_analysis",
  "type": "cache",
  "key_expr": "{{input}}",
  "ttl_seconds": 3600,
  "inner": {
    "id": "expensive_llm",
    "type": "llm",
    "model": "claude",
    "prompt": "Perform deep analysis of: {{input}}"
  }
}
```

**Parameters:**
- `key_expr`: Cache key expression (e.g., `"{{input}}"`, `"static-key"`)
- `ttl_seconds`: Time-to-live in seconds (0 = infinite cache)
- `inner`: The node whose results will be cached

**Use Cases:**
- Caching LLM responses for repeated queries
- Avoiding redundant API calls in loops
- Memoizing expensive computations

**Mermaid Syntax:**
```mermaid
[[Cache:{{input}},3600,expensive_llm]]
```

### Batch Node (List Processing)

Process list items in configurable batches:

```json
{
  "id": "batch_processor",
  "type": "batch",
  "batch_size": 5,
  "parallel": true,
  "collect_strategy": "list",
  "inner": {
    "id": "process_item",
    "type": "llm",
    "model": "gemini",
    "prompt": "Process this item: {{batch_processor_item}}"
  }
}
```

**Parameters:**
- `batch_size`: Number of items per batch
- `parallel`: Process items within batch in parallel (`true`/`false`)
- `collect_strategy`: How to collect results
  - `list`: Return as JSON array
  - `concat`: Concatenate results with newlines
  - `first`: Return first successful result
  - `last`: Return last successful result
- `inner`: Node to apply to each item

**Input Format:**
- JSON array: `["item1", "item2", "item3"]`
- Newline-separated text: `"item1\nitem2\nitem3"`

**Item Access:**
Inside the inner node, access the current item via `{{node_id_item}}`:
```
{{batch_processor_item}}  // Current item being processed
```

**Example: Batch Translation**
```json
{
  "id": "translate_batch",
  "type": "batch",
  "batch_size": 10,
  "parallel": true,
  "collect_strategy": "list",
  "inner": {
    "id": "translate",
    "type": "llm",
    "model": "gemini",
    "prompt": "Translate to Korean: {{translate_batch_item}}"
  }
}
```

**Mermaid Syntax:**
```mermaid
[[Batch:5,true,process_item]]
```

### Resilience Patterns in Practice

**Example: Robust API Integration**

```json
{
  "id": "robust_llm_chain",
  "nodes": [
    {
      "id": "with_retry_and_fallback",
      "type": "retry",
      "max_attempts": 2,
      "backoff": "exponential:1.0",
      "retry_on": ["rate_limit"],
      "node": {
        "id": "fallback_chain",
        "type": "fallback",
        "primary": {
          "id": "fast",
          "type": "llm",
          "model": "gemini",
          "prompt": "{{input}}"
        },
        "fallbacks": [
          {
            "id": "backup",
            "type": "llm",
            "model": "claude",
            "prompt": "{{input}}"
          }
        ]
      }
    }
  ],
  "output": "with_retry_and_fallback"
}
```

## Configuration

| Option | Default | Description |
|--------|---------|-------------|
| `max_depth` | 4 | Maximum recursion depth |
| `max_concurrency` | 3 | Parallel execution limit |
| `timeout` | 300 | Default timeout (seconds) |
| `trace` | false | Enable execution tracing |

## Registry API

The registry stores reusable chains for `chain_ref` references:

```ocaml
(* Register a chain *)
Chain_registry.register ~description:"My chain" chain

(* Look up by ID *)
let chain = Chain_registry.lookup "my_chain_v1"

(* List all registered chains *)
let all = Chain_registry.list_all ()

(* Statistics *)
let stats = Chain_registry.stats ()
```

### Registry Features

- In-memory storage (Hashtbl) for fast lookup
- Optional file-based persistence
- Thread-safe via Mutex
- Version tracking on updates

## Example: PR Review Pipeline

```json
{
  "id": "pr_review_v1",
  "nodes": [
    { "id": "lint", "type": "tool", "name": "eslint", "timeout": 30 },
    { "id": "type_check", "type": "tool", "name": "tsc", "timeout": 60 },
    {
      "id": "parallel_review",
      "type": "fanout",
      "branches": [
        { "id": "security", "type": "llm", "model": "gemini",
          "prompt": "Security vulnerability analysis: {{input}}" },
        { "id": "quality", "type": "llm", "model": "claude",
          "prompt": "Code quality review: {{input}}" }
      ]
    },
    {
      "id": "verdict",
      "type": "quorum",
      "required": 2,
      "nodes": [
        { "id": "s_check", "type": "chain_ref", "ref": "security_summary" },
        { "id": "q_check", "type": "chain_ref", "ref": "quality_summary" }
      ]
    }
  ],
  "output": "verdict",
  "config": { "max_depth": 4, "timeout": 600, "trace": true }
}
```

## Example: MAGI Trinity Consensus

```json
{
  "id": "magi_consensus",
  "nodes": [
    { "id": "casper", "type": "llm", "model": "gemini",
      "prompt": "Strategic perspective: {{input}}" },
    { "id": "balthasar", "type": "llm", "model": "claude",
      "prompt": "Value-based judgment: {{input}}" },
    { "id": "melchior", "type": "llm", "model": "codex",
      "prompt": "Technical analysis: {{input}}" },
    {
      "id": "consensus",
      "type": "quorum",
      "required": 2,
      "nodes": [
        { "id": "c_ref", "type": "chain_ref", "ref": "casper" },
        { "id": "b_ref", "type": "chain_ref", "ref": "balthasar" },
        { "id": "m_ref", "type": "chain_ref", "ref": "melchior" }
      ]
    }
  ],
  "output": "consensus"
}
```

## Example: PO Feature Spec Pipeline (Designer + Dev Context)

Product Owner가 디자이너 에셋과 개발 정보를 병렬 수집 후, 통합 분석하여 기능 명세를 생성하는 파이프라인:

```json
{
  "id": "po_feature_spec_pipeline",
  "nodes": [
    {
      "id": "gather_context",
      "type": "fanout",
      "nodes": [
        {
          "id": "designer_assets",
          "type": "pipeline",
          "nodes": [
            {
              "id": "fetch_figma",
              "type": "tool",
              "name": "figma_get_node_summary",
              "args": { "file_key": "{{figma_file_key}}", "node_id": "{{figma_node_id}}" }
            },
            {
              "id": "analyze_design",
              "type": "llm",
              "model": "gemini",
              "prompt": "Analyze this Figma design and extract:\n1. UI components used\n2. User flow\n3. Interaction patterns\n4. Accessibility considerations\n\nDesign DSL:\n{{fetch_figma.output}}"
            }
          ]
        },
        {
          "id": "dev_context",
          "type": "pipeline",
          "nodes": [
            {
              "id": "fetch_jira",
              "type": "tool",
              "name": "jira_get_issue",
              "args": { "issue_key": "{{jira_issue_key}}" }
            },
            {
              "id": "fetch_related_code",
              "type": "tool",
              "name": "grep_codebase",
              "args": { "pattern": "{{feature_keyword}}", "path": "src/" }
            },
            {
              "id": "analyze_tech",
              "type": "llm",
              "model": "codex",
              "prompt": "Analyze technical context:\n\nJIRA Issue:\n{{fetch_jira.output}}\n\nRelated Code:\n{{fetch_related_code.output}}\n\nExtract:\n1. Existing API endpoints\n2. Data models involved\n3. Technical constraints\n4. Estimated complexity"
            }
          ]
        }
      ]
    },
    {
      "id": "merge_context",
      "type": "merge",
      "strategy": "concat",
      "nodes": [
        { "id": "d_ref", "type": "chain_ref", "ref": "designer_assets" },
        { "id": "t_ref", "type": "chain_ref", "ref": "dev_context" }
      ]
    },
    {
      "id": "generate_spec",
      "type": "goal_driven",
      "goal_metric": "completeness",
      "goal_operator": "gte",
      "goal_value": 0.85,
      "max_iterations": 5,
      "measure_func": "llm_judge",
      "conversational": true,
      "relay_models": ["claude", "gemini"],
      "strategy_hints": {
        "below_50": "expand_scope",
        "above_50": "refine_details"
      },
      "action_node": {
        "id": "spec_writer",
        "type": "llm",
        "model": "claude",
        "prompt": "## Feature Specification Writer (Iteration {{iteration}}/{{max_iterations}})\n\nStrategy: {{strategy}}\nCurrent completeness: {{progress}}\n\n### Context\n{{merge_context.output}}\n\n### Task\nGenerate a comprehensive feature specification including:\n1. User Stories (Given/When/Then)\n2. Acceptance Criteria\n3. Technical Requirements\n4. API Contract (if applicable)\n5. Edge Cases & Error Handling\n6. Testing Checklist\n\n{{step:Focus on core user stories first,Add technical requirements,Refine edge cases,Final polish}}"
      }
    },
    {
      "id": "review_spec",
      "type": "evaluator",
      "candidates": [
        {
          "id": "po_review",
          "type": "llm",
          "model": "gemini",
          "prompt": "As a Product Owner, review this spec for business value and user impact:\n\n{{generate_spec.output}}\n\nScore 0.0-1.0 and provide feedback."
        },
        {
          "id": "dev_review",
          "type": "llm",
          "model": "codex",
          "prompt": "As a Senior Developer, review this spec for technical feasibility:\n\n{{generate_spec.output}}\n\nScore 0.0-1.0 and flag any implementation concerns."
        }
      ],
      "scoring_func": "llm_judge",
      "scoring_prompt": "Evaluate this review for thoroughness and actionable feedback",
      "select_strategy": "all",
      "min_score": 0.7
    }
  ],
  "output": "review_spec",
  "config": {
    "max_depth": 5,
    "max_concurrency": 3,
    "timeout": 900,
    "trace": true
  }
}
```

### Pipeline Flow Visualization

```
┌─────────────────────────────────────────────────────────────┐
│                  PO Feature Spec Pipeline                    │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌─────────────┐          ┌─────────────┐                   │
│  │ Figma MCP   │          │  JIRA API   │                   │
│  │ (Designer)  │          │   (Dev)     │                   │
│  └──────┬──────┘          └──────┬──────┘                   │
│         │                        │                           │
│         ▼                        ▼                           │
│  ┌─────────────┐          ┌─────────────┐                   │
│  │ Gemini      │          │  Codex      │   ← Parallel      │
│  │ Design      │          │  Tech       │     Analysis      │
│  │ Analysis    │          │  Analysis   │                   │
│  └──────┬──────┘          └──────┬──────┘                   │
│         │                        │                           │
│         └──────────┬─────────────┘                          │
│                    ▼                                         │
│            ┌─────────────┐                                  │
│            │   Merge     │   ← Context Fusion               │
│            │  (concat)   │                                  │
│            └──────┬──────┘                                  │
│                   │                                          │
│                   ▼                                          │
│  ┌─────────────────────────────────────────────┐           │
│  │         GoalDriven Spec Writer              │           │
│  │  ┌─────────────────────────────────────┐   │           │
│  │  │ Iteration 1: Core user stories      │   │           │
│  │  │ Iteration 2: Technical requirements │   │  ← Auto   │
│  │  │ Iteration 3: Edge cases            │   │    Refine  │
│  │  │ Iteration 4: Final polish          │   │           │
│  │  └─────────────────────────────────────┘   │           │
│  │  Model Relay: Claude → Gemini → Claude     │           │
│  └──────────────────┬──────────────────────────┘           │
│                     │                                        │
│                     ▼                                        │
│       ┌─────────────────────────────┐                       │
│       │      Evaluator (2/2)        │   ← Dual Review       │
│       │  ┌─────────┐ ┌─────────┐   │                       │
│       │  │PO Review│ │Dev Review│   │                       │
│       │  │(Gemini) │ │ (Codex) │   │                       │
│       │  └─────────┘ └─────────┘   │                       │
│       └─────────────────────────────┘                       │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### Key Features Demonstrated

| Feature | Usage |
|---------|-------|
| **Fanout** | Designer + Dev 정보 병렬 수집 |
| **Tool Nodes** | Figma MCP, JIRA API, Grep 통합 |
| **Merge** | 컨텍스트 통합 |
| **GoalDriven** | 85% 완성도까지 반복 개선 |
| **Conversational** | 모델 간 컨텍스트 릴레이 |
| **Iteration Vars** | `{{iteration}}`, `{{strategy}}`, `{{step:...}}` |
| **Evaluator** | PO + Dev 동시 리뷰 |

### Iteration Variables in Action

```
Iteration 1: "Focus on core user stories first"
  → strategy: "expand_scope" (below 50%)

Iteration 2: "Add technical requirements"
  → strategy: "expand_scope" (at 45%)

Iteration 3: "Refine edge cases"
  → strategy: "refine_details" (at 65%)

Iteration 4: "Final polish"
  → strategy: "refine_details" (at 80%)

Iteration 5: Goal achieved (85%+)
  → Context summarized, relayed to Gemini
```

## Module Overview

| Module | Purpose |
|--------|---------|
| `chain_types.ml` | Type definitions (node_type, chain, result) |
| `chain_parser.ml` | JSON → AST parsing and validation |
| `chain_compiler.ml` | AST → Execution plan (DAG topology) |
| `chain_executor_eio.ml` | Eio-based parallel execution |
| `chain_registry.ml` | Chain storage and lookup |

## Testing

```bash
# Run chain engine tests
dune exec test/test_chain_engine.exe

# Run all tests
dune runtest
```

## MCP Tools (Planned)

- `chain.run` - Execute a chain
- `chain.validate` - Validate chain syntax
- `chain.register` - Register a chain in the registry
- `chain.list` - List registered chains

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
  "prompt": "Summarize: {{input}}",
  "timeout": 30
}
```

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

# Mermaid WYSIWYE Syntax Test

WYSIWYE: What You See Is What You Execute

## Test 1: LLM Nodes (Rectangle = Processing)
```mermaid
graph LR
    gemini["Parse input"] --> claude["Review content"]
    claude --> output["Final result"]
```

## Test 2: Quorum Consensus (Diamond = Decision)
```mermaid
graph TB
    input["User query"] --> casper["Strategic view"]
    input --> balthasar["Value judgment"]
    input --> melchior["Technical analysis"]
    casper --> quorum_2{"Consensus 2/3"}
    balthasar --> quorum_2
    melchior --> quorum_2
```

## Test 3: Tool Integration
```mermaid
graph LR
    eslint["Run linter"] --> tsc["Type check"]
    tsc --> claude["Code review"]
```

## Test 4: Subroutine/ChainRef (Double bracket = Reference)
```mermaid
graph LR
    input["Start"] --> my_chain[["Reusable workflow"]]
    my_chain --> output["Complete"]
```

## Test 5: Direction Vertical (TB)
```mermaid
graph TB
    step1["First"]
    step2["Second"]
    step3["Third"]
    step1 --> step2 --> step3
```

## Test 6: Flowchart TD (alias for TB)
```mermaid
flowchart TD
    A["Top node"]
    B["Bottom node"]
    A --> B
```

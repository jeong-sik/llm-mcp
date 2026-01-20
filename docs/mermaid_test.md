# Mermaid Render Test

## Test 1: Quoted text with single quotes
\`\`\`mermaid
graph LR
    A["LLM:gemini 'Parse input'"] --> B["LLM:claude 'Review'"]
    B --> C{"Quorum:2"}
\`\`\`

## Test 2: Simple colon
\`\`\`mermaid
graph LR
    A["LLM:gemini"] --> B["Tool:eslint"]
    B --> C{"Gate:is_valid"}
\`\`\`

## Test 3: Subroutine with colon
\`\`\`mermaid
graph LR
    A[["Ref:my_chain"]] --> B[["Pipeline:a,b,c"]]
\`\`\`

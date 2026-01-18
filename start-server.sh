#!/bin/bash
# Start OCaml llm-mcp server

cd "$(dirname "$0")"

# Ensure opam env is set
eval $(opam env)

# Run on port 8935 (different from Python version on 8932)
exec ./_build/default/bin/main.exe --port 8935

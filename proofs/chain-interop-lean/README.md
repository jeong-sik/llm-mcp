# Chain Interop (Lean)

This directory provides a Lean 4 mechanized proof for the lossless roundtrip *model*:
`parse (embed c) = c`.

## Scope

- This proves a **model**, not the OCaml implementation.
- The model captures the invariants used by the production system:
  - Mermaid embeds the full Chain JSON (`@chain_full`).
  - Parser prioritizes embedded JSON when present.

## Run (requires Lean)

```bash
cd proofs/chain-interop-lean
lake build
```

If Lean is not installed, install via `elan`:

```bash
curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh
```


#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

echo "=== Distributed Test Suite ==="
echo "Running concurrent + A2A + distributed tests"
echo ""

PASS=0
FAIL=0

run_test() {
  local name="$1"
  local exe="$2"
  echo "--- $name ---"
  if opam exec -- dune exec "$exe" 2>&1; then
    echo "  PASS: $name"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $name"
    FAIL=$((FAIL + 1))
  fi
  echo ""
}

run_test "Concurrent Chains" test/test_concurrent_chains.exe
run_test "A2A E2E Pipeline" test/test_chain_a2a_e2e.exe
run_test "Distributed Chains" test/test_chain_distributed.exe

echo "=== Results: $PASS passed, $FAIL failed ==="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
exit 0

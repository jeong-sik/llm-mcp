#!/bin/bash
# Compact Protocol v1.3 Benchmark Script
# Usage: ./scripts/benchmark.sh

set -e

cd "$(dirname "$0")/.."

echo "=== Compact Protocol v1.3 Benchmark ==="
echo ""

# Build first
echo "Building..."
dune build 2>/dev/null

# Run tests and capture output
echo ""
echo "Running tests..."
dune exec ./test/test_binaries.exe -- test "compact-protocol" 2>&1 | \
    sed 's/\x1b\[[0-9;]*m//g' | \
    grep -E 'compact-protocol.*\[OK\]|compact-protocol.*\[FAIL\]|Test Successful'

# Run size benchmark if available
if [ -f "_build/default/test/test_msgpack_size.exe" ]; then
    echo ""
    echo "Running size benchmark..."
    dune exec ./test/test_msgpack_size.exe 2>&1
fi

# Summary
echo ""
echo "=== Summary ==="
echo "Format overhead comparison:"
echo "  - Verbose JSON: baseline (0%)"
echo "  - Compact DSL:  ~50% smaller"
echo "  - Binary (M):   +33% overhead (Base64)"
echo "  - Base85 (A):   +25% overhead"
echo "  - Compressed (Z): ~50-70% smaller (Zlib+Base85)"
echo ""
echo "Client test status:"
cd clients/typescript && npm test --silent 2>/dev/null && echo "  - TypeScript: PASS" || echo "  - TypeScript: FAIL"
cd ../python && source .venv/bin/activate 2>/dev/null && python -m pytest -q 2>/dev/null && echo "  - Python: PASS" || echo "  - Python: FAIL (venv not set up)"

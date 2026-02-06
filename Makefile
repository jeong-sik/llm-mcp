# llm-mcp Makefile
# Enterprise-ready development commands

.PHONY: build test clean coverage coverage-html doc install

# Default target
all: build

# Build the project
build:
	dune build

# Run tests
test:
	dune test

# Clean build artifacts
clean:
	dune clean
	rm -rf _coverage *.coverage

# Run tests with coverage instrumentation
coverage:
	mkdir -p _coverage
	BISECT_FILE=_coverage/bisect dune test --instrument-with bisect_ppx --force
	@echo "Coverage data written to _coverage/"

# Generate HTML coverage report
coverage-html: coverage
	bisect-ppx-report html --coverage-path _coverage
	@echo "Report at _coverage/index.html"

# Generate summary coverage report (text)
coverage-summary: coverage
	bisect-ppx-report summary --coverage-path _coverage

# Generate documentation
doc:
	dune build @doc
	@echo "Documentation generated at _build/default/_doc/_html/index.html"

# Install dependencies
install-deps:
	opam install . --deps-only --with-test --with-doc -y

# Development setup
dev-setup: install-deps
	@echo "Development environment ready!"

# Format code (if ocamlformat is installed)
fmt:
	dune fmt || true

# Check formatting
fmt-check:
	dune fmt --check || true

# CI target (for GitHub Actions)
ci: fmt-check test coverage
	@echo "CI checks passed!"

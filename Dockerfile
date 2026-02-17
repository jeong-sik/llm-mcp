# syntax=docker/dockerfile:1
# LLM-MCP Cloud Run Dockerfile
# OCaml + Eio for HTTP MCP server

FROM ocaml/opam:ubuntu-22.04-ocaml-5.1 AS builder

# Install system dependencies
RUN sudo apt-get update && sudo apt-get install -y \
    build-essential \
    gmp-dev \
    libev-dev \
    libssl-dev \
    zlib1g-dev \
    git \
    curl \
    && sudo rm -rf /var/lib/apt/lists/*

# Install opam dependencies
RUN opam install -y dune cmdliner yojson cohttp cohttp-lwt-unix eio eio_main httpun-eio

# Copy source code
WORKDIR /app
COPY . .

# Build the project
RUN opam exec -- dune build --profile=release

# Runtime stage
FROM ocaml/opam:ubuntu-22.04-ocaml-5.1 AS runtime

# Install minimal system dependencies
RUN sudo apt-get update && sudo apt-get install -y \
    libev-dev \
    libssl-dev \
    zlib1g-dev \
    && sudo rm -rf /var/lib/apt/lists/*

# Copy only the built binary
COPY --from=builder /app/_build/default/bin/main_eio.exe /usr/local/bin/llm-mcp

# Copy data directories if needed
COPY --from=builder /app/data/ /app/data/

# Create non-root user
RUN useradd -m -u 1000 llmuser
USER llmuser

# Set working directory
WORKDIR /app

# Cloud Run injects PORT env var
ENV PORT=8932
EXPOSE ${PORT}

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:${PORT}/health || exit 1

# Run the server
CMD ["/usr/local/bin/llm-mcp", "--port", "8932", "--host", "0.0.0.0"]
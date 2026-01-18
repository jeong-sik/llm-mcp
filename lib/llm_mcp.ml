(** LLM-MCP: Multi-LLM MCP Server

    MAGI Trinity: Unified MCP wrapper for multiple LLM CLIs
    - Gemini CLI (gemini) → CASPER in MAGI
    - Codex CLI (codex exec) → MELCHIOR in MAGI
    - Claude Code (claude -p) → BALTHASAR in MAGI
    - Ollama (local LLM) → Free local inference

    Compact Protocol v4:
    - Native zstd compression (68-75% vs zlib 54-58%)
    - Trained dictionaries for LLM responses (+20-30%p for small payloads)
    - Content-type detection (code, JSON, markdown, mixed)

    MCP Protocol: 2025-11-25
*)

module Types = Types
module Tools = Tools
module Cli_runner = Cli_runner
module Parallel_utils = Parallel_utils
module Mcp_session = Mcp_session
module Sse = Sse
module Notification_sse = Notification_sse
module Mcp_server = Mcp_server
module Common = Common
module Run_log = Run_log

(* Extracted modules from tools.ml refactoring *)
module Tool_config = Tool_config
module Ollama_parser = Ollama_parser

(* Compact Protocol v4: Trained Dictionary Compression *)
module Dictionary = Dictionary

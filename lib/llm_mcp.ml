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

    Library structure:
    - llm_mcp.common: Pure OCaml shared modules (Types, Common, etc.)
    - llm_mcp.eio: Pure Eio modules (Tools_eio, Mcp_server_eio, etc.)
    - llm_mcp: Legacy Lwt modules (Tools, Mcp_server, etc.)

    For Eio-based code, use llm_mcp.eio directly:
      open Llm_mcp_eio  (* or just use llm_mcp.eio in dune *)
*)

(* Re-export common modules (from llm_mcp.common, wrapped=false) *)
module Types = Types
module Common = Common
module Dictionary = Dictionary
module Ollama_parser = Ollama_parser
module Tool_config = Tool_config
module Tool_parsers = Tool_parsers

(* Re-export Lwt-based modules *)
module Tools = Tools
module Cli_runner = Cli_runner
module Parallel_utils = Parallel_utils
module Mcp_session = Mcp_session
module Sse = Sse
module Notification_sse = Notification_sse
module Mcp_server = Mcp_server
module Run_log = Run_log
module Agent_loop = Agent_loop
module Mcp_client = Mcp_client

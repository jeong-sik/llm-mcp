(** Agent Core - Reusable Agent Loop abstraction

    This module provides a functor-based approach to building agentic loops
    that can work with different LLM backends, tool executors, and state managers.

    {1 Quick Start}

    {[
      (* Use the built-in Ollama backend *)
      module My_Tools : Agent_sigs.TOOL_EXECUTOR = struct ... end
      module My_Loop = Agent_loop_functor.Make(Ollama_backend)(My_Tools)(Default_state)

      let config = Ollama_backend.{
        base_url = "http://127.0.0.1:11434";
        model = "llama3";
        temperature = 0.7;
        stream = false;
        timeout_ms = Some 60_000;
      }

      let result = My_Loop.run
        ~config:Agent_types.default_loop_config
        ~backend_config:config
        ~initial_prompt:"Hello"
        ~tools:[]
        ()
    ]}

    {1 Key Features}

    - {b Retry with backoff}: Automatic retry on transient failures
    - {b Timeout per turn}: Prevents infinite hangs
    - {b Memory management}: Sliding window to prevent OOM
    - {b Composable}: Mix and match backends, tools, state managers

    {1 Available Modules}

    - {!Agent_types} - Shared type definitions
    - {!Agent_sigs} - Module type signatures
    - {!Agent_loop_functor} - The main functor
    - {!Default_state} - Simple state manager implementation
    - {!Orchestrator} - Multi-agent coordination patterns

    {1 Built-in Backends}

    - {!Ollama_backend} - For local LLMs via Ollama
    - {!Claude_cli_backend} - For Claude Code CLI
    - {!Openai_backend} - For OpenAI API (GPT-4, GPT-3.5, etc.)

    {1 Orchestration Patterns}

    - {b Goal-based Loop}: Run until goal reached, not just turn-limited
    - {b Evaluator-Optimizer}: Evaluate results, iterate until quality
    - {b Pipeline}: Sequential agent handoffs (A → B → C)
    - {b Orchestrator-Workers}: Central coordinator distributes tasks
*)

module Types = Agent_types
module Sigs = Agent_sigs
module Retry = Retry
module Timeout = Timeout
module Agent_loop_functor = Agent_loop_functor
module Make_Loop = Agent_loop_functor.Make
module Default_state = Default_state
module Ollama_backend = Ollama_backend
module Claude_cli_backend = Claude_cli_backend
module Openai_backend = Openai_backend
module Orchestrator = Orchestrator

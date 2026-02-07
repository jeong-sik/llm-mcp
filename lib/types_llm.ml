(** LLM Tool Arguments - Discriminated Union for all tool types

    This module contains the main tool_args type that dispatches
    to different LLM backends and chain operations. *)

open Types_core
open Types_glm

(** Tool arguments - strongly typed discriminated union for all tool calls *)
type tool_args =
  | Gemini of {
      prompt : string;
      model : string;
      thinking_level : thinking_level;
      yolo : bool;
      output_format : output_format;
      timeout : int;
      stream : bool;
      use_cli : bool;  (* true=CLI with MASC, false=direct API (faster) *)
      fallback_to_api : bool;  (* true=fallback to API on CLI failure *)
    }
  | Claude of {
      prompt : string;
      model : string;
      long_context : bool;
      system_prompt : string option;
      output_format : output_format;
      allowed_tools : string list;
      working_directory : string;
      timeout : int;
      stream : bool;
      use_cli : bool;  (* true=CLI, false=direct Anthropic API *)
      fallback_to_api : bool;  (* true=fallback to API on CLI failure *)
      api_key : string option;  (* Override ANTHROPIC_API_KEY env var *)
    }
  | Codex of {
      prompt : string;
      model : string;
      reasoning_effort : reasoning_effort;
      sandbox : sandbox_policy;
      working_directory : string option;
      timeout : int;
      stream : bool;
      use_cli : bool;  (* true=CLI, false=direct OpenAI API *)
      fallback_to_api : bool;  (* true=fallback to API on CLI failure *)
    }
  | Ollama of {
      prompt : string;
      model : string;  (* devstral, deepseek-r1:32b, qwen3-coder:30b, etc *)
      system_prompt : string option;
      temperature : float;
      timeout : int;
      stream : bool;  (* Enable SSE streaming *)
      tools : tool_schema list option;  (* MCP tools for function calling *)
    }
  | OllamaList  (* List available Ollama models *)
  | GeminiList of {
      filter : string option;  (* Substring match on model id (case-insensitive) *)
      include_all : bool;      (* Include non-generateContent models *)
    }
  | ChainRun of {
      chain : Yojson.Safe.t option;  (* Chain definition as JSON *)
      mermaid : string option;       (* Mermaid flowchart text (WYSIWYE) *)
      chain_id : string option;      (* Registered chain ID to load from data/chains/ *)
      input : string option;         (* Optional initial input *)
      trace : bool;                  (* Enable execution tracing *)
      checkpoint_enabled : bool;     (* Save checkpoints after each node *)
      timeout : int option;          (* Overall timeout in seconds *)
    }
  | ChainValidate of {
      chain : Yojson.Safe.t option;  (* Chain definition to validate *)
      mermaid : string option;       (* Mermaid flowchart text to validate *)
      strict : bool;                 (* Strict validation (completeness + format) *)
    }
  | ChainList
  | ChainToMermaid of {
      chain : Yojson.Safe.t;         (* Chain definition to convert - required *)
    }
  | ChainVisualize of {
      chain : Yojson.Safe.t;         (* Chain definition to visualize - required *)
    }
  | ChainConvert of {
      from_format : string;          (* Source format: "json" or "mermaid" *)
      to_format : string;            (* Target format: "json" or "mermaid" *)
      input : Yojson.Safe.t;         (* Input content (JSON object or string) *)
      pretty : bool;                 (* Pretty-print JSON output *)
    }
  | ChainOrchestrate of {
      goal : string;                       (* Goal description for the orchestration *)
      chain : Yojson.Safe.t option;        (* Initial chain definition (optional) *)
      tasks : Yojson.Safe.t option;        (* Optional task list for design (overrides chain-derived tasks) *)
      chain_id : string option;            (* Registered chain ID to load from data/chains/ *)
      max_replans : int;                   (* Maximum re-planning attempts *)
      timeout : int;                       (* Overall timeout in seconds *)
      trace : bool;                        (* Enable execution tracing *)
      verify_on_complete : bool;           (* Run LLM verification on completion *)
      orchestrator_model : string;         (* LLM model for Design/Verify: gemini, claude, codex, ollama, stub *)
    }
  | GhPrDiff of {
      repo : string;                       (* Repository in owner/name format *)
      pr_number : int;                     (* Pull Request number *)
    }
  | SlackPost of {
      channel : string;                    (* Slack channel ID or name *)
      text : string;                       (* Message text to post *)
      thread_ts : string option;           (* Thread timestamp for reply (optional) *)
    }
  | ChainCheckpoints of {
      chain_id : string option;            (* Filter by chain ID, or list all if None *)
      max_age_hours : int option;          (* Filter by age, cleanup if specified with cleanup=true *)
      cleanup : bool;                      (* Delete old checkpoints instead of listing *)
    }
  | ChainResume of {
      run_id : string;                     (* Run ID to resume from *)
      trace : bool;                        (* Enable execution tracing *)
    }
  | PromptRegister of {
      id : string;
      template : string;
      version : string option;
    }
  | PromptList
  | PromptGet of {
      id : string;
      version : string option;
    }
  | Glm of {
      prompt : string;
      model : string;  (* glm-4.7, glm-4.6, glm-4.5, glm-4.5-air *)
      system_prompt : string option;
      temperature : float;
      max_tokens : int option;
      timeout : int;
      stream : bool;
      thinking : bool;  (* Enable/disable chain-of-thought reasoning *)
      do_sample : bool;  (* true=diverse sampling, false=greedy deterministic *)
      web_search : bool;  (* DEPRECATED: Use tools instead. Kept for backward compat *)
      tools : glm_tool list;  (* Generic tool list: web_search, function, code_interpreter *)
      api_key : string option;  (* Override ZAI_API_KEY env var *)
    }
  | GlmTranslate of {
      text : string;
      source_lang : string;
      target_lang : string;
      strategy : glm_translation_strategy;
      model : string;
      timeout : int;
    }
  | SetStreamDelta of { enabled : bool }  (* Runtime toggle for SSE stream delta *)
  | GetStreamDelta  (* Get current stream delta status *)

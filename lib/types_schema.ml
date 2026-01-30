(** Tool Schemas for MCP - Generated JSON schemas for all tools

    This module contains the schema definitions for all LLM and Chain tools
    exposed via the MCP protocol. *)

open Types_core

(** Common response_format schema for all tools *)
let response_format_schema =
  ("response_format", `Assoc [
    ("type", `String "string");
    ("enum", `List [
      `String "verbose"; `String "compact"; `String "binary";
      `String "base85"; `String "compressed"; `String "auto"
    ]);
    ("default", `String "verbose");
    ("description", `String "Output format: verbose (JSON), compact (DSL ~50% smaller), binary (Base64), base85 (25% vs 33% overhead), compressed (Zlib+Base85), auto (adaptive based on size). v1.3: auto selects best format based on response length.");
  ])

(** Generate tool schemas for MCP *)
let gemini_schema : tool_schema = {
  name = "gemini";
  description = {|Run Gemini CLI in non-interactive mode.

Use cases:
- Quick questions to Gemini
- Code generation with Gemini 3 Pro (state-of-the-art reasoning)
- Alternative perspective from Google's model
- Complex agentic workflows with 1M token context

Parameters:
- prompt: The prompt to send
- model: Model name (default: gemini-3-pro-preview)
- thinking_level: Reasoning depth - "low" or "high" (default: high)
- budget_mode: Token-saving defaults (optional)
- yolo: Auto-approve all actions (default: false)
- timeout: Timeout in seconds (default: 300)|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "The prompt to send to Gemini");
      ]);
      ("model", `Assoc [
        ("type", `String "string");
        ("description", `String "Model name");
        ("default", `String "gemini-3-pro-preview");
      ]);
      ("thinking_level", `Assoc [
        ("type", `String "string");
        ("enum", `List [`String "low"; `String "high"]);
        ("default", `String "high");
        ("description", `String "Reasoning depth (Gemini 3 only)");
      ]);
      ("budget_mode", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable token-saving defaults");
        ("default", `Bool false);
      ]);
      ("yolo", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Auto-approve all actions");
        ("default", `Bool false);
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Timeout in seconds");
        ("default", `Int 300);
      ]);
      ("stream", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable SSE streaming (false recommended for MCP)");
        ("default", `Bool false);
      ]);
      ("use_cli", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Use CLI (slower, MASC-enabled) or direct API (faster, no MASC)");
        ("default", `Bool true);
      ]);
      ("fallback_to_api", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Fallback to direct API if CLI fails.");
        ("default", `Bool true);
      ]);
      response_format_schema;
    ]);
    ("required", `List [`String "prompt"]);
  ];
}

let claude_schema : tool_schema = {
  name = "claude-cli";
  description = {|Run Claude Code CLI in print mode (-p).

Use cases:
- Delegate tasks to another Claude instance (BALTHASAR in MAGI)
- Get a fresh perspective without current context
- Run Claude with different system prompts

Parameters:
- prompt: The prompt to send
- model: Model alias (default: opus for Opus 4.5)
- long_context: Enable 1M context window beta (default: false, requires API key)
- budget_mode: Token-saving defaults (optional)
- system_prompt: Custom system prompt (optional)
- output_format: text, json, or stream-json (default: text)
- allowed_tools: List of allowed tools (optional)
- working_directory: Directory for Claude to work in
- timeout: Timeout in seconds (default: 300)

Note: long_context=true uses API key (charges apply), false uses Max subscription.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "The prompt to send to Claude");
      ]);
      ("model", `Assoc [
        ("type", `String "string");
        ("description", `String "Model alias");
        ("default", `String "opus");
      ]);
      ("long_context", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable 1M context window (requires API key, charges apply)");
        ("default", `Bool false);
      ]);
      ("budget_mode", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable token-saving defaults");
        ("default", `Bool false);
      ]);
      ("system_prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "Custom system prompt");
      ]);
      ("output_format", `Assoc [
        ("type", `String "string");
        ("enum", `List [`String "text"; `String "json"; `String "stream-json"]);
        ("default", `String "text");
        ("description", `String "Output format");
      ]);
      ("allowed_tools", `Assoc [
        ("type", `String "array");
        ("items", `Assoc [("type", `String "string")]);
        ("description", `String "List of allowed tools");
      ]);
      ("working_directory", `Assoc [
        ("type", `String "string");
        ("description", `String "Working directory for Claude");
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Timeout in seconds");
        ("default", `Int 300);
      ]);
      ("stream", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable SSE streaming (false recommended for MCP)");
        ("default", `Bool false);
      ]);
      ("use_cli", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Use CLI mode (MASC integration). Set false for direct Anthropic API.");
        ("default", `Bool true);
      ]);
      ("fallback_to_api", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Fallback to direct API if CLI fails.");
        ("default", `Bool true);
      ]);
      response_format_schema;
    ]);
    ("required", `List [`String "prompt"]);
  ];
}

let codex_schema : tool_schema = {
  name = "codex";
  description = {|Run OpenAI Codex CLI (GPT-5.2) in non-interactive mode.

Use cases:
- Code generation with GPT-5.2 (MELCHIOR in MAGI)
- Complex reasoning tasks with xhigh reasoning effort
- Alternative AI perspective for code review
- Parallel execution with Gemini for MAGI consensus

Parameters:
- prompt: The prompt to send
- model: Model name (default: gpt-5.2)
- reasoning_effort: "low", "medium", "high", "xhigh" (default: xhigh)
- budget_mode: Token-saving defaults (optional)
- sandbox: "read-only", "workspace-write", "danger-full-access" (default: workspace-write)
- working_directory: Directory for Codex to work in
- timeout: Timeout in seconds (default: 300)

CLI Direct Usage (without MCP):
  # CORRECT: use 'codex exec' for non-interactive
  echo 'prompt...' | codex exec -c 'model="gpt-5.2"' -
  codex exec -c 'model="gpt-5.2"' "prompt"
  codex exec review  # built-in code review

  # WRONG: common mistakes
  codex -p "..."     # -p is profile, NOT prompt!
  codex --json ...   # no --json flag
  codex -a auto ...  # -a values: untrusted|on-failure|on-request|never|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "The prompt to send to Codex/GPT-5.2");
      ]);
      ("model", `Assoc [
        ("type", `String "string");
        ("description", `String "Model name");
        ("default", `String "gpt-5.2-codex");
      ]);
      ("reasoning_effort", `Assoc [
        ("type", `String "string");
        ("enum", `List [`String "low"; `String "medium"; `String "high"; `String "xhigh"]);
        ("default", `String "xhigh");
        ("description", `String "Reasoning depth level");
      ]);
      ("budget_mode", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable token-saving defaults");
        ("default", `Bool false);
      ]);
      ("sandbox", `Assoc [
        ("type", `String "string");
        ("enum", `List [`String "read-only"; `String "workspace-write"; `String "danger-full-access"]);
        ("default", `String "workspace-write");
        ("description", `String "Sandbox policy for shell commands");
      ]);
      ("working_directory", `Assoc [
        ("type", `String "string");
        ("description", `String "Working directory for Codex");
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Timeout in seconds");
        ("default", `Int 300);
      ]);
      ("stream", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable SSE streaming (false recommended for MCP)");
        ("default", `Bool false);
      ]);
      ("use_cli", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Use CLI mode (MASC integration). Set false for direct API (faster).");
        ("default", `Bool true);
      ]);
      ("fallback_to_api", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Fallback to direct API if CLI fails.");
        ("default", `Bool true);
      ]);
      response_format_schema;
    ]);
    ("required", `List [`String "prompt"]);
  ];
}

let ollama_schema : tool_schema = {
  name = "ollama";
  description = {|Run local LLM via Ollama with optional MCP tool support.

Use cases:
- Run Devstral, DeepSeek-R1, Qwen3-Coder, GLM-4.7-Flash locally (free, no API key)
- Function calling with tool-capable models (devstral, qwen3, llama3.3, glm-4.7-flash)
- Privacy-first: all processing on local machine
- 128GB RAM can run multiple 30B+ models simultaneously

Recommended models:
- glm-4.7-flash: 30B MoE (3B active), fastest reasoning, ~32 tok/s
- devstral: Code-focused, function calling
- qwen3-coder:30b: Strong coding, thinking mode
- deepseek-r1:32b: Deep reasoning

Parameters:
- prompt: The prompt to send
- model: Model name (default: devstral). Examples: glm-4.7-flash, devstral, qwen3-coder:30b
- system_prompt: System prompt for context (optional)
- temperature: Creativity level 0.0-2.0 (default: 0.7)
- timeout: Timeout in seconds (default: 300)
- tools: Array of tool definitions for function calling (optional)

Tool Calling:
When tools are provided, uses /api/chat endpoint instead of /api/generate.
Model must support tools capability (devstral, qwen3, llama3.3, etc).|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "The prompt to send to Ollama");
      ]);
      ("model", `Assoc [
        ("type", `String "string");
        ("description", `String "Model name (glm-4.7-flash, devstral, qwen3-coder:30b, deepseek-r1:32b, etc)");
        ("default", `String "devstral");
      ]);
      ("system_prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "System prompt for context");
      ]);
      ("temperature", `Assoc [
        ("type", `String "number");
        ("description", `String "Creativity level 0.0-2.0");
        ("default", `Float 0.7);
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Timeout in seconds");
        ("default", `Int 300);
      ]);
      ("tools", `Assoc [
        ("type", `String "array");
        ("description", `String "Array of MCP tool definitions for function calling. Each tool has name, description, and input_schema (JSON Schema).");
        ("items", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("name", `Assoc [("type", `String "string")]);
            ("description", `Assoc [("type", `String "string")]);
            ("input_schema", `Assoc [("type", `String "object")]);
          ]);
        ]);
      ]);
      ("stream", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable SSE streaming (false recommended for MCP)");
        ("default", `Bool false);
      ]);
      response_format_schema;
    ]);
    ("required", `List [`String "prompt"]);
  ];
}

let ollama_list_schema : tool_schema = {
  name = "ollama_list";
  description = {|List available Ollama models installed locally.

Returns a JSON array of model information including:
- name: Model name (use this for the 'model' parameter in ollama tool)
- size: Model size on disk
- modified: Last modification time

Use this to discover which models are available before calling the ollama tool.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc []);
    ("required", `List []);
  ];
}

(* ============================================================================
   GLM (Z.ai) Cloud API Schema
   ============================================================================ *)

let glm_schema : tool_schema = {
  name = "glm";
  description = {|Run glm-4.7 via Z.ai Cloud API (OpenAI-compatible) with Function Calling support.

glm-4.7 is a 355B parameter MoE model (32B active) with:
- State-of-the-art reasoning, coding, and agent capabilities
- 200K context window, 128K output
- 55+ tokens per second
- **Function Calling**: Define custom tools for GLM to invoke

Tool Types:
- web_search: Real-time web search for current information
- function: Custom function calling (OpenAI-compatible schema)
- code_interpreter: Execute Python code in sandbox

Models (lowercase required by Z.ai API):
- glm-4.7 (default): Best performance, MoE 355B/32B active
- glm-4.6: Previous generation
- glm-4.5: Older, cost-efficient
- glm-4.5-air: Lightweight version

Parameters:
- prompt: The prompt to send (required)
- model: Model name (default: glm-4.7)
- system_prompt: System prompt for context (optional)
- temperature: Creativity level 0.0-2.0 (default: 0.7)
- max_tokens: Max tokens to generate (default: 131072 = 128K full)
- timeout: Timeout in seconds (default: 300)
- stream: Enable SSE streaming (default: false, MCP-friendly)
- thinking: Enable chain-of-thought reasoning (default: false, MCP-friendly)
- do_sample: true=diverse sampling, false=greedy deterministic (default: true)
- web_search: (DEPRECATED) Use tools instead. Enable web search
- tools: Array of tool definitions (web_search, function, code_interpreter)

Tools Example:
```json
{
  "tools": [
    {"type": "web_search"},
    {"type": "function", "function": {"name": "get_weather", "parameters": {...}}}
  ]
}
```

NOTE: Defaults are optimized for MCP tool calls (synchronous, immediate response).
For SSE streaming or CoT reasoning, explicitly set stream=true or thinking=true.

Requires ZAI_API_KEY environment variable.
Coding Plan subscribers: Uses /api/coding/paas/v4 endpoint.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "The prompt to send to GLM");
      ]);
      ("model", `Assoc [
        ("type", `String "string");
        ("description", `String "Model name: glm-4.7 (default), glm-4.6, glm-4.5, glm-4.5-air (lowercase required)");
        ("default", `String "glm-4.7");
      ]);
      ("system_prompt", `Assoc [
        ("type", `String "string");
        ("description", `String "System prompt for context");
      ]);
      ("temperature", `Assoc [
        ("type", `String "number");
        ("description", `String "Creativity level 0.0-2.0");
        ("default", `Float 0.7);
      ]);
      ("max_tokens", `Assoc [
        ("type", `String "integer");
        ("description", `String "Max tokens to generate (default: 131072 = 128K full)");
        ("default", `Int 131072);
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Timeout in seconds");
        ("default", `Int 300);
      ]);
      ("stream", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable SSE streaming (false recommended for MCP tool calls)");
        ("default", `Bool false);
      ]);
      ("thinking", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable chain-of-thought reasoning (false recommended for MCP tool calls)");
        ("default", `Bool false);
      ]);
      ("do_sample", `Assoc [
        ("type", `String "boolean");
        ("description", `String "true=diverse sampling, false=greedy deterministic");
        ("default", `Bool true);
      ]);
      ("web_search", `Assoc [
        ("type", `String "boolean");
        ("description", `String "DEPRECATED: Use tools instead. Enable web search for backward compatibility");
        ("default", `Bool false);
      ]);
      ("tools", `Assoc [
        ("type", `String "array");
        ("description", `String "Array of tool definitions. Each tool: {type: 'web_search'|'function'|'code_interpreter', function?: {...}}");
        ("items", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("type", `Assoc [
              ("type", `String "string");
              ("enum", `List [`String "web_search"; `String "function"; `String "code_interpreter"]);
            ]);
            ("function", `Assoc [
              ("type", `String "object");
              ("description", `String "Function definition for function type tools");
              ("properties", `Assoc [
                ("name", `Assoc [("type", `String "string")]);
                ("description", `Assoc [("type", `String "string")]);
                ("parameters", `Assoc [("type", `String "object")]);
              ]);
            ]);
            ("web_search", `Assoc [
              ("type", `String "object");
              ("properties", `Assoc [
                ("enable", `Assoc [("type", `String "boolean")]);
                ("search_result", `Assoc [("type", `String "boolean")]);
              ]);
            ]);
            ("code_interpreter", `Assoc [
              ("type", `String "object");
              ("properties", `Assoc [
                ("sandbox", `Assoc [
                  ("type", `String "string");
                  ("enum", `List [`String "auto"; `String "none"]);
                ]);
              ]);
            ]);
          ]);
        ]);
        ("default", `List []);
      ]);
      response_format_schema;
    ]);
    ("required", `List [`String "prompt"]);
  ];
}

(** GLM Translation Tool Schema *)
let glm_translate_schema : tool_schema = {
  name = "glm.translate";
  description = {|Translation Agent using GLM 4.7 with 6 different strategies.

Strategies:
- general: Direct translation (fastest)
- paraphrased: Free/natural translation prioritizing readability
- two_step: Literal translation → Paraphrased refinement
- three_stage: Literal → Paraphrased → Expert review
- reflective: Draft → Self-critique → Revised translation (highest quality)
- chain_of_thought: Step-by-step reasoning translation

Use 'general' for speed, 'reflective' for quality, 'chain_of_thought' for complex text.

Requires ZAI_API_KEY environment variable.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("text", `Assoc [
        ("type", `String "string");
        ("description", `String "Text to translate");
      ]);
      ("source_lang", `Assoc [
        ("type", `String "string");
        ("description", `String "Source language (e.g., 'en', 'ko', 'zh', 'ja')");
      ]);
      ("target_lang", `Assoc [
        ("type", `String "string");
        ("description", `String "Target language (e.g., 'ko', 'en', 'zh', 'ja')");
      ]);
      ("strategy", `Assoc [
        ("type", `String "string");
        ("description", `String "Translation strategy");
        ("enum", `List [
          `String "general";
          `String "paraphrased";
          `String "two_step";
          `String "three_stage";
          `String "reflective";
          `String "chain_of_thought";
        ]);
        ("default", `String "general");
      ]);
      ("model", `Assoc [
        ("type", `String "string");
        ("description", `String "Model name (default: glm-4.7)");
        ("default", `String "glm-4.7");
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Timeout in seconds");
        ("default", `Int 120);
      ]);
    ]);
    ("required", `List [`String "text"; `String "source_lang"; `String "target_lang"]);
  ];
}

(* ============================================================================
   Stream Delta Runtime Toggle Schemas
   ============================================================================ *)

let set_stream_delta_schema : tool_schema = {
  name = "set_stream_delta";
  description = {|Toggle SSE stream delta broadcasting at runtime (no restart needed).

When enabled, LLM streaming responses broadcast token-by-token SSE events.
Use this for:
- Real-time token display in dashboards
- Debugging streaming behavior
- Agent monitoring

Default: disabled (for performance).
Changes take effect immediately for subsequent LLM calls.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("enabled", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable (true) or disable (false) SSE stream delta broadcasting");
        ("default", `Bool true);
      ]);
    ]);
    ("required", `List []);
  ];
}

let get_stream_delta_schema : tool_schema = {
  name = "get_stream_delta";
  description = {|Get current SSE stream delta status.

Returns whether stream delta broadcasting is currently enabled.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc []);
    ("required", `List []);
  ];
}

(* ============================================================================
   Chain Engine Schemas - Workflow Orchestration DSL
   ============================================================================ *)

let chain_run_schema : tool_schema = {
  name = "chain.run";
  description = {|Execute a Chain DSL workflow.

Parameters:
- chain: Chain DSL JSON (one of chain/mermaid required)
- mermaid: Mermaid flowchart text (one of chain/mermaid required)
- input: Initial input (string or JSON object) available as {{input.*}}
- trace: Enable execution trace (default: false)
- timeout: Overall timeout in seconds (default: 300)

Example Mermaid:
```mermaid
graph LR
    A[LLM:gemini "Analyze this"] --> B{Quorum:2}
    A --> C[LLM:claude "Review this"]
    C --> B
```|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("chain", `Assoc [
        ("type", `String "object");
        ("description", `String "Chain DSL definition with nodes and output");
      ]);
      ("mermaid", `Assoc [
        ("type", `String "string");
        ("description", `String "Mermaid flowchart text (WYSIWYE - What You See Is What You Execute)");
      ]);
      ("input", `Assoc [
        ("oneOf", `List [
          `Assoc [("type", `String "string")];
          `Assoc [("type", `String "object")];
        ]);
        ("description", `String "Initial input for chain (string or JSON object). Accessible as {{input.*}}");
      ]);
      ("trace", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable execution trace");
        ("default", `Bool false);
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Overall timeout in seconds");
        ("default", `Int 300);
      ]);
    ]);
  ];
}

let chain_validate_schema : tool_schema = {
  name = "chain.validate";
  description = {|Validate a Chain DSL definition without executing it.

Parameters:
- chain: Chain DSL JSON to validate (one of chain/mermaid required)
- mermaid: Mermaid flowchart text to validate (one of chain/mermaid required)
- strict: Strict validation (completeness + format), default true|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("chain", `Assoc [
        ("type", `String "object");
        ("description", `String "Chain DSL definition to validate");
      ]);
      ("mermaid", `Assoc [
        ("type", `String "string");
        ("description", `String "Mermaid flowchart text to validate");
      ]);
      ("strict", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Strict validation: completeness + format");
        ("default", `Bool true);
      ]);
    ]);
  ];
}

let chain_to_mermaid_schema : tool_schema = {
  name = "chain.to_mermaid";
  description = {|Convert a Chain DSL definition to Mermaid flowchart text.

Parameters:
- chain: Chain DSL JSON to convert (required)

Returns: Mermaid flowchart text that can be rendered or edited visually.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("chain", `Assoc [
        ("type", `String "object");
        ("description", `String "Chain DSL definition to convert to Mermaid");
      ]);
    ]);
    ("required", `List [`String "chain"]);
  ];
}

let chain_visualize_schema : tool_schema = {
  name = "chain.visualize";
  description = {|Generate ASCII visualization of a Chain DSL graph (terminal-friendly).

Parameters:
- chain: Chain DSL JSON to visualize (required)

Returns: ASCII tree representation with:
- Node types indicated by emojis (🤖 LLM, 🔧 Tool, 🗳️ Quorum, etc.)
- Tree structure showing data flow
- Header with chain ID, direction, node count, and output|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("chain", `Assoc [
        ("type", `String "object");
        ("description", `String "Chain DSL definition to visualize");
      ]);
    ]);
    ("required", `List [`String "chain"]);
  ];
}

let chain_convert_schema : tool_schema = {
  name = "chain.convert";
  description = {|Bidirectional conversion between Chain DSL formats.

Supported conversions:
- JSON → Mermaid: Visual flowchart from JSON definition
- Mermaid → JSON: JSON definition from flowchart

Parameters:
- from: Source format ("json" or "mermaid")
- to: Target format ("json" or "mermaid")
- input: The input content (JSON object or Mermaid string)
- pretty: For JSON output, pretty-print (default: true)

Example Mermaid → JSON:
  {"from": "mermaid", "to": "json", "input": "graph LR\n    A[LLM:gemini \"Hello\"]"}

Example JSON → Mermaid:
  {"from": "json", "to": "mermaid", "input": {"id": "test", "nodes": [...], "output": "A"}}|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("from", `Assoc [
        ("type", `String "string");
        ("description", `String "Source format: json or mermaid");
        ("enum", `List [`String "json"; `String "mermaid"]);
      ]);
      ("to", `Assoc [
        ("type", `String "string");
        ("description", `String "Target format: json or mermaid");
        ("enum", `List [`String "json"; `String "mermaid"]);
      ]);
      ("input", `Assoc [
        ("description", `String "Input content (JSON object or Mermaid string)");
      ]);
      ("pretty", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Pretty-print JSON output");
        ("default", `Bool true);
      ]);
    ]);
    ("required", `List [`String "from"; `String "to"; `String "input"]);
  ];
}

let chain_list_schema : tool_schema = {
  name = "chain.list";
  description = "List all registered chains in the registry.";
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc []);
  ];
}

let chain_checkpoints_schema : tool_schema = {
  name = "chain.checkpoints";
  description = "List saved checkpoints for chain executions. Can filter by chain_id and cleanup old checkpoints.";
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("chain_id", `Assoc [
        ("type", `String "string");
        ("description", `String "Filter checkpoints by chain ID (optional)");
      ]);
      ("cleanup_days", `Assoc [
        ("type", `String "integer");
        ("description", `String "Delete checkpoints older than N days (optional)");
      ]);
    ]);
  ];
}

let chain_resume_schema : tool_schema = {
  name = "chain.resume";
  description = "Resume a chain execution from a saved checkpoint.";
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("run_id", `Assoc [
        ("type", `String "string");
        ("description", `String "The run_id of the checkpoint to resume from");
      ]);
      ("input", `Assoc [
        ("type", `String "object");
        ("description", `String "Additional input to merge with checkpoint state (optional)");
      ]);
    ]);
    ("required", `List [`String "run_id"]);
  ];
}

let prompt_register_schema : tool_schema = {
  name = "prompt.register";
  description = "Register a versioned prompt template in the registry.";
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("id", `Assoc [
        ("type", `String "string");
        ("description", `String "Unique identifier for the prompt");
      ]);
      ("template", `Assoc [
        ("type", `String "string");
        ("description", `String "Prompt template with {{variable}} placeholders");
      ]);
      ("version", `Assoc [
        ("type", `String "string");
        ("description", `String "Semantic version (e.g., '1.0.0')");
      ]);
    ]);
    ("required", `List [`String "id"; `String "template"]);
  ];
}

let prompt_list_schema : tool_schema = {
  name = "prompt.list";
  description = "List all registered prompts with their versions and usage metrics.";
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc []);
  ];
}

let prompt_get_schema : tool_schema = {
  name = "prompt.get";
  description = "Get a prompt template by ID, optionally specifying a version.";
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("id", `Assoc [
        ("type", `String "string");
        ("description", `String "Prompt ID to retrieve");
      ]);
      ("version", `Assoc [
        ("type", `String "string");
        ("description", `String "Specific version to retrieve (optional, defaults to latest)");
      ]);
    ]);
    ("required", `List [`String "id"]);
  ];
}

let chain_orchestrate_schema : tool_schema = {
  name = "chain.orchestrate";
  description = {|Execute a goal-driven orchestration workflow with automatic re-planning.

Unlike chain.run which executes once, chain.orchestrate will:
1. Design/execute a chain to achieve the goal
2. Verify if the goal is met (via LLM verification)
3. Re-plan and retry if needed (up to max_replans times)
4. Return when goal is achieved or max attempts reached

Parameters:
- goal: Goal description to achieve (required)
- chain: Initial chain definition (optional, will be auto-designed if not provided)
- tasks: Optional task list for design (overrides chain-derived tasks)
- max_replans: Maximum re-planning attempts (default: 3)
- timeout: Overall timeout in seconds (default: 600)
- trace: Enable execution tracing (default: false)
- verify_on_complete: Run LLM verification on completion (default: true)|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("goal", `Assoc [
        ("type", `String "string");
        ("description", `String "Goal description to achieve");
      ]);
      ("chain", `Assoc [
        ("type", `String "object");
        ("description", `String "Initial chain definition (optional)");
      ]);
      ("tasks", `Assoc [
        ("type", `String "array");
        ("description", `String "Optional task list for design (overrides chain-derived tasks)");
        ("items", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("task_id", `Assoc [("type", `String "string")]);
            ("title", `Assoc [("type", `String "string")]);
            ("description", `Assoc [("type", `String "string")]);
            ("priority", `Assoc [("type", `String "integer")]);
            ("status", `Assoc [("type", `String "string")]);
            ("assignee", `Assoc [("type", `String "string")]);
          ]);
        ]);
      ]);
      ("max_replans", `Assoc [
        ("type", `String "integer");
        ("description", `String "Maximum re-planning attempts (default: 3)");
        ("default", `Int 3);
      ]);
      ("timeout", `Assoc [
        ("type", `String "integer");
        ("description", `String "Overall timeout in seconds (default: 600)");
        ("default", `Int 600);
      ]);
      ("trace", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable execution tracing (default: false)");
        ("default", `Bool false);
      ]);
      ("verify_on_complete", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Run LLM verification on completion (default: true)");
        ("default", `Bool true);
      ]);
      ("orchestrator_model", `Assoc [
        ("type", `String "string");
        ("description", `String "LLM model for Design/Verify phases: gemini, claude, codex, ollama, stub (default: gemini)");
        ("default", `String "gemini");
        ("enum", `List [`String "gemini"; `String "claude"; `String "codex"; `String "ollama"; `String "stub"]);
      ]);
    ]);
    ("required", `List [`String "goal"]);
  ];
}

(** External tool: GitHub PR diff *)
let gh_pr_diff_schema : tool_schema = {
  name = "gh_pr_diff";
  description = {|Fetch GitHub Pull Request diff using gh CLI.

Parameters:
- repo: Repository in owner/name format (e.g., "jeong-sik/llm-mcp")
- pr_number: Pull Request number (integer)

Returns the diff text that can be used for code review analysis.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("repo", `Assoc [
        ("type", `String "string");
        ("description", `String "Repository in owner/name format");
      ]);
      ("pr_number", `Assoc [
        ("type", `String "integer");
        ("description", `String "Pull Request number");
      ]);
    ]);
    ("required", `List [`String "repo"; `String "pr_number"]);
  ];
}

(** External tool: Post message to Slack channel *)
let slack_post_schema : tool_schema = {
  name = "slack_post";
  description = {|Post a message to a Slack channel.

Parameters:
- channel: Slack channel ID or name (e.g., "#general" or "C01234567")
- text: Message text to post
- thread_ts: Optional thread timestamp for reply (threaded message)

Requires SLACK_BOT_TOKEN environment variable.|};
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("channel", `Assoc [
        ("type", `String "string");
        ("description", `String "Slack channel ID or name");
      ]);
      ("text", `Assoc [
        ("type", `String "string");
        ("description", `String "Message text to post");
      ]);
      ("thread_ts", `Assoc [
        ("type", `String "string");
        ("description", `String "Thread timestamp for reply (optional)");
      ]);
    ]);
    ("required", `List [`String "channel"; `String "text"]);
  ];
}

let all_schemas = [
  gemini_schema;
  claude_schema;
  codex_schema;
  ollama_schema;
  ollama_list_schema;
  glm_schema;
  glm_translate_schema;
  set_stream_delta_schema;
  get_stream_delta_schema;
  chain_run_schema;
  chain_validate_schema;
  chain_convert_schema;
  chain_list_schema;
  chain_checkpoints_schema;
  chain_resume_schema;
  chain_to_mermaid_schema;
  chain_visualize_schema;
  chain_orchestrate_schema;
  prompt_register_schema;
  prompt_list_schema;
  prompt_get_schema;
  gh_pr_diff_schema;
  slack_post_schema;
]

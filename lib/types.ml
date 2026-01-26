(** Tool types and JSON schemas for MAGI Trinity *)

(** Thinking level for Gemini 3 *)
type thinking_level = Low | High [@@deriving yojson]

let thinking_level_of_string = function
  | "low" -> Low
  | "high" | _ -> High

let string_of_thinking_level = function
  | Low -> "low"
  | High -> "high"

(** Reasoning effort for Codex/GPT-5.2 *)
type reasoning_effort = RLow | RMedium | RHigh | RXhigh [@@deriving yojson]

let reasoning_effort_of_string = function
  | "low" -> RLow
  | "medium" -> RMedium
  | "high" -> RHigh
  | "xhigh" | _ -> RXhigh

let string_of_reasoning_effort = function
  | RLow -> "low"
  | RMedium -> "medium"
  | RHigh -> "high"
  | RXhigh -> "xhigh"

(** Sandbox policy for Codex *)
type sandbox_policy = ReadOnly | WorkspaceWrite | DangerFullAccess [@@deriving yojson]

let sandbox_policy_of_string = function
  | "read-only" -> ReadOnly
  | "danger-full-access" -> DangerFullAccess
  | "workspace-write" | _ -> WorkspaceWrite

let string_of_sandbox_policy = function
  | ReadOnly -> "read-only"
  | WorkspaceWrite -> "workspace-write"
  | DangerFullAccess -> "danger-full-access"

(** Output format for Claude CLI *)
type output_format = Text | Json | StreamJson [@@deriving yojson]

let output_format_of_string = function
  | "json" -> Json
  | "stream-json" -> StreamJson
  | "text" | _ -> Text

let string_of_output_format = function
  | Text -> "text"
  | Json -> "json"
  | StreamJson -> "stream-json"

(** MCP Tool schema - defined early for use in tool_args *)
type tool_schema = {
  name : string;
  description : string;
  input_schema : Yojson.Safe.t;
}

(** Tool arguments - strongly typed! *)
type tool_args =
  | Gemini of {
      prompt : string;
      model : string;
      thinking_level : thinking_level;
      yolo : bool;
      timeout : int;
      stream : bool;
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
    }
  | Codex of {
      prompt : string;
      model : string;
      reasoning_effort : reasoning_effort;
      sandbox : sandbox_policy;
      working_directory : string option;
      timeout : int;
      stream : bool;
      search : bool;  (* Enable web search via --search flag *)
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
  | ChainRun of {
      chain : Yojson.Safe.t option;  (* Chain definition as JSON *)
      mermaid : string option;       (* Mermaid flowchart text (WYSIWYE) *)
      input : string option;         (* Optional initial input *)
      trace : bool;                  (* Enable execution tracing *)
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
      web_search : bool;  (* Enable web search tool for current information *)
    }
  | SetStreamDelta of { enabled : bool }  (* Runtime toggle for SSE stream delta *)
  | GetStreamDelta  (* Get current stream delta status *)

(** Gemini-specific error classification for retry logic.
    These errors are detected by parsing Gemini CLI stderr/stdout.

    Reference: https://github.com/google-gemini/gemini-cli/issues/3814
    The "function response parts" error is a known bug in Gemini CLI
    that occurs when conversation history gets corrupted during
    context compression or long sessions.
*)
type gemini_error =
  | FunctionCallSyncError   (** "number of function response parts" - recoverable with retry *)
  | ContextTooLongError     (** Context exceeds limit - need to truncate prompt *)
  | RateLimitError          (** API rate limit - need backoff *)
  | AuthenticationError     (** Invalid API key or token expired *)
  | UnknownGeminiError of string  (** Unclassified error *)

(** Check if string contains substring (using Str module) *)
let str_contains ~substring s =
  try
    let _ = Str.search_forward (Str.regexp_string substring) s 0 in
    true
  with Not_found -> false

let classify_gemini_error response =
  let s = String.lowercase_ascii response in
  if String.length s > 0 then begin
    (* Function call sync error - most common, recoverable *)
    if str_contains ~substring:"function response parts" s ||
       str_contains ~substring:"function call parts" s ||
       (str_contains ~substring:"invalid_argument" s && str_contains ~substring:"function" s)
    then Some FunctionCallSyncError
    (* Context too long *)
    else if str_contains ~substring:"context" s &&
            (str_contains ~substring:"too long" s || str_contains ~substring:"exceeds" s)
    then Some ContextTooLongError
    (* Rate limit *)
    else if str_contains ~substring:"rate limit" s ||
            str_contains ~substring:"quota" s ||
            str_contains ~substring:"resource_exhausted" s
    then Some RateLimitError
    (* Authentication *)
    else if str_contains ~substring:"authentication" s ||
            str_contains ~substring:"unauthorized" s ||
            str_contains ~substring:"api key" s
    then Some AuthenticationError
    (* Check for generic HTTP error indicators - must look like an error message *)
    (* More specific patterns to avoid false positives on normal responses *)
    else if str_contains ~substring:"error:" s && str_contains ~substring:"400" s
    then Some (UnknownGeminiError "HTTP 400 Bad Request")
    else if str_contains ~substring:"error 400" s
    then Some (UnknownGeminiError "HTTP 400 Bad Request")
    else if str_contains ~substring:"400 bad request" s
    then Some (UnknownGeminiError "HTTP 400 Bad Request")
    else None
  end
  else None

let string_of_gemini_error = function
  | FunctionCallSyncError -> "FunctionCallSyncError"
  | ContextTooLongError -> "ContextTooLongError"
  | RateLimitError -> "RateLimitError"
  | AuthenticationError -> "AuthenticationError"
  | UnknownGeminiError msg -> Printf.sprintf "UnknownGeminiError(%s)" msg

let is_recoverable_gemini_error = function
  | FunctionCallSyncError -> true   (* Retry with fresh session *)
  | RateLimitError -> true          (* Retry after backoff *)
  | ContextTooLongError -> false    (* Need user intervention *)
  | AuthenticationError -> false    (* Need user intervention *)
  | UnknownGeminiError _ -> false   (* Don't know if safe to retry *)

(** Tool execution result *)
type tool_result = {
  model : string;
  returncode : int;
  response : string;
  extra : (string * string) list;  (* Additional metadata *)
}

let tool_result_to_yojson { model; returncode; response; extra } =
  let base = [
    ("model", `String model);
    ("returncode", `Int returncode);
    ("response", `String response);
  ] in
  let extra_json = List.map (fun (k, v) -> (k, `String v)) extra in
  `Assoc (base @ extra_json)

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
        ("description", `String "Enable SSE keepalive streaming");
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
        ("description", `String "Enable SSE keepalive streaming");
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
        ("description", `String "Enable SSE keepalive streaming");
        ("default", `Bool true);
      ]);
      ("search", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable web search (Bing) for Codex to access current information");
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
        ("description", `String "Enable SSE streaming with token deltas");
        ("default", `Bool true);
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
  description = {|Run glm-4.7 via Z.ai Cloud API (OpenAI-compatible).

glm-4.7 is a 355B parameter MoE model (32B active) with:
- State-of-the-art reasoning, coding, and agent capabilities
- 200K context window, 128K output
- 55+ tokens per second

Use cases:
- Complex reasoning and multi-step problem solving
- Code generation with long context (200K)
- Agent workflows requiring fast responses
- MAGI Trinity: Use as cloud alternative to local Ollama

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
- stream: Enable streaming (default: true)
- thinking: Enable chain-of-thought reasoning (default: true)
- do_sample: true=diverse sampling, false=greedy deterministic (default: true)

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
        ("description", `String "Enable SSE streaming");
        ("default", `Bool true);
      ]);
      ("thinking", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable chain-of-thought reasoning (disable for faster, direct responses)");
        ("default", `Bool true);
      ]);
      ("do_sample", `Assoc [
        ("type", `String "boolean");
        ("description", `String "true=diverse sampling, false=greedy deterministic");
        ("default", `Bool true);
      ]);
      ("web_search", `Assoc [
        ("type", `String "boolean");
        ("description", `String "Enable web search tool for accessing current information with citations");
        ("default", `Bool true);
      ]);
      response_format_schema;
    ]);
    ("required", `List [`String "prompt"]);
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

(* ============================================================================
   Compact Protocol v0.1 - LLM-to-LLM Communication
   ============================================================================ *)

(** Response format for tool output *)
type response_format =
  | Verbose     (* Full JSON, human-readable *)
  | Compact     (* DSL: OK|G3|150|result *)
  | Binary      (* msgpack, base64 encoded *)
  | Base85      (* msgpack, base85 (ascii85) encoded - 25% overhead vs 33% *)
  | Compressed  (* zlib + base85 - best for large responses *)
  | ZstdDict    (* Compact Protocol v4: zstd with trained dictionary - best compression *)
  | Auto        (* Adaptive: auto-select based on response size *)

(** Structured error types for decoding (v1.3)
    Provides compile-time exhaustiveness checking and rich error context *)
type decode_error =
  | InvalidBase85Char of char * int    (* char, position *)
  | InvalidBase85Length of int         (* actual length *)
  | DecompressionFailed of string      (* underlying error message *)
  | InvalidMsgpack of string           (* parse error *)
  | InvalidFormat of string            (* unrecognized format *)
  | MissingField of string             (* required field name *)

let string_of_decode_error = function
  | InvalidBase85Char (c, pos) ->
      Printf.sprintf "Invalid Base85 character '%c' at position %d" c pos
  | InvalidBase85Length len ->
      Printf.sprintf "Invalid Base85 length: %d (must be 0, or >= 2)" len
  | DecompressionFailed msg ->
      Printf.sprintf "Decompression failed: %s" msg
  | InvalidMsgpack msg ->
      Printf.sprintf "Invalid MessagePack: %s" msg
  | InvalidFormat msg ->
      Printf.sprintf "Invalid format: %s" msg
  | MissingField name ->
      Printf.sprintf "Missing required field: %s" name

(** Size thresholds for Auto format selection (v1.3)
    - Short responses: Compact DSL is most readable
    - Medium: Base85 encoding (better than Base64)
    - Large: Zlib compression kicks in *)
let auto_compact_threshold = 50    (* < 50 bytes: use Compact DSL *)
let auto_base85_threshold = 500    (* 50-500 bytes: use Base85 *)
(* > 500 bytes: use Compressed *)

let response_format_of_string = function
  | "compact" | "dsl" -> Compact
  | "binary" | "msgpack" -> Binary
  | "base85" | "ascii85" -> Base85
  | "compressed" | "zlib" -> Compressed
  | "zstd" | "zstd-dict" | "dict" -> ZstdDict  (* Compact Protocol v4 *)
  | "auto" | "adaptive" -> Auto
  | "verbose" | "json" | _ -> Verbose

let string_of_response_format = function
  | Verbose -> "verbose"
  | Compact -> "compact"
  | Binary -> "binary"
  | Base85 -> "base85"
  | Compressed -> "compressed"
  | ZstdDict -> "zstd-dict"
  | Auto -> "auto"

(** Model codes for compact protocol *)
type model_code = G3 | C4 | X5 | OL | Unknown of string

(** Parse model string to model_code.
    v1.0: Handles both short codes ("G3") and full names ("gemini (gemini-3-pro-preview)") *)
let model_code_of_string s =
  (* Try exact match first *)
  match s with
  | "G3" | "gemini" | "gemini-3-pro" | "gemini-3-pro-preview" -> G3
  | "C4" | "claude" | "claude-cli" | "opus" -> C4
  | "X5" | "codex" | "gpt-5.2-codex" -> X5
  | "OL" | "ollama" | "devstral" -> OL
  | _ ->
      (* v1.0: Try prefix matching for "tool (model)" format *)
      let lower = String.lowercase_ascii s in
      if String.length lower >= 6 && String.sub lower 0 6 = "gemini" then G3
      else if String.length lower >= 6 && String.sub lower 0 6 = "claude" then C4
      else if String.length lower >= 5 && String.sub lower 0 5 = "codex" then X5
      else if String.length lower >= 6 && String.sub lower 0 6 = "ollama" then OL
      else Unknown s

let string_of_model_code = function
  | G3 -> "G3"
  | C4 -> "C4"
  | X5 -> "X5"
  | OL -> "OL"
  | Unknown s -> s

let full_model_name = function
  | G3 -> "gemini-3-pro"
  | C4 -> "claude-opus-4.5"
  | X5 -> "gpt-5.2-codex"
  | OL -> "ollama"
  | Unknown s -> s

(** v2/v3: Integer encoding for model codes *)
let int_of_model_code = function
  | G3 -> 0
  | C4 -> 1
  | X5 -> 2
  | OL -> 3
  | Unknown _ -> 0  (* fallback to G3 *)

let model_code_of_int = function
  | 0 -> G3
  | 1 -> C4
  | 2 -> X5
  | 3 -> OL
  | _ -> Unknown "?"

(** Status codes for compact protocol *)
type status_code = OK | ERR | PART | STREAM | Reserved of string

let status_code_of_string = function
  | "OK" | "ok" -> OK
  | "ERR" | "err" | "error" -> ERR
  | "PART" | "partial" -> PART
  | "STREAM" | "streaming" -> STREAM
  | s -> Reserved s

let string_of_status_code = function
  | OK -> "OK"
  | ERR -> "ERR"
  | PART -> "PART"
  | STREAM -> "STREAM"
  | Reserved s -> s

(** v2/v3: Integer encoding for status codes *)
let int_of_status_code = function
  | OK -> 0
  | ERR -> 1
  | PART -> 2
  | STREAM -> 3
  | Reserved _ -> 0  (* fallback to OK *)

let status_code_of_int = function
  | 0 -> OK
  | 1 -> ERR
  | 2 -> PART
  | 3 -> STREAM
  | n -> Reserved (Printf.sprintf "Unknown(%d)" n)

(** Compact flags - parsed from string like "T2B500Y" *)
type compact_flags = {
  thinking : int option;      (* T0, T1, T2 *)
  long_context : bool;        (* L1 - 1M context beta *)
  reasoning : int option;     (* R0-R3 *)
  sandbox : int option;       (* S0-S2 *)
  budget : int option;        (* B{n} *)
  yolo : bool;                (* Y *)
}

let empty_flags = {
  thinking = None;
  long_context = false;
  reasoning = None;
  sandbox = None;
  budget = None;
  yolo = false;
}

(** Parse compact flags from string like "T2B500Y" *)
let parse_flags s =
  let len = String.length s in
  let rec parse i acc =
    if i >= len then acc
    else
      let c = s.[i] in
      match c with
      | 'T' when i + 1 < len ->
          let v = Char.code s.[i+1] - Char.code '0' in
          parse (i+2) { acc with thinking = Some v }
      | 'L' | 'U' when i + 1 < len ->  (* L = long_context, U = legacy ultrathink *)
          let v = s.[i+1] = '1' in
          parse (i+2) { acc with long_context = v }
      | 'R' when i + 1 < len ->
          let v = Char.code s.[i+1] - Char.code '0' in
          parse (i+2) { acc with reasoning = Some v }
      | 'S' when i + 1 < len ->
          let v = Char.code s.[i+1] - Char.code '0' in
          parse (i+2) { acc with sandbox = Some v }
      | 'B' ->
          (* Parse number until non-digit *)
          let j = ref (i+1) in
          while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
          let num_str = String.sub s (i+1) (!j - i - 1) in
          let budget = if num_str = "" then None else Some (int_of_string num_str) in
          parse !j { acc with budget }
      | 'Y' -> parse (i+1) { acc with yolo = true }
      | _ -> parse (i+1) acc
  in
  parse 0 empty_flags

(** Serialize flags to compact string *)
let flags_to_string f =
  let parts = [] in
  let parts = match f.thinking with Some v -> Printf.sprintf "T%d" v :: parts | None -> parts in
  let parts = if f.long_context then "L1" :: parts else parts in
  let parts = match f.reasoning with Some v -> Printf.sprintf "R%d" v :: parts | None -> parts in
  let parts = match f.sandbox with Some v -> Printf.sprintf "S%d" v :: parts | None -> parts in
  let parts = match f.budget with Some v -> Printf.sprintf "B%d" v :: parts | None -> parts in
  let parts = if f.yolo then "Y" :: parts else parts in
  String.concat "" (List.rev parts)

(** Compact response record *)
type compact_response = {
  version : int;
  status : status_code;
  model : model_code;
  tokens : int;
  result : string;
}

(* =========================================================================
   Bidirectional Compact Protocol v2.0 - INPUT COMPRESSION
   ========================================================================= *)

(** Delta operation for incremental updates *)
type delta_op =
  | Full of string           (* D|F|content - full replace *)
  | Append of string         (* D|+|content - append *)
  | Replace of int * string  (* D|R|pos|content - replace at position *)
  | Compressed of delta_op   (* D|Z{op}|... - compressed delta *)

(** Tool definition cache entry *)
type tool_cache_entry = {
  tool_id : string;          (* Short hash, e.g., "t_abc123" *)
  tool_name : string;        (* Full tool name *)
  schema_hash : string;      (* SHA256 of JSON schema *)
  cached_at : float;         (* Unix timestamp *)
}

(** Compact request record - INPUT compression *)
type compact_request = {
  req_version : int;                      (* Protocol version *)
  tool_ref : string option;               (* Cached tool reference (hash) *)
  tool_def : string option;               (* Full tool def (first time only) *)
  args : string;                          (* MessagePack encoded arguments *)
  context_deltas : delta_op list;         (* Conversation history as deltas *)
  system_ref : string option;             (* System prompt hash reference *)
  system_prompt : string option;          (* Full system prompt (first time) *)
}

(** Empty/default compact request *)
let empty_compact_request = {
  req_version = 1;
  tool_ref = None;
  tool_def = None;
  args = "";
  context_deltas = [];
  system_ref = None;
  system_prompt = None;
}

(** {1 Cache Configuration - Memory Leak Prevention} *)

let max_tool_cache_size = 256
let max_system_prompt_cache_size = 64

(** In-memory tool cache *)
let tool_cache : (string, tool_cache_entry) Hashtbl.t = Hashtbl.create 64

(** In-memory system prompt cache *)
let system_prompt_cache : (string, string) Hashtbl.t = Hashtbl.create 16

(** Evict oldest entries when cache exceeds max size (LRU) *)
let evict_oldest_tool_cache () =
  if Hashtbl.length tool_cache > max_tool_cache_size then begin
    let entries = Hashtbl.fold (fun k v acc -> (k, v.cached_at) :: acc) tool_cache [] in
    let sorted = List.sort (fun (_, t1) (_, t2) -> compare t1 t2) entries in
    let to_remove = max 1 (List.length sorted / 4) in
    List.iteri (fun i (k, _) -> if i < to_remove then Hashtbl.remove tool_cache k) sorted
  end

(** Generate short hash for caching (MD5, first 8 chars) *)
let short_hash s =
  let digest = Digest.string s in
  let hex = Digest.to_hex digest in
  String.sub hex 0 8  (* First 8 chars *)

(** Register tool definition, return cache reference *)
let cache_tool_def ~name ~schema : string =
  evict_oldest_tool_cache ();  (* Memory leak prevention *)
  let hash = short_hash (name ^ schema) in
  let id = "t_" ^ hash in
  if not (Hashtbl.mem tool_cache id) then begin
    Hashtbl.add tool_cache id {
      tool_id = id;
      tool_name = name;
      schema_hash = short_hash schema;
      cached_at = Unix.gettimeofday ();
    }
  end;
  id

(** Lookup cached tool *)
let lookup_tool id : tool_cache_entry option =
  Hashtbl.find_opt tool_cache id

(** Register system prompt, return hash reference *)
let cache_system_prompt prompt : string =
  (* Simple size check - prompt cache is string-keyed so no timestamp *)
  if Hashtbl.length system_prompt_cache > max_system_prompt_cache_size then
    Hashtbl.clear system_prompt_cache;  (* Reset on overflow *)
  let hash = "s_" ^ short_hash prompt in
  if not (Hashtbl.mem system_prompt_cache hash) then
    Hashtbl.add system_prompt_cache hash prompt;
  hash

(** Lookup cached system prompt *)
let lookup_system_prompt hash : string option =
  Hashtbl.find_opt system_prompt_cache hash

(** Encode delta operation to string *)
let rec encode_delta = function
  | Full s -> "D|F|" ^ s
  | Append s -> "D|+|" ^ s
  | Replace (pos, s) -> Printf.sprintf "D|R|%d|%s" pos s
  | Compressed op ->
      let inner = encode_delta op in
      (match Grpc_core.Codec.Gzip.encoder ~level:4 inner with
       | Ok compressed -> "D|Z|" ^ Base64.encode_string compressed
       | Error _ -> encode_delta op)  (* Fallback to uncompressed *)

(** Decode delta operation from string *)
and decode_delta s =
  if String.length s < 4 then None
  else if String.sub s 0 4 = "D|F|" then
    Some (Full (String.sub s 4 (String.length s - 4)))
  else if String.sub s 0 4 = "D|+|" then
    Some (Append (String.sub s 4 (String.length s - 4)))
  else if String.sub s 0 4 = "D|R|" then begin
    let rest = String.sub s 4 (String.length s - 4) in
    match String.index_opt rest '|' with
    | Some idx ->
        let pos = int_of_string (String.sub rest 0 idx) in
        let content = String.sub rest (idx + 1) (String.length rest - idx - 1) in
        Some (Replace (pos, content))
    | None -> None
  end
  else if String.sub s 0 4 = "D|Z|" then begin
    let rest = String.sub s 4 (String.length s - 4) in
    let decoded = Base64.decode_exn rest in
    match Grpc_core.Codec.Gzip.decoder decoded with
    | Ok decompressed -> decode_delta decompressed
    | Error _ -> None
  end
  else None

(** Apply delta to content *)
let rec apply_delta content = function
  | Full s -> s
  | Append s -> content ^ s
  | Replace (pos, s) ->
      if pos >= String.length content then content ^ s
      else String.sub content 0 pos ^ s
  | Compressed op -> apply_delta content op

(** Find longest common prefix length between two strings *)
let common_prefix_length s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let min_len = min len1 len2 in
  let rec find_diff i =
    if i >= min_len then i
    else if s1.[i] = s2.[i] then find_diff (i + 1)
    else i
  in
  find_diff 0

(** Compute diff between old and new content, return optimal delta.

    Strategy: Find longest common prefix, then choose the most compact encoding:
    - If new = old: no change needed (return Full "" or caller handles)
    - If new = old + suffix: use Append
    - If old is prefix of new with modifications: use Replace from diff point
    - Otherwise: use Full replacement

    Always compares encoding sizes and returns the smallest option. *)
let compute_delta ~old_content ~new_content : delta_op =
  if old_content = new_content then
    (* No change - Full empty is still more compact than encoding position *)
    Full new_content
  else if old_content = "" then
    Full new_content
  else
    let old_len = String.length old_content in
    let new_len = String.length new_content in
    let prefix_len = common_prefix_length old_content new_content in

    (* Check if it's a pure append case *)
    if prefix_len = old_len && new_len > old_len then
      (* New content is old + appended *)
      let suffix = String.sub new_content old_len (new_len - old_len) in
      Append suffix
    else if prefix_len > 0 then
      (* Some common prefix exists - use Replace from the diff point *)
      let replacement = String.sub new_content prefix_len (new_len - prefix_len) in
      let replace_op = Replace (prefix_len, replacement) in
      (* Compare encoding sizes: Full vs Replace *)
      let full_size = 2 + new_len in  (* "F|" + content *)
      let replace_size = 2 + String.length (string_of_int prefix_len) + 1 + String.length replacement in  (* "R|pos|" + content *)
      if replace_size < full_size then replace_op else Full new_content
    else
      (* No common prefix - full replacement *)
      Full new_content

(** Encode compact_request to MessagePack binary *)
let encode_compact_request (r : compact_request) : string =
  let open Msgpck in
  let deltas_encoded = List.map (fun d -> of_string (encode_delta d)) r.context_deltas in
  let msg = of_list [
    of_int r.req_version;
    (match r.tool_ref with Some s -> of_string s | None -> Nil);
    (match r.tool_def with Some s -> of_string s | None -> Nil);
    of_string r.args;
    of_list deltas_encoded;
    (match r.system_ref with Some s -> of_string s | None -> Nil);
    (match r.system_prompt with Some s -> of_string s | None -> Nil);
  ] in
  let buf = StringBuf.to_string msg in
  Buffer.contents buf

(** Decode MessagePack binary to compact_request *)
let decode_compact_request (data : string) : compact_request option =
  try
    let open Msgpck in
    let (_bytes_read, msg) = StringBuf.read ~pos:0 data in
    match to_list msg with
    | [ver; tool_ref; tool_def; args; deltas; sys_ref; sys_prompt] ->
        let to_string_opt = function Nil -> None | v -> Some (to_string v) in
        let decode_deltas lst =
          List.filter_map (fun v -> decode_delta (to_string v)) (to_list lst)
        in
        Some {
          req_version = to_int ver;
          tool_ref = to_string_opt tool_ref;
          tool_def = to_string_opt tool_def;
          args = to_string args;
          context_deltas = decode_deltas deltas;
          system_ref = to_string_opt sys_ref;
          system_prompt = to_string_opt sys_prompt;
        }
    | _ -> None
  with _ -> None

(** Encode response to compact DSL: "RES|OK|G3|150|result" *)
let encode_compact_response r =
  Printf.sprintf "RES|%s|%s|%d|%s"
    (string_of_status_code r.status)
    (string_of_model_code r.model)
    r.tokens
    r.result

(** Decode compact DSL to response *)
let decode_compact_response s =
  match String.split_on_char '|' s with
  | "RES" :: status :: model :: tokens :: rest ->
      Some {
        version = 1;
        status = status_code_of_string status;
        model = model_code_of_string model;
        tokens = (try int_of_string tokens with _ -> 0);
        result = String.concat "|" rest;  (* Result may contain | *)
      }
  | _ -> None

(** Default MessagePack encoding version.
    v1 = string status/model (legacy, most compatible)
    v2 = int status/model (smaller)
    v3 = int + optional tokens omission (smallest) *)
let default_msgpack_version = ref 3

(** Convert tool_result to compact_response *)
let tool_result_to_compact ?(version = !default_msgpack_version) (r : tool_result) : compact_response =
  let tokens =
    match List.assoc_opt "tokens" r.extra with
    | Some t -> (try int_of_string t with _ -> 0)
    | None -> 0
  in
  {
    version;
    status = if r.returncode = 0 then OK else ERR;
    model = model_code_of_string r.model;
    tokens;
    result = r.response;
  }

(** Encode compact_response to MessagePack binary.
    v1: [1, "OK", "G3", tokens, result] - string encoding
    v2: [2, 0, 0, tokens, result] - int encoding
    v3: [3, 0, 0, result] or [3, 0, 0, tokens, result] - int, optional tokens *)
let encode_msgpack_response (r : compact_response) : string =
  let open Msgpck in
  let msg = match r.version with
    | 1 ->
        (* v1: String encoding for status/model *)
        of_list [
          of_int 1;
          of_string (string_of_status_code r.status);
          of_string (string_of_model_code r.model);
          of_int r.tokens;
          of_string r.result;
        ]
    | 2 ->
        (* v2: Int encoding for status/model *)
        of_list [
          of_int 2;
          of_int (int_of_status_code r.status);
          of_int (int_of_model_code r.model);
          of_int r.tokens;
          of_string r.result;
        ]
    | 3 ->
        (* v3: Int encoding, omit tokens if zero *)
        if r.tokens = 0 then
          of_list [
            of_int 3;
            of_int (int_of_status_code r.status);
            of_int (int_of_model_code r.model);
            of_string r.result;
          ]
        else
          of_list [
            of_int 3;
            of_int (int_of_status_code r.status);
            of_int (int_of_model_code r.model);
            of_int r.tokens;
            of_string r.result;
          ]
    | _ ->
        (* Default to v1 for unknown versions *)
        of_list [
          of_int 1;
          of_string (string_of_status_code r.status);
          of_string (string_of_model_code r.model);
          of_int r.tokens;
          of_string r.result;
        ]
  in
  let buf = StringBuf.to_string msg in
  Buffer.contents buf

(** Decode MessagePack binary to compact_response.
    Auto-detects version from first element. *)
let decode_msgpack_response (data : string) : compact_response option =
  try
    let open Msgpck in
    let (_bytes_read, msg) = StringBuf.read ~pos:0 data in
    match to_list msg with
    (* v1: [1, "OK", "G3", tokens, result] *)
    | [ver; status; model; tokens; result] when to_int ver = 1 ->
        Some {
          version = 1;
          status = status_code_of_string (to_string status);
          model = model_code_of_string (to_string model);
          tokens = to_int tokens;
          result = to_string result;
        }
    (* v2: [2, status_int, model_int, tokens, result] *)
    | [ver; status; model; tokens; result] when to_int ver = 2 ->
        Some {
          version = 2;
          status = status_code_of_int (to_int status);
          model = model_code_of_int (to_int model);
          tokens = to_int tokens;
          result = to_string result;
        }
    (* v3: [3, status_int, model_int, result] - tokens omitted (=0) *)
    | [ver; status; model; result] when to_int ver = 3 ->
        Some {
          version = 3;
          status = status_code_of_int (to_int status);
          model = model_code_of_int (to_int model);
          tokens = 0;
          result = to_string result;
        }
    (* v3: [3, status_int, model_int, tokens, result] - tokens present *)
    | [ver; status; model; tokens; result] when to_int ver = 3 ->
        Some {
          version = 3;
          status = status_code_of_int (to_int status);
          model = model_code_of_int (to_int model);
          tokens = to_int tokens;
          result = to_string result;
        }
    | _ -> None
  with _ -> None

(** Base85 (ASCII85) encoding - 25% overhead vs Base64's 33%
    Z85 alphabet: 0-9A-Za-z.-:+=^!/*?&<>()[]{}@%$#
    Each 4 bytes → 5 characters *)
let base85_alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-:+=^!/*?&<>()[]{}@%$#"

let encode_base85 (data : string) : string =
  let len = String.length data in
  let buf = Buffer.create ((len * 5 / 4) + 5) in
  let i = ref 0 in
  while !i + 4 <= len do
    let b0 = Char.code data.[!i] in
    let b1 = Char.code data.[!i + 1] in
    let b2 = Char.code data.[!i + 2] in
    let b3 = Char.code data.[!i + 3] in
    let value = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
    (* Convert to base 85 *)
    let c4 = value mod 85 in
    let v1 = value / 85 in
    let c3 = v1 mod 85 in
    let v2 = v1 / 85 in
    let c2 = v2 mod 85 in
    let v3 = v2 / 85 in
    let c1 = v3 mod 85 in
    let c0 = v3 / 85 in
    Buffer.add_char buf base85_alphabet.[c0];
    Buffer.add_char buf base85_alphabet.[c1];
    Buffer.add_char buf base85_alphabet.[c2];
    Buffer.add_char buf base85_alphabet.[c3];
    Buffer.add_char buf base85_alphabet.[c4];
    i := !i + 4
  done;
  (* Handle remaining bytes (1-3) *)
  let remaining = len - !i in
  if remaining > 0 then begin
    let padded = Bytes.make 4 '\000' in
    for j = 0 to remaining - 1 do
      Bytes.set padded j data.[!i + j]
    done;
    let b0 = Char.code (Bytes.get padded 0) in
    let b1 = Char.code (Bytes.get padded 1) in
    let b2 = Char.code (Bytes.get padded 2) in
    let b3 = Char.code (Bytes.get padded 3) in
    let value = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
    let c4 = value mod 85 in
    let v1 = value / 85 in
    let c3 = v1 mod 85 in
    let v2 = v1 / 85 in
    let c2 = v2 mod 85 in
    let v3 = v2 / 85 in
    let c1 = v3 mod 85 in
    let c0 = v3 / 85 in
    (* Only output chars needed for remaining bytes + 1 *)
    let out_chars = remaining + 1 in
    if out_chars >= 1 then Buffer.add_char buf base85_alphabet.[c0];
    if out_chars >= 2 then Buffer.add_char buf base85_alphabet.[c1];
    if out_chars >= 3 then Buffer.add_char buf base85_alphabet.[c2];
    if out_chars >= 4 then Buffer.add_char buf base85_alphabet.[c3];
    if out_chars >= 5 then Buffer.add_char buf base85_alphabet.[c4]
  end;
  Buffer.contents buf

(** Decode Base85 encoded string back to binary data.
    v1.3: Returns structured decode_error for better error handling. *)
let decode_base85 (data : string) : (string, decode_error) result =
  if String.length data = 0 then Ok ""
  else begin
    (* Build reverse lookup table *)
    let reverse_table = Array.make 256 (-1) in
    String.iteri (fun i c -> reverse_table.(Char.code c) <- i) base85_alphabet;

    let len = String.length data in

    (* Check for invalid length early *)
    if len = 1 then Error (InvalidBase85Length len)
    else begin
      let buf = Buffer.create ((len * 4 / 5) + 4) in
      let error = ref None in
      let i = ref 0 in

      (* Helper to find first invalid char in a range *)
      let find_invalid_char start count =
        let rec check j =
          if j >= count then None
          else
            let pos = start + j in
            let c = data.[pos] in
            if reverse_table.(Char.code c) < 0 then Some (c, pos)
            else check (j + 1)
        in check 0
      in

      (* Decode full 5-char blocks *)
      while !error = None && !i + 5 <= len do
        match find_invalid_char !i 5 with
        | Some (c, pos) -> error := Some (InvalidBase85Char (c, pos))
        | None ->
            let c0 = reverse_table.(Char.code data.[!i]) in
            let c1 = reverse_table.(Char.code data.[!i + 1]) in
            let c2 = reverse_table.(Char.code data.[!i + 2]) in
            let c3 = reverse_table.(Char.code data.[!i + 3]) in
            let c4 = reverse_table.(Char.code data.[!i + 4]) in
            let value = c0 * 52200625 + c1 * 614125 + c2 * 7225 + c3 * 85 + c4 in
            Buffer.add_char buf (Char.chr ((value lsr 24) land 0xFF));
            Buffer.add_char buf (Char.chr ((value lsr 16) land 0xFF));
            Buffer.add_char buf (Char.chr ((value lsr 8) land 0xFF));
            Buffer.add_char buf (Char.chr (value land 0xFF));
            i := !i + 5
      done;

      match !error with
      | Some e -> Error e
      | None ->
          (* Handle remaining chars (2-4) → (1-3) bytes *)
          let remaining = len - !i in
          if remaining > 0 then begin
            match find_invalid_char !i remaining with
            | Some (c, pos) -> Error (InvalidBase85Char (c, pos))
            | None ->
                let padded = Array.make 5 84 in
                for j = 0 to remaining - 1 do
                  padded.(j) <- reverse_table.(Char.code data.[!i + j])
                done;
                let value = padded.(0) * 52200625 + padded.(1) * 614125 +
                            padded.(2) * 7225 + padded.(3) * 85 + padded.(4) in
                let out_bytes = remaining - 1 in
                if out_bytes >= 1 then Buffer.add_char buf (Char.chr ((value lsr 24) land 0xFF));
                if out_bytes >= 2 then Buffer.add_char buf (Char.chr ((value lsr 16) land 0xFF));
                if out_bytes >= 3 then Buffer.add_char buf (Char.chr ((value lsr 8) land 0xFF));
                Ok (Buffer.contents buf)
          end
          else Ok (Buffer.contents buf)
    end
  end

(** Convenience wrapper that converts decode_error to string.
    For backward compatibility with existing code. *)
let decode_base85_string (data : string) : (string, string) result =
  match decode_base85 data with
  | Ok s -> Ok s
  | Error e -> Error (string_of_decode_error e)

(** Format tool_result based on response_format.
    v1.3: Adaptive format selection based on response size. *)
let format_tool_result ~(format : response_format) (r : tool_result) : string =
  let verbose_fallback () = Yojson.Safe.to_string (tool_result_to_yojson r) in
  let compact_response = lazy (tool_result_to_compact r) in
  let msgpack_bytes = lazy (encode_msgpack_response (Lazy.force compact_response)) in
  match format with
  | Verbose -> verbose_fallback ()
  | Compact ->
      (try
        encode_compact_response (Lazy.force compact_response)
      with _ -> verbose_fallback ())
  | Binary ->
      (try
        Printf.sprintf "M%s" (Base64.encode_string (Lazy.force msgpack_bytes))
      with _ -> verbose_fallback ())
  | Base85 ->
      (try
        Printf.sprintf "A%s" (encode_base85 (Lazy.force msgpack_bytes))
      with _ -> verbose_fallback ())
  | Compressed ->
      (try
        (* v1.3: Real Gzip compression using grpc-core codec *)
        let msgpack = Lazy.force msgpack_bytes in
        match Grpc_core.Codec.Gzip.encoder ~level:4 msgpack with
        | Ok compressed -> Printf.sprintf "Z%s" (encode_base85 compressed)
        | Error _ -> verbose_fallback ()
      with _ -> verbose_fallback ())
  | ZstdDict ->
      (try
        (* Compact Protocol v4: Zstd with trained dictionary *)
        let msgpack = Lazy.force msgpack_bytes in
        match Dictionary.load_default () with
        | Some dict ->
            let compressed = Dictionary.compress_with_dict dict msgpack in
            if String.length compressed < String.length msgpack then
              Printf.sprintf "D%s" (encode_base85 compressed)  (* D = Dictionary-compressed *)
            else
              (* Dictionary didn't help, fall back to raw zstd *)
              Printf.sprintf "S%s" (encode_base85 (Zstd.compress ~level:3 msgpack))
        | None ->
            (* No dictionary, use raw zstd *)
            Printf.sprintf "S%s" (encode_base85 (Zstd.compress ~level:3 msgpack))
      with _ -> verbose_fallback ())
  | Auto ->
      (* v1.4 Adaptive Format Selection with Zstd (Compact Protocol v4) *)
      let result_len = String.length r.response in
      if result_len < auto_compact_threshold then
        (* Short: readable Compact DSL *)
        (try encode_compact_response (Lazy.force compact_response)
         with _ -> verbose_fallback ())
      else if result_len < auto_base85_threshold then
        (* Medium: efficient Base85 *)
        (try Printf.sprintf "A%s" (encode_base85 (Lazy.force msgpack_bytes))
         with _ -> verbose_fallback ())
      else
        (* Large: Zstd compression (replaces Gzip for better ratio) *)
        (try
          let msgpack = Lazy.force msgpack_bytes in
          let compressed = Zstd.compress ~level:3 msgpack in
          Printf.sprintf "S%s" (encode_base85 compressed)  (* S = Standard Zstd *)
         with _ -> verbose_fallback ())

(** Decode formatted response back to compact_response.
    Compact Protocol v4: Supports all format prefixes.

    Format prefixes:
    - M: MessagePack + Base64
    - A: MessagePack + Base85
    - Z: Gzip + Base85
    - S: Zstd + Base85 (v4)
    - D: Dictionary Zstd + Base85 (v4)
    - RES|...: DSL format
    - {...}: JSON verbose

    @since v1.4 *)
let decode_formatted_response (data : string) : (compact_response, string) result =
  if String.length data = 0 then
    Error "Empty response"
  else
    let prefix = data.[0] in
    let rest = String.sub data 1 (String.length data - 1) in
    match prefix with
    | 'M' ->
        (* MessagePack + Base64 *)
        (try
          let decoded = Base64.decode_exn rest in
          match decode_msgpack_response decoded with
          | Some r -> Ok r
          | None -> Error "Invalid MessagePack in M format"
        with e -> Error (Printf.sprintf "M decode failed: %s" (Printexc.to_string e)))
    | 'A' ->
        (* MessagePack + Base85 *)
        (match decode_base85 rest with
        | Ok decoded ->
            (match decode_msgpack_response decoded with
            | Some r -> Ok r
            | None -> Error "Invalid MessagePack in A format")
        | Error e -> Error (Printf.sprintf "A decode failed: %s" (string_of_decode_error e)))
    | 'Z' ->
        (* Gzip + Base85 *)
        (match decode_base85 rest with
        | Ok decoded ->
            (match Grpc_core.Codec.Gzip.decoder decoded with
            | Ok decompressed ->
                (match decode_msgpack_response decompressed with
                | Some r -> Ok r
                | None -> Error "Invalid MessagePack in Z format")
            | Error e -> Error (Printf.sprintf "Z decompress failed: %s" e))
        | Error e -> Error (Printf.sprintf "Z decode failed: %s" (string_of_decode_error e)))
    | 'S' ->
        (* Zstd + Base85 (Compact Protocol v4) *)
        (match decode_base85 rest with
        | Ok decoded ->
            (try
              let estimated_size = max 4096 (String.length decoded * 8) in
              let decompressed = Zstd.decompress estimated_size decoded in
              match decode_msgpack_response decompressed with
              | Some r -> Ok r
              | None -> Error "Invalid MessagePack in S format"
            with Zstd.Error e -> Error (Printf.sprintf "S decompress failed: %s" e))
        | Error e -> Error (Printf.sprintf "S decode failed: %s" (string_of_decode_error e)))
    | 'D' ->
        (* Dictionary Zstd + Base85 (Compact Protocol v4) *)
        (match decode_base85 rest with
        | Ok decoded ->
            (match Dictionary.load_default () with
            | Some dict ->
                (match Dictionary.decompress_with_dict dict decoded with
                | Ok decompressed ->
                    (match decode_msgpack_response decompressed with
                    | Some r -> Ok r
                    | None -> Error "Invalid MessagePack in D format")
                | Error e -> Error (Printf.sprintf "D decompress failed: %s" e))
            | None -> Error "D format requires dictionary but none loaded")
        | Error e -> Error (Printf.sprintf "D decode failed: %s" (string_of_decode_error e)))
    | 'R' ->
        (* DSL format: RES|OK|G3|150|result *)
        (match decode_compact_response data with
        | Some r -> Ok r
        | None -> Error "Invalid DSL format")
    | '{' ->
        (* JSON verbose - parse and convert *)
        (try
          let json = Yojson.Safe.from_string data in
          let open Yojson.Safe.Util in
          Ok {
            version = 1;
            status = (try status_code_of_string (json |> member "status" |> to_string) with _ -> OK);
            model = (try model_code_of_string (json |> member "model" |> to_string) with _ -> Unknown "");
            tokens = (try json |> member "tokens" |> to_int with _ -> 0);
            result = (try json |> member "response" |> to_string with _ -> "");
          }
        with e -> Error (Printf.sprintf "JSON parse failed: %s" (Printexc.to_string e)))
    | _ ->
        Error (Printf.sprintf "Unknown format prefix: %c" prefix)

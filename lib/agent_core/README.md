# Agent Core

> Reusable Agent Loop abstraction with retry, timeout, and memory management.

## Quick Start (Built-in Ollama Backend)

```ocaml
open Agent_core

(* 1. Define your tools *)
module My_Tools : Sigs.TOOL_EXECUTOR = struct
  let execute tc =
    match tc.Types.name with
    | "search" -> Lwt.return (Result.Ok (Types.ToolSuccess "Found: ..."))
    | _ -> Lwt.return (Result.Error "Unknown tool")

  let to_message tc result =
    let open Types in
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.name }

  let available_tools () = ["search"; "calculate"]
end

(* 2. Create the loop with built-in Ollama backend *)
module My_Loop = Make_Loop(Ollama_backend)(My_Tools)(Default_state)

(* 3. Run it! *)
let () =
  let backend_config = Ollama_backend.{
    base_url = "http://127.0.0.1:11434";
    model = "llama3";
    temperature = 0.7;
    stream = false;
    timeout_ms = Some 60_000;
  } in

  let loop_config = {
    Types.default_loop_config with
    max_turns = 10;
    timeout_ms = 30000;
    max_messages = 50;
  } in

  let result = Lwt_main.run (
    My_Loop.run
      ~config:loop_config
      ~backend_config
      ~initial_prompt:"Hello, solve this problem..."
      ~tools:[]
      ()
  ) in

  match result with
  | Types.Completed { response; turns_used } ->
      Printf.printf "Done in %d turns: %s\n" turns_used response
  | Types.MaxTurnsReached { last_response; _ } ->
      Printf.printf "Max turns: %s\n" last_response
  | Types.TimedOut { turns_completed } ->
      Printf.printf "Timed out after %d turns\n" turns_completed
  | Types.Error msg ->
      Printf.printf "Error: %s\n" msg
  | Types.CircuitOpen ->
      Printf.printf "Circuit breaker tripped\n"
```

## Custom Backend Example

```ocaml
(* Define your own LLM backend *)
module My_Backend : Agent_core.Sigs.LLM_BACKEND = struct
  type config = { api_url: string; model: string }
  type response = { content: string; tool_calls: tool_call list option }

  let name = "my_llm"

  let call ~config ~messages ~tools =
    (* Your LLM API call here *)
    Lwt.return (Result.Ok { content = "Hello!"; tool_calls = None })

  let parse_tool_calls r = r.tool_calls
  let extract_content r = r.content
  let is_final r = r.tool_calls = None
end

module My_Loop = Agent_core.Make_Loop(My_Backend)(My_Tools)(Agent_core.Default_state)
```

## Key Features

### 1. Retry with Exponential Backoff

```ocaml
let policy = {
  max_attempts = 5;
  initial_delay_ms = 100;
  max_delay_ms = 5000;
  backoff_multiplier = 2.0;
  jitter = true;  (* ±25% randomness *)
}

let result = Agent_core.Retry.with_retry policy (fun () ->
  (* Your fallible operation *)
  call_api ()
)
```

### 2. Timeout per Turn

```ocaml
(* Wrap any Lwt operation with timeout *)
let result = Agent_core.Timeout.with_timeout_ms 5000 (fun () ->
  slow_operation ()
) in
match result with
| Some value -> (* completed *)
| None -> (* timed out *)
```

### 3. Memory Management (Sliding Window)

```ocaml
(* Automatically trims old messages, preserving system message *)
let config = {
  default_loop_config with
  max_messages = 100;  (* Keep last 100 messages *)
}

(* Manual trimming *)
let state = Default_state.trim_to_window state ~max_messages:50
```

### 4. Callbacks for Monitoring

```ocaml
let callbacks = {
  on_turn_start = Some (fun turn ->
    Printf.printf "Turn %d starting...\n" turn;
    Lwt.return_unit);
  on_turn_end = Some (fun turn msg ->
    Printf.printf "Turn %d: %s\n" turn msg.content;
    Lwt.return_unit);
  on_tool_call = Some (fun tc result ->
    Printf.printf "Tool %s: %s\n" tc.name (match result with ToolSuccess s -> s | _ -> "error");
    Lwt.return_unit);
  on_error = Some (fun err ->
    Printf.eprintf "Error: %s\n" err;
    Lwt.return_unit);
}

My_Loop.run ~callbacks ~config ...
```

## Module Structure

```
agent_core/
├── agent_core.ml          # Main entry point
├── agent_types.ml         # Shared types (message, tool_call, etc.)
├── agent_sigs.ml          # Module signatures (LLM_BACKEND, TOOL_EXECUTOR, STATE_MANAGER)
├── agent_loop_functor.ml  # The main functor
├── retry.ml               # Exponential backoff with jitter
├── timeout.ml             # Lwt-based timeout
├── default_state.ml       # Default state manager
├── ollama_backend.ml      # Built-in Ollama LLM backend
├── BENCHMARKS.md          # Performance benchmarks
└── README.md              # This file
```

## Built-in Backends

### Ollama Backend

The `Ollama_backend` module provides a ready-to-use implementation for local LLMs via Ollama:

```ocaml
(* Configuration options *)
type config = {
  base_url : string;       (* Default: "http://127.0.0.1:11434" *)
  model : string;          (* e.g., "llama3", "qwen3", "devstral" *)
  temperature : float;     (* 0.0 - 2.0 *)
  stream : bool;           (* Streaming support *)
  timeout_ms : int option; (* Request timeout *)
}

(* Utility functions *)
val health_check : ?base_url:string -> unit -> bool Lwt.t
val list_models : ?base_url:string -> unit -> (string list, string) result Lwt.t
```

### Claude CLI Backend

The `Claude_cli_backend` module wraps the Claude Code CLI for programmatic access:

```ocaml
(* Configuration options *)
type config = {
  model : string;              (* "sonnet", "opus", "haiku" *)
  timeout_ms : int option;     (* Command timeout *)
  system_prompt : string option;
  allowed_tools : string list option;
  print_mode : bool;           (* Always true for non-interactive *)
}

(* Utility functions *)
val is_available : unit -> bool Lwt.t
val version : unit -> (string, string) result Lwt.t
```

### OpenAI Backend

The `Openai_backend` module provides access to OpenAI's API (GPT-4, GPT-3.5, etc.):

```ocaml
(* Configuration options *)
type config = {
  api_key : string;           (* OPENAI_API_KEY *)
  model : string;             (* "gpt-4", "gpt-3.5-turbo", etc. *)
  temperature : float;        (* 0.0 - 2.0 *)
  base_url : string;          (* For Azure or proxies *)
  timeout_ms : int option;
  max_tokens : int option;
  organization : string option;
}

(* Utility functions *)
val has_api_key : config -> bool
val config_from_env : ?model:string -> unit -> config
val list_models : config:config -> unit -> (string list, string) result Lwt.t
```

## Types Reference

```ocaml
(* Message roles *)
type role = System | User | Assistant | Tool

(* Tool call from LLM *)
type tool_call = {
  id: string;
  name: string;
  arguments: Yojson.Safe.t;
}

(* Tool execution result *)
type tool_result =
  | ToolSuccess of string
  | ToolError of string

(* Loop configuration *)
type loop_config = {
  max_turns: int;           (* Default: 10 *)
  timeout_ms: int;          (* Default: 30000 *)
  max_messages: int;        (* Default: 100 *)
  retry_policy: retry_policy;
}

(* Loop result *)
type loop_result =
  | Completed of { response: string; turns_used: int }
  | MaxTurnsReached of { last_response: string; turns_used: int }
  | TimedOut of { turns_completed: int }
  | Error of string
```

## Benchmarks

See [BENCHMARKS.md](./BENCHMARKS.md) for detailed performance results.

**TL;DR:**
- 3,346 turns/minute sustained
- 55.7 problems/sec throughput
- 90% failure rate → still completes with retries
- Memory bounded via sliding window
- 44 tests (unit + stress + chaos + soak)

## Installation

```bash
# In your dune file
(libraries llm_mcp.agent_core)

# Build
dune build
```

## License

MIT

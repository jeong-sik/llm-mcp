# Agent Core Eio

> Direct-style Agent abstraction with Eio (replaces Lwt-based agent_core)

## LLM Backends

### Ollama Backend

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let config = Ollama_backend_eio.{
    base_url = "http://127.0.0.1:11434";
    model = "llama3";
    temperature = 0.7;
    stream = false;
    timeout_ms = Some 60_000;
  } in

  let result = Ollama_backend_eio.call
    ~sw ~net ~config
    ~messages:[{ role = User; content = "Hello"; ... }]
    ~tools:[]
```

### Claude CLI Backend

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let config = Claude_cli_backend_eio.{
    model = "sonnet";  (* or "opus", "haiku" *)
    timeout_ms = Some 120_000;
    system_prompt = None;
    allowed_tools = None;
    print_mode = true;
  } in

  let result = Claude_cli_backend_eio.call
    ~sw ~proc_mgr ~clock ~config
    ~messages ~tools
```

### OpenAI Backend

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let config = Openai_backend_eio.{
    api_key = Sys.getenv "OPENAI_API_KEY";
    model = "gpt-4";
    temperature = 0.7;
    base_url = "https://api.openai.com/v1";
    timeout_ms = Some 60_000;
    max_tokens = None;
    organization = None;
  } in

  let result = Openai_backend_eio.call
    ~sw ~net ~config
    ~messages ~tools
```

## Validator System

Composable validation with monadic operations:

```ocaml
(* Sequential composition *)
module V1_then_V2 = (val Compose.seq (module V1) (module V2))

(* Parallel execution *)
module V1_and_V2 = (val Compose.parallel ~sw (module V1) (module V2))

(* Dynamic selection *)
module Dynamic = (val Compose.bind (module Assess) select_next)

(* Quorum consensus *)
module Quorum = (val Compose.quorum ~sw ~required:2 validators)
```

## Key Differences from agent_core (Lwt)

| Aspect | agent_core (Lwt) | agent_core_eio |
|--------|------------------|----------------|
| Concurrency | Monadic (`Lwt.t`) | Direct style |
| HTTP | cohttp-lwt-unix | cohttp-eio |
| Process | Lwt_process | Eio.Process |
| Entry point | `Lwt_main.run` | `Eio_main.run` |
| Capabilities | Global | Passed as args |

## Capability Pattern

Eio uses explicit capability passing instead of global resources:

```ocaml
(* HTTP client needs network capability *)
let call ~sw ~net ~config = ...

(* CLI backend needs process manager *)
let call ~sw ~proc_mgr ~clock ~config = ...

(* Validators need switch for parallelism *)
let module Para = (val Compose.parallel ~sw v1 v2)
```

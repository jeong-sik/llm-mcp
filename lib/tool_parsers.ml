(** Tool argument parsers and command builders - Pure OCaml, NO Lwt!

    This module contains pure utility functions for:
    - Parsing JSON arguments into tool_args types
    - Building CLI commands for various LLM tools
    - Parsing/cleaning LLM responses

    Shared between llm_mcp_eio (pure Eio) and llm_mcp (Lwt) libraries.
*)

open Types

(* Re-export Tool_config utilities *)
let budget_mode_value = Tool_config.budget_mode_value

(** {1 Environment-based Defaults}
    These can be configured in .mcp.json's env field:
    {
      "mcpServers": {
        "llm-mcp": {
          "env": {
            "LLM_MCP_USE_CLI": "true",
            "LLM_MCP_FALLBACK_TO_API": "true"
          }
        }
      }
    }
*)

(** {1 Default Constants} *)

(** Default timeout for LLM tool calls (seconds) *)
let default_timeout = 300

(** Default stream setting for LLM tools *)
let default_stream = true

(** Get use_cli default from LLM_MCP_USE_CLI env var (default: true) *)
let default_use_cli () =
  match Sys.getenv_opt "LLM_MCP_USE_CLI" with
  | Some "false" | Some "0" | Some "no" -> false
  | _ -> true

(** Get fallback_to_api default from LLM_MCP_FALLBACK_TO_API env var (default: true) *)
let default_fallback_to_api () =
  match Sys.getenv_opt "LLM_MCP_FALLBACK_TO_API" with
  | Some "false" | Some "0" | Some "no" -> false
  | _ -> true

(** Resolve Claude model aliases to stable IDs. *)
let resolve_claude_model (model : string) : string =
  match String.lowercase_ascii model with
  | "claude" | "sonnet" -> "claude-3-5-sonnet-20241022"
  | "haiku" -> "claude-3-5-haiku-20241022"
  | "opus" | "opus-4" -> "claude-opus-4-20250514"
  | _ -> model

(** Resolve Gemini model aliases to stable IDs (for CLI / API parity). *)
let resolve_gemini_model (model : string) : string =
  match String.lowercase_ascii model with
  | "gemini" -> "gemini-3-pro-preview"
  | "pro" -> "gemini-2.5-pro"
  | "flash" -> "gemini-2.5-flash"
  | "flash-lite" -> "gemini-2.5-flash-lite"
  | "3-pro" -> "gemini-3-pro-preview"
  | "3-flash" -> "gemini-3-flash-preview"
  | _ -> model

(** {1 Argument Parsers} *)

(** Parse JSON arguments for Gemini tool *)
let parse_gemini_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"gemini-3-pro-preview" in
  let budget_mode = budget_mode_value json in
  let thinking_level =
    json |> member "thinking_level" |> to_string_option
    |> Option.value ~default:(if budget_mode then "low" else "high")
    |> thinking_level_of_string in
  let yolo = json |> member "yolo" |> to_bool_option |> Option.value ~default:false in
  let output_format =
    json |> member "output_format" |> to_string_option
    |> Option.value ~default:"text"
    |> output_format_of_string in
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:default_timeout in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:default_stream in
  let use_cli =
    match json |> member "use_cli" |> to_bool_option with
    | Some v -> v
    | None ->
        (* If an API key is available, prefer direct API by default:
           - broader model coverage than the CLI in some setups
           - avoids "only 2.0 is visible" drift from local CLI installs *)
        let api_key = Tools_tracer.get_api_key "GEMINI_API_KEY" |> String.trim in
        if api_key <> "" then false else default_use_cli ()
  in
  let fallback_to_api = json |> member "fallback_to_api" |> to_bool_option |> Option.value ~default:(default_fallback_to_api ()) in
  Gemini { prompt; model; thinking_level; yolo; output_format; timeout; stream; use_cli; fallback_to_api }

(** Parse JSON arguments for Claude tool *)
let parse_claude_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"sonnet" in
  (* long_context enables 1M context beta (requires API key, charges apply)
     Also support legacy "ultrathink" parameter for backwards compatibility *)
  let long_context =
    match json |> member "long_context" |> to_bool_option with
    | Some v -> v
    | None -> json |> member "ultrathink" |> to_bool_option |> Option.value ~default:false in
  let system_prompt = json |> member "system_prompt" |> to_string_option in
  let output_format =
    json |> member "output_format" |> to_string_option
    |> Option.value ~default:"text"
    |> output_format_of_string in
  let allowed_tools =
    try json |> member "allowed_tools" |> to_list |> List.map to_string
    with Type_error _ -> [] in
  let working_directory =
    json |> member "working_directory" |> to_string_option
    |> Option.value ~default:(Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp") in
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:default_timeout in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:default_stream in
  let use_cli = json |> member "use_cli" |> to_bool_option |> Option.value ~default:(default_use_cli ()) in
  let fallback_to_api = json |> member "fallback_to_api" |> to_bool_option |> Option.value ~default:(default_fallback_to_api ()) in
  let api_key = json |> member "api_key" |> to_string_option in
  Claude { prompt; model; long_context; system_prompt; output_format; allowed_tools; working_directory; timeout; stream; use_cli; fallback_to_api; api_key }

(** Parse JSON arguments for Codex tool *)
let parse_codex_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"gpt-5.2" in
  (* Default to "medium" - xhigh takes too long for most tasks *)
  let reasoning_effort =
    json |> member "reasoning_effort" |> to_string_option
    |> Option.value ~default:"medium"
    |> reasoning_effort_of_string in
  let sandbox =
    json |> member "sandbox" |> to_string_option
    |> Option.value ~default:"workspace-write"
    |> sandbox_policy_of_string in
  let working_directory = json |> member "working_directory" |> to_string_option in
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:default_timeout in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:default_stream in
  let use_cli = json |> member "use_cli" |> to_bool_option |> Option.value ~default:(default_use_cli ()) in
  let fallback_to_api = json |> member "fallback_to_api" |> to_bool_option |> Option.value ~default:(default_fallback_to_api ()) in
  Codex { prompt; model; reasoning_effort; sandbox; working_directory; timeout; stream; use_cli; fallback_to_api }

(** Parse JSON arguments for Ollama (local LLM) tool *)
let parse_ollama_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"devstral" in
  let system_prompt = json |> member "system_prompt" |> to_string_option in
  let temperature =
    try json |> member "temperature" |> to_float
    with Type_error _ -> 0.7 in
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:default_timeout in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:default_stream in
  let tools = match json |> member "tools" with
    | `Null -> None
    | `List tool_list ->
        Some (List.filter_map (fun tool_json ->
          try
            let name = tool_json |> member "name" |> to_string in
            let description = tool_json |> member "description" |> to_string_option |> Option.value ~default:"" in
            let input_schema = tool_json |> member "input_schema" in
            Some { Types.name; description; input_schema }
          with _ -> None
        ) tool_list)
    | _ -> None
  in
  Ollama { prompt; model; system_prompt; temperature; timeout; stream; tools }

let parse_ollama_list_args (_json : Yojson.Safe.t) : tool_args =
  OllamaList

(** Parse JSON arguments for gemini_list tool *)
let parse_gemini_list_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let filter = json |> member "filter" |> to_string_option in
  let include_all = json |> member "include_all" |> to_bool_option |> Option.value ~default:false in
  GeminiList { filter; include_all }

(** Parse JSON arguments for GLM (Z.ai) tool *)
(** Parse a single GLM tool from JSON *)
let parse_glm_tool (json : Yojson.Safe.t) : Types.glm_tool =
  let open Yojson.Safe.Util in
  let tool_type_str = json |> member "type" |> to_string in
  let tool_type = Types.glm_tool_type_of_string_unsafe tool_type_str in
  match tool_type with
  | Types.GlmWebSearch ->
      let web_search_obj = json |> member "web_search" in
      let enable = (try web_search_obj |> member "enable" |> to_bool with _ -> true) in
      let search_result = (try web_search_obj |> member "search_result" |> to_bool with _ -> true) in
      { Types.tool_type = Types.GlmWebSearch;
        function_schema = None;
        web_search_config = Some (enable, search_result);
        code_interpreter_config = None }
  | Types.GlmFunction ->
      let func = json |> member "function" in
      let func_name = func |> member "name" |> to_string in
      let func_description =
        (try func |> member "description" |> to_string with _ -> "") in
      (* Parse parameters from JSON Schema format *)
      let func_parameters = 
        let params = func |> member "parameters" in
        let properties = params |> member "properties" in
        let required_list = 
          try params |> member "required" |> to_list |> List.map to_string
          with _ -> []
        in
        match properties with
        | `Assoc props ->
          List.filter_map (fun (name, prop) ->
            try
              let param_type = 
                match prop |> member "type" |> to_string with
                | "string" -> Types.SchemaString
                | "number" -> Types.SchemaNumber
                | "integer" -> Types.SchemaInteger
                | "boolean" -> Types.SchemaBoolean
                | "array" -> Types.SchemaArray Types.SchemaString
                | "object" -> Types.SchemaObject
                | _ -> Types.SchemaString
              in
              let param_description = 
                try Some (prop |> member "description" |> to_string)
                with _ -> None
              in
              let param_enum =
                try Some (prop |> member "enum" |> to_list |> List.map to_string)
                with _ -> None
              in
              Some { Types.param_name = name; param_type; param_description;
                     param_required = List.mem name required_list; param_enum }
            with _ -> None
          ) props
        | _ -> []
      in
      let schema = { Types.func_name; func_description; func_parameters } in
      { Types.tool_type = Types.GlmFunction;
        function_schema = Some schema;
        web_search_config = None;
        code_interpreter_config = None }
  | Types.GlmCodeInterpreter ->
      let ci_obj = json |> member "code_interpreter" in
      let sandbox = (try ci_obj |> member "sandbox" |> to_string with _ -> "auto") in
      { Types.tool_type = Types.GlmCodeInterpreter;
        function_schema = None;
        web_search_config = None;
        code_interpreter_config = Some sandbox }

(** Parse tools array from JSON *)
let parse_glm_tools (json : Yojson.Safe.t) : Types.glm_tool list =
  let open Yojson.Safe.Util in
  match json |> member "tools" with
  | `Null -> []
  | `List tools -> List.map parse_glm_tool tools
  | _ -> []

let parse_glm_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"glm-5" in
  let system_prompt = json |> member "system_prompt" |> to_string_option in
  let temperature =
    try json |> member "temperature" |> to_float
    with Type_error _ -> 0.7 in
  let max_tokens =
    try Some (json |> member "max_tokens" |> to_int)
    with Type_error _ -> Some 131072 in  (* GLM-4.7: 200K context, 128K (131072) output max *)
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:default_timeout in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:default_stream in
  let thinking = json |> member "thinking" |> to_bool_option |> Option.value ~default:false in
  let do_sample = json |> member "do_sample" |> to_bool_option |> Option.value ~default:true in
  (* DEPRECATED: web_search bool, use tools instead *)
  let web_search = json |> member "web_search" |> to_bool_option |> Option.value ~default:false in
  (* New: Parse tools array *)
  let tools = parse_glm_tools json in
  let api_key = json |> member "api_key" |> to_string_option in
  Glm { prompt; model; system_prompt; temperature; max_tokens; timeout; stream; thinking; do_sample; web_search; tools; api_key }

(** Parse JSON arguments for glm.translate tool *)
let parse_glm_translate_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let text = json |> member "text" |> to_string in
  let source_lang = json |> member "source_lang" |> to_string in
  let target_lang = json |> member "target_lang" |> to_string in
  let strategy_str = json |> member "strategy" |> to_string_option |> Option.value ~default:"general" in
  let strategy = Types.translation_strategy_of_string_unsafe strategy_str in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"glm-5" in
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:120 in
  GlmTranslate { text; source_lang; target_lang; strategy; model; timeout }

(** Parse JSON arguments for set_stream_delta tool *)
let parse_set_stream_delta_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let enabled = json |> member "enabled" |> to_bool_option |> Option.value ~default:true in
  SetStreamDelta { enabled }

(** Parse JSON arguments for get_stream_delta tool *)
let parse_get_stream_delta_args (_json : Yojson.Safe.t) : tool_args =
  GetStreamDelta

(** Parse JSON arguments for chain.run tool *)
let parse_chain_run_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain =
    match json |> member "chain" with
    | `Null -> None
    | c -> Some c
  in
  let mermaid = json |> member "mermaid" |> to_string_option in
  let chain_id = json |> member "chain_id" |> to_string_option in
  (* Handle input as string, object, or null *)
  let input = match json |> member "input" with
    | `Null -> None
    | `String s -> Some s
    | other -> Some (Yojson.Safe.to_string other)  (* Serialize objects to JSON string *)
  in
  let trace = Safe_parse.json_bool ~context:"chain.run" ~default:false json "trace" in
  let checkpoint_enabled = Safe_parse.json_bool ~context:"chain.run" ~default:false json "checkpoint_enabled" in
  let timeout = Safe_parse.json_int_opt json "timeout" in
  ChainRun { chain; mermaid; chain_id; input; trace; checkpoint_enabled; timeout }

(** Parse JSON arguments for chain.validate tool *)
let parse_chain_validate_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain =
    match json |> member "chain" with
    | `Null -> None
    | c -> Some c
  in
  let mermaid = json |> member "mermaid" |> to_string_option in
  let strict = Safe_parse.json_bool ~context:"chain.validate" ~default:true json "strict" in
  ChainValidate { chain; mermaid; strict }

(** Parse JSON arguments for chain.to_mermaid tool *)
let parse_chain_to_mermaid_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain = json |> member "chain" in
  ChainToMermaid { chain }

(** Parse JSON arguments for chain.visualize tool *)
let parse_chain_visualize_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain = json |> member "chain" in
  ChainVisualize { chain }

(** Parse JSON arguments for chain.convert tool *)
let parse_chain_convert_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let from_format = json |> member "from" |> to_string in
  let to_format = json |> member "to" |> to_string in
  let input = json |> member "input" in
  let pretty = Safe_parse.json_bool ~context:"chain.convert" ~default:true json "pretty" in
  ChainConvert { from_format; to_format; input; pretty }

(** Parse JSON arguments for chain.orchestrate tool *)
let parse_chain_orchestrate_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let goal = json |> member "goal" |> to_string in
  let chain =
    match json |> member "chain" with
    | `Null -> None
    | c -> Some c
  in
  let tasks =
    match json |> member "tasks" with
    | `Null -> None
    | t -> Some t
  in
  let chain_id = json |> member "chain_id" |> to_string_option in
  let max_replans = Safe_parse.json_int ~context:"chain.orchestrate" ~default:3 json "max_replans" in
  let timeout = Safe_parse.json_int ~context:"chain.orchestrate" ~default:600 json "timeout" in
  let trace = Safe_parse.json_bool ~context:"chain.orchestrate" ~default:false json "trace" in
  let verify_on_complete = Safe_parse.json_bool ~context:"chain.orchestrate" ~default:true json "verify_on_complete" in
  let orchestrator_model =
    json |> member "orchestrator_model" |> to_string_option
    |> Option.value ~default:"gemini"
  in
  ChainOrchestrate { goal; chain; tasks; chain_id; max_replans; timeout; trace; verify_on_complete; orchestrator_model }

(** Parse JSON arguments for gh_pr_diff tool *)
let parse_gh_pr_diff_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let repo = json |> member "repo" |> to_string in
  let pr_number = json |> member "pr_number" |> to_int in
  GhPrDiff { repo; pr_number }

(** Parse JSON arguments for slack_post tool *)
let parse_slack_post_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let channel = json |> member "channel" |> to_string in
  let text = json |> member "text" |> to_string in
  let thread_ts = json |> member "thread_ts" |> to_string_option in
  SlackPost { channel; text; thread_ts }

(** Parse JSON arguments for chain.checkpoints tool *)
let parse_chain_checkpoints_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain_id = json |> member "chain_id" |> to_string_option in
  let max_age_hours = json |> member "max_age_hours" |> to_int_option in
  let cleanup = json |> member "cleanup" |> to_bool_option |> Option.value ~default:false in
  ChainCheckpoints { chain_id; max_age_hours; cleanup }

(** Parse JSON arguments for chain.resume tool *)
let parse_chain_resume_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let run_id = json |> member "run_id" |> to_string in
  let trace = json |> member "trace" |> to_bool_option |> Option.value ~default:false in
  ChainResume { run_id; trace }

(** Parse JSON arguments for prompt.register tool *)
let parse_prompt_register_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_string in
  let template = json |> member "template" |> to_string in
  let version = json |> member "version" |> to_string_option in
  PromptRegister { id; template; version }

(** Parse JSON arguments for prompt.get tool *)
let parse_prompt_get_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_string in
  let version = json |> member "version" |> to_string_option in
  PromptGet { id; version }

(** {1 Command Builders} *)

(** Build thinking prompt prefix based on thinking level *)
let thinking_prompt_prefix = function
  | High -> "Think step by step carefully, considering multiple perspectives and edge cases before answering.\n\n"
  | Low -> ""

(** Build Gemini CLI command *)
let build_gemini_cmd args =
  match args with
  | Gemini { prompt; model; yolo; thinking_level; output_format; _ } ->
      let prefix = thinking_prompt_prefix thinking_level in
      let enhanced_prompt = if String.length prefix > 0 then prefix ^ prompt else prompt in
      let model = resolve_gemini_model model in
      let cmd = ["gemini"; "-m"; model; "-p"] in
      let cmd = if yolo then cmd @ ["--yolo"] else cmd in
      let cmd = match output_format with
        | Text -> cmd
        | Json -> cmd @ ["--output-format"; "json"]
        | StreamJson -> cmd @ ["--output-format"; "stream-json"]
      in
      Ok (cmd @ [enhanced_prompt])
  | _ -> Error "Invalid args for Gemini"

let default_me_root () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  Filename.concat home "me"

let find_repo_root me_root =
  match Sys.getenv_opt "LLM_MCP_REPO_ROOT" with
  | Some path -> path
  | None ->
      let candidates =
        [Filename.concat me_root "llm-mcp"; Filename.concat me_root "workspace/llm-mcp"]
      in
      match List.find_opt Sys.file_exists candidates with
      | Some path -> path
      | None ->
          let workspace = Filename.concat me_root "workspace" in
          let rec find_in_workspace entries =
            match entries with
            | [] -> None
            | dir :: rest ->
                let candidate = Filename.concat (Filename.concat workspace dir) "llm-mcp" in
                if Sys.file_exists candidate then Some candidate else find_in_workspace rest
          in
          (match Sys.readdir workspace with
          | entries -> (
              match find_in_workspace (Array.to_list entries) with
              | Some path -> path
              | None -> Filename.concat me_root "workspace/llm-mcp")
          | exception _ -> Filename.concat me_root "workspace/llm-mcp")

(** Build Claude CLI command *)
let build_claude_cmd args =
  match args with
  | Claude { prompt; model; long_context; system_prompt; output_format; allowed_tools; _ } ->
      let me_root = Sys.getenv_opt "ME_ROOT" |> Option.value ~default:(default_me_root ()) in
      let repo_root = find_repo_root me_root in
      let wrapper = Filename.concat repo_root "scripts/claude-wrapper.sh" in
      let cmd = [wrapper; "-p"; "--model"; model] in
      (* --betas context-1m enables 1M context but requires API key (charges apply)
         Only add when: 1) explicitly requested via long_context=true
                        2) ANTHROPIC_API_KEY is set (API key mode, not Claude Max subscription) *)
      let has_api_key = Sys.getenv_opt "ANTHROPIC_API_KEY" |> Option.is_some in
      let cmd = if long_context && has_api_key then
        cmd @ ["--betas"; "context-1m-2025-08-07"]
      else cmd in
      let cmd = cmd @ ["--settings"; {|{"disableAllHooks": true}|}] in
      let cmd = match output_format with
        | Text -> cmd
        | Json -> cmd @ ["--output-format"; "json"]
        | StreamJson -> cmd @ ["--output-format"; "stream-json"]
      in
      let cmd = match system_prompt with
        | Some sp -> cmd @ ["--system-prompt"; sp]
        | None -> cmd
      in
      let cmd = match allowed_tools with
        | [] -> cmd
        | tools -> cmd @ ["--allowed-tools"] @ tools
      in
      Ok (cmd @ [prompt])
  | _ -> Error "Invalid args for Claude"

(** Build Codex CLI command *)
let build_codex_cmd args =
  match args with
  | Codex { prompt; model; reasoning_effort; sandbox; working_directory; _ } ->
      let effort_str = string_of_reasoning_effort reasoning_effort in
      let sandbox_str = string_of_sandbox_policy sandbox in
      let cmd = [
        "codex"; "exec";
        "-m"; model;
        "-c"; Printf.sprintf {|reasoning_effort="%s"|} effort_str;
        "--sandbox"; sandbox_str;
        "--full-auto";
      ] in
      let cmd = match working_directory with
        | Some dir -> cmd @ ["-C"; dir]
        | None -> cmd
      in
      Ok (cmd @ [prompt])
  | _ -> Error "Invalid args for Codex"

(** Convert MCP tool_schema to Ollama tool format *)
let tool_schema_to_ollama_tool (t : Types.tool_schema) : Yojson.Safe.t =
  `Assoc [
    ("type", `String "function");
    ("function", `Assoc [
      ("name", `String t.name);
      ("description", `String t.description);
      ("parameters", t.input_schema);
    ]);
  ]

(** Build Ollama API request using curl *)
let build_ollama_curl_cmd ?(force_stream=None) args =
  match args with
  | Ollama { prompt; model; system_prompt; temperature; stream; tools; _ } ->
      (* Auto-inject few-shot examples based on keywords *)
      let (prompt, _) = Fewshot_injector.inject_safe prompt in
      let stream_val = match force_stream with Some s -> s | None -> stream in
      let (endpoint, json_payload) = match tools with
        | Some tool_list when List.length tool_list > 0 ->
            let messages = match system_prompt with
              | Some sp -> [
                  `Assoc [("role", `String "system"); ("content", `String sp)];
                  `Assoc [("role", `String "user"); ("content", `String prompt)];
                ]
              | None -> [
                  `Assoc [("role", `String "user"); ("content", `String prompt)];
                ]
            in
            let tools_json = `List (List.map tool_schema_to_ollama_tool tool_list) in
            let payload = `Assoc [
              ("model", `String model);
              ("messages", `List messages);
              ("tools", tools_json);
              ("stream", `Bool stream_val);
              ("options", `Assoc [("temperature", `Float temperature)]);
            ] in
            ("/api/chat", Yojson.Safe.to_string payload)
        | _ ->
            let system_field = match system_prompt with
              | Some sp -> Printf.sprintf {|, "system": %s|} (Yojson.Safe.to_string (`String sp))
              | None -> ""
            in
            let payload = Printf.sprintf
              {|{"model": %s, "prompt": %s, "stream": %b, "options": {"temperature": %.1f}%s}|}
              (Yojson.Safe.to_string (`String model))
              (Yojson.Safe.to_string (`String prompt))
              stream_val
              temperature
              system_field
            in
            ("/api/generate", payload)
      in
      let url = "http://localhost:11434" ^ endpoint in
      if stream_val then
        Ok ["curl"; "-sN"; url; "-d"; json_payload]
      else
        Ok ["curl"; "-s"; url; "-d"; json_payload]
  | _ -> Error "Invalid args for Ollama"

(** {1 Response Parsers} *)

(* Re-export from Ollama_parser *)
let parse_ollama_chunk = Ollama_parser.parse_chunk
let parse_ollama_response = Ollama_parser.parse_response

(** Serialize tool_calls to JSON for response extra field *)
let tool_calls_to_json (calls : Ollama_parser.tool_call list) : string =
  let call_to_json (c : Ollama_parser.tool_call) =
    `Assoc [
      ("name", `String c.name);
      ("arguments", `String c.arguments);
    ]
  in
  `List (List.map call_to_json calls) |> Yojson.Safe.to_string

(** Clean Codex output (remove headers) *)
let clean_codex_output output =
  let lines = String.split_on_char '\n' output in
  let string_contains_ci haystack needle =
    let h = String.lowercase_ascii haystack in
    let n = String.lowercase_ascii needle in
    let nlen = String.length n in
    let hlen = String.length h in
    if nlen > hlen then false
    else
      let rec check i =
        if i > hlen - nlen then false
        else if String.sub h i nlen = n then true
        else check (i + 1)
      in
      check 0
  in
  let is_codex_marker line =
    let trimmed = String.trim line in
    String.lowercase_ascii trimmed = "codex"
  in
  let contains_codex line = string_contains_ci line "codex" in
  let is_tokens_line line = string_contains_ci line "tokens used" in
  let rec find_last_codex_marker idx best = function
    | [] -> best
    | line :: rest ->
        if is_codex_marker line then find_last_codex_marker (idx + 1) (Some idx) rest
        else find_last_codex_marker (idx + 1) best rest
  in
  let marker_idx = find_last_codex_marker 0 None lines in
  let start_idx = match marker_idx with
    | Some idx -> idx + 1
    | None ->
        let rec find_first idx = function
          | [] -> None
          | line :: rest ->
              if contains_codex line then Some (idx + 1)
              else find_first (idx + 1) rest
        in
        (match find_first 0 lines with Some i -> i | None -> 0)
  in
  let rec extract idx acc = function
    | [] -> List.rev acc
    | line :: rest ->
        if idx < start_idx then extract (idx + 1) acc rest
        else if is_tokens_line line then List.rev acc
        else extract (idx + 1) (line :: acc) rest
  in
  let result = extract 0 [] lines |> String.concat "\n" |> String.trim in
  if String.length result > 0 then result else output

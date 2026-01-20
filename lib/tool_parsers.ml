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
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:300 in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:true in
  Gemini { prompt; model; thinking_level; yolo; timeout; stream }

(** Parse JSON arguments for Claude tool *)
let parse_claude_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"opus" in
  let budget_mode = budget_mode_value json in
  let ultrathink =
    json |> member "ultrathink" |> to_bool_option
    |> Option.value ~default:(not budget_mode) in
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
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:300 in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:true in
  Claude { prompt; model; ultrathink; system_prompt; output_format; allowed_tools; working_directory; timeout; stream }

(** Parse JSON arguments for Codex tool *)
let parse_codex_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"gpt-5.2" in
  let budget_mode = budget_mode_value json in
  let reasoning_effort =
    json |> member "reasoning_effort" |> to_string_option
    |> Option.value ~default:(if budget_mode then "medium" else "xhigh")
    |> reasoning_effort_of_string in
  let sandbox =
    json |> member "sandbox" |> to_string_option
    |> Option.value ~default:"workspace-write"
    |> sandbox_policy_of_string in
  let working_directory = json |> member "working_directory" |> to_string_option in
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:300 in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:true in
  Codex { prompt; model; reasoning_effort; sandbox; working_directory; timeout; stream }

(** Parse JSON arguments for Ollama (local LLM) tool *)
let parse_ollama_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let prompt = json |> member "prompt" |> to_string in
  let model = json |> member "model" |> to_string_option |> Option.value ~default:"devstral" in
  let system_prompt = json |> member "system_prompt" |> to_string_option in
  let temperature =
    try json |> member "temperature" |> to_float
    with Type_error _ -> 0.7 in
  let timeout = json |> member "timeout" |> to_int_option |> Option.value ~default:300 in
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:true in
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

(** Parse JSON arguments for chain.run tool *)
let[@warning "-32"] parse_chain_run_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain =
    match json |> member "chain" with
    | `Null -> None
    | c -> Some c
  in
  let mermaid = json |> member "mermaid" |> to_string_option in
  let input = json |> member "input" |> to_string_option in
  let trace =
    try json |> member "trace" |> to_bool
    with _ -> false
  in
  ChainRun { chain; mermaid; input; trace }

(** Parse JSON arguments for chain.validate tool *)
let[@warning "-32"] parse_chain_validate_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain =
    match json |> member "chain" with
    | `Null -> None
    | c -> Some c
  in
  let mermaid = json |> member "mermaid" |> to_string_option in
  ChainValidate { chain; mermaid }

(** Parse JSON arguments for chain.to_mermaid tool *)
let parse_chain_to_mermaid_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let chain = json |> member "chain" in
  ChainToMermaid { chain }

(** Parse JSON arguments for chain.orchestrate tool *)
let[@warning "-32"] parse_chain_orchestrate_args (json : Yojson.Safe.t) : tool_args =
  let open Yojson.Safe.Util in
  let goal = json |> member "goal" |> to_string in
  let chain =
    match json |> member "chain" with
    | `Null -> None
    | c -> Some c
  in
  let max_replans =
    try json |> member "max_replans" |> to_int
    with _ -> 3
  in
  let timeout =
    try json |> member "timeout" |> to_int
    with _ -> 600
  in
  let trace =
    try json |> member "trace" |> to_bool
    with _ -> false
  in
  let verify_on_complete =
    try json |> member "verify_on_complete" |> to_bool
    with _ -> true
  in
  ChainOrchestrate { goal; chain; max_replans; timeout; trace; verify_on_complete }

(** {1 Command Builders} *)

(** Build thinking prompt prefix based on thinking level *)
let thinking_prompt_prefix = function
  | High -> "Think step by step carefully, considering multiple perspectives and edge cases before answering.\n\n"
  | Low -> ""

(** Build Gemini CLI command *)
let build_gemini_cmd args =
  match args with
  | Gemini { prompt; model; yolo; thinking_level; _ } ->
      let prefix = thinking_prompt_prefix thinking_level in
      let enhanced_prompt = if String.length prefix > 0 then prefix ^ prompt else prompt in
      let cmd = ["gemini"; "-m"; model] in
      let cmd = if yolo then cmd @ ["--yolo"] else cmd in
      Ok (cmd @ [enhanced_prompt])
  | _ -> Error "Invalid args for Gemini"

(** Build Claude CLI command *)
let build_claude_cmd args =
  match args with
  | Claude { prompt; model; ultrathink; system_prompt; output_format; allowed_tools; _ } ->
      let me_root = Sys.getenv_opt "ME_ROOT" |> Option.value ~default:"/Users/dancer/me" in
      let wrapper = me_root ^ "/features/llm-mcp/scripts/claude-wrapper.sh" in
      let cmd = [wrapper; "-p"; "--model"; model] in
      let cmd = if ultrathink then
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

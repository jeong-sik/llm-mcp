(** MAGI Trinity tool implementations *)

open Types

(* Re-export Tool_config utilities for backward compatibility *)
let budget_mode_value = Tool_config.budget_mode_value

(* Re-export MCP config types and functions from Tool_config *)
type mcp_server_config = Tool_config.mcp_server_config
let get_mcp_server_url = Tool_config.get_mcp_server_url
let get_mcp_server_config = Tool_config.get_mcp_server_config

(* Call an external MCP tool via HTTP *)
let call_external_mcp ~server_name ~tool_name ~arguments ~timeout =
  match get_mcp_server_url server_name with
  | None -> Lwt.return (Printf.sprintf "Error: MCP server '%s' not found or not HTTP type" server_name)
  | Some url ->
      let request_body = `Assoc [
        ("jsonrpc", `String "2.0");
        ("id", `Int 1);
        ("method", `String "tools/call");
        ("params", `Assoc [
          ("name", `String tool_name);
          ("arguments", arguments);
        ]);
      ] |> Yojson.Safe.to_string in

      let open Lwt.Syntax in
      let* result = Cli_runner.run_command ~timeout "curl" [
        "-s"; "-X"; "POST"; url;
        "-H"; "Content-Type: application/json";
        "-H"; "Accept: application/json, text/event-stream";
        "-d"; request_body
      ] in
      match result with
      | Error (Cli_runner.Timeout t) -> Lwt.return (Printf.sprintf "Error: MCP call to %s/%s timed out after %ds" server_name tool_name t)
      | Error (Cli_runner.ProcessError msg) -> Lwt.return (Printf.sprintf "Error: MCP call failed: %s" msg)
      | Ok r ->
          (* Parse SSE response to extract result *)
          let lines = String.split_on_char '\n' r.stdout in
          let data_line = List.find_opt (fun l -> String.length l > 5 && String.sub l 0 5 = "data:") lines in
          match data_line with
          | None -> Lwt.return r.stdout  (* Return raw output if no SSE data *)
          | Some line ->
              let json_str = String.sub line 6 (String.length line - 6) |> String.trim in
              try
                let json = Yojson.Safe.from_string json_str in
                let open Yojson.Safe.Util in
                let content = json |> member "result" |> member "content" in
                match content with
                | `List items ->
                    let texts = List.filter_map (fun item ->
                      match item |> member "type" |> to_string_option with
                      | Some "text" -> item |> member "text" |> to_string_option
                      | _ -> None
                    ) items in
                    Lwt.return (String.concat "\n" texts)
                | _ -> Lwt.return json_str
              with _ -> Lwt.return r.stdout

(* Call an external MCP tool via stdio (subprocess) - SHELL INJECTION SAFE
   Uses direct process spawn with argv array, no shell involved.
   Fixed based on MAGI Trinity decision: Option C (Process module with argv list) *)
let call_stdio_mcp ~server_name ~command ~args ~tool_name ~arguments ~timeout =
  let request_body = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "tools/call");
    ("params", `Assoc [
      ("name", `String tool_name);
      ("arguments", arguments);
    ]);
  ] |> Yojson.Safe.to_string in

  let open Lwt.Syntax in
  (* Use run_command_with_stdin - passes argv directly, NO SHELL *)
  let effective_timeout = min timeout 60 in  (* Cap stdio timeout at 60s *)
  let* result = Cli_runner.run_command_with_stdin
    ~timeout:effective_timeout
    ~stdin_data:request_body
    command
    args
  in
  match result with
  | Error (Cli_runner.Timeout t) ->
      Lwt.return (Printf.sprintf "Error: stdio MCP call to %s/%s timed out after %ds" server_name tool_name t)
  | Error (Cli_runner.ProcessError msg) ->
      Lwt.return (Printf.sprintf "Error: stdio MCP call failed: %s" msg)
  | Ok r ->
      if r.exit_code <> 0 then
        Lwt.return (Printf.sprintf "Error: stdio MCP call exited with code %d: %s" r.exit_code r.stderr)
      else
        (* Parse JSON-RPC response *)
        try
          let json = Yojson.Safe.from_string r.stdout in
          let open Yojson.Safe.Util in
          let content = json |> member "result" |> member "content" in
          match content with
          | `List items ->
              let texts = List.filter_map (fun item ->
                match item |> member "type" |> to_string_option with
                | Some "text" -> item |> member "text" |> to_string_option
                | _ -> None
              ) items in
              Lwt.return (String.concat "\n" texts)
          | _ ->
              (* Try direct result *)
              let result_str = json |> member "result" |> to_string_option in
              Lwt.return (Option.value result_str ~default:r.stdout)
        with _ -> Lwt.return r.stdout

(* Unified MCP call - routes to HTTP or stdio based on server config *)
let call_mcp ~server_name ~tool_name ~arguments ~timeout =
  match get_mcp_server_config server_name with
  | None -> Lwt.return (Printf.sprintf "Error: MCP server '%s' not found in config" server_name)
  | Some config ->
      match config.server_type, config.url, config.command with
      | "http", Some _url, _ ->
          (* Use HTTP endpoint *)
          call_external_mcp ~server_name ~tool_name ~arguments ~timeout
      | _, _, Some cmd ->
          (* Use stdio subprocess *)
          call_stdio_mcp ~server_name ~command:cmd ~args:config.args ~tool_name ~arguments ~timeout
      | _ ->
          Lwt.return (Printf.sprintf "Error: MCP server '%s' has no valid URL or command" server_name)

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
  (* Default stream=true for SSE keepalive - prevents timeout on cold start *)
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
  (* Default stream=true for SSE keepalive - prevents timeout on cold start *)
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
  (* Default stream=true for SSE keepalive - prevents timeout on cold start *)
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
  (* Default stream=true for SSE keepalive - consistent with other tools *)
  let stream = json |> member "stream" |> to_bool_option |> Option.value ~default:true in
  Ollama { prompt; model; system_prompt; temperature; timeout; stream }

let parse_ollama_list_args (_json : Yojson.Safe.t) : tool_args =
  OllamaList

(** Build thinking prompt prefix based on thinking level.

    Since Gemini CLI doesn't support thinkingLevel parameter (Issue #6693 closed as stale),
    we use prompt engineering as a workaround to encourage deeper reasoning.
*)
let thinking_prompt_prefix = function
  | High -> "Think step by step carefully, considering multiple perspectives and edge cases before answering.\n\n"
  | Low -> ""

(** Build Gemini CLI command with thinking prompt workaround.

    NOTE: thinking_level is NOT a native CLI parameter.
    As of Gemini CLI v0.22.5 (2025-01), there is no CLI flag for thinkingLevel.
    This is an API-level parameter only:
    - Gemini 3 models: thinkingLevel = "low" | "high"
    - Gemini 2.5 models: thinkingBudget = 0-24576 tokens

    See: https://ai.google.dev/gemini-api/docs/thinking

    WORKAROUND: Prepend thinking instructions to prompt for "high" thinking level.
*)
let build_gemini_cmd args =
  match args with
  | Gemini { prompt; model; yolo; thinking_level; _ } ->
      (* Apply thinking prompt workaround *)
      let prefix = thinking_prompt_prefix thinking_level in
      let enhanced_prompt = if String.length prefix > 0 then prefix ^ prompt else prompt in
      let cmd = ["gemini"; "-m"; model] in
      let cmd = if yolo then cmd @ ["--yolo"] else cmd in
      cmd @ [enhanced_prompt]
  | _ -> failwith "Invalid args for Gemini"

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
      cmd @ [prompt]
  | _ -> failwith "Invalid args for Claude"

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
      cmd @ [prompt]
  | _ -> failwith "Invalid args for Codex"

(** Build Ollama API request using curl.

    Uses REST API instead of CLI for reliability.
    API supports native system prompt via "system" field.
*)
let build_ollama_curl_cmd ?(force_stream=None) args =
  match args with
  | Ollama { prompt; model; system_prompt; temperature; stream; _ } ->
      (* Build JSON payload for ollama API *)
      let system_field = match system_prompt with
        | Some sp -> Printf.sprintf {|, "system": %s|} (Yojson.Safe.to_string (`String sp))
        | None -> ""
      in
      let stream_val = match force_stream with Some s -> s | None -> stream in
      let json_payload = Printf.sprintf
        {|{"model": %s, "prompt": %s, "stream": %b, "options": {"temperature": %.1f}%s}|}
        (Yojson.Safe.to_string (`String model))
        (Yojson.Safe.to_string (`String prompt))
        stream_val
        temperature
        system_field
      in
      (* Use --no-buffer for streaming to get real-time output *)
      if stream_val then
        ["curl"; "-sN"; "http://localhost:11434/api/generate"; "-d"; json_payload]
      else
        ["curl"; "-s"; "http://localhost:11434/api/generate"; "-d"; json_payload]
  | _ -> failwith "Invalid args for Ollama"

(** Parse ollama API JSON response.
    API returns: {"model": "...", "response": "...", "done": true, ...}
    On error: {"error": "..."}
*)
(** Parse ollama streaming chunk.
    Streaming returns: {"response": "token", "done": false}
    Final chunk: {"response": "", "done": true, "total_duration": ...}
*)
(* Use Ollama_parser for chunk parsing *)
let parse_ollama_chunk = Ollama_parser.parse_chunk

(** Execute ollama with streaming, calling on_token for each token.
    Returns full response when done. *)
let execute_ollama_streaming ~on_token args =
  let open Lwt.Syntax in
  let open Cli_runner in
  (* Build command and get model info based on tool type *)
  let (cmd_list, model_name, extra_base) = match args with
    | Ollama { model; temperature; timeout = _; _ } ->
        (build_ollama_curl_cmd ~force_stream:(Some true) args,
         Printf.sprintf "ollama (%s)" model,
         [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true")])
    | _ -> ([], "unknown", [])
  in
  let timeout = match args with
    | Ollama { timeout; _ } -> timeout
    | _ -> 300
  in
  if cmd_list = [] then
    Lwt.return { model = "unknown"; returncode = -1; response = "Invalid args"; extra = [] }
  else begin
    let cmd = List.hd cmd_list in
    let cmd_args = List.tl cmd_list in
    let full_response = Buffer.create 1024 in
    let on_line line =
      match parse_ollama_chunk line with
      | Ok (token, _done) ->
          Buffer.add_string full_response token;
          on_token token
      | Error _ -> Lwt.return_unit
    in
    let* result = run_streaming_command ~timeout ~on_line cmd cmd_args in
    match result with
    | Ok _ ->
        Lwt.return { model = model_name;
          returncode = 0;
          response = Buffer.contents full_response;
          extra = extra_base @ [("streamed", "true")]; }
    | Error (Timeout t) ->
        Lwt.return { model = model_name;
          returncode = -1;
          response = Printf.sprintf "Timeout after %ds" t;
          extra = extra_base; }
    | Error (ProcessError msg) ->
        Lwt.return { model = model_name;
          returncode = -1;
          response = Printf.sprintf "Error: %s" msg;
          extra = extra_base; }
  end

(* Use Ollama_parser for response parsing *)
let parse_ollama_response = Ollama_parser.parse_response

(** Clean Codex output (remove headers)
    Codex v0.79.0 outputs:
    - "OpenAI Codex v..." header at the top
    - "codex" as a standalone line marking model response start
    - "tokens used" at the end

    Strategy: Find LAST "codex" marker, capture until "tokens used" *)
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
  (* Find if line is exactly "codex" (the response marker) *)
  let is_codex_marker line =
    let trimmed = String.trim line in
    String.lowercase_ascii trimmed = "codex"
  in
  (* Find if line contains "codex" anywhere (for fallback) *)
  let contains_codex line = string_contains_ci line "codex" in
  let is_tokens_line line = string_contains_ci line "tokens used" in

  (* First try: find standalone "codex" marker (preferred) *)
  let rec find_last_codex_marker idx best = function
    | [] -> best
    | line :: rest ->
        if is_codex_marker line then find_last_codex_marker (idx + 1) (Some idx) rest
        else find_last_codex_marker (idx + 1) best rest
  in
  let marker_idx = find_last_codex_marker 0 None lines in

  let start_idx = match marker_idx with
    | Some idx -> idx + 1  (* start after "codex" marker *)
    | None ->
        (* Fallback: find first line containing "codex" (old behavior) *)
        let rec find_first idx = function
          | [] -> None
          | line :: rest ->
              if contains_codex line then Some (idx + 1)
              else find_first (idx + 1) rest
        in
        (match find_first 0 lines with Some i -> i | None -> 0)
  in

  (* Extract lines from start_idx until "tokens used" *)
  let rec extract idx acc = function
    | [] -> List.rev acc
    | line :: rest ->
        if idx < start_idx then extract (idx + 1) acc rest
        else if is_tokens_line line then List.rev acc
        else extract (idx + 1) (line :: acc) rest
  in
  let result = extract 0 [] lines |> String.concat "\n" |> String.trim in
  if String.length result > 0 then result else output

(** Default retry configuration for Gemini errors *)
let default_max_retries = 2
let default_base_delay = 1.0  (* seconds *)

(** Calculate exponential backoff delay: base * 2^attempt *)
let exponential_backoff ~base_delay attempt =
  base_delay *. (Float.pow 2.0 (Float.of_int attempt))

(** Execute Gemini with automatic retry for recoverable errors.

    Implements exponential backoff retry for:
    - FunctionCallSyncError: Known Gemini CLI bug, retry with fresh request
    - RateLimitError: API quota, retry after delay

    Non-recoverable errors (ContextTooLongError, AuthenticationError) are
    returned immediately without retry.

    @param max_retries Maximum retry attempts (default: 2)
    @param base_delay Initial delay in seconds (default: 1.0)
*)
let execute_gemini_with_retry
    ?(max_retries = default_max_retries)
    ?(base_delay = default_base_delay)
    ~model ~thinking_level ~timeout ~args () : tool_result Lwt.t =
  let open Lwt.Syntax in
  let open Cli_runner in

  let cmd_list = build_gemini_cmd args in
  let cmd = List.hd cmd_list in
  let cmd_args = List.tl cmd_list in
  let thinking_applied = thinking_level = High in

  let rec attempt n =
    let* result = run_command ~timeout cmd cmd_args in
    match result with
    | Ok r ->
        let response = get_output r in
        (* Check for Gemini-specific errors in response *)
        (match classify_gemini_error response with
        | Some error when is_recoverable_gemini_error error && n < max_retries ->
            (* Recoverable error - retry with backoff *)
            let delay = exponential_backoff ~base_delay n in
            let _error_name = string_of_gemini_error error in  (* For future logging *)
            let* () = Lwt_unix.sleep delay in
            (* Retry with fresh request *)
            attempt (n + 1)
        | Some error ->
            (* Non-recoverable error or max retries reached *)
            let error_name = string_of_gemini_error error in
            let extra = [
              ("thinking_level", string_of_thinking_level thinking_level);
              ("thinking_prompt_applied", string_of_bool thinking_applied);
              ("gemini_error", error_name);
              ("retry_attempts", string_of_int n);
              ("recoverable", string_of_bool (is_recoverable_gemini_error error));
            ] in
            Lwt.return { model = Printf.sprintf "gemini (%s)" model;
              returncode = (if r.exit_code = 0 then 1 else r.exit_code);  (* Force error code *)
              response;
              extra; }
        | None ->
            (* Success - no error detected *)
            let extra = [
              ("thinking_level", string_of_thinking_level thinking_level);
              ("thinking_prompt_applied", string_of_bool thinking_applied);
            ] @ (if n > 0 then [("retry_attempts", string_of_int n)] else [])
            in
            Lwt.return { model = Printf.sprintf "gemini (%s)" model;
              returncode = r.exit_code;
              response;
              extra; })
    | Error (Timeout t) ->
        let extra = [
          ("thinking_level", string_of_thinking_level thinking_level);
          ("thinking_prompt_applied", string_of_bool thinking_applied);
          ("retry_attempts", string_of_int n);
        ] in
        Lwt.return { model = Printf.sprintf "gemini (%s)" model;
          returncode = -1;
          response = Printf.sprintf "Timeout after %ds" t;
          extra; }
    | Error (ProcessError msg) ->
        let extra = [
          ("thinking_level", string_of_thinking_level thinking_level);
          ("thinking_prompt_applied", string_of_bool thinking_applied);
          ("retry_attempts", string_of_int n);
        ] in
        Lwt.return { model = Printf.sprintf "gemini (%s)" model;
          returncode = -1;
          response = Printf.sprintf "Error: %s" msg;
          extra; }
  in
  attempt 0

(** Execute a tool and return result *)
let execute args : tool_result Lwt.t =
  let open Lwt.Syntax in
  let open Cli_runner in

  match args with
  | Gemini { model; thinking_level; timeout; _ } ->
      (* Use retry-enabled Gemini execution *)
      execute_gemini_with_retry ~model ~thinking_level ~timeout ~args ()

  | Claude { model; ultrathink; working_directory; timeout; _ } ->
      let cmd_list = build_claude_cmd args in
      let cmd = List.hd cmd_list in
      let cmd_args = List.tl cmd_list in
      let* result = run_command ~cwd:working_directory ~safe_tmpdir:true ~timeout cmd cmd_args in
      (match result with
      | Ok r ->
          Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
            returncode = r.exit_code;
            response = get_output r;
            extra = [("ultrathink", string_of_bool ultrathink)]; }
      | Error (Timeout t) ->
          Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
            returncode = -1;
            response = Printf.sprintf "Timeout after %ds" t;
            extra = [("ultrathink", string_of_bool ultrathink)]; }
      | Error (ProcessError msg) ->
          Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
            returncode = -1;
            response = Printf.sprintf "Error: %s" msg;
            extra = [("ultrathink", string_of_bool ultrathink)]; })

  | Codex { model; reasoning_effort; timeout; _ } ->
      let cmd_list = build_codex_cmd args in
      let cmd = List.hd cmd_list in
      let cmd_args = List.tl cmd_list in
      let* result = run_command ~timeout cmd cmd_args in
      (match result with
      | Ok r ->
          Lwt.return { model = Printf.sprintf "codex (%s)" model;
            returncode = r.exit_code;
            response = clean_codex_output (get_output r);
            extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; }
      | Error (Timeout t) ->
          Lwt.return { model = Printf.sprintf "codex (%s)" model;
            returncode = -1;
            response = Printf.sprintf "Timeout after %ds" t;
            extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; }
      | Error (ProcessError msg) ->
          Lwt.return { model = Printf.sprintf "codex (%s)" model;
            returncode = -1;
            response = Printf.sprintf "Error: %s" msg;
            extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; })

  | Ollama { model; temperature; timeout; _ } ->
      (* Use REST API via curl for reliability *)
      let cmd_list = build_ollama_curl_cmd args in
      let cmd = List.hd cmd_list in
      let cmd_args = List.tl cmd_list in
      let* result = run_command ~timeout cmd cmd_args in
      (match result with
      | Ok r ->
          let response = match parse_ollama_response r.stdout with
            | Ok resp -> resp
            | Error err -> Printf.sprintf "Error: %s" err
          in
          let returncode = if String.length response > 0 && not (String.sub response 0 (min 6 (String.length response)) = "Error:") then 0 else -1 in
          Lwt.return { model = Printf.sprintf "ollama (%s)" model;
            returncode;
            response;
            extra = [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true")]; }
      | Error (Timeout t) ->
          Lwt.return { model = Printf.sprintf "ollama (%s)" model;
            returncode = -1;
            response = Printf.sprintf "Timeout after %ds" t;
            extra = [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true")]; }
      | Error (ProcessError msg) ->
          Lwt.return { model = Printf.sprintf "ollama (%s)" model;
            returncode = -1;
            response = Printf.sprintf "Error: %s" msg;
            extra = [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true")]; })

  | OllamaList ->
      (* Run ollama list and parse output to JSON *)
      let cmd = "ollama" in
      let cmd_args = ["list"] in
      let* result = run_command ~timeout:30 cmd cmd_args in
      (match result with
      | Ok r ->
          (* Parse ollama list output into JSON array *)
          let lines = String.split_on_char '\n' r.stdout in
          let models = lines
            |> List.filter (fun line -> String.length line > 0)
            |> List.tl  (* Skip header line: "NAME ID SIZE MODIFIED" *)
            |> List.filter_map (fun line ->
                (* Parse: "model:tag    id    size    modified" *)
                let parts = String.split_on_char '\t' line in
                let parts = List.concat_map (String.split_on_char ' ') parts in
                let parts = List.filter (fun s -> String.length s > 0) parts in
                match parts with
                | name :: _id :: size :: size_unit :: rest ->
                    let modified = String.concat " " rest in
                    Some (`Assoc [
                      ("name", `String name);
                      ("size", `String (size ^ " " ^ size_unit));
                      ("modified", `String modified);
                    ])
                | _ -> None)
          in
          let json = `List models in
          Lwt.return { model = "ollama_list";
            returncode = 0;
            response = Yojson.Safe.to_string json;
            extra = [("count", string_of_int (List.length models))]; }
      | Error (Timeout t) ->
          Lwt.return { model = "ollama_list";
            returncode = -1;
            response = Printf.sprintf "Timeout after %ds" t;
            extra = []; }
      | Error (ProcessError msg) ->
          Lwt.return { model = "ollama_list";
            returncode = -1;
            response = Printf.sprintf "Error: %s" msg;
            extra = []; })

(** Execute a tool and format result based on response_format.

    This is the main entry point for LLM-to-LLM communication where
    token efficiency matters. The format parameter controls output:
    - Verbose: Full JSON (human readable, for debugging)
    - Compact: DSL format "RES|OK|G3|150|result" (~70% token savings)
    - Binary: Base64-encoded compact (for high-volume scenarios)
*)
let execute_formatted ~(format : response_format) args : string Lwt.t =
  let open Lwt.Syntax in
  let* result = execute args in
  Lwt.return (format_tool_result ~format result)

(** Execute with default Verbose format (backwards compatible) *)
let execute_verbose args : string Lwt.t =
  execute_formatted ~format:Verbose args

(** Execute with Compact format (for MAGI inter-agent communication) *)
let execute_compact args : string Lwt.t =
  execute_formatted ~format:Compact args

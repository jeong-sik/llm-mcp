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

(** {1 Re-exports from Tool_parsers (Pure, no Lwt)} *)

(* Pure functions - from Tool_parsers module *)
let parse_gemini_args = Tool_parsers.parse_gemini_args
let parse_claude_args = Tool_parsers.parse_claude_args
let parse_codex_args = Tool_parsers.parse_codex_args
let parse_ollama_args = Tool_parsers.parse_ollama_args
let parse_ollama_list_args = Tool_parsers.parse_ollama_list_args
let build_gemini_cmd = Tool_parsers.build_gemini_cmd
let build_claude_cmd = Tool_parsers.build_claude_cmd
let build_codex_cmd = Tool_parsers.build_codex_cmd
let build_ollama_curl_cmd = Tool_parsers.build_ollama_curl_cmd
let parse_ollama_response = Tool_parsers.parse_ollama_response
let parse_ollama_chunk = Tool_parsers.parse_ollama_chunk
let clean_codex_output = Tool_parsers.clean_codex_output

(* Ollama helpers *)
let thinking_prompt_prefix = Tool_parsers.thinking_prompt_prefix
let tool_schema_to_ollama_tool = Tool_parsers.tool_schema_to_ollama_tool
let tool_calls_to_json = Tool_parsers.tool_calls_to_json

(* Gemini error handling - from Types module *)
let classify_gemini_error = Types.classify_gemini_error
let is_recoverable_gemini_error = Types.is_recoverable_gemini_error
let string_of_gemini_error = Types.string_of_gemini_error

let execute_ollama_streaming ~on_token args =
  let open Lwt.Syntax in
  let open Cli_runner in
  (* Build command and get model info based on tool type *)
  let (cmd_list, model_name, extra_base, has_tools, err_msg) = match args with
    | Ollama { model; temperature; tools; timeout = _; _ } ->
        let has_tools = match tools with Some l when List.length l > 0 -> true | _ -> false in
        (build_ollama_curl_cmd ~force_stream:(Some true) args,
         Printf.sprintf "ollama (%s)" model,
         [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true")],
         has_tools,
         None)
    | _ -> (Error "Invalid args for Ollama", "unknown", [], false, Some "Invalid args for Ollama")
  in
  let timeout = match args with
    | Ollama { timeout; _ } -> timeout
    | _ -> 300
  in
  match cmd_list with
  | Error err ->
      let response = Option.value err_msg ~default:err in
      Lwt.return { model = model_name; returncode = -1; response; extra = extra_base }
  | Ok cmd_list ->
      if cmd_list = [] then
        Lwt.return { model = model_name; returncode = -1; response = "Invalid args"; extra = extra_base }
      else begin
        let cmd = List.hd cmd_list in
        let cmd_args = List.tl cmd_list in
        let full_response = Buffer.create 1024 in
        let accumulated_tool_calls = ref [] in
        let on_line line =
          if has_tools then
            (* Parse chat API response (with tools support) *)
            match Ollama_parser.parse_chat_chunk line with
            | Ok (token, tool_calls, _done) ->
                Buffer.add_string full_response token;
                if tool_calls <> [] then accumulated_tool_calls := tool_calls;
                on_token token
            | Error _ -> Lwt.return_unit
          else
            (* Parse generate API response (no tools) *)
            match parse_ollama_chunk line with
            | Ok (token, _done) ->
                Buffer.add_string full_response token;
                on_token token
            | Error _ -> Lwt.return_unit
        in
        let* result = run_streaming_command ~timeout ~on_line cmd cmd_args in
        match result with
        | Ok _ ->
            let extra = extra_base @ [("streamed", "true")] in
            let extra = if !accumulated_tool_calls <> [] then
              extra @ [("tool_calls", tool_calls_to_json !accumulated_tool_calls)]
            else extra in
            Lwt.return { model = model_name;
              returncode = 0;
              response = Buffer.contents full_response;
              extra; }
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
(** {1 Lwt-specific execution functions} *)

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

  let thinking_applied = thinking_level = High in

  match build_gemini_cmd args with
  | Error err ->
      let extra = [
        ("thinking_level", string_of_thinking_level thinking_level);
        ("thinking_prompt_applied", string_of_bool thinking_applied);
        ("invalid_args", "true");
      ] in
      Lwt.return { model = Printf.sprintf "gemini (%s)" model;
        returncode = -1;
        response = err;
        extra; }
  | Ok cmd_list ->
      let cmd = List.hd cmd_list in
      let cmd_args = List.tl cmd_list in
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
      (match build_claude_cmd args with
      | Error err ->
          Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
            returncode = -1;
            response = err;
            extra = [("ultrathink", string_of_bool ultrathink); ("invalid_args", "true")]; }
      | Ok cmd_list ->
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
                extra = [("ultrathink", string_of_bool ultrathink)]; }))

  | Codex { model; reasoning_effort; timeout; _ } ->
      (match build_codex_cmd args with
      | Error err ->
          Lwt.return { model = Printf.sprintf "codex (%s)" model;
            returncode = -1;
            response = err;
            extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort); ("invalid_args", "true")]; }
      | Ok cmd_list ->
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
                extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; }))

  | Ollama { model; temperature; timeout; _ } ->
      (* Use REST API via curl for reliability *)
      (match build_ollama_curl_cmd args with
      | Error err ->
          Lwt.return { model = Printf.sprintf "ollama (%s)" model;
            returncode = -1;
            response = err;
            extra = [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true"); ("invalid_args", "true")]; }
      | Ok cmd_list ->
          let cmd = List.hd cmd_list in
          let cmd_args = List.tl cmd_list in
          let* result = run_command ~timeout cmd cmd_args in
          (match result with
          | Ok r ->
              let response = match parse_ollama_response r.stdout with
                | Ok resp -> resp
                | Error err -> Printf.sprintf "Error: %s" err
              in
              let returncode =
                if String.length response > 0 && not (String.sub response 0 (min 6 (String.length response)) = "Error:")
                then 0 else -1
              in
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
                extra = [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true")]; }))

  | OllamaList ->
      (* Run ollama list and parse output to JSON *)
      let cmd = "ollama" in
      let cmd_args = ["list"] in
      let* result = run_command ~timeout:30 cmd cmd_args in
      (match result with
      | Ok r ->
          (* Parse ollama list output into JSON array *)
          let lines =
            String.split_on_char '\n' r.stdout
            |> List.filter (fun line -> String.length (String.trim line) > 0)
          in
          let is_header line =
            let upper = String.uppercase_ascii (String.trim line) in
            String.length upper >= 4 && String.sub upper 0 4 = "NAME"
          in
          let data_lines = match lines with
            | [] -> []
            | first :: rest -> if is_header first then rest else lines
          in
          let models = data_lines
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

(** Execute Ollama with agentic tool calling loop.

    When tools are provided, this uses Agent_loop to:
    1. Send prompt to Ollama with tool definitions
    2. Execute any tool_calls from Ollama's response
    3. Send tool results back to Ollama
    4. Repeat until no more tool_calls or max_turns reached

    This enables tool-capable models (devstral, qwen3, llama3.3)
    to use MCP tools natively through llm-mcp.

    @param tools MCP tool schemas for function calling
    @param external_mcp_url URL of external MCP server for tool execution
    @param on_turn Callback for each turn (turn number, prompt)
*)
let execute_ollama_agentic
    ~(tools : Types.tool_schema list)
    ?(external_mcp_url : string option = None)
    ?(on_turn : int -> string -> unit Lwt.t = fun _ _ -> Lwt.return_unit)
    args : tool_result Lwt.t =
  let open Lwt.Syntax in
  match args with
  | Ollama { model; prompt; system_prompt; temperature; timeout = _; stream = _; tools = _ } ->
      let system = match system_prompt with
        | Some s -> s
        | None -> "You are a helpful assistant with access to tools."
      in
      let* result = Agent_loop.run
        ~model
        ~prompt
        ~system_prompt:system
        ~temperature
        ~max_turns:10
        ~external_mcp_url
        ~tools
        ~on_turn
        ()
      in
      (match result with
      | Ok response ->
          Lwt.return {
            model = Printf.sprintf "ollama (%s) [agentic]" model;
            returncode = 0;
            response;
            extra = [
              ("temperature", Printf.sprintf "%.1f" temperature);
              ("local", "true");
              ("agentic", "true");
              ("tools_count", string_of_int (List.length tools));
            ];
          }
      | Error err ->
          Lwt.return {
            model = Printf.sprintf "ollama (%s) [agentic]" model;
            returncode = -1;
            response = Printf.sprintf "Agent loop error: %s" err;
            extra = [
              ("temperature", Printf.sprintf "%.1f" temperature);
              ("local", "true");
              ("agentic", "true");
              ("error", "true");
            ];
          })
  | _ ->
      (* Non-Ollama args: fall back to regular execute *)
      execute args

(** MAGI Trinity tool implementations - Eio Direct-Style

    Pure Eio version of tools.ml using:
    - Direct-style (no monadic bind)
    - Cli_runner_eio for subprocess execution
    - Eio.Time for delays
    - Structured concurrency

    Re-exports pure functions from tools.ml (parse_*, build_*, etc.)
*)

open Printf
open Types
open Cli_runner_eio

(** {1 Re-exports from Tools} *)

(* Pure functions - no Lwt, just re-export *)
let budget_mode_value = Tool_config.budget_mode_value
let parse_gemini_args = Tools.parse_gemini_args
let parse_claude_args = Tools.parse_claude_args
let parse_codex_args = Tools.parse_codex_args
let parse_ollama_args = Tools.parse_ollama_args
let build_gemini_cmd = Tools.build_gemini_cmd
let build_claude_cmd = Tools.build_claude_cmd
let build_codex_cmd = Tools.build_codex_cmd
let build_ollama_curl_cmd = Tools.build_ollama_curl_cmd
let parse_ollama_response = Ollama_parser.parse_response
let clean_codex_output = Tools.clean_codex_output

(* Gemini error handling - from Types module *)
let classify_gemini_error = Types.classify_gemini_error
let is_recoverable_gemini_error = Types.is_recoverable_gemini_error
let string_of_gemini_error = Types.string_of_gemini_error

(** {1 MCP Client Calls - Direct Style} *)

(** Call an external MCP tool via HTTP using curl subprocess *)
let call_external_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments ~timeout =
  match Tool_config.get_mcp_server_url server_name with
  | None -> sprintf "Error: MCP server '%s' not found or not HTTP type" server_name
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

      let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" [
        "-s"; "-X"; "POST"; url;
        "-H"; "Content-Type: application/json";
        "-H"; "Accept: application/json, text/event-stream";
        "-d"; request_body
      ] in
      match result with
      | Error (Timeout t) -> sprintf "Error: MCP call to %s/%s timed out after %ds" server_name tool_name t
      | Error (ProcessError msg) -> sprintf "Error: MCP call failed: %s" msg
      | Ok r ->
          (* Parse SSE response to extract result *)
          let lines = String.split_on_char '\n' r.stdout in
          let data_line = List.find_opt (fun l -> String.length l > 5 && String.sub l 0 5 = "data:") lines in
          match data_line with
          | None -> r.stdout  (* Return raw output if no SSE data *)
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
                    String.concat "\n" texts
                | _ -> json_str
              with _ -> r.stdout

(** Call MCP via stdio subprocess - shell injection safe *)
let call_stdio_mcp ~sw ~proc_mgr ~clock ~server_name ~command ~args ~tool_name ~arguments ~timeout =
  let request_body = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "tools/call");
    ("params", `Assoc [
      ("name", `String tool_name);
      ("arguments", arguments);
    ]);
  ] |> Yojson.Safe.to_string in

  let effective_timeout = min timeout 60 in
  let result = run_command_with_stdin ~sw ~proc_mgr ~clock
    ~timeout:effective_timeout
    ~stdin_data:request_body
    command args
  in
  match result with
  | Error (Timeout t) ->
      sprintf "Error: stdio MCP call to %s/%s timed out after %ds" server_name tool_name t
  | Error (ProcessError msg) ->
      sprintf "Error: stdio MCP call failed: %s" msg
  | Ok r ->
      if r.exit_code <> 0 then
        sprintf "Error: stdio MCP call exited with code %d: %s" r.exit_code r.stderr
      else
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
              String.concat "\n" texts
          | _ ->
              let result_str = json |> member "result" |> to_string_option in
              Option.value result_str ~default:r.stdout
        with _ -> r.stdout

(** Unified MCP call dispatcher *)
let call_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments ~timeout =
  match Tool_config.get_mcp_server_config server_name with
  | None -> sprintf "Error: MCP server '%s' not found in config" server_name
  | Some config ->
      match config.url, config.command with
      | Some _, _ ->
          call_external_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments ~timeout
      | None, Some cmd ->
          call_stdio_mcp ~sw ~proc_mgr ~clock ~server_name ~command:cmd ~args:config.args ~tool_name ~arguments ~timeout
      | None, None ->
          sprintf "Error: MCP server '%s' has no valid URL or command" server_name

(** {1 Streaming Execution} *)

(** Execute Ollama with token streaming callback *)
let execute_ollama_streaming ~sw ~proc_mgr ~clock ~on_token args =
  let (cmd_result, model_name, extra_base, has_tools, err_msg) = match args with
    | Ollama { model; temperature; tools; timeout = _; _ } ->
        let has_tools = match tools with Some l when List.length l > 0 -> true | _ -> false in
        (build_ollama_curl_cmd ~force_stream:(Some true) args,
         sprintf "ollama (%s)" model,
         [("temperature", sprintf "%.1f" temperature); ("local", "true")],
         has_tools,
         None)
    | _ -> (Error "Invalid args for Ollama", "unknown", [], false, Some "Invalid args for Ollama")
  in
  let timeout = match args with
    | Ollama { timeout; _ } -> timeout
    | _ -> 300
  in
  match cmd_result with
  | Error err ->
      let response = Option.value err_msg ~default:err in
      { model = model_name; returncode = -1; response; extra = extra_base }
  | Ok cmd_list ->
      if cmd_list = [] then
        { model = model_name; returncode = -1; response = "Invalid args"; extra = extra_base }
      else begin
        let cmd = List.hd cmd_list in
        let cmd_args = List.tl cmd_list in
        let full_response = Buffer.create 1024 in
        let accumulated_tool_calls = ref [] in
        let on_line line =
          if has_tools then
            match Ollama_parser.parse_chat_chunk line with
            | Ok (token, tool_calls, _done) ->
                Buffer.add_string full_response token;
                if tool_calls <> [] then accumulated_tool_calls := tool_calls;
                on_token token
            | Error _ -> ()
          else
            match Tools.parse_ollama_chunk line with
            | Ok (token, _done) ->
                Buffer.add_string full_response token;
                on_token token
            | Error _ -> ()
        in
        let result = run_streaming_command ~sw ~proc_mgr ~clock ~timeout ~on_line cmd cmd_args in
        match result with
        | Ok _ ->
            let extra = extra_base @ [("streamed", "true")] in
            let extra = if !accumulated_tool_calls <> [] then
              extra @ [("tool_calls", Tools.tool_calls_to_json !accumulated_tool_calls)]
            else extra in
            { model = model_name;
              returncode = 0;
              response = Buffer.contents full_response;
              extra; }
        | Error (Timeout t) ->
            { model = model_name;
              returncode = -1;
              response = sprintf "Timeout after %ds" t;
              extra = extra_base; }
        | Error (ProcessError msg) ->
            { model = model_name;
              returncode = -1;
              response = sprintf "Error: %s" msg;
              extra = extra_base; }
      end

(** {1 Gemini with Retry} *)

let default_max_retries = 2
let default_base_delay = 1.0

let exponential_backoff ~base_delay attempt =
  base_delay *. (Float.pow 2.0 (Float.of_int attempt))

(** Execute Gemini with automatic retry for recoverable errors *)
let execute_gemini_with_retry ~sw ~proc_mgr ~clock
    ?(max_retries = default_max_retries)
    ?(base_delay = default_base_delay)
    ~model ~thinking_level ~timeout ~args () =

  let thinking_applied = thinking_level = High in

  match build_gemini_cmd args with
  | Error err ->
      let extra = [
        ("thinking_level", string_of_thinking_level thinking_level);
        ("thinking_prompt_applied", string_of_bool thinking_applied);
        ("invalid_args", "true");
      ] in
      { model = sprintf "gemini (%s)" model;
        returncode = -1;
        response = err;
        extra; }
  | Ok cmd_list ->
      let cmd = List.hd cmd_list in
      let cmd_args = List.tl cmd_list in
      let rec attempt n =
        let result = run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args in
        match result with
        | Ok r ->
            let response = get_output r in
            (match classify_gemini_error response with
            | Some error when is_recoverable_gemini_error error && n < max_retries ->
                (* Recoverable error - retry with backoff *)
                let delay = exponential_backoff ~base_delay n in
                Eio.Time.sleep clock delay;
                attempt (n + 1)
            | Some error ->
                (* Non-recoverable error or max retries reached *)
                let extra = [
                  ("thinking_level", string_of_thinking_level thinking_level);
                  ("thinking_prompt_applied", string_of_bool thinking_applied);
                  ("error_type", string_of_gemini_error error);
                  ("retry_attempts", string_of_int n);
                ] in
                { model = sprintf "gemini (%s)" model;
                  returncode = -1;
                  response;
                  extra; }
            | None ->
                (* Success *)
                let extra = [
                  ("thinking_level", string_of_thinking_level thinking_level);
                  ("thinking_prompt_applied", string_of_bool thinking_applied);
                  ("retry_attempts", string_of_int n);
                ] in
                { model = sprintf "gemini (%s)" model;
                  returncode = r.exit_code;
                  response;
                  extra; })
        | Error (Timeout t) ->
            let extra = [
              ("thinking_level", string_of_thinking_level thinking_level);
              ("thinking_prompt_applied", string_of_bool thinking_applied);
              ("retry_attempts", string_of_int n);
            ] in
            { model = sprintf "gemini (%s)" model;
              returncode = -1;
              response = sprintf "Timeout after %ds" t;
              extra; }
        | Error (ProcessError msg) ->
            let extra = [
              ("thinking_level", string_of_thinking_level thinking_level);
              ("thinking_prompt_applied", string_of_bool thinking_applied);
              ("retry_attempts", string_of_int n);
            ] in
            { model = sprintf "gemini (%s)" model;
              returncode = -1;
              response = sprintf "Error: %s" msg;
              extra; }
      in
      attempt 0

(** {1 Main Execute Function} *)

(** Execute a tool and return result - Direct Style *)
let execute ~sw ~proc_mgr ~clock args : tool_result =
  match args with
  | Gemini { model; thinking_level; timeout; _ } ->
      execute_gemini_with_retry ~sw ~proc_mgr ~clock ~model ~thinking_level ~timeout ~args ()

  | Claude { model; ultrathink; working_directory = _; timeout; _ } ->
      (* Note: working_directory support requires fs from env - use execute_with_env for cwd support *)
      (match build_claude_cmd args with
      | Error err ->
          { model = sprintf "claude-cli (%s)" model;
            returncode = -1;
            response = err;
            extra = [("ultrathink", string_of_bool ultrathink); ("invalid_args", "true")]; }
      | Ok cmd_list ->
          let cmd = List.hd cmd_list in
          let cmd_args = List.tl cmd_list in
          let result = run_command ~sw ~proc_mgr ~clock ~safe_tmpdir:true ~timeout cmd cmd_args in
          match result with
          | Ok r ->
              { model = sprintf "claude-cli (%s)" model;
                returncode = r.exit_code;
                response = get_output r;
                extra = [("ultrathink", string_of_bool ultrathink)]; }
          | Error (Timeout t) ->
              { model = sprintf "claude-cli (%s)" model;
                returncode = -1;
                response = sprintf "Timeout after %ds" t;
                extra = [("ultrathink", string_of_bool ultrathink)]; }
          | Error (ProcessError msg) ->
              { model = sprintf "claude-cli (%s)" model;
                returncode = -1;
                response = sprintf "Error: %s" msg;
                extra = [("ultrathink", string_of_bool ultrathink)]; })

  | Codex { model; reasoning_effort; timeout; _ } ->
      (match build_codex_cmd args with
      | Error err ->
          { model = sprintf "codex (%s)" model;
            returncode = -1;
            response = err;
            extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort); ("invalid_args", "true")]; }
      | Ok cmd_list ->
          let cmd = List.hd cmd_list in
          let cmd_args = List.tl cmd_list in
          let result = run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args in
          match result with
          | Ok r ->
              { model = sprintf "codex (%s)" model;
                returncode = r.exit_code;
                response = clean_codex_output (get_output r);
                extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; }
          | Error (Timeout t) ->
              { model = sprintf "codex (%s)" model;
                returncode = -1;
                response = sprintf "Timeout after %ds" t;
                extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; }
          | Error (ProcessError msg) ->
              { model = sprintf "codex (%s)" model;
                returncode = -1;
                response = sprintf "Error: %s" msg;
                extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; })

  | Ollama { model; temperature; timeout; _ } ->
      (match build_ollama_curl_cmd args with
      | Error err ->
          { model = sprintf "ollama (%s)" model;
            returncode = -1;
            response = err;
            extra = [("temperature", sprintf "%.1f" temperature); ("local", "true"); ("invalid_args", "true")]; }
      | Ok cmd_list ->
          let cmd = List.hd cmd_list in
          let cmd_args = List.tl cmd_list in
          let result = run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args in
          match result with
          | Ok r ->
              let response = match parse_ollama_response r.stdout with
                | Ok resp -> resp
                | Error err -> sprintf "Error: %s" err
              in
              let returncode =
                if String.length response > 0 && not (String.sub response 0 (min 6 (String.length response)) = "Error:")
                then 0 else -1
              in
              { model = sprintf "ollama (%s)" model;
                returncode;
                response;
                extra = [("temperature", sprintf "%.1f" temperature); ("local", "true")]; }
          | Error (Timeout t) ->
              { model = sprintf "ollama (%s)" model;
                returncode = -1;
                response = sprintf "Timeout after %ds" t;
                extra = [("temperature", sprintf "%.1f" temperature); ("local", "true")]; }
          | Error (ProcessError msg) ->
              { model = sprintf "ollama (%s)" model;
                returncode = -1;
                response = sprintf "Error: %s" msg;
                extra = [("temperature", sprintf "%.1f" temperature); ("local", "true")]; })

  | OllamaList ->
      let cmd = "ollama" in
      let cmd_args = ["list"] in
      let result = run_command ~sw ~proc_mgr ~clock ~timeout:30 cmd cmd_args in
      (match result with
      | Ok r ->
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
          let response = `List models |> Yojson.Safe.to_string in
          { model = "ollama_list";
            returncode = 0;
            response;
            extra = [("local", "true"); ("count", string_of_int (List.length models))]; }
      | Error (Timeout t) ->
          { model = "ollama_list";
            returncode = -1;
            response = sprintf "Timeout after %ds" t;
            extra = [("local", "true")]; }
      | Error (ProcessError msg) ->
          { model = "ollama_list";
            returncode = -1;
            response = sprintf "Error: %s" msg;
            extra = [("local", "true")]; })

(** {1 Convenience Wrappers} *)

(** Execute with Eio env (extracts proc_mgr and clock) *)
let execute_with_env ~sw ~env args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  execute ~sw ~proc_mgr ~clock args

(** Execute Ollama streaming with Eio env *)
let execute_ollama_streaming_with_env ~sw ~env ~on_token args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  execute_ollama_streaming ~sw ~proc_mgr ~clock ~on_token args

(** Call MCP with Eio env *)
let call_mcp_with_env ~sw ~env ~server_name ~tool_name ~arguments ~timeout =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  call_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments ~timeout

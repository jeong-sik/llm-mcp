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

(** {1 Re-exports from Tool_parsers (Pure, no Lwt)} *)

(* Pure functions - from Tool_parsers module *)
let budget_mode_value = Tool_parsers.budget_mode_value
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

(* MCP config helpers *)
let get_mcp_server_url = Tool_config.get_mcp_server_url
let get_mcp_server_config = Tool_config.get_mcp_server_config

(* Ollama helpers *)
let thinking_prompt_prefix = Tool_parsers.thinking_prompt_prefix
let tool_schema_to_ollama_tool = Tool_parsers.tool_schema_to_ollama_tool
let tool_calls_to_json = Tool_parsers.tool_calls_to_json

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
            match Tool_parsers.parse_ollama_chunk line with
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
              extra @ [("tool_calls", Tool_parsers.tool_calls_to_json !accumulated_tool_calls)]
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

(** {1 Format-Aware Execution} *)

(** Execute a tool and format result based on response_format.

    This is the main entry point for LLM-to-LLM communication where
    token efficiency matters. The format parameter controls output:
    - Verbose: Full JSON (human readable, for debugging)
    - Compact: DSL format "RES|OK|G3|150|result" (~70% token savings)
    - Binary: Base64-encoded compact (for high-volume scenarios)
*)
let execute_formatted ~sw ~proc_mgr ~clock ~(format : response_format) args : string =
  let result = execute ~sw ~proc_mgr ~clock args in
  format_tool_result ~format result

(** Execute with default Verbose format (backwards compatible) *)
let execute_verbose ~sw ~proc_mgr ~clock args : string =
  execute_formatted ~sw ~proc_mgr ~clock ~format:Verbose args

(** Execute with Compact format (for MAGI inter-agent communication) *)
let execute_compact ~sw ~proc_mgr ~clock args : string =
  execute_formatted ~sw ~proc_mgr ~clock ~format:Compact args

(** Convenience wrappers with Eio env *)
let execute_formatted_with_env ~sw ~env ~format args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  execute_formatted ~sw ~proc_mgr ~clock ~format args

let execute_verbose_with_env ~sw ~env args =
  execute_formatted_with_env ~sw ~env ~format:Verbose args

let execute_compact_with_env ~sw ~env args =
  execute_formatted_with_env ~sw ~env ~format:Compact args

(** {1 Ollama Agentic Execution} *)

(** Ollama API endpoint *)
let ollama_base_url = "http://127.0.0.1:11434"

(** Conversation message type for agentic loop *)
type agent_message = {
  role : string;
  content : string;
  tool_calls : Ollama_parser.tool_call list option;
  name : string option;
}

(** Convert message to JSON for Ollama API *)
let agent_message_to_json msg =
  let base = [
    ("role", `String msg.role);
    ("content", `String msg.content);
  ] in
  let with_name = match msg.name with
    | Some n -> base @ [("name", `String n)]
    | None -> base
  in
  `Assoc with_name

(** Build Ollama chat request *)
let build_agentic_request ~model ~temperature ~tools messages =
  `Assoc [
    ("model", `String model);
    ("messages", `List (List.map agent_message_to_json messages));
    ("stream", `Bool false);
    ("options", `Assoc [
      ("temperature", `Float temperature);
    ]);
    ("tools", `List tools);
  ]

(** Call Ollama chat API - Eio version using curl subprocess *)
let call_ollama_chat_eio ~sw ~proc_mgr ~clock ~timeout request_json =
  let body = Yojson.Safe.to_string request_json in
  let url = ollama_base_url ^ "/api/chat" in
  let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" [
    "-s"; "-X"; "POST"; url;
    "-H"; "Content-Type: application/json";
    "-d"; body
  ] in
  match result with
  | Error (Timeout t) -> Error (sprintf "Timeout after %ds" t)
  | Error (ProcessError msg) -> Error (sprintf "Connection error: %s" msg)
  | Ok r ->
      if r.exit_code <> 0 then
        Error (sprintf "curl failed with exit code %d: %s" r.exit_code r.stderr)
      else
        Ok r.stdout

(** Execute a single tool call - calls external MCP if configured *)
let execute_tool_call_eio ~sw ~proc_mgr ~clock ~timeout ~external_mcp_url (tc : Ollama_parser.tool_call) =
  let args = try Yojson.Safe.from_string tc.arguments with _ -> `Null in
  match external_mcp_url with
  | Some url ->
      (* Call external MCP server *)
      let request_body = `Assoc [
        ("jsonrpc", `String "2.0");
        ("id", `Int 1);
        ("method", `String "tools/call");
        ("params", `Assoc [
          ("name", `String tc.name);
          ("arguments", args);
        ]);
      ] |> Yojson.Safe.to_string in
      let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" [
        "-s"; "-X"; "POST"; url;
        "-H"; "Content-Type: application/json";
        "-d"; request_body
      ] in
      (match result with
      | Error _ -> (tc.name, sprintf "Error: MCP call to %s failed" tc.name)
      | Ok r ->
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
                (tc.name, String.concat "\n" texts)
            | _ -> (tc.name, r.stdout)
          with _ -> (tc.name, r.stdout))
  | None ->
      (tc.name, sprintf "Error: Unknown tool '%s' and no external MCP configured" tc.name)

(** Single agent turn result *)
type turn_result =
  | TurnDone of string
  | TurnContinue of Ollama_parser.tool_call list
  | TurnError of string

(** Execute single agent turn *)
let execute_agentic_turn ~sw ~proc_mgr ~clock ~timeout request_body =
  let result = call_ollama_chat_eio ~sw ~proc_mgr ~clock ~timeout request_body in
  match result with
  | Error err -> TurnError err
  | Ok body_str ->
      match Ollama_parser.parse_chat_result body_str with
      | Error e -> TurnError e
      | Ok (Ollama_parser.TextResponse text) -> TurnDone text
      | Ok (Ollama_parser.ToolCalls calls) -> TurnContinue calls
      | Ok (Ollama_parser.TextWithTools (text, calls)) ->
          if calls = [] then TurnDone text
          else TurnContinue calls

(** Execute Ollama with agentic tool calling loop.

    When tools are provided, this:
    1. Sends prompt to Ollama with tool definitions
    2. Executes any tool_calls from Ollama's response
    3. Sends tool results back to Ollama
    4. Repeats until no more tool_calls or max_turns reached

    This enables tool-capable models (devstral, qwen3, llama3.3)
    to use MCP tools natively through llm-mcp.

    @param sw Eio switch for structured concurrency
    @param proc_mgr Eio process manager
    @param clock Eio clock
    @param tools MCP tool schemas for function calling
    @param external_mcp_url URL of external MCP server for tool execution
    @param on_turn Callback for each turn (turn number, prompt)
*)
let execute_ollama_agentic
    ~sw ~proc_mgr ~clock
    ~(tools : Types.tool_schema list)
    ?(external_mcp_url : string option = None)
    ?(on_turn : int -> string -> unit = fun _ _ -> ())
    args : tool_result =
  match args with
  | Ollama { model; prompt; system_prompt; temperature; timeout; stream = _; tools = _ } ->
      let max_turns = 10 in

      (* Convert tools to Ollama format *)
      let ollama_tools = List.map (fun (t : Types.tool_schema) ->
        `Assoc [
          ("type", `String "function");
          ("function", `Assoc [
            ("name", `String t.name);
            ("description", `String t.description);
            ("parameters", t.input_schema);
          ]);
        ]
      ) tools in

      let system = match system_prompt with
        | Some s -> s
        | None -> "You are a helpful assistant with access to tools."
      in

      let initial_messages = [
        { role = "system"; content = system; tool_calls = None; name = None }
      ] in

      (* Process tool calls recursively *)
      let rec process_tool_calls messages turn tool_calls =
        if turn >= max_turns then
          Error (sprintf "Max turns (%d) reached" max_turns)
        else begin
          (* Execute tool calls *)
          let tool_results = List.map (fun tc ->
            execute_tool_call_eio ~sw ~proc_mgr ~clock ~timeout ~external_mcp_url tc
          ) tool_calls in

          (* Build assistant message with tool calls *)
          let assistant_msg = {
            role = "assistant";
            content = "";
            tool_calls = Some tool_calls;
            name = None;
          } in

          (* Build tool result messages *)
          let tool_msgs = List.map (fun (name, result) ->
            { role = "tool"; content = result; tool_calls = None; name = Some name }
          ) tool_results in

          let new_messages = messages @ [assistant_msg] @ tool_msgs in

          (* Send continuation request *)
          let request = build_agentic_request ~model ~temperature ~tools:ollama_tools new_messages in
          let next_result = execute_agentic_turn ~sw ~proc_mgr ~clock ~timeout request in

          match next_result with
          | TurnDone text -> Ok text
          | TurnError err -> Error err
          | TurnContinue more_calls ->
              process_tool_calls new_messages (turn + 1) more_calls
        end
      in

      (* Main loop *)
      let run_loop () =
        on_turn 0 prompt;

        let user_msg = { role = "user"; content = prompt; tool_calls = None; name = None } in
        let messages = initial_messages @ [user_msg] in

        let request = build_agentic_request ~model ~temperature ~tools:ollama_tools messages in
        let result = execute_agentic_turn ~sw ~proc_mgr ~clock ~timeout request in

        match result with
        | TurnDone text -> Ok text
        | TurnError err -> Error err
        | TurnContinue tool_calls ->
            process_tool_calls messages 1 tool_calls
      in

      (match run_loop () with
      | Ok response ->
          {
            model = sprintf "ollama (%s) [agentic]" model;
            returncode = 0;
            response;
            extra = [
              ("temperature", sprintf "%.1f" temperature);
              ("local", "true");
              ("agentic", "true");
              ("tools_count", string_of_int (List.length tools));
            ];
          }
      | Error err ->
          {
            model = sprintf "ollama (%s) [agentic]" model;
            returncode = -1;
            response = sprintf "Agent loop error: %s" err;
            extra = [
              ("temperature", sprintf "%.1f" temperature);
              ("local", "true");
              ("agentic", "true");
              ("error", "true");
            ];
          })

  | _ ->
      (* Non-Ollama args: fall back to regular execute *)
      execute ~sw ~proc_mgr ~clock args

(** Execute Ollama agentic with Eio env *)
let execute_ollama_agentic_with_env ~sw ~env ~tools ?external_mcp_url ?on_turn args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  execute_ollama_agentic ~sw ~proc_mgr ~clock ~tools ?external_mcp_url ?on_turn args

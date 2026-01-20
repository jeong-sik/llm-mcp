(** MAGI Trinity tool implementations *)

open Types

(* Re-export Tool_config utilities for backward compatibility *)
let budget_mode_value = Tool_config.budget_mode_value

(* Re-export MCP config types and functions from Tool_config *)
type mcp_server_config = Tool_config.mcp_server_config
let get_mcp_server_url = Tool_config.get_mcp_server_url
let get_mcp_server_config = Tool_config.get_mcp_server_config

let exponential_backoff ~base_delay attempt =
  base_delay *. (2. ** float_of_int attempt)

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
                let result = json |> member "result" in
                let error = json |> member "error" in
                if error <> `Null then
                  let msg = try error |> member "message" |> to_string
                            with _ -> "Unknown error" in
                  Lwt.return (Printf.sprintf "Error: %s" msg)
                else
                  let content = result |> member "content" in
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

(* Call an external MCP tool via stdio (subprocess) - SHELL INJECTION SAFE *)
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
  let effective_timeout = min timeout 60 in
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
        try
          let json = Yojson.Safe.from_string r.stdout in
          let open Yojson.Safe.Util in
          let result = json |> member "result" in
          let error = json |> member "error" in
          if error <> `Null then
            let msg = try error |> member "message" |> to_string
                      with _ -> "Unknown error" in
            Lwt.return (Printf.sprintf "Error: %s" msg)
          else
            let content = result |> member "content" in
            match content with
            | `List items ->
                let texts = List.filter_map (fun item ->
                  match item |> member "type" |> to_string_option with
                  | Some "text" -> item |> member "text" |> to_string_option
                  | _ -> None
                ) items in
                Lwt.return (String.concat "\n" texts)
            | _ ->
                let result_str = result |> to_string_option in
                Lwt.return (Option.value result_str ~default:r.stdout)
        with _ -> Lwt.return r.stdout

(* Unified MCP call - routes to HTTP or stdio based on server config *)
let call_mcp ~server_name ~tool_name ~arguments ~timeout =
  match get_mcp_server_config server_name with
  | None -> Lwt.return (Printf.sprintf "Error: MCP server '%s' not found in config" server_name)
  | Some config ->
      match config.server_type, config.url, config.command with
      | "http", Some _url, _ ->
          call_external_mcp ~server_name ~tool_name ~arguments ~timeout
      | _, _, Some cmd ->
          call_stdio_mcp ~server_name ~command:cmd ~args:config.args ~tool_name ~arguments ~timeout
      | _ ->
          Lwt.return (Printf.sprintf "Error: MCP server '%s' has no valid URL or command" server_name)

(** {1 Tool Parsers Re-exports} *)

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

let thinking_prompt_prefix = Tool_parsers.thinking_prompt_prefix
let tool_schema_to_ollama_tool = Tool_parsers.tool_schema_to_ollama_tool
let tool_calls_to_json = Tool_parsers.tool_calls_to_json

let classify_gemini_error = Types.classify_gemini_error
let is_recoverable_gemini_error = Types.is_recoverable_gemini_error
let string_of_gemini_error = Types.string_of_gemini_error

let execute_ollama_streaming ~on_token args =
  let open Lwt.Syntax in
  let open Cli_runner in
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
            match Ollama_parser.parse_chat_chunk line with
            | Ok (token, tool_calls, _done) ->
                Buffer.add_string full_response token;
                if tool_calls <> [] then accumulated_tool_calls := tool_calls;
                on_token token
            | Error _ -> Lwt.return_unit
          else
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

(** {1 Gemini with Resilience} *)

let gemini_breaker = Mcp_resilience.create_circuit_breaker ~name:"gemini_cli_lwt" ~failure_threshold:3 ()

let with_retry_lwt ~(policy : Mcp_resilience.retry_policy) ~circuit_breaker ~op_name:_ op =
  let open Lwt.Syntax in
  let rec attempt n last_error =
    let cb_allows = match circuit_breaker with
      | None -> true
      | Some cb -> Mcp_resilience.circuit_allows cb
    in
    if not cb_allows then
      Lwt.return Mcp_resilience.CircuitOpen
    else if n > policy.max_attempts then
      Lwt.return (Mcp_resilience.Error (Option.value last_error ~default:"Max attempts reached"))
    else
      let* () =
        if n > 1 then
          let delay_ms = Mcp_resilience.calculate_delay policy (n - 1) in
          Lwt_unix.sleep (delay_ms /. 1000.0)
        else
          Lwt.return_unit
      in
      let* result = op () in
      match result with
      | Ok v ->
          (match circuit_breaker with Some cb -> Mcp_resilience.circuit_record_success cb | None -> ());
          Lwt.return (Mcp_resilience.Ok v)
      | Error err ->
          (match circuit_breaker with Some cb -> Mcp_resilience.circuit_record_failure cb | None -> ());
          attempt (n + 1) (Some err)
  in
  attempt 1 None

let execute_gemini_with_retry
    ?(max_retries = 2)
    ~model ~thinking_level ~timeout ~args () : tool_result Lwt.t =
  let open Lwt.Syntax in
  let open Cli_runner in

  let thinking_applied = thinking_level = High in

  match build_gemini_cmd args with
  | Error err ->
      Lwt.return { model = Printf.sprintf "gemini (%s)" model;
        returncode = -1; response = err; extra = [("invalid_args", "true")]; }
  | Ok cmd_list ->
      let cmd = List.hd cmd_list in
      let cmd_args = List.tl cmd_list in
      let policy = { Mcp_resilience.default_policy with max_attempts = max_retries + 1 } in
      
      let op () =
        let* result = run_command ~timeout cmd cmd_args in
        match result with
        | Ok r ->
            let response = get_output r in
            (match classify_gemini_error response with
            | Some error when is_recoverable_gemini_error error ->
                Lwt.return (Result.Error (string_of_gemini_error error))
            | _ -> Lwt.return (Result.Ok (r.exit_code, response)))
        | Error (Timeout t) -> Lwt.return (Result.Error (Printf.sprintf "Timeout after %ds" t))
        | Error (ProcessError msg) -> Lwt.return (Result.Error (Printf.sprintf "Error: %s" msg))
      in
      
      let* result = with_retry_lwt
        ~policy ~circuit_breaker:(Some gemini_breaker) ~op_name:"gemini_call_lwt" op in
      
      match result with
      | Ok (exit_code, response) ->
          let extra = [
            ("thinking_level", string_of_thinking_level thinking_level);
            ("thinking_prompt_applied", string_of_bool thinking_applied);
          ] in
          Lwt.return { model = Printf.sprintf "gemini (%s)" model; returncode = exit_code; response; extra }
      | Error err ->
          Lwt.return { model = Printf.sprintf "gemini (%s)" model; returncode = -1; response = err; extra = [] }
      | CircuitOpen ->
          Lwt.return { model = Printf.sprintf "gemini (%s)" model; returncode = -1; response = "Circuit breaker open"; extra = [] }
      | TimedOut ->
          Lwt.return { model = Printf.sprintf "gemini (%s)" model; returncode = -1; response = "Operation timed out"; extra = [] }

(** Execute a tool and return result *)
let execute args : tool_result Lwt.t =
  let open Lwt.Syntax in
  let open Cli_runner in

  match args with
  | Gemini { model; thinking_level; timeout; _ } ->
      execute_gemini_with_retry ~model ~thinking_level ~timeout ~args ()

  | Claude { model; ultrathink; working_directory; timeout; _ } ->
      (match build_claude_cmd args with
      | Error err ->
          Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
            returncode = -1; response = err; extra = []; }
      | Ok cmd_list ->
          let cmd = List.hd cmd_list in
          let cmd_args = List.tl cmd_list in
          let* result = run_command ~cwd:working_directory ~safe_tmpdir:true ~timeout cmd cmd_args in
          (match result with
          | Ok r ->
              Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
                returncode = r.exit_code; response = get_output r;
                extra = [("ultrathink", string_of_bool ultrathink)]; }
          | Error (Timeout t) ->
              Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
                returncode = -1; response = Printf.sprintf "Timeout after %ds" t; extra = []; }
          | Error (ProcessError msg) ->
              Lwt.return { model = Printf.sprintf "claude-cli (%s)" model;
                returncode = -1; response = Printf.sprintf "Error: %s" msg; extra = []; }))

  | Codex { model; reasoning_effort; timeout; _ } ->
      (match build_codex_cmd args with
      | Error err ->
          Lwt.return { model = Printf.sprintf "codex (%s)" model;
            returncode = -1; response = err; extra = []; }
      | Ok cmd_list ->
          let cmd = List.hd cmd_list in
          let cmd_args = List.tl cmd_list in
          let* result = run_command ~timeout cmd cmd_args in
          (match result with
          | Ok r ->
              Lwt.return { model = Printf.sprintf "codex (%s)" model;
                returncode = r.exit_code; response = clean_codex_output (get_output r);
                extra = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)]; }
          | Error (Timeout t) ->
              Lwt.return { model = Printf.sprintf "codex (%s)" model;
                returncode = -1; response = Printf.sprintf "Timeout after %ds" t; extra = []; }
          | Error (ProcessError msg) ->
              Lwt.return { model = Printf.sprintf "codex (%s)" model;
                returncode = -1; response = Printf.sprintf "Error: %s" msg; extra = []; }))

  | Ollama { model; temperature; timeout; _ } ->
      (match build_ollama_curl_cmd args with
      | Error err ->
          Lwt.return { model = Printf.sprintf "ollama (%s)" model;
            returncode = -1; response = err; extra = []; }
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
                returncode; response;
                extra = [("temperature", Printf.sprintf "%.1f" temperature); ("local", "true")]; }
          | Error (Timeout t) ->
              Lwt.return { model = Printf.sprintf "ollama (%s)" model;
                returncode = -1; response = Printf.sprintf "Timeout after %ds" t; extra = []; }
          | Error (ProcessError msg) ->
              Lwt.return { model = Printf.sprintf "ollama (%s)" model;
                returncode = -1; response = Printf.sprintf "Error: %s" msg; extra = []; }))

  | OllamaList ->
      let cmd = "ollama" in
      let cmd_args = ["list"] in
      let* result = run_command ~timeout:30 cmd cmd_args in
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
          let json = `List models in
          Lwt.return { model = "ollama_list";
            returncode = 0; response = Yojson.Safe.to_string json;
            extra = [("count", string_of_int (List.length models))]; }
      | Error (Timeout t) ->
          Lwt.return { model = "ollama_list";
            returncode = -1; response = Printf.sprintf "Timeout after %ds" t; extra = []; }
      | Error (ProcessError msg) ->
          Lwt.return { model = "ollama_list";
            returncode = -1; response = Printf.sprintf "Error: %s" msg; extra = []; })

  | ChainRun _ ->
      (* Chain execution requires Eio direct-style concurrency *)
      Lwt.return { model = "chain.run";
        returncode = -1;
        response = "Chain execution not available in Lwt mode. Use Eio server (start-llm-mcp.sh --http)";
        extra = []; }

  | ChainValidate { chain } ->
      (* Validation can run synchronously *)
      let result = match Chain_parser.parse_chain chain with
        | Error msg ->
            { model = "chain.validate";
              returncode = -1;
              response = Printf.sprintf "Parse error: %s" msg;
              extra = [("stage", "parse"); ("valid", "false")]; }
        | Ok parsed_chain ->
            (match Chain_parser.validate_chain parsed_chain with
            | Error msg ->
                { model = "chain.validate";
                  returncode = -1;
                  response = Printf.sprintf "Validation error: %s" msg;
                  extra = [("stage", "validate"); ("valid", "false")]; }
            | Ok () ->
                (match Chain_compiler.compile parsed_chain with
                | Error msg ->
                    { model = "chain.validate";
                      returncode = -1;
                      response = Printf.sprintf "Compile error: %s" msg;
                      extra = [("stage", "compile"); ("valid", "false")]; }
                | Ok plan ->
                    let node_count = List.length parsed_chain.Chain_types.nodes in
                    let depth = plan.Chain_types.depth in
                    let parallel_groups = List.length plan.Chain_types.parallel_groups in
                    { model = "chain.validate";
                      returncode = 0;
                      response = Printf.sprintf "Chain '%s' is valid: %d nodes, depth %d, %d parallel groups"
                        parsed_chain.Chain_types.id node_count depth parallel_groups;
                      extra = [
                        ("valid", "true");
                        ("chain_id", parsed_chain.Chain_types.id);
                        ("node_count", string_of_int node_count);
                        ("depth", string_of_int depth);
                        ("parallel_groups", string_of_int parallel_groups);
                      ]; }))
      in
      Lwt.return result

(** Format results *)
let execute_formatted ~(format : response_format) args : string Lwt.t =
  let open Lwt.Syntax in
  let* result = execute args in
  Lwt.return (format_tool_result ~format result)

let execute_verbose args : string Lwt.t =
  execute_formatted ~format:Verbose args

let execute_compact args : string Lwt.t =
  execute_formatted ~format:Compact args

(** Ollama agentic loop *)
let execute_ollama_agentic
    ~(tools : Types.tool_schema list)
    ?(external_mcp_url : string option = None)
    ?(on_turn : int -> string -> unit Lwt.t = fun _ _ -> Lwt.return_unit)
    args : tool_result Lwt.t =
  let open Lwt.Syntax in
  match args with
  | Ollama { model; prompt; system_prompt; temperature; _ } ->
      let system = match system_prompt with
        | Some s -> s
        | None -> "You are a helpful assistant with access to tools."
      in
      let* result = Agent_loop.run
        ~model ~prompt ~system_prompt:system ~temperature ~max_turns:10
        ~external_mcp_url ~tools ~on_turn ()
      in
      (match result with
      | Ok response ->
          Lwt.return {
            model = Printf.sprintf "ollama (%s) [agentic]" model;
            returncode = 0; response;
            extra = [("agentic", "true"); ("tools_count", string_of_int (List.length tools))];
          }
      | Error err ->
          Lwt.return {
            model = Printf.sprintf "ollama (%s) [agentic]" model;
            returncode = -1; response = Printf.sprintf "Agent loop error: %s" err;
            extra = [("agentic", "true"); ("error", "true")];
          })
  | _ -> execute args

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
let parse_chain_run_args = Tool_parsers.parse_chain_run_args
let parse_chain_validate_args = Tool_parsers.parse_chain_validate_args
let parse_chain_to_mermaid_args = Tool_parsers.parse_chain_to_mermaid_args
let parse_chain_visualize_args = Tool_parsers.parse_chain_visualize_args
let parse_chain_convert_args = Tool_parsers.parse_chain_convert_args
let parse_chain_orchestrate_args = Tool_parsers.parse_chain_orchestrate_args
let parse_gh_pr_diff_args = Tool_parsers.parse_gh_pr_diff_args
let parse_slack_post_args = Tool_parsers.parse_slack_post_args
let parse_chain_checkpoints_args = Tool_parsers.parse_chain_checkpoints_args
let parse_chain_resume_args = Tool_parsers.parse_chain_resume_args
let parse_prompt_register_args = Tool_parsers.parse_prompt_register_args
let parse_prompt_get_args = Tool_parsers.parse_prompt_get_args
let build_gemini_cmd = Tool_parsers.build_gemini_cmd
let build_claude_cmd = Tool_parsers.build_claude_cmd
let build_codex_cmd = Tool_parsers.build_codex_cmd
let build_ollama_curl_cmd = Tool_parsers.build_ollama_curl_cmd
let parse_ollama_response = Tool_parsers.parse_ollama_response
let parse_ollama_chunk = Tool_parsers.parse_ollama_chunk
let clean_codex_output = Tool_parsers.clean_codex_output
let exponential_backoff ~base_delay attempt =
  base_delay *. (2. ** float_of_int attempt)

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
                let result = json |> member "result" in
                let error = json |> member "error" in
                if error <> `Null then
                  let msg = try error |> member "message" |> to_string
                            with _ -> "Unknown error" in
                  sprintf "Error: %s" msg
                else
                  let content = result |> member "content" in
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
          let result = json |> member "result" in
          let error = json |> member "error" in
          if error <> `Null then
            let msg = try error |> member "message" |> to_string
                      with _ -> "Unknown error" in
            sprintf "Error: %s" msg
          else
            let content = result |> member "content" in
            match content with
            | `List items ->
                let texts = List.filter_map (fun item ->
                  match item |> member "type" |> to_string_option with
                  | Some "text" -> item |> member "text" |> to_string_option
                  | _ -> None
                ) items in
                String.concat "\n" texts
            | _ ->
                let result_str = result |> to_string_option in
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

(** {1 Gemini with Resilience} *)

let gemini_breaker = Mcp_resilience.create_circuit_breaker ~name:"gemini_cli" ~failure_threshold:3 ()

(** Execute Gemini with automatic retry for recoverable errors *)
let execute_gemini_with_retry ~sw ~proc_mgr ~clock
    ?(max_retries = 2)
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
      let policy = { Mcp_resilience.default_policy with max_attempts = max_retries + 1 } in
      
      let op () =
        let result = run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args in
        let outcome =
          match result with
          | Ok r ->
              let response = get_output r in
              (match classify_gemini_error response with
              | Some error when is_recoverable_gemini_error error ->
                  Result.Error (string_of_gemini_error error)
              | _ -> Result.Ok (r.exit_code, response))
          | Error (Timeout t) -> Result.Error (sprintf "Timeout after %ds" t)
          | Error (ProcessError msg) -> Result.Error (sprintf "Error: %s" msg)
        in
        match outcome with
        | Ok value -> Mcp_resilience.Ok value
        | Error err -> Mcp_resilience.Error err
      in
      
      let classify _ = Mcp_resilience.Retry in
      match Mcp_resilience.with_retry_eio
              ~clock
              ~policy
              ~circuit_breaker:(Some gemini_breaker)
              ~op_name:"gemini_call"
              ~classify
              op
      with
      | Ok (exit_code, response) ->
          let extra = [
            ("thinking_level", string_of_thinking_level thinking_level);
            ("thinking_prompt_applied", string_of_bool thinking_applied);
          ] in
          { model = sprintf "gemini (%s)" model; returncode = exit_code; response; extra }
      | Error err ->
          { model = sprintf "gemini (%s)" model; returncode = -1; response = err; extra = [] }
      | CircuitOpen ->
          { model = sprintf "gemini (%s)" model; returncode = -1; response = "Circuit breaker open"; extra = [] }
      | TimedOut ->
          { model = sprintf "gemini (%s)" model; returncode = -1; response = "Operation timed out"; extra = [] }

(** {1 External Tools} *)

(** Default timeout for external tools (60 seconds) *)
let external_tool_timeout = 60

(** Execute GitHub PR diff command using gh CLI *)
let execute_gh_pr_diff ~sw ~proc_mgr ~clock ~repo ~pr_number : tool_result =
  eprintf "[gh_pr_diff] Fetching diff for %s#%d\n%!" repo pr_number;
  let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
    "gh" ["pr"; "diff"; string_of_int pr_number; "-R"; repo] in
  match result with
  | Ok r ->
      { model = "gh_pr_diff";
        returncode = r.exit_code;
        response = get_output r;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number)]; }
  | Error (Timeout t) ->
      { model = "gh_pr_diff";
        returncode = -1;
        response = sprintf "Timeout after %ds" t;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number); ("error", "timeout")]; }
  | Error (ProcessError msg) ->
      { model = "gh_pr_diff";
        returncode = -1;
        response = sprintf "Error: %s" msg;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number); ("error", msg)]; }

(** Execute Slack post message using curl *)
let execute_slack_post ~sw ~proc_mgr ~clock ~channel ~text ~thread_ts : tool_result =
  let slack_token = Sys.getenv_opt "SLACK_BOT_TOKEN" in
  match slack_token with
  | None ->
      { model = "slack_post";
        returncode = -1;
        response = "Error: SLACK_BOT_TOKEN environment variable not set";
        extra = [("error", "missing_token")]; }
  | Some token ->
      (* Build JSON payload *)
      let payload = `Assoc (
        [("channel", `String channel); ("text", `String text)]
        @ (match thread_ts with Some ts -> [("thread_ts", `String ts)] | None -> [])
      ) in
      let json_str = Yojson.Safe.to_string payload in
      eprintf "[slack_post] Posting to channel %s\n%!" channel;
      let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
        "curl" [
          "-s"; "-X"; "POST";
          "-H"; "Content-Type: application/json; charset=utf-8";
          "-H"; sprintf "Authorization: Bearer %s" token;
          "-d"; json_str;
          "https://slack.com/api/chat.postMessage"
        ] in
      match result with
      | Ok r ->
          (* Parse Slack API response *)
          (try
            let response_json = Yojson.Safe.from_string r.stdout in
            let ok = Yojson.Safe.Util.(response_json |> member "ok" |> to_bool) in
            if ok then
              { model = "slack_post";
                returncode = 0;
                response = sprintf "Message posted to %s" channel;
                extra = [("channel", channel)]; }
            else
              let error = Yojson.Safe.Util.(response_json |> member "error" |> to_string) in
              { model = "slack_post";
                returncode = -1;
                response = sprintf "Slack API error: %s" error;
                extra = [("error", error)]; }
          with _ ->
            { model = "slack_post";
              returncode = r.exit_code;
              response = r.stdout;
              extra = [("channel", channel)]; })
      | Error (Timeout t) ->
          { model = "slack_post";
            returncode = -1;
            response = sprintf "Timeout after %ds" t;
            extra = [("channel", channel); ("error", "timeout")]; }
      | Error (ProcessError msg) ->
          { model = "slack_post";
            returncode = -1;
            response = sprintf "Error: %s" msg;
            extra = [("channel", channel); ("error", msg)]; }

(** {1 Langfuse Tracing Helpers} *)

(** Extract model name from tool_args for tracing *)
let get_model_name_for_tracing = function
  | Gemini { model; _ } -> sprintf "gemini:%s" model
  | Claude { model; _ } -> sprintf "claude:%s" model
  | Codex { model; _ } -> sprintf "codex:%s" model
  | Ollama { model; _ } -> sprintf "ollama:%s" model
  | OllamaList -> "ollama:list"
  | ChainRun _ -> "chain:run"
  | ChainValidate _ -> "chain:validate"
  | ChainList -> "chain:list"
  | ChainToMermaid _ -> "chain:to_mermaid"
  | ChainVisualize _ -> "chain:visualize"
  | ChainConvert _ -> "chain:convert"
  | ChainOrchestrate _ -> "chain:orchestrate"
  | ChainCheckpoints _ -> "chain:checkpoints"
  | ChainResume _ -> "chain:resume"
  | PromptRegister _ -> "prompt:register"
  | PromptList -> "prompt:list"
  | PromptGet _ -> "prompt:get"
  | GhPrDiff _ -> "tool:gh_pr_diff"
  | SlackPost _ -> "tool:slack_post"

(** Extract input/prompt from tool_args for tracing *)
let get_input_for_tracing = function
  | Gemini { prompt; _ } -> prompt
  | Claude { prompt; _ } -> prompt
  | Codex { prompt; _ } -> prompt
  | Ollama { prompt; _ } -> prompt
  | OllamaList -> "(list models)"
  | ChainRun { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainValidate { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainOrchestrate { chain; _ } ->
      (match chain with
       | Some j -> sprintf "(orchestrate: %s)" (Yojson.Safe.to_string j)
       | None -> "(orchestrate: preset)")
  | _ -> "(non-llm operation)"

(** {1 Main Execute Function} *)

(** Execute a tool and return result - Direct Style *)
let rec execute ~sw ~proc_mgr ~clock args : tool_result =
  match args with
  | Gemini { model; thinking_level; timeout; _ } ->
      execute_gemini_with_retry ~sw ~proc_mgr ~clock ~model ~thinking_level ~timeout ~args ()

  | Claude { model; long_context; working_directory = _; timeout; _ } ->
      (match build_claude_cmd args with
      | Error err ->
          { model = sprintf "claude-cli (%s)" model;
            returncode = -1;
            response = err;
            extra = [("long_context", string_of_bool long_context); ("invalid_args", "true")]; }
      | Ok cmd_list ->
          let cmd = List.hd cmd_list in
          let cmd_args = List.tl cmd_list in
          let result = run_command ~sw ~proc_mgr ~clock ~safe_tmpdir:true ~timeout cmd cmd_args in
          match result with
          | Ok r ->
              { model = sprintf "claude-cli (%s)" model;
                returncode = r.exit_code;
                response = get_output r;
                extra = [("long_context", string_of_bool long_context)]; }
          | Error (Timeout t) ->
              { model = sprintf "claude-cli (%s)" model;
                returncode = -1;
                response = sprintf "Timeout after %ds" t;
                extra = [("long_context", string_of_bool long_context)]; }
          | Error (ProcessError msg) ->
              { model = sprintf "claude-cli (%s)" model;
                returncode = -1;
                response = sprintf "Error: %s" msg;
                extra = [("long_context", string_of_bool long_context)]; })

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

  | Ollama { model; temperature; timeout; tools; _ } ->
      (* Force stream=false for non-streaming execute path *)
      let has_tools = match tools with Some l when List.length l > 0 -> true | _ -> false in
      (match build_ollama_curl_cmd ~force_stream:(Some false) args with
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
              (* Use chat parser when tools are present, otherwise use generate parser *)
              let (response, extra_fields) =
                if has_tools then begin
                  match Ollama_parser.parse_chat_result r.stdout with
                  | Ok (Ollama_parser.TextResponse (text, thinking)) ->
                      let extra = match thinking with
                        | Some t -> [("thinking", t)]
                        | None -> []
                      in
                      (text, extra)
                  | Ok (Ollama_parser.ToolCalls (calls, thinking)) ->
                      let calls_json = tool_calls_to_json calls in
                      let extra = [("tool_calls", calls_json)] in
                      let extra = match thinking with
                        | Some t -> ("thinking", t) :: extra
                        | None -> extra
                      in
                      ("", extra)
                  | Ok (Ollama_parser.TextWithTools (text, calls, thinking)) ->
                      let calls_json = tool_calls_to_json calls in
                      let extra = [("tool_calls", calls_json)] in
                      let extra = match thinking with
                        | Some t -> ("thinking", t) :: extra
                        | None -> extra
                      in
                      (text, extra)
                  | Error err -> (sprintf "Error: %s" err, [])
                end else begin
                  match parse_ollama_response r.stdout with
                  | Ok resp -> (resp, [])
                  | Error err -> (sprintf "Error: %s" err, [])
                end
              in
              (* Success if: has response text OR has tool_calls, and not an error *)
              let has_tool_calls = List.exists (fun (k, _) -> k = "tool_calls") extra_fields in
              let returncode =
                if (String.length response > 0 || has_tool_calls) &&
                   not (String.length response >= 6 && String.sub response 0 6 = "Error:")
                then 0 else -1
              in
              { model = sprintf "ollama (%s)" model;
                returncode;
                response;
                extra = [("temperature", sprintf "%.1f" temperature); ("local", "true")] @ extra_fields; }
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

  | ChainRun { chain; mermaid; input; trace } ->
      (* Parse from either JSON or Mermaid (WYSIWYE) *)
      let parse_result = match (chain, mermaid) with
        | (Some c, _) -> Chain_parser.parse_chain c
        | (_, Some m) -> Chain_mermaid_parser.parse_chain m
        | (None, None) -> Error "Either 'chain' (JSON) or 'mermaid' (string) is required"
      in
      (match parse_result with
      | Error msg ->
          { model = "chain.run";
            returncode = -1;
            response = sprintf "Parse error: %s" msg;
            extra = [("stage", "parse")]; }
      | Ok parsed_chain ->
          match Chain_compiler.compile parsed_chain with
          | Error msg ->
              { model = "chain.run";
                returncode = -1;
                response = sprintf "Compile error: %s" msg;
                extra = [("stage", "compile")]; }
          | Ok plan ->
              (* Use chain's global timeout for all nodes *)
              let node_timeout = parsed_chain.Chain_types.config.Chain_types.timeout in
              let env_truthy name =
                match Sys.getenv_opt name with
                | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
                | _ -> false
              in
              let trace_effective = trace || env_truthy "LLM_MCP_CHAIN_FORCE_TRACE" in
              let starts_with ~prefix s =
                let prefix_len = String.length prefix in
                String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
              in
              let split_tool_name name =
                match String.index_opt name '.' with
                | None -> None
                | Some idx ->
                    let server = String.sub name 0 idx in
                    let tool_len = String.length name - idx - 1 in
                    if server = "" || tool_len <= 0 then None
                    else Some (server, String.sub name (idx + 1) tool_len)
              in
              (* Create exec_fn that routes to appropriate LLM *)
              let exec_fn ~model ?system ~prompt ?tools () =
                let _ = system in  (* Unused for now, available for future enhancement *)
                (* Convert Yojson.Safe.t tools to tool_schema list option *)
                let parsed_tools = match tools with
                  | None -> None
                  | Some json ->
                      let open Yojson.Safe.Util in
                      match json with
                      | `List items ->
                          let schemas = List.filter_map (fun item ->
                            try
                              Some {
                                Types.name = item |> member "name" |> to_string;
                                description = item |> member "description" |> to_string;
                                input_schema = item |> member "input_schema";
                              }
                            with _ -> None
                          ) items in
                          if schemas = [] then None else Some schemas
                      | _ -> None
                in
                let args = match String.lowercase_ascii model with
                  | "stub" | "mock" ->
                      (* Stub model for tests and local smoke runs *)
                      Types.Gemini {
                        prompt;
                        model = "stub";
                        thinking_level = Types.Low;
                        yolo = false;
                        timeout = node_timeout;
                        stream = false;
                      }
                  | "gemini" | "gemini-3-pro-preview" | "gemini-2.5-pro" ->
                      Types.Gemini {
                        prompt;
                        model = "gemini-3-pro-preview";
                        thinking_level = Types.High;
                        yolo = false;
                        timeout = node_timeout;
                        stream = false;
                      }
                  | "claude" | "opus" | "opus-4" | "sonnet" | "haiku" | "haiku-4.5" ->
                      Types.Claude {
                        prompt;
                        model;
                        long_context = true;
                        system_prompt = None;
                        output_format = Types.Text;
                        allowed_tools = [];
                        working_directory = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp";
                        timeout = node_timeout;
                        stream = false;
                      }
                  | "codex" | "gpt-5.2" ->
                      Types.Codex {
                        prompt;
                        model = "gpt-5.2";
                        reasoning_effort = Types.RXhigh;
                        sandbox = Types.WorkspaceWrite;
                        working_directory = None;
                        timeout = node_timeout;
                        stream = false;
                      }
                  | "ollama" ->
                      (* Plain ollama defaults to qwen3:1.7b for fast testing *)
                      Types.Ollama {
                        prompt;
                        model = "qwen3:1.7b";
                        system_prompt = None;
                        temperature = 0.7;
                        timeout = node_timeout;
                        stream = false;
                        tools = parsed_tools;  (* Pass through tools from Chain DSL *)
                      }
                  | m when String.length m > 7 && String.sub m 0 7 = "ollama:" ->
                      let ollama_model = String.sub m 7 (String.length m - 7) in
                      Types.Ollama {
                        prompt;
                        model = ollama_model;
                        system_prompt = None;
                        temperature = 0.7;
                        timeout = node_timeout;
                        stream = false;
                        tools = parsed_tools;  (* Pass through tools from Chain DSL *)
                      }
                  | _ ->
                      (* Default to Gemini for unknown models *)
                      Types.Gemini {
                        prompt;
                        model = "gemini-3-pro-preview";
                        thinking_level = Types.High;
                        yolo = false;
                        timeout = node_timeout;
                        stream = false;
                      }
                in
                match args with
                | Types.Gemini { model = "stub"; _ } ->
                    Ok (Printf.sprintf "[stub]%s" prompt)
                | _ ->
                    let result = execute ~sw ~proc_mgr ~clock args in
                    if result.returncode = 0 then Ok result.response
                    else Error result.response
              in
              let tool_exec ~name ~args =
                match split_tool_name name with
                | Some (server_name, tool_name) ->
                    let output =
                      call_mcp ~sw ~proc_mgr ~clock
                        ~server_name ~tool_name ~arguments:args ~timeout:node_timeout
                    in
                    if starts_with ~prefix:"Error:" output then Error output else Ok output
                | None ->
                    let result =
                      match name with
                      | "gemini" ->
                          let args = parse_gemini_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "claude-cli" ->
                          let args = parse_claude_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "codex" ->
                          let args = parse_codex_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "ollama" ->
                          let args = parse_ollama_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "ollama_list" ->
                          let args = parse_ollama_list_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "echo" ->
                          (* Simple echo tool for testing *)
                          let input = try args |> Yojson.Safe.Util.member "input" |> Yojson.Safe.Util.to_string
                                      with _ -> Yojson.Safe.to_string args in
                          { Types.model = "echo"; returncode = 0; response = input; extra = [] }
                      | "identity" ->
                          (* Identity tool: returns args unchanged *)
                          { Types.model = "identity"; returncode = 0; response = Yojson.Safe.to_string args; extra = [] }
                      | _ ->
                          { Types.model = "chain.tool";
                            returncode = -1;
                            response = sprintf "Unknown tool: %s" name;
                            extra = [("tool", name)];
                          }
                    in
                    if result.returncode = 0 then Ok result.response else Error result.response
              in
              (* Input is passed directly to executor for first node injection *)
              let _ = input in  (* Will be used by executor *)
              let result = Chain_executor_eio.execute
                ~sw ~clock ~timeout:node_timeout ~trace:trace_effective ~exec_fn ~tool_exec plan
              in
              let run_id = List.assoc_opt "run_id" result.Chain_types.metadata in
              { model = "chain.run";
                returncode = if result.Chain_types.success then 0 else -1;
                response = result.Chain_types.output;
                extra = [
                  ("chain_id", result.Chain_types.chain_id);
                  ("duration_ms", string_of_int result.Chain_types.duration_ms);
                  ("trace_count", string_of_int (List.length result.Chain_types.trace));
                ] @ (match run_id with Some id -> [("run_id", id)] | None -> []); })


  | ChainValidate { chain; mermaid } ->
      (* Parse from either JSON or Mermaid, then validate *)
      let parse_result = match (chain, mermaid) with
        | (Some c, _) -> Chain_parser.parse_chain c
        | (_, Some m) -> Chain_mermaid_parser.parse_chain m
        | (None, None) -> Error "Either 'chain' (JSON) or 'mermaid' (string) is required"
      in
      (match parse_result with
      | Error msg ->
          { model = "chain.validate";
            returncode = -1;
            response = sprintf "Parse error: %s" msg;
            extra = [("stage", "parse"); ("valid", "false")]; }
      | Ok parsed_chain ->
          match Chain_parser.validate_chain parsed_chain with
          | Error msg ->
              { model = "chain.validate";
                returncode = -1;
                response = sprintf "Validation error: %s" msg;
                extra = [("stage", "validate"); ("valid", "false")]; }
          | Ok () ->
              (* Also try to compile to check DAG validity *)
              match Chain_compiler.compile parsed_chain with
              | Error msg ->
                  { model = "chain.validate";
                    returncode = -1;
                    response = sprintf "Compile error: %s" msg;
                    extra = [("stage", "compile"); ("valid", "false")]; }
              | Ok plan ->
                  let node_count = List.length parsed_chain.Chain_types.nodes in
                  let depth = plan.Chain_types.depth in
                  let parallel_groups = List.length plan.Chain_types.parallel_groups in
                  { model = "chain.validate";
                    returncode = 0;
                    response = sprintf "Chain '%s' is valid: %d nodes, depth %d, %d parallel groups"
                      parsed_chain.Chain_types.id node_count depth parallel_groups;
                    extra = [
                      ("valid", "true");
                      ("chain_id", parsed_chain.Chain_types.id);
                      ("node_count", string_of_int node_count);
                      ("depth", string_of_int depth);
                      ("parallel_groups", string_of_int parallel_groups);
                    ]; })

  | ChainList ->
      let ids = Chain_registry.list_ids () in
      let response = String.concat ", " ids in
      { model = "chain.list";
        returncode = 0;
        response = "Registered chains: " ^ response;
        extra = [("count", string_of_int (List.length ids))];
      }

  | ChainToMermaid { chain } ->
      (* Parse JSON to Chain AST, then convert to Mermaid *)
      (match Chain_parser.parse_chain chain with
      | Error msg ->
          { model = "chain.to_mermaid";
            returncode = -1;
            response = sprintf "Parse error: %s" msg;
            extra = [("stage", "parse")]; }
      | Ok parsed_chain ->
          let mermaid_text = Chain_mermaid_parser.chain_to_mermaid parsed_chain in
          { model = "chain.to_mermaid";
            returncode = 0;
            response = mermaid_text;
            extra = [
              ("chain_id", parsed_chain.Chain_types.id);
              ("node_count", string_of_int (List.length parsed_chain.Chain_types.nodes));
            ]; })

  | ChainVisualize { chain } ->
      (* Parse JSON to Chain AST, then convert to ASCII visualization *)
      (match Chain_parser.parse_chain chain with
      | Error msg ->
          { model = "chain.visualize";
            returncode = -1;
            response = sprintf "Parse error: %s" msg;
            extra = [("stage", "parse")]; }
      | Ok parsed_chain ->
          let ascii_text = Chain_mermaid_parser.chain_to_ascii parsed_chain in
          { model = "chain.visualize";
            returncode = 0;
            response = ascii_text;
            extra = [
              ("chain_id", parsed_chain.Chain_types.id);
              ("node_count", string_of_int (List.length parsed_chain.Chain_types.nodes));
              ("output", parsed_chain.Chain_types.output);
            ]; })

  | ChainConvert { from_format; to_format; input; pretty } ->
      (* Bidirectional conversion: JSON <-> Mermaid *)
      (match (from_format, to_format) with
       | ("json", "mermaid") ->
           (* JSON → Mermaid: parse JSON, convert to Mermaid *)
           (match Chain_parser.parse_chain input with
            | Error msg ->
                { model = "chain.convert";
                  returncode = -1;
                  response = sprintf "JSON parse error: %s" msg;
                  extra = [("from", "json"); ("to", "mermaid"); ("stage", "parse")]; }
            | Ok chain ->
                let mermaid = Chain_mermaid_parser.chain_to_mermaid chain in
                { model = "chain.convert";
                  returncode = 0;
                  response = mermaid;
                  extra = [
                    ("from", "json"); ("to", "mermaid");
                    ("chain_id", chain.Chain_types.id);
                    ("node_count", string_of_int (List.length chain.Chain_types.nodes));
                  ]; })

       | ("mermaid", "json") ->
           (* Mermaid → JSON: parse Mermaid, convert to JSON *)
           let mermaid_text = match input with
             | `String s -> s
             | _ -> Yojson.Safe.to_string input
           in
           (match Chain_mermaid_parser.parse_chain mermaid_text with
            | Error msg ->
                { model = "chain.convert";
                  returncode = -1;
                  response = sprintf "Mermaid parse error: %s" msg;
                  extra = [("from", "mermaid"); ("to", "json"); ("stage", "parse")]; }
            | Ok chain ->
                let json_str = Chain_parser.chain_to_json_string ~pretty chain in
                { model = "chain.convert";
                  returncode = 0;
                  response = json_str;
                  extra = [
                    ("from", "mermaid"); ("to", "json");
                    ("chain_id", chain.Chain_types.id);
                    ("node_count", string_of_int (List.length chain.Chain_types.nodes));
                  ]; })

       | (f, t) when f = t ->
           { model = "chain.convert";
             returncode = -1;
             response = sprintf "Same format conversion (from=%s, to=%s) is not meaningful" f t;
             extra = [("from", f); ("to", t)]; }

       | (f, t) ->
           { model = "chain.convert";
             returncode = -1;
             response = sprintf "Unsupported conversion: %s → %s (supported: json↔mermaid)" f t;
             extra = [("from", f); ("to", t)]; })

  | ChainOrchestrate { goal; chain; max_replans; timeout; trace; verify_on_complete; orchestrator_model } ->
      (* Build orchestration config *)
      let config : Chain_orchestrator_eio.orchestration_config = {
        max_replans;
        timeout_ms = timeout * 1000;
        trace_enabled = trace;
        verify_on_complete;
      } in

      (* Create llm_call adapter: ~prompt:string -> string
         Uses specified orchestrator_model (default: gemini) *)
      let llm_call ~prompt =
        match orchestrator_model with
        | "stub" ->
            (* Stub model for testing - returns valid Chain JSON for Design phase *)
            Printf.sprintf {|Here is a chain to accomplish the goal:

```json
{
  "id": "stub_chain_%d",
  "nodes": [
    {"id": "step1", "type": "llm", "model": "stub", "prompt": "Process: %s"}
  ],
  "output": "step1",
  "config": {"timeout": 30, "trace": true}
}
```

This chain will execute the goal using a stub model.|}
              (Random.int 10000)
              (String.escaped (String.sub prompt 0 (min 50 (String.length prompt))))
        | "claude" | "claude-cli" ->
            let args = Types.Claude {
              prompt;
              model = "sonnet";
              long_context = false;
              system_prompt = Some "You are a chain orchestrator. Design, analyze, and verify workflows.";
              output_format = Types.Text;
              allowed_tools = [];
              working_directory = Sys.getcwd ();
              timeout = 120;
              stream = false;
            } in
            let result = execute ~sw ~proc_mgr ~clock args in
            if result.returncode = 0 then result.response
            else failwith (Printf.sprintf "Claude call failed: %s" result.response)
        | "codex" ->
            let args = Types.Codex {
              prompt;
              model = "gpt-5.2";
              reasoning_effort = Types.RHigh;
              sandbox = Types.WorkspaceWrite;
              working_directory = Some (Sys.getcwd ());
              timeout = 120;
              stream = false;
            } in
            let result = execute ~sw ~proc_mgr ~clock args in
            if result.returncode = 0 then result.response
            else failwith (Printf.sprintf "Codex call failed: %s" result.response)
        | "ollama" ->
            let args = Types.Ollama {
              prompt;
              model = "devstral";
              system_prompt = Some "You are a chain orchestrator. Design, analyze, and verify workflows.";
              temperature = 0.7;
              timeout = 120;
              stream = false;
              tools = None;
            } in
            let result = execute ~sw ~proc_mgr ~clock args in
            if result.returncode = 0 then result.response
            else failwith (Printf.sprintf "Ollama call failed: %s" result.response)
        | "gemini" | _ ->
            let args = Types.Gemini {
              prompt;
              model = "gemini-3-pro-preview";
              thinking_level = Types.High;
              yolo = false;
              timeout = 120;
              stream = false;
            } in
            let result = execute ~sw ~proc_mgr ~clock args in
            if result.returncode = 0 then result.response
            else failwith (Printf.sprintf "Gemini call failed: %s" result.response)
      in

      (* Helper: parse "server.tool" format *)
      let split_tool_name name =
        match String.index_opt name '.' with
        | None -> None
        | Some idx ->
            let server = String.sub name 0 idx in
            let tool_len = String.length name - idx - 1 in
            if server = "" || tool_len <= 0 then None
            else Some (server, String.sub name (idx + 1) tool_len)
      in

      (* Create tool_exec adapter: ~name:string -> ~args:Yojson.Safe.t -> Yojson.Safe.t *)
      let tool_exec ~name ~args:tool_args =
        let node_timeout = timeout in
        match split_tool_name name with
        | Some (server_name, tool_name) ->
            let output = call_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments:tool_args ~timeout:node_timeout in
            (try Yojson.Safe.from_string output with _ -> `String output)
        | None ->
            (* Direct tool execution *)
            let result = match name with
              | "gemini" ->
                  let parsed = parse_gemini_args tool_args in
                  execute ~sw ~proc_mgr ~clock parsed
              | "claude-cli" | "claude" ->
                  let parsed = parse_claude_args tool_args in
                  execute ~sw ~proc_mgr ~clock parsed
              | "codex" ->
                  let parsed = parse_codex_args tool_args in
                  execute ~sw ~proc_mgr ~clock parsed
              | "ollama" ->
                  let parsed = parse_ollama_args tool_args in
                  execute ~sw ~proc_mgr ~clock parsed
              | "echo" ->
                  (* Simple echo tool for testing: returns the input args as a string *)
                  let input = try tool_args |> Yojson.Safe.Util.member "input" |> Yojson.Safe.Util.to_string
                              with _ -> Yojson.Safe.to_string tool_args in
                  { model = "echo"; returncode = 0; response = input; extra = [] }
              | "identity" ->
                  (* Identity tool: returns args unchanged *)
                  { model = "identity"; returncode = 0; response = Yojson.Safe.to_string tool_args; extra = [] }
              | _ ->
                  { model = name; returncode = 1; response = Printf.sprintf "Unknown tool: %s" name; extra = [] }
            in
            if result.returncode = 0 then
              (try Yojson.Safe.from_string result.response with _ -> `String result.response)
            else
              `Assoc [("error", `String result.response)]
      in

      (* Create tasks from chain if provided, otherwise empty *)
      let tasks = match chain with
        | Some chain_json ->
            (try
              let open Yojson.Safe.Util in
              let nodes = chain_json |> member "nodes" |> to_list in
              List.mapi (fun i node ->
                let title = node |> member "id" |> to_string_option |> Option.value ~default:(Printf.sprintf "task-%d" i) in
                Chain_composer.{
                  task_id = Printf.sprintf "task-%03d" (i + 1);
                  title;
                  description = Some (Yojson.Safe.to_string node);
                  priority = i + 1;
                  status = "todo";
                  assignee = None;
                  metadata = [];
                }
              ) nodes
            with _ -> [])
        | None -> []
      in

      (* Run orchestration *)
      (match Chain_orchestrator_eio.orchestrate ~sw ~clock ~config ~llm_call ~tool_exec ~goal ~tasks with
      | Ok result ->
          let metrics_json = match result.final_metrics with
            | Some m -> Chain_evaluator.chain_metrics_to_yojson m |> Yojson.Safe.to_string
            | None -> "null"
          in
          let verification_json = match result.verification with
            | Some v -> Chain_evaluator.verification_result_to_yojson v |> Yojson.Safe.to_string
            | None -> "null"
          in
          { model = "chain.orchestrate";
            returncode = if result.success then 0 else 1;
            response = result.summary;
            extra = [
              ("success", string_of_bool result.success);
              ("total_replans", string_of_int result.total_replans);
              ("metrics", metrics_json);
              ("verification", verification_json);
            ]; }
      | Error err ->
          let error_msg = match err with
            | Chain_orchestrator_eio.DesignFailed msg -> Printf.sprintf "Design failed: %s" msg
            | Chain_orchestrator_eio.CompileFailed msg -> Printf.sprintf "Compile failed: %s" msg
            | Chain_orchestrator_eio.ExecutionFailed msg -> Printf.sprintf "Execution failed: %s" msg
            | Chain_orchestrator_eio.VerificationFailed msg -> Printf.sprintf "Verification failed: %s" msg
            | Chain_orchestrator_eio.MaxReplansExceeded -> "Max replans exceeded"
            | Chain_orchestrator_eio.Timeout -> "Orchestration timeout"
          in
          { model = "chain.orchestrate";
            returncode = 1;
            response = error_msg;
            extra = [("error_type", Yojson.Safe.to_string (Chain_orchestrator_eio.orchestration_error_to_yojson err))]; })

  | GhPrDiff { repo; pr_number } ->
      execute_gh_pr_diff ~sw ~proc_mgr ~clock ~repo ~pr_number

  | SlackPost { channel; text; thread_ts } ->
      execute_slack_post ~sw ~proc_mgr ~clock ~channel ~text ~thread_ts

  | ChainCheckpoints { chain_id; max_age_hours; cleanup } ->
      let _ = (sw, proc_mgr, clock) in  (* Unused but needed for signature consistency *)
      let store = Checkpoint_store.create () in
      if cleanup then
        (* Cleanup mode: delete old checkpoints *)
        let max_hours = Option.value max_age_hours ~default:168 in  (* Default: 1 week *)
        let deleted = Checkpoint_store.cleanup_old store ~max_age_hours:max_hours in
        { model = "chain.checkpoints";
          returncode = 0;
          response = sprintf "Deleted %d checkpoints older than %d hours" deleted max_hours;
          extra = [("deleted", string_of_int deleted); ("max_age_hours", string_of_int max_hours)]; }
      else
        (* List mode: show checkpoints *)
        let checkpoints = match chain_id with
          | Some cid -> Checkpoint_store.list_checkpoints store ~chain_id:cid
          | None -> Checkpoint_store.list_all store
        in
        let filtered = match max_age_hours with
          | Some hours ->
              let now = Unix.gettimeofday () in
              let max_age_secs = float_of_int hours *. 3600.0 in
              List.filter (fun cp -> now -. cp.Checkpoint_store.timestamp < max_age_secs) checkpoints
          | None -> checkpoints
        in
        let json_list = filtered |> List.map (fun cp ->
          `Assoc [
            ("run_id", `String cp.Checkpoint_store.run_id);
            ("chain_id", `String cp.Checkpoint_store.chain_id);
            ("node_id", `String cp.Checkpoint_store.node_id);
            ("timestamp", `Float cp.Checkpoint_store.timestamp);
            ("outputs_count", `Int (List.length cp.Checkpoint_store.outputs));
          ]
        ) in
        let response = Yojson.Safe.pretty_to_string (`List json_list) in
        { model = "chain.checkpoints";
          returncode = 0;
          response;
          extra = [("count", string_of_int (List.length filtered))]; }

  | ChainResume { run_id; trace } ->
      let store = Checkpoint_store.create () in
      (match Checkpoint_store.load store ~run_id with
       | Error msg ->
           { model = "chain.resume";
             returncode = -1;
             response = sprintf "Failed to load checkpoint: %s" msg;
             extra = [("run_id", run_id)]; }
       | Ok cp ->
           (* Load the original chain from registry or reparse *)
           let chain_id = cp.Checkpoint_store.chain_id in
           (match Chain_registry.lookup chain_id with
            | None ->
                { model = "chain.resume";
                  returncode = -1;
                  response = sprintf "Chain '%s' not found in registry. Cannot resume without chain definition." chain_id;
                  extra = [("run_id", run_id); ("chain_id", chain_id)]; }
            | Some chain ->
                (match Chain_compiler.compile chain with
                 | Error msg ->
                     { model = "chain.resume";
                       returncode = -1;
                       response = sprintf "Failed to compile chain: %s" msg;
                       extra = [("run_id", run_id); ("chain_id", chain_id)]; }
                 | Ok plan ->
                     let node_timeout = chain.Chain_types.config.Chain_types.timeout in
                     let starts_with ~prefix s =
                       let prefix_len = String.length prefix in
                       String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
                     in
                     let split_tool_name name =
                       match String.index_opt name '.' with
                       | None -> None
                       | Some idx ->
                           let server = String.sub name 0 idx in
                           let tool_len = String.length name - idx - 1 in
                           if server = "" || tool_len <= 0 then None
                           else Some (server, String.sub name (idx + 1) tool_len)
                     in
                     (* Create exec_fn that routes to appropriate LLM *)
                     let exec_fn ~model ?system ~prompt ?tools () =
                       let _ = system in
                       let parsed_tools = match tools with
                         | None -> None
                         | Some json ->
                             let open Yojson.Safe.Util in
                             match json with
                             | `List items ->
                                 let schemas = List.filter_map (fun item ->
                                   try
                                     Some {
                                       Types.name = item |> member "name" |> to_string;
                                      description = (match item |> member "description" |> to_string_option with Some s -> s | None -> "");
                                      input_schema = item |> member "parameters";
                                     }
                                   with _ -> None
                                 ) items in
                                 if List.length schemas > 0 then Some schemas else None
                             | _ -> None
                       in
                       let model_name = String.lowercase_ascii model in
                       let result =
                         if starts_with ~prefix:"gemini" model_name then
                           execute ~sw ~proc_mgr ~clock (Gemini {
                             prompt; model; thinking_level = High; yolo = false; timeout = node_timeout; stream = false
                           })
                         else if starts_with ~prefix:"claude" model_name then
                           execute ~sw ~proc_mgr ~clock (Claude {
                             prompt; model; long_context = false; system_prompt = None;
                             output_format = Text; allowed_tools = []; working_directory = "";
                             timeout = node_timeout; stream = false
                           })
                         else if starts_with ~prefix:"codex" model_name || starts_with ~prefix:"gpt" model_name then
                           execute ~sw ~proc_mgr ~clock (Codex {
                             prompt; model; reasoning_effort = RHigh; sandbox = ReadOnly;
                             working_directory = None; timeout = node_timeout; stream = false
                           })
                         else if starts_with ~prefix:"ollama" model_name then
                           let ollama_model = match String.index_opt model ':' with
                             | Some idx -> String.sub model (idx + 1) (String.length model - idx - 1)
                             | None -> "devstral"
                           in
                           execute ~sw ~proc_mgr ~clock (Ollama {
                             prompt; model = ollama_model; system_prompt = None;
                             temperature = 0.7; timeout = node_timeout; stream = false; tools = parsed_tools
                           })
                         else
                           { model = sprintf "unknown:%s" model; returncode = 1;
                             response = sprintf "Unknown model: %s" model; extra = [] }
                       in
                       if result.returncode = 0 then Ok result.response
                       else Error (sprintf "LLM error: %s" result.response)
                     in
                     let tool_exec ~name ~args:tool_args =
                       match split_tool_name name with
                       | Some (server_name, tool_name) ->
                           let output = call_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments:tool_args ~timeout:node_timeout in
                           Ok output
                       | None ->
                           Error (sprintf "Unknown tool: %s" name)
                     in
                     (* Create checkpoint config for resume *)
                     let checkpoint_config = Chain_executor_eio.make_checkpoint_config
                       ~store ~enabled:true ~resume_from:run_id ()
                     in
                     let result = Chain_executor_eio.execute
                       ~sw ~clock ~timeout:node_timeout ~trace ~exec_fn ~tool_exec
                       ~checkpoint:checkpoint_config plan
                     in
                     { model = "chain.resume";
                       returncode = if result.Chain_types.success then 0 else -1;
                       response = result.Chain_types.output;
                       extra = [
                         ("run_id", run_id);
                         ("chain_id", chain_id);
                         ("resumed_from_node", cp.Checkpoint_store.node_id);
                         ("duration_ms", string_of_int result.Chain_types.duration_ms);
                       ]; })))

  (* ============= Prompt Registry Tools ============= *)

  | PromptRegister { id; template; version } ->
      let version = Option.value version ~default:"1.0.0" in
      let variables = Prompt_registry.extract_variables template in
      let entry : Prompt_registry.prompt_entry = {
        id;
        template;
        version;
        variables;
        metrics = None;
        created_at = Unix.gettimeofday ();
        deprecated = false;
      } in
      Prompt_registry.register entry;
      { model = "prompt.register";
        returncode = 0;
        response = sprintf "Registered prompt '%s' v%s with %d variables: %s"
          id version
          (List.length variables)
          (String.concat ", " variables);
        extra = [
          ("id", id);
          ("version", version);
          ("variables", String.concat ", " variables);
        ]; }

  | PromptList ->
      let all = Prompt_registry.list_all () in
      let json_list = List.map (fun (e : Prompt_registry.prompt_entry) ->
        `Assoc [
          ("id", `String e.id);
          ("version", `String e.version);
          ("variables", `List (List.map (fun v -> `String v) e.variables));
          ("deprecated", `Bool e.deprecated);
          ("metrics", match e.metrics with
            | Some m -> `Assoc [
                ("usage_count", `Int m.Prompt_registry.usage_count);
                ("avg_score", `Float m.Prompt_registry.avg_score);
              ]
            | None -> `Null);
        ]
      ) all in
      { model = "prompt.list";
        returncode = 0;
        response = Yojson.Safe.pretty_to_string (`List json_list);
        extra = [("count", string_of_int (List.length all))]; }

  | PromptGet { id; version } ->
      (match Prompt_registry.get ~id ?version () with
       | Some entry ->
           { model = "prompt.get";
             returncode = 0;
             response = sprintf "# %s v%s\n\n%s\n\nVariables: %s"
               entry.Prompt_registry.id
               entry.Prompt_registry.version
               entry.Prompt_registry.template
               (String.concat ", " entry.Prompt_registry.variables);
             extra = [
               ("id", entry.Prompt_registry.id);
               ("version", entry.Prompt_registry.version);
             ]; }
       | None ->
           { model = "prompt.get";
             returncode = 1;
             response = sprintf "Prompt '%s'%s not found"
               id (match version with Some v -> " v" ^ v | None -> "");
             extra = []; })

(** {1 Convenience Wrappers} *)

(** Execute with Eio env (extracts proc_mgr and clock) *)
let execute_with_env ~sw ~env args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  execute ~sw ~proc_mgr ~clock args

(** Execute with Langfuse tracing - wraps execute for observability *)
let execute_with_tracing ~sw ~proc_mgr ~clock args : tool_result =
  let model_name = get_model_name_for_tracing args in
  let input_str = get_input_for_tracing args in

  (* Create Langfuse trace and generation if enabled *)
  let trace =
    if Langfuse.is_enabled () then
      Some (Langfuse.create_trace ~name:("tool:" ^ model_name) ())
    else None
  in
  let gen = match trace with
    | Some t ->
        Some (Langfuse.create_generation ~trace:t ~name:model_name ~model:model_name ~input:input_str ())
    | None -> None
  in

  (* Execute the actual tool *)
  let result = execute ~sw ~proc_mgr ~clock args in

  (* End Langfuse generation and trace *)
  (match gen with
   | Some g ->
       if result.returncode = 0 then
         Langfuse.end_generation g ~output:result.response ~prompt_tokens:0 ~completion_tokens:0
       else
         Langfuse.error_generation g ~message:result.response
   | None -> ());
  (match trace with
   | Some t ->
       t.Langfuse.metadata <- [
         ("model", model_name);
         ("returncode", string_of_int result.returncode);
         ("response_length", string_of_int (String.length result.response));
       ];
       Langfuse.end_trace t
   | None -> ());

  result

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
            let result = json |> member "result" in
            let error = json |> member "error" in
            if error <> `Null then
              let msg = try error |> member "message" |> to_string
                        with _ -> "Unknown error" in
              (tc.name, sprintf "Error: %s" msg)
            else
              let content = result |> member "content" in
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
      | Ok (Ollama_parser.TextResponse (text, thinking)) ->
          let response = match thinking with
            | Some t -> sprintf "[Thinking]\n%s\n\n[Response]\n%s" t text
            | None -> text
          in
          TurnDone response
      | Ok (Ollama_parser.ToolCalls (calls, _thinking)) -> TurnContinue calls
      | Ok (Ollama_parser.TextWithTools (text, calls, thinking)) ->
          if calls = [] then
            let response = match thinking with
              | Some t -> sprintf "[Thinking]\n%s\n\n[Response]\n%s" t text
              | None -> text
            in
            TurnDone response
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

(* ============================================================================
   Chain Engine - Workflow Orchestration DSL Execution
   ============================================================================ *)

(** Execute a chain DSL definition *)
let execute_chain ~sw ~proc_mgr ~clock ~(chain_json : Yojson.Safe.t) ~trace ~timeout =
  match Chain_parser.parse_chain chain_json with
  | Error msg ->
      {
        model = "chain.run";
        returncode = -1;
        response = sprintf "Chain parse error: %s" msg;
        extra = [("error", "parse_error")];
      }
  | Ok chain ->
      (match Chain_compiler.compile chain with
      | Error msg ->
          {
            model = "chain.run";
            returncode = -1;
            response = sprintf "Chain compile error: %s" msg;
            extra = [("error", "compile_error")];
          }
      | Ok plan ->
          let starts_with ~prefix s =
            let prefix_len = String.length prefix in
            String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
          in
          let split_tool_name name =
            match String.index_opt name '.' with
            | None -> None
            | Some idx ->
                let server = String.sub name 0 idx in
                let tool_len = String.length name - idx - 1 in
                if server = "" || tool_len <= 0 then None
                else Some (server, String.sub name (idx + 1) tool_len)
          in
          let exec_fn ~model ?system ~prompt ?tools () =
            let _ = system in  (* Unused for now, available for future enhancement *)
            (* Convert Yojson.Safe.t tools to tool_schema list option *)
            let parsed_tools = match tools with
              | None -> None
              | Some json ->
                  let open Yojson.Safe.Util in
                  match json with
                  | `List items ->
                      let schemas = List.filter_map (fun item ->
                        try
                          Some {
                            Types.name = item |> member "name" |> to_string;
                            description = item |> member "description" |> to_string;
                            input_schema = item |> member "input_schema";
                          }
                        with _ -> None
                      ) items in
                      if schemas = [] then None else Some schemas
                  | _ -> None
            in
            match model with
            | "stub" | "mock" ->
                Ok (sprintf "[%s]%s" model prompt)
            | "gemini" ->
                let args = Gemini {
                  prompt;
                  model = "gemini-3-pro-preview";
                  thinking_level = High;
                  yolo = false;
                  timeout;
                  stream = false;
                } in
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response else Error result.response
            | "claude" ->
                let working_directory =
                  Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp"
                in
                let args = Claude {
                  prompt;
                  model = "opus";
                  long_context = true;
                  system_prompt = None;
                  output_format = Text;
                  allowed_tools = [];
                  working_directory;
                  timeout;
                  stream = false;
                } in
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response else Error result.response
            | "codex" ->
                let args = Codex {
                  prompt;
                  model = "gpt-5.2-codex";
                  reasoning_effort = RXhigh;
                  sandbox = WorkspaceWrite;
                  working_directory = None;
                  timeout;
                  stream = false;
                } in
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response else Error result.response
            | m when String.length m > 7 && String.sub m 0 7 = "ollama:" ->
                let ollama_model = String.sub m 7 (String.length m - 7) in
                let args = Ollama {
                  prompt;
                  model = ollama_model;
                  system_prompt = None;
                  temperature = 0.7;
                  timeout;
                  stream = false;
                  tools = parsed_tools;
                } in
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response else Error result.response
            | _ ->
                let args = Ollama {
                  prompt;
                  model;
                  system_prompt = None;
                  temperature = 0.7;
                  timeout;
                  stream = false;
                  tools = parsed_tools;
                } in
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response else Error result.response
          in
          let tool_exec ~name ~args =
            match split_tool_name name with
            | Some (server_name, tool_name) ->
                let output =
                  call_mcp ~sw ~proc_mgr ~clock
                    ~server_name ~tool_name ~arguments:args ~timeout
                in
                if starts_with ~prefix:"Error:" output then Error output else Ok output
            | None ->
                let result =
                  match name with
                  | "gemini" ->
                      let args = parse_gemini_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "claude-cli" ->
                      let args = parse_claude_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "codex" ->
                      let args = parse_codex_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "ollama" ->
                      let args = parse_ollama_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "ollama_list" ->
                      let args = parse_ollama_list_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "echo" ->
                      (* Simple echo tool for testing *)
                      let input = try args |> Yojson.Safe.Util.member "input" |> Yojson.Safe.Util.to_string
                                  with _ -> Yojson.Safe.to_string args in
                      { Types.model = "echo"; returncode = 0; response = input; extra = [] }
                  | "identity" ->
                      (* Identity tool: returns args unchanged *)
                      { Types.model = "identity"; returncode = 0; response = Yojson.Safe.to_string args; extra = [] }
                  | _ ->
                      { Types.model = "chain.tool";
                        returncode = -1;
                        response = sprintf "Unknown tool: %s" name;
                        extra = [("tool", name)];
                      }
                in
                if result.returncode = 0 then Ok result.response else Error result.response
          in
          let result = Chain_executor_eio.execute ~sw ~clock ~timeout ~trace ~exec_fn ~tool_exec plan in
          {
            model = sprintf "chain.run (%s)" chain.Chain_types.id;
            returncode = (if result.success then 0 else -1);
            response = result.output;
            extra = [
              ("chain_id", chain.Chain_types.id);
              ("duration_ms", string_of_int result.duration_ms);
              ("trace", if trace then "enabled" else "disabled");
            ];
          })

(** Validate a chain DSL definition without executing *)
let validate_chain ~(chain_json : Yojson.Safe.t) =
  match Chain_parser.parse_chain chain_json with
  | Error msg ->
      {
        model = "chain.validate";
        returncode = -1;
        response = sprintf "Parse error: %s" msg;
        extra = [("valid", "false"); ("error_type", "parse")];
      }
  | Ok chain ->
      (match Chain_compiler.compile chain with
      | Error msg ->
          {
            model = "chain.validate";
            returncode = -1;
            response = sprintf "Compile error: %s" msg;
            extra = [("valid", "false"); ("error_type", "compile")];
          }
      | Ok _plan ->
          {
            model = "chain.validate";
            returncode = 0;
            response = sprintf "Chain '%s' is valid" chain.Chain_types.id;
            extra = [
              ("valid", "true");
              ("chain_id", chain.Chain_types.id);
            ];
          })

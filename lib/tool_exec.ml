(** Tool Execution - Main dispatcher for all MCP tools *)

open Printf
open Types
open Cli_runner_eio

(* Import from split modules *)
module Mcp_client = Mcp_client
module Llm_clients = Llm_clients

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
  | Glm { model; _ } -> sprintf "glm:%s" model
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
  | SetStreamDelta _ -> "config:set_stream_delta"
  | GetStreamDelta -> "config:get_stream_delta"

(** Extract input/prompt from tool_args for tracing *) 
let get_input_for_tracing = function
  | Gemini { prompt; _ } -> prompt
  | Claude { prompt; _ } -> prompt
  | Codex { prompt; _ } -> prompt
  | Ollama { prompt; _ } -> prompt
  | OllamaList -> "(list models)"
  | Glm { prompt; _ } -> prompt
  | ChainRun { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainValidate { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainOrchestrate { chain; _ } ->
      (match chain with
       | Some j -> sprintf "(orchestrate: %s)" (Yojson.Safe.to_string j)
       | None -> "(orchestrate: preset)")
  | _ -> "(non-llm operation)"

let classify_error_class (r : tool_result) : string option =
  if r.returncode = 0 then None
  else if String.length r.response >= 7 && String.sub r.response 0 7 = "Timeout" then
    Some "timeout"
  else if String.length r.response >= 6 && String.sub r.response 0 6 = "Error:" then
    Some "tool_error"
  else
    Some "llm_error"

let result_streamed (r : tool_result) : bool =
  match List.assoc_opt "streamed" r.extra with
  | Some "true" -> true
  | _ -> false

(** {1 Main Execute Function} *)

(** Execute a tool and return result - Direct Style *) 
let rec execute ~sw ~proc_mgr ~clock args : tool_result =
  let log_enabled = Run_log_eio.enabled () in
  let stream_flag = match args with
    | Ollama { stream; _ } -> stream
    | Gemini { stream; _ } -> stream
    | Claude { stream; _ } -> stream
    | Codex { stream; _ } -> stream
    | Glm { stream; _ } -> stream
    | _ -> false
  in
  let model_for_log = get_model_name_for_tracing args in
  let stream_id_opt =
    match args with
    | Ollama { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Gemini { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Claude { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Codex { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Glm { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | _ -> None
  in
  let log_extra =
    match stream_id_opt with
    | Some sid -> [("stream_id", sid)]
    | None -> []
  in
  let tool_for_log = match args with
    | ChainRun _ -> "chain.run"
    | ChainValidate _ -> "chain.validate"
    | ChainList -> "chain.list"
    | ChainToMermaid _ -> "chain.to_mermaid"
    | ChainVisualize _ -> "chain.visualize"
    | ChainConvert _ -> "chain.convert"
    | ChainOrchestrate _ -> "chain.orchestrate"
    | ChainCheckpoints _ -> "chain.checkpoints"
    | ChainResume _ -> "chain.resume"
    | OllamaList -> "ollama.list"
    | PromptRegister _ -> "prompt.register"
    | PromptList -> "prompt.list"
    | PromptGet _ -> "prompt.get"
    | GhPrDiff _ -> "gh.pr.diff"
    | SlackPost _ -> "slack.post"
    | _ -> "llm"
  in
  let prompt_for_log = get_input_for_tracing args in
  let log_start_ts = Unix.gettimeofday () in
  if log_enabled then
    Run_log_eio.record_event
      ~event:"llm_call"
      ~tool:tool_for_log
      ~model:model_for_log
      ~prompt_chars:(String.length prompt_for_log)
      ~streamed:stream_flag
      ~extra:log_extra
      ()
  else
    ();
  match args with
  | Gemini { prompt; model; thinking_level; timeout; stream; use_cli; _ } ->
      let result =
        match String.lowercase_ascii model with
        | "stub" | "mock" ->
            { Types.model = "stub"; returncode = 0; response = sprintf "[stub]%s" prompt; extra = [] }
        | "simple-test" ->
            { Types.model = "simple-test"; returncode = 0; response = sprintf "[test]%s" prompt; extra = [] }
        | _ ->
            if use_cli then
              Llm_clients.execute_gemini_with_retry ~sw ~proc_mgr ~clock ~model ~thinking_level ~timeout ~stream ~args ()
            else
              Llm_clients.execute_gemini_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~thinking_level ~timeout ~stream
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Unix.gettimeofday () -. log_start_ts) *. 1000.0))
          ~success:(result.returncode = 0)
          ~streamed:(result_streamed result)
          ~extra:log_extra
          ?error_class:(classify_error_class result)
          ()
      else
        ();
      result

  | Claude { prompt; model; long_context; system_prompt; working_directory = _; timeout; stream; use_cli; fallback_to_api; _ } ->
      let model_name = sprintf "claude-cli (%s)" model in
      let extra_base = [("long_context", string_of_bool long_context)] in

      let execute_via_cli () =
        match Tool_parsers.build_claude_cmd args with
        | Error err ->
            { model = model_name;
              returncode = -1;
              response = err;
              extra = extra_base @ [("invalid_args", "true")]; }
        | Ok cmd_list ->
            let cmd = List.hd cmd_list in
            let cmd_args = List.tl cmd_list in
            if stream then
              Llm_clients.execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args
            else begin
              let result = run_command ~sw ~proc_mgr ~clock ~safe_tmpdir:true ~timeout cmd cmd_args in
              match result with
              | Ok r ->
                  { model = model_name;
                    returncode = r.exit_code;
                    response = get_output r;
                    extra = extra_base; }
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
      in

      let result =
        if use_cli then begin
          let cli_result = execute_via_cli () in
          if cli_result.returncode <> 0 && fallback_to_api then begin
            let api_result = Llm_clients.execute_claude_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~system_prompt ~timeout ~stream in
            if api_result.returncode = 0 then
              { api_result with extra = api_result.extra @ [("fallback_used", "true")] }
            else
              { cli_result with extra = cli_result.extra @ [("fallback_attempted", "true"); ("fallback_error", api_result.response)] }
          end else
            cli_result
        end else
          Llm_clients.execute_claude_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~system_prompt ~timeout ~stream
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Unix.gettimeofday () -. log_start_ts) *. 1000.0))
          ~success:(result.returncode = 0)
          ~streamed:(result_streamed result)
          ~extra:log_extra
          ?error_class:(classify_error_class result)
          ()
      else
        ();
      result

  | Codex { prompt; model; reasoning_effort; timeout; stream; use_cli; fallback_to_api; _ } ->
      let model_name = sprintf "codex (%s)" model in
      let extra_base = [("reasoning_effort", string_of_reasoning_effort reasoning_effort)] in

      let execute_via_cli () =
        match Tool_parsers.build_codex_cmd args with
        | Error err ->
            { model = model_name;
              returncode = -1;
              response = err;
              extra = extra_base @ [("invalid_args", "true")]; }
        | Ok cmd_list ->
            let cmd = List.hd cmd_list in
            let cmd_args = List.tl cmd_list in
            if stream then begin
              let stream_result = Llm_clients.execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args in
              { stream_result with response = Tool_parsers.clean_codex_output stream_result.response }
            end else begin
              let result = run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args in
              match result with
              | Ok r ->
                  { model = model_name;
                    returncode = r.exit_code;
                    response = Tool_parsers.clean_codex_output (get_output r);
                    extra = extra_base; }
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
      in

      let result =
        if use_cli then begin
          let cli_result = execute_via_cli () in
          if cli_result.returncode <> 0 && fallback_to_api then begin
            let api_result = Llm_clients.execute_codex_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~timeout ~stream in
            if api_result.returncode = 0 then
              { api_result with extra = api_result.extra @ [("fallback_used", "true")] }
            else
              { cli_result with extra = cli_result.extra @ [("fallback_attempted", "true"); ("fallback_error", api_result.response)] }
          end else
            cli_result
        end else
          Llm_clients.execute_codex_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~timeout ~stream
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Unix.gettimeofday () -. log_start_ts) *. 1000.0))
          ~success:(result.returncode = 0)
          ~streamed:(result_streamed result)
          ~extra:log_extra
          ?error_class:(classify_error_class result)
          ()
      else
        ();
      result

  | Ollama { model; temperature; timeout; tools; stream; _ } ->
      let has_tools = match tools with Some l when List.length l > 0 -> true | _ -> false in
      let execute_nonstream () =
        match Tool_parsers.build_ollama_curl_cmd ~force_stream:(Some false) args with
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
                        let calls_json = Tool_parsers.tool_calls_to_json calls in
                        let extra = [("tool_calls", calls_json)] in
                        let extra = match thinking with
                          | Some t -> ("thinking", t) :: extra
                          | None -> extra
                        in
                        ("", extra)
                    | Ok (Ollama_parser.TextWithTools (text, calls, thinking)) ->
                        let calls_json = Tool_parsers.tool_calls_to_json calls in
                        let extra = [("tool_calls", calls_json)] in
                        let extra = match thinking with
                          | Some t -> ("thinking", t) :: extra
                          | None -> extra
                        in
                        (text, extra)
                    | Error err -> (sprintf "Error: %s" err, [])
                  end else begin
                    match Tool_parsers.parse_ollama_response r.stdout with
                    | Ok resp -> (resp, [])
                    | Error err -> (sprintf "Error: %s" err, [])
                  end
                in
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
                  extra = [("temperature", sprintf "%.1f" temperature); ("local", "true")]; }
      in
      let result =
        if stream then
          let streaming =
            Llm_clients.execute_ollama_streaming ~sw ~proc_mgr ~clock ~on_token:(fun _ -> ())
              ?stream_id:stream_id_opt args
          in
          if streaming.returncode = 0 then streaming
          else begin
            if log_enabled then
              Run_log_eio.record_event
                ~event:"stream_fallback"
                ~tool:tool_for_log
                ~model:model_for_log
                ~success:false
                ~error_class:"stream_fallback"
                ~error:streaming.response
                ~extra:([("fallback", "non_stream")] @ log_extra)
                ()
            else
              ();
            let fallback = execute_nonstream () in
            (match stream_id_opt with
             | Some sid ->
                 { fallback with extra = fallback.extra @ [("stream_id", sid); ("stream_fallback", "true")] }
             | None -> fallback)
          end
        else
          execute_nonstream ()
      in
      let result =
        match stream_id_opt with
        | Some sid ->
            if List.assoc_opt "stream_id" result.extra = None then
              { result with extra = result.extra @ [("stream_id", sid)] }
            else result
        | None -> result
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Unix.gettimeofday () -. log_start_ts) *. 1000.0))
          ~success:(result.returncode = 0)
          ~streamed:(result_streamed result)
          ~extra:log_extra
          ?error_class:(classify_error_class result)
          ()
      else
        ();
      result

  | Glm args ->
      (* Delegate GLM execution to Llm_clients *) 
      Llm_clients.execute_gemini_direct_api ~sw ~proc_mgr ~clock ~model:args.model ~prompt:args.prompt ~thinking_level:Types.Low ~timeout:args.timeout ~stream:args.stream (* Stub - GLM needs porting to Llm_clients if it was in original tools_eio *)

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
        | (_, Some m) -> Chain_mermaid_parser.parse_mermaid_to_chain m
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
                let delim_idx =
                  match String.index_opt name '.' with
                  | Some idx -> Some idx
                  | None -> String.index_opt name ':'
                in
                match delim_idx with
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
                  | "stub" | "mock" | "simple-test" ->
                      (* Stub model for tests and local smoke runs *) 
                      Types.Gemini {
                        prompt;
                        model = (if String.lowercase_ascii model = "simple-test" then "simple-test" else "stub");
                        thinking_level = Types.Low;
                        yolo = false;
                        output_format = Types.Text;
                        timeout = node_timeout;
                        stream = false;
                        use_cli = false;  (* Stub uses direct API *) 
                        fallback_to_api = false;
                      }
                  | "gemini" | "gemini-3-pro-preview" | "gemini-2.5-pro" ->
                      Types.Gemini {
                        prompt;
                        model = "gemini-3-pro-preview";
                        thinking_level = Types.High;
                        yolo = false;
                        output_format = Types.Text;
                        timeout = node_timeout;
                        stream = false;
                        use_cli = true;  (* MASC integration enabled *) 
                        fallback_to_api = true;
                      }
                  | "claude" | "opus" | "opus-4" | "sonnet" | "claude-3-5-sonnet" | "haiku" | "haiku-4.5" ->
                      Types.Claude {
                        prompt;
                        model = (if model = "sonnet" || model = "claude-3-5-sonnet" then "claude-3-5-sonnet-latest" else model);
                        long_context = true;
                        system_prompt = None;
                        output_format = Types.Text;
                        allowed_tools = [];
                        working_directory = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp";
                        timeout = node_timeout;
                        stream = false;
                        use_cli = true;
                        fallback_to_api = true;
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
                        use_cli = true;
                        fallback_to_api = true;
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
                  | "glm" | "glm-4.7" | "glm-4.6" | "glm-4.5" ->
                      Types.Glm {
                        prompt;
                        model = "glm-4.7";
                        system_prompt = None;
                        temperature = 0.7;
                        max_tokens = Some 4096;
                        timeout = node_timeout;
                        stream = false;
                        thinking = false;  (* Faster for chain execution *) 
                        do_sample = true;
                        web_search = false;
                      }
                  | _ ->
                      (* Default to Gemini for unknown models *) 
                      Types.Gemini {
                        prompt;
                        model = "gemini-3-pro-preview";
                        thinking_level = Types.High;
                        yolo = false;
                        output_format = Types.Text;
                        timeout = node_timeout;
                        stream = false;
                        use_cli = true;  (* MASC integration enabled *) 
                        fallback_to_api = true;
                      }
                in
                match args with
                | Types.Gemini { model = "stub"; _ } ->
                    Ok (Printf.sprintf "[stub]%s" prompt)
                | Types.Gemini { model = "simple-test"; _ } ->
                    Ok (Printf.sprintf "[test]%s" prompt)
                | _ ->
                    let result = execute ~sw ~proc_mgr ~clock args in
                    if result.returncode = 0 then Ok result.response
                    else Error result.response
              in
              let tool_exec ~name ~args =
                match split_tool_name name with
                | Some (server_name, tool_name) ->
                    let output =
                      Mcp_client.call_mcp ~sw ~proc_mgr ~clock
                        ~server_name ~tool_name ~arguments:args ~timeout:node_timeout
                    in
                    if starts_with ~prefix:"Error:" output then Error output else Ok output
                | None ->
                    let result =
                      match name with
                      | "gemini" ->
                          let args = Tool_parsers.parse_gemini_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "claude-cli" ->
                          let args = Tool_parsers.parse_claude_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "codex" ->
                          let args = Tool_parsers.parse_codex_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "ollama" ->
                          let args = Tool_parsers.parse_ollama_args args in
                          execute ~sw ~proc_mgr ~clock args
                      | "ollama_list" ->
                          let args = Tool_parsers.parse_ollama_list_args args in
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
              let result = 
                Mcp_client.with_masc_hook ~sw ~proc_mgr ~clock ~label:"chain.run" (fun () ->
                  Chain_executor_eio.execute
                    ~sw ~clock ~timeout:node_timeout ~trace:trace_effective ~exec_fn ~tool_exec ?input plan)
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


  | ChainValidate { chain; mermaid; strict } ->
      (* Parse from either JSON or Mermaid, then validate *) 
      let parse_result = match (chain, mermaid) with
        | (Some c, _) -> Chain_parser.parse_chain c
        | (_, Some m) -> Chain_mermaid_parser.parse_mermaid_to_chain m
        | (None, None) -> Error "Either 'chain' (JSON) or 'mermaid' (string) is required"
      in
      (match parse_result with
      | Error msg ->
          { model = "chain.validate";
            returncode = -1;
            response = sprintf "Parse error: %s" msg;
            extra = [("stage", "parse"); ("valid", "false")]; }
      | Ok parsed_chain ->
          let validation =
            if strict then Chain_parser.validate_chain_strict parsed_chain
            else Chain_parser.validate_chain parsed_chain
          in
          match validation with
          | Error msg ->
              { model = "chain.validate";
                returncode = -1;
                response = sprintf "Validation error: %s" msg;
                extra = [("stage", "validate"); ("valid", "false")];
              }
          | Ok () ->
              (* Also try to compile to check DAG validity *) 
              match Chain_compiler.compile parsed_chain with
              | Error msg ->
                  { model = "chain.validate";
                    returncode = -1;
                    response = sprintf "Compile error: %s" msg;
                    extra = [("stage", "compile"); ("valid", "false")];
                  }
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
                      ("strict", if strict then "true" else "false");
                      ("nodes", string_of_int node_count);
                      ("depth", string_of_int depth);
                    ]; })

  | ChainList ->
      let chains = Chain_registry.list_entries () in
      let json = `List (List.map (fun (id, entry) ->
        `Assoc [
          ("id", `String id);
          ("description", match entry.Chain_registry.description with Some d -> `String d | None -> `Null);
          ("registered_at", `Float entry.registered_at);
          ("version", `Int entry.version);
        ]
      ) chains) in
      { model = "chain.list";
        returncode = 0;
        response = Yojson.Safe.to_string json;
        extra = [("count", string_of_int (List.length chains))]; }

  | ChainToMermaid { chain } ->
      (match Chain_parser.parse_chain chain with
       | Ok parsed_chain ->
           let result = Chain_mermaid_parser.chain_to_mermaid parsed_chain in
           { model = "chain.to_mermaid"; returncode = 0; response = result; extra = [] }
       | Error e ->
           { model = "chain.to_mermaid"; returncode = -1; response = sprintf "Parse error: %s" e; extra = [] })

  | ChainVisualize { chain } ->
      (match Chain_parser.parse_chain chain with
       | Ok parsed_chain ->
           let result = Chain_mermaid_parser.chain_to_ascii parsed_chain in
           { model = "chain.visualize"; returncode = 0; response = result; extra = [] }
       | Error e ->
           { model = "chain.visualize"; returncode = -1; response = sprintf "Parse error: %s" e; extra = [] })

  | ChainConvert { from_format; to_format; input; pretty } ->
      let result = match (String.lowercase_ascii from_format, String.lowercase_ascii to_format) with
        | ("json", "mermaid") ->
            (match Chain_parser.parse_chain input with
             | Ok chain ->
                 Ok (Chain_mermaid_parser.chain_to_mermaid chain)
             | Error e -> Error (sprintf "JSON parse error: %s" e))
        | ("mermaid", "json") ->
            (match input with
             | `String s ->
                 (match Chain_mermaid_parser.parse_mermaid_to_chain s with
                  | Ok chain ->
                      Ok (Chain_parser.chain_to_json_string ~pretty chain)
                  | Error e -> Error (sprintf "Mermaid parse error: %s" e))
             | _ -> Error "Input must be a string for Mermaid")
        | _ -> Error "Unsupported conversion"
      in
      (match result with
       | Ok output ->
           { model = "chain.convert";
             returncode = 0;
             response = output;
             extra = []; }
       | Error e ->
           { model = "chain.convert";
             returncode = -1;
             response = e;
             extra = []; })

  | ChainOrchestrate _ ->
      (* This tool is handled by Chain_orchestrator_eio directly in the server *) 
      { model = "chain.orchestrate";
        returncode = -1;
        response = "chain.orchestrate should be handled by server router, not Tools_eio";
        extra = []; }

  | ChainCheckpoints { chain_id; max_age_hours; cleanup } ->
      (if cleanup then
         let days = match max_age_hours with Some h -> h / 24 | None -> 7 in
         Chain_run_store.cleanup_old_runs ~days;
         { model = "chain.checkpoints"; returncode = 0; response = "Cleanup complete"; extra = [] }
       else
         let runs = match chain_id with
           | Some id -> Chain_run_store.list_runs_by_chain id
           | None -> Chain_run_store.list_runs ()
         in
         let json = `List (List.map (fun r ->
           Chain_run_store.run_to_json r
         ) runs) in
         { model = "chain.checkpoints"; returncode = 0; response = Yojson.Safe.to_string json; extra = [] })

    | ChainResume { run_id; trace=_trace } ->
        (match Chain_run_store.get_run ~run_id with
         | None ->
             { model = "chain.resume";
               returncode = -1;
               response = sprintf "Run not found: %s" run_id;
               extra = []; }
         | Some _run ->
             (* TODO: Implement resume logic *)
             { model = "chain.resume";
               returncode = -1;
               response = "Resume logic not implemented yet";
               extra = []; })
  | PromptRegister { id; template; version } ->
      let entry = {
        Prompt_registry.id;
        template;
        version = Option.value version ~default:"1.0.0";
        variables = []; (* Auto-extracted *)
        metrics = None;
        created_at = Unix.gettimeofday ();
        deprecated = false;
      } in
      Prompt_registry.register entry;
      { model = "prompt.register"; returncode = 0; response = "Prompt registered"; extra = [] }

  | PromptList ->
      let prompts = Prompt_registry.list_all () in
      let json = `List (List.map Prompt_registry.prompt_entry_to_yojson prompts) in
      { model = "prompt.list"; returncode = 0; response = Yojson.Safe.to_string json; extra = [] }

  | PromptGet { id; version } ->
      (match Prompt_registry.get ~id ?version () with
       | Some prompt ->
           { model = "prompt.get"; returncode = 0; response = prompt.template; extra = [] }
       | None ->
           { model = "prompt.get"; returncode = -1; response = "Prompt not found"; extra = [] })

  | GhPrDiff { repo; pr_number } ->
      execute_gh_pr_diff ~sw ~proc_mgr ~clock ~repo ~pr_number

  | SlackPost { channel; text; thread_ts } ->
      execute_slack_post ~sw ~proc_mgr ~clock ~channel ~text ~thread_ts

  | SetStreamDelta { enabled } ->
      let new_state = Llm_clients.set_stream_delta enabled in
      { model = "config:set_stream_delta";
        returncode = 0;
        response = sprintf "Stream delta broadcasting is now %s" (if new_state then "ENABLED" else "DISABLED");
        extra = [("enabled", string_of_bool new_state)] }

  | GetStreamDelta ->
      let enabled = Llm_clients.get_stream_delta () in
      { model = "config:get_stream_delta";
        returncode = 0;
        response = string_of_bool enabled;
        extra = [("enabled", string_of_bool enabled)] }

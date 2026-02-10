(** MAGI Trinity tool implementations - Eio Direct-Style

    Pure Eio version of tools.ml using:
    - Direct-style (no monadic bind)
    - Cli_runner_eio for subprocess execution
    - Eio.Time for delays
    - Structured concurrency

    Re-exports pure functions from tools.ml (parse_*, build_*, etc.)
*)

(* Fiber-safe random state for ID generation *)
let tools_rng = Random.State.make_self_init ()

open Printf
open Types
open Cli_runner_eio

(** {1 Re-exports from Tool_parsers (Pure, no Lwt)} *)

(* Pure functions - from Tool_parsers module *)
let budget_mode_value = Tool_parsers.budget_mode_value
let parse_gemini_args = Tool_parsers.parse_gemini_args
let parse_gemini_list_args = Tool_parsers.parse_gemini_list_args
let parse_claude_args = Tool_parsers.parse_claude_args
let parse_codex_args = Tool_parsers.parse_codex_args
let parse_ollama_args = Tool_parsers.parse_ollama_args
let parse_ollama_list_args = Tool_parsers.parse_ollama_list_args
let parse_glm_args = Tool_parsers.parse_glm_args
let parse_glm_translate_args = Tool_parsers.parse_glm_translate_args
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
let parse_set_stream_delta_args = Tool_parsers.parse_set_stream_delta_args
let parse_get_stream_delta_args = Tool_parsers.parse_get_stream_delta_args
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

let env_truthy value =
  match String.lowercase_ascii value with
  | "1" | "true" | "yes" | "on" -> true
  | "0" | "false" | "no" | "off" -> false
  | _ -> false

(* Ollama helpers *)
let thinking_prompt_prefix = Tool_parsers.thinking_prompt_prefix
let tool_schema_to_ollama_tool = Tool_parsers.tool_schema_to_ollama_tool
let tool_calls_to_json = Tool_parsers.tool_calls_to_json

(* Gemini error handling - from Types module *)
let classify_gemini_error = Types.classify_gemini_error
let is_recoverable_gemini_error = Types.is_recoverable_gemini_error
let string_of_gemini_error = Types.string_of_gemini_error

(** {1 MCP Client Calls - Direct Style} *)

(** Parse MCP HTTP response - from Tools_mcp_parse module *)
let parse_mcp_http_response = Tools_mcp_parse.parse_http_response

(** Call an external MCP tool via HTTP using curl subprocess *)
let call_external_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments ~timeout =
  match Tool_config.get_mcp_server_url server_name with
  | None -> sprintf "Error: MCP server '%s' not found or not HTTP type" server_name
  | Some url ->
      let request_body = Tools_mcp_parse.build_tool_call_request ~tool_name ~arguments in
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
          parse_mcp_http_response r.stdout |> Option.value ~default:r.stdout

(** Call MCP via stdio subprocess - shell injection safe *)
let call_stdio_mcp ~sw ~proc_mgr ~clock ~server_name ~command ~args ~tool_name ~arguments ~timeout =
  let request_body = Tools_mcp_parse.build_tool_call_request ~tool_name ~arguments in
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
            let msg = Safe_parse.json_string ~context:"mcp:error" ~default:"Unknown error" error "message" in
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

let masc_enabled () =
  match Sys.getenv_opt "LLM_MCP_MASC_HOOK" with
  | Some v -> env_truthy v
  | None -> false

let masc_agent_base () =
  Sys.getenv_opt "LLM_MCP_MASC_AGENT" |> Option.value ~default:"llm-mcp"

let masc_heartbeat_interval () =
  match Sys.getenv_opt "LLM_MCP_MASC_HEARTBEAT_SEC" with
  | Some v -> int_of_string_opt v |> Option.value ~default:30
  | None -> 30

let masc_available () =
  match get_mcp_server_config "masc" with
  | Some cfg -> cfg.url <> None || cfg.command <> None
  | None -> false

let call_masc_tool ~sw ~proc_mgr ~clock ~tool_name ~arguments =
  call_mcp ~sw ~proc_mgr ~clock ~server_name:"masc" ~tool_name ~arguments ~timeout:5

let with_masc_hook ~sw ~proc_mgr ~clock ~label f =
  if not (masc_enabled () && masc_available ()) then
    f ()
  else
    let base = masc_agent_base () in
    let ts = int_of_float (Time_compat.now ()) in
    let agent =
      let safe_label = String.map (fun c -> if c = '.' then '-' else c) label in
      Printf.sprintf "%s-%s-%d" base safe_label ts
    in
    let join_args = `Assoc [
      ("agent_name", `String agent);
      ("capabilities", `List [`String "chain"]);
    ] in
    let heartbeat_args = `Assoc [("agent_name", `String agent)] in
    let leave_args = `Assoc [("agent_name", `String agent)] in
    (* Eio-idiomatic: use Switch.on_release for MASC session lifecycle *)
    Eio.Switch.run (fun masc_sw ->
      (* Register cleanup first, before join *)
      Eio.Switch.on_release masc_sw (fun () ->
        try
          ignore (call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_leave" ~arguments:leave_args)
        with ex ->
          Log.warn "masc_hook" "masc_leave failed in on_release: %s"
            (Printexc.to_string ex)
      );
      (* Now join *)
      let _ = call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_join" ~arguments:join_args in
      (* Heartbeat fiber in same switch - will be cancelled when switch exits *)
      let interval = float_of_int (masc_heartbeat_interval ()) in
      let _ =
        Eio.Fiber.fork ~sw:masc_sw (fun () ->
          let rec loop () =
            (* Heartbeat must never crash the main request flow. *)
            (try
               ignore (call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_heartbeat" ~arguments:heartbeat_args)
             with exn ->
               eprintf "[masc] heartbeat error: %s\n%!" (Printexc.to_string exn));
            Eio.Time.sleep clock interval;
            loop ()
          in
          loop ())
      in
      f ()
    )

(** {1 Streaming Execution} - Config from Tools_stream_config *)
let stream_delta_enabled = Tools_stream_config.enabled
let set_stream_delta = Tools_stream_config.set
let get_stream_delta = Tools_stream_config.get
let stream_delta_max_events = Tools_stream_config.max_events
let stream_delta_max_chars = Tools_stream_config.max_chars
let generate_stream_id = Tools_stream_config.generate_id

(** Execute Ollama with token streaming callback *)
let execute_ollama_streaming ~sw ~proc_mgr ~clock ~on_token ?stream_id args =
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
  let stream_id =
    match stream_id with
    | Some sid -> sid
    | None -> generate_stream_id model_name
  in
  let delta_enabled = stream_delta_enabled () in
  let delta_max_events = stream_delta_max_events () in
  let delta_max_chars = stream_delta_max_chars () in
  let delta_count = ref 0 in
  let delta_truncated = ref false in
  let total_chars = ref 0 in
  let broadcast_delta json =
    if delta_enabled then
      try Notification_sse.broadcast json with exn ->
        Printf.eprintf "[LLM] Delta broadcast failed: %s\n%!" (Printexc.to_string exn)
    else
      ()
  in
  let emit_start () =
    broadcast_delta (`Assoc [
      ("type", `String "llm_stream_start");
      ("stream_id", `String stream_id);
      ("model", `String model_name);
      ("has_tools", `Bool has_tools);
    ])
  in
  let emit_end ~success ?error () =
    let base = [
      ("type", `String "llm_stream_end");
      ("stream_id", `String stream_id);
      ("model", `String model_name);
      ("success", `Bool success);
      ("chunks", `Int !delta_count);
      ("total_chars", `Int !total_chars);
      ("truncated", `Bool !delta_truncated);
    ] in
    let fields = match error with
      | Some msg -> base @ [("error", `String msg)]
      | None -> base
    in
    broadcast_delta (`Assoc fields)
  in
  emit_start ();
  match cmd_result with
  | Error err ->
      let response = Option.value err_msg ~default:err in
      emit_end ~success:false ~error:response ();
      { model = model_name; returncode = -1; response; extra = extra_base }
  | Ok [] ->
      let response = "Invalid args" in
      emit_end ~success:false ~error:response ();
      { model = model_name; returncode = -1; response; extra = extra_base }
  | Ok (cmd :: cmd_args) ->
      begin
        let full_response = Buffer.create 1024 in
        let accumulated_tool_calls = ref [] in
        let truncated_notice_sent = ref false in
        let on_line line =
          if has_tools then
            match Ollama_parser.parse_chat_chunk line with
            | Ok (token, tool_calls, _done) ->
                Buffer.add_string full_response token;
                if tool_calls <> [] then accumulated_tool_calls := tool_calls;
                total_chars := !total_chars + String.length token;
                incr delta_count;
                if !delta_count <= delta_max_events then begin
                  let truncated = String.length token > delta_max_chars in
                  let delta =
                    if truncated then String.sub token 0 delta_max_chars else token
                  in
                  if truncated then delta_truncated := true;
                  broadcast_delta (`Assoc [
                    ("type", `String "llm_delta");
                    ("stream_id", `String stream_id);
                    ("index", `Int !delta_count);
                    ("delta", `String delta);
                    ("truncated", `Bool truncated);
                    ("orig_len", `Int (String.length token));
                  ])
                end else if not !truncated_notice_sent then begin
                  truncated_notice_sent := true;
                  delta_truncated := true;
                  broadcast_delta (`Assoc [
                    ("type", `String "llm_delta_truncated");
                    ("stream_id", `String stream_id);
                    ("max_events", `Int delta_max_events);
                  ])
                end;
                on_token token
            | Error _ -> ()
          else
            match Tool_parsers.parse_ollama_chunk line with
            | Ok (token, _done) ->
                Buffer.add_string full_response token;
                total_chars := !total_chars + String.length token;
                incr delta_count;
                if !delta_count <= delta_max_events then begin
                  let truncated = String.length token > delta_max_chars in
                  let delta =
                    if truncated then String.sub token 0 delta_max_chars else token
                  in
                  if truncated then delta_truncated := true;
                  broadcast_delta (`Assoc [
                    ("type", `String "llm_delta");
                    ("stream_id", `String stream_id);
                    ("index", `Int !delta_count);
                    ("delta", `String delta);
                    ("truncated", `Bool truncated);
                    ("orig_len", `Int (String.length token));
                  ])
                end else if not !truncated_notice_sent then begin
                  truncated_notice_sent := true;
                  delta_truncated := true;
                  broadcast_delta (`Assoc [
                    ("type", `String "llm_delta_truncated");
                    ("stream_id", `String stream_id);
                    ("max_events", `Int delta_max_events);
                  ])
                end;
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
            let extra = extra @ [("stream_id", stream_id)] in
            emit_end ~success:true ();
            { model = model_name;
              returncode = 0;
              response = Buffer.contents full_response;
              extra; }
        | Error (Timeout t) ->
            let response = Printf.sprintf "Timeout after %ds" t in
            emit_end ~success:false ~error:response ();
            { model = model_name;
              returncode = -1;
              response;
              extra = extra_base; }
        | Error (ProcessError msg) ->
            let response = sprintf "Error: %s" msg in
            emit_end ~success:false ~error:response ();
            { model = model_name;
              returncode = -1;
              response;
              extra = extra_base; }
      end

(** {1 Generic CLI Streaming} *)

(** Execute a plain-text CLI tool with line-by-line streaming.
    Used for Claude CLI, Codex CLI, and Gemini CLI which output text lines. *)
let execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args =
  let stream_id = generate_stream_id model_name in
  let delta_enabled = stream_delta_enabled () in
  let delta_max_events = stream_delta_max_events () in
  let delta_max_chars = stream_delta_max_chars () in
  let delta_count = ref 0 in
  let delta_truncated = ref false in
  let total_chars = ref 0 in
  let broadcast_delta json =
    if delta_enabled then
      try Notification_sse.broadcast json
      with exn ->
        (* Log broadcast failures but don't interrupt streaming *)
        eprintf "[cli_streaming] SSE broadcast failed: %s\n%!" (Printexc.to_string exn)
    else
      ()
  in
  let emit_start () =
    broadcast_delta (`Assoc [
      ("type", `String "llm_stream_start");
      ("stream_id", `String stream_id);
      ("model", `String model_name);
      ("has_tools", `Bool false);
    ])
  in
  let emit_end ~success ?error () =
    let base = [
      ("type", `String "llm_stream_end");
      ("stream_id", `String stream_id);
      ("model", `String model_name);
      ("success", `Bool success);
      ("chunks", `Int !delta_count);
      ("total_chars", `Int !total_chars);
      ("truncated", `Bool !delta_truncated);
    ] in
    let fields = match error with
      | Some msg -> base @ [("error", `String msg)]
      | None -> base
    in
    broadcast_delta (`Assoc fields)
  in
  emit_start ();
  let full_response = Buffer.create 4096 in
  let truncated_notice_sent = ref false in
  let on_line line =
    (* Each line is a text chunk - add newline back for proper formatting *)
    (* Preserve empty lines for markdown/code formatting *)
    let chunk = line ^ "\n" in
    Buffer.add_string full_response chunk;
    total_chars := !total_chars + String.length chunk;
    incr delta_count;
    if !delta_count <= delta_max_events then begin
      let truncated = String.length chunk > delta_max_chars in
      let delta =
        if truncated then String.sub chunk 0 delta_max_chars else chunk
      in
      if truncated then delta_truncated := true;
      broadcast_delta (`Assoc [
        ("type", `String "llm_delta");
        ("stream_id", `String stream_id);
        ("index", `Int !delta_count);
        ("delta", `String delta);
        ("truncated", `Bool truncated);
        ("orig_len", `Int (String.length chunk));
      ])
    end else if not !truncated_notice_sent then begin
      truncated_notice_sent := true;
      delta_truncated := true;
      broadcast_delta (`Assoc [
        ("type", `String "llm_delta_truncated");
        ("stream_id", `String stream_id);
        ("max_events", `Int delta_max_events);
      ])
    end
  in
  let result = run_streaming_command ~sw ~proc_mgr ~clock ~timeout ~on_line cmd cmd_args in
  let partial_response = Buffer.contents full_response in
  let extra_with_stream = extra_base @ [("streamed", "true"); ("stream_id", stream_id)] in
  match result with
  | Ok exit_code ->
      let success = (exit_code = 0) in
      emit_end ~success ();
      { model = model_name;
        returncode = exit_code;
        response = partial_response;
        extra = extra_with_stream; }
  | Error (Timeout t) ->
      (* Preserve partial response on timeout - don't lose streamed content *)
      let error_msg = sprintf "Timeout after %ds" t in
      emit_end ~success:false ~error:error_msg ();
      let response = if String.length partial_response > 0
        then partial_response ^ "\n\n[TIMEOUT: " ^ error_msg ^ "]"
        else error_msg in
      { model = model_name;
        returncode = -1;
        response;
        extra = extra_with_stream @ [("timeout", "true")]; }
  | Error (ProcessError msg) ->
      (* Preserve partial response on error - don't lose streamed content *)
      emit_end ~success:false ~error:msg ();
      let response = if String.length partial_response > 0
        then partial_response ^ "\n\n[ERROR: " ^ msg ^ "]"
        else sprintf "Error: %s" msg in
      { model = model_name;
        returncode = -1;
        response;
        extra = extra_with_stream @ [("process_error", "true")]; }

(** {1 Gemini with Resilience} *)

(** Execute Gemini via Direct API - delegated to Handler_gemini *)
let execute_gemini_direct_api = Handler_gemini.execute_direct_api

(** Execute Claude via Direct Anthropic API - delegated to Handler_claude *)
let execute_claude_direct_api = Handler_claude.execute_direct_api

(** Execute Codex via Direct OpenAI API - delegated to Handler_codex *)
let execute_codex_direct_api = Handler_codex.execute_direct_api

(** Execute Gemini with automatic retry - delegated to Handler_gemini *)
let execute_gemini_with_retry ~sw ~proc_mgr ~clock
    ?max_retries
    ~model ~thinking_level ~timeout ~stream ~args () =
  Handler_gemini.execute_with_retry ~sw ~proc_mgr ~clock
    ?max_retries
    ~model ~thinking_level ~timeout ~stream ~args
    ~execute_cli_streaming ()

(** {1 External Tools} - delegated to Handler_utils *)

let external_tool_timeout = Handler_utils.external_tool_timeout

(** Execute GitHub PR diff - delegated to Handler_utils *)
let execute_gh_pr_diff = Handler_utils.execute_gh_pr_diff

(** Execute Slack post message - delegated to Handler_utils *)
let execute_slack_post = Handler_utils.execute_slack_post

(** {1 Langfuse Tracing Helpers} - From Tools_tracer module *)
let get_model_name_for_tracing = Tools_tracer.get_model_name
let get_input_for_tracing = Tools_tracer.get_input
let classify_error_class = Tools_tracer.classify_error
let result_streamed = Tools_tracer.was_streamed

(** Apply Chain DSL `system` into backend-specific system_prompt when supported. *)
let with_system_prompt (system : string option) (args : Types.tool_args) : Types.tool_args =
  match system, args with
  | None, _ -> args
  | Some _, Claude c -> Claude { c with system_prompt = system }
  | Some _, Ollama o -> Ollama { o with system_prompt = system }
  | Some _, Glm g -> Glm { g with system_prompt = system }
  | Some _, _ -> args

(** Build tool_args for Chain DSL LLM nodes.

    Pure function (no Eio), suitable for unit tests. *)
let chain_llm_args
    ~(timeout : int)
    ~(gemini_use_cli : bool)
    ~(parsed_tools : Types.tool_schema list option)
    ~(model : string)
    ?(system : string option)
    ~(prompt : string)
    () : Types.tool_args =
  let starts_with ~prefix s =
    let prefix_len = String.length prefix in
    String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
  in
  let is_gemini_model m =
    m = "gemini" ||
    m = "pro" || m = "flash" || m = "flash-lite" ||
    m = "3-pro" || m = "3-flash" ||
    starts_with ~prefix:"gemini-" m
  in
  let args =
    match String.lowercase_ascii model with
    | "stub" | "mock" ->
        (* Stub model for tests and local smoke runs *)
        Types.Gemini {
          prompt;
          model = "stub";
          thinking_level = Types.Low;
          yolo = false;
          output_format = Types.Text;
          timeout;
          stream = false;
          use_cli = false;  (* Stub uses direct API *)
          fallback_to_api = false;
        }
    | m when is_gemini_model m ->
        Types.Gemini {
          prompt;
          model;
          thinking_level = Types.High;
          yolo = false;
          output_format = Types.Text;
          timeout;
          stream = false;
          use_cli = gemini_use_cli;
          fallback_to_api = true;
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
          timeout;
          stream = false;
          use_cli = true;
          fallback_to_api = true;
          api_key = None;
        }
    | "codex" | "gpt-5.2" ->
        Types.Codex {
          prompt;
          model = "gpt-5.2";
          reasoning_effort = Types.RXhigh;
          sandbox = Types.WorkspaceWrite;
          working_directory = None;
          timeout;
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
          timeout;
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
          timeout;
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
          timeout;
          stream = false;
          thinking = false;  (* Faster for chain execution *)
          do_sample = true;
          web_search = false;
          tools = [];  (* No tools for chain execution by default *)
          api_key = None;
        }
    | _ ->
        (* Default to Gemini for unknown models *)
        Types.Gemini {
          prompt;
          model = "gemini-3-pro-preview";
          thinking_level = Types.High;
          yolo = false;
          output_format = Types.Text;
          timeout;
          stream = false;
          use_cli = gemini_use_cli;
          fallback_to_api = true;
        }
  in
  with_system_prompt system args

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
    if stream_flag then Some (generate_stream_id model_for_log) else None
  in
  let log_extra =
    match stream_id_opt with
    | Some sid -> [("stream_id", sid)]
    | None -> []
  in
  let tool_for_log = Tools_tracer.get_tool_name args in
  let prompt_for_log = get_input_for_tracing args in
  let log_start_ts = Time_compat.now () in
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
  | Gemini { prompt; model; thinking_level; timeout; stream; use_cli; fallback_to_api; _ } ->
      let result =
        if use_cli then begin
          (* CLI mode: slower but MASC-enabled *)
          let cli_result =
            execute_gemini_with_retry ~sw ~proc_mgr ~clock ~model ~thinking_level ~timeout ~stream ~args ()
          in
          if cli_result.returncode <> 0 && fallback_to_api then
            (* CLI failed, try API fallback *)
            execute_gemini_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~thinking_level ~timeout ~stream
          else
            cli_result
        end else
          (* Direct API mode: faster, no MASC *)
          execute_gemini_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~thinking_level ~timeout ~stream
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Time_compat.now () -. log_start_ts) *. 1000.0))
          ~success:(result.returncode = 0)
          ~streamed:(result_streamed result)
          ~extra:log_extra
          ?error_class:(classify_error_class result)
          ()
      else
        ();
      result

  | Claude { prompt; model; long_context; system_prompt; working_directory = _; timeout; stream; use_cli; fallback_to_api; api_key; _ } ->
      let model_name = sprintf "claude-cli (%s)" model in
      let extra_base = [("long_context", string_of_bool long_context)] in

      (* Helper: execute via CLI *)
      let execute_via_cli () =
        match build_claude_cmd args with
        | Error err ->
            { model = model_name;
              returncode = -1;
              response = err;
              extra = extra_base @ [("invalid_args", "true")]; }
        | Ok [] ->
            { model = model_name;
              returncode = -1;
              response = "Empty command list";
              extra = extra_base @ [("invalid_args", "true")]; }
        | Ok (cmd :: cmd_args) ->
            if stream then
              execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args
            else begin
              let result = run_command ~sw ~proc_mgr ~clock ~safe_tmpdir:true ~timeout cmd cmd_args in
              match result with
              | Ok r ->
                  { model = model_name;
                    returncode = r.exit_code;
                    response = get_output r;
                    extra = extra_base; }
              | Error (Timeout t) -> Tools_tracer.timeout_result ~model:model_name ~extra:extra_base t
              | Error (ProcessError msg) -> Tools_tracer.process_error_result ~model:model_name ~extra:extra_base msg
            end
      in

      (* Execute based on use_cli flag with optional fallback *)
      let result =
        if use_cli then begin
          let cli_result = execute_via_cli () in
          if cli_result.returncode <> 0 && fallback_to_api then begin
            (* CLI failed, try API fallback *)
            let api_result = execute_claude_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~system_prompt ~timeout ~stream ?api_key_override:api_key () in
            if api_result.returncode = 0 then
              { api_result with extra = api_result.extra @ [("fallback_used", "true")] }
            else
              (* Both failed, return CLI error with note about fallback attempt *)
              { cli_result with extra = cli_result.extra @ [("fallback_attempted", "true"); ("fallback_error", api_result.response)] }
          end else
            cli_result
        end else
          (* Direct API mode *)
          execute_claude_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~system_prompt ~timeout ~stream ?api_key_override:api_key ()
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Time_compat.now () -. log_start_ts) *. 1000.0))
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

      (* Helper: execute via CLI *)
      let execute_via_cli () =
        match build_codex_cmd args with
        | Error err ->
            { model = model_name;
              returncode = -1;
              response = err;
              extra = extra_base @ [("invalid_args", "true")]; }
        | Ok [] ->
            { model = model_name;
              returncode = -1;
              response = "Empty command list";
              extra = extra_base @ [("invalid_args", "true")]; }
        | Ok (cmd :: cmd_args) ->
            if stream then begin
              let stream_result = execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args in
              { stream_result with response = clean_codex_output stream_result.response }
            end else begin
              let result = run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args in
              match result with
              | Ok r ->
                  { model = model_name;
                    returncode = r.exit_code;
                    response = clean_codex_output (get_output r);
                    extra = extra_base; }
              | Error (Timeout t) -> Tools_tracer.timeout_result ~model:model_name ~extra:extra_base t
              | Error (ProcessError msg) -> Tools_tracer.process_error_result ~model:model_name ~extra:extra_base msg
            end
      in

      (* Execute based on use_cli flag with optional fallback *)
      let result =
        if use_cli then begin
          let cli_result = execute_via_cli () in
          if cli_result.returncode <> 0 && fallback_to_api then begin
            (* CLI failed, try API fallback *)
            let api_result = execute_codex_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~timeout ~stream in
            if api_result.returncode = 0 then
              { api_result with extra = api_result.extra @ [("fallback_used", "true")] }
            else
              { cli_result with extra = cli_result.extra @ [("fallback_attempted", "true"); ("fallback_error", api_result.response)] }
          end else
            cli_result
        end else
          execute_codex_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~timeout ~stream
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Time_compat.now () -. log_start_ts) *. 1000.0))
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
        match build_ollama_curl_cmd ~force_stream:(Some false) args with
        | Error err ->
            { model = sprintf "ollama (%s)" model;
              returncode = -1;
              response = err;
              extra = [("temperature", sprintf "%.1f" temperature); ("local", "true"); ("invalid_args", "true")]; }
        | Ok [] ->
            { model = sprintf "ollama (%s)" model;
              returncode = -1;
              response = "Empty command list";
              extra = [("temperature", sprintf "%.1f" temperature); ("local", "true"); ("invalid_args", "true")]; }
        | Ok (cmd :: cmd_args) ->
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
                  extra = [("temperature", sprintf "%.1f" temperature); ("local", "true")]; }
      in
      let result =
        if stream then
          let streaming =
            execute_ollama_streaming ~sw ~proc_mgr ~clock ~on_token:(fun _ -> ())
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
          ~duration_ms:(int_of_float ((Time_compat.now () -. log_start_ts) *. 1000.0))
          ~success:(result.returncode = 0)
          ~streamed:(result_streamed result)
          ~extra:log_extra
          ?error_class:(classify_error_class result)
          ()
      else
        ();
      result

  | Glm { prompt; model; system_prompt; temperature; max_tokens; timeout; stream; thinking; do_sample; web_search; tools; api_key } ->
      (* Z.ai GLM Cloud API - OpenAI-compatible with Function Calling *)
      let model_name = sprintf "glm (%s)" model in
      (* Resolve API key: explicit override > ZAI_API_KEY env var *)
      let resolved_key = match api_key with
        | Some k -> Some k
        | None ->
          match Tools_tracer.require_api_key ~env_var:"ZAI_API_KEY" ~model:model_name ~extra:[] with
          | Some _err -> None
          | None -> Some (Tools_tracer.get_api_key "ZAI_API_KEY")
      in
      (match resolved_key with
      | None ->
        (match Tools_tracer.require_api_key ~env_var:"ZAI_API_KEY" ~model:model_name ~extra:[] with
         | Some err -> err
         | None -> Tools_tracer.process_error_result ~model:model_name ~extra:[] "No API key")
      | Some api_key ->
        begin
        (* Build OpenAI-compatible request body *)
        let messages = match system_prompt with
          | Some sys ->
              `List [
                `Assoc [("role", `String "system"); ("content", `String sys)];
                `Assoc [("role", `String "user"); ("content", `String prompt)]
              ]
          | None ->
              `List [
                `Assoc [("role", `String "user"); ("content", `String prompt)]
              ]
        in
        (* GLM-4.7 thinking parameter: {"type": "enabled"} or {"type": "disabled"} *)
        let thinking_obj = `Assoc [("type", `String (if thinking then "enabled" else "disabled"))] in
        let body_fields = [
          ("model", `String model);
          ("messages", messages);
          ("temperature", `Float temperature);
          ("do_sample", `Bool do_sample);
          ("thinking", thinking_obj);
          ("stream", `Bool stream);
        ] in
        let body_fields = match max_tokens with
          | Some n -> body_fields @ [("max_tokens", `Int n)]
          | None -> body_fields
        in
        (* Build tools list: combine explicit tools with web_search backward compat *)
        let all_tools =
          if List.length tools > 0 then
            (* Use new tools array *)
            tools
          else if web_search then
            (* DEPRECATED: fallback to web_search bool for backward compat *)
            [Types.glm_web_search_tool ()]
          else
            []
        in
        let body_fields =
          if List.length all_tools > 0 then
            body_fields @ [("tools", Types.glm_tools_to_json all_tools)]
          else
            body_fields
        in
        let body = Yojson.Safe.to_string (`Assoc body_fields) in
        let cmd = "curl" in

        if stream then begin
          (* Streaming mode: use run_streaming_command with SSE passthrough *)
          let cmd_args = [
            "-s"; "-N"; "-X"; "POST";  (* -N for no-buffer SSE *)
            "https://api.z.ai/api/coding/paas/v4/chat/completions";
            "-H"; "Content-Type: application/json";
            "-H"; sprintf "Authorization: Bearer %s" api_key;
            "-d"; body;
            "--max-time"; string_of_int timeout;
          ] in
          let content_buffer = Buffer.create 1024 in
          let reasoning_buffer = Buffer.create 1024 in
          let chunk_count = ref 0 in
          (* Phase 2: Tool calls accumulator for streaming
             Tool calls come in chunks: id/name first, then arguments in pieces *)
          let tool_calls_acc : (string, (string * string * Buffer.t)) Hashtbl.t = Hashtbl.create 4 in
          let tool_calls_complete = ref false in
          let on_line line =
            (* Parse SSE data lines: "data: {...}" or "data: [DONE]" *)
            if String.length line > 6 && String.sub line 0 6 = "data: " then begin
              let data = String.sub line 6 (String.length line - 6) in
              if data <> "[DONE]" then begin
                try
                  let json = Yojson.Safe.from_string data in
                  let open Yojson.Safe.Util in
                  let choices = json |> member "choices" |> to_list in
                  (match choices with
                  | first_choice :: _ ->
                    let delta = first_choice |> member "delta" in
                    let finish_reason = first_choice |> member "finish_reason" in
                    (* Check for tool_calls finish reason *)
                    (try
                      if to_string finish_reason = "tool_calls" then
                        tool_calls_complete := true
                    with _ -> ());
                    (* Extract content chunk *)
                    (try
                      let content_chunk = delta |> member "content" |> to_string in
                      if String.length content_chunk > 0 then begin
                        Buffer.add_string content_buffer content_chunk;
                        incr chunk_count;
                        (* Broadcast chunk via MCP SSE *)
                        Notification_sse.broadcast (`Assoc [
                          ("jsonrpc", `String "2.0");
                          ("method", `String "notifications/glm/chunk");
                          ("params", `Assoc [
                            ("model", `String model);
                            ("chunk", `String content_chunk);
                            ("type", `String "content");
                            ("index", `Int !chunk_count);
                          ]);
                        ])
                      end
                    with _ -> ());
                    (* Extract reasoning chunk *)
                    (try
                      let reasoning_chunk = delta |> member "reasoning_content" |> to_string in
                      if String.length reasoning_chunk > 0 then begin
                        Buffer.add_string reasoning_buffer reasoning_chunk;
                        (* Broadcast reasoning chunk *)
                        Notification_sse.broadcast (`Assoc [
                          ("jsonrpc", `String "2.0");
                          ("method", `String "notifications/glm/chunk");
                          ("params", `Assoc [
                            ("model", `String model);
                            ("chunk", `String reasoning_chunk);
                            ("type", `String "reasoning");
                            ("index", `Int !chunk_count);
                          ]);
                        ])
                      end
                    with _ -> ());
                    (* Phase 2: Extract tool_calls chunks *)
                    (try
                      let tool_calls = delta |> member "tool_calls" |> to_list in
                      List.iter (fun tc ->
                        let index = tc |> member "index" |> to_int in
                        let key = string_of_int index in
                        (* Get or create accumulator for this tool call *)
                        let (call_id, func_name, args_buf) =
                          try Hashtbl.find tool_calls_acc key
                          with Not_found ->
                            let buf = Buffer.create 256 in
                            Hashtbl.add tool_calls_acc key ("", "", buf);
                            ("", "", buf)
                        in
                        (* Update call_id if present *)
                        let call_id = try
                          let id = tc |> member "id" |> to_string in
                          if String.length id > 0 then id else call_id
                        with _ -> call_id in
                        (* Update function name if present *)
                        let func_name = try
                          let func = tc |> member "function" in
                          let name = func |> member "name" |> to_string in
                          if String.length name > 0 then name else func_name
                        with _ -> func_name in
                        (* Accumulate arguments *)
                        (try
                          let func = tc |> member "function" in
                          let args_chunk = func |> member "arguments" |> to_string in
                          if String.length args_chunk > 0 then begin
                            Buffer.add_string args_buf args_chunk;
                            (* Broadcast tool call chunk *)
                            Notification_sse.broadcast (`Assoc [
                              ("jsonrpc", `String "2.0");
                              ("method", `String "notifications/glm/tool_call_chunk");
                              ("params", `Assoc [
                                ("model", `String model);
                                ("index", `Int index);
                                ("call_id", `String call_id);
                                ("function_name", `String func_name);
                                ("arguments_chunk", `String args_chunk);
                                ("streaming", `Bool true);
                              ]);
                            ])
                          end
                        with _ -> ());
                        (* Update accumulator *)
                        Hashtbl.replace tool_calls_acc key (call_id, func_name, args_buf)
                      ) tool_calls
                    with _ -> ())
                  | [] -> ())
                with _ -> ()
              end else begin
                (* [DONE] - broadcast completion with tool calls if any *)
                let tool_calls_json =
                  if Hashtbl.length tool_calls_acc > 0 then
                    let calls = Hashtbl.fold (fun _ (call_id, func_name, args_buf) acc ->
                      let args = Buffer.contents args_buf in
                      `Assoc [
                        ("id", `String call_id);
                        ("type", `String "function");
                        ("function", `Assoc [
                          ("name", `String func_name);
                          ("arguments", `String args);
                        ]);
                      ] :: acc
                    ) tool_calls_acc [] in
                    Some (`List calls)
                  else None
                in
                let params = [
                  ("model", `String model);
                  ("total_chunks", `Int !chunk_count);
                ] in
                let params = match tool_calls_json with
                  | Some tc -> params @ [("tool_calls", tc); ("has_tool_calls", `Bool true)]
                  | None -> params
                in
                Notification_sse.broadcast (`Assoc [
                  ("jsonrpc", `String "2.0");
                  ("method", `String "notifications/glm/done");
                  ("params", `Assoc params);
                ])
              end
            end
          in
          let result = run_streaming_command ~sw ~proc_mgr ~clock ~timeout ~on_line cmd cmd_args in
          match result with
          | Ok _ ->
              let content = Buffer.contents content_buffer in
              let reasoning = Buffer.contents reasoning_buffer in
              (* Phase 2: Collect accumulated tool calls *)
              let tool_calls_list =
                if Hashtbl.length tool_calls_acc > 0 then
                  Hashtbl.fold (fun _ (call_id, func_name, args_buf) acc ->
                    let args = Buffer.contents args_buf in
                    `Assoc [
                      ("id", `String call_id);
                      ("type", `String "function");
                      ("function", `Assoc [
                        ("name", `String func_name);
                        ("arguments", `String args);
                      ]);
                    ] :: acc
                  ) tool_calls_acc []
                else []
              in
              let final_response =
                if String.length content > 0 then content
                else if String.length reasoning > 0 then sprintf "[Thinking]\n%s" reasoning
                else if List.length tool_calls_list > 0 then
                  (* If only tool calls, return them as JSON *)
                  Yojson.Safe.to_string (`Assoc [("tool_calls", `List tool_calls_list)])
                else "Empty response" in
              let extra = [("temperature", sprintf "%.1f" temperature); ("cloud", "zai"); ("streamed", "true")] in
              let extra = if String.length reasoning > 0 then extra @ [("has_reasoning", "true")] else extra in
              let extra = if List.length tool_calls_list > 0 then
                extra @ [("has_tool_calls", "true"); ("tool_calls_count", string_of_int (List.length tool_calls_list))]
              else extra in
              { model = sprintf "glm (%s)" model;
                returncode = 0;
                response = final_response;
                extra; }
          | Error (Timeout t) ->
              { model = sprintf "glm (%s)" model;
                returncode = -1;
                response = sprintf "Timeout after %ds" t;
                extra = [("error", "timeout")]; }
          | Error (ProcessError msg) ->
              { model = sprintf "glm (%s)" model;
                returncode = -1;
                response = sprintf "Error: %s" msg;
                extra = [("error", "process_error")]; }
        end
        else begin
          (* Non-streaming mode: wait for complete response with retry *)
          let cmd_args = [
            "-s"; "-X"; "POST";
            "https://api.z.ai/api/coding/paas/v4/chat/completions";
            "-H"; "Content-Type: application/json";
            "-H"; sprintf "Authorization: Bearer %s" api_key;
            "-d"; body;
            "--max-time"; string_of_int timeout;
          ] in
          (* Wrap with retry logic for rate limits and transient errors *)
          let glm_call () =
            match run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args with
            | Ok r ->
                (* Check for API errors that should trigger retry *)
                if Retry.is_rate_limit_error r.stdout || Retry.is_temporary_error r.stdout then
                  Error r.stdout
                else
                  Ok r
            | Error (Timeout t) -> Error (sprintf "Timeout after %ds" t)
            | Error (ProcessError msg) -> Error msg
          in
          let retry_result = Retry.with_rate_limit_retry ~clock glm_call in
          let result = Retry.to_result retry_result in
        match result with
        | Ok r ->
            (* Parse OpenAI-compatible response with GLM-4.7 thinking support *)
            (try
              let json = Yojson.Safe.from_string r.stdout in
              let open Yojson.Safe.Util in
              let choices = json |> member "choices" |> to_list in
              (match choices with
              | first_choice :: _ ->
                let message = first_choice |> member "message" in
                (* GLM-4.7 returns reasoning_content for thinking, content for final answer *)
                let content = Safe_parse.json_string ~context:"glm:content" ~default:"" message "content" in
                let reasoning = Safe_parse.json_string ~context:"glm:reasoning" ~default:"" message "reasoning_content" in
                (* Use content if available, otherwise use reasoning_content *)
                let final_response =
                  if String.length content > 0 then content
                  else if String.length reasoning > 0 then sprintf "[Thinking]\n%s" reasoning
                  else "Empty response" in
                let extra = [("temperature", sprintf "%.1f" temperature); ("cloud", "zai")] in
                let extra = if String.length reasoning > 0 then
                  extra @ [("has_reasoning", "true")] else extra in
                { model = sprintf "glm (%s)" model;
                  returncode = 0;
                  response = final_response;
                  extra; }
              | [] ->
                { model = sprintf "glm (%s)" model;
                  returncode = -1;
                  response = "Error: No choices in response";
                  extra = [("error", "no_choices")]; })
            with e ->
              (* Check if it's an API error *)
              (try
                let json = Yojson.Safe.from_string r.stdout in
                let open Yojson.Safe.Util in
                let error_msg = json |> member "error" |> member "message" |> to_string in
                { model = sprintf "glm (%s)" model;
                  returncode = -1;
                  response = sprintf "API Error: %s" error_msg;
                  extra = [("error", "api_error")]; }
              with _ ->
                { model = sprintf "glm (%s)" model;
                  returncode = -1;
                  response = sprintf "Error parsing response: %s. Raw: %s" (Printexc.to_string e) r.stdout;
                  extra = [("error", "parse_error")]; }))
        | Error err_msg ->
            (* Error after all retries exhausted *)
            let error_type =
              if Retry.is_rate_limit_error err_msg then "rate_limit"
              else if Retry.is_temporary_error err_msg then "temporary_error"
              else if Retry.is_connection_error err_msg then "connection_error"
              else "error"
            in
            { model = sprintf "glm (%s)" model;
              returncode = -1;
              response = sprintf "Error after retries: %s" err_msg;
              extra = [("error", error_type); ("retried", "true")]; }
        end  (* close else begin for non-streaming *)
        end)  (* close match require_api_key *)

  | GeminiList { filter; include_all } ->
      Handler_gemini.list_models ~sw ~proc_mgr ~clock ~filter ~include_all ()

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

  | ChainRun { chain; mermaid; chain_id; input; trace; checkpoint_enabled; timeout = user_timeout } ->
      (* Parse from either JSON, Mermaid (WYSIWYE), or a registered preset chain_id. *)
      let parse_result =
        match (chain, mermaid, chain_id) with
        | (Some c, _, _) -> Chain_parser.parse_chain c
        | (_, Some m, _) -> Chain_mermaid_parser.parse_mermaid_to_chain m
        | (None, None, Some id) ->
            (match Chain_registry.lookup id with
             | Some c -> Ok c
             | None -> Error (sprintf "Chain '%s' not found in registry (use chain.list)" id))
        | (None, None, None) ->
            Error "Either 'chain' (JSON) or 'mermaid' (string) or 'chain_id' is required"
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
              (* Use user timeout if provided, else chain's global timeout *)
              let chain_timeout = parsed_chain.Chain_types.config.Chain_types.timeout in
              let timeout_int = match user_timeout with
                | Some t -> t
                | None -> chain_timeout
              in
              (* Create checkpoint config if enabled *)
              let checkpoint_config = if checkpoint_enabled then
                Some (Chain_executor_eio.make_checkpoint_config ~enabled:true ())
              else
                None
              in
              let env_truthy name =
                match Sys.getenv_opt name with
                | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
                | _ -> false
              in
              let trace_effective = trace || env_truthy "LLM_MCP_CHAIN_FORCE_TRACE" in
              let gemini_api_key = Tools_tracer.get_api_key "GEMINI_API_KEY" |> String.trim in
              let gemini_use_cli_default =
                (* Prefer direct API when key is present to avoid local CLI drift
                   (e.g. "only 2.0 models are visible" depending on CLI install). *)
                if gemini_api_key <> "" then false else Tool_parsers.default_use_cli ()
              in
              let starts_with ~prefix s =
                let prefix_len = String.length prefix in
                String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
              in
              let is_gemini_model m =
                m = "gemini" ||
                m = "pro" || m = "flash" || m = "flash-lite" ||
                m = "3-pro" || m = "3-flash" ||
                starts_with ~prefix:"gemini-" m
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
              let exec_fn ~model ?system ~prompt ?tools ?thinking () =
                let _ = thinking in  (* Available for future enhancement *)
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
                        output_format = Types.Text;
                        timeout = timeout_int;
                        stream = false;
                        use_cli = false;  (* Stub uses direct API *)
                        fallback_to_api = false;
                      }
                  | m when is_gemini_model m ->
                      Types.Gemini {
                        prompt;
                        model;
                        thinking_level = Types.High;
                        yolo = false;
                        output_format = Types.Text;
                        timeout = timeout_int;
                        stream = false;
                        use_cli = gemini_use_cli_default;
                        fallback_to_api = true;
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
                        timeout = timeout_int;
                        stream = false;
                        use_cli = true;
                        fallback_to_api = true;
                        api_key = None;
                      }
                  | "codex" | "gpt-5.2" ->
                      Types.Codex {
                        prompt;
                        model = "gpt-5.2";
                        reasoning_effort = Types.RXhigh;
                        sandbox = Types.WorkspaceWrite;
                        working_directory = None;
                        timeout = timeout_int;
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
                        timeout = timeout_int;
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
                        timeout = timeout_int;
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
                        timeout = timeout_int;
                        stream = false;
                        thinking = false;  (* Faster for chain execution *)
                        do_sample = true;
                        web_search = false;
                        tools = [];  (* No tools for chain execution by default *)
                        api_key = None;
                      }
                  | _ ->
                      (* Default to Gemini for unknown models *)
                      Types.Gemini {
                        prompt;
                        model = "gemini-3-pro-preview";
                        thinking_level = Types.High;
                        yolo = false;
                        output_format = Types.Text;
                        timeout = timeout_int;
                        stream = false;
                        use_cli = gemini_use_cli_default;
                        fallback_to_api = true;
                      }
                in
                let args = with_system_prompt system args in
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
                        ~server_name ~tool_name ~arguments:args ~timeout:timeout_int
                    in
                    if starts_with ~prefix:"Error:" output then Error output else Ok output
                | None ->
                    let result =
	                      match name with
	                      | "gemini" ->
	                          let args = parse_gemini_args args in
	                          execute ~sw ~proc_mgr ~clock args
	                      | "gemini_list" ->
	                          let args = parse_gemini_list_args args in
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
              let result =
                with_masc_hook ~sw ~proc_mgr ~clock ~label:"chain.run" (fun () ->
                  Chain_executor_eio.execute
                    ~sw ~clock ~timeout:timeout_int ~trace:trace_effective ~exec_fn ~tool_exec ?input ?checkpoint:checkpoint_config plan)
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
           (match Chain_mermaid_parser.parse_mermaid_to_chain mermaid_text with
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

  | ChainOrchestrate { goal; chain; tasks; chain_id; max_replans; timeout; trace; verify_on_complete; orchestrator_model } ->
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
              (Random.State.int tools_rng 10000)
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
              use_cli = true;
              fallback_to_api = true;
              api_key = None;
            } in
            let result = execute ~sw ~proc_mgr ~clock args in
            if result.returncode = 0 then result.response
            else failwith (Printf.sprintf "Claude call failed: %s" result.response)
        | "codex" ->
            let args = Types.Codex {
              prompt;
              model = "gpt-5.2";
              reasoning_effort = Types.RLow;
              sandbox = Types.WorkspaceWrite;
              working_directory = Some (Sys.getcwd ());
              timeout = 120;
              stream = false;
              use_cli = true;
              fallback_to_api = true;
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
        | model ->
            let args = Types.Gemini {
              prompt;
              model;
              thinking_level = Types.High;
              yolo = false;
              output_format = Types.Text;
              timeout = 120;
              stream = false;
              use_cli = false;  (* Avoid CLI exec issues in orchestration *)
              fallback_to_api = false;
            } in
            let result = execute ~sw ~proc_mgr ~clock args in
            if result.returncode = 0 then result.response
            else failwith (Printf.sprintf "Gemini call failed: %s" result.response)
      in

      (* Helper: parse "server.tool" format *)
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

      (* Load chain from chain_id if provided and chain is not given *)
      let effective_chain = match chain with
        | Some _ -> chain  (* Use provided chain *)
        | None ->
            (* Try to load from chain_id using LLM_MCP_REPO_ROOT for absolute path *)
            (match chain_id with
             | Some cid ->
                 let default_me_root () =
                   let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
                   Filename.concat home "me"
                 in
                 let find_repo_root () =
                   match Sys.getenv_opt "LLM_MCP_REPO_ROOT" with
                   | Some path ->
                       Log.info "chain_id_load" "Using LLM_MCP_REPO_ROOT: %s" path;
                       path
                   | None ->
                       let me_root = Sys.getenv_opt "ME_ROOT" |> Option.value ~default:(default_me_root ()) in
                       Log.info "chain_id_load" "ME_ROOT: %s" me_root;
                       let candidates = [
                         Filename.concat me_root "llm-mcp";
                         Filename.concat (Filename.concat me_root "workspace") "llm-mcp";
                         Filename.concat (Filename.concat (Filename.concat me_root "workspace") "yousleepwhen") "llm-mcp";
                       ] in
                       List.iter (fun c -> Log.info "chain_id_load" "Candidate: %s (exists: %b)" c (Sys.file_exists c)) candidates;
                       match List.find_opt Sys.file_exists candidates with
                       | Some path -> Log.info "chain_id_load" "Found repo root: %s" path; path
                       | None -> Log.warn "chain_id_load" "No repo root found, using cwd"; "."
                 in
                 let repo_root = find_repo_root () in
                 let chain_path = Filename.concat (Filename.concat repo_root "data") (Filename.concat "chains" (cid ^ ".json")) in
                 Log.info "chain_id_load" "Chain path: %s (exists: %b)" chain_path (Sys.file_exists chain_path);
                 (try
                   let content =
                     In_channel.with_open_bin chain_path (fun ic ->
                       really_input_string ic (in_channel_length ic))
                   in
                   Log.info "chain_id_load" "Loaded chain file (%d bytes)" (String.length content);
                   Some (Yojson.Safe.from_string content)
                 with e ->
                   Log.error "chain_id_load" "Failed to load chain: %s" (Printexc.to_string e);
                   None)
             | None -> None)
      in

      let parse_tasks_from_json (json: Yojson.Safe.t) =
        match json with
        | `List items ->
            List.filter_map (fun item ->
              match Chain_composer.masc_task_of_yojson item with
              | Ok t -> Some t
              | Error _ -> None
            ) items
        | _ -> []
      in

      (* Create tasks from explicit tasks list or from chain if provided, otherwise empty *)
      let tasks_from_input = match tasks with
        | Some t -> parse_tasks_from_json t
        | None -> []
      in
      let tasks_from_chain = match effective_chain with
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
      let tasks = if tasks_from_input <> [] then tasks_from_input else tasks_from_chain in

      (* FIXED: Use effective_chain (includes chain_id loaded file), not just chain *)
      let initial_chain =
        match effective_chain with
        | None ->
            Log.info "chain_orchestrate" "effective_chain is None";
            Ok None
        | Some chain_json ->
            Log.info "chain_orchestrate" "effective_chain present, attempting parse...";
            (match Chain_parser.parse_chain chain_json with
             | Ok parsed ->
                 Log.info "chain_orchestrate" "Parse SUCCESS: chain id=%s, nodes=%d"
                   parsed.id (List.length parsed.nodes);
                 Ok (Some parsed)
             | Error msg ->
                 Log.error "chain_orchestrate" "Parse FAILED: %s" msg;
                 Error msg)
      in

      (* Run orchestration *)
      (match initial_chain with
      | Error msg ->
          { model = "chain.orchestrate";
            returncode = 1;
            response = Printf.sprintf "Chain parse error: %s" msg;
            extra = [("stage", "parse")]; }
      | Ok initial_chain ->
      Log.info "chain_orchestrate" "Calling orchestrator with initial_chain=%s"
        (match initial_chain with Some c -> Printf.sprintf "Some(id=%s)" c.id | None -> "None");
      (match with_masc_hook ~sw ~proc_mgr ~clock ~label:"chain.orchestrate" (fun () ->
        Chain_orchestrator_eio.orchestrate ~sw ~clock ~config ~llm_call ~tool_exec ~goal ~tasks ~initial_chain) with
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
      )

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
              let now = Time_compat.now () in
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
                     let exec_fn ~model ?system ~prompt ?tools ?thinking () =
                       let _ = thinking in
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
                           prompt; model; thinking_level = High; yolo = false; output_format = Text; timeout = node_timeout; stream = false; use_cli = true; fallback_to_api = true
                          })
                         else if starts_with ~prefix:"claude" model_name then
                           execute ~sw ~proc_mgr ~clock (Claude {
                             prompt; model; long_context = false; system_prompt = system;
                             output_format = Text; allowed_tools = []; working_directory = "";
                             timeout = node_timeout; stream = false; use_cli = true; fallback_to_api = true;
                             api_key = None
                           })
                         else if starts_with ~prefix:"codex" model_name || starts_with ~prefix:"gpt" model_name then
                           execute ~sw ~proc_mgr ~clock (Codex {
                             prompt; model; reasoning_effort = RHigh; sandbox = ReadOnly;
                             working_directory = None; timeout = node_timeout; stream = false;
                             use_cli = true; fallback_to_api = true
                           })
                         else if starts_with ~prefix:"ollama" model_name then
                           let ollama_model = match String.index_opt model ':' with
                             | Some idx -> String.sub model (idx + 1) (String.length model - idx - 1)
                             | None -> "devstral"
                           in
                           execute ~sw ~proc_mgr ~clock (Ollama {
                             prompt; model = ollama_model; system_prompt = system;
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
                     let result =
                       with_masc_hook ~sw ~proc_mgr ~clock ~label:"chain.resume" (fun () ->
                         Chain_executor_eio.execute
                           ~sw ~clock ~timeout:node_timeout ~trace ~exec_fn ~tool_exec
                           ~checkpoint:checkpoint_config plan)
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
        created_at = Time_compat.now ();
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

  | GlmTranslate { text; source_lang; target_lang; strategy; model; timeout } ->
      (* GLM Translation Agent - 6 strategies *)
      let model_name = sprintf "glm.translate (%s)" model in
      (match Tools_tracer.require_api_key ~env_var:"ZAI_API_KEY" ~model:model_name ~extra:[] with
      | Some err -> err
      | None ->
        let api_key = Tools_tracer.get_api_key "ZAI_API_KEY" in
        begin
        (* Build translation prompt based on strategy *)
        let strategy_name = Types.string_of_translation_strategy strategy in
        let prompt = match strategy with
          | Types.TransGeneral ->
              sprintf "Translate the following text from %s to %s:\n\n%s" source_lang target_lang text
          | Types.TransParaphrased ->
              sprintf "Translate the following text from %s to %s. Focus on natural, fluent expression rather than literal translation:\n\n%s" source_lang target_lang text
          | Types.TransTwoStep ->
              sprintf {|Translate the following text from %s to %s using two steps:

Step 1: Provide a literal translation
Step 2: Refine into a natural, fluent translation

Text: %s

Provide both translations, then give the final refined version.|} source_lang target_lang text
          | Types.TransThreeStage ->
              sprintf {|Translate the following text from %s to %s using three stages:

Stage 1: Literal translation
Stage 2: Natural/paraphrased translation
Stage 3: Expert review and final polish

Text: %s

Show each stage and provide the final polished translation.|} source_lang target_lang text
          | Types.TransReflective ->
              sprintf {|Translate the following text from %s to %s using reflective translation:

1. Draft Translation: Provide your initial translation
2. Self-Critique: What could be improved? Consider accuracy, fluency, cultural appropriateness
3. Revised Translation: Apply improvements based on your critique

Text: %s

Show all three steps and provide the final revised translation.|} source_lang target_lang text
          | Types.TransChainOfThought ->
              sprintf {|Translate the following text from %s to %s step by step:

1. Identify key concepts and terminology
2. Consider cultural and contextual nuances
3. Choose appropriate register and tone
4. Translate with detailed reasoning

Text: %s

Show your reasoning process and provide the final translation.|} source_lang target_lang text
        in
        let messages = `List [
          `Assoc [("role", `String "system"); ("content", `String (sprintf "You are an expert translator specializing in %s to %s translation. Use the %s translation strategy." source_lang target_lang strategy_name))];
          `Assoc [("role", `String "user"); ("content", `String prompt)]
        ] in
        let thinking_enabled = match strategy with
          | Types.TransReflective | Types.TransChainOfThought -> true
          | _ -> false
        in
        let thinking_obj = `Assoc [("type", `String (if thinking_enabled then "enabled" else "disabled"))] in
        let body_fields = [
          ("model", `String model);
          ("messages", messages);
          ("temperature", `Float 0.3);  (* Lower temp for translation accuracy *)
          ("do_sample", `Bool true);
          ("thinking", thinking_obj);
          ("stream", `Bool false);
        ] in
        let body = Yojson.Safe.to_string (`Assoc body_fields) in
        let cmd = "curl" in
        let cmd_args = [
          "-s"; "-X"; "POST";
          "https://api.z.ai/api/coding/paas/v4/chat/completions";
          "-H"; "Content-Type: application/json";
          "-H"; sprintf "Authorization: Bearer %s" api_key;
          "-d"; body;
          "--max-time"; string_of_int timeout;
        ] in
        (* Wrap with retry logic *)
        let translate_call () =
          match run_command ~sw ~proc_mgr ~clock cmd cmd_args ~timeout:(timeout * 1000) with
          | Ok r ->
              if Retry.is_rate_limit_error r.stdout || Retry.is_temporary_error r.stdout then
                Error r.stdout
              else
                Ok r
          | Error (Timeout rc) -> Error (sprintf "Timeout (code %d)" rc)
          | Error (ProcessError msg) -> Error msg
        in
        let retry_result = Retry.with_rate_limit_retry ~clock translate_call in
        let result = Retry.to_result retry_result in
        match result with
        | Error err_msg ->
            { model = sprintf "glm.translate:%s:error" model;
              returncode = 1;
              response = sprintf "Translation error after retries: %s" err_msg;
              extra = [
                ("strategy", strategy_name);
                ("source_lang", source_lang);
                ("target_lang", target_lang);
                ("error", "true");
                ("retried", "true");
              ]; }
        | Ok r ->
            (* Parse response from stdout *)
            let translation =
              try
                let json = Yojson.Safe.from_string r.stdout in
                let open Yojson.Safe.Util in
                let choices = json |> member "choices" |> to_list in
                if List.length choices > 0 then
                  (List.hd choices) |> member "message" |> member "content" |> to_string
                else
                  r.stdout
              with _ -> r.stdout
            in
            { model = sprintf "glm.translate:%s:%s" model strategy_name;
              returncode = r.exit_code;
              response = translation;
              extra = [
                ("strategy", strategy_name);
                ("source_lang", source_lang);
                ("target_lang", target_lang);
              ]; }
        end)  (* close match require_api_key *)

  | SetStreamDelta { enabled } ->
      let _ = (sw, proc_mgr, clock) in  (* Unused but needed for signature consistency *)
      let was = stream_delta_enabled () in  (* Capture BEFORE setting *)
      let current = set_stream_delta enabled in
      { model = "set_stream_delta";
        returncode = 0;
        response = sprintf "SSE stream delta broadcasting %s (was: %s)"
          (if current then "enabled" else "disabled")
          (if was then "enabled" else "disabled");
        extra = [("enabled", string_of_bool current); ("was", string_of_bool was)]; }

  | GetStreamDelta ->
      let _ = (sw, proc_mgr, clock) in  (* Unused but needed for signature consistency *)
      let status = get_stream_delta () in
      let source = Tools_stream_config.source () in
      { model = "get_stream_delta";
        returncode = 0;
        response = sprintf "SSE stream delta: %s (source: %s)"
          (if status then "enabled" else "disabled")
          source;
        extra = [("enabled", string_of_bool status); ("source", source)]; }

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

(** {1 Ollama Agentic Execution} - Re-exports from Tools_ollama_agentic *)

let ollama_base_url = Tools_ollama_agentic.base_url
type agent_message = Tools_ollama_agentic.agent_message = {
  role : string;
  content : string;
  tool_calls : Ollama_parser.tool_call list option;
  name : string option;
}
let agent_message_to_json = Tools_ollama_agentic.agent_message_to_json
let build_agentic_request = Tools_ollama_agentic.build_chat_request

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
              let msg = Safe_parse.json_string ~context:"mcp:error" ~default:"Unknown error" error "message" in
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
          let exec_fn ~model ?system ~prompt ?tools ?thinking () =
            let _ = thinking in  (* Available for future enhancement *)
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
            let lowered = String.lowercase_ascii model in
            let is_gemini_model m =
              m = "gemini" ||
              m = "pro" || m = "flash" || m = "flash-lite" ||
              m = "3-pro" || m = "3-flash" ||
              starts_with ~prefix:"gemini-" m
            in
            match lowered with
            | "stub" | "mock" ->
                Ok (sprintf "[%s]%s" model prompt)
            | m when is_gemini_model m ->
                let args = Gemini {
                  prompt;
                  model;
                  thinking_level = High;
                  yolo = false;
                  output_format = Text;
                  timeout;
                  stream = false;
                  use_cli = false;  (* Avoid CLI exec issues; use direct API *)
                  fallback_to_api = true;
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
                  system_prompt = system;
                  output_format = Text;
                  allowed_tools = [];
                  working_directory;
                  timeout;
                  stream = false;
                  use_cli = true;
                  fallback_to_api = true;
                  api_key = None;
                } in
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response else Error result.response
            | "codex" ->
                let args = Codex {
                  prompt;
                  model = "gpt-5.2";
                  reasoning_effort = RHigh;
                  sandbox = WorkspaceWrite;
                  working_directory = None;
                  timeout;
                  stream = false;
                  use_cli = true;
                  fallback_to_api = true;
                } in
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response else Error result.response
            | m when String.length m > 7 && String.sub m 0 7 = "ollama:" ->
                let ollama_model = String.sub model 7 (String.length model - 7) in
                let args = Ollama {
                  prompt;
                  model = ollama_model;
                  system_prompt = system;
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
                  system_prompt = system;
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
	                  | "gemini_list" ->
	                      let args = parse_gemini_list_args args in
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
          let result =
            with_masc_hook ~sw ~proc_mgr ~clock ~label:"chain.run.registry" (fun () ->
              Chain_executor_eio.execute ~sw ~clock ~timeout ~trace ~exec_fn ~tool_exec plan)
          in
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

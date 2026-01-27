(** LLM Clients - Direct API & CLI integrations for Gemini, Claude, Codex, Ollama *)

open Printf
open Types
open Cli_runner_eio

(* Gemini error handling - from Types module *)
let classify_gemini_error = Types.classify_gemini_error
let is_recoverable_gemini_error = Types.is_recoverable_gemini_error
let string_of_gemini_error = Types.string_of_gemini_error

(* Ollama helpers *)
let tool_calls_to_json = Tool_parsers.tool_calls_to_json

(** {1 Streaming Execution} *)

(** Runtime toggle for stream delta (can be changed without restart) *) 
let stream_delta_override : bool option ref = ref None

let stream_delta_enabled () =
  match !stream_delta_override with
  | Some v -> v  (* Runtime override takes precedence *)
  | None ->
      match Sys.getenv_opt "LLM_MCP_STREAM_DELTA" with
      | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
      | _ -> false

let set_stream_delta enabled =
  stream_delta_override := Some enabled;
  enabled

let get_stream_delta () =
  stream_delta_enabled ()

let stream_delta_max_events () =
  match Sys.getenv_opt "LLM_MCP_STREAM_DELTA_MAX_EVENTS" with
  | Some v -> (try int_of_string v with _ -> 2000)
  | None -> 2000

let stream_delta_max_chars () =
  match Sys.getenv_opt "LLM_MCP_STREAM_DELTA_MAX_CHARS" with
  | Some v -> (try int_of_string v with _ -> 200)
  | None -> 200

let generate_stream_id model_name =
  let ts = int_of_float (Unix.gettimeofday () *. 1000.) in
  Printf.sprintf "%s:%d:%d" model_name ts (Random.int 1000000)

(** Execute Ollama with token streaming callback *)
let execute_ollama_streaming ~sw ~proc_mgr ~clock ~on_token ?stream_id args =
  let (cmd_result, model_name, extra_base, has_tools, err_msg) = match args with
    | Ollama { model; temperature; tools; timeout = _; _ } ->
        let has_tools = match tools with Some l when List.length l > 0 -> true | _ -> false in
        (Tool_parsers.build_ollama_curl_cmd ~force_stream:(Some true) args,
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
      try Notification_sse.broadcast json with _ -> ()
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
  | Ok cmd_list ->
      if cmd_list = [] then
        let response = "Invalid args" in
        emit_end ~success:false ~error:response ();
        { model = model_name; returncode = -1; response; extra = extra_base }
      else begin
        let cmd = List.hd cmd_list in
        let cmd_args = List.tl cmd_list in
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

let gemini_breaker = Mcp_resilience.create_circuit_breaker ~name:"gemini_cli" ~failure_threshold:3 ()

(** Execute Gemini via Direct API (faster, no MASC) *)
let execute_gemini_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~thinking_level ~timeout ~stream =
  let model_name = sprintf "gemini-api (%s)" model in
  let thinking_applied = thinking_level = High in
  let extra_base = [
    ("thinking_level", string_of_thinking_level thinking_level);
    ("thinking_prompt_applied", string_of_bool thinking_applied);
    ("use_cli", "false");
  ] in

  (* Get API key *)
  let api_key = match Sys.getenv_opt "GEMINI_API_KEY" with
    | Some k -> k
    | None -> ""
  in
  if String.length api_key = 0 then
    { model = model_name;
      returncode = -1;
      response = "Error: GEMINI_API_KEY environment variable not set";
      extra = extra_base @ [("error", "missing_api_key")]; }
  else begin
    (* Build API request body *)
    let contents = `List [
      `Assoc [
        ("parts", `List [
          `Assoc [("text", `String prompt)]
        ])
      ]
    ] in
    let body = `Assoc [("contents", contents)] in
    let body_str = Yojson.Safe.to_string body in

    (* Map model alias to API model ID *)
    let api_model = match model with
      | "pro" -> "gemini-2.5-pro"
      | "flash" -> "gemini-2.5-flash"
      | "flash-lite" -> "gemini-2.5-flash-lite"
      | "3-pro" -> "gemini-3-pro-preview"
      | "3-flash" -> "gemini-3-flash-preview"
      | m -> m
    in

    (* Build curl command *)
    let url = sprintf "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent" api_model in
    let curl_args = [
      "-s"; "-S";
      "-H"; sprintf "x-goog-api-key: %s" api_key;
      "-H"; "Content-Type: application/json";
      "-d"; body_str;
      url
    ] in

    let _ = stream in
    let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" curl_args in
    match result with
    | Ok r ->
        let raw_response = get_output r in
        (try
          let json = Yojson.Safe.from_string raw_response in
          let open Yojson.Safe.Util in
          let text = json
            |> member "candidates"
            |> index 0
            |> member "content"
            |> member "parts"
            |> index 0
            |> member "text"
            |> to_string
          in
          { model = model_name;
            returncode = 0;
            response = text;
            extra = extra_base; }
        with _ ->
          (try
            let json = Yojson.Safe.from_string raw_response in
            let open Yojson.Safe.Util in
            let error_msg = json |> member "error" |> member "message" |> to_string in
            { model = model_name;
              returncode = -1;
              response = sprintf "API Error: %s" error_msg;
              extra = extra_base @ [("api_error", "true")]; }
          with _ ->
            { model = model_name;
              returncode = -1;
              response = sprintf "Failed to parse API response: %s" (String.sub raw_response 0 (min 200 (String.length raw_response)));
              extra = extra_base @ [("parse_error", "true")]; }))
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

(** Execute Claude via Direct Anthropic API (faster, no CLI overhead) *)
let execute_claude_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~system_prompt ~timeout ~stream =
  let model_name = sprintf "claude-api (%s)" model in
  let extra_base = [("use_cli", "false")] in

  (* Get API key *)
  let api_key = match Sys.getenv_opt "ANTHROPIC_API_KEY" with
    | Some k -> k
    | None -> ""
  in
  if String.length api_key = 0 then
    { model = model_name;
      returncode = -1;
      response = "Error: ANTHROPIC_API_KEY environment variable not set";
      extra = extra_base @ [("error", "missing_api_key")]; }
  else begin
    let api_model = match model with
      | "opus" -> "claude-opus-4-20250514"
      | "sonnet" -> "claude-sonnet-4-20250514"
      | "haiku" -> "claude-3-5-haiku-20241022"
      | m -> m
    in

    let messages = `List [
      `Assoc [("role", `String "user"); ("content", `String prompt)]
    ] in
    let body_fields = [
      ("model", `String api_model);
      ("max_tokens", `Int 4096);
      ("messages", messages);
    ] in
    let body_fields = match system_prompt with
      | Some sp -> ("system", `String sp) :: body_fields
      | None -> body_fields
    in
    let body = `Assoc body_fields in
    let body_str = Yojson.Safe.to_string body in

    let curl_args = [
      "-s"; "-S";
      "-H"; sprintf "x-api-key: %s" api_key;
      "-H"; "content-type: application/json";
      "-H"; "anthropic-version: 2023-06-01";
      "-d"; body_str;
      "https://api.anthropic.com/v1/messages"
    ] in

    let _ = stream in
    let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" curl_args in
    match result with
    | Ok r ->
        let raw_response = get_output r in
        (try
          let json = Yojson.Safe.from_string raw_response in
          let open Yojson.Safe.Util in
          let text = json
            |> member "content"
            |> index 0
            |> member "text"
            |> to_string
          in
          { model = model_name;
            returncode = 0;
            response = text;
            extra = extra_base; }
        with _ ->
          (try
            let json = Yojson.Safe.from_string raw_response in
            let open Yojson.Safe.Util in
            let error_msg = json |> member "error" |> member "message" |> to_string in
            { model = model_name;
              returncode = -1;
              response = sprintf "API Error: %s" error_msg;
              extra = extra_base @ [("api_error", "true")]; }
          with _ ->
            { model = model_name;
              returncode = -1;
              response = sprintf "Failed to parse API response: %s" (String.sub raw_response 0 (min 200 (String.length raw_response)));
              extra = extra_base @ [("parse_error", "true")]; }))
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

(** Execute Codex via Direct OpenAI API (faster, no CLI overhead) *)
let execute_codex_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~timeout ~stream =
  let model_name = sprintf "codex-api (%s)" model in
  let extra_base = [("use_cli", "false")] in

  (* Get API key *)
  let api_key = match Sys.getenv_opt "OPENAI_API_KEY" with
    | Some k -> k
    | None -> ""
  in
  if String.length api_key = 0 then
    { model = model_name;
      returncode = -1;
      response = "Error: OPENAI_API_KEY environment variable not set";
      extra = extra_base @ [("error", "missing_api_key")]; }
  else begin
    let api_model = match model with
      | "gpt-5.2" -> "gpt-5.2"
      | "gpt-5" -> "gpt-5"
      | "o3" -> "o3"
      | "o4-mini" -> "o4-mini"
      | m -> m
    in

    let messages = `List [
      `Assoc [("role", `String "user"); ("content", `String prompt)]
    ] in
    let body = `Assoc [
      ("model", `String api_model);
      ("messages", messages);
    ] in
    let body_str = Yojson.Safe.to_string body in

    let curl_args = [
      "-s"; "-S";
      "-H"; sprintf "Authorization: Bearer %s" api_key;
      "-H"; "Content-Type: application/json";
      "-d"; body_str;
      "https://api.openai.com/v1/chat/completions"
    ] in

    let _ = stream in
    let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" curl_args in
    match result with
    | Ok r ->
        let raw_response = get_output r in
        (try
          let json = Yojson.Safe.from_string raw_response in
          let open Yojson.Safe.Util in
          let text = json
            |> member "choices"
            |> index 0
            |> member "message"
            |> member "content"
            |> to_string
          in
          { model = model_name;
            returncode = 0;
            response = text;
            extra = extra_base; }
        with _ ->
          (try
            let json = Yojson.Safe.from_string raw_response in
            let open Yojson.Safe.Util in
            let error_msg = json |> member "error" |> member "message" |> to_string in
            { model = model_name;
              returncode = -1;
              response = sprintf "API Error: %s" error_msg;
              extra = extra_base @ [("api_error", "true")]; }
          with _ ->
            { model = model_name;
              returncode = -1;
              response = sprintf "Failed to parse API response: %s" (String.sub raw_response 0 (min 200 (String.length raw_response)));
              extra = extra_base @ [("parse_error", "true")]; }))
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

(** Execute Gemini with automatic retry for recoverable errors *)
let execute_gemini_with_retry ~sw ~proc_mgr ~clock
    ?(max_retries = 2)
    ~model ~thinking_level ~timeout ~stream ~args () =

  let thinking_applied = thinking_level = High in
  let model_name = sprintf "gemini (%s)" model in
  let extra_base = [
    ("thinking_level", string_of_thinking_level thinking_level);
    ("thinking_prompt_applied", string_of_bool thinking_applied);
  ] in

  match Tool_parsers.build_gemini_cmd args with
  | Error err ->
      { model = model_name;
        returncode = -1;
        response = err;
        extra = extra_base @ [("invalid_args", "true")]; }
  | Ok cmd_list ->
      let cmd = List.hd cmd_list in
      let cmd_args = List.tl cmd_list in

      if stream then
        execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args
      else begin
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
            { model = model_name; returncode = exit_code; response; extra = extra_base }
        | Error err ->
            { model = model_name; returncode = -1; response = err; extra = [] }
        | CircuitOpen ->
            { model = model_name; returncode = -1; response = "Circuit breaker open"; extra = [] }
        | TimedOut ->
            { model = model_name; returncode = -1; response = "Operation timed out"; extra = [] }
      end

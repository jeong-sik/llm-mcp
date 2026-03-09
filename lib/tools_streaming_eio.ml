open Printf
open Types
open Cli_runner_eio

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
    | None -> Tools_stream_config.generate_id model_name
  in
  let delta_enabled = Tools_stream_config.enabled () in
  let delta_max_events = Tools_stream_config.max_events () in
  let delta_max_chars = Tools_stream_config.max_chars () in
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

let execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args =
  let stream_id = Tools_stream_config.generate_id model_name in
  let delta_enabled = Tools_stream_config.enabled () in
  let delta_max_events = Tools_stream_config.max_events () in
  let delta_max_chars = Tools_stream_config.max_chars () in
  let delta_count = ref 0 in
  let delta_truncated = ref false in
  let total_chars = ref 0 in
  let broadcast_delta json =
    if delta_enabled then
      try Notification_sse.broadcast json
      with exn ->
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
      let success = exit_code = 0 in
      emit_end ~success ();
      { model = model_name;
        returncode = exit_code;
        response = partial_response;
        extra = extra_with_stream; }
  | Error (Timeout t) ->
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
      emit_end ~success:false ~error:msg ();
      let response = if String.length partial_response > 0
        then partial_response ^ "\n\n[ERROR: " ^ msg ^ "]"
        else sprintf "Error: %s" msg in
      { model = model_name;
        returncode = -1;
        response;
        extra = extra_with_stream @ [("process_error", "true")]; }

open Printf
open Cli_runner_eio

let env_truthy value =
  match String.lowercase_ascii value with
  | "1" | "true" | "yes" | "on" -> true
  | "0" | "false" | "no" | "off" -> false
  | _ -> false

let parse_mcp_http_response = Tools_mcp_parse.parse_http_response

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
  match Tool_config.get_mcp_server_config "masc" with
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
    Eio.Switch.run (fun masc_sw ->
      Eio.Switch.on_release masc_sw (fun () ->
        try
          ignore (call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_leave" ~arguments:leave_args)
        with ex ->
          Log.warn "masc_hook" "masc_leave failed in on_release: %s"
            (Printexc.to_string ex)
      );
      let _ = call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_join" ~arguments:join_args in
      let interval = float_of_int (masc_heartbeat_interval ()) in
      let _ =
        Eio.Fiber.fork ~sw:masc_sw (fun () ->
          let rec loop () =
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

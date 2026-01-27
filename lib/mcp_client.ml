(** MCP Client - HTTP/Stdio client for tool execution *)

open Printf
open Cli_runner_eio

(** Parse MCP HTTP response (streamable-http SSE or plain JSON-RPC) *)
let parse_mcp_http_response (body : string) : string option =
  let parse_json_result json_str =
    try
      let json = Yojson.Safe.from_string json_str in
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let error = json |> member "error" in
      if error <> `Null then
        let msg = try error |> member "message" |> to_string
                  with _ -> "Unknown error" in
        Some (Printf.sprintf "Error: %s" msg)
      else
        let content = result |> member "content" in
        (match content with
         | `List items ->
             let texts = List.filter_map (fun item ->
               match item |> member "type" |> to_string_option with
               | Some "text" -> item |> member "text" |> to_string_option
               | _ -> None
             ) items in
             Some (String.concat "\n" texts)
         | _ ->
             (match result with
              | `Assoc _ | `List _ ->
                  Some (Yojson.Safe.to_string result)
              | _ ->
                  let result_str = result |> to_string_option in
                  Some (Option.value result_str ~default:json_str)))
    with _ -> None
  in
  let lines = String.split_on_char '\n' body in
  let data_lines =
    List.filter_map (fun l ->
      if String.length l > 5 && String.sub l 0 5 = "data:" then
        Some (String.sub l 5 (String.length l - 5) |> String.trim)
      else None
    ) lines
  in
  let rec find_last_json = function
    | [] -> None
    | h :: t ->
        (match parse_json_result h with
         | Some v -> Some v
         | None -> find_last_json t)
  in
  match find_last_json (List.rev data_lines) with
  | Some v -> Some v
  | None -> parse_json_result body

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
          parse_mcp_http_response r.stdout |> Option.value ~default:r.stdout

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

(** {1 MASC Integration} *)

let env_truthy value =
  match String.lowercase_ascii value with
  | "1" | "true" | "yes" | "on" -> true
  | "0" | "false" | "no" | "off" -> false
  | _ -> false

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
    let ts = int_of_float (Unix.gettimeofday ()) in
    let agent =
      let safe_label = String.map (fun c -> if c = '.' then '-' else c) label in
      Printf.sprintf "%s-%s-%d" base safe_label ts
    in
    let join_args = `Assoc [
      ("agent_name", `String agent);
      ("capabilities", `List [`String "chain"])
    ] in
    let heartbeat_args = `Assoc [("agent_name", `String agent)] in
    let leave_args = `Assoc [("agent_name", `String agent)] in
    let _ = call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_join" ~arguments:join_args in
    Fun.protect
      ~finally:(fun () ->
        ignore (call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_leave" ~arguments:leave_args))
      (fun () ->
        Eio.Switch.run (fun hb_sw ->
          let interval = float_of_int (masc_heartbeat_interval ()) in
          let _ =
            Eio.Fiber.fork ~sw:hb_sw (fun () ->
              let rec loop () =
                ignore (call_masc_tool ~sw ~proc_mgr ~clock ~tool_name:"masc_heartbeat" ~arguments:heartbeat_args);
                Eio.Time.sleep clock interval;
                loop ()
              in
              loop ())
          in
          f ()))
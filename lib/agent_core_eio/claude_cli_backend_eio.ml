(** Claude CLI Backend Eio - Effect-based LLM_BACKEND for Claude Code CLI

    Direct-style version using Eio instead of Lwt.
    Wraps the `claude` CLI tool with JSON output mode for programmatic access.
    Note: Tool execution is handled internally by Claude CLI, so this backend
    only returns final responses (no tool_calls).

    Usage:
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
        let proc_mgr = Eio.Stdenv.process_mgr env in
        let config = Claude_cli_backend_eio.{
          model = "opus";  (* or "sonnet", "haiku" *)
          timeout_ms = Some 120_000;
          system_prompt = None;
          allowed_tools = None;
        } in

        let module Backend = Claude_cli_backend_eio in
        let result = Backend.call ~sw ~proc_mgr ~config ~messages ~tools in
        ...
    ]}
*)

open Agent_types

(** {1 Configuration} *)

type config = {
  model : string;              (** Model alias: "opus", "sonnet", "haiku" *)
  timeout_ms : int option;     (** Command timeout in ms (None = no timeout) *)
  system_prompt : string option;  (** Optional system prompt override *)
  allowed_tools : string list option;  (** Allowed tools (None = default) *)
  print_mode : bool;           (** Always true for non-interactive mode *)
}

let default_config = {
  model = "opus";
  timeout_ms = Some 120_000;  (* 2 minutes default *)
  system_prompt = None;
  allowed_tools = None;
  print_mode = true;
}

(** {1 Response Types} *)

type response = {
  content : string;
  tool_calls : tool_call list;  (* Always empty - Claude CLI handles tools internally *)
  model : string option;
  cost_usd : float option;
  duration_ms : int option;
  session_id : string option;
}

(** {1 Command Building} *)

(** Build claude CLI command with arguments *)
let build_command ~(config:config) ~(prompt:string) =
  let base = ["claude"; "-p"; "--output-format"; "json"] in
  let with_model = base @ ["--model"; config.model] in
  let with_system = match config.system_prompt with
    | Some sp -> with_model @ ["--system-prompt"; sp]
    | None -> with_model
  in
  let with_tools = match config.allowed_tools with
    | Some tools -> with_system @ ["--allowedTools"] @ tools
    | None -> with_system
  in
  (* Add the prompt at the end *)
  with_tools @ [prompt]

(** {1 Response Parsing} *)

(** Parse JSON response from Claude CLI *)
let parse_response json_str : (response, string) result =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in

    (* Claude CLI JSON output format *)
    let result = json |> member "result" in
    let content = result |> to_string_option |> Option.value ~default:"" in
    let model = json |> member "model" |> to_string_option in
    let cost_usd = json |> member "costUSD" |> to_float_option in
    let duration_ms = json |> member "durationMs" |> to_int_option in
    let session_id = json |> member "sessionId" |> to_string_option in

    Result.Ok {
      content;
      tool_calls = [];  (* Claude CLI handles tools internally *)
      model;
      cost_usd;
      duration_ms;
      session_id;
    }
  with
  | Yojson.Json_error msg -> Result.Error (Printf.sprintf "JSON parse error: %s" msg)
  | e -> Result.Error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))

(** Parse streaming JSON lines (for future use) *)
let parse_stream_line line : (string option, string) result =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    match json |> member "type" |> to_string_option with
    | Some "assistant" ->
      let message = json |> member "message" in
      let content = message |> member "content" |> to_string_option in
      Result.Ok content
    | Some "result" ->
      let result = json |> member "result" |> to_string_option in
      Result.Ok result
    | _ -> Result.Ok None
  with _ -> Result.Ok None

(** {1 Process Execution - Eio Version} *)

(** Execute claude CLI command and capture output (simple version without pipes) *)
let exec_command ~sw ~proc_mgr ~clock ~(config:config) ~(cmd:string list) : (string, string) result =
  let cmd_str = String.concat " " (List.map (fun s ->
      if String.contains s ' ' || String.contains s '"' then
        Printf.sprintf "'%s'" s
      else s
    ) cmd)
  in

  try
    (* Use shell to execute the command *)
    let proc = Eio.Process.spawn ~sw proc_mgr ["/bin/sh"; "-c"; cmd_str] in

    (* Wait for process to complete *)
    let wait_for_process () =
      match Eio.Process.await proc with
      | `Exited 0 ->
        (* Read output from stdout - for now return empty as we need pipes *)
        Result.Ok ""
      | `Exited code ->
        Result.Error (Printf.sprintf "Claude CLI exited with code %d" code)
      | `Signaled sig_ ->
        Result.Error (Printf.sprintf "Claude CLI killed by signal %d" sig_)
    in

    (* Apply timeout if configured *)
    match config.timeout_ms with
    | None -> wait_for_process ()
    | Some ms ->
      Eio.Fiber.first
        (fun () -> wait_for_process ())
        (fun () ->
           Eio.Time.sleep clock (float_of_int ms /. 1000.0);
           Eio.Process.signal proc Sys.sigkill;
           Result.Error "Claude CLI timeout")
  with e ->
    Result.Error (Printf.sprintf "Process error: %s" (Printexc.to_string e))

(** Execute claude CLI with proper stdout/stderr capture *)
let exec_command_with_output ~sw ~proc_mgr ~clock ~(config:config) ~(cmd:string list) : (string, string) result =
  let cmd_str = String.concat " " (List.map (fun s ->
      if String.contains s ' ' || String.contains s '"' then
        Printf.sprintf "'%s'" s
      else s
    ) cmd)
  in

  try
    (* Create pipes for stdout/stderr using Eio.Process.pipe *)
    let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw proc_mgr in

    (* Spawn process with pipes *)
    let proc = Eio.Process.spawn ~sw proc_mgr
        ~stdout:stdout_w
        ~stderr:stderr_w
        ["/bin/sh"; "-c"; cmd_str]
    in

    (* Close write ends after spawn *)
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;

    (* Read stdout in fiber *)
    let stdout_content = ref "" in
    let stderr_content = ref "" in

    Eio.Fiber.both
      (fun () ->
         stdout_content := Eio.Buf_read.(of_flow ~max_size:(10 * 1024 * 1024) stdout_r |> take_all))
      (fun () ->
         stderr_content := Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) stderr_r |> take_all));

    (* Wait for process *)
    let wait_for_process () =
      match Eio.Process.await proc with
      | `Exited 0 -> Result.Ok !stdout_content
      | `Exited code ->
        Result.Error (Printf.sprintf "Claude CLI exited with code %d: %s" code !stderr_content)
      | `Signaled sig_ ->
        Result.Error (Printf.sprintf "Claude CLI killed by signal %d" sig_)
    in

    (* Apply timeout if configured *)
    match config.timeout_ms with
    | None -> wait_for_process ()
    | Some ms ->
      Eio.Fiber.first
        (fun () -> wait_for_process ())
        (fun () ->
           Eio.Time.sleep clock (float_of_int ms /. 1000.0);
           Eio.Process.signal proc Sys.sigkill;
           Result.Error "Claude CLI timeout")
  with e ->
    Result.Error (Printf.sprintf "Process error: %s" (Printexc.to_string e))

(** {1 LLM_BACKEND Implementation} *)

let name = "claude_cli_eio"

let call ~sw ~proc_mgr ~clock ~(config:config) ~(messages:message list) ~(tools:tool list) =
  (* Build prompt from messages *)
  let prompt = messages
    |> List.filter (fun m -> m.role = User || m.role = Assistant)
    |> List.map (fun m ->
        let role_str = match m.role with
          | User -> "User"
          | Assistant -> "Assistant"
          | System -> "System"
          | Tool -> "Tool"
        in
        Printf.sprintf "[%s]: %s" role_str m.content
      )
    |> String.concat "\n\n"
  in

  (* Note: tools parameter is ignored - Claude CLI handles tools internally *)
  let _ = tools in

  let cmd = build_command ~config ~prompt in
  match exec_command_with_output ~sw ~proc_mgr ~clock ~config ~cmd with
  | Result.Error e -> Result.Error e
  | Result.Ok stdout ->
    (* Try to parse as JSON first *)
    match parse_response stdout with
    | Result.Ok resp -> Result.Ok resp
    | Result.Error _ ->
      (* If not JSON, treat raw output as content *)
      Result.Ok {
        content = String.trim stdout;
        tool_calls = [];
        model = Some config.model;
        cost_usd = None;
        duration_ms = None;
        session_id = None;
      }

let parse_tool_calls _resp = None  (* Claude CLI handles tools internally *)

let extract_content resp = resp.content

let is_final _resp = true  (* Always final - no external tool calls *)

(** {1 Utility Functions} *)

(** Check if claude CLI is available *)
let is_available ~sw ~proc_mgr () =
  try
    let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
    let proc = Eio.Process.spawn ~sw proc_mgr ~stdout:stdout_w
        ["/bin/sh"; "-c"; "which claude"]
    in
    Eio.Flow.close stdout_w;
    let output = Eio.Buf_read.(of_flow ~max_size:4096 stdout_r |> take_all) in
    let _ = Eio.Process.await proc in
    String.length (String.trim output) > 0
  with _ ->
    false

(** Get claude CLI version *)
let version ~sw ~proc_mgr () =
  try
    let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
    let proc = Eio.Process.spawn ~sw proc_mgr ~stdout:stdout_w
        ["/bin/sh"; "-c"; "claude --version"]
    in
    Eio.Flow.close stdout_w;
    let output = Eio.Buf_read.(of_flow ~max_size:4096 stdout_r |> take_all) in
    let _ = Eio.Process.await proc in
    Result.Ok (String.trim output)
  with e ->
    Result.Error (Printexc.to_string e)

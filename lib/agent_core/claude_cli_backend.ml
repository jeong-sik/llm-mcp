(** Claude CLI Backend - LLM_BACKEND implementation for Claude Code CLI

    Wraps the `claude` CLI tool with JSON output mode for programmatic access.
    Note: Tool execution is handled internally by Claude CLI, so this backend
    only returns final responses (no tool_calls).

    Usage:
    {[
      module My_Loop = Agent_loop_functor.Make(Claude_cli_backend)(My_Tools)(Default_state)

      let config = Claude_cli_backend.{
        model = "sonnet";  (* or "opus", "haiku" *)
        timeout_ms = Some 120_000;
        system_prompt = None;
        allowed_tools = None;  (* None = default tools *)
      }

      let result = My_Loop.run ~config ~backend_config:config ...
    ]}
*)

open Lwt.Syntax
open Agent_types

(** {1 Configuration} *)

type config = {
  model : string;              (** Model alias: "sonnet", "opus", "haiku" *)
  timeout_ms : int option;     (** Command timeout in ms (None = no timeout) *)
  system_prompt : string option;  (** Optional system prompt override *)
  allowed_tools : string list option;  (** Allowed tools (None = default) *)
  print_mode : bool;           (** Always true for non-interactive mode *)
}

let default_config = {
  model = "sonnet";
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

(** {1 Process Execution} *)

(** Execute claude CLI command and capture output *)
let exec_command ~(config:config) ~(cmd:string list) : (string, string) result Lwt.t =
  let cmd_str = String.concat " " (List.map (fun s ->
      if String.contains s ' ' || String.contains s '"' then
        Printf.sprintf "'%s'" s
      else s
    ) cmd)
  in

  (* Create process *)
  let* result =
    try%lwt
      let process = Lwt_process.open_process_full
          (Lwt_process.shell cmd_str) in

      (* Read stdout *)
      let* stdout = Lwt_io.read process#stdout in
      let* stderr = Lwt_io.read process#stderr in
      let* status = process#close in

      match status with
      | Unix.WEXITED 0 -> Lwt.return (Result.Ok stdout)
      | Unix.WEXITED code ->
        Lwt.return (Result.Error (Printf.sprintf "Claude CLI exited with code %d: %s" code stderr))
      | Unix.WSIGNALED sig_ ->
        Lwt.return (Result.Error (Printf.sprintf "Claude CLI killed by signal %d" sig_))
      | Unix.WSTOPPED sig_ ->
        Lwt.return (Result.Error (Printf.sprintf "Claude CLI stopped by signal %d" sig_))
    with e ->
      Lwt.return (Result.Error (Printf.sprintf "Process error: %s" (Printexc.to_string e)))
  in

  (* Apply timeout if configured *)
  match config.timeout_ms with
  | None -> Lwt.return result
  | Some ms ->
    let timeout = Lwt_unix.sleep (float_of_int ms /. 1000.0) in
    Lwt.pick [
      (let* r = Lwt.return result in Lwt.return r);
      (let* () = timeout in Lwt.return (Result.Error "Claude CLI timeout"))
    ]

(** {1 LLM_BACKEND Implementation} *)

let name = "claude_cli"

let call ~(config:config) ~(messages:message list) ~(tools:tool list) =
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
  let* result = exec_command ~config ~cmd in

  match result with
  | Result.Error e -> Lwt.return (Result.Error e)
  | Result.Ok stdout ->
    (* Try to parse as JSON first *)
    match parse_response stdout with
    | Result.Ok resp -> Lwt.return (Result.Ok resp)
    | Result.Error _ ->
      (* If not JSON, treat raw output as content *)
      Lwt.return (Result.Ok {
        content = String.trim stdout;
        tool_calls = [];
        model = Some config.model;
        cost_usd = None;
        duration_ms = None;
        session_id = None;
      })

let parse_tool_calls _resp = None  (* Claude CLI handles tools internally *)

let extract_content resp = resp.content

let is_final _resp = true  (* Always final - no external tool calls *)

(** {1 Utility Functions} *)

(** Check if claude CLI is available *)
let is_available () =
  try%lwt
    let process = Lwt_process.open_process_in
        (Lwt_process.shell "which claude") in
    let* output = Lwt_io.read process#stdout in
    let* _ = process#close in
    Lwt.return (String.length (String.trim output) > 0)
  with _ ->
    Lwt.return false

(** Get claude CLI version *)
let version () =
  try%lwt
    let process = Lwt_process.open_process_in
        (Lwt_process.shell "claude --version") in
    let* output = Lwt_io.read process#stdout in
    let* _ = process#close in
    Lwt.return (Result.Ok (String.trim output))
  with e ->
    Lwt.return (Result.Error (Printexc.to_string e))

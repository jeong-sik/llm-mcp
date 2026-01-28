(** CLI command execution with Eio - Direct-Style Implementation

    Pure Eio subprocess execution for llm-mcp.
    Replaces Lwt_process with Eio.Process.

    Key differences from Lwt version:
    - Direct-style (no monadic bind)
    - Structured concurrency with Eio.Switch
    - Fiber-based parallelism for stdout/stderr
    - Timeout via Eio.Time with fiber cancellation
*)

(** {1 Types} *)

type run_result = {
  stdout : string;
  stderr : string;
  exit_code : int;
}

type run_error =
  | Timeout of int
  | ProcessError of string

(** {1 Helper Functions} *)

(** Read all content from a flow *)
let read_all flow =
  Eio.Buf_read.(of_flow ~max_size:max_int flow |> take_all)

(** Get output preferring stdout, falling back to stderr *)
let get_output result =
  let stdout = String.trim result.stdout in
  let stderr = String.trim result.stderr in
  if String.length stdout > 0 then stdout else stderr

(** {1 Core Runner Functions} *)

(** Run a CLI command with timeout using Eio.Process

    Uses structured concurrency:
    - Spawns process in switch scope
    - Reads stdout/stderr in parallel fibers
    - Timeout via Eio.Time.with_timeout
*)
let run_command ~sw ~proc_mgr ~clock ?cwd ?(safe_tmpdir = false) ~timeout cmd args =
  (* Build command list *)
  let cmd_list = cmd :: args in

  (* Safe TMPDIR to avoid FSWatcher socket file issues *)
  let safe_tmp = "/tmp/claude-safe" in
  let () = if safe_tmpdir then
    try Unix.mkdir safe_tmp 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  in
  let env = if safe_tmpdir then
    let base_env = Unix.environment () in
    let filtered = Array.to_list base_env
      |> List.filter (fun s -> not (String.length s >= 7 && String.sub s 0 7 = "TMPDIR="))
    in
    Some (Array.of_list (("TMPDIR=" ^ safe_tmp) :: filtered))
  else None
  in

  (* Run with timeout *)
  let timeout_duration = Float.of_int timeout in

  try
    let result = Eio.Time.with_timeout clock timeout_duration (fun () ->
      (* Create pipes for stdout/stderr *)
      let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
      let stderr_r, stderr_w = Eio.Process.pipe ~sw proc_mgr in

      (* Spawn process with pipes
         Note: cwd is now an Eio.Path.t, pre-constructed by caller *)
      let proc = Eio.Process.spawn ~sw proc_mgr
        ?cwd
        ~stdout:stdout_w
        ~stderr:stderr_w
        ?env
        cmd_list
      in

      (* Close write ends after spawn (parent doesn't write) *)
      Eio.Flow.close stdout_w;
      Eio.Flow.close stderr_w;

      (* Read stdout and stderr in parallel *)
      let stdout_content = ref "" in
      let stderr_content = ref "" in

      Eio.Fiber.both
        (fun () -> stdout_content := read_all stdout_r)
        (fun () -> stderr_content := read_all stderr_r);

      (* Close read ends after reading *)
      Eio.Flow.close stdout_r;
      Eio.Flow.close stderr_r;

      (* Wait for process to complete *)
      let status = Eio.Process.await proc in
      let exit_code = match status with
        | `Exited n -> n
        | `Signaled _ -> -1
      in

      Ok { stdout = !stdout_content; stderr = !stderr_content; exit_code }
    ) in
    match result with
    | Ok r -> Ok r
    | Error `Timeout -> Error (Timeout timeout)
  with exn ->
    Error (ProcessError (Printexc.to_string exn))

(** Run a streaming command, calling on_line for each stdout line.
    Used for ollama streaming API responses.
*)
let run_streaming_command ~sw ~proc_mgr ~clock ~timeout ~on_line cmd args =
  let cmd_list = cmd :: args in
  let timeout_duration = Float.of_int timeout in

  try
    let result = Eio.Time.with_timeout clock timeout_duration (fun () ->
      let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in

      let proc = Eio.Process.spawn ~sw proc_mgr
        ~stdout:stdout_w
        cmd_list
      in

      Eio.Flow.close stdout_w;

      (* Read lines and call callback *)
      let reader = Eio.Buf_read.of_flow ~max_size:max_int stdout_r in
      let rec read_lines () =
        match Eio.Buf_read.line reader with
        | line ->
            on_line line;
            read_lines ()
        | exception End_of_file -> ()
      in
      read_lines ();

      (* Close read end after reading *)
      Eio.Flow.close stdout_r;

      let status = Eio.Process.await proc in
      let exit_code = match status with
        | `Exited n -> n
        | `Signaled _ -> -1
      in
      Ok exit_code
    ) in
    match result with
    | Ok r -> Ok r
    | Error `Timeout -> Error (Timeout timeout)
  with exn ->
    Error (ProcessError (Printexc.to_string exn))

(** Run a command with stdin input (shell injection-safe).
    Spawns process with argv array directly, writes to stdin, reads stdout/stderr.
    Used for MCP stdio servers that need JSON-RPC input.
*)
let run_command_with_stdin ~sw ~proc_mgr ~clock ~timeout ~stdin_data cmd args =
  let cmd_list = cmd :: args in
  let timeout_duration = Float.of_int timeout in

  try
    let result = Eio.Time.with_timeout clock timeout_duration (fun () ->
      let stdin_r, stdin_w = Eio.Process.pipe ~sw proc_mgr in
      let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
      let stderr_r, stderr_w = Eio.Process.pipe ~sw proc_mgr in

      let proc = Eio.Process.spawn ~sw proc_mgr
        ~stdin:stdin_r
        ~stdout:stdout_w
        ~stderr:stderr_w
        cmd_list
      in

      (* Close parent ends *)
      Eio.Flow.close stdin_r;
      Eio.Flow.close stdout_w;
      Eio.Flow.close stderr_w;

      (* Write stdin data *)
      (try
        Eio.Flow.copy_string stdin_data stdin_w;
        Eio.Flow.close stdin_w
      with _ ->
        Eio.Flow.close stdin_w);

      (* Read stdout and stderr in parallel *)
      let stdout_content = ref "" in
      let stderr_content = ref "" in

      Eio.Fiber.both
        (fun () -> stdout_content := read_all stdout_r)
        (fun () -> stderr_content := read_all stderr_r);

      (* Close read ends after reading *)
      Eio.Flow.close stdout_r;
      Eio.Flow.close stderr_r;

      let status = Eio.Process.await proc in
      let exit_code = match status with
        | `Exited n -> n
        | `Signaled n -> -(128 + n)
      in

      Ok { stdout = !stdout_content; stderr = !stderr_content; exit_code }
    ) in
    match result with
    | Ok r -> Ok r
    | Error `Timeout -> Error (Timeout timeout)
  with exn ->
    Error (ProcessError (Printexc.to_string exn))

(** {1 Convenience Wrappers}

    These hide the Eio context threading for simpler usage.
    Requires running inside Eio.Switch.run scope.
*)

(** Run command with env (extracts proc_mgr and clock from env) *)
let run_with_env ~sw ~env ?cwd ?safe_tmpdir ~timeout cmd args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  run_command ~sw ~proc_mgr ~clock ?cwd ?safe_tmpdir ~timeout cmd args

(** Run streaming command with env *)
let run_streaming_with_env ~sw ~env ~timeout ~on_line cmd args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  run_streaming_command ~sw ~proc_mgr ~clock ~timeout ~on_line cmd args

(** Run command with stdin using env *)
let run_with_stdin_env ~sw ~env ~timeout ~stdin_data cmd args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  run_command_with_stdin ~sw ~proc_mgr ~clock ~timeout ~stdin_data cmd args

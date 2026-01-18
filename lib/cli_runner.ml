(** CLI command execution with Lwt *)

type run_result = {
  stdout : string;
  stderr : string;
  exit_code : int;
}

type run_error =
  | Timeout of int
  | ProcessError of string

(** Run a CLI command with timeout using Lwt_process *)
let run_command ?cwd ?(safe_tmpdir : bool = false) ~timeout cmd args =
  let open Lwt.Syntax in

  (* Build command *)
  let full_cmd = Array.of_list (cmd :: args) in
  let shell_cmd = ("", full_cmd) in

  (* Safe TMPDIR to avoid FSWatcher socket file issues *)
  let safe_tmp = "/tmp/claude-safe" in
  let () = if safe_tmpdir then
    try Unix.mkdir safe_tmp 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  in
  let env = if safe_tmpdir then
    let base_env = Unix.environment () in
    let filtered = Array.to_list base_env
      |> List.filter (fun s -> not (String.sub s 0 (min 7 (String.length s)) = "TMPDIR="))
    in
    Some (Array.of_list (("TMPDIR=" ^ safe_tmp) :: filtered))
  else None
  in

  (* Create process with optional cwd and env *)
  let process =
    match cwd, env with
    | Some dir, Some e ->
        Lwt_process.open_process_full ~cwd:dir ~env:e shell_cmd
    | Some dir, None ->
        Lwt_process.open_process_full ~cwd:dir shell_cmd
    | None, Some e ->
        Lwt_process.open_process_full ~env:e shell_cmd
    | None, None ->
        Lwt_process.open_process_full shell_cmd
  in

  (* Read stdout and stderr concurrently *)
  let read_all channel =
    let buf = Buffer.create 4096 in
    let rec loop () =
      let* line_opt =
        Lwt.catch
          (fun () ->
          let+ line = Lwt_io.read_line channel in
          Some line)
          (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)
      in
      match line_opt with
      | Some line ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
      | None -> Lwt.return (Buffer.contents buf)
    in
    loop ()
  in

  (* Run with timeout *)
  let run_process () =
    (* Read stdout and stderr CONCURRENTLY to avoid deadlock *)
    let* (stdout_content, stderr_content) =
      Lwt.both (read_all process#stdout) (read_all process#stderr)
    in
    let* status = process#status in
    let exit_code = match status with
      | Unix.WEXITED n -> n
      | Unix.WSIGNALED _ -> -1
      | Unix.WSTOPPED _ -> -1
    in
    Lwt.return (Ok { stdout = stdout_content; stderr = stderr_content; exit_code })
  in

  let timeout_thread =
    let* () = Lwt_unix.sleep (Float.of_int timeout) in
    process#kill Sys.sigkill;
    (* Wait for process to be reaped - prevents zombie *)
    let* _ = process#status in
    Lwt.return (Error (Timeout timeout))
  in

  Lwt.catch
    (fun () -> Lwt.pick [run_process (); timeout_thread])
    (fun exn -> Lwt.return (Error (ProcessError (Printexc.to_string exn))))

(** Run a streaming command, calling on_line for each stdout line.
    Used for ollama streaming API responses. *)
let run_streaming_command ~timeout ~on_line cmd args =
  let open Lwt.Syntax in

  let full_cmd = Array.of_list (cmd :: args) in
  let shell_cmd = ("", full_cmd) in

  let process = Lwt_process.open_process_full shell_cmd in

  (* Read and process lines as they arrive *)
  let rec read_lines () =
    let* line_opt =
      Lwt.catch
        (fun () ->
          let+ line = Lwt_io.read_line process#stdout in
          Some line)
        (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)
    in
    match line_opt with
    | Some line ->
        let* () = on_line line in
        read_lines ()
    | None -> Lwt.return_unit
  in

  let run_process () =
    let* () = read_lines () in
    let* status = process#status in
    let exit_code = match status with
      | Unix.WEXITED n -> n
      | Unix.WSIGNALED _ -> -1
      | Unix.WSTOPPED _ -> -1
    in
    Lwt.return (Ok exit_code)
  in

  let timeout_thread =
    let* () = Lwt_unix.sleep (Float.of_int timeout) in
    process#kill Sys.sigkill;
    let* _ = process#status in
    Lwt.return (Error (Timeout timeout))
  in

  Lwt.catch
    (fun () -> Lwt.pick [run_process (); timeout_thread])
    (fun exn -> Lwt.return (Error (ProcessError (Printexc.to_string exn))))

(** Run a command with stdin input (shell injection-safe).
    Spawns process with argv array directly, writes to stdin, reads stdout/stderr.
    Used for MCP stdio servers that need JSON-RPC input. *)
let run_command_with_stdin ~timeout ~stdin_data cmd args =
  let open Lwt.Syntax in

  (* Build command as argv array - NO SHELL INVOLVED *)
  let argv = Array.of_list (cmd :: args) in
  let exec_spec = ("", argv) in  (* Lwt_process format: ("", argv) = direct exec *)

  let process = Lwt_process.open_process_full exec_spec in

  (* Read all from channel *)
  let read_all channel =
    let buf = Buffer.create 4096 in
    let rec loop () =
      let* line_opt =
        Lwt.catch
          (fun () ->
            let+ line = Lwt_io.read_line channel in
            Some line)
          (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)
      in
      match line_opt with
      | Some line ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
      | None -> Lwt.return (Buffer.contents buf)
    in
    loop ()
  in

  let run_process () =
    (* Write to stdin then close it - handle EPIPE if process exits early *)
    let* () =
      Lwt.catch
        (fun () ->
          let* () = Lwt_io.write process#stdin stdin_data in
          Lwt_io.close process#stdin)
        (fun _exn -> Lwt_io.close process#stdin)  (* Close even on EPIPE *)
    in

    (* Read stdout and stderr CONCURRENTLY to avoid deadlock *)
    let* (stdout_content, stderr_content) =
      Lwt.both (read_all process#stdout) (read_all process#stderr)
    in

    (* Explicitly close channels to release file descriptors *)
    let* () = Lwt_io.close process#stdout in
    let* () = Lwt_io.close process#stderr in

    let* status = process#status in
    let exit_code = match status with
      | Unix.WEXITED n -> n
      | Unix.WSIGNALED n -> -(128 + n)  (* Signal exit code convention *)
      | Unix.WSTOPPED _ -> -1
    in
    Lwt.return (Ok { stdout = stdout_content; stderr = stderr_content; exit_code })
  in

  let timeout_thread =
    let* () = Lwt_unix.sleep (Float.of_int timeout) in
    process#kill Sys.sigkill;
    let* _ = process#status in
    Lwt.return (Error (Timeout timeout))
  in

  Lwt.catch
    (fun () -> Lwt.pick [run_process (); timeout_thread])
    (fun exn -> Lwt.return (Error (ProcessError (Printexc.to_string exn))))

(** Get output preferring stdout, falling back to stderr *)
let get_output result =
  let stdout = String.trim result.stdout in
  let stderr = String.trim result.stderr in
  if String.length stdout > 0 then stdout else stderr

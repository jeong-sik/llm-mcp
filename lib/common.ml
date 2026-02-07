(** Common utilities for OCaml binaries

    Shared functions used across multiple binaries to avoid code duplication.
    Following DRY principle for:
    - Environment variable handling
    - Directory operations
    - Timestamp formatting
    - File I/O helpers (with proper resource cleanup)
*)

(** Get ME_ROOT environment variable, defaulting to ~/me *)
let me_root =
  try Sys.getenv "ME_ROOT"
  with Not_found -> Filename.concat (Sys.getenv "HOME") "me"

(** Ensure directory exists, creating parent directories as needed *)
let rec ensure_dir path =
  if not (Sys.file_exists path) then begin
    ensure_dir (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error _ -> ()
  end

(** Best-effort recursive deletion (rm -rf). *)
let rec rm_rf path =
  if Sys.file_exists path then
    try
      if Sys.is_directory path then begin
        Sys.readdir path
        |> Array.iter (fun name -> rm_rf (Filename.concat path name));
        Unix.rmdir path
      end else
        Sys.remove path
    with
    | Sys_error _ | Unix.Unix_error _ -> ()

(** Locate an executable in PATH (like `which`, but without spawning a shell). *)
let which (prog : string) : string option =
  let is_executable p =
    try
      Unix.access p [Unix.X_OK];
      true
    with Unix.Unix_error _ -> false
  in
  if not (Filename.is_relative prog) then
    if Sys.file_exists prog && is_executable prog then Some prog else None
  else
    let path =
      Sys.getenv_opt "PATH"
      |> Option.value ~default:""
      |> String.split_on_char ':'
    in
    let rec loop = function
      | [] -> None
      | dir :: rest ->
          let cand = Filename.concat dir prog in
          if Sys.file_exists cand && is_executable cand then Some cand else loop rest
    in
    loop path

let command_exists (prog : string) : bool =
  Option.is_some (which prog)

(** Get current timestamp as integer *)
let timestamp () = int_of_float (Unix.time ())

(** Format current time as YYYY-MM-DD HH:MM:SS *)
let time_str () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(** Format current date as YYYY-MM-DD *)
let date_str () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

(** Build path relative to ME_ROOT *)
let me_path parts =
  List.fold_left Filename.concat me_root parts

(** Safe file read - returns None on error.
    Uses In_channel.with_open_text for automatic resource cleanup. *)
let read_file_opt path =
  try
    Some (In_channel.with_open_text path In_channel.input_all)
  with Sys_error _ -> None

(** Safe file write - returns false on error.
    Uses Out_channel.with_open_text for automatic resource cleanup. *)
let write_file path content =
  try
    Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc content
    );
    true
  with Sys_error _ -> false

(** Read JSON file *)
let read_json_opt path =
  match read_file_opt path with
  | None -> None
  | Some s ->
      try Some (Yojson.Safe.from_string s)
      with Yojson.Json_error _ -> None

(** Write JSON file with pretty formatting *)
let write_json path json =
  write_file path (Yojson.Safe.pretty_to_string json)

(** Take first n items from list *)
let take n lst =
  let rec aux acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] n lst

(** Check if string contains substring *)
let contains ~substring s =
  try
    let _ = Str.search_forward (Str.regexp_string substring) s 0 in
    true
  with Not_found -> false

(** Safe get from association list *)
let assoc_opt key lst =
  try Some (List.assoc key lst)
  with Not_found -> None

(** Print emoji message (common output pattern) *)
let emoji_print emoji tag msg =
  Printf.printf "%s [%s] %s\n" emoji tag msg

(** Print success message *)
let print_success msg = emoji_print "✅" "SUCCESS" msg

(** Print error message *)
let print_error msg = emoji_print "❌" "ERROR" msg

(** Print info message *)
let print_info tag msg = emoji_print "💡" tag msg

(** Print warning message *)
let print_warning msg = emoji_print "⚠️" "WARNING" msg

(* ============================================ *)
(* Finalizer Guard                              *)
(* ============================================ *)

let env_true name =
  match Sys.getenv_opt name with
  | None -> false
  | Some v ->
      let v = String.trim v |> String.lowercase_ascii in
      v = "1" || v = "true" || v = "yes" || v = "on"

let strict_finalizers () = env_true "LLM_MCP_STRICT_FINALIZERS"

let handle_finalizer_error ~module_name ~label ~during_exception ~backtrace ex =
  let suffix = if during_exception then " (during exception)" else "" in
  Log.warn module_name "%s failed in finalizer%s: %s" label suffix
    (Printexc.to_string ex);
  if (not during_exception) && strict_finalizers () then
    Printexc.raise_with_backtrace ex backtrace

let protect ~module_name ~finally_label ~finally f =
  match f () with
  | v ->
      (try finally () with
       | ex ->
           let bt = Printexc.get_raw_backtrace () in
           handle_finalizer_error ~module_name ~label:finally_label
             ~during_exception:false ~backtrace:bt ex);
      v
  | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      (try finally () with
       | ex2 ->
           let bt2 = Printexc.get_raw_backtrace () in
           handle_finalizer_error ~module_name ~label:finally_label
             ~during_exception:true ~backtrace:bt2 ex2);
      Printexc.raise_with_backtrace ex bt

(* ============================================ *)
(* File I/O Extended                            *)
(* ============================================ *)

(** Read file as list of lines.
    Uses In_channel.with_open_text for automatic resource cleanup. *)
let read_lines path =
  try
    In_channel.with_open_text path (fun ic ->
      let lines = ref [] in
      begin
        try
          while true do
            lines := In_channel.input_line ic :: !lines
          done
        with End_of_file -> ()
      end;
      List.filter_map Fun.id !lines |> List.rev
    )
  with Sys_error _ -> []

(** Read the tail of a file as list of lines with safety limits.
    - max_bytes: read at most this many bytes from the end of the file
    - max_lines: keep only the last N lines in memory
    Returns [] on non-regular files or errors. *)
let read_lines_tail ~max_bytes ~max_lines path =
  if max_bytes <= 0 || max_lines <= 0 then []
  else
    try
      let st = Unix.stat path in
      if st.Unix.st_kind <> Unix.S_REG then []
      else
        In_channel.with_open_text path (fun ic ->
          let size = st.Unix.st_size in
          let start = if size > max_bytes then size - max_bytes else 0 in
          if start > 0 then begin
            In_channel.seek ic (Int64.of_int start);
            (* Drop partial line to align to next full line. *)
            ignore (In_channel.input_line ic)
          end;
          let q = Queue.create () in
          begin
            try
              while true do
                match In_channel.input_line ic with
                | Some line ->
                    Queue.add line q;
                    if Queue.length q > max_lines then ignore (Queue.take q)
                | None -> raise End_of_file
              done
            with End_of_file -> ()
          end;
          let lines = ref [] in
          Queue.iter (fun l -> lines := l :: !lines) q;
          List.rev !lines
        )
    with Sys_error _ | Unix.Unix_error _ -> []

(** List files with given suffix in directory *)
let list_files_with_suffix dir suffix =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    Array.to_list (Sys.readdir dir)
    |> List.filter (fun f -> Filename.check_suffix f suffix)
    |> List.map (fun f -> Filename.concat dir f)

(* ============================================ *)
(* Timestamp Extended                           *)
(* ============================================ *)

(** Get current ISO 8601 timestamp (YYYY-MM-DDTHH:MM:SS) *)
let iso_timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** Get date N days ago as YYYY-MM-DD *)
let date_n_days_ago n =
  let time = Unix.time () -. (float_of_int n *. 86400.0) in
  let tm = Unix.localtime time in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

(** Get current month directory name (YYYY-MM) *)
let month_dir () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)

(** Get current day as DD string *)
let day_str () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d" tm.Unix.tm_mday

(* ============================================ *)
(* Subprocess Execution (argv only)             *)
(* ============================================ *)

module Subprocess = struct
  type stderr_mode = [ `Capture | `Dev_null | `Stdout ]

  type result = {
    stdout : string;
    stderr : string;
    status : Unix.process_status;
    timed_out : bool;
  }

  let env_with_overrides (overrides : (string * string) list) : string array =
    let tbl = Hashtbl.create 32 in
    Array.iter
      (fun kv ->
        match String.index_opt kv '=' with
        | None -> ()
        | Some i ->
            let k = String.sub kv 0 i in
            let v = String.sub kv (i + 1) (String.length kv - i - 1) in
            Hashtbl.replace tbl k v)
      (Unix.environment ());
    List.iter (fun (k, v) -> Hashtbl.replace tbl k v) overrides;
    Hashtbl.to_seq tbl |> Seq.map (fun (k, v) -> k ^ "=" ^ v) |> Array.of_seq

  let write_all fd (s : string) =
    let len = String.length s in
    let rec loop off =
      if off >= len then ()
      else
        match Unix.write_substring fd s off (len - off) with
        | 0 -> ()
        | n -> loop (off + n)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop off
    in
    loop 0

  let run_capture
      ?stdin
      ?(env_overrides = [])
      ?timeout_s
      ?(stderr = `Capture)
      (prog : string)
      (args : string list)
    : result
    =
    let argv = Array.of_list (prog :: args) in
    let env = env_with_overrides env_overrides in

    let stdout_r, stdout_w = Unix.pipe () in

    let stderr_pipe =
      match stderr with
      | `Capture ->
          let r, w = Unix.pipe () in
          Some (r, w)
      | `Stdout | `Dev_null -> None
    in

    let stdin_fd, stdin_write_opt =
      match stdin with
      | None ->
          (Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o644, None)
      | Some s ->
          let r, w = Unix.pipe () in
          (r, Some (w, s))
    in

    let stderr_fd, stderr_r_opt, close_stderr_w_opt, close_devnull_out_opt =
      match stderr with
      | `Capture ->
          let r, w = Option.get stderr_pipe in
          (w, Some r, Some w, None)
      | `Stdout -> (stdout_w, None, None, None)
      | `Dev_null ->
          let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o644 in
          (devnull, None, None, Some devnull)
    in

    let pid =
      Unix.create_process_env prog argv env stdin_fd stdout_w stderr_fd
    in

    (* Parent closes child-side FDs. *)
    Unix.close stdout_w;
    (match close_stderr_w_opt with Some fd -> Unix.close fd | None -> ());
    Unix.close stdin_fd;
    (match close_devnull_out_opt with Some fd -> Unix.close fd | None -> ());

    (* Write stdin, if provided, then close the write end. *)
    (match stdin_write_opt with
     | None -> ()
     | Some (w, s) ->
         Fun.protect
           ~finally:(fun () -> (try Unix.close w with _ -> ()))
           (fun () -> write_all w s));

    Unix.set_nonblock stdout_r;
    Option.iter Unix.set_nonblock stderr_r_opt;

    let stdout_buf = Buffer.create 4096 in
    let stderr_buf = Buffer.create 1024 in
    let tmp = Bytes.create 8192 in

    let status_ref : Unix.process_status option ref = ref None in
    let timed_out_ref = ref false in
    let start = Unix.gettimeofday () in

    let set_status st =
      match !status_ref with Some _ -> () | None -> status_ref := Some st
    in

    let check_status_nohang () =
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ -> ()
      | _, st -> set_status st
    in

    let terminate () =
      if not !timed_out_ref then begin
        timed_out_ref := true;
        (try Unix.kill pid Sys.sigterm with _ -> ());
        for _i = 1 to 10 do
          check_status_nohang ();
          if !status_ref = None then Unix.sleepf 0.05
        done;
        if !status_ref = None then (try Unix.kill pid Sys.sigkill with _ -> ())
      end
    in

    let rec drain_fd fd buf =
      match Unix.read fd tmp 0 (Bytes.length tmp) with
      | 0 -> `Eof
      | n ->
          Buffer.add_subbytes buf tmp 0 n;
          `More
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain_fd fd buf
      | exception Unix.Unix_error (Unix.EAGAIN, _, _)
      | exception Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> `Wait
    in

    let rec loop open_fds =
      if open_fds = [] then ()
      else begin
        (match timeout_s with
         | None -> ()
         | Some t ->
             let elapsed = Unix.gettimeofday () -. start in
             if elapsed >= t then terminate ());

        check_status_nohang ();

        let select_timeout =
          match timeout_s with
          | None -> 0.2
          | Some t ->
              let elapsed = Unix.gettimeofday () -. start in
              let remaining = t -. elapsed in
              if remaining <= 0.0 then 0.0 else min 0.2 remaining
        in
        let readable, _, _ = Unix.select open_fds [] [] select_timeout in

        let still_open = ref open_fds in
        let close_fd fd =
          (try Unix.close fd with _ -> ());
          still_open := List.filter (fun x -> x <> fd) !still_open
        in

        List.iter
          (fun fd ->
            let target_buf = if fd = stdout_r then stdout_buf else stderr_buf in
            let rec drain_loop () =
              match drain_fd fd target_buf with
              | `More -> drain_loop ()
              | `Wait -> ()
              | `Eof -> close_fd fd
            in
            drain_loop ())
          readable;

        loop !still_open
      end
    in

    Fun.protect
      ~finally:(fun () ->
        (try Unix.close stdout_r with _ -> ());
        Option.iter (fun fd -> try Unix.close fd with _ -> ()) stderr_r_opt)
      (fun () ->
        let open_fds =
          stdout_r :: (match stderr_r_opt with Some fd -> [ fd ] | None -> [])
        in
        loop open_fds);

    (match !status_ref with
     | Some _ -> ()
     | None ->
         let _, st = Unix.waitpid [] pid in
         set_status st);

    {
      stdout = Buffer.contents stdout_buf;
      stderr = (match stderr with `Capture -> Buffer.contents stderr_buf | _ -> "");
      status = Option.get !status_ref;
      timed_out = !timed_out_ref;
    }

  let stdout_lines (s : string) : string list =
    String.split_on_char '\n' s
    |> List.map String.trim
    |> List.filter (fun l -> l <> "")

  let run_stdout_lines ?timeout_s ?(stderr = `Dev_null) prog args =
    let res = run_capture ?timeout_s ~stderr prog args in
    match res.status with
    | Unix.WEXITED 0 -> stdout_lines res.stdout
    | _ -> []
end

(** Run external command (argv only) and return stdout lines on success. *)
let run_command ?timeout_s ?(stderr = `Dev_null) prog args =
  try Subprocess.run_stdout_lines ?timeout_s ~stderr prog args
  with _ -> []

(** Run external command (argv only) and return stdout as a string on success. *)
let run_cmd ?timeout_s ?(stderr = `Dev_null) prog args =
  String.concat "\n" (run_command ?timeout_s ~stderr prog args)

(** Escape special characters for JSON string *)
let json_escape s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Run git command in specified directory (argv only). *)
let run_git_command repo_path args =
  run_command ~stderr:`Dev_null "git" ("-C" :: repo_path :: args)

(* ============================================ *)
(* Output Formatting                            *)
(* ============================================ *)

(** Format context/warnings as human-readable output *)
let format_result_output result =
  let open Yojson.Safe.Util in
  let buf = Buffer.create 256 in

  (* Context section *)
  let context =
    try result |> member "context" |> to_list |> List.map to_string
    with Yojson.Safe.Util.Type_error _ -> []
  in
  if List.length context > 0 then begin
    Buffer.add_string buf "📋 Context:\n";
    List.iter (fun msg ->
      Buffer.add_string buf (Printf.sprintf "   %s\n" msg)
    ) context;
    Buffer.add_char buf '\n'
  end;

  (* Warnings section *)
  let warnings =
    try result |> member "warnings" |> to_list |> List.map to_string
    with Yojson.Safe.Util.Type_error _ -> []
  in
  if List.length warnings > 0 then begin
    Buffer.add_string buf "⚠️  Warnings:\n";
    List.iter (fun msg ->
      Buffer.add_string buf (Printf.sprintf "   %s\n" msg)
    ) warnings;
    Buffer.add_char buf '\n'
  end;

  Buffer.contents buf

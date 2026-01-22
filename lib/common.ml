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
(* Shell Command Execution                      *)
(* ============================================ *)

(** Run shell command and return output lines.
    Uses Fun.protect for proper process cleanup on exception. *)
let run_command cmd =
  try
    let ic = Unix.open_process_in cmd in
    Fun.protect ~finally:(fun () -> ignore (Unix.close_process_in ic)) (fun () ->
      let lines = ref [] in
      begin
        try
          while true do
            lines := input_line ic :: !lines
          done
        with End_of_file -> ()
      end;
      List.rev !lines
    )
  with Unix.Unix_error _ | Sys_error _ -> []

(** Run command and return output as string *)
let run_cmd cmd =
  String.concat "\n" (run_command cmd)

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

(** Run git command in specified directory *)
let run_git_command repo_path args =
  let cmd = String.concat " " ("git" :: args) in
  let full_cmd = Printf.sprintf "cd %s && %s 2>/dev/null" (Filename.quote repo_path) cmd in
  run_command full_cmd

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

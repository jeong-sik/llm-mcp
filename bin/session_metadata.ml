(** session_metadata - Session Metadata Collector

    OCaml port of features/hooks/lib/metadata.py
    Collects session metadata including timestamps, duration, git commits, modified files.

    Usage:
      echo '{"session_id": "xxx"}' | session_metadata
      session_metadata  # reads from stdin or uses defaults
*)

(** Read session start time from state file *)
let read_session_start_time state_dir =
  let start_time_file = Filename.concat state_dir "session-start-time" in
  let start_timestamp_file = Filename.concat state_dir "session-start-timestamp" in

  if not (Sys.file_exists start_time_file) then
    `Assoc [
      ("start_time", `Null);
      ("start_timestamp", `Null);
      ("error", `String "session-start-time file not found")
    ]
  else
    try
      let ic = open_in start_time_file in
      let start_time = int_of_string (String.trim (input_line ic)) in
      close_in ic;

      let start_timestamp =
        if Sys.file_exists start_timestamp_file then begin
          let ic2 = open_in start_timestamp_file in
          let ts = String.trim (input_line ic2) in
          close_in ic2;
          ts
        end else "Unknown"
      in
      `Assoc [
        ("start_time", `Int start_time);
        ("start_timestamp", `String start_timestamp);
        ("error", `Null)
      ]
    with e ->
      `Assoc [
        ("start_time", `Null);
        ("start_timestamp", `Null);
        ("error", `String (Printexc.to_string e))
      ]

(* Use Common module for git commands and timestamps *)
let run_git_command = Common.run_git_command
let iso_timestamp = Common.iso_timestamp

(** Get Git commits since given timestamp *)
let get_git_commits repo_path since_timestamp =
  let args = ["log"; Printf.sprintf "--since=@%d" since_timestamp; "--format=%h %s"] in
  run_git_command repo_path args

(** Get modified files compared to previous commit *)
let get_modified_files repo_path =
  let args = ["diff"; "--name-only"; "HEAD@{1}"; "HEAD"] in
  run_git_command repo_path args

(** Collect session metadata *)
let collect_metadata session_input =
  let me_root = Common.me_root in
  let state_dir = Filename.concat (Filename.concat me_root ".claude") "state" in

  (* Current time *)
  let end_time = int_of_float (Unix.time ()) in
  let end_timestamp = iso_timestamp () in

  (* Read session start *)
  let start_info = read_session_start_time state_dir in
  let open Yojson.Safe.Util in

  let start_time_opt =
    try Some (start_info |> member "start_time" |> to_int)
    with Type_error _ -> None
  in

  let start_timestamp =
    try start_info |> member "start_timestamp" |> to_string
    with Type_error _ -> "Unknown"
  in

  (* Calculate duration *)
  let duration_min = match start_time_opt with
    | Some start_time -> (end_time - start_time) / 60
    | None -> 0
  in

  (* Git info *)
  let commits, modified_files = match start_time_opt with
    | Some start_time ->
        (get_git_commits me_root start_time, get_modified_files me_root)
    | None -> ([], [])
  in

  (* Extract session_id from input if available *)
  let session_id =
    if String.length session_input > 0 then
      try
        let json = Yojson.Safe.from_string session_input in
        Some (json |> member "session_id" |> to_string)
      with Yojson.Json_error _ | Type_error _ -> None
    else None
  in

  (* Build metadata *)
  let base_metadata = [
    ("session_start", `String start_timestamp);
    ("session_end", `String end_timestamp);
    ("duration_minutes", `Int duration_min);
    ("git_commits", `List (List.map (fun s -> `String s) commits));
    ("git_commit_count", `Int (List.length commits));
    ("modified_files", `List (List.map (fun s -> `String s) modified_files));
    ("modified_file_count", `Int (List.length modified_files));
    ("collected_at", `String end_timestamp);
  ] in

  let metadata = match session_id with
    | Some id -> ("session_id", `String id) :: base_metadata
    | None -> base_metadata
  in

  `Assoc metadata

(** Save metadata to JSON file *)
let save_metadata metadata output_path =
  try
    (* Create parent directories *)
    let parent = Filename.dirname output_path in
    Common.ensure_dir parent;

    let oc = open_out output_path in
    output_string oc (Yojson.Safe.pretty_to_string metadata);
    output_char oc '\n';
    close_out oc;
    true
  with Sys_error _ -> false

(** Main function *)
let main () =
  (* Read session input from stdin *)
  let session_input =
    if Unix.isatty Unix.stdin then ""
    else begin
      let buf = Buffer.create 256 in
      try
        while true do
          Buffer.add_string buf (input_line stdin);
          Buffer.add_char buf '\n'
        done;
        Buffer.contents buf
      with End_of_file -> String.trim (Buffer.contents buf)
    end
  in

  (* Collect metadata *)
  let metadata = collect_metadata session_input in

  (* Determine output path *)
  let me_root = Common.me_root in
  let month_dir = Common.month_dir () in
  let day = Common.day_str () in

  let output_path = Filename.concat
    (Filename.concat (Filename.concat me_root "claude") month_dir)
    (day ^ "-metadata.json")
  in

  (* Save and output *)
  if save_metadata metadata output_path then begin
    prerr_endline (Printf.sprintf "✅ Metadata saved: %s" output_path);
    print_endline (Yojson.Safe.pretty_to_string metadata);
    0
  end else begin
    prerr_endline "❌ Failed to save metadata";
    1
  end

let () = exit (main ())

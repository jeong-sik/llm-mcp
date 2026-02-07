[@@@warning "-27-32-33-34-37-69"]

(** Job Dispatcher for MASC Session Hook *)

open Common
open Yojson.Safe.Util

let run_capture_stdout prog args =
  let argv = Array.of_list (prog :: args) in
  let env = Unix.environment () in
  let devnull_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o644 in
  let devnull_err = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o644 in
  let stdout_r, stdout_w = Unix.pipe () in
  let pid = Unix.create_process_env prog argv env devnull_in stdout_w devnull_err in
  Unix.close devnull_in;
  Unix.close devnull_err;
  Unix.close stdout_w;
  let output =
    let ic = Unix.in_channel_of_descr stdout_r in
    Fun.protect
      ~finally:(fun () -> (try close_in ic with _ -> ()))
      (fun () -> In_channel.input_all ic)
  in
  let _pid, _status = Unix.waitpid [] pid in
  output

let count_newer_md_files ~marker_mtime root_dir =
  let count = ref 0 in
  let rec walk dir =
    try
      Sys.readdir dir
      |> Array.iter (fun name ->
        let path = Filename.concat dir name in
        try
          if Sys.is_directory path then walk path
          else if Filename.check_suffix path ".md" then begin
            let st = Unix.stat path in
            if st.Unix.st_mtime > marker_mtime then incr count
          end
        with _ -> ())
    with _ -> ()
  in
  if Sys.file_exists root_dir && Sys.is_directory root_dir then walk root_dir;
  !count

(* Check embedding sync status *)
let check_embedding_sync () =
  let me_root = Common.me_root in
  let last_sync_file = Filename.concat me_root "memory/cache/.last-embedding-sync" in
  
  if not (Sys.file_exists last_sync_file) then
    ["📝 Embedding: First run (baseline needed)"], []
  else
    let marker_mtime =
      try (Unix.stat last_sync_file).Unix.st_mtime with _ -> Unix.time ()
    in
    let count =
      count_newer_md_files ~marker_mtime (Filename.concat me_root "claude")
      + count_newer_md_files ~marker_mtime
          (Filename.concat me_root "memory/procedural-memory")
    in
    
    if count >= 10 then
      [], [Printf.sprintf "🔄 Embedding sync needed: %d new docs" count; "💡 Run: /sync-memory"]
    else if count > 0 then
      [Printf.sprintf "📊 Embedding status: %d new docs (threshold: 10)" count], []
    else
      ["✅ Embedding: Up to date"], []

(* Check WebSearch pending *)
let check_websearch () =
  let me_root = Common.me_root in
  let script = Filename.concat me_root "scripts/auto-websearch.py" in
  
  if Sys.file_exists script then
    let output = run_capture_stdout "python3" [ script; "pending" ] in
    match int_of_string_opt (String.trim output) with
    | Some n when n > 0 ->
        [], [Printf.sprintf "🔍 WebSearch suggestions available (%d pending)" n]
    | _ -> [], []
  else
    [], []

(* Main processing *)
let process_jobs json_input =
  let context = ref [] in
  let warnings = ref [] in
  
  (* 1. Embedding Sync *)
  let ctx, warn = check_embedding_sync () in
  context := !context @ ctx;
  warnings := !warnings @ warn;
  
  (* 2. WebSearch Check *)
  let ctx, warn = check_websearch () in
  context := !context @ ctx;
  warnings := !warnings @ warn;
  
  `Assoc [
    ("context", `List (List.map (fun s -> `String s) !context));
    ("warnings", `List (List.map (fun s -> `String s) !warnings));
    ("jobs_started", `Int 0)
  ]

(* Entry point *)
let () =
  let input_json = 
    try Yojson.Safe.from_channel stdin 
    with Yojson.Json_error _ -> `Assoc [] 
  in
  let result = process_jobs input_json in
  print_endline "--- JSON ---";
  print_endline (Yojson.Safe.pretty_to_string result)

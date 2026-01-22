(** Journal Tool - OCaml Port of journal_archiver.py *)

open Cmdliner

[@@@warning "-27-32-34-37-69"]

(* --- Utils --- *)

let run_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let all_input = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  all_input

let today_str () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

(* --- Logic --- *)

let get_journal_path date =
  (* Date format: YYYY-MM-DD *)
  let year_month = String.sub date 0 7 in (* YYYY-MM *)
  let day = String.sub date 8 2 in (* DD *)
  Filename.concat (Filename.concat (Filename.concat Llm_mcp.Common.me_root "claude") year_month) (day ^ ".md")

let count_lines file =
  if not (Sys.file_exists file) then 0
  else
    let cmd = Printf.sprintf "wc -l < %s" file in
    try int_of_string (String.trim (run_cmd cmd)) with Failure _ -> 0

let check_current threshold =
  let today = today_str () in
  let path = get_journal_path today in
  let lines = count_lines path in
  
  if lines > threshold then
    Printf.printf "⚠️  Journal %s has %d lines (threshold: %d). Archive recommended.\n" path lines threshold
  else
    Printf.printf "✅ Journal %s is healthy (%d lines)\n" path lines

(* --- CLI --- *)

let threshold_arg =
  let doc = "Line threshold for archiving" in
  Arg.(value & opt int 500 & info ["threshold"] ~doc)

let check_current_cmd =
  let doc = "Check current journal size" in
  let info = Cmd.info "check-current" ~doc in
  Cmd.v info Term.(const check_current $ threshold_arg)

let main_cmd =
  let doc = "Journal management tool" in
  let info = Cmd.info "journal_tool" ~doc in
  Cmd.group info [check_current_cmd]

let () = exit (Cmd.eval main_cmd)

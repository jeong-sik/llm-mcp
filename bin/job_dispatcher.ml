[@@@warning "-27-32-33-34-37-69"]

(** Job Dispatcher for MASC Session Hook *)

open Llm_mcp.Common
open Yojson.Safe.Util

(* Run shell command and return stdout *)
let run_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let all_input = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  all_input

(* Check embedding sync status *)
let check_embedding_sync () =
  let me_root = Llm_mcp.Common.me_root in
  let last_sync_file = Filename.concat me_root "memory/cache/.last-embedding-sync" in
  
  if not (Sys.file_exists last_sync_file) then
    ["📝 Embedding: First run (baseline needed)"], []
  else
    (* Count files newer than marker *)
    let cmd = Printf.sprintf "find %s/claude %s/memory/procedural-memory -name '*.md' -type f -newer %s 2>/dev/null | wc -l" 
      me_root me_root last_sync_file in
    let count_str = String.trim (run_cmd cmd) in
    let count = try int_of_string count_str with Failure _ -> 0 in
    
    if count >= 10 then
      [], [Printf.sprintf "🔄 Embedding sync needed: %d new docs" count; "💡 Run: /sync-memory"]
    else if count > 0 then
      [Printf.sprintf "📊 Embedding status: %d new docs (threshold: 10)" count], []
    else
      ["✅ Embedding: Up to date"], []

(* Check WebSearch pending *)
let check_websearch () =
  let me_root = Llm_mcp.Common.me_root in
  let script = Filename.concat me_root "scripts/auto-websearch.py" in
  
  if Sys.file_exists script then
    let cmd = Printf.sprintf "python3 %s pending 2>/dev/null" script in
    let output = run_cmd cmd in
    if String.contains output '3' && String.length output > 0 then (* Simple heuristic *)
       (* Parse output for better message later *)
       [], ["🔍 WebSearch suggestions available (run auto-websearch.py)"]
    else
       [], []
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

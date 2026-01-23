(** Ouroboros v52.0: Neural Forager (Profiler) - OCaml
    Measures system performance and identifies bottlenecks. *)

open Common

let profile_log = Filename.concat me_root "logs/evolution/profiler.jsonl"

let score duration =
  if duration < 1.0 then 95
  else if duration < 5.0 then 70
  else 40

let profile_command label command =
  Printf.printf "⏱️  [PROFILER] Monitoring: %s...\n" label;

  let start_time = Unix.gettimeofday () in

  let ic = Unix.open_process_in (command ^ " 2>&1") in
  let output = Buffer.create 256 in
  (try while true do Buffer.add_string output (input_line ic); Buffer.add_char output '\n' done
   with End_of_file -> ());
  let exit_status = Unix.close_process_in ic in

  let end_time = Unix.gettimeofday () in
  let duration = end_time -. start_time in

  let success = match exit_status with Unix.WEXITED 0 -> true | _ -> false in
  let output_size = Buffer.length output in

  Printf.printf "✅ Finished in %.4fs. (Efficiency Score: %d/100)\n" duration (score duration);

  (* Save stats *)
  ensure_dir (Filename.dirname profile_log);
  let stats = `Assoc [
    ("timestamp", `String (string_of_float (Unix.time ())));
    ("label", `String label);
    ("duration", `Float duration);
    ("success", `Bool success);
    ("output_size", `Int output_size);
  ] in
  let oc = open_out_gen [Open_append; Open_creat] 0o644 profile_log in
  output_string oc (Yojson.Safe.to_string stats ^ "\n");
  close_out oc

let print_usage () =
  print_endline "Usage: ouroboros-profiler '<command>'"

let () =
  if Array.length Sys.argv < 2 then begin
    print_usage ();
    exit 1
  end;
  profile_command "External Task" Sys.argv.(1)

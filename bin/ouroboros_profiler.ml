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

  let prog, args =
    match command with
    | [] -> ("", [])
    | p :: rest -> (p, rest)
  in
  if prog = "" then begin
    prerr_endline "❌ Missing command";
    exit 2
  end;
  let argv = Array.of_list (prog :: args) in
  let env = Unix.environment () in
  let devnull_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o644 in
  let out_r, out_w = Unix.pipe () in
  (* Merge stderr into stdout; only count bytes to avoid buffering huge output. *)
  let pid = Unix.create_process_env prog argv env devnull_in out_w out_w in
  Unix.close devnull_in;
  Unix.close out_w;
  let tmp = Bytes.create 8192 in
  let rec count_bytes acc =
    match Unix.read out_r tmp 0 (Bytes.length tmp) with
    | 0 -> acc
    | n -> count_bytes (acc + n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> count_bytes acc
  in
  let output_size = count_bytes 0 in
  (try Unix.close out_r with _ -> ());
  let _pid, exit_status = Unix.waitpid [] pid in

  let end_time = Unix.gettimeofday () in
  let duration = end_time -. start_time in

  let success = match exit_status with Unix.WEXITED 0 -> true | _ -> false in

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
  print_endline "Usage: ouroboros-profiler -- <command> [args...]"

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "--" :: cmd when cmd <> [] ->
      profile_command "External Task" cmd
  | _ ->
    print_usage ();
    exit 1

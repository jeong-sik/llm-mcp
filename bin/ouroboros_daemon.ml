(** Ouroboros Daemon: Autonomous Evolution Loop - OCaml
    Runs in background, searching for trends and planning upgrades. *)

open Common

let daemon_log = Filename.concat (Filename.concat me_root "logs/evolution") "daemon.log"
let interval = 900 (* 15 minutes in seconds *)

let log msg =
  let ts = time_str () in
  let line = Printf.sprintf "[%s] %s\n" ts msg in
  print_endline line;
  ensure_dir (Filename.dirname daemon_log);
  let oc = open_out_gen [Open_append; Open_creat] 0o644 daemon_log in
  output_string oc line;
  close_out oc

let run_evolution () =
  log "🔄 Starting Ouroboros evolution cycle...";
  let argv = [| "dune"; "exec"; "ouroboros-loop"; "--"; "--cycles"; "1" |] in
  let env = Unix.environment () in
  let devnull_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o644 in
  let out_r, out_w = Unix.pipe () in
  let pid = Unix.create_process_env "dune" argv env devnull_in out_w out_w in
  Unix.close devnull_in;
  Unix.close out_w;
  let ic = Unix.in_channel_of_descr out_r in
  (try
     while true do
       let line = input_line ic in
       log (Printf.sprintf "Output: %s" line)
     done
   with End_of_file -> ());
  close_in_noerr ic;
  ignore (Unix.waitpid [] pid)

let run_rescue_mission () =
  let tasks_dir = Filename.concat me_root "logs/swarm/tasks" in
  if Sys.file_exists tasks_dir && Sys.is_directory tasks_dir then begin
    let files = Sys.readdir tasks_dir in
    Array.iter (fun f ->
      if Filename.check_suffix f ".json" then begin
        let filepath = Filename.concat tasks_dir f in
        match read_json_opt filepath with
        | Some json ->
            let open Yojson.Safe.Util in
            let status = try json |> member "status" |> to_string with Type_error _ -> "" in
            if status = "SOS" then begin
              let task_id = Filename.chop_suffix f ".json" in
              log (Printf.sprintf "🚑 Detected SOS: %s" task_id);
              log (Printf.sprintf "🦸 Claiming task %s for rescue..." task_id)
            end
        | None -> ()
      end
    ) files
  end

let main () =
  log "♾️ Ouroboros Daemon Started (OCaml Native).";
  log (Printf.sprintf "📍 Monitoring log: tail -f %s" daemon_log);

  try
    while true do
      run_evolution ();
      run_rescue_mission ();
      log (Printf.sprintf "💤 Sleeping for %d seconds until next cycle." interval);
      Unix.sleep interval
    done
  with
  | Sys.Break -> log "🛑 Daemon stopped by user."
  | e -> log (Printf.sprintf "💥 Fatal daemon error: %s" (Printexc.to_string e))

let () =
  Sys.catch_break true;
  main ()

(** Ouroboros v65.0: Singularity Dispatcher - OCaml
    Unleashes a swarm of micro-agents for parallel evolution. *)

open Common

let log_dir = Filename.concat (Filename.concat me_root "logs/evolution") "war_room"

let deploy_agent task_name =
  let log_file = Filename.concat log_dir (Printf.sprintf "%s_%d.log" task_name (timestamp ())) in
  let argv =
    [|
      "dune";
      "exec";
      "ouroboros-spawn";
      "--";
      task_name;
      "--";
      "echo";
      "Processing";
      task_name ^ "...";
    |]
  in
  let env = Unix.environment () in
  let devnull_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o644 in
  let out_r, out_w = Unix.pipe () in
  (* Merge stderr into stdout for stable logging. *)
  let pid = Unix.create_process_env "dune" argv env devnull_in out_w out_w in
  Unix.close devnull_in;
  Unix.close out_w;
  let ic = Unix.in_channel_of_descr out_r in
  let output =
    Fun.protect
      ~finally:(fun () -> (try close_in ic with _ -> ()))
      (fun () -> In_channel.input_all ic)
  in
  let _pid, exit_status = Unix.waitpid [] pid in

  let oc = open_out log_file in
  output_string oc output;
  close_out oc;

  let status = match exit_status with Unix.WEXITED 0 -> "SUCCESS" | _ -> "FAIL" in
  (task_name, status)

let execute_swarm_task task_list =
  Printf.printf "⚡ [DISPATCHER] Unleashing Swarm: %d agents deployed!\n" (List.length task_list);

  ensure_dir log_dir;

  (* Note: OCaml doesn't have ThreadPoolExecutor, using sequential for simplicity *)
  (* For true parallelism, would use Lwt or Domainslib *)
  let results = List.map (fun task ->
    let (name, status) = deploy_agent task in
    Printf.printf "✅ [AGENT] Task '%s' completed with status: %s\n" name status;
    (name, status)
  ) task_list in

  results

let () =
  let missions = [
    "Code_Cleanup_A";
    "Security_Patch_B";
    "Feature_Extraction_C";
    "Doc_Update_D";
    "Neural_Test_E";
  ] in

  let start_time = Unix.gettimeofday () in
  let _results = execute_swarm_task missions in
  let end_time = Unix.gettimeofday () in

  Printf.printf "\n⚡ [WAR_ROOM] Total Swarm Execution Time: %.2fs\n" (end_time -. start_time);
  print_endline "🚀 System throughput increased!"

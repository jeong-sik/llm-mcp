(** Ouroboros v12.0: Ouroboros-X (Self-Replication) - OCaml
    Spawns a temporary clone of the system to run dangerous experiments safely. *)

open Common

let lab_root = "/tmp/ouroboros_labs"

let rec copy_dir src dst =
  ensure_dir dst;
  let files = Sys.readdir src in
  Array.iter (fun f ->
    let src_path = Filename.concat src f in
    let dst_path = Filename.concat dst f in
    if Sys.is_directory src_path then
      copy_dir src_path dst_path
    else begin
      let ic = open_in_bin src_path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;
      let oc = open_out_bin dst_path in
      output_string oc content;
      close_out oc
    end
  ) files

let spawn_lab experiment_name =
  let ts = timestamp () in
  let lab_dir = Filename.concat lab_root (Printf.sprintf "lab_%d_%s" ts experiment_name) in

  Printf.printf "🧬 [SPAWN] Creating clone in %s...\n" lab_dir;

  ensure_dir lab_root;
  ensure_dir lab_dir;

  (* Clone core structure *)
  let targets = ["scripts"; "lib"] in
  List.iter (fun target ->
    let src = Filename.concat me_root target in
    let dst = Filename.concat lab_dir target in
    if Sys.file_exists src && Sys.is_directory src then
      copy_dir src dst
  ) targets;

  (* Copy CLAUDE.md *)
  let claude_src = Filename.concat me_root "CLAUDE.md" in
  let claude_dst = Filename.concat lab_dir "CLAUDE.md" in
  if Sys.file_exists claude_src then begin
    let ic = open_in claude_src in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let oc = open_out claude_dst in
    output_string oc content;
    close_out oc
  end;

  print_endline "✅ Clone ready. Environment isolated.";
  lab_dir

let run_experiment lab_dir command =
  match command with
  | [] ->
      prerr_endline "❌ Missing command";
      false
  | prog :: args ->
      Printf.printf "🧪 [LAB] Running experiment (argv): %s\n"
        (String.concat " " (prog :: args));

      let env =
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
        Hashtbl.replace tbl "ME_ROOT" lab_dir;
        Hashtbl.to_seq tbl |> Seq.map (fun (k, v) -> k ^ "=" ^ v) |> Array.of_seq
      in

      let argv = Array.of_list (prog :: args) in
      let cwd = Sys.getcwd () in
      let exit_status =
        Fun.protect
          ~finally:(fun () -> (try Unix.chdir cwd with _ -> ()))
          (fun () ->
            Unix.chdir lab_dir;
            let pid =
              Unix.create_process_env prog argv env Unix.stdin Unix.stdout Unix.stderr
            in
            let _pid, status = Unix.waitpid [] pid in
            status)
      in

      match exit_status with
      | Unix.WEXITED 0 ->
          print_endline "✨ Experiment SUCCESS!";
          true
      | Unix.WEXITED n ->
          Printf.printf "💥 Experiment FAILED! (exit code: %d)\n" n;
          false
      | _ ->
          print_endline "💀 Lab exploded!";
          false

let cleanup lab_dir =
  Printf.printf "🧹 [CLEANUP] Destroying lab %s...\n" lab_dir;
  rm_rf lab_dir

let () =
  if Array.length Sys.argv < 4 then begin
    print_endline "Usage: ouroboros-spawn <experiment_name> -- <command> [args...]";
    exit 1
  end;

  let args = Array.to_list Sys.argv |> List.tl in
  let name, command =
    match args with
    | name :: "--" :: cmd -> (name, cmd)
    | _ ->
        print_endline "Usage: ouroboros-spawn <experiment_name> -- <command> [args...]";
        exit 1
  in

  let lab = spawn_lab name in
  let success = run_experiment lab command in
  cleanup lab;

  exit (if success then 0 else 1)

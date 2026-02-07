(** Ouroboros CLI Hub Wrapper (v2.0) - OCaml
    Dispatches commands to the various Ouroboros engines. *)

open Common

let scripts_dir = Filename.concat me_root "scripts"

let run_script script_name args =
  let script_path = Filename.concat scripts_dir script_name in
  if not (Sys.file_exists script_path) then begin
    Printf.printf "❌ Script not found: %s\n" script_path;
    exit 1
  end;
  let argv = Array.of_list ("python3" :: script_path :: args) in
  let env = Unix.environment () in
  let pid =
    Unix.create_process_env "python3" argv env Unix.stdin Unix.stdout Unix.stderr
  in
  let _pid, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> exit n
  | _ -> exit 1

let usage () =
  print_endline {|
🧬 OUROBOROS SYSTEM COMMANDS (sb ouroboros)

Intelligence:
  sb ouroboros evolve [N]   Run autonomous evolution loop
  sb ouroboros curriculum   Generate daily learning goals
  sb ouroboros wisdom       Synthesize high-level wisdom
  sb ouroboros quantum      Run probabilistic analysis
  sb ouroboros scholar      Conduct academic verification

Sensory & Action:
  sb ouroboros nerve <cmd>  Feel 'pain' during execution
  sb ouroboros spawn <exp>  Clone system for isolated lab
  sb ouroboros status       Check consciousness & heartbeat

Monitoring:
  sb dashboard              Open v2.0 Matrix Dashboard
  sb nas                    Scan system neural architecture
|}

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] -> usage (); exit 1
  | cmd :: rest ->
      match cmd with
      | "evolve" ->
          let cycles = match rest with h :: _ -> h | [] -> "1" in
          run_script "ouroboros_loop.py" ["--cycles"; cycles]
      | "curriculum" -> run_script "ouroboros_curriculum.py" []
      | "wisdom" -> run_script "wisdom_synthesizer.py" []
      | "quantum" -> run_script "../lib/quantum_brain.py" []
      | "scholar" -> run_script "ouroboros_scholar.py" rest
      | "nerve" -> run_script "ouroboros_nerve.py" rest
      | "spawn" -> run_script "ouroboros_spawn.py" rest
      | "status" ->
          run_script "ouroboros_sense.py" ["Status Check"];
          print_endline "\n[Heartbeat]";
          let heartbeat_log = Filename.concat me_root "logs/evolution/heartbeat.log" in
          read_lines_tail ~max_bytes:8192 ~max_lines:5 heartbeat_log
          |> List.iter print_endline
      | _ -> usage ()

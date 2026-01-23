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
  let cmd = Printf.sprintf "python3 %s %s" script_path (String.concat " " args) in
  let exit_code = Unix.system cmd in
  match exit_code with
  | Unix.WEXITED n when n <> 0 -> exit n
  | _ -> ()

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
          ignore (Unix.system (Printf.sprintf "tail -n 5 %s" heartbeat_log))
      | _ -> usage ()

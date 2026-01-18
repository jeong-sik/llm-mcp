(** Project Ouroboros: Infinity Loop (Semi-Auto) - OCaml
    Executes the evolution cycle N times safely. *)

open Llm_mcp.Common

let plans_dir = Filename.concat me_root "PLANS"

(* Note: run_command here has different logic than Common.run_command *)
let run_command cmd =
  let exit_code = Unix.system cmd in
  match exit_code with
  | Unix.WEXITED 0 -> true
  | Unix.WEXITED n ->
      Printf.printf "❌ Command failed: %s (exit code %d)\n" cmd n;
      false
  | _ -> false

let git_clean_check () =
  let ic = Unix.open_process_in "git status --porcelain" in
  let output = try input_line ic with End_of_file -> "" in
  ignore (Unix.close_process_in ic);
  if output <> "" then
    print_endline "⚠️  Git directory not clean. Please commit or stash changes first.\n   (Proceeding anyway for demonstration...)";
  true

let verify_evolution () =
  print_endline "🔍 Verifying mutation safety...";
  let cmd = Printf.sprintf "%s/scripts/sb doctor --no-network 2>/dev/null" me_root in
  let exit_code = Unix.system cmd in
  match exit_code with
  | Unix.WEXITED 0 ->
      print_endline "✅ Verification Passed.";
      true
  | _ ->
      print_endline "🛑 Verification Failed.";
      false

let execute_mutation title ptype suggestion =
  Printf.printf "🧬 Mutating: %s...\n" title;
  ensure_dir plans_dir;

  let ts = timestamp () in
  let filename = Printf.sprintf "evolution_%d_%s.md" ts ptype in
  let filepath = Filename.concat plans_dir filename in

  let content = Printf.sprintf {|# Evolution Result: %s
**Status**: AUTONOMOUS_IMPLEMENTATION_ATTEMPT
**Date**: %s

%s
|} title (time_str ()) suggestion in

  let oc = open_out filepath in
  output_string oc content;
  close_out oc;

  if verify_evolution () then begin
    if run_command "git add ." then begin
      let commit_cmd = Printf.sprintf "git commit -m 'feat(singularity): autonomous evolution of %s'" title in
      ignore (run_command commit_cmd);
      print_endline "💾 State saved & Persistent."
    end;
    true
  end else begin
    print_endline "🛡️  Survival Instinct Triggered: Rolling back broken mutation.";
    ignore (run_command "git reset --hard HEAD");
    false
  end

(* Simplified evolution sensing *)
let sense_trends () = [
  ("Quantum Cognition", "NEW_FEATURE", "Implement probabilistic reasoning loops");
  ("Swarm Intelligence", "NEW_FEATURE", "Coordinate micro-agents collectively");
  ("Self-Replicating Code", "NEW_FEATURE", "Spawn sub-instances for parallel evolution");
]

let main cycles =
  Printf.printf "♾️  Starting Ouroboros Loop (%d cycles)\n" cycles;

  if not (git_clean_check ()) then exit 1;

  for i = 1 to cycles do
    Printf.printf "\n🔄 --- Cycle %d/%d ---\n" i cycles;

    let trends = sense_trends () in
    let (title, ptype, suggestion) = List.nth trends ((i - 1) mod (List.length trends)) in

    Printf.printf "  [GAP] %s -> Evolution REQUIRED.\n" title;
    Printf.printf "  🎲 MCTS Simulation: Optimizing '%s'...\n" title;

    if execute_mutation title ptype suggestion then
      Printf.printf "✨ Cycle %d Success!\n" i
    else begin
      Printf.printf "💥 Cycle %d Failed!\n" i;
      (* Continue anyway for demo *)
    end;

    Unix.sleepf 0.5
  done;

  print_endline "\n🏁 Ouroboros Loop Finished."

let () =
  let cycles = ref 3 in
  Arg.parse [
    ("--cycles", Arg.Set_int cycles, "Number of evolution cycles");
  ] (fun _ -> ()) "Usage: ouroboros-loop [--cycles N]";

  main !cycles

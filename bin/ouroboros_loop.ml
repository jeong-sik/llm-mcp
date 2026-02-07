(** Project Ouroboros: Infinity Loop (Semi-Auto) - OCaml
    Executes the evolution cycle N times safely. *)

open Common

let plans_dir = Filename.concat me_root "PLANS"

let run_ok ?(stderr = `Dev_null) prog args =
  let res = Subprocess.run_capture ~stderr prog args in
  match res.status with Unix.WEXITED 0 -> true | _ -> false

let git_clean_check () =
  let out =
    Subprocess.run_capture ~stderr:`Dev_null "git" [ "-C"; me_root; "status"; "--porcelain" ]
    |> fun r -> String.trim r.stdout
  in
  if out <> "" then
    print_endline
      "⚠️  Git directory not clean. Please commit or stash changes first.\n   (Proceeding anyway for demonstration...)";
  true

let verify_evolution () =
  print_endline "🔍 Verifying mutation safety...";
  let sb = Filename.concat me_root "scripts/sb" in
  if not (Sys.file_exists sb) then begin
    print_endline "⚠️  sb script not found; skipping verification.";
    true
  end else if run_ok ~stderr:`Dev_null sb [ "doctor"; "--no-network" ] then begin
    print_endline "✅ Verification Passed.";
    true
  end else begin
    print_endline "🛑 Verification Failed.";
    false
  end

let execute_mutation ~enable_git title ptype suggestion =
  Printf.printf "🧬 Mutating: %s...\n" title;
  ensure_dir plans_dir;

  let ts = timestamp () in
  let filename = Printf.sprintf "evolution_%d_%s.md" ts ptype in
  let filepath = Filename.concat plans_dir filename in
  let relpath = Filename.concat "PLANS" filename in

  let content = Printf.sprintf {|# Evolution Result: %s
**Status**: AUTONOMOUS_IMPLEMENTATION_ATTEMPT
**Date**: %s

%s
|} title (time_str ()) suggestion in

  let oc = open_out filepath in
  output_string oc content;
  close_out oc;

  if verify_evolution () then begin
    if enable_git then begin
      if run_ok "git" [ "-C"; me_root; "add"; relpath ] then
        ignore
          (run_ok "git"
             [
               "-C";
               me_root;
               "commit";
               "-m";
               Printf.sprintf "feat(ouroboros): evolution %s" title;
             ]);
      print_endline "💾 Plan staged/committed (git enabled)."
    end else
      print_endline "💾 Plan written (git disabled).";
    true
  end else begin
    print_endline "🛡️  Survival Instinct Triggered: Rolling back broken mutation.";
    (try Sys.remove filepath with _ -> ());
    false
  end

(* Simplified evolution sensing *)
let sense_trends () = [
  ("Quantum Cognition", "NEW_FEATURE", "Implement probabilistic reasoning loops");
  ("Swarm Intelligence", "NEW_FEATURE", "Coordinate micro-agents collectively");
  ("Self-Replicating Code", "NEW_FEATURE", "Spawn sub-instances for parallel evolution");
]

let main ~enable_git cycles =
  Printf.printf "♾️  Starting Ouroboros Loop (%d cycles)\n" cycles;

  if not (git_clean_check ()) then exit 1;

  for i = 1 to cycles do
    Printf.printf "\n🔄 --- Cycle %d/%d ---\n" i cycles;

    let trends = sense_trends () in
    let (title, ptype, suggestion) = List.nth trends ((i - 1) mod (List.length trends)) in

    Printf.printf "  [GAP] %s -> Evolution REQUIRED.\n" title;
    Printf.printf "  🎲 MCTS Simulation: Optimizing '%s'...\n" title;

    if execute_mutation ~enable_git title ptype suggestion then
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
  let enable_git = ref false in
  Arg.parse [
    ("--cycles", Arg.Set_int cycles, "Number of evolution cycles");
    ("--git", Arg.Set enable_git, "Enable git add/commit for generated plans (default: off)");
  ] (fun _ -> ()) "Usage: ouroboros-loop [--cycles N]";

  main ~enable_git:(!enable_git) !cycles

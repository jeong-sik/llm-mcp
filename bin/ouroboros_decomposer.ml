(** Ouroboros v66.0: Automatic Decomposer - OCaml
    Breaks down high-level commands into parallelizable sub-tasks. *)

open Llm_mcp.Common

let wisdom_dir = Filename.concat (Filename.concat me_root "knowledge") "wisdom"

let strategies = [
  ("optimize", ["Performance_Profiling"; "Code_Minification"; "Memory_Leak_Check"; "Dependency_Pruning"]);
  ("security", ["Secret_Scan"; "Permission_Audit"; "Entry_Point_Hardening"; "Dependency_Vulnerability_Check"]);
  ("refactor", ["Docstring_Update"; "Type_Hint_Injection"; "Dead_Code_Elimination"; "Naming_Consistency_Check"]);
  ("evolve", ["Trend_Scouting"; "Curriculum_Generation"; "Wisdom_Synthesis"; "Neural_Consolidation"]);
]

let incorporate_previous_wisdom () =
  print_endline "🔍 [FEEDBACK] Recalling previous mission outcomes...";
  if Sys.file_exists wisdom_dir && Sys.is_directory wisdom_dir then begin
    let files = Sys.readdir wisdom_dir
      |> Array.to_list
      |> List.filter (fun f -> String.length f > 13 && String.sub f 0 13 = "swarm_wisdom_")
      |> List.sort compare
      |> List.rev in
    match files with
    | f :: _ ->
        let filepath = Filename.concat wisdom_dir f in
        let content =
          let ic = open_in filepath in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic; s
        in
        if String.length content > 0 &&
           (try ignore (Str.search_forward (Str.regexp "Success Rate: 100.0%") content 0); true
            with Not_found -> false)
        then "🚀 Previous mission was PERFECT. Maintaining high-speed velocity."
        else begin
          print_endline "⚠️  [ADAPT] Previous mission had friction. Adding safety measures.";
          "CAUTION"
        end
    | [] -> "No previous data. Using default parameters."
  end else "No previous data. Using default parameters."

let decompose_and_run command =
  let feedback = incorporate_previous_wisdom () in
  Printf.printf "💡 [STRATEGY] %s\n" feedback;

  Printf.printf "🧐 [DECOMPOSER] Analyzing Master's Command: '%s'\n" command;

  let cmd_lower = String.lowercase_ascii command in
  let target_tasks = List.fold_left (fun acc (key, subtasks) ->
    if String.length cmd_lower >= String.length key &&
       (try ignore (Str.search_forward (Str.regexp key) cmd_lower 0); true
        with Not_found -> false)
    then acc @ subtasks
    else acc
  ) [] strategies in

  let target_tasks =
    if feedback = "CAUTION" then target_tasks @ ["Final_Safety_Audit"]
    else target_tasks
  in

  let target_tasks =
    if target_tasks = [] then [Str.global_replace (Str.regexp " ") "_" command]
    else target_tasks
  in

  Printf.printf "🎯 [PLAN] Identified %d parallel tracks:\n" (List.length target_tasks);
  List.iter (fun t -> Printf.printf "   • %s\n" t) target_tasks;

  print_endline "\n📡 [DISPATCH] Ready to dispatch via ouroboros-dispatcher..."

let print_usage () =
  print_endline "Usage: ouroboros-decomposer 'High-level Command'"

let () =
  if Array.length Sys.argv < 2 then begin
    print_usage ();
    exit 1
  end;
  decompose_and_run Sys.argv.(1)

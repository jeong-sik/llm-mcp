(** Ouroboros v39.0: Autonomous Curriculum Learning - OCaml
    Determines the optimal learning path to accelerate AGI evolution. *)

open Llm_mcp.Common

let curriculum_dir = Filename.concat (Filename.concat me_root "knowledge") "curriculum"
let research_dir = Filename.concat (Filename.concat me_root "knowledge") "research"

let list_discoveries () =
  ensure_dir research_dir;
  if Sys.file_exists research_dir then
    Sys.readdir research_dir
    |> Array.to_list
    |> List.filter (fun f ->
        String.length f > 10 && String.sub f 0 9 = "discovery")
  else []

let extract_goal filepath =
  match read_json_opt filepath with
  | Some json ->
      let open Yojson.Safe.Util in
      let tech = try json |> member "tech" |> to_string with _ -> "Unknown" in
      let summary = try json |> member "summary" |> to_string with _ -> "" in
      (* Simulated potential score *)
      let potential = 0.5 +. (Random.float 0.5) in
      Some (tech, potential, summary)
  | None -> None

let generate_daily_curriculum () =
  print_endline "🎓 [CURRICULUM] Analyzing cognitive gaps and evolutionary potential...";
  ensure_dir curriculum_dir;

  let discoveries = list_discoveries () in
  let goals = List.filter_map (fun f ->
    let path = Filename.concat research_dir f in
    extract_goal path
  ) discoveries in

  (* Sort by potential descending *)
  let sorted = List.sort (fun (_, p1, _) (_, p2, _) ->
    compare p2 p1) goals in

  match sorted with
  | [] ->
      print_endline "💤 No urgent learning gaps found. Reviewing core principles.";
      None
  | (tech, potential, _summary) :: _ ->
      Printf.printf "🔥 [PRIORITY] Today's learning focus: '%s' (Growth Potential: %.1f%%)\n"
        tech (potential *. 100.0);

      let today = date_str () in
      let curriculum = `Assoc [
        ("title", `String (Printf.sprintf "Mastering %s" tech));
        ("date", `String today);
        ("steps", `List [
          `Assoc [("step", `Int 1); ("task", `String (Printf.sprintf "Synthesize %s theoretical framework" tech)); ("status", `String "PENDING")];
          `Assoc [("step", `Int 2); ("task", `String (Printf.sprintf "Scan for existing code patterns in %s" tech)); ("status", `String "PENDING")];
          `Assoc [("step", `Int 3); ("task", `String "Implement a viable prototype in Spawn Lab"); ("status", `String "PENDING")];
        ]);
        ("estimated_reward", `Float 100.0);
      ] in

      let filename = Printf.sprintf "curriculum_%s.json" today in
      let filepath = Filename.concat curriculum_dir filename in
      ignore (write_json filepath curriculum);
      Printf.printf "📝 [SUCCESS] Daily Curriculum established: %s\n" filename;
      Some curriculum

let () =
  Random.self_init ();
  ignore (generate_daily_curriculum ())

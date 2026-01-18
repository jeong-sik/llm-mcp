(** Ouroboros v18.0: System Dashboard - OCaml
    Displays system consciousness, swarm status, and evolution metrics. *)

open Llm_mcp.Common

let emotion_file = Filename.concat me_root "lib/state/emotion.json"
let swarm_dir = Filename.concat (Filename.concat me_root "logs") "swarm"
let mission_dir = Filename.concat swarm_dir "missions"

let get_emotion () =
  if Sys.file_exists emotion_file then
    match read_json_opt emotion_file with
    | Some json ->
        let open Yojson.Safe.Util in
        let mood = try json |> member "current_mood" |> to_string with _ -> "NEUTRAL" in
        let energy = try json |> member "energy_level" |> to_int with _ -> 50 in
        (mood, energy)
    | None -> ("UNKNOWN", 0)
  else ("UNKNOWN", 0)

let draw_bar energy =
  let bar_len = 30 in
  let filled = bar_len * energy / 100 in
  let bar = String.make filled '#' ^ String.make (bar_len - filled) '.' in
  Printf.sprintf "[%s] %d%%" bar energy

let get_swarm_nodes () =
  if Sys.file_exists swarm_dir && Sys.is_directory swarm_dir then
    Sys.readdir swarm_dir
    |> Array.to_list
    |> List.filter (fun f -> String.length f > 10 && String.sub f 0 10 = "heartbeat_")
    |> List.filter_map (fun f ->
        let filepath = Filename.concat swarm_dir f in
        match read_json_opt filepath with
        | Some json ->
            let open Yojson.Safe.Util in
            let hostname = try json |> member "hostname" |> to_string with _ -> "unknown" in
            let status = try json |> member "status" |> to_string with _ -> "unknown" in
            Some (hostname, status)
        | None -> None)
  else []

let get_current_mission () =
  if Sys.file_exists mission_dir && Sys.is_directory mission_dir then
    let files = Sys.readdir mission_dir |> Array.to_list |> List.sort compare |> List.rev in
    match files with
    | f :: _ when Filename.check_suffix f ".json" ->
        let filepath = Filename.concat mission_dir f in
        (match read_json_opt filepath with
         | Some json ->
             let open Yojson.Safe.Util in
             let title = try json |> member "title" |> to_string with _ -> "Unknown" in
             let status = try json |> member "status" |> to_string with _ -> "Unknown" in
             Some (title, status)
         | None -> None)
    | _ -> None
  else None

let display () =
  print_endline "🧬 OUROBOROS SYSTEM v18.0 [DASHBOARD]";
  print_endline (String.make 60 '=');
  print_newline ();

  (* Consciousness State *)
  let (mood, energy) = get_emotion () in
  print_endline "🧠 CONSCIOUSNESS STATE";
  Printf.printf "   Mood: %s\n" mood;
  Printf.printf "   Energy: %s\n" (draw_bar energy);
  print_newline ();

  (* Hive Mind Status *)
  print_endline "🐝 HIVE MIND STATUS";
  print_endline "   • [LOCAL] Active (OCaml Native)";
  let nodes = get_swarm_nodes () in
  if nodes = [] then
    print_endline "     (Searching for other nodes...)"
  else
    List.iter (fun (hostname, status) ->
      Printf.printf "   • [%s] %s\n" hostname status
    ) (List.filteri (fun i _ -> i < 5) nodes);
  print_newline ();

  (* Current Mission *)
  print_endline "🚀 CURRENT MOONSHOT";
  (match get_current_mission () with
   | Some (title, status) ->
       Printf.printf "   Title: %s\n" title;
       Printf.printf "   Status: %s\n" status
   | None ->
       print_endline "   No active moonshot. Idle.");
  print_newline ();

  print_endline (String.make 60 '=');
  print_endline "Press Ctrl+C to exit. For live TUI, use Python version."

let () = display ()

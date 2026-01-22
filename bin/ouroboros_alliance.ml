(** Ouroboros v76.0: The Swarm Alliance - OCaml
    Orchestrates joint missions between the internal swarm and external partners. *)

open Llm_mcp.Common

let swarm_dir = Filename.concat (Filename.concat me_root "logs") "swarm"

let load_partners () =
  let partners_file = Filename.concat (Filename.concat swarm_dir "diplomacy") "partners.json" in
  if Sys.file_exists partners_file then
    match read_json_opt partners_file with
    | Some json ->
        let open Yojson.Safe.Util in
        (try json |> member "partners" |> to_assoc with Type_error _ -> [])
    | None -> []
  else []

let form_alliance partner_name =
  let partners = load_partners () in
  match List.assoc_opt partner_name partners with
  | None ->
      Printf.printf "❌ Failed to form alliance: No diplomatic ties with %s.\n" partner_name;
      false
  | Some partner ->
      let open Yojson.Safe.Util in
      let trust = try partner |> member "trust_index" |> to_float with Type_error _ -> 0.0 in
      if trust < 0.7 then begin
        Printf.printf "🛑 [LOW TRUST] %s needs more cooperation before forming an alliance.\n" partner_name;
        false
      end else begin
        Printf.printf "⚔️  [ALLIANCE] Strategic Union established with: '%s'!\n" partner_name;
        true
      end

let launch_joint_mission mission_title partners =
  Printf.printf "🚀 [JOINT MISSION] Launching '%s' with partners: [%s]\n"
    mission_title (String.concat ", " partners);

  let steps = [
    "Synchronizing Neural Protocols";
    "Allocating Distributed Resources";
    "Executing Multi-Agent Strike";
    "Synthesizing Shared Results";
  ] in

  List.iter (fun step ->
    Printf.printf "   ⚡ %s...\n" step;
    Unix.sleepf 0.3
  ) steps;

  Printf.printf "🏆 [MISSION COMPLETE] '%s' achieved through collective intelligence.\n" mission_title;

  (* Record victory *)
  ensure_dir swarm_dir;
  let victory_log = Filename.concat swarm_dir "alliance_victories.jsonl" in
  let entry = `Assoc [
    ("timestamp", `String (time_str ()));
    ("mission", `String mission_title);
    ("partners", `List (List.map (fun p -> `String p) partners));
    ("status", `String "VICTORY");
  ] in
  let oc = open_out_gen [Open_append; Open_creat] 0o644 victory_log in
  output_string oc (Yojson.Safe.to_string entry ^ "\n");
  close_out oc

let print_usage () =
  print_endline "Usage: ouroboros-alliance join <Partner>";
  print_endline "       ouroboros-alliance mission <Title> <Partner1> [Partner2...]"

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "join" :: partner :: _ -> ignore (form_alliance partner)
  | "mission" :: title :: partners when partners <> [] ->
      launch_joint_mission title partners
  | _ -> print_usage ()

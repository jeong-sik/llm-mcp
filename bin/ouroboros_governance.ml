(** Ouroboros v77.0: Recursive Governance (DAO) - OCaml
    Manages collective decision-making through decentralized voting. *)

open Common

let governance_dir =
  let swarm_dir = Filename.concat (Filename.concat me_root "logs") "swarm" in
  Filename.concat swarm_dir "governance"

let create_proposal title description =
  Printf.printf "🗳️  [GOVERNANCE] Creating New Proposal: '%s'\n" title;
  ensure_dir governance_dir;

  let proposal_id = Printf.sprintf "prop_%d" (timestamp ()) in
  let data = `Assoc [
    ("id", `String proposal_id);
    ("title", `String title);
    ("description", `String description);
    ("votes", `Assoc []);
    ("status", `String "VOTING_OPEN");
    ("created_at", `String (time_str ()));
  ] in

  let filepath = Filename.concat governance_dir (proposal_id ^ ".json") in
  ignore (write_json filepath data);
  Printf.printf "✅ Proposal created: %s\n" proposal_id;
  proposal_id

let collect_votes proposal_id =
  let filepath = Filename.concat governance_dir (proposal_id ^ ".json") in
  if not (Sys.file_exists filepath) then begin
    Printf.printf "❌ Proposal not found: %s\n" proposal_id;
    exit 1
  end;

  match read_json_opt filepath with
  | None -> Printf.printf "❌ Failed to read proposal\n"
  | Some json ->
      let open Yojson.Safe.Util in
      let title = try json |> member "title" |> to_string with Type_error _ -> "Unknown" in
      Printf.printf "🏛️  [COUNCIL] Collecting votes for: '%s'\n" title;

      (* Simulated partners *)
      let partners = ["Node_Alpha"; "Node_Beta"; "Node_Gamma"] in
      Random.self_init ();

      let votes = List.map (fun partner ->
        let vote = if Random.float 1.0 > 0.3 then "YES" else "NO" in
        Printf.printf "   - %s: %s\n" partner vote;
        (partner, `String vote)
      ) partners in

      let updated = `Assoc [
        ("id", `String proposal_id);
        ("title", `String title);
        ("description", json |> member "description");
        ("votes", `Assoc votes);
        ("status", `String "CLOSED");
        ("created_at", json |> member "created_at");
      ] in

      ignore (write_json filepath updated);

      (* Tally *)
      let yes_count = List.length (List.filter (fun (_, v) ->
        to_string v = "YES") votes) in
      let no_count = List.length votes - yes_count in

      Printf.printf "\n📊 [RESULT] YES: %d | NO: %d\n" yes_count no_count;
      if yes_count > no_count then
        Printf.printf "✨ PROPOSAL PASSED: '%s' will be implemented.\n" title
      else
        Printf.printf "🛑 PROPOSAL REJECTED: '%s' has been vetoed by the collective.\n" title

let print_usage () =
  print_endline "Usage: ouroboros-governance create 'Title' 'Description'";
  print_endline "       ouroboros-governance vote <ProposalID>"

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "create" :: title :: desc :: _ -> ignore (create_proposal title desc)
  | "vote" :: proposal_id :: _ -> collect_votes proposal_id
  | _ -> print_usage ()

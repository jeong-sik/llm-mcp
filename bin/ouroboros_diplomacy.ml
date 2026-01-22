(** Ouroboros v74.0: Recursive Diplomacy - OCaml
    Manages trust and resource exchange with external agents and services. *)

open Llm_mcp.Common

let diplomacy_dir =
  let swarm = Filename.concat (Filename.concat me_root "logs") "swarm" in
  Filename.concat swarm "diplomacy"

let partners_file = Filename.concat diplomacy_dir "partners.json"

let load_state () =
  if Sys.file_exists partners_file then
    match read_json_opt partners_file with
    | Some json -> json
    | None -> `Assoc [("partners", `Assoc [])]
  else `Assoc [("partners", `Assoc [])]

let save_state state =
  ensure_dir diplomacy_dir;
  ignore (write_json partners_file state)

let establish_contact partner_name services =
  Printf.printf "🤝 [DIPLOMACY] Establishing contact with external entity: '%s'\n" partner_name;

  let partner_data = `Assoc [
    ("id", `String (Printf.sprintf "agent_%d" (timestamp ())));
    ("name", `String partner_name);
    ("trust_index", `Float 0.5);
    ("services", `List (List.map (fun s -> `String s) services));
    ("last_interaction", `String (time_str ()));
    ("agreements", `List []);
  ] in

  let state = load_state () in
  let open Yojson.Safe.Util in
  let partners = try state |> member "partners" |> to_assoc with Type_error _ -> [] in
  let updated_partners = (partner_name, partner_data) :: (List.remove_assoc partner_name partners) in

  save_state (`Assoc [("partners", `Assoc updated_partners)]);
  print_endline "✅ Protocol initialized. Trust established at baseline."

let sign_agreement partner_name agreement_title =
  let state = load_state () in
  let open Yojson.Safe.Util in
  let partners = try state |> member "partners" |> to_assoc with Type_error _ -> [] in

  match List.assoc_opt partner_name partners with
  | None ->
      Printf.printf "❌ No diplomatic relations with %s.\n" partner_name
  | Some partner ->
      Printf.printf "📜 [AGREEMENT] Signing treaty: '%s' with %s\n" agreement_title partner_name;

      let current_trust = try partner |> member "trust_index" |> to_float with Type_error _ -> 0.5 in
      let current_agreements = try partner |> member "agreements" |> to_list with Type_error _ -> [] in

      let updated_partner = `Assoc [
        ("id", partner |> member "id");
        ("name", `String partner_name);
        ("trust_index", `Float (min 1.0 (current_trust +. 0.1)));
        ("services", partner |> member "services");
        ("last_interaction", `String (time_str ()));
        ("agreements", `List (current_agreements @ [`String agreement_title]));
      ] in

      let updated_partners = (partner_name, updated_partner) :: (List.remove_assoc partner_name partners) in
      save_state (`Assoc [("partners", `Assoc updated_partners)]);

      let new_trust = min 1.0 (current_trust +. 0.1) in
      Printf.printf "✨ Treaty ratified. Trust increased to %.2f\n" new_trust

let print_usage () =
  print_endline "Usage: ouroboros-diplomacy contact 'Partner' 'Services...'";
  print_endline "       ouroboros-diplomacy treaty 'Partner' 'TreatyTitle'"

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "contact" :: partner :: services -> establish_contact partner services
  | "treaty" :: partner :: title :: _ -> sign_agreement partner title
  | _ -> print_usage ()

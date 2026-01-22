(** Ouroboros v75.0: Galactic Trade - OCaml
    Facilitates knowledge exchange between the swarm and external partners. *)

open Common

let knowledge_dir = Filename.concat (Filename.concat me_root "knowledge") "imported"
let diplomacy_dir =
  let swarm = Filename.concat (Filename.concat me_root "logs") "swarm" in
  Filename.concat swarm "diplomacy"

let load_partners () =
  let partners_file = Filename.concat diplomacy_dir "partners.json" in
  if Sys.file_exists partners_file then
    match read_json_opt partners_file with
    | Some json ->
        let open Yojson.Safe.Util in
        (try json |> member "partners" |> to_assoc with Type_error _ -> [])
    | None -> []
  else []

let import_knowledge item_name source =
  ensure_dir knowledge_dir;
  let filename = Printf.sprintf "imported_%s.md" (String.lowercase_ascii item_name) in
  let filepath = Filename.concat knowledge_dir filename in

  let content = Printf.sprintf {|# Imported Knowledge: %s
**Source**: %s
**Acquired**: %s
**Status**: Integrated

## Overview
Automated summary of the imported %s technology...
|} item_name source (date_str ()) item_name in

  let oc = open_out filepath in
  output_string oc content;
  close_out oc;
  Printf.printf "📦 [STOCKED] New knowledge added to warehouse: %s\n" filename

let propose_trade partner_name export_item import_item =
  Printf.printf "🐫 [TRADE] Proposing exchange with %s:\n" partner_name;
  Printf.printf "   📤 Export: %s\n" export_item;
  Printf.printf "   📥 Import: %s\n" import_item;

  let partners = load_partners () in
  match List.assoc_opt partner_name partners with
  | None ->
      Printf.printf "❌ Trade failed: No diplomatic relations with %s.\n" partner_name;
      false
  | Some partner ->
      let open Yojson.Safe.Util in
      let trust = try partner |> member "trust_index" |> to_float with Type_error _ -> 0.0 in
      Printf.printf "⚖️  [MERCHANT] Partner Trust Index: %.2f\n" trust;

      if trust < 0.6 then begin
        print_endline "🛑 [DENIED] Trust too low for high-value knowledge exchange.";
        false
      end else begin
        print_endline "⏳ Negotiating terms...";
        Unix.sleepf 0.5;
        import_knowledge import_item partner_name;
        print_endline "💰 [SUCCESS] Trade completed. Wealth of knowledge increased.";
        true
      end

let print_usage () =
  print_endline "Usage: ouroboros-trade <Partner> <ExportItem> <ImportItem>"

let () =
  if Array.length Sys.argv < 4 then
    print_usage ()
  else
    ignore (propose_trade Sys.argv.(1) Sys.argv.(2) Sys.argv.(3))

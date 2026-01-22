(** Ouroboros CLI: Akasha Interface - OCaml
    Manifests and consults universal evolution principles. *)

open Common

let akasha_dir = Filename.concat (Filename.concat me_root "knowledge") "akasha"

let manifest_principle title essence count =
  ensure_dir akasha_dir;
  let timestamp = timestamp () in
  let filename = Printf.sprintf "principle_%d.json" timestamp in
  let filepath = Filename.concat akasha_dir filename in
  let json = `Assoc [
    ("title", `String title);
    ("essence", `String essence);
    ("evidence_count", `Int count);
    ("created_at", `String (time_str ()));
  ] in
  ignore (write_json filepath json);
  Printf.printf "📜 [AKASHA] Manifested principle: '%s'\n" title;
  Printf.printf "   Evidence count: %d\n" count;
  Printf.printf "   Saved to: %s\n" filename

let search_records query =
  ensure_dir akasha_dir;
  let files = Sys.readdir akasha_dir in
  Printf.printf "🔍 [AKASHA] Searching for: '%s'\n" query;
  Array.iter (fun f ->
    if String.sub f 0 (min 9 (String.length f)) = "principle" then begin
      let filepath = Filename.concat akasha_dir f in
      match read_json_opt filepath with
      | Some json ->
          let open Yojson.Safe.Util in
          let title = try json |> member "title" |> to_string with Type_error _ -> "Unknown" in
          if query = "*" || String.lowercase_ascii title
             |> fun t -> String.length t > 0 &&
                         (try ignore (Str.search_forward (Str.regexp_string (String.lowercase_ascii query)) t 0); true
                          with Not_found -> false) then
            Printf.printf "  📜 %s\n" title
      | None -> ()
    end
  ) files

let print_usage () =
  print_endline "Usage: ouroboros-akasha manifest 'Title' 'Essence' [EvidenceCount]";
  print_endline "       ouroboros-akasha search 'Query'"

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "manifest" :: title :: essence :: rest ->
      let count = match rest with
        | n :: _ -> (try int_of_string n with Failure _ -> 1)
        | [] -> 1
      in
      manifest_principle title essence count
  | "search" :: rest ->
      let query = match rest with q :: _ -> q | [] -> "*" in
      search_records query
  | _ -> print_usage ()

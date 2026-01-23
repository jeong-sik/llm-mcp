(** Ouroboros v36.0: Self-Specifying Engine (The Legislator) - OCaml
    Analyzes code to generate Formal Gatekeeper contracts. *)

let analyze_file filepath =
  Printf.printf "📖 [LEGISLATOR] Reading code for legal review: %s\n" (Filename.basename filepath);

  if not (Sys.file_exists filepath) then begin
    Printf.printf "❌ File not found: %s\n" filepath;
    exit 1
  end;

  let ic = open_in filepath in
  let n = in_channel_length ic in
  let content = really_input_string ic n in
  close_in ic;

  (* Look for function definitions and infer contracts *)
  let func_pattern = Str.regexp "let[ \t]+\\([a-z_][a-zA-Z0-9_]*\\)" in
  let param_pattern = Str.regexp "\\(count\\|amount\\|size\\|index\\)" in

  let rec find_functions start =
    try
      let _ = Str.search_forward func_pattern content start in
      let func_name = Str.matched_group 1 content in
      let end_pos = Str.match_end () in

      (* Check if any numeric param keywords nearby *)
      let lookahead = min (end_pos + 100) (String.length content) in
      let snippet = String.sub content end_pos (lookahead - end_pos) in

      (try
        let _ = Str.search_forward param_pattern snippet 0 in
        let param = Str.matched_string snippet in
        Printf.printf "⚖️  [LEGISLATOR] Inferred contract for '%s': %s >= 0\n" func_name param
      with Not_found -> ());

      find_functions end_pos
    with Not_found -> ()
  in

  find_functions 0;
  print_endline "📜 [ANALYSIS] Legal review complete. Manual injection required."

let print_usage () =
  print_endline "Usage: ouroboros-formal <file.ml>"

let () =
  if Array.length Sys.argv < 2 then begin
    print_usage ();
    exit 1
  end;
  analyze_file Sys.argv.(1)

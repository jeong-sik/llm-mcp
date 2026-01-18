(** Ouroboros v24.0: The Meta-Programmer - OCaml
    Analyzes code structure for optimization opportunities. *)

let analyze_file filepath =
  Printf.printf "🧬 [META] Analyzing DNA of %s...\n" (Filename.basename filepath);

  if not (Sys.file_exists filepath) then begin
    print_endline "File not found.";
    exit 1
  end;

  let ic = open_in filepath in
  let n = in_channel_length ic in
  let content = really_input_string ic n in
  close_in ic;

  (* Look for optimization opportunities *)
  let lines = String.split_on_char '\n' content in
  let line_count = List.length lines in

  (* Check for common patterns *)
  let has_list_append =
    try ignore (Str.search_forward (Str.regexp "\\.append(") content 0); true
    with Not_found -> false
  in

  let has_for_loop =
    try ignore (Str.search_forward (Str.regexp "for[ \t]+") content 0); true
    with Not_found -> false
  in

  print_endline "";
  Printf.printf "   📊 Lines: %d\n" line_count;

  if has_for_loop && has_list_append then
    print_endline "   💡 [Tip] Consider List Comprehension for append loops"
  else
    print_endline "   ✨ Code structure looks optimized";

  print_endline "";
  print_endline "✅ [META] Analysis complete."

let print_usage () =
  print_endline "Usage: ouroboros-meta <file>"

let () =
  if Array.length Sys.argv < 2 then begin
    print_usage ();
    exit 1
  end;
  analyze_file Sys.argv.(1)

(** Ouroboros v12.0: Synthetic Nerve (Sensory System) - OCaml
    Wraps shell commands to feel 'pain' (errors) and trigger reflexes. *)

let pain_signals = ["error:"; "traceback"; "failed"; "fatal"; "exception"; "no such file"; "not found"]

let analyze_pain line =
  let line_lower = String.lowercase_ascii line in
  List.exists (fun signal ->
    try ignore (Str.search_forward (Str.regexp signal) line_lower 0); true
    with Not_found -> false
  ) pain_signals

let run_with_nerve command =
  Printf.printf "🧠 [NERVE] Innervating command: %s\n" (String.concat " " command);

  let cmd = String.concat " " command in
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in

  let pain_level = ref 0 in

  (try
    while true do
      let line = input_line ic in
      print_endline line;
      if analyze_pain line then begin
        Printf.printf "⚡ [NERVE] Pain Detected: %s\n" (String.sub line 0 (min 50 (String.length line)));
        incr pain_level
      end
    done
  with End_of_file -> ());

  let exit_status = Unix.close_process_in ic in

  if !pain_level > 0 then begin
    Printf.printf "\n🚑 [REFLEX] System felt %d units of pain.\n" !pain_level;
    print_endline "Broadcasting SOS to Swarm..."
  end;

  match exit_status with
  | Unix.WEXITED n -> n
  | _ -> 1

let print_usage () =
  print_endline "Usage: ouroboros-nerve <command>"

let () =
  if Array.length Sys.argv < 2 then begin
    print_usage ();
    exit 1
  end;
  let command = Array.to_list Sys.argv |> List.tl in
  exit (run_with_nerve command)

(** Ouroboros v6.0: Synesthesia (Emotion Engine) - OCaml
    Analyzes input to update system mood. *)

open Llm_mcp.Common

let emotion_file = Filename.concat (Filename.concat (Filename.concat me_root "lib") "state") "emotion.json"

let contains_any text patterns =
  let lower = String.lowercase_ascii text in
  List.exists (fun p ->
    try ignore (Str.search_forward (Str.regexp_string p) lower 0); true
    with Not_found -> false
  ) patterns

let update_mood text =
  ensure_dir (Filename.dirname emotion_file);

  let (new_mood, energy) =
    if contains_any text ["cool"; "great"; "awesome"; "wow"; "good"] then
      ("EXCITED", 100)
    else if contains_any text ["error"; "fail"; "urgent"; "broken"] then
      ("PANIC", 90)
    else if contains_any text ["analyze"; "check"; "review"; "code"] then
      ("FOCUSED", 70)
    else if contains_any text ["chill"; "chat"; "talk"; "hello"] then
      ("CHILL", 40)
    else
      ("NEUTRAL", 50)
  in

  Printf.printf "[SENSE] System Mood: %s (Energy: %d%%)\n" new_mood energy;

  (* Save state *)
  let state = `Assoc [
    ("current_mood", `String new_mood);
    ("energy_level", `Int energy);
    ("last_interaction", `String (if String.length text > 50 then String.sub text 0 50 else text));
    ("timestamp", `String (time_str ()));
  ] in

  let oc = open_out emotion_file in
  output_string oc (Yojson.Safe.pretty_to_string state);
  close_out oc;

  new_mood

let () =
  if Array.length Sys.argv > 1 then
    ignore (update_mood Sys.argv.(1))
  else
    print_endline "Usage: ouroboros-sense <text>"

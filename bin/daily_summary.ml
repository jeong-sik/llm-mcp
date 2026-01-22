(** daily_summary - Daily retrospective generator

    Ouroboros v8.0: Episodic Memory (Session Summary)
    Compiles a daily retrospective of evolution and emotions.

    Usage:
      daily-summary
*)

open Llm_mcp.Common

let diary_dir = Filename.concat (Filename.concat me_root "knowledge") "diary"
let plans_dir = Filename.concat me_root "PLANS"
let emotion_file =
  Filename.concat (Filename.concat (Filename.concat me_root "lib") "state") "emotion.json"

(** List evolution plans *)
let list_plans () =
  if Sys.file_exists plans_dir && Sys.is_directory plans_dir then begin
    let files = Sys.readdir plans_dir in
    Array.to_list files
    |> List.filter (fun f ->
        String.length f > 10 && String.sub f 0 10 = "evolution_" &&
        String.length f > 3 && String.sub f (String.length f - 3) 3 = ".md")
    |> List.map (fun f -> Printf.sprintf "- [%s](PLANS/%s)" f f)
  end else
    []

(** Get mood summary from emotion.json *)
let get_mood_summary () =
  if Sys.file_exists emotion_file then
    try
      let ic = open_in emotion_file in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      let json = Yojson.Safe.from_string s in
      match json with
      | `Assoc fields ->
          let mood = match List.assoc_opt "current_mood" fields with
            | Some (`String m) -> m
            | _ -> "Unknown"
          in
          let energy = match List.assoc_opt "energy_level" fields with
            | Some (`Int e) -> string_of_int e
            | Some (`Float e) -> string_of_int (int_of_float e)
            | _ -> "?"
          in
          Printf.sprintf "**Final Mood**: %s (%s%%)" mood energy
      | _ -> "Neutral"
    with Yojson.Safe.Util.Type_error _ | Sys_error _ | Yojson.Json_error _ -> "Neutral"
  else
    "Neutral"

(** Generate summary *)
let generate_summary () =
  ensure_dir (Filename.concat me_root "knowledge");
  ensure_dir diary_dir;

  let date = date_str () in
  let report_file = Filename.concat diary_dir (Printf.sprintf "retrospective_%s.md" date) in

  let plans = list_plans () in
  let plans_text = if plans = [] then "None" else String.concat "\n" plans in
  let mood_summary = get_mood_summary () in

  let content = Printf.sprintf {|# 📔 Daily Retrospective: %s

## 🧬 Evolution Plans
%s

## 🧠 System State
%s

## 📝 Key Events
- Agentic/Graph/Multimodal RAG implemented.
- LNN & Hive Mind active.
- Ouroboros Singular loop running.
- v51.0 Token Miser pruning active.
|} date plans_text mood_summary in

  let oc = open_out report_file in
  output_string oc content;
  close_out oc;

  Printf.printf "✅ Retrospective saved to: %s\n" report_file

let () = generate_summary ()

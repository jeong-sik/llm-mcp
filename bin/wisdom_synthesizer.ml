(** wisdom_synthesizer - Knowledge fusion system

    Ouroboros v40.0: Wisdom Synthesizer
    Fuses fragmented knowledge into emergent high-level engineering wisdom.

    Usage:
      wisdom-synthesizer
*)

open Llm_mcp.Common

let wisdom_dir = Filename.concat (Filename.concat me_root "knowledge") "wisdom"
let research_dir = Filename.concat (Filename.concat me_root "knowledge") "research"
let curriculum_dir = Filename.concat (Filename.concat me_root "knowledge") "curriculum"

(** Extract title/tech from JSON file *)
let extract_seed path =
  try
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    let json = Yojson.Safe.from_string s in
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "tech" fields with
         | Some (`String t) -> Some t
         | _ ->
             (match List.assoc_opt "title" fields with
              | Some (`String t) -> Some t
              | _ -> None))
    | _ -> None
  with _ -> None

(** Gather knowledge seeds from research and curriculum directories *)
let gather_seeds () =
  let seeds = ref [] in

  (* Scan research directory *)
  if Sys.file_exists research_dir && Sys.is_directory research_dir then begin
    let files = Sys.readdir research_dir in
    Array.iter (fun f ->
      if String.length f > 10 && String.sub f 0 10 = "discovery_" then
        let path = Filename.concat research_dir f in
        match extract_seed path with
        | Some s -> seeds := s :: !seeds
        | None -> ()
    ) files
  end;

  (* Scan curriculum directory *)
  if Sys.file_exists curriculum_dir && Sys.is_directory curriculum_dir then begin
    let files = Sys.readdir curriculum_dir in
    Array.iter (fun f ->
      if String.length f > 11 && String.sub f 0 11 = "curriculum_" then
        let path = Filename.concat curriculum_dir f in
        match extract_seed path with
        | Some s -> seeds := s :: !seeds
        | None -> ()
    ) files
  end;

  List.rev !seeds

(** Get last word of a string *)
let last_word s =
  let words = String.split_on_char ' ' s in
  match List.rev words with
  | [] -> s
  | w :: _ -> w

(** Record wisdom to file *)
let record_wisdom ~title ~parents ~thesis ~impact =
  ensure_dir wisdom_dir;

  let ts = time_str () in
  let unix_ts = int_of_float (Unix.time ()) in
  let filename = Printf.sprintf "wisdom_%d.md" unix_ts in
  let filepath = Filename.concat wisdom_dir filename in

  let parent_a, parent_b = match parents with
    | a :: b :: _ -> (a, b)
    | a :: [] -> (a, "Unknown")
    | [] -> ("Unknown", "Unknown")
  in

  let content = Printf.sprintf {|# 🏮 Ouroboros Wisdom: %s

**Born from the union of:**
- %s
- %s

**Thesis**:
%s

**Strategic Impact**:
%s

---
*Synthesized by Ouroboros v40.0 on %s*
|} title parent_a parent_b thesis impact ts in

  let oc = open_out filepath in
  output_string oc content;
  close_out oc;

  Printf.printf "✨ [EUREKA] New wisdom synthesized: %s\n" filename

(** Main synthesis logic *)
let synthesize () =
  print_endline "🔮 [WISDOM] Accessing the Collective Unconscious of the Swarm...";

  let seeds = gather_seeds () in

  if List.length seeds < 2 then begin
    print_endline "💤 Not enough knowledge seeds to synthesize wisdom. Keep studying.";
    exit 0
  end;

  (* Pick last two seeds to fuse *)
  let rev_seeds = List.rev seeds in
  let seed_a = List.hd rev_seeds in
  let seed_b = List.nth rev_seeds 1 in

  Printf.printf "🧪 [FUSION] Merging '%s' with '%s'...\n" seed_a seed_b;

  (* Create emergent concept *)
  let emergent_concept = Printf.sprintf "The %s-%s Unified Framework"
    (last_word seed_a) (last_word seed_b) in
  let thesis = Printf.sprintf
    "An advanced architecture combining the probabilistic nature of %s with the structural integrity of %s."
    seed_a seed_b in

  record_wisdom
    ~title:emergent_concept
    ~parents:[seed_a; seed_b]
    ~thesis
    ~impact:"Revolutionary"

let () = synthesize ()

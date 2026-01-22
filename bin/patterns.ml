[@@@warning "-27-32-33-34-37-69"]

(** Pattern Extractor for MASC Session Hook *)

open Common
open Yojson.Safe.Util

(* STAR Pattern Structure *)
type star_pattern = {
  topic: string;
  situation: string;
  task: string;
  action: string;
  result: string;
  aha_moment: string option;
  quality_score: float;
}

(* Legacy Pattern Structure *)
type legacy_pattern = {
  topic: string;
  pattern: string;
  recommendation: string;
  warning: string;
}

type pattern_result = 
  | Star of star_pattern
  | Legacy of legacy_pattern
  | None_found

(* Extract content from retrospective markdown *)
let extract_star_pattern (content: string) : pattern_result = 
  None_found

let extract_legacy_pattern (content: string) : pattern_result = 
  try
    let lines = String.split_on_char '\n' content in
    let rec find_section acc in_section = function
      | [] -> List.rev acc
      | line :: rest ->
          if String.starts_with ~prefix:"## Procedural Pattern:" line then
            find_section [line] true rest
          else if in_section then
            if String.starts_with ~prefix:"## " line then List.rev acc
            else find_section (line :: acc) true rest
          else
            find_section acc false rest
    in
    let section_lines = find_section [] false lines in
    
    if section_lines = [] then None_found
    else
      let topic = 
        match section_lines with
        | hd :: _ -> 
            let prefix = "## Procedural Pattern:" in
            if String.length hd > String.length prefix then
              String.trim (String.sub hd (String.length prefix) (String.length hd - String.length prefix))
            else "Unknown"
        | [] -> "Unknown"
      in
      (* Extract fields *)
      let find_field prefix lines = 
        List.find_map (fun line ->
          if String.starts_with ~prefix line then
            Some (String.trim (String.sub line (String.length prefix) (String.length line - String.length prefix)))
          else None
        ) lines |> Option.value ~default:""
      in
      
      let pattern = find_field "**패턴**:" section_lines in
      let recommendation = find_field "**권장**:" section_lines in
      let warning = find_field "**경고**:" section_lines in
      
      if pattern = "" then None_found
      else Legacy { topic; pattern; recommendation; warning }
  with Yojson.Json_error _ -> None_found

(* Main processing logic *)
let process_patterns json_input = 
  let transcript_path = 
    try json_input |> member "transcript_path" |> to_string 
    with Type_error _ -> "" 
  in
  
  let context = ref [] in
  let warnings = ref [] in
  let patterns_found = ref 0 in
  
  if transcript_path = "" then
    warnings := "No transcript_path provided" :: !warnings
  else if not (Sys.file_exists transcript_path) then
    warnings := (Printf.sprintf "Transcript not found: %s" transcript_path) :: !warnings
  else begin
    try
        (* Read transcript file *)
        let content = 
          let ic = open_in transcript_path in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          s
        in
        
        (* Try extraction *)
        match extract_legacy_pattern content with
        | Legacy p -> 
            patterns_found := 1;
            context := (Printf.sprintf "Legacy pattern found: %s" p.topic) :: !context
        | _ -> ()
    with e ->
        warnings := (Printf.sprintf "Error reading transcript: %s" (Printexc.to_string e)) :: !warnings
  end;

  (* Construct Output JSON *)
  `Assoc [
    ("context", `List (List.map (fun s -> `String s) !context));
    ("warnings", `List (List.map (fun s -> `String s) !warnings));
    ("patterns_found", `Int !patterns_found) 
  ]

(* Entry point *)
let () = 
  (* Use me_root value to avoid unused module warning *)
  let _ = Common.me_root in
  
  let input_json = 
    try Yojson.Safe.from_channel stdin 
    with Yojson.Json_error _ -> `Assoc [] 
  in
  let result = process_patterns input_json in
  print_endline "--- JSON ---";
  print_endline (Yojson.Safe.pretty_to_string result)

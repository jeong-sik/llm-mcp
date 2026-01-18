(** retrospective_checker - Retrospective Status Checker

    OCaml port of features/hooks/lib/retrospective.py
    Checks if session retrospective exists (DD-sessions/*.md).

    Usage:
      retrospective_checker
*)

let completion_marker = "복명 완료"

(* Use Common module utilities *)
let list_md_files dir = Llm_mcp.Common.list_files_with_suffix dir ".md"
let read_file = Llm_mcp.Common.read_file_opt
let contains s sub = Llm_mcp.Common.contains ~substring:sub s
let get_date_parts () = (Llm_mcp.Common.month_dir (), Llm_mcp.Common.day_str ())

(** Check retrospective status *)
let check_retrospective () =
  let me_root = Llm_mcp.Common.me_root in
  let (month_dir, day) = get_date_parts () in
  let sessions_dir = Filename.concat
    (Filename.concat (Filename.concat me_root "claude") month_dir)
    (day ^ "-sessions")
  in

  let retro_files = list_md_files sessions_dir in
  let file_count = List.length retro_files in

  let exists = file_count > 0 in
  let context = ref [] in
  let warnings = ref [] in
  let has_marker = ref false in

  if exists then begin
    context := !context @ [Printf.sprintf "✅ **회고 %d개**: %s-sessions/" file_count day];

    (* Check for completion marker in any file *)
    List.iter (fun retro_file ->
      if not !has_marker then
        match read_file retro_file with
        | Some content when contains content completion_marker ->
            has_marker := true;
            context := !context @ [Printf.sprintf "🎸 **%s** found in %s" completion_marker (Filename.basename retro_file)]
        | _ -> ()
    ) retro_files
  end else begin
    context := !context @ ["📊 **Session 기록**: PostgreSQL session_logs에 자동 저장됨"];
    context := !context @ ["💡 상세 회고 필요시: /handoff 또는 DD-sessions/*.md 작성"]
  end;

  `Assoc [
    ("exists", `Bool exists);
    ("has_marker", `Bool !has_marker);
    ("context", `List (List.map (fun s -> `String s) !context));
    ("warnings", `List (List.map (fun s -> `String s) !warnings));
    ("path", if exists then `String sessions_dir else `Null);
  ]

(** Format output as human-readable text *)
let format_output = Llm_mcp.Common.format_result_output

(** Main function *)
let main () =
  let result = check_retrospective () in

  (* Print formatted output *)
  print_string (format_output result);

  (* Print JSON for programmatic use *)
  print_endline "--- JSON ---";
  print_endline (Yojson.Safe.pretty_to_string result);

  (* Exit code *)
  let open Yojson.Safe.Util in
  let has_marker = result |> member "has_marker" |> to_bool in
  let warnings = result |> member "warnings" |> to_list in
  if has_marker || List.length warnings = 0 then 0 else 1

let () = exit (main ())

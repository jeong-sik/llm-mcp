(** transcript_search.ml - Search Claude transcripts from local jsonl files
    OCaml replacement for scripts/conversation-search.py *)

open Cmdliner

let home_dir = try Unix.getenv "HOME" with Not_found -> "/tmp"

let transcripts_dir =
  Filename.concat home_dir ".claude/projects/-Users-dancer-me"

let find_recent_files base_dir days =
  let cutoff = Unix.time () -. (float_of_int days *. 86400.) in
  let rec walk dir acc =
    try
      let entries = Sys.readdir dir in
      Array.fold_left
        (fun acc entry ->
          let path = Filename.concat dir entry in
          try
            let stats = Unix.stat path in
            if stats.st_kind = Unix.S_DIR then walk path acc
            else if
              Filename.check_suffix entry ".jsonl"
              && stats.st_mtime > cutoff
            then (path, stats.st_mtime) :: acc
            else acc
          with Unix.Unix_error _ -> acc)
        acc entries
    with Sys_error _ -> acc
  in
  walk base_dir []
  |> List.sort (fun (_, m1) (_, m2) -> compare m2 m1)
  |> List.map fst

let extract_text json =
  let open Yojson.Safe.Util in
  try
    let content = json |> member "message" |> member "content" in
    match content with
    | `String s -> s
    | `List items ->
        items
        |> List.filter_map (fun item ->
               try Some (item |> member "text" |> to_string)
               with _ -> None)
        |> String.concat "\n"
    | _ -> ""
  with _ -> ""

let search_file filepath query role =
  let pattern = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote query) in
  let matches = ref [] in
  let ic = open_in filepath in
  let line_num = ref 0 in
  try
    while true do
      let line = input_line ic in
      incr line_num;
      try
        let json = Yojson.Safe.from_string line in
        let msg_type =
          try Yojson.Safe.Util.(json |> member "type" |> to_string)
          with _ -> ""
        in
        let should_check =
          match role with
          | "all" -> true
          | "user" -> msg_type = "human"
          | "assistant" -> msg_type = "assistant"
          | _ -> true
        in
        if should_check then (
          let text = extract_text json in
          if Re.execp pattern text then
            let snippet =
              let len = String.length text in
              if len > 200 then String.sub text 0 200 ^ "..." else text
            in
            let session = Filename.basename (Filename.dirname filepath) in
            matches :=
              (filepath, session, !line_num, msg_type, snippet) :: !matches)
      with _ -> ()
    done;
    !matches
  with End_of_file ->
    close_in ic;
    List.rev !matches

let run query days role limit =
  if not (Sys.file_exists transcripts_dir) then
    Printf.eprintf "❌ Transcript directory not found: %s\n" transcripts_dir
  else
    let files = find_recent_files transcripts_dir days in
    if files = [] then
      Printf.printf "📭 No transcripts found in last %d days\n" days
    else (
      Printf.printf "🔍 Searching '%s' in %d files (last %d days)...\n\n" query
        (List.length files) days;

      let all_matches = ref [] in
      List.iter
        (fun f ->
          if List.length !all_matches < limit then
            let matches = search_file f query role in
            all_matches := !all_matches @ matches)
        files;

      let matches = List.filteri (fun i _ -> i < limit) !all_matches in

      if matches = [] then
        Printf.printf "📭 No matches found for '%s'\n" query
      else (
        (* Group by session *)
        let by_session = Hashtbl.create 16 in
        List.iter
          (fun (_, session, _, msg_type, snippet) ->
            let existing =
              try Hashtbl.find by_session session with Not_found -> []
            in
            Hashtbl.replace by_session session
              ((msg_type, snippet) :: existing))
          matches;

        Hashtbl.iter
          (fun session items ->
            Printf.printf "📄 Session: %s...\n"
              (if String.length session > 12 then String.sub session 0 12
               else session);
            List.iter
              (fun (msg_type, snippet) ->
                let emoji = if msg_type = "human" then "👤" else "🤖" in
                let clean =
                  String.map (fun c -> if c = '\n' then ' ' else c) snippet
                in
                let short = if String.length clean > 150 then String.sub clean 0 150 ^ "..." else clean in
                Printf.printf "  %s %s\n" emoji short)
              (List.rev items);
            print_newline ())
          by_session;

        Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
        Printf.printf "✅ Found %d matches in %d sessions\n"
          (List.length matches)
          (Hashtbl.length by_session)))

let query_arg =
  let doc = "Search keyword" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

let days_arg =
  let doc = "Search range in days" in
  Arg.(value & opt int 30 & info [ "days"; "d" ] ~docv:"DAYS" ~doc)

let role_arg =
  let doc = "Filter by role: all, user, assistant" in
  Arg.(value & opt string "all" & info [ "role"; "r" ] ~docv:"ROLE" ~doc)

let limit_arg =
  let doc = "Maximum results" in
  Arg.(value & opt int 20 & info [ "limit"; "l" ] ~docv:"LIMIT" ~doc)

let cmd =
  let doc = "Search Claude conversation transcripts" in
  let info = Cmd.info "transcript-search" ~doc in
  Cmd.v info Term.(const run $ query_arg $ days_arg $ role_arg $ limit_arg)

let () = exit (Cmd.eval cmd)

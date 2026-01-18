(** previously_on - "Previously on Second Brain..." X-Files style session summary

    OCaml port of scripts/previously-on.py
    Queries Neo4j for recent sessions, pending PRs, and related memories.

    Usage:
      previously-on [--limit N] [--days N] [--style narrative|bullet]
*)

open Lwt.Syntax

(** Extract records from query result
    Records come as: {"records": [[[field1, field2]], [[field1, field2]]]}
    We need to unwrap to: [[field1, field2], [field1, field2]] *)
let extract_records result =
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "records" fields with
       | Some (`List records) ->
           (* Unwrap each record: [[fields]] -> [fields] *)
           List.filter_map (fun r ->
             match r with
             | `List [`List fields] -> Some (`List fields)
             | `List ((`List _) :: _ as inner) -> Some (`List inner)
             | _ -> Some r
           ) records
       | _ -> [])
  | _ -> []

(** Get string value from JSON *)
let json_string row idx =
  match row with
  | `List items ->
      (match List.nth_opt items idx with
       | Some (`String s) -> s
       | Some `Null -> ""
       | _ -> "")
  | _ -> ""

(** Convert days since 1970-01-01 to YYYY-MM-DD *)
let days_to_date days =
  (* Accurate calculation with leap years *)
  let is_leap year = (year mod 4 = 0 && year mod 100 <> 0) || (year mod 400 = 0) in
  let days_in_year year = if is_leap year then 366 else 365 in
  let days_in_month year month =
    match month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 -> 30
    | 2 -> if is_leap year then 29 else 28
    | _ -> 30
  in
  let rec find_year remaining year =
    let d = days_in_year year in
    if remaining < d then (year, remaining)
    else find_year (remaining - d) (year + 1)
  in
  let rec find_month remaining year month =
    let d = days_in_month year month in
    if remaining < d then (month, remaining + 1)  (* +1 because days are 1-indexed *)
    else find_month (remaining - d) year (month + 1)
  in
  let (year, remaining) = find_year days 1970 in
  let (month, day) = find_month remaining year 1 in
  Printf.sprintf "%d-%02d-%02d" year month day

(** Extract date from Neo4j Date structure (tag 68 = 0x44)
    Structure format: {_struct_tag: 68, fields: [days_since_epoch]}
    or directly from toString() which returns "YYYY-MM-DD" *)
let json_date row idx =
  match row with
  | `List items ->
      (match List.nth_opt items idx with
       | Some (`String s) -> s  (* Already a string like "2026-01-03" *)
       | Some (`Assoc fields) ->
           (* Neo4j Date structure: {_struct_tag: 68, fields: [days]} *)
           (match List.assoc_opt "_struct_tag" fields with
            | Some (`Int 68) ->  (* Date tag *)
                (match List.assoc_opt "fields" fields with
                 | Some (`List [`Int days]) -> days_to_date days
                 | _ -> "")
            | _ -> "")
       | Some `Null -> ""
       | _ -> "")
  | _ -> ""

(** Get int value from JSON *)
let json_int row idx =
  match row with
  | `List items ->
      (match List.nth_opt items idx with
       | Some (`Int n) -> n
       | Some (`Float f) -> int_of_float f
       | _ -> 0)
  | _ -> 0

(** Get list from JSON *)
let json_list row idx =
  match row with
  | `List items ->
      (match List.nth_opt items idx with
       | Some (`List l) -> l
       | _ -> [])
  | _ -> []

(** Format date string: 2025-12-05 -> 12/05 *)
let format_date_short date_str =
  try
    let parts = String.split_on_char '-' date_str in
    match parts with
    | [_; m; d] -> Printf.sprintf "%s/%s" m d
    | _ -> date_str
  with _ -> date_str

(** Take first n elements from list *)
let rec take n lst =
  match lst, n with
  | [], _ -> []
  | _, 0 -> []
  | x :: xs, n -> x :: take (n - 1) xs

(** Get recent sessions from Neo4j *)
let get_recent_sessions conn ~limit ~days =
  let cypher = Printf.sprintf {|
    MATCH (s:Session)
    WHERE s.date >= date() - duration({days: %d})
    OPTIONAL MATCH (s)-[:WORKED_ON]->(p:Project)
    OPTIONAL MATCH (s)-[:CREATED]->(pr:PullRequest)
    WITH s,
         collect(DISTINCT p.name) as projects,
         collect(DISTINCT {number: pr.number, repo: pr.repo}) as prs
    ORDER BY s.date DESC, s.updated_at DESC
    LIMIT %d
    RETURN s.session_id as session_id,
           s.date as date,
           s.mood as mood,
           projects,
           prs
  |} days limit in
  let* result = Neo4j_bolt.Bolt.query conn ~cypher () in
  match result with
  | Error _ -> Lwt.return []
  | Ok data -> Lwt.return (extract_records data)

(** Get pending PRs *)
let get_pending_prs conn =
  let cypher = {|
    MATCH (pr:PullRequest)
    WHERE pr.status = 'draft' OR pr.status = 'open'
    RETURN pr.number as number, pr.repo as repo, pr.status as status
    ORDER BY pr.number DESC
    LIMIT 5
  |} in
  let* result = Neo4j_bolt.Bolt.query conn ~cypher () in
  match result with
  | Error _ -> Lwt.return []
  | Ok data -> Lwt.return (extract_records data)

(** Format a single session as one-liner *)
let format_session_oneliner session idx =
  let date = json_date session 1 in
  let date_short = format_date_short date in
  let mood = json_string session 2 in
  let projects = json_list session 3
    |> List.filter_map (function `String s when s <> "" -> Some s | _ -> None) in
  let prs = json_list session 4
    |> List.filter_map (fun pr ->
        match pr with
        | `Assoc fields ->
            (match List.assoc_opt "number" fields with
             | Some (`Int num) when num > 0 -> Some (Printf.sprintf "#%d" num)
             | Some (`Float num) when num > 0.0 -> Some (Printf.sprintf "#%d" (int_of_float num))
             | _ -> None)
        | _ -> None) in

  let parts = ref [] in
  (match projects with
   | p :: _ -> parts := Printf.sprintf "**%s**" p :: !parts
   | [] -> ());
  (match prs with
   | _ :: _ -> parts := Printf.sprintf "PR %s" (String.concat "," (take 2 prs)) :: !parts
   | [] -> ());

  let summary = if !parts = [] then "세션 기록" else String.concat " | " (List.rev !parts) in
  let mood_suffix = if mood = "" then "" else Printf.sprintf " [%s]" mood in

  Printf.sprintf "   %d. [%s] %s%s" idx date_short summary mood_suffix

(** Format PR for display *)
let format_pr row =
  let num = json_int row 0 in
  let repo = json_string row 1 in
  let status = json_string row 2 in
  let repo_short =
    try
      let parts = String.split_on_char '/' repo in
      let last = List.nth_opt parts (List.length parts - 1) in
      Option.value ~default:"unknown" last
    with _ -> "unknown"
  in
  let repo_short = if String.length repo_short > 15 then String.sub repo_short 0 15 else repo_short in
  let emoji = if status = "draft" then "📝" else "🔄" in
  Printf.sprintf "      %s #%d (%s)" emoji num repo_short

(** Format narrative output *)
let format_narrative sessions pending_prs =
  if sessions = [] && pending_prs = [] then ""
  else begin
    let lines = ref [] in
    lines := "🎬 **Previously on Second Brain...**" :: !lines;
    lines := "" :: !lines;

    (* Recent sessions - up to 5 *)
    List.iteri (fun i session ->
      if i < 5 then
        lines := format_session_oneliner session (i + 1) :: !lines
    ) sessions;

    (* Pending PRs *)
    if pending_prs <> [] then begin
      lines := "" :: !lines;
      lines := "   📌 **대기 중 PR**:" :: !lines;
      List.iter (fun pr ->
        lines := format_pr pr :: !lines
      ) (take 3 pending_prs)
    end;

    String.concat "\n" (List.rev !lines)
  end

(** Format bullet output *)
let format_bullet sessions =
  if sessions = [] then ""
  else begin
    let lines = ref [] in
    lines := "🎬 **Previously on Second Brain...**" :: !lines;

    List.iteri (fun i session ->
      if i < 3 then begin
        let date = json_date session 1 in
        let mood = json_string session 2 in
        let mood_suffix = if mood = "" then "" else Printf.sprintf " [%s]" mood in
        let projects = json_list session 3
          |> List.filter_map (function `String s when s <> "" -> Some s | _ -> None) in
        let summary = match projects with
          | p :: _ -> Printf.sprintf "Project: %s" p
          | [] -> "Session recorded"
        in
        lines := Printf.sprintf "   %d. %s%s: %s" (i + 1) date mood_suffix summary :: !lines
      end
    ) sessions;

    String.concat "\n" (List.rev !lines)
  end

(** Main logic *)
let run ~limit ~days ~style =
  let* conn_result = Neo4j_bolt.Bolt.connect () in
  match conn_result with
  | Error e ->
      Printf.eprintf "⚠️ Neo4j 연결 실패: %s\n" (Neo4j_bolt.Bolt.error_to_string e);
      Lwt.return 0  (* Exit cleanly to not block session start *)
  | Ok conn ->
      Lwt.catch (fun () ->
        let* sessions = get_recent_sessions conn ~limit ~days in
        let* pending_prs = get_pending_prs conn in
        let* () = Neo4j_bolt.Bolt.close conn in

        let output = match style with
          | "bullet" -> format_bullet sessions
          | _ -> format_narrative sessions pending_prs
        in

        if output <> "" then print_endline output;
        Lwt.return 0
      ) (fun exn ->
        Printf.eprintf "⚠️ Previously on... 로딩 실패: %s\n" (Printexc.to_string exn);
        let* _ = Neo4j_bolt.Bolt.close conn in
        Lwt.return 0
      )

(** CLI entry point *)
let () =
  let limit = ref 5 in
  let days = ref 7 in
  let style = ref "narrative" in

  let specs = [
    ("--limit", Arg.Set_int limit, "Number of recent sessions (default: 5)");
    ("--days", Arg.Set_int days, "Look back N days (default: 7)");
    ("--style", Arg.Set_string style, "Output style: narrative or bullet (default: narrative)");
  ] in

  Arg.parse specs (fun _ -> ()) "previously-on [OPTIONS]";

  let code = Lwt_main.run (run ~limit:!limit ~days:!days ~style:!style) in
  exit code

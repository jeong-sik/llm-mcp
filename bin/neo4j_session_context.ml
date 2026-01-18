(** neo4j_session_context - Neo4j Session Context Loader

    OCaml port of scripts/neo4j-session-context.py
    Fetch recent learnings, decisions, and work stream context for session start.

    Usage:
      neo4j-session-context

    Output: Multi-line context with emoji prefixes
      🔄 WorkStream title (세션 N개)
      🎯 High-priority decision
      📋 PK-12345: JIRA summary
*)

open Lwt.Syntax

(** Truncate string with ellipsis *)
let truncate s max_len =
  if String.length s <= max_len then s
  else String.sub s 0 (max_len - 3) ^ "..."

(** Extract records from query result *)
let extract_records result =
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "records" fields with
       | Some (`List records) -> records
       | _ -> [])
  | _ -> []

(** Query 1: Active WorkStreams *)
let query_workstreams conn =
  let cypher = {|
    MATCH (ws:WorkStream)
    WHERE ws.status = 'in_progress'
    OPTIONAL MATCH (s:Session)-[:PART_OF]->(ws)
    WITH ws, count(s) as session_count
    ORDER BY ws.start_date DESC
    LIMIT 3
    RETURN ws.title as title, session_count as count
  |} in
  let* result = Neo4j_bolt.Bolt.query conn ~cypher () in
  match result with
  | Error _ -> Lwt.return []
  | Ok data ->
      let records = extract_records data in
      let items = List.filter_map (fun row ->
        match row with
        | `List [`String title; count_val] ->
            let cnt = match count_val with
              | `Int n -> n
              | `Float f -> int_of_float f
              | _ -> 0
            in
            let title' = truncate title 45 in
            Some (Printf.sprintf "🔄 %s (세션 %d개)" title' cnt)
        | _ -> None
      ) records in
      Lwt.return items

(** Query 2: High-priority decisions *)
let query_decisions conn =
  let cypher = {|
    MATCH (d:Decision)
    WHERE d.priority IN ['High', 'Critical']
      AND d.status IN ['Approved', 'In Progress']
    RETURN d.title as title
    ORDER BY d.title
    LIMIT 3
  |} in
  let* result = Neo4j_bolt.Bolt.query conn ~cypher () in
  match result with
  | Error _ -> Lwt.return []
  | Ok data ->
      let records = extract_records data in
      let items = List.filter_map (fun row ->
        match row with
        | `List [`String title] ->
            let title' = truncate title 50 in
            Some (Printf.sprintf "🎯 %s" title')
        | _ -> None
      ) records in
      Lwt.return items

(** Query 3: JIRA issues in progress *)
let query_jira_issues conn =
  let cypher = {|
    MATCH (j:JIRAIssue)
    WHERE j.status = 'In Progress'
      AND j.priority IN ['High', 'Highest']
    RETURN j.key as key, j.summary as summary
    ORDER BY j.key DESC
    LIMIT 3
  |} in
  let* result = Neo4j_bolt.Bolt.query conn ~cypher () in
  match result with
  | Error _ -> Lwt.return []
  | Ok data ->
      let records = extract_records data in
      let items = List.filter_map (fun row ->
        match row with
        | `List [`String key; `String summary] ->
            let summary' = truncate summary 35 in
            Some (Printf.sprintf "📋 %s: %s" key summary')
        | _ -> None
      ) records in
      Lwt.return items

(** Main: Get session context *)
let get_session_context () =
  let* conn_result = Neo4j_bolt.Bolt.connect () in
  match conn_result with
  | Error e ->
      Printf.eprintf "⚠️  Neo4j connect failed: %s\n" (Neo4j_bolt.Bolt.error_to_string e);
      Lwt.return ""
  | Ok conn ->
      let* workstreams = query_workstreams conn in
      let* decisions = query_decisions conn in
      let* jira = query_jira_issues conn in
      let* () = Neo4j_bolt.Bolt.close conn in
      let all = workstreams @ decisions @ jira in
      Lwt.return (String.concat "\n" all)

let () =
  let context = Lwt_main.run (get_session_context ()) in
  if context <> "" then print_endline context

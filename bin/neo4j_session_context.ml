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

(** Get Neo4j URI from environment *)
let get_neo4j_uri () =
  try Sys.getenv "NEO4J_URI"
  with Not_found ->
    try Sys.getenv "RAILWAY_NEO4J_URL"
    with Not_found -> "neo4j+s://your-neo4j-host.example.com:7687"

(** Get Neo4j config from environment *)
let get_config () =
  let uri = get_neo4j_uri () in
  let username = try Sys.getenv "NEO4J_USER" with Not_found -> "neo4j" in
  let password =
    try Sys.getenv "NEO4J_PASSWORD"
    with Not_found -> failwith "NEO4J_PASSWORD environment variable not set"
  in
  Neo4j_bolt_eio.Bolt.config_from_uri ~username ~password uri

(** Query 1: Active WorkStreams *)
let query_workstreams ~clock conn =
  let cypher = {|
    MATCH (ws:WorkStream)
    WHERE ws.status = 'in_progress'
    OPTIONAL MATCH (s:Session)-[:PART_OF]->(ws)
    WITH ws, count(s) as session_count
    ORDER BY ws.start_date DESC
    LIMIT 3
    RETURN ws.title as title, session_count as count
  |} in
  match Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params:(`Assoc []) () with
  | Error _ -> []
  | Ok data ->
      let records = extract_records data in
      List.filter_map (fun row ->
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
      ) records

(** Query 2: High-priority decisions *)
let query_decisions ~clock conn =
  let cypher = {|
    MATCH (d:Decision)
    WHERE d.priority IN ['High', 'Critical']
      AND d.status IN ['Approved', 'In Progress']
    RETURN d.title as title
    ORDER BY d.title
    LIMIT 3
  |} in
  match Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params:(`Assoc []) () with
  | Error _ -> []
  | Ok data ->
      let records = extract_records data in
      List.filter_map (fun row ->
        match row with
        | `List [`String title] ->
            let title' = truncate title 50 in
            Some (Printf.sprintf "🎯 %s" title')
        | _ -> None
      ) records

(** Query 3: JIRA issues in progress *)
let query_jira_issues ~clock conn =
  let cypher = {|
    MATCH (j:JIRAIssue)
    WHERE j.status = 'In Progress'
      AND j.priority IN ['High', 'Highest']
    RETURN j.key as key, j.summary as summary
    ORDER BY j.key DESC
    LIMIT 3
  |} in
  match Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params:(`Assoc []) () with
  | Error _ -> []
  | Ok data ->
      let records = extract_records data in
      List.filter_map (fun row ->
        match row with
        | `List [`String key; `String summary] ->
            let summary' = truncate summary 35 in
            Some (Printf.sprintf "📋 %s: %s" key summary')
        | _ -> None
      ) records

(** Main: Get session context *)
let get_session_context ~sw ~net ~clock =
  try
    let config = get_config () in
    match Neo4j_bolt_eio.Bolt.connect ~sw ~net ~clock ~config () with
    | Error e ->
        Printf.eprintf "⚠️  Neo4j connect failed: %s\n" (Neo4j_bolt_eio.Bolt.error_to_string e);
        ""
    | Ok conn ->
        let workstreams = query_workstreams ~clock conn in
        let decisions = query_decisions ~clock conn in
        let jira = query_jira_issues ~clock conn in
        Neo4j_bolt_eio.Bolt.close conn;
        let all = workstreams @ decisions @ jira in
        String.concat "\n" all
  with exn ->
    Printf.eprintf "⚠️  Error: %s\n" (Printexc.to_string exn);
    ""

let () =
  let context =
    Eio_main.run @@ fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    get_session_context ~sw ~net ~clock
  in
  if context <> "" then print_endline context

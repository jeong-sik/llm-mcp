(** neo4j_session - Neo4j Session Saver

    Save session data to Neo4j for "Previously on..." feature.
    Extracts session info from hook input and creates Session node with relationships.

    Usage:
      echo '{"session_id":"abc","transcript":"PK-12345 #1234"}' | neo4j-session

    Environment:
      NEO4J_URI - Connection URI (default: from env)
      NEO4J_PASSWORD - Password for neo4j user
*)

(** Extract string from JSON *)
let extract_string json key =
  match Yojson.Safe.Util.member key json with
  | `String s -> s
  | _ -> ""

(** Find all regex matches in text *)
let find_all_matches pattern text =
  let re = Re.Pcre.regexp pattern in
  let matches = Re.all re text in
  List.map (fun g -> Re.Group.get g 0) matches
  |> List.sort_uniq String.compare

(** Detect projects from transcript *)
let detect_projects transcript =
  let lower = String.lowercase_ascii transcript in
  let projects = ref [] in
  if String.length lower > 0 then begin
    if Re.execp (Re.Pcre.regexp ~flags:[`CASELESS] "kidsnote|PK-") transcript then
      projects := "Kidsnote" :: !projects;
    if Re.execp (Re.Pcre.regexp ~flags:[`CASELESS] "neo4j") transcript then
      projects := "Neo4j" :: !projects;
    if Re.execp (Re.Pcre.regexp ~flags:[`CASELESS] "agent-api") transcript then
      projects := "agent-api" :: !projects;
    if Re.execp (Re.Pcre.regexp ~flags:[`CASELESS] "second brain|/me/") transcript then
      projects := "Second Brain" :: !projects;
  end;
  List.rev !projects

(** Take first n items from list *)
let take n lst =
  let rec aux acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] n lst

(** Get Neo4j URI from environment *)
let get_neo4j_uri () =
  try Sys.getenv "NEO4J_URI"
  with Not_found ->
    try Sys.getenv "RAILWAY_NEO4J_URL"
    with Not_found -> "neo4j+s://turntable.proxy.rlwy.net:11490"

(** Get Neo4j config from environment *)
let get_config () =
  let uri = get_neo4j_uri () in
  let username = try Sys.getenv "NEO4J_USER" with Not_found -> "neo4j" in
  let password =
    try Sys.getenv "NEO4J_PASSWORD"
    with Not_found -> failwith "NEO4J_PASSWORD environment variable not set"
  in
  Neo4j_bolt_eio.Bolt.config_from_uri ~username ~password uri

(** Save session to Neo4j *)
let save_session ~sw ~net ~clock ~session_id ~transcript =
  (* Extract entities *)
  let prs = find_all_matches {|#(\d{3,5})|} transcript |> take 5 in
  let issues = find_all_matches {|(PK-\d{5})|} transcript |> take 5 in
  let projects = detect_projects transcript |> take 3 in

  let today =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02d" (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday
  in

  (* Connect to Neo4j *)
  let config = get_config () in
  match Neo4j_bolt_eio.Bolt.connect ~sw ~net ~clock ~config () with
  | Error e ->
      let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt_eio.Bolt.error_to_string e) in
      Error msg
  | Ok conn ->
      (* Create Session node *)
      let cypher = {|
        MERGE (s:Session {session_id: $session_id})
        SET s.date = date($date),
            s.updated_at = datetime(),
            s.source = 'session-end-hook-ocaml'
      |} in
      let params = `Assoc [
        ("session_id", `String session_id);
        ("date", `String today);
      ] in
      let _ = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params () in

      (* Create Project relationships *)
      List.iter (fun project ->
        let cypher = {|
          MATCH (s:Session {session_id: $session_id})
          MERGE (p:Project {name: $project})
          MERGE (s)-[:WORKED_ON]->(p)
        |} in
        let params = `Assoc [
          ("session_id", `String session_id);
          ("project", `String project);
        ] in
        let _ = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params () in
        ()
      ) projects;

      (* Create PR relationships *)
      List.iter (fun pr_num ->
        let pr_num_clean =
          if String.length pr_num > 0 && pr_num.[0] = '#' then
            String.sub pr_num 1 (String.length pr_num - 1)
          else pr_num
        in
        let cypher = {|
          MATCH (s:Session {session_id: $session_id})
          MERGE (pr:PullRequest {number: $pr_num})
          MERGE (s)-[:CREATED]->(pr)
        |} in
        let params = `Assoc [
          ("session_id", `String session_id);
          ("pr_num", `Int (int_of_string pr_num_clean));
        ] in
        let _ = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params () in
        ()
      ) (take 3 prs);

      (* Create JIRA relationships *)
      List.iter (fun issue_key ->
        let cypher = {|
          MATCH (s:Session {session_id: $session_id})
          MERGE (i:JiraIssue {key: $issue_key})
          MERGE (s)-[:WORKED_ON]->(i)
        |} in
        let params = `Assoc [
          ("session_id", `String session_id);
          ("issue_key", `String issue_key);
        ] in
        let _ = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params () in
        ()
      ) (take 3 issues);

      Neo4j_bolt_eio.Bolt.close conn;

      (* Build context output *)
      let context = ref [] in
      context := Printf.sprintf "Neo4j: Session %s... saved" (String.sub session_id 0 (min 8 (String.length session_id))) :: !context;
      if projects <> [] then
        context := Printf.sprintf "   Projects: %s" (String.concat ", " projects) :: !context;
      if prs <> [] then
        context := Printf.sprintf "   PRs: %s" (String.concat ", " prs) :: !context;

      Ok (List.rev !context)

(** Read all stdin *)
let read_stdin () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_channel buf stdin 4096
    done;
    Buffer.contents buf
  with End_of_file ->
    Buffer.contents buf

(** Output JSON result *)
let output_json context warnings =
  let context_json = `List (List.map (fun s -> `String s) context) in
  let warnings_json = `List (List.map (fun s -> `String s) warnings) in
  let result = `Assoc [
    ("context", context_json);
    ("warnings", warnings_json);
  ] in
  print_endline "--- JSON ---";
  print_endline (Yojson.Safe.to_string result)

(** Main *)
let main ~sw ~net ~clock =
  let input = read_stdin () in
  if String.trim input = "" then begin
    output_json [] ["No input data"];
    0
  end else
    try
      let json = Yojson.Safe.from_string input in
      let session_id = extract_string json "session_id" in

      if session_id = "" then begin
        output_json [] ["No session_id"];
        0
      end else begin
        let transcript = extract_string json "transcript" in
        let result = save_session ~sw ~net ~clock ~session_id ~transcript in
        match result with
        | Ok context ->
            output_json context [];
            0
        | Error msg ->
            output_json [] [msg];
            1
      end
    with
    | Yojson.Json_error msg ->
        output_json [] [Printf.sprintf "JSON parse error: %s" msg];
        1

let () =
  let exit_code =
    Eio_main.run @@ fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    main ~sw ~net ~clock
  in
  exit exit_code

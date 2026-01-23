(** subagent_logger - Neo4j SubAgent Completion Logger (Eio)

    Fast OCaml-based Neo4j logging for subagent completion.
    Creates Session/SubAgent nodes and RAN_SUBAGENT relationship.

    Usage:
      subagent-logger --session-id <id> --agent-id <id>

    Environment:
      NEO4J_URI - Connection URI (default: from env)
      NEO4J_PASSWORD - Password for neo4j user
*)

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

(** Log subagent completion to Neo4j *)
let log_subagent ~sw ~net ~clock ~session_id ~agent_id =
  let cypher = {|
    MERGE (s:Session {id: $session_id})
    MERGE (a:SubAgent {id: $agent_id})
    MERGE (s)-[r:RAN_SUBAGENT]->(a)
    SET r.completed_at = datetime()
  |} in

  let params = `Assoc [
    ("session_id", `String session_id);
    ("agent_id", `String agent_id);
  ] in

  try
    let config = get_config () in
    match Neo4j_bolt_eio.Bolt.connect ~sw ~net ~clock ~config () with
    | Error e ->
        Printf.eprintf "Connect failed: %s\n" (Neo4j_bolt_eio.Bolt.error_to_string e);
        1
    | Ok conn ->
        let query_result = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher ~params () in
        Neo4j_bolt_eio.Bolt.close conn;
        match query_result with
        | Ok _ ->
            (* Silent success - hook runs in background *)
            0
        | Error e ->
            Printf.eprintf "Query failed: %s\n" (Neo4j_bolt_eio.Bolt.error_to_string e);
            1
  with exn ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
    1

(** Main entry point with argument parsing *)
let main () =
  let session_id = ref "" in
  let agent_id = ref "" in

  let specs = [
    ("--session-id", Arg.Set_string session_id, "Session ID");
    ("--agent-id", Arg.Set_string agent_id, "Agent/SubAgent ID");
  ] in

  let usage = "subagent-logger --session-id <id> --agent-id <id>" in
  Arg.parse specs (fun _ -> ()) usage;

  if !session_id = "" || !agent_id = "" then begin
    Printf.eprintf "Error: Both --session-id and --agent-id are required\n";
    Printf.eprintf "Usage: %s\n" usage;
    1
  end else
    Eio_main.run @@ fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    log_subagent ~sw ~net ~clock ~session_id:!session_id ~agent_id:!agent_id

let () = exit (main ())

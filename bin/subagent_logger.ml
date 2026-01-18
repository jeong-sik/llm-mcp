(** subagent_logger - Neo4j SubAgent Completion Logger

    Fast OCaml-based Neo4j logging for subagent completion.
    Creates Session/SubAgent nodes and RAN_SUBAGENT relationship.

    Usage:
      subagent-logger --session-id <id> --agent-id <id>

    Environment:
      NEO4J_URI - Connection URI (default: from env)
      NEO4J_PASSWORD - Password for neo4j user
*)

open Lwt.Syntax

(** Log subagent completion to Neo4j *)
let log_subagent ~session_id ~agent_id =
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

  let* conn_result = Neo4j_bolt.Bolt.connect () in
  match conn_result with
  | Error e ->
      Printf.eprintf "Connect failed: %s\n" (Neo4j_bolt.Bolt.error_to_string e);
      Lwt.return 1
  | Ok conn ->
      let* query_result = Neo4j_bolt.Bolt.query conn ~cypher ~params () in
      let* () = Neo4j_bolt.Bolt.close conn in
      match query_result with
      | Ok _ ->
          (* Silent success - hook runs in background *)
          Lwt.return 0
      | Error e ->
          Printf.eprintf "Query failed: %s\n" (Neo4j_bolt.Bolt.error_to_string e);
          Lwt.return 1

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
    Lwt_main.run (log_subagent ~session_id:!session_id ~agent_id:!agent_id)

let () = exit (main ())

(** neo4j_health - Neo4j Health Check Utility

    Fast OCaml-based Neo4j connectivity check.
    Returns exit code 0 if healthy, 1 if not.

    Usage:
      neo4j-health            # Quick connectivity check
      neo4j-health --json     # JSON output with latency

    Environment:
      NEO4J_URI - Connection URI (default: neo4j+s://turntable.proxy.rlwy.net:11490)
      NEO4J_PASSWORD - Password for neo4j user
*)

open Lwt.Syntax

(** Get Neo4j URI from environment for display *)
let get_neo4j_uri () =
  try Sys.getenv "NEO4J_URI"
  with Not_found ->
    try Sys.getenv "RAILWAY_NEO4J_URL"
    with Not_found -> "neo4j+s://turntable.proxy.rlwy.net:11490"

(** Check Neo4j connectivity *)
let check_health ~json_output =
  let uri = get_neo4j_uri () in
  let start_time = Unix.gettimeofday () in

  let* result =
    Lwt.catch
      (fun () ->
        (* Connect using environment variables (NEO4J_URI, NEO4J_PASSWORD) *)
        let* conn_result = Neo4j_bolt.Bolt.connect () in
        match conn_result with
        | Error e ->
            Lwt.return (Error (Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e)))
        | Ok conn ->
            (* Simple ping query *)
            let* query_result = Neo4j_bolt.Bolt.query conn ~cypher:"RETURN 1 as ping" ~params:(`Assoc []) () in
            let* () = Neo4j_bolt.Bolt.close conn in
            match query_result with
            | Ok _ -> Lwt.return (Ok ())
            | Error e -> Lwt.return (Error (Printf.sprintf "Query failed: %s" (Neo4j_bolt.Bolt.error_to_string e))))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
  in

  let elapsed_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in

  match result with
  | Ok () ->
      if json_output then begin
        let json = `Assoc [
          ("status", `String "healthy");
          ("latency_ms", `Float elapsed_ms);
          ("uri", `String uri);
        ] in
        print_endline (Yojson.Safe.to_string json)
      end else
        Printf.printf "✅ Neo4j healthy (%.0fms)\n" elapsed_ms;
      Lwt.return 0
  | Error msg ->
      if json_output then begin
        let json = `Assoc [
          ("status", `String "unhealthy");
          ("error", `String msg);
          ("uri", `String uri);
        ] in
        print_endline (Yojson.Safe.to_string json)
      end else
        Printf.eprintf "❌ Neo4j unhealthy: %s\n" msg;
      Lwt.return 1

(** Main entry point *)
let main () =
  let json_output = Array.length Sys.argv > 1 && Sys.argv.(1) = "--json" in
  Lwt_main.run (check_health ~json_output)

let () = exit (main ())

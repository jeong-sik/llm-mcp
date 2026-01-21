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

(** Get Neo4j URI from environment for display *)
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

(** Check Neo4j connectivity *)
let check_health ~sw ~net ~clock ~json_output =
  let uri = get_neo4j_uri () in
  let start_time = Unix.gettimeofday () in

  let result =
    try
      let config = get_config () in
      match Neo4j_bolt_eio.Bolt.connect ~sw ~net ~clock ~config () with
      | Error e ->
          Error (Printf.sprintf "Connect failed: %s" (Neo4j_bolt_eio.Bolt.error_to_string e))
      | Ok conn ->
          (* Simple ping query *)
          let query_result = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher:"RETURN 1 as ping" ~params:(`Assoc []) () in
          Neo4j_bolt_eio.Bolt.close conn;
          match query_result with
          | Ok _ -> Ok ()
          | Error e -> Error (Printf.sprintf "Query failed: %s" (Neo4j_bolt_eio.Bolt.error_to_string e))
    with exn -> Error (Printexc.to_string exn)
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
        Printf.printf "Neo4j healthy (%.0fms)\n" elapsed_ms;
      0
  | Error msg ->
      if json_output then begin
        let json = `Assoc [
          ("status", `String "unhealthy");
          ("error", `String msg);
          ("uri", `String uri);
        ] in
        print_endline (Yojson.Safe.to_string json)
      end else
        Printf.eprintf "Neo4j unhealthy: %s\n" msg;
      1

(** Main entry point *)
let () =
  let json_output = Array.length Sys.argv > 1 && Sys.argv.(1) = "--json" in
  let exit_code =
    Eio_main.run @@ fun env ->
    Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    check_health ~sw ~net ~clock ~json_output
  in
  exit exit_code

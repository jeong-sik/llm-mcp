(** pg_health - PostgreSQL Health Check Utility (Caqti + Eio)

    Fast OCaml-based PostgreSQL connectivity check.
    Returns exit code 0 if healthy, 1 if not.

    Usage:
      pg-health            # Quick connectivity check
      pg-health --json     # JSON output with latency

    Environment:
      SUPABASE_DB_URL or SB_PG_URL - PostgreSQL connection URL (postgres://user:pass@host:port/db)
*)

(** Get PostgreSQL URL from environment *)
let get_pg_url () =
  match Sys.getenv_opt "SUPABASE_DB_URL" with
  | Some url -> url
  | None ->
      match Sys.getenv_opt "SB_PG_URL" with
      | Some url -> url
      | None -> failwith "SUPABASE_DB_URL (or SB_PG_URL) environment variable not set"

(** Parse URL to get host for display (hide password) *)
let safe_url url =
  (* postgres://user:pass@host:port/db -> postgres://***@host:port/db *)
  try
    let at_pos = String.rindex url '@' in
    let prefix_end = String.index url '/' + 2 in  (* after :// *)
    String.sub url 0 prefix_end ^ "***" ^ String.sub url at_pos (String.length url - at_pos)
  with _ -> "postgres://***@..."

(** Check PostgreSQL connectivity using Caqti *)
let check_health ~sw ~stdenv ~json_output =
  let url = get_pg_url () in
  let start_time = Unix.gettimeofday () in

  let result =
    try
      let uri = Uri.of_string url in
      match Caqti_eio_unix.connect ~sw ~stdenv uri with
      | Error err ->
          Error (Printf.sprintf "Connect failed: %s" (Caqti_error.show err))
      | Ok (module Db : Caqti_eio.CONNECTION) ->
          (* Simple ping query *)
          let open Caqti_request.Infix in
          let ping_query = (Caqti_type.unit ->! Caqti_type.int) "SELECT 1" in
          match Db.find ping_query () with
          | Ok 1 -> Ok ()
          | Ok _ -> Error "Unexpected result from ping query"
          | Error err -> Error (Printf.sprintf "Query failed: %s" (Caqti_error.show err))
    with exn -> Error (Printexc.to_string exn)
  in

  let end_time = Unix.gettimeofday () in
  let latency_ms = (end_time -. start_time) *. 1000.0 in

  match result with
  | Ok () ->
      if json_output then
        Printf.printf {|{"status":"healthy","latency_ms":%.2f,"url":"%s"}|} latency_ms (safe_url url)
      else begin
        Printf.printf "✅ PostgreSQL is healthy\n";
        Printf.printf "   URL: %s\n" (safe_url url);
        Printf.printf "   Latency: %.2fms\n" latency_ms
      end;
      0
  | Error msg ->
      if json_output then
        Printf.printf {|{"status":"unhealthy","error":"%s","url":"%s"}|}
          (String.escaped msg) (safe_url url)
      else begin
        Printf.printf "❌ PostgreSQL is unhealthy\n";
        Printf.printf "   URL: %s\n" (safe_url url);
        Printf.printf "   Error: %s\n" msg
      end;
      1

let () =
  let json_output = Array.length Sys.argv > 1 && Sys.argv.(1) = "--json" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let exit_code = check_health ~sw ~stdenv ~json_output in
  exit exit_code

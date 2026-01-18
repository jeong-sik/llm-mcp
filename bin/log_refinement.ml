(** log_refinement - Query Refinement Logger

    OCaml port of .claude/hooks/log-refinement.py
    Logs query refinements to PostgreSQL for few-shot learning.

    Usage:
      log_refinement --original <text> --refined <text> [--confidence <int>] [--context <json>]
                     [--user-choice <str>] [--session-id <id>] [--get-recent <n>]
*)

open Cmdliner

(** Get ME_ROOT from Common module *)
let me_root () =
  match Sys.getenv_opt "ME_ROOT" with
  | Some path -> path
  | None ->
      match Sys.getenv_opt "HOME" with
      | Some home -> Filename.concat home "me"
      | None -> "/tmp/me"

(** Get PostgreSQL URL from environment *)
let pg_url () =
  Sys.getenv_opt "RAILWAY_PG_URL"

(** Get current timestamp in ISO format *)
let iso_timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.000Z"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(** Get today's date string YYYY-MM-DD *)
let today_str () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

(** Build refinement type from parameters *)
let build_refinement_type ~confidence ~user_choice ~needs_refinement =
  match needs_refinement with
  | Some true -> "needed"
  | Some false -> "not_needed"
  | None ->
      if confidence < 65 then "low_confidence"
      else match user_choice with
        | Some choice -> "user_choice:" ^ choice
        | None -> "unknown"

(** Insert refinement into PostgreSQL *)
let insert_refinement ~original ~refined ~refinement_type ~session_id =
  let open Lwt.Syntax in
  match pg_url () with
  | None ->
      prerr_endline "[log-refinement] RAILWAY_PG_URL not set";
      Lwt.return false
  | Some url ->
      let uri = Uri.of_string url in
      let* conn_result = Caqti_lwt_unix.connect uri in
      match conn_result with
      | Error err ->
          prerr_endline (Printf.sprintf "[log-refinement] DB error: %s" (Caqti_error.show err));
          Lwt.return false
      | Ok (module Db : Caqti_lwt.CONNECTION) ->
          let open Caqti_request.Infix in
          let open Caqti_type.Std in
          let insert_query = (t4 string string string (option string) ->. unit) ~oneshot:true
            {|INSERT INTO telemetry.refinements
              (timestamp, original_query, refined_query, refinement_type, session_id)
              VALUES (NOW(), $1, $2, $3, $4)|}
          in
          let* result = Db.exec insert_query (original, refined, refinement_type, session_id) in
          let* () = Db.disconnect () in
          match result with
          | Ok () -> Lwt.return true
          | Error err ->
              prerr_endline (Printf.sprintf "[log-refinement] Insert error: %s" (Caqti_error.show err));
              Lwt.return false

(** Get recent refinements from PostgreSQL *)
let get_recent_refinements n =
  let open Lwt.Syntax in
  match pg_url () with
  | None ->
      prerr_endline "[log-refinement] RAILWAY_PG_URL not set";
      Lwt.return []
  | Some url ->
      let uri = Uri.of_string url in
      let* conn_result = Caqti_lwt_unix.connect uri in
      match conn_result with
      | Error err ->
          prerr_endline (Printf.sprintf "[log-refinement] DB error: %s" (Caqti_error.show err));
          Lwt.return []
      | Ok (module Db : Caqti_lwt.CONNECTION) ->
          let open Caqti_request.Infix in
          let open Caqti_type.Std in
          (* Return (original, refined, type, timestamp) tuples *)
          let select_query = (int ->* t4 string string string (option string)) ~oneshot:true
            {|SELECT original_query, refined_query, refinement_type,
                     TO_CHAR(timestamp, 'YYYY-MM-DD"T"HH24:MI:SS"Z"') as ts
              FROM telemetry.refinements
              WHERE refinement_type LIKE 'user_choice:%'
              ORDER BY timestamp DESC
              LIMIT $1|}
          in
          let* result = Db.collect_list select_query n in
          let* () = Db.disconnect () in
          match result with
          | Ok rows -> Lwt.return rows
          | Error err ->
              prerr_endline (Printf.sprintf "[log-refinement] Query error: %s" (Caqti_error.show err));
              Lwt.return []

(** JSONL fallback for backwards compatibility *)
let fallback_to_jsonl ~original ~refined ~confidence ~ambiguity_score
    ~needs_refinement ~context_json:_ ~user_choice ~session_id =
  let log_dir = Filename.concat (me_root ()) ".claude/refinement-logs" in
  (* Create directory if needed *)
  (try Unix.mkdir log_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  let log_file = Filename.concat log_dir (Printf.sprintf "refinements-%s.jsonl" (today_str ())) in

  let entry = Printf.sprintf
    {|{"timestamp":"%s","original_query":%s,"refined_query":%s,"confidence":%d,"ambiguity_score":%d,"needs_refinement":%s,"context_summary":{},"user_choice":%s,"session_id":%s}|}
    (iso_timestamp ())
    (Yojson.Safe.to_string (`String original))
    (Yojson.Safe.to_string (`String refined))
    confidence
    ambiguity_score
    (if needs_refinement then "true" else "false")
    (match user_choice with Some c -> Yojson.Safe.to_string (`String c) | None -> "null")
    (match session_id with Some s -> Yojson.Safe.to_string (`String s) | None -> "null")
  in

  let oc = open_out_gen [Open_append; Open_creat] 0o644 log_file in
  output_string oc (entry ^ "\n");
  close_out oc;
  true

(** Log a refinement decision *)
let log_refinement ~original ~refined ~confidence ~context_json:_
    ~user_choice ~ambiguity_score ~needs_refinement ~session_id =
  let refinement_type = build_refinement_type ~confidence ~user_choice ~needs_refinement in
  let session = match session_id with
    | Some s -> Some s
    | None -> Sys.getenv_opt "CLAUDE_SESSION_ID"
  in

  (* Try PostgreSQL first *)
  let pg_success = Lwt_main.run (
    insert_refinement ~original ~refined ~refinement_type ~session_id:session
  ) in

  if pg_success then true
  else begin
    (* Fallback to JSONL *)
    let amb_score = match ambiguity_score with
      | Some s -> s
      | None -> 100 - confidence
    in
    let needs_ref = match needs_refinement with
      | Some b -> b
      | None -> confidence < 65
    in
    fallback_to_jsonl ~original ~refined ~confidence ~ambiguity_score:amb_score
      ~needs_refinement:needs_ref ~context_json:() ~user_choice ~session_id:session
  end

(** Format recent refinements as JSON *)
let format_recent_as_json rows =
  let items = List.map (fun (original, refined, rtype, ts) ->
    let user_choice =
      if String.length rtype > 12 && String.sub rtype 0 12 = "user_choice:" then
        String.sub rtype 12 (String.length rtype - 12)
      else ""
    in
    `Assoc [
      ("original_query", `String original);
      ("refined_query", `String refined);
      ("user_choice", `String user_choice);
      ("timestamp", match ts with Some t -> `String t | None -> `Null);
    ]
  ) rows in
  Yojson.Safe.pretty_to_string (`List items)

(** CLI argument definitions *)
let original_arg =
  let doc = "Original user query" in
  Arg.(value & opt (some string) None & info ["original"] ~doc)

let refined_arg =
  let doc = "Refined query suggestion" in
  Arg.(value & opt (some string) None & info ["refined"] ~doc)

let confidence_arg =
  let doc = "Confidence score (0-100)" in
  Arg.(value & opt int 50 & info ["confidence"] ~doc)

let context_arg =
  let doc = "Query context as JSON" in
  Arg.(value & opt string "{}" & info ["context"] ~doc)

let user_choice_arg =
  let doc = "User choice (if answered)" in
  Arg.(value & opt (some string) None & info ["user-choice"] ~doc)

let ambiguity_arg =
  let doc = "Ambiguity score (0-100)" in
  Arg.(value & opt (some int) None & info ["ambiguity-score"] ~doc)

let needs_refinement_arg =
  let doc = "Whether refinement was needed (true/false)" in
  Arg.(value & opt (some string) None & info ["needs-refinement"] ~doc)

let session_id_arg =
  let doc = "Session ID for tracking" in
  Arg.(value & opt (some string) None & info ["session-id"] ~doc)

let get_recent_arg =
  let doc = "Get N recent refinements" in
  Arg.(value & opt (some int) None & info ["get-recent"] ~doc)

let main original refined confidence context_json user_choice
    ambiguity_score needs_refinement_str session_id get_recent =
  match get_recent with
  | Some n ->
      (* Get recent refinements mode *)
      let rows = Lwt_main.run (get_recent_refinements n) in
      print_endline (format_recent_as_json rows)
  | None ->
      (* Log refinement mode *)
      match original, refined with
      | Some orig, Some ref ->
          let needs_ref = match needs_refinement_str with
            | Some "true" -> Some true
            | Some "false" -> Some false
            | _ -> None
          in
          let success = log_refinement
            ~original:orig
            ~refined:ref
            ~confidence
            ~context_json
            ~user_choice
            ~ambiguity_score
            ~needs_refinement:needs_ref
            ~session_id
          in
          print_endline (Printf.sprintf {|{"status":"%s"}|} (if success then "logged" else "failed"))
      | _ ->
          prerr_endline "Error: --original and --refined are required";
          exit 1

let cmd =
  let doc = "Log query refinement decisions" in
  let info = Cmd.info "log_refinement" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const main
    $ original_arg $ refined_arg $ confidence_arg $ context_arg
    $ user_choice_arg $ ambiguity_arg $ needs_refinement_arg
    $ session_id_arg $ get_recent_arg)

let () = ignore (Cmd.eval cmd)

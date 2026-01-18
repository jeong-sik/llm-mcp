(** session_logger - PostgreSQL Session Logger (OCaml)

    Fast CLI for session_logs table operations.
    Uses psql for simplicity and fast cold start.

    Usage:
      session-logger start <session_id> [segment]
      session-logger end <session_id> [summary]
*)

(** Parse PostgreSQL URL into psql connection args *)
let parse_pg_url url =
  (* postgresql://user:pass@host:port/db *)
  let url =
    if String.length url > 14 && String.sub url 0 13 = "postgresql://" then
      String.sub url 13 (String.length url - 13)
    else if String.length url > 11 && String.sub url 0 11 = "postgres://" then
      String.sub url 11 (String.length url - 11)
    else url
  in
  try
    let at_pos = String.index url '@' in
    let creds = String.sub url 0 at_pos in
    let rest = String.sub url (at_pos + 1) (String.length url - at_pos - 1) in

    let colon_pos = String.index creds ':' in
    let user = String.sub creds 0 colon_pos in
    let password = String.sub creds (colon_pos + 1) (String.length creds - colon_pos - 1) in

    let slash_pos = String.index rest '/' in
    let host_port = String.sub rest 0 slash_pos in
    let database = String.sub rest (slash_pos + 1) (String.length rest - slash_pos - 1) in

    let colon_pos2 = String.index host_port ':' in
    let host = String.sub host_port 0 colon_pos2 in
    let port = String.sub host_port (colon_pos2 + 1) (String.length host_port - colon_pos2 - 1) in

    Some (host, port, user, password, database)
  with Not_found -> None

(** Run psql command and return success *)
let run_psql host port user password database sql =
  let cmd = Printf.sprintf
    "PGPASSWORD=%s psql -h %s -p %s -U %s -d %s -c %s -t -q 2>/dev/null"
    (Filename.quote password)
    (Filename.quote host)
    (Filename.quote port)
    (Filename.quote user)
    (Filename.quote database)
    (Filename.quote sql)
  in
  let ic = Unix.open_process_in cmd in
  let output =
    try
      let buf = Buffer.create 256 in
      (try while true do Buffer.add_string buf (input_line ic); Buffer.add_char buf '\n' done
       with End_of_file -> ());
      String.trim (Buffer.contents buf)
    with _ -> ""
  in
  let status = Unix.close_process_in ic in
  (status = Unix.WEXITED 0, output)

(** Get ISO timestamp *)
let iso_now () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(** Get today's date *)
let today_date () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

(** Escape single quotes in SQL *)
let sql_escape s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    if c = '\'' then Buffer.add_string buf "''"
    else Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Log session start *)
let log_start host port user password database session_id segment =
  let now = iso_now () in
  let today = today_date () in
  let sql = Printf.sprintf
    "INSERT INTO session_logs (session_id, date, start_time, segment_number, metadata) \
     VALUES ('%s', '%s', '%s', %d, '{}') RETURNING id"
    (sql_escape session_id) today now segment
  in
  let success, output = run_psql host port user password database sql in
  if success then begin
    let log_id = String.trim output in
    Printf.printf "✅ Session started: %s (log_id=%s)\n" session_id log_id;
    0
  end else begin
    Printf.eprintf "❌ Failed to start session: %s\n" session_id;
    1
  end

(** Log session end *)
let log_end host port user password database session_id summary =
  let now = iso_now () in
  let summary_sql = match summary with
    | Some s -> Printf.sprintf ", summary = '%s'" (sql_escape s)
    | None -> ""
  in
  let sql = Printf.sprintf
    "UPDATE session_logs SET end_time = '%s', updated_at = NOW()%s \
     WHERE session_id = '%s' AND end_time IS NULL"
    now summary_sql (sql_escape session_id)
  in
  let success, _ = run_psql host port user password database sql in
  if success then begin
    Printf.printf "✅ Session ended: %s\n" session_id;
    0
  end else begin
    Printf.eprintf "❌ Failed to end session: %s\n" session_id;
    1
  end

(** Main *)
let main () =
  let pg_url = try Sys.getenv "RAILWAY_PG_URL" with Not_found -> "" in

  if String.length pg_url = 0 then begin
    prerr_endline "❌ RAILWAY_PG_URL not set";
    exit 1
  end;

  match parse_pg_url pg_url with
  | None ->
      prerr_endline "❌ Invalid PostgreSQL URL format";
      exit 1
  | Some (host, port, user, password, database) ->
      let args = Array.to_list Sys.argv in
      match args with
      | _ :: "start" :: session_id :: rest ->
          let segment = match rest with
            | s :: _ -> (try int_of_string s with _ -> 1)
            | [] -> 1
          in
          exit (log_start host port user password database session_id segment)

      | _ :: "end" :: session_id :: rest ->
          let summary = match rest with
            | s :: _ -> Some s
            | [] -> None
          in
          exit (log_end host port user password database session_id summary)

      | _ ->
          print_endline "Usage: session-logger <command> [args]";
          print_endline "Commands:";
          print_endline "  start <session_id> [segment]  - Log session start";
          print_endline "  end <session_id> [summary]    - Log session end";
          exit 0

let () = main ()

(** doctor - llm-mcp Pre-flight Health Check

    Comprehensive system health check before running chains or tests.

    Usage:
      llm-mcp-doctor           # Full check
      llm-mcp-doctor --json    # JSON output
      llm-mcp-doctor --quick   # Server + backends only

    Checks:
      1. Server health (/health endpoint)
      2. Backends (Ollama, Claude CLI, Gemini, Codex)
      3. Neo4j connectivity
      4. Environment variables
      5. Chain compiler
*)

open Lwt.Syntax

(** ANSI colors *)
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let red s = Printf.sprintf "\027[31m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s

(** Check result type *)
type check_result = {
  name: string;
  status: [`Ok | `Warn | `Fail];
  message: string;
  latency_ms: float option;
}

(** Run shell command and capture output *)
let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let output = try input_line ic with End_of_file -> "" in
  let _ = Unix.close_process_in ic in
  output

(** Check if command exists *)
let command_exists cmd =
  let exit_code = Sys.command (Printf.sprintf "which %s > /dev/null 2>&1" cmd) in
  exit_code = 0

(** Get environment variable with default *)
let get_env_opt key =
  try Some (Sys.getenv key) with Not_found -> None

(** 1. Check server health *)
let check_server () =
  let start = Unix.gettimeofday () in
  let output = run_command "curl -s http://127.0.0.1:8932/health 2>/dev/null" in
  let latency = (Unix.gettimeofday () -. start) *. 1000.0 in
  if String.length output > 0 && String.sub output 0 1 = "{" then
    { name = "Server"; status = `Ok;
      message = "llm-mcp running"; latency_ms = Some latency }
  else
    { name = "Server"; status = `Fail;
      message = "Not running (start with: ./start-llm-mcp.sh)"; latency_ms = None }

(** 2. Check Ollama backend *)
let check_ollama () =
  if not (command_exists "ollama") then
    { name = "Ollama"; status = `Fail; message = "Not installed"; latency_ms = None }
  else
    let output = run_command "curl -s http://127.0.0.1:11434/api/tags 2>/dev/null | head -c 100" in
    if String.length output > 0 && String.sub output 0 1 = "{" then
      let models = run_command "ollama list 2>/dev/null | tail -n +2 | wc -l | tr -d ' '" in
      { name = "Ollama"; status = `Ok;
        message = Printf.sprintf "%s models loaded" (String.trim models); latency_ms = None }
    else
      { name = "Ollama"; status = `Warn;
        message = "Installed but not running"; latency_ms = None }

(** 3. Check Claude CLI *)
let check_claude_cli () =
  if command_exists "claude" then
    { name = "Claude CLI"; status = `Ok; message = "Available"; latency_ms = None }
  else
    { name = "Claude CLI"; status = `Warn;
      message = "Not found (npm i -g @anthropic-ai/claude-code)"; latency_ms = None }

(** 4. Check Gemini *)
let check_gemini () =
  match get_env_opt "GOOGLE_AI_API_KEY", get_env_opt "GEMINI_API_KEY" with
  | Some _, _ | _, Some _ ->
    { name = "Gemini"; status = `Ok; message = "API key set"; latency_ms = None }
  | None, None ->
    { name = "Gemini"; status = `Warn;
      message = "No API key (GOOGLE_AI_API_KEY or GEMINI_API_KEY)"; latency_ms = None }

(** 5. Check Codex *)
let check_codex () =
  if command_exists "codex" then
    { name = "Codex"; status = `Ok; message = "Available"; latency_ms = None }
  else
    { name = "Codex"; status = `Warn;
      message = "Not found (npm i -g @openai/codex)"; latency_ms = None }

(** 6. Check Neo4j connectivity *)
let check_neo4j () =
  let start_time = Unix.gettimeofday () in
  Lwt_main.run (
    let* result =
      Lwt.catch
        (fun () ->
          let* conn_result = Neo4j_bolt.Bolt.connect () in
          match conn_result with
          | Error _ -> Lwt.return (Error "Connection failed")
          | Ok conn ->
              let* query_result = Neo4j_bolt.Bolt.query conn
                ~cypher:"RETURN 1 as ping" ~params:(`Assoc []) () in
              let* () = Neo4j_bolt.Bolt.close conn in
              match query_result with
              | Ok _ -> Lwt.return (Ok ())
              | Error _ -> Lwt.return (Error "Query failed"))
        (fun _ -> Lwt.return (Error "Exception"))
    in
    let latency = (Unix.gettimeofday () -. start_time) *. 1000.0 in
    match result with
    | Ok () ->
      Lwt.return { name = "Neo4j"; status = `Ok;
                   message = "Connected"; latency_ms = Some latency }
    | Error msg ->
      Lwt.return { name = "Neo4j"; status = `Fail;
                   message = msg; latency_ms = None }
  )

(** 7. Check required environment variables *)
let check_env_vars () =
  let required = [
    ("NEO4J_PASSWORD", get_env_opt "NEO4J_PASSWORD");
    ("ANTHROPIC_API_KEY", get_env_opt "ANTHROPIC_API_KEY");
  ] in
  let missing = List.filter (fun (_, v) -> v = None) required
                |> List.map fst in
  if missing = [] then
    { name = "Env vars"; status = `Ok; message = "All set"; latency_ms = None }
  else
    { name = "Env vars"; status = `Warn;
      message = Printf.sprintf "Missing: %s" (String.concat ", " missing);
      latency_ms = None }

(** Print single check result *)
let print_result r =
  let icon = match r.status with
    | `Ok -> green "✓"
    | `Warn -> yellow "⚠"
    | `Fail -> red "✗"
  in
  let latency_str = match r.latency_ms with
    | Some ms -> dim (Printf.sprintf " (%.0fms)" ms)
    | None -> ""
  in
  Printf.printf "  %s %-12s %s%s\n" icon r.name r.message latency_str

(** Print results as JSON *)
let print_json results =
  let json = `Assoc [
    ("checks", `List (List.map (fun r ->
      `Assoc [
        ("name", `String r.name);
        ("status", `String (match r.status with
          | `Ok -> "ok" | `Warn -> "warn" | `Fail -> "fail"));
        ("message", `String r.message);
        ("latency_ms", match r.latency_ms with
          | Some ms -> `Float ms | None -> `Null);
      ]
    ) results));
    ("summary", `Assoc [
      ("total", `Int (List.length results));
      ("ok", `Int (List.length (List.filter (fun r -> r.status = `Ok) results)));
      ("warn", `Int (List.length (List.filter (fun r -> r.status = `Warn) results)));
      ("fail", `Int (List.length (List.filter (fun r -> r.status = `Fail) results)));
    ]);
  ] in
  print_endline (Yojson.Safe.pretty_to_string json)

(** Main doctor function *)
let run_doctor ~json_output ~quick =
  let results =
    [ check_server ();
      check_ollama ();
      check_claude_cli ();
      check_gemini ();
      check_codex ();
    ] @
    (if quick then [] else [
      check_neo4j ();
      check_env_vars ();
    ])
  in

  if json_output then
    print_json results
  else begin
    Printf.printf "\n🩺 llm-mcp Doctor\n";
    Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
    List.iter print_result results;
    Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";

    let fails = List.filter (fun r -> r.status = `Fail) results in
    let warns = List.filter (fun r -> r.status = `Warn) results in

    if fails = [] && warns = [] then
      Printf.printf "%s All systems operational\n\n" (green "✓")
    else if fails = [] then
      Printf.printf "%s %d warning(s), but operational\n\n" (yellow "⚠") (List.length warns)
    else
      Printf.printf "%s %d critical issue(s)\n\n" (red "✗") (List.length fails)
  end;

  (* Exit code based on failures *)
  let has_fail = List.exists (fun r -> r.status = `Fail) results in
  if has_fail then exit 1

(** CLI *)
let () =
  let json_output = ref false in
  let quick = ref false in
  let specs = [
    ("--json", Arg.Set json_output, "Output as JSON");
    ("--quick", Arg.Set quick, "Quick check (server + backends only)");
  ] in
  Arg.parse specs (fun _ -> ()) "llm-mcp-doctor: Pre-flight health check";
  run_doctor ~json_output:!json_output ~quick:!quick

(** hook_input - Fast Hook Input Parser

    Extracts all common hook fields from JSON input in one pass.
    Faster than multiple jq calls for each field.

    Usage:
      echo '{"session_id":"abc","transcript_path":"/path"}' | hook-input
      hook-input < input.json
      hook-input --field session_id < input.json

    Output formats:
      --shell   : KEY=VALUE format (default)
      --json    : JSON output
      --field X : Single field value
*)

let extract_string json key =
  match Yojson.Safe.Util.member key json with
  | `String s -> s
  | `Null -> ""
  | _ -> ""

(** Shell-escape a value for safe assignment *)
let shell_escape s =
  let buf = Buffer.create (String.length s + 10) in
  Buffer.add_char buf '\'';
  String.iter (fun c ->
    if c = '\'' then Buffer.add_string buf "'\\''"
    else Buffer.add_char buf c
  ) s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

(** Extract all common hook fields *)
let parse_hook_input input_json output_format field_only =
  try
    let json = Yojson.Safe.from_string input_json in

    (* Common hook fields *)
    let session_id = extract_string json "session_id" in
    let transcript_path = extract_string json "transcript_path" in
    let hook_event_name = extract_string json "hook_event_name" in
    let prompt = extract_string json "prompt" in
    let user_message = extract_string json "user_message" in
    let project_path = extract_string json "project_path" in
    let cwd = extract_string json "cwd" in
    let stop_hook_active = extract_string json "stop_hook_active" in
    (* PreCompact/Tool fields *)
    let trigger = extract_string json "trigger" in
    let custom_instructions = extract_string json "custom_instructions" in
    let tool_name = extract_string json "tool_name" in
    let tool_input = extract_string json "tool_input" in
    (* Session source and SubAgent fields *)
    let source = extract_string json "source" in
    let agent_id = extract_string json "agent_id" in
    let agent_transcript_path = extract_string json "agent_transcript_path" in
    let exit_code = extract_string json "exit_code" in

    (* If specific field requested *)
    match field_only with
    | Some f ->
        let value = extract_string json f in
        print_endline value
    | None ->
        match output_format with
        | "json" ->
            let out = `Assoc [
              ("session_id", `String session_id);
              ("transcript_path", `String transcript_path);
              ("hook_event_name", `String hook_event_name);
              ("prompt", `String prompt);
              ("user_message", `String user_message);
              ("project_path", `String project_path);
              ("cwd", `String cwd);
              ("stop_hook_active", `String stop_hook_active);
              ("trigger", `String trigger);
              ("custom_instructions", `String custom_instructions);
              ("tool_name", `String tool_name);
              ("tool_input", `String tool_input);
              ("source", `String source);
              ("agent_id", `String agent_id);
              ("agent_transcript_path", `String agent_transcript_path);
              ("exit_code", `String exit_code);
            ] in
            print_endline (Yojson.Safe.to_string out)
        | _ -> (* shell format *)
            Printf.printf "SESSION_ID=%s\n" (shell_escape session_id);
            Printf.printf "TRANSCRIPT_PATH=%s\n" (shell_escape transcript_path);
            Printf.printf "HOOK_EVENT_NAME=%s\n" (shell_escape hook_event_name);
            Printf.printf "PROMPT=%s\n" (shell_escape prompt);
            Printf.printf "USER_MESSAGE=%s\n" (shell_escape user_message);
            Printf.printf "PROJECT_PATH=%s\n" (shell_escape project_path);
            Printf.printf "CWD=%s\n" (shell_escape cwd);
            Printf.printf "STOP_HOOK_ACTIVE=%s\n" (shell_escape stop_hook_active);
            (* PreCompact/Tool specific fields *)
            Printf.printf "TRIGGER=%s\n" (shell_escape trigger);
            Printf.printf "CUSTOM_INSTRUCTIONS=%s\n" (shell_escape custom_instructions);
            Printf.printf "TOOL_NAME=%s\n" (shell_escape tool_name);
            Printf.printf "TOOL_INPUT=%s\n" (shell_escape tool_input);
            (* Session source and SubAgent fields *)
            Printf.printf "SOURCE=%s\n" (shell_escape source);
            Printf.printf "AGENT_ID=%s\n" (shell_escape agent_id);
            Printf.printf "AGENT_TRANSCRIPT=%s\n" (shell_escape agent_transcript_path);
            Printf.printf "EXIT_CODE=%s\n" (shell_escape exit_code)
  with
  | Yojson.Json_error msg ->
      Printf.eprintf "JSON parse error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      exit 1

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

(** Main *)
let () =
  let output_format = ref "shell" in
  let field_only = ref None in

  let specs = [
    ("--shell", Arg.Unit (fun () -> output_format := "shell"), "Output as KEY=VALUE (default)");
    ("--json", Arg.Unit (fun () -> output_format := "json"), "Output as JSON");
    ("--field", Arg.String (fun f -> field_only := Some f), "Extract single field");
    ("-f", Arg.String (fun f -> field_only := Some f), "Extract single field (short)");
  ] in

  let usage = "hook-input [--shell|--json] [--field NAME] < input.json" in
  Arg.parse specs (fun _ -> ()) usage;

  let input = read_stdin () in
  parse_hook_input input !output_format !field_only

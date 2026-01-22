(** claude_cli - Claude CLI Wrapper for OCaml

    OCaml port of .claude/hooks/claude_cli.py
    Wraps 'claude -p' command for easy Claude API access using Claude Max subscription.

    Usage as CLI:
      claude_cli --prompt "분석해줘: ..." [--model haiku] [--format json]

    Can also be used as a library (see claude_query function).
*)

(** Model alias normalization *)
let normalize_model model =
  let lower = String.lowercase_ascii model in
  if String.length lower >= 5 && String.sub lower 0 5 = "haiku" then "haiku"
  else if String.length lower >= 6 && String.sub lower 0 6 = "sonnet" then "sonnet"
  else if String.length lower >= 4 && String.sub lower 0 4 = "opus" then "opus"
  (* Check if contains *)
  else if String.length lower > 0 then
    let contains s sub =
      let slen = String.length s in
      let plen = String.length sub in
      if plen > slen then false
      else
        let rec check i =
          if i > slen - plen then false
          else if String.sub s i plen = sub then true
          else check (i + 1)
        in check 0
    in
    if contains lower "haiku" then "haiku"
    else if contains lower "sonnet" then "sonnet"
    else if contains lower "opus" then "opus"
    else "haiku"  (* fallback *)
  else "haiku"

(** Query Claude CLI and return response

    @param prompt User prompt
    @param model Model alias (haiku, sonnet, opus)
    @param output_format Output format (text, json, stream-json)
    @param system_prompt Optional system prompt
    @param max_budget_usd Optional budget limit
    @param timeout Timeout in seconds (default 60)
    @return Response string or JSON
*)
let claude_query
    ?(model = "haiku")
    ?(output_format = "text")
    ?(system_prompt = None)
    ?(max_budget_usd = None)
    ?(timeout = 60)
    prompt
  =
  let model_alias = normalize_model model in

  (* Build command *)
  let cmd_parts = ["claude"; "-p"; "--model"; model_alias] in
  let cmd_parts =
    if output_format <> "text" then
      cmd_parts @ ["--output-format"; output_format]
    else cmd_parts
  in
  let cmd_parts = match system_prompt with
    | Some sp -> cmd_parts @ ["--system-prompt"; sp]
    | None -> cmd_parts
  in
  let cmd_parts = match max_budget_usd with
    | Some budget -> cmd_parts @ ["--max-budget-usd"; Printf.sprintf "%.2f" budget]
    | None -> cmd_parts
  in

  let cmd = String.concat " " (List.map Filename.quote cmd_parts) in

  (* Run with timeout using alarm signal *)
  let old_handler = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ ->
    raise (Failure (Printf.sprintf "claude CLI timed out after %ds" timeout))
  )) in
  ignore (Unix.alarm timeout);

  try
    (* Open process with stdin/stdout *)
    let (out_ch, in_ch, err_ch) = Unix.open_process_full cmd (Unix.environment ()) in

    (* Write prompt to stdin *)
    output_string in_ch prompt;
    close_out in_ch;

    (* Read stdout *)
    let output = Buffer.create 4096 in
    begin
      try
        while true do
          Buffer.add_string output (input_line out_ch);
          Buffer.add_char output '\n'
        done
      with End_of_file -> ()
    end;

    (* Read stderr *)
    let stderr_buf = Buffer.create 256 in
    begin
      try
        while true do
          Buffer.add_string stderr_buf (input_line err_ch);
          Buffer.add_char stderr_buf '\n'
        done
      with End_of_file -> ()
    end;

    (* Get exit status *)
    let status = Unix.close_process_full (out_ch, in_ch, err_ch) in

    (* Cancel alarm *)
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler);

    match status with
    | Unix.WEXITED 0 ->
        let result = String.trim (Buffer.contents output) in
        Ok result
    | Unix.WEXITED code ->
        let err = String.trim (Buffer.contents stderr_buf) in
        Error (Printf.sprintf "claude CLI exited with code %d: %s" code err)
    | Unix.WSIGNALED sig_num ->
        Error (Printf.sprintf "claude CLI killed by signal %d" sig_num)
    | Unix.WSTOPPED sig_num ->
        Error (Printf.sprintf "claude CLI stopped by signal %d" sig_num)

  with
  | Failure msg ->
      ignore (Unix.alarm 0);
      ignore (Sys.signal Sys.sigalrm old_handler);
      Error msg
  | e ->
      ignore (Unix.alarm 0);
      ignore (Sys.signal Sys.sigalrm old_handler);
      Error (Printexc.to_string e)

(** Parse JSON response from claude CLI *)
let parse_json_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    Ok json
  with e ->
    Error (Printf.sprintf "JSON parse failed: %s" (Printexc.to_string e))

(** Create messages in anthropic SDK compatible format *)
let _claude_messages_create
    ?(model = "claude-haiku-4-5-20251001")
    ?(max_tokens = 512)
    ?(temperature = 0.3)
    ?(system = None)
    messages
  =
  ignore max_tokens;
  ignore temperature;

  let model_alias = normalize_model model in

  (* Build prompt from messages *)
  let prompt = String.concat "\n" (
    List.map (fun msg ->
      let open Yojson.Safe.Util in
      let role = msg |> member "role" |> to_string in
      let content = msg |> member "content" |> to_string in
      Printf.sprintf "[%s]: %s" role content
    ) messages
  ) in

  (* Query with JSON output *)
  match claude_query ~model:model_alias ~output_format:"json" ?system_prompt:system prompt with
  | Error e -> Error e
  | Ok json_str ->
      match parse_json_response json_str with
      | Error e -> Error e
      | Ok json ->
          let open Yojson.Safe.Util in
          try
            let result_text = json |> member "result" |> to_string in
            let input_tokens = try json |> member "num_input_tokens" |> to_int with Type_error _ -> 0 in
            let output_tokens = try json |> member "num_output_tokens" |> to_int with Type_error _ -> 0 in
            let cost_usd = try json |> member "cost_usd" |> to_float with Type_error _ -> 0.0 in

            Ok (`Assoc [
              ("content", `List [`Assoc [("text", `String result_text)]]);
              ("model", `String model);
              ("usage", `Assoc [
                ("input_tokens", `Int input_tokens);
                ("output_tokens", `Int output_tokens);
              ]);
              ("cost_usd", `Float cost_usd);
            ])
          with Type_error _ ->
            (* If result key doesn't exist, treat raw as text *)
            Ok (`Assoc [
              ("content", `List [`Assoc [("text", `String json_str)]]);
              ("model", `String model);
              ("usage", `Assoc [
                ("input_tokens", `Int 0);
                ("output_tokens", `Int 0);
              ]);
            ])

(** CLI interface *)
open Cmdliner

let prompt_arg =
  let doc = "Prompt to send to Claude" in
  Arg.(required & opt (some string) None & info ["prompt"; "p"] ~doc)

let model_arg =
  let doc = "Model to use (haiku, sonnet, opus)" in
  Arg.(value & opt string "haiku" & info ["model"; "m"] ~doc)

let format_arg =
  let doc = "Output format (text, json)" in
  Arg.(value & opt string "text" & info ["format"; "f"] ~doc)

let system_arg =
  let doc = "System prompt" in
  Arg.(value & opt (some string) None & info ["system"; "s"] ~doc)

let timeout_arg =
  let doc = "Timeout in seconds" in
  Arg.(value & opt int 60 & info ["timeout"; "t"] ~doc)

let main prompt model format_str system_prompt timeout =
  match claude_query ~model ~output_format:format_str ~system_prompt ~timeout prompt with
  | Ok result ->
      print_endline result
  | Error e ->
      prerr_endline (Printf.sprintf "Error: %s" e);
      exit 1

let cmd =
  let doc = "Claude CLI wrapper - query Claude using 'claude -p' command" in
  let info = Cmd.info "claude_cli" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const main $ prompt_arg $ model_arg $ format_arg $ system_arg $ timeout_arg)

let () = ignore (Cmd.eval cmd)

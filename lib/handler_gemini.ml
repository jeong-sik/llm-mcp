(** handler_gemini.ml - Gemini API handler (Direct + Retry)
    Extracted from tools_eio.ml Phase 2 *)

open Printf
open Types
open Cli_runner_eio

(** Circuit breaker for Gemini CLI calls *)
let gemini_breaker = Mcp_resilience.create_circuit_breaker ~name:"gemini_cli" ~failure_threshold:3 ()

(** Execute Gemini via Direct API (faster, no MASC) *)
let execute_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~thinking_level ~timeout ~stream =
  let model_name = sprintf "gemini-api (%s)" model in
  let thinking_applied = thinking_level = High in
  let extra_base = [
    ("thinking_level", string_of_thinking_level thinking_level);
    ("thinking_prompt_applied", string_of_bool thinking_applied);
    ("use_cli", "false");
  ] in

  match Tools_tracer.require_api_key ~env_var:"GEMINI_API_KEY" ~model:model_name ~extra:extra_base with
  | Some err -> err
  | None ->
    let api_key = Tools_tracer.get_api_key "GEMINI_API_KEY" in
    begin (* Build API request body *)
    let contents = `List [
      `Assoc [
        ("parts", `List [
          `Assoc [("text", `String prompt)]
        ])
      ]
    ] in
    let body = `Assoc [("contents", contents)] in
    let body_str = Yojson.Safe.to_string body in

    (* Map model alias to API model ID
       - Stable: gemini-2.5-pro, gemini-2.5-flash, gemini-2.5-flash-lite
       - Preview: gemini-3-pro-preview, gemini-3-flash-preview
       - Deprecated (2026/03/31): gemini-2.0-* *)
    let api_model = match model with
      | "gemini" -> "gemini-3-pro-preview"
      | "pro" -> "gemini-2.5-pro"
      | "flash" -> "gemini-2.5-flash"
      | "flash-lite" -> "gemini-2.5-flash-lite"
      | "3-pro" -> "gemini-3-pro-preview"
      | "3-flash" -> "gemini-3-flash-preview"
      | m -> m  (* Pass through exact model names like "gemini-2.5-pro" *)
    in

    (* Build curl command *)
    let url = sprintf "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent" api_model in
    let curl_args = [
      "-s"; "-S";  (* silent but show errors *)
      "-H"; sprintf "x-goog-api-key: %s" api_key;
      "-H"; "Content-Type: application/json";
      "-d"; body_str;
      url
    ] in

    (* Note: stream parameter is ignored - Direct API always uses sync call.
       For true streaming, use CLI mode (use_cli=true) *)
    let _ = stream in
    let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" curl_args in
    match result with
    | Ok r ->
        let raw_response = get_output r in
        (* Parse Gemini API response - extract text from candidates[0].content.parts[0].text *)
        (try
          let json = Yojson.Safe.from_string raw_response in
          let open Yojson.Safe.Util in
          let text = json
            |> member "candidates"
            |> index 0
            |> member "content"
            |> member "parts"
            |> index 0
            |> member "text"
            |> to_string
          in
          Tools_tracer.success_result ~model:model_name ~extra:extra_base text
        with _ ->
          (* Check for error response *)
          (try
            let json = Yojson.Safe.from_string raw_response in
            let open Yojson.Safe.Util in
            let error_msg = json |> member "error" |> member "message" |> to_string in
            { model = model_name;
              returncode = -1;
              response = sprintf "API Error: %s" error_msg;
              extra = extra_base @ [("api_error", "true")]; }
          with _ ->
            { model = model_name;
              returncode = -1;
              response = sprintf "Failed to parse API response: %s" (String.sub raw_response 0 (min 200 (String.length raw_response)));
              extra = extra_base @ [("parse_error", "true")]; }))
    | Error (Timeout t) -> Tools_tracer.timeout_result ~model:model_name ~extra:extra_base t
    | Error (ProcessError msg) -> Tools_tracer.process_error_result ~model:model_name ~extra:extra_base msg
  end

(** Execute Gemini with automatic retry for recoverable errors *)
let execute_with_retry ~sw ~proc_mgr ~clock
    ?(max_retries = 2)
    ~model ~thinking_level ~timeout ~stream ~args
    ~execute_cli_streaming () =

  let thinking_applied = thinking_level = High in
  let model_name = sprintf "gemini (%s)" model in
  let extra_base = [
    ("thinking_level", string_of_thinking_level thinking_level);
    ("thinking_prompt_applied", string_of_bool thinking_applied);
  ] in

  match Tool_parsers.build_gemini_cmd args with
  | Error err ->
      { model = model_name;
        returncode = -1;
        response = err;
        extra = extra_base @ [("invalid_args", "true")]; }
  | Ok [] ->
      { model = model_name;
        returncode = -1;
        response = "Empty command list";
        extra = extra_base @ [("invalid_args", "true")]; }
  | Ok (cmd :: cmd_args) ->
      (* Use streaming mode if enabled *)
      if stream then
        execute_cli_streaming ~sw ~proc_mgr ~clock ~timeout ~model_name ~extra_base cmd cmd_args
      else begin
        (* Non-streaming mode with retry logic *)
        let policy = { Mcp_resilience.default_policy with max_attempts = max_retries + 1 } in

        let op () =
          let result = run_command ~sw ~proc_mgr ~clock ~timeout cmd cmd_args in
          let outcome =
            match result with
            | Ok r ->
                let response = get_output r in
                (match classify_gemini_error response with
                | Some error when is_recoverable_gemini_error error ->
                    Result.Error (string_of_gemini_error error)
                | _ -> Result.Ok (r.exit_code, response))
            | Error (Timeout t) -> Result.Error (sprintf "Timeout after %ds" t)
            | Error (ProcessError msg) -> Result.Error (sprintf "Error: %s" msg)
          in
          match outcome with
          | Ok value -> Mcp_resilience.Ok value
          | Error err -> Mcp_resilience.Error err
        in

        let classify _ = Mcp_resilience.Retry in
        match Mcp_resilience.with_retry_eio
                ~clock
                ~policy
                ~circuit_breaker:(Some gemini_breaker)
                ~op_name:"gemini_call"
                ~classify
                op
        with
        | Ok (exit_code, response) ->
            { model = model_name; returncode = exit_code; response; extra = extra_base }
        | Error err ->
            { model = model_name; returncode = -1; response = err; extra = [] }
        | CircuitOpen ->
            { model = model_name; returncode = -1; response = "Circuit breaker open"; extra = [] }
        | TimedOut ->
            { model = model_name; returncode = -1; response = "Operation timed out"; extra = [] }
      end

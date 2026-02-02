(** handler_claude.ml - Claude/Anthropic API handler
    Extracted from tools_eio.ml Phase 2 *)

open Printf
open Types
open Cli_runner_eio

(** Execute Claude via Direct Anthropic API (faster, no CLI overhead)
    @param api_key_override Optional API key override. When provided, skips
    ANTHROPIC_API_KEY env var lookup. Used for multi-account rotation. *)
let execute_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~system_prompt ~timeout ~stream ?api_key_override () =
  let model_name = sprintf "claude-api (%s)" model in
  let extra_base = [("use_cli", "false")] in

  (* Resolve API key: override > env var *)
  let resolved_key = match api_key_override with
    | Some k -> Some k
    | None ->
      match Tools_tracer.require_api_key ~env_var:"ANTHROPIC_API_KEY" ~model:model_name ~extra:extra_base with
      | Some _err -> None
      | None -> Some (Tools_tracer.get_api_key "ANTHROPIC_API_KEY")
  in
  match resolved_key with
  | None ->
    (* No override and env var missing — return error like before *)
    (match Tools_tracer.require_api_key ~env_var:"ANTHROPIC_API_KEY" ~model:model_name ~extra:extra_base with
     | Some err -> err
     | None -> (* unreachable *) Tools_tracer.process_error_result ~model:model_name ~extra:extra_base "No API key")
  | Some api_key ->
    begin (* Map model alias to API model ID *)
    let api_model = Tool_parsers.resolve_claude_model model in

    (* Build API request body *)
    let messages = `List [
      `Assoc [("role", `String "user"); ("content", `String prompt)]
    ] in
    let body_fields = [
      ("model", `String api_model);
      ("max_tokens", `Int 4096);
      ("messages", messages);
    ] in
    let body_fields = match system_prompt with
      | Some sp -> ("system", `String sp) :: body_fields
      | None -> body_fields
    in
    let body = `Assoc body_fields in
    let body_str = Yojson.Safe.to_string body in

    (* Build curl command *)
    let curl_args = [
      "-s"; "-S";
      "-H"; sprintf "x-api-key: %s" api_key;
      "-H"; "content-type: application/json";
      "-H"; "anthropic-version: 2023-06-01";
      "-d"; body_str;
      "https://api.anthropic.com/v1/messages"
    ] in

    let _ = stream in  (* stream not supported in direct API mode *)
    let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" curl_args in
    match result with
    | Ok r ->
        let raw_response = get_output r in
        (try
          let json = Yojson.Safe.from_string raw_response in
          let open Yojson.Safe.Util in
          (* Extract text from content[0].text *)
          let text = json
            |> member "content"
            |> index 0
            |> member "text"
            |> to_string
          in
          Tools_tracer.success_result ~model:model_name ~extra:extra_base text
        with _ ->
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

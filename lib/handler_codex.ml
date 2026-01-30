(** handler_codex.ml - Codex/OpenAI API handler
    Extracted from tools_eio.ml Phase 2 *)

open Printf
open Types
open Cli_runner_eio

(** Execute Codex via Direct OpenAI API (faster, no CLI overhead) *)
let execute_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~timeout ~stream =
  let model_name = sprintf "codex-api (%s)" model in
  let extra_base = [("use_cli", "false")] in
  match Tools_tracer.require_api_key ~env_var:"OPENAI_API_KEY" ~model:model_name ~extra:extra_base with
  | Some err -> err
  | None ->
    let api_key = Tools_tracer.get_api_key "OPENAI_API_KEY" in
    begin
    (* Map model alias to API model ID *)
    let api_model = match model with
      | "gpt-5.2" -> "gpt-5.2"
      | "gpt-5" -> "gpt-5"
      | "o3" -> "o3"
      | "o4-mini" -> "o4-mini"
      | m -> m
    in

    (* Build API request body *)
    let messages = `List [
      `Assoc [("role", `String "user"); ("content", `String prompt)]
    ] in
    let body = `Assoc [
      ("model", `String api_model);
      ("messages", messages);
    ] in
    let body_str = Yojson.Safe.to_string body in

    (* Build curl command *)
    let curl_args = [
      "-s"; "-S";
      "-H"; sprintf "Authorization: Bearer %s" api_key;
      "-H"; "Content-Type: application/json";
      "-d"; body_str;
      "https://api.openai.com/v1/chat/completions"
    ] in

    let _ = stream in  (* stream not supported in direct API mode *)
    let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" curl_args in
    match result with
    | Ok r ->
        let raw_response = get_output r in
        (try
          let json = Yojson.Safe.from_string raw_response in
          let open Yojson.Safe.Util in
          (* Extract text from choices[0].message.content *)
          let text = json
            |> member "choices"
            |> index 0
            |> member "message"
            |> member "content"
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

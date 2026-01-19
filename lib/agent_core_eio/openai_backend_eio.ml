(** OpenAI Backend Eio - Effect-based LLM_BACKEND implementation for OpenAI API

    Direct-style version using Eio instead of Lwt.
    Wraps OpenAI's chat completion endpoint with tool/function calling support.
    Compatible with GPT-4, GPT-3.5-turbo, and other OpenAI-compatible APIs.

    Usage:
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
        let net = Eio.Stdenv.net env in
        let config = Openai_backend_eio.{
          api_key = Sys.getenv "OPENAI_API_KEY";
          model = "gpt-4";
          temperature = 0.7;
          base_url = "https://api.openai.com/v1";
          timeout_ms = Some 60_000;
        } in

        let module Backend = Openai_backend_eio in
        let result = Backend.call ~sw ~net ~config ~messages ~tools in
        ...
    ]}
*)

open Agent_types

(** {1 HTTPS/TLS Support} *)

(** Create HTTPS context for secure connections using system CA certificates *)
let make_https_ctx () =
  match Ca_certs.authenticator () with
  | Error (`Msg m) ->
    Printf.eprintf "Warning: Failed to load system CAs: %s. HTTPS will fail.\n%!" m;
    None
  | Ok authenticator ->
    (* Tls.Config.client returns Result - handle it *)
    match Tls.Config.client ~authenticator () with
    | Error (`Msg m) ->
      Printf.eprintf "Warning: TLS config error: %s. HTTPS will fail.\n%!" m;
      None
    | Ok tls_config ->
      Some (fun uri raw ->
          let host =
            Uri.host uri
            |> Option.map (fun h -> Domain_name.(host_exn (of_string_exn h)))
          in
          Tls_eio.client_of_flow tls_config ?host raw)

(** {1 Configuration} *)

type config = {
  api_key : string;           (** OpenAI API key *)
  model : string;             (** Model name: "gpt-4", "gpt-3.5-turbo", etc. *)
  temperature : float;        (** Temperature (0.0 - 2.0) *)
  base_url : string;          (** API base URL (for Azure or proxies) *)
  timeout_ms : int option;    (** Request timeout in ms *)
  max_tokens : int option;    (** Max tokens to generate *)
  organization : string option;  (** OpenAI organization ID *)
}

let default_config = {
  api_key = "";
  model = "gpt-4";
  temperature = 0.7;
  base_url = "https://api.openai.com/v1";
  timeout_ms = Some 60_000;
  max_tokens = None;
  organization = None;
}

(** {1 Response Types} *)

type response = {
  content : string;
  tool_calls : tool_call list;
  model : string option;
  finish_reason : string option;
  usage : usage option;
}

and usage = {
  prompt_tokens : int;
  completion_tokens : int;
  total_tokens : int;
}

(** {1 Request Building} *)

(** Convert tool parameters to OpenAI JSON schema format *)
let param_to_json (name, schema) =
  let base = [
    ("type", `String schema.param_type);
    ("description", `String schema.description);
  ] in
  let with_enum = match schema.enum with
    | Some values -> base @ [("enum", `List (List.map (fun v -> `String v) values))]
    | None -> base
  in
  (name, `Assoc with_enum)

(** Convert tool definition to OpenAI function format *)
let tool_to_json tool =
  let required = List.filter_map (fun (name, schema) ->
      if schema.required then Some (`String name) else None
    ) tool.parameters
  in
  `Assoc [
    ("type", `String "function");
    ("function", `Assoc [
        ("name", `String tool.name);
        ("description", `String tool.description);
        ("parameters", `Assoc [
            ("type", `String "object");
            ("properties", `Assoc (List.map param_to_json tool.parameters));
            ("required", `List required);
          ]);
      ]);
  ]

(** Convert message to OpenAI format *)
let message_to_json msg =
  let role_str = match msg.role with
    | System -> "system"
    | User -> "user"
    | Assistant -> "assistant"
    | Tool -> "tool"
  in
  let base = [
    ("role", `String role_str);
    ("content", `String msg.content);
  ] in
  (* Add tool_calls for assistant messages *)
  let with_tool_calls = match msg.tool_calls with
    | Some calls when calls <> [] ->
      base @ [("tool_calls", `List (List.map (fun tc ->
          `Assoc [
            ("id", `String tc.id);
            ("type", `String "function");
            ("function", `Assoc [
                ("name", `String tc.name);
                ("arguments", match tc.arguments with
                  | `String s -> `String s
                  | json -> `String (Yojson.Safe.to_string json));
              ]);
          ]
        ) calls))]
    | _ -> base
  in
  (* Add tool_call_id for tool messages *)
  let final = match msg.role, msg.name with
    | Tool, Some n -> with_tool_calls @ [("tool_call_id", `String n)]
    | _, Some n -> with_tool_calls @ [("name", `String n)]
    | _ -> with_tool_calls
  in
  `Assoc final

(** Build chat completion request body *)
let build_request ~(config:config) ~(messages:message list) ~(tools:tool list) =
  let messages_json = List.map message_to_json messages in
  let tools_json = List.map tool_to_json tools in
  let base = [
    ("model", `String config.model);
    ("messages", `List messages_json);
    ("temperature", `Float config.temperature);
  ] in
  let with_tools = if tools <> [] then
      base @ [("tools", `List tools_json)]
    else base
  in
  let with_max_tokens = match config.max_tokens with
    | Some mt -> with_tools @ [("max_tokens", `Int mt)]
    | None -> with_tools
  in
  `Assoc with_max_tokens

(** {1 Response Parsing} *)

(** Parse tool calls from OpenAI response *)
let parse_tool_calls_json json =
  let open Yojson.Safe.Util in
  match json with
  | `List calls ->
    List.filter_map (fun call ->
        try
          let id = call |> member "id" |> to_string in
          let func = call |> member "function" in
          let name = func |> member "name" |> to_string in
          let args_str = func |> member "arguments" |> to_string in
          let arguments = try Yojson.Safe.from_string args_str with _ -> `String args_str in
          Some { id; name; arguments }
        with _ -> None
      ) calls
  | _ -> []

(** Parse usage from OpenAI response *)
let parse_usage_json json =
  let open Yojson.Safe.Util in
  try
    Some {
      prompt_tokens = json |> member "prompt_tokens" |> to_int;
      completion_tokens = json |> member "completion_tokens" |> to_int;
      total_tokens = json |> member "total_tokens" |> to_int;
    }
  with _ -> None

(** Parse chat completion response from OpenAI *)
let parse_response json_str : (response, string) result =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in

    (* Check for error *)
    (match json |> member "error" with
     | `Null -> ()
     | err ->
       let msg = err |> member "message" |> to_string_option |> Option.value ~default:"Unknown error" in
       raise (Failure msg));

    let choices = json |> member "choices" |> to_list in
    if List.length choices = 0 then
      Result.Error "No choices in response"
    else begin
      let choice = List.hd choices in
      let message = choice |> member "message" in
      let content = message |> member "content" |> to_string_option |> Option.value ~default:"" in
      let tool_calls = parse_tool_calls_json (message |> member "tool_calls") in
      let finish_reason = choice |> member "finish_reason" |> to_string_option in
      let model = json |> member "model" |> to_string_option in
      let usage = parse_usage_json (json |> member "usage") in

      Result.Ok { content; tool_calls; model; finish_reason; usage }
    end
  with
  | Failure msg -> Result.Error msg
  | Yojson.Json_error msg -> Result.Error (Printf.sprintf "JSON parse error: %s" msg)
  | e -> Result.Error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))

(** {1 HTTP Client - Eio Version} *)

let call_http ~sw ~net ~(config:config) ~body =
  let uri = Uri.of_string (config.base_url ^ "/chat/completions") in
  let headers =
    let h = Cohttp.Header.init () in
    let h = Cohttp.Header.add h "Content-Type" "application/json" in
    let h = Cohttp.Header.add h "Authorization" ("Bearer " ^ config.api_key) in
    match config.organization with
    | Some org -> Cohttp.Header.add h "OpenAI-Organization" org
    | None -> h
  in
  let body_str = Yojson.Safe.to_string body in
  let req_body = Cohttp_eio.Body.of_string body_str in

  try
    (* Create HTTP client with HTTPS/TLS support *)
    let https = make_https_ctx () in
    let client = Cohttp_eio.Client.make ~https net in
    let (resp, resp_body) = Cohttp_eio.Client.post client ~sw ~headers ~body:req_body uri in

    let status = Cohttp.Response.status resp in
    let body_str = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) resp_body |> take_all) in

    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
      Result.Ok body_str
    else
      Result.Error (Printf.sprintf "OpenAI HTTP %d: %s"
                      (Cohttp.Code.code_of_status status) body_str)
  with e ->
    Result.Error (Printf.sprintf "Connection error: %s" (Printexc.to_string e))

(** {1 LLM_BACKEND Implementation} *)

let name = "openai_eio"

let call ~sw ~net ~(config:config) ~(messages:message list) ~(tools:tool list) =
  let request = build_request ~config ~messages ~tools in
  match call_http ~sw ~net ~config ~body:request with
  | Result.Error e -> Result.Error e
  | Result.Ok body_str -> parse_response body_str

let parse_tool_calls resp =
  if resp.tool_calls = [] then None
  else Some resp.tool_calls

let extract_content resp = resp.content

let is_final resp =
  resp.tool_calls = []

(** {1 Utility Functions} *)

(** Check if API key is set *)
let has_api_key config =
  String.length config.api_key > 0

(** List available models (requires valid API key) *)
let list_models ~sw ~net ~(config:config) () =
  let uri = Uri.of_string (config.base_url ^ "/models") in
  let headers =
    let h = Cohttp.Header.init () in
    Cohttp.Header.add h "Authorization" ("Bearer " ^ config.api_key)
  in

  try
    let https = make_https_ctx () in
    let client = Cohttp_eio.Client.make ~https net in
    let (_resp, body) = Cohttp_eio.Client.get client ~sw ~headers uri in
    let body_str = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
    let json = Yojson.Safe.from_string body_str in
    let models = Yojson.Safe.Util.(json |> member "data" |> to_list) in
    let names = List.filter_map (fun m ->
        Yojson.Safe.Util.(m |> member "id" |> to_string_option)
      ) models
    in
    Result.Ok names
  with e ->
    Result.Error (Printexc.to_string e)

(** Create config from environment *)
let config_from_env ?model () =
  let api_key = Sys.getenv_opt "OPENAI_API_KEY" |> Option.value ~default:"" in
  let base_url = Sys.getenv_opt "OPENAI_BASE_URL" |> Option.value ~default:default_config.base_url in
  let model = match model with
    | Some m -> m
    | None -> Sys.getenv_opt "OPENAI_MODEL" |> Option.value ~default:default_config.model
  in
  { default_config with api_key; base_url; model }

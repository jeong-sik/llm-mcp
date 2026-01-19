(** OpenAI Backend - LLM_BACKEND implementation for OpenAI API

    Wraps OpenAI's chat completion endpoint with tool/function calling support.
    Compatible with GPT-4, GPT-3.5-turbo, and other OpenAI models.

    Usage:
    {[
      module My_Loop = Agent_loop_functor.Make(Openai_backend)(My_Tools)(Default_state)

      let config = Openai_backend.{
        api_key = Sys.getenv "OPENAI_API_KEY";
        model = "gpt-4";
        temperature = 0.7;
        base_url = "https://api.openai.com/v1";
        timeout_ms = Some 60_000;
      }

      let result = My_Loop.run ~config ~backend_config:config ...
    ]}
*)

open Lwt.Syntax
open Agent_types

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

(** {1 HTTP Client} *)

let call_http ~(config:config) ~body =
  let open Cohttp in
  let open Cohttp_lwt_unix in

  let uri = Uri.of_string (config.base_url ^ "/chat/completions") in
  let headers =
    let h = Header.init () in
    let h = Header.add h "Content-Type" "application/json" in
    let h = Header.add h "Authorization" ("Bearer " ^ config.api_key) in
    match config.organization with
    | Some org -> Header.add h "OpenAI-Organization" org
    | None -> h
  in
  let body_str = Yojson.Safe.to_string body in

  try%lwt
    let* (resp, body) = Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body_str) uri in
    let* body_str = Cohttp_lwt.Body.to_string body in

    let status = Response.status resp in
    if Code.is_success (Code.code_of_status status) then
      Lwt.return (Result.Ok body_str)
    else
      Lwt.return (Result.Error (Printf.sprintf "OpenAI HTTP %d: %s"
                                  (Code.code_of_status status) body_str))
  with e ->
    Lwt.return (Result.Error (Printf.sprintf "Connection error: %s" (Printexc.to_string e)))

(** {1 LLM_BACKEND Implementation} *)

let name = "openai"

let call ~(config:config) ~(messages:message list) ~(tools:tool list) =
  let request = build_request ~config ~messages ~tools in
  let* result = call_http ~config ~body:request in
  match result with
  | Result.Error e -> Lwt.return (Result.Error e)
  | Result.Ok body_str ->
    match parse_response body_str with
    | Result.Error e -> Lwt.return (Result.Error e)
    | Result.Ok resp -> Lwt.return (Result.Ok resp)

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
let list_models ~(config:config) () =
  let open Cohttp in
  let open Cohttp_lwt_unix in

  let uri = Uri.of_string (config.base_url ^ "/models") in
  let headers =
    let h = Header.init () in
    Header.add h "Authorization" ("Bearer " ^ config.api_key)
  in

  try%lwt
    let* (_, body) = Client.get ~headers uri in
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_str in
    let models = Yojson.Safe.Util.(json |> member "data" |> to_list) in
    let names = List.filter_map (fun m ->
        Yojson.Safe.Util.(m |> member "id" |> to_string_option)
      ) models
    in
    Lwt.return (Result.Ok names)
  with e ->
    Lwt.return (Result.Error (Printexc.to_string e))

(** Create config from environment *)
let config_from_env ?model () =
  let api_key = Sys.getenv_opt "OPENAI_API_KEY" |> Option.value ~default:"" in
  let base_url = Sys.getenv_opt "OPENAI_BASE_URL" |> Option.value ~default:default_config.base_url in
  let model = match model with
    | Some m -> m
    | None -> Sys.getenv_opt "OPENAI_MODEL" |> Option.value ~default:default_config.model
  in
  { default_config with api_key; base_url; model }

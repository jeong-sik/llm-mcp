(** Ollama Backend - LLM_BACKEND implementation for Ollama API

    Wraps Ollama's /api/chat endpoint with proper message formatting,
    tool calling support, and streaming capabilities.

    Usage:
    {[
      module My_Loop = Agent_loop_functor.Make(Ollama_backend)(My_Tools)(Default_state)

      let config = Ollama_backend.{
        base_url = "http://127.0.0.1:11434";
        model = "llama3";
        temperature = 0.7;
        stream = false;
      }

      let result = My_Loop.run ~config ~backend_config:config ...
    ]}
*)

open Lwt.Syntax
open Agent_types

(** {1 Configuration} *)

type config = {
  base_url : string;       (** Ollama server URL (default: http://127.0.0.1:11434) *)
  model : string;          (** Model name (e.g., "llama3", "qwen3", "devstral") *)
  temperature : float;     (** Temperature (0.0 - 2.0) *)
  stream : bool;           (** Enable streaming (not yet fully supported in agent loop) *)
  timeout_ms : int option; (** Request timeout in ms (None = no timeout) *)
}

let default_config = {
  base_url = "http://127.0.0.1:11434";
  model = "llama3";
  temperature = 0.7;
  stream = false;
  timeout_ms = Some 60_000;
}

(** {1 Response Types} *)

type response = {
  content : string;
  tool_calls : tool_call list;
  done_ : bool;
  model : string option;
  eval_count : int option;  (** Tokens generated *)
  eval_duration : int option;  (** Generation time in ns *)
}

(** {1 Request Building} *)

(** Convert tool parameters to Ollama JSON schema format *)
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

(** Convert tool definition to Ollama format *)
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

(** Build chat request body *)
let build_request ~(config:config) ~(messages:message list) ~(tools:tool list) =
  let messages_json = List.map (fun msg ->
      let base = [
        ("role", `String (role_to_string msg.role));
        ("content", `String msg.content);
      ] in
      (* Add tool_calls if present (for assistant messages) *)
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
      let with_name = match msg.role, msg.name with
        | Tool, Some n -> with_tool_calls @ [("tool_call_id", `String n)]
        | _, Some n -> with_tool_calls @ [("name", `String n)]
        | _ -> with_tool_calls
      in
      `Assoc with_name
    ) messages
  in
  let tools_json = List.map tool_to_json tools in
  `Assoc [
    ("model", `String config.model);
    ("messages", `List messages_json);
    ("options", `Assoc [("temperature", `Float config.temperature)]);
    ("tools", `List tools_json);
    ("stream", `Bool config.stream);
  ]

(** {1 Response Parsing} *)

(** Parse tool calls from Ollama response *)
let parse_tool_calls_json json =
  let open Yojson.Safe.Util in
  match json with
  | `List calls ->
    List.filter_map (fun call ->
        try
          let id = call |> member "id" |> to_string_option |> Option.value ~default:(Printf.sprintf "call_%d" (Random.int 100000)) in
          let func = call |> member "function" in
          let name = func |> member "name" |> to_string in
          let args = func |> member "arguments" in
          let arguments = match args with
            | `String s -> (try Yojson.Safe.from_string s with _ -> `String s)
            | json -> json
          in
          Some { id; name; arguments }
        with _ -> None
      ) calls
  | _ -> []

(** Parse chat response from Ollama *)
let parse_response json_str : (response, string) result =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in

    (* Check for error *)
    match json |> member "error" |> to_string_option with
    | Some err -> Result.Error err
    | None ->
      let message = json |> member "message" in
      let content = message |> member "content" |> to_string_option |> Option.value ~default:"" in
      let tool_calls = parse_tool_calls_json (message |> member "tool_calls") in
      let done_ = json |> member "done" |> to_bool_option |> Option.value ~default:true in
      let model = json |> member "model" |> to_string_option in
      let eval_count = json |> member "eval_count" |> to_int_option in
      let eval_duration = json |> member "eval_duration" |> to_int_option in
      Result.Ok { content; tool_calls; done_; model; eval_count; eval_duration }
  with
  | Yojson.Json_error msg -> Result.Error (Printf.sprintf "JSON parse error: %s" msg)
  | e -> Result.Error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))

(** {1 HTTP Client} *)

let call_http ~(config:config) ~body =
  let open Cohttp in
  let open Cohttp_lwt_unix in

  let uri = Uri.of_string (config.base_url ^ "/api/chat") in
  let headers = Header.init_with "Content-Type" "application/json" in
  let body_str = Yojson.Safe.to_string body in

  try%lwt
    let* (resp, body) = Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body_str) uri in
    let* body_str = Cohttp_lwt.Body.to_string body in

    let status = Response.status resp in
    if Code.is_success (Code.code_of_status status) then
      Lwt.return (Result.Ok body_str)
    else
      Lwt.return (Result.Error (Printf.sprintf "Ollama HTTP %d: %s"
                                  (Code.code_of_status status) body_str))
  with e ->
    Lwt.return (Result.Error (Printf.sprintf "Connection error: %s" (Printexc.to_string e)))

(** {1 LLM_BACKEND Implementation} *)

let name = "ollama"

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

(** Check if Ollama server is reachable *)
let health_check ?(base_url = "http://127.0.0.1:11434") () =
  let open Cohttp_lwt_unix in
  let uri = Uri.of_string (base_url ^ "/api/tags") in
  try%lwt
    let* (resp, _) = Client.get uri in
    let status = Cohttp.Response.status resp in
    Lwt.return (Cohttp.Code.is_success (Cohttp.Code.code_of_status status))
  with _ ->
    Lwt.return false

(** List available models from Ollama *)
let list_models ?(base_url = "http://127.0.0.1:11434") () =
  let open Cohttp_lwt_unix in
  let uri = Uri.of_string (base_url ^ "/api/tags") in
  try%lwt
    let* (_, body) = Client.get uri in
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_str in
    let models = Yojson.Safe.Util.(json |> member "models" |> to_list) in
    let names = List.filter_map (fun m ->
        Yojson.Safe.Util.(m |> member "name" |> to_string_option)
      ) models
    in
    Lwt.return (Result.Ok names)
  with e ->
    Lwt.return (Result.Error (Printexc.to_string e))

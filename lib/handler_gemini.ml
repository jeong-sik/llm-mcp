(** handler_gemini.ml - Gemini API handler (Direct + Retry)
    Extracted from tools_eio.ml Phase 2 *)

open Printf
open Types
open Cli_runner_eio

(** Circuit breaker for Gemini CLI calls *)
let gemini_breaker = Mcp_resilience.create_circuit_breaker ~name:"gemini_cli" ~failure_threshold:3 ()

(** Gemini model aliases used across tools. *)
let gemini_model_aliases : (string * string) list = [
  ("gemini", "gemini-3-pro-preview");
  ("pro", "gemini-2.5-pro");
  ("flash", "gemini-2.5-flash");
  ("flash-lite", "gemini-2.5-flash-lite");
  ("3-pro", "gemini-3-pro-preview");
  ("3-flash", "gemini-3-flash-preview");
]

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let strip_prefix ~prefix s =
  if starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else
    s

let string_contains_ci ~needle haystack =
  let n = String.lowercase_ascii needle in
  let h = String.lowercase_ascii haystack in
  match Str.search_forward (Str.regexp_string n) h 0 with
  | _ -> true
  | exception Not_found -> false

(** Parse /v1beta/models list response into a compact JSON list.
    Pure (no I/O) so it can be unit-tested. *)
let parse_models_response ~(include_all : bool) ~(filter : string option) (json : Yojson.Safe.t)
  : Yojson.Safe.t list =
  let open Yojson.Safe.Util in
  let models =
    match json |> member "models" with
    | `List xs -> xs
    | _ -> []
  in
  let matches_filter id =
    match filter with
    | None -> true
    | Some f -> string_contains_ci ~needle:f id
  in
  let parse_methods (m : Yojson.Safe.t) : string list =
    match m |> member "supportedGenerationMethods" with
    | `List xs ->
        List.filter_map (fun x -> try Some (to_string x) with _ -> None) xs
    | _ -> []
  in
  let to_opt_string (x : Yojson.Safe.t) : string option =
    try Some (to_string x) with _ -> None
  in
  let to_opt_int (x : Yojson.Safe.t) : int option =
    try Some (to_int x) with _ -> None
  in
  models
  |> List.filter_map (fun m ->
    let full_name = m |> member "name" |> to_opt_string in
    match full_name with
    | None -> None
    | Some full ->
        let id = strip_prefix ~prefix:"models/" full in
        if not (matches_filter id) then None
        else
          let methods = parse_methods m in
          let supports_generate = List.mem "generateContent" methods in
          if (not include_all) && (not supports_generate) then None
          else
            let display_name = m |> member "displayName" |> to_opt_string in
            let description = m |> member "description" |> to_opt_string in
            let input_token_limit = m |> member "inputTokenLimit" |> to_opt_int in
            let output_token_limit = m |> member "outputTokenLimit" |> to_opt_int in
            Some (`Assoc [
              ("id", `String id);
              ("full_name", `String full);
              ("display_name", (match display_name with Some s -> `String s | None -> `Null));
              ("description", (match description with Some s -> `String s | None -> `Null));
              ("input_token_limit", (match input_token_limit with Some n -> `Int n | None -> `Null));
              ("output_token_limit", (match output_token_limit with Some n -> `Int n | None -> `Null));
              ("supported_generation_methods", `List (List.map (fun s -> `String s) methods));
              ("supports_generate_content", `Bool supports_generate);
            ]))

let aliases_to_json () : Yojson.Safe.t =
  `Assoc (List.map (fun (a, m) -> (a, `String m)) gemini_model_aliases)

let static_models_to_json ~(filter : string option) : Yojson.Safe.t list =
  let unique = Hashtbl.create 16 in
  let models =
    gemini_model_aliases
    |> List.map snd
    |> List.filter (fun id -> if Hashtbl.mem unique id then false else (Hashtbl.add unique id (); true))
  in
  let matches_filter id =
    match filter with
    | None -> true
    | Some f -> string_contains_ci ~needle:f id
  in
  models
  |> List.filter matches_filter
  |> List.map (fun id -> `Assoc [
    ("id", `String id);
    ("source", `String "static");
  ])

let list_models ~sw ~proc_mgr ~clock ~(filter : string option) ~(include_all : bool) () : tool_result =
  let model_name = "gemini_list" in
  let api_key = Tools_tracer.get_api_key "GEMINI_API_KEY" in
  if String.length api_key = 0 then
    let models = static_models_to_json ~filter in
    let body = `Assoc [
      ("source", `String "static");
      ("note", `String "Set GEMINI_API_KEY to fetch live models from API.");
      ("filter", (match filter with Some f -> `String f | None -> `Null));
      ("include_all", `Bool include_all);
      ("count", `Int (List.length models));
      ("models", `List models);
      ("aliases", aliases_to_json ());
    ] |> Yojson.Safe.to_string
    in
    { model = model_name; returncode = 0; response = body; extra = [] }
  else
    let base_url = "https://generativelanguage.googleapis.com/v1beta/models" in
    let build_url page_token =
      let page_size = "200" in
      match page_token with
      | None ->
          sprintf "%s?pageSize=%s" base_url page_size
      | Some tok ->
          sprintf "%s?pageSize=%s&pageToken=%s" base_url page_size (Uri.pct_encode tok)
    in
    let fetch_page page_token =
      let url = build_url page_token in
      let curl_args = [
        "-s"; "-S";
        "-H"; sprintf "x-goog-api-key: %s" api_key;
        url
      ] in
      match run_command ~sw ~proc_mgr ~clock ~timeout:30 "curl" curl_args with
      | Ok r -> Ok (get_output r)
      | Error (Timeout t) -> Error (Tools_tracer.timeout_result ~model:model_name ~extra:[] t)
      | Error (ProcessError msg) -> Error (Tools_tracer.process_error_result ~model:model_name ~extra:[] msg)
    in
    let parse_next_page_token (j : Yojson.Safe.t) : string option =
      try Yojson.Safe.Util.(j |> member "nextPageToken" |> to_string_option)
      with _ -> None
    in
    let parse_api_error (j : Yojson.Safe.t) : string option =
      match Yojson.Safe.Util.member "error" j with
      | `Null -> None
      | err ->
          let msg =
            try Yojson.Safe.Util.(err |> member "message" |> to_string)
            with _ -> "Unknown API error"
          in
          Some msg
    in
    let parse_json (raw : string) : (Yojson.Safe.t, tool_result) result =
      match Yojson.Safe.from_string raw with
      | j -> Ok j
      | exception _ ->
          let snippet =
            let len = min 200 (String.length raw) in
            String.sub raw 0 len
          in
          Error { model = model_name;
                  returncode = -1;
                  response = sprintf "Failed to parse API response: %s" snippet;
                  extra = []; }
    in
    let rec loop page_token acc pages =
      if pages >= 10 then
        Ok acc
      else
        match fetch_page page_token with
        | Error err -> Error err
        | Ok raw ->
            (match parse_json raw with
             | Error err -> Error err
             | Ok j ->
                 (match parse_api_error j with
                  | Some msg ->
                      Error { model = model_name;
                              returncode = -1;
                              response = sprintf "API Error: %s" msg;
                              extra = []; }
                  | None ->
                      let models = parse_models_response ~include_all ~filter j in
                      let acc = acc @ models in
                      match parse_next_page_token j with
                      | None -> Ok acc
                      | Some tok -> loop (Some tok) acc (pages + 1)))
    in
    match loop None [] 0 with
    | Error err -> err
    | Ok models ->
        let body = `Assoc [
          ("source", `String "api");
          ("filter", (match filter with Some f -> `String f | None -> `Null));
          ("include_all", `Bool include_all);
          ("count", `Int (List.length models));
          ("models", `List models);
          ("aliases", aliases_to_json ());
        ] |> Yojson.Safe.to_string
        in
        { model = model_name; returncode = 0; response = body; extra = [] }

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

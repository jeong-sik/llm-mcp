(** MCP Protocol Server Implementation with Lwt

    Implements MCP 2025-11-25 with legacy protocol compatibility.
    See: https://modelcontextprotocol.io/specification/2025-11-25
*)

open Types

module P = Mcp_protocol.Protocol
module R = Mcp_protocol.Resources

(** JSON-RPC request (id is optional for notifications) *)
type jsonrpc_request = {
  jsonrpc : string;
  id : Yojson.Safe.t option; [@default None]
  method_ : string; [@key "method"]
  params : Yojson.Safe.t option; [@default None]
} [@@deriving yojson { strict = false }]

let has_field key = function
  | `Assoc fields -> List.exists (fun (k, _) -> k = key) fields
  | _ -> false

let get_field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let is_jsonrpc_v2 json =
  match get_field "jsonrpc" json with
  | Some (`String "2.0") -> true
  | _ -> false

let is_jsonrpc_response json =
  match json with
  | `Assoc _ ->
      let has_result = has_field "result" json in
      let has_error = has_field "error" json in
      let has_method = has_field "method" json in
      let has_id = has_field "id" json in
      is_jsonrpc_v2 json && has_id && (has_result || has_error) && not has_method
  | _ -> false

(** Response type: JSON or SSE stream *)
type response_type =
  | JsonResponse of Yojson.Safe.t
  | SseStream of (Sse.stream_state * (unit -> string Lwt.t))
  | SseTokenStream of (Sse.stream_state * string Lwt_stream.t * (unit -> unit Lwt.t))
      (* stream_state, event_stream, starter function *)
  | NoResponse  (* For notifications *)

(** Check if request is a notification (no id) *)
let is_notification req = req.id = None

(** Get id or null *)
let get_id req = match req.id with Some id -> id | None -> `Null

(** JSON-RPC response *)
let make_response ~id result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ]

let make_error ~id code message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
  ]

(** MCP protocol version support (legacy + current) *)
let supported_protocol_versions = [
  "2024-11-05";
  "2025-03-26";
  "2025-11-25";
]

let default_protocol_version = Mcp_session.protocol_version

let normalize_protocol_version version =
  if List.mem version supported_protocol_versions then version
  else default_protocol_version

let protocol_version_from_params params =
  let open Yojson.Safe.Util in
  match params with
  | Some (`Assoc _ as p) ->
      (try p |> member "protocolVersion" |> to_string
       with _ -> default_protocol_version)
  | _ -> default_protocol_version

let client_wants_streaming params =
  let open Yojson.Safe.Util in
  match params with
  | Some (`Assoc _ as p) ->
      (try p |> member "capabilities" |> member "experimental" |> member "streamingToolCalls" |> to_bool
       with _ -> false)
  | _ -> false

(** MCP Server capabilities - version-aware *)
let server_info = `Assoc [
  ("name", `String "llm-mcp");
  ("version", `String "2.0.1");
]

let capabilities_for_version ~include_experimental version =
  let tools = `Assoc [
    ("listChanged", `Bool false);  (* We don't support dynamic tool list *)
  ] in
  let resources = `Assoc [
    ("listChanged", `Bool false);
  ] in
  let base = [("tools", tools); ("resources", resources)] in
  if version = "2025-11-25" && include_experimental then
    `Assoc (base @ [
      ("experimental", `Assoc [
        ("streamingToolCalls", `Bool true);  (* SSE for long ops *)
      ]);
    ])
  else
    `Assoc base

(* ===== Resources (read-only context) ===== *)

let resources : P.resource list = [
  {
    uri = "llm://runs?since_ts=0&limit=50";
    name = "Recent Runs";
    description = "Recent llm-mcp tool executions (JSONL-backed, no prompt content)";
    mime_type = "application/json";
  };
  {
    uri = "llm://stats?since_ts=0&until_ts=0";
    name = "Run Statistics";
    description = "Aggregated stats over llm-mcp runs";
    mime_type = "application/json";
  };
]

let resource_templates : R.resource_template list = [
  {
    uri_template = "llm://runs{?since_ts,limit}";
    name = "Runs (range)";
    description = "Read runs with optional since_ts and limit";
    mime_type = "application/json";
  };
  {
    uri_template = "llm://stats{?since_ts,until_ts}";
    name = "Stats (range)";
    description = "Read aggregated stats with optional since_ts and until_ts";
    mime_type = "application/json";
  };
]

let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let prompt_chars_of_args (args : tool_args) =
  match args with
  | Gemini { prompt; _ }
  | Claude { prompt; _ }
  | Codex { prompt; _ }
  | Ollama { prompt; _ } -> String.length prompt
  | OllamaList -> 0  (* No prompt for list operation *)

let split_once s ch =
  match String.index_opt s ch with
  | None -> (s, None)
  | Some i ->
      let left = String.sub s 0 i in
      let right = String.sub s (i + 1) (String.length s - i - 1) in
      (left, Some right)

let parse_query_params query =
  query
  |> String.split_on_char '&'
  |> List.filter_map (fun part ->
    if part = "" then None
    else
      match String.split_on_char '=' part with
      | [k; v] -> Some (k, v)
      | [k] -> Some (k, "")
      | _ -> None)

let int_query_param params key ~default =
  match Common.assoc_opt key params with
  | None -> default
  | Some s -> (try int_of_string s with _ -> default)

let llm_resource_id uri_str =
  let prefix = "llm://" in
  if not (starts_with ~prefix uri_str) then None
  else
    let rest = String.sub uri_str (String.length prefix) (String.length uri_str - String.length prefix) in
    let id, _query = split_once rest '?' in
    Some id

(** Handle initialize request - creates new session *)
let handle_initialize ~session_opt ~params id =
  let protocol_version =
    params |> protocol_version_from_params |> normalize_protocol_version
  in
  let session = match session_opt with
    | Some s ->
        (* Reuse existing session, update protocol *)
        Mcp_session.set_negotiated_protocol s protocol_version;
        s
    | None -> Mcp_session.create_session ~protocol:protocol_version ()
  in
  Mcp_session.mark_initialized session;
  let include_experimental = client_wants_streaming params in
  (session, Lwt.return @@ JsonResponse (make_response ~id (`Assoc [
    ("protocolVersion", `String protocol_version);
    ("serverInfo", server_info);
    ("capabilities", capabilities_for_version ~include_experimental protocol_version);
    ("instructions", `String "LLM-MCP provides multi-LLM access (MAGI Trinity). \
      Tools: gemini (CASPER/strategy), claude-cli (BALTHASAR/values), codex (MELCHIOR/code), ollama (local). \
      Use response_format='compact' for 64% token savings. \
      For MAGI consensus, call 2+ LLMs and compare results.");
  ])))

(** Handle tools/list request *)
let handle_list_tools id =
  let tools = List.map (fun (schema : tool_schema) ->
    `Assoc [
      ("name", `String schema.name);
      ("description", `String schema.description);
      ("inputSchema", schema.input_schema);
    ]
  ) all_schemas in
  Lwt.return @@ JsonResponse (make_response ~id (`Assoc [("tools", `List tools)]))

(** Handle tools/call request with SSE streaming support *)
let handle_call_tool ~wants_stream id params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let name = params |> member "name" |> to_string in
  let arguments = params |> member "arguments" in

  (* Parse response_format from arguments (global parameter for all tools)
     When budget_mode=true, default to Compact for token savings *)
  let budget_mode = Tools.budget_mode_value arguments in
  let response_format =
    arguments |> member "response_format" |> to_string_option
    |> Option.map response_format_of_string
    |> Option.value ~default:(if budget_mode then Compact else Verbose)
  in

  let args_opt = match name with
    | "gemini" -> Some (Tools.parse_gemini_args arguments)
    | "claude-cli" -> Some (Tools.parse_claude_args arguments)
    | "codex" -> Some (Tools.parse_codex_args arguments)
    | "ollama" -> Some (Tools.parse_ollama_args arguments)
    | "ollama_list" -> Some (Tools.parse_ollama_list_args arguments)
    | _ -> None
  in
  match args_opt with
  | None ->
      Lwt.return @@ JsonResponse (make_error ~id (-32602) (Printf.sprintf "Unknown tool: %s" name))
  | Some args ->

  let prompt_chars = prompt_chars_of_args args in

  let record_run ~streamed started_at result =
    let duration_ms =
      int_of_float ((Unix.gettimeofday () -. started_at) *. 1000.0)
    in
    Lwt.async (fun () -> Run_log.record ~tool:name ~streamed ~prompt_chars ~duration_ms result)
  in

  (* Format result based on response_format:
     - Verbose: Full JSON (default, backwards compatible)
     - Compact: DSL "RES|OK|G3|150|result" (~70% token savings)
     - Binary: Base64-encoded compact (for high-volume)
  *)
  let format_result result =
    match response_format with
    | Verbose ->
        let result_json = tool_result_to_yojson result in
        Yojson.Safe.pretty_to_string result_json
    | Compact | Binary | Base85 | Compressed | ZstdDict | Auto ->
        format_tool_result ~format:response_format result
  in

  (* Check if args request token-by-token streaming *)
  let wants_token_stream = match args with
    | Types.Ollama { stream = true; _ } -> true
    | _ -> false
  in

  (* Enable SSE keepalive if the client accepts streaming or caller requests it. *)
  let stream_requested = match args with
    | Types.Gemini { stream; _ }
    | Types.Claude { stream; _ }
    | Types.Codex { stream; _ }
    | Types.Ollama { stream; _ } -> stream
    | Types.OllamaList -> false
  in
  let wants_keepalive = wants_stream || stream_requested in

  (* For slow tools (local ollama models) with streaming client, use SSE *)
  let is_local_model = name = "ollama" in

  (* Token-by-token streaming via SseTokenStream *)
  if wants_keepalive && is_local_model && wants_token_stream then begin
    let sse_stream = Sse.create_stream () in
    let (push_stream, push) = Lwt_stream.create () in
    let started_at = ref (Unix.gettimeofday ()) in
    let full_response = Buffer.create 1024 in

    let starter () =
      started_at := Unix.gettimeofday ();
      let on_token token =
        Buffer.add_string full_response token;
        (* Emit SSE event for each token *)
        let event = Sse.progress_event sse_stream ~progress:0.0 ~message:token in
        push (Some event);
        Lwt.return_unit
      in
      let* result = Tools.execute_ollama_streaming ~on_token args in
      let () = record_run ~streamed:true !started_at result in
      let formatted = format_result result in
      (* Final response event *)
      let response = make_response ~id (`Assoc [
        ("content", `List [
          `Assoc [
            ("type", `String "text");
            ("text", `String formatted);
          ]
        ]);
        ("isError", `Bool (result.returncode <> 0));
      ]) in
      push (Some (Sse.json_event sse_stream response));
      push None;  (* Close stream *)
      Lwt.return_unit
    in
    Lwt.return @@ SseTokenStream (sse_stream, push_stream, starter)

  end else if wants_keepalive then begin
    (* SSE keepalive for long-running tools *)
    let stream = Sse.create_stream () in
    let (push_stream, push) = Lwt_stream.create () in
    let finished = ref false in

    let rec keepalive () =
      if !finished then Lwt.return_unit
      else
        let event = Sse.progress_event stream ~progress:0.0 ~message:"working" in
        push (Some event);
        let* () = Lwt_unix.sleep 5.0 in
        keepalive ()
    in

    let starter () =
      let started_at = Unix.gettimeofday () in
      (* Start keepalive loop immediately to avoid client timeouts. *)
      Lwt.async keepalive;
      let* result = Tools.execute args in
      finished := true;
      let () = record_run ~streamed:true started_at result in
      let formatted = format_result result in
      let response = make_response ~id (`Assoc [
        ("content", `List [
          `Assoc [
            ("type", `String "text");
            ("text", `String formatted);
          ]
        ]);
        ("isError", `Bool (result.returncode <> 0));
      ]) in
      push (Some (Sse.json_event stream response));
      push None;
      Lwt.return_unit
    in
    Lwt.return @@ SseTokenStream (stream, push_stream, starter)
  end else begin
    (* Standard JSON response for fast tools *)
    let started_at = Unix.gettimeofday () in
    let* result = Tools.execute args in
    let () = record_run ~streamed:false started_at result in
    let formatted = format_result result in
    Lwt.return @@ JsonResponse (make_response ~id (`Assoc [
      ("content", `List [
        `Assoc [
          ("type", `String "text");
          ("text", `String formatted);
        ]
      ]);
      ("isError", `Bool (result.returncode <> 0));
    ]))
  end

let handle_read_resource id params =
  let open Yojson.Safe.Util in
  match params with
  | None -> Lwt.return @@ JsonResponse (make_error ~id (-32602) "Missing params")
  | Some (`Assoc _ as p) ->
      let uri_str = try p |> member "uri" |> to_string with _ -> "" in
      if uri_str = "" then
        Lwt.return @@ JsonResponse (make_error ~id (-32602) "Missing uri")
      else
        let _base, query_opt = split_once uri_str '?' in
        let query_params =
          match query_opt with
          | None -> []
          | Some q -> parse_query_params q
        in
        (match llm_resource_id uri_str with
        | Some "runs" ->
            let since_ts = int_query_param query_params "since_ts" ~default:0 in
            let limit = int_query_param query_params "limit" ~default:50 in
            let events = Run_log.read_recent ~since_ts ~limit in
            let json = `List events in
            let contents : R.content list = [
              { uri = uri_str; mime_type = "application/json"; text = Yojson.Safe.pretty_to_string json }
            ] in
            Lwt.return @@ JsonResponse (make_response ~id (R.read_result contents))
        | Some "stats" ->
            let since_ts = int_query_param query_params "since_ts" ~default:0 in
            let until_ts = int_query_param query_params "until_ts" ~default:0 in
            let json = Run_log.stats ~since_ts ~until_ts in
            let contents : R.content list = [
              { uri = uri_str; mime_type = "application/json"; text = Yojson.Safe.pretty_to_string json }
            ] in
            Lwt.return @@ JsonResponse (make_response ~id (R.read_result contents))
        | Some resource_id ->
            Lwt.return @@ JsonResponse (make_error ~id (-32602) ("Unknown resource: llm://" ^ resource_id))
        | None ->
            Lwt.return @@ JsonResponse (make_error ~id (-32602) ("Unknown resource: " ^ uri_str)))
  | Some _ ->
      Lwt.return @@ JsonResponse (make_error ~id (-32602) "Invalid params")

(** Route JSON-RPC request to handler
    @param session_opt: Optional existing session
    @param wants_stream: Client accepts SSE (text/event-stream)
    @return: (session option * response_type) - session for initialize, response for all
*)
let handle_request ~session_opt ~wants_stream request_str =
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      let json =
        try Ok (Yojson.Safe.from_string request_str)
        with exn -> Error (Printexc.to_string exn)
      in
      match json with
      | Error msg ->
          Lwt.return (session_opt, JsonResponse (make_error ~id:`Null (-32700) ("Parse error: " ^ msg)))
      | Ok json ->
          if is_jsonrpc_response json then
            Lwt.return (session_opt, NoResponse)
          else if not (is_jsonrpc_v2 json) then
            Lwt.return (session_opt, JsonResponse (make_error ~id:`Null (-32600) "Invalid Request: jsonrpc must be 2.0"))
          else
            match jsonrpc_request_of_yojson json with
            | Error msg ->
                Lwt.return (session_opt, JsonResponse (make_error ~id:`Null (-32600) ("Invalid Request: " ^ msg)))
            | Ok req ->
                let id = get_id req in
                (* Handle notifications (no id = no response) *)
                if is_notification req then
                  Lwt.return (session_opt, NoResponse)
                else
                  (match req.method_ with
                  | "initialize" ->
                      let (session, response_lwt) = handle_initialize ~session_opt ~params:req.params id in
                      let* resp = response_lwt in
                      Lwt.return (Some session, resp)
                  | "initialized" ->
                      Lwt.return (session_opt, JsonResponse (make_response ~id `Null))
                  | "tools/list" ->
                      let* resp = handle_list_tools id in
                      Lwt.return (session_opt, resp)
                  | "tools/call" ->
                      (match req.params with
                      | Some params ->
                          let* resp = handle_call_tool ~wants_stream id params in
                          Lwt.return (session_opt, resp)
                      | None ->
                          Lwt.return (session_opt, JsonResponse (make_error ~id (-32602) "Missing params")))
                  | "resources/list" ->
                      let result = R.list_result resources in
                      Lwt.return (session_opt, JsonResponse (make_response ~id result))
                  | "resources/templates/list" ->
                      let result = R.templates_list_result resource_templates in
                      Lwt.return (session_opt, JsonResponse (make_response ~id result))
                  | "resources/read" ->
                      let* resp = handle_read_resource id req.params in
                      Lwt.return (session_opt, resp)
                  | method_ ->
                      Lwt.return (session_opt, JsonResponse (make_error ~id (-32601) ("Method not found: " ^ method_))))
    )
    (fun exn ->
      Lwt.return (session_opt, JsonResponse (make_error ~id:`Null (-32603) ("Internal error: " ^ Printexc.to_string exn))))

(** Run MCP server in stdio mode (no SSE streaming, JSON only) *)
let run_stdio () =
  let open Lwt.Syntax in
  let rec loop ~session_opt =
    let* line_opt =
      Lwt.catch
        (fun () ->
          let+ line = Lwt_io.read_line Lwt_io.stdin in
          Some line)
        (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)
    in
    match line_opt with
    | None -> Lwt.return_unit
    | Some line when String.length line = 0 -> loop ~session_opt
    | Some line ->
        let* (new_session_opt, response) = handle_request ~session_opt ~wants_stream:false line in
        (match response with
        | NoResponse -> ()  (* Notifications don't need response *)
        | JsonResponse json ->
            let response_str = Yojson.Safe.to_string json in
            Lwt_io.printl response_str |> ignore
        | SseStream _ | SseTokenStream _ ->
            (* SSE not supported in stdio mode, should not happen with wants_stream:false *)
            ());
        loop ~session_opt:new_session_opt
  in
  loop ~session_opt:None

(** Health check response *)
let health_response () =
  Yojson.Safe.to_string (`Assoc [
    ("status", `String "ok");
    ("server", `String "llm-mcp");
    ("transport", `String "http");
    ("language", `String "ocaml");
  ])

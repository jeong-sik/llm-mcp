(** Agent Loop for Ollama Tool Calling - Pure Eio Version

    Implements the agentic loop pattern:
    1. Send prompt to Ollama with available tools
    2. If Ollama returns tool_calls, execute them
    3. Send tool results back to Ollama
    4. Repeat until no more tool_calls or max_turns reached

    This enables tool-capable models (devstral, qwen3, llama3.3)
    to use MCP tools natively through llm-mcp.

    Pure Eio: Uses Cohttp_eio for HTTP, Eio.Fiber for parallelism.
*)

open Ollama_parser

(** Maximum turns for agentic loop *)
let default_max_turns = 10

(** Ollama API endpoint *)
let ollama_base_url = "http://127.0.0.1:11434"

(** Conversation message type *)
type message = {
  role : string;
  content : string;
  tool_calls : tool_call list option;
  name : string option;  (* for tool response messages *)
}

(** Conversation state *)
type conversation_state = {
  messages : message list;
  turn : int;
  model : string;
  temperature : float;
  tools : Yojson.Safe.t list;
}

(** Convert message to JSON *)
let message_to_json msg =
  let base = [
    ("role", `String msg.role);
    ("content", `String msg.content);
  ] in
  let with_name = match msg.name with
    | Some n -> base @ [("name", `String n)]
    | None -> base
  in
  `Assoc with_name

(** Build Ollama chat request *)
let build_chat_request state prompt =
  let user_msg = { role = "user"; content = prompt; tool_calls = None; name = None } in
  let all_messages = state.messages @ [user_msg] in
  `Assoc [
    ("model", `String state.model);
    ("messages", `List (List.map message_to_json all_messages));
    ("stream", `Bool false);
    ("options", `Assoc [
      ("temperature", `Float state.temperature);
    ]);
    ("tools", `List state.tools);
  ]

(** Build chat request with tool results *)
let build_continuation_request state tool_results =
  let tool_messages = List.map (fun (name, result) ->
    { role = "tool"; content = result; tool_calls = None; name = Some name }
  ) tool_results in
  let all_messages = state.messages @ tool_messages in
  `Assoc [
    ("model", `String state.model);
    ("messages", `List (List.map message_to_json all_messages));
    ("stream", `Bool false);
    ("options", `Assoc [
      ("temperature", `Float state.temperature);
    ]);
    ("tools", `List state.tools);
  ]

(** Call Ollama chat API using Eio HTTP client *)
let call_ollama_chat ~sw ~net request =
  let uri = Uri.of_string (ollama_base_url ^ "/api/chat") in
  let body = Yojson.Safe.to_string request in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body_source = Eio.Flow.string_source body in
  let client = Cohttp_eio.Client.make ~https:None net in

  try
    let resp, resp_body = Cohttp_eio.Client.post client ~sw ~headers ~body:body_source uri in
    let body_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_int in
    let status = Cohttp.Response.status resp in
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
      Ok body_str
    else
      Error (Printf.sprintf "Ollama error: %s" body_str)
  with e ->
    Error (Printf.sprintf "Connection error: %s" (Printexc.to_string e))

(** Built-in tool executor type - simple name + function *)
type tool_executor_fn = Yojson.Safe.t -> (string, string) result

(** Default tool executors registry *)
let builtin_executors : (string * tool_executor_fn) list ref = ref []

(** Register a tool executor *)
let register_executor name fn =
  builtin_executors := (name, fn) :: !builtin_executors

(** Find executor by name *)
let find_executor name =
  List.assoc_opt name !builtin_executors

(** Execute a tool call *)
let execute_tool_call ~sw ~net ~clock (tc : tool_call) ~external_mcp_url =
  let args = try Yojson.Safe.from_string tc.arguments with Yojson.Json_error _ -> `Null in

  (* First check if we have a built-in executor *)
  match find_executor tc.name with
  | Some executor_fn ->
      let result = executor_fn args in
      (tc.name, match result with Ok s -> s | Error e -> "Error: " ^ e)
  | None ->
      (* Fall back to external MCP call *)
      match external_mcp_url with
      | Some url ->
          let result = Mcp_client_eio.call_tool_once ~sw ~net ~clock ~url ~tool_name:tc.name ~arguments:args in
          (tc.name, match result with
            | Ok json -> Yojson.Safe.to_string json
            | Error e -> "Error: " ^ e)
      | None ->
          (tc.name, Printf.sprintf "Error: Unknown tool '%s' and no external MCP configured" tc.name)

(** Execute multiple tool calls in parallel using Eio fibers *)
let execute_tool_calls ~sw ~net ~clock tool_calls ~external_mcp_url =
  let max_tool_calls =
    match Sys.getenv_opt "LLM_MCP_MAX_TOOL_CALLS_PER_TURN" with
    | Some v ->
        (try max 0 (int_of_string (String.trim v)) with _ -> 16)
    | None -> 16
  in
  let tool_calls =
    if max_tool_calls <= 0 then tool_calls
    else
      let total = List.length tool_calls in
      if total <= max_tool_calls then tool_calls
      else
        let rec take acc n lst =
          if n <= 0 then List.rev acc
          else match lst with
            | [] -> List.rev acc
            | x :: xs -> take (x :: acc) (n - 1) xs
        in
        Printf.eprintf "[llm-mcp] tool_calls capped: %d -> %d\n%!" total max_tool_calls;
        take [] max_tool_calls tool_calls
  in
  let serial =
    match Sys.getenv_opt "LLM_MCP_TOOL_CALLS_SERIAL" with
    | Some v ->
        let v = String.lowercase_ascii (String.trim v) in
        v = "1" || v = "true" || v = "yes"
    | None -> false
  in
  if serial then
    List.map (fun tc -> execute_tool_call ~sw ~net ~clock tc ~external_mcp_url) tool_calls
  else
    Eio.Fiber.List.map (fun tc -> execute_tool_call ~sw ~net ~clock tc ~external_mcp_url) tool_calls

(** Single agent turn result *)
type turn_result =
  | TurnDone of string                (* Final text response *)
  | TurnContinue of tool_call list    (* Tool calls to execute *)
  | TurnError of string               (* Error message *)

(** Execute single agent turn *)
let execute_turn ~sw ~net _state request_body =
  let result = call_ollama_chat ~sw ~net request_body in

  match result with
  | Error err -> TurnError err
  | Ok body_str ->
      match parse_chat_result body_str with
      | Error e -> TurnError e
      | Ok (TextResponse (text, thinking)) ->
          let response = match thinking with
            | Some t -> Printf.sprintf "[Thinking]\n%s\n\n[Response]\n%s" t text
            | None -> text
          in
          TurnDone response
      | Ok (ToolCalls (calls, _thinking)) -> TurnContinue calls
      | Ok (TextWithTools (text, calls, thinking)) ->
          (* Model provided both text and tool calls - process tool calls *)
          if calls = [] then
            let response = match thinking with
              | Some t -> Printf.sprintf "[Thinking]\n%s\n\n[Response]\n%s" t text
              | None -> text
            in
            TurnDone response
          else TurnContinue calls

(** Run agent loop *)
let run
    ~sw
    ~net
    ~clock
    ~model
    ~prompt
    ?(system_prompt = "You are a helpful assistant with access to tools.")
    ?(temperature = 0.7)
    ?(max_turns = default_max_turns)
    ?(external_mcp_url = None)
    ~tools
    ?(on_turn = fun _ _ -> ())
    () =
  (* Convert tools to Ollama format *)
  let ollama_tools = List.map (fun (t : Types.tool_schema) ->
    `Assoc [
      ("type", `String "function");
      ("function", `Assoc [
        ("name", `String t.name);
        ("description", `String t.description);
        ("parameters", t.input_schema);
      ]);
    ]
  ) tools in

  let initial_state = {
    messages = [{ role = "system"; content = system_prompt; tool_calls = None; name = None }];
    turn = 0;
    model;
    temperature;
    tools = ollama_tools;
  } in

  (* Helper to process tool calls and return updated state *)
  let rec process_tool_calls state tool_calls =
    if state.turn >= max_turns then
      Error (Printf.sprintf "Max turns (%d) reached" max_turns)
    else begin
      (* Execute tool calls in parallel *)
      let tool_results = execute_tool_calls ~sw ~net ~clock tool_calls ~external_mcp_url in

      (* Build assistant message with tool calls indicator *)
      let assistant_msg = {
        role = "assistant";
        content = "";
        tool_calls = Some tool_calls;
        name = None;
      } in

      (* Build tool result messages *)
      let tool_msgs = List.map (fun (name, result) ->
        { role = "tool"; content = result; tool_calls = None; name = Some name }
      ) tool_results in

      let new_state = {
        state with
        messages = state.messages @ [assistant_msg] @ tool_msgs;
        turn = state.turn + 1;
      } in

      (* Send continuation request with tool results *)
      let continuation_request = build_continuation_request new_state [] in
      let next_result = execute_turn ~sw ~net new_state continuation_request in

      match next_result with
      | TurnDone text -> Ok text
      | TurnError err -> Error err
      | TurnContinue more_calls ->
          (* Recursively process more tool calls *)
          process_tool_calls new_state more_calls
    end
  in

  let loop state current_prompt =
    if state.turn >= max_turns then
      Error (Printf.sprintf "Max turns (%d) reached" max_turns)
    else begin
      on_turn state.turn current_prompt;

      let user_msg = { role = "user"; content = current_prompt; tool_calls = None; name = None } in
      let state_with_user = { state with messages = state.messages @ [user_msg] } in

      let request_body = build_chat_request state current_prompt in
      let result = execute_turn ~sw ~net state request_body in

      match result with
      | TurnDone text -> Ok text
      | TurnError err -> Error err
      | TurnContinue tool_calls ->
          process_tool_calls state_with_user tool_calls
    end
  in

  loop initial_state prompt

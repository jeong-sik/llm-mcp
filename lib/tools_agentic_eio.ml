open Printf
open Types
open Cli_runner_eio

let ollama_base_url () = Tools_ollama_agentic.base_url ()

type agent_message = Tools_ollama_agentic.agent_message = {
  role : string;
  content : string;
  tool_calls : Ollama_parser.tool_call list option;
  name : string option;
}

let agent_message_to_json = Tools_ollama_agentic.agent_message_to_json
let build_agentic_request = Tools_ollama_agentic.build_chat_request

let call_ollama_chat_eio ~sw ~proc_mgr ~clock ~timeout request_json =
  let body = Yojson.Safe.to_string request_json in
  let url = ollama_base_url () ^ "/api/chat" in
  let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" [
    "-s"; "-X"; "POST"; url;
    "-H"; "Content-Type: application/json";
    "-d"; body
  ] in
  match result with
  | Error (Timeout t) -> Error (sprintf "Timeout after %ds" t)
  | Error (ProcessError msg) -> Error (sprintf "Connection error: %s" msg)
  | Ok r ->
      if r.exit_code <> 0 then
        Error (sprintf "curl failed with exit code %d: %s" r.exit_code r.stderr)
      else
        Ok r.stdout

let execute_tool_call_eio ~sw ~proc_mgr ~clock ~timeout ~external_mcp_url (tc : Ollama_parser.tool_call) =
  let args = try Yojson.Safe.from_string tc.arguments with _ -> `Null in
  match external_mcp_url with
  | Some url ->
      let request_body = `Assoc [
        ("jsonrpc", `String "2.0");
        ("id", `Int 1);
        ("method", `String "tools/call");
        ("params", `Assoc [
          ("name", `String tc.name);
          ("arguments", args);
        ]);
      ] |> Yojson.Safe.to_string in
      let result = run_command ~sw ~proc_mgr ~clock ~timeout "curl" [
        "-s"; "-X"; "POST"; url;
        "-H"; "Content-Type: application/json";
        "-d"; request_body
      ] in
      (match result with
      | Error _ -> (tc.name, sprintf "Error: MCP call to %s failed" tc.name)
      | Ok r ->
          try
            let json = Yojson.Safe.from_string r.stdout in
            let open Yojson.Safe.Util in
            let result = json |> member "result" in
            let error = json |> member "error" in
            if error <> `Null then
              let msg = Safe_parse.json_string ~context:"mcp:error" ~default:"Unknown error" error "message" in
              (tc.name, sprintf "Error: %s" msg)
            else
              let content = result |> member "content" in
              match content with
              | `List items ->
                  let texts = List.filter_map (fun item ->
                    match item |> member "type" |> to_string_option with
                    | Some "text" -> item |> member "text" |> to_string_option
                    | _ -> None
                  ) items in
                  (tc.name, String.concat "\n" texts)
              | _ -> (tc.name, r.stdout)
          with _ -> (tc.name, r.stdout))
  | None ->
      (tc.name, sprintf "Error: Unknown tool '%s' and no external MCP configured" tc.name)

type turn_result =
  | TurnDone of string
  | TurnContinue of Ollama_parser.tool_call list
  | TurnError of string

let execute_agentic_turn ~sw ~proc_mgr ~clock ~timeout request_body =
  let result = call_ollama_chat_eio ~sw ~proc_mgr ~clock ~timeout request_body in
  match result with
  | Error err -> TurnError err
  | Ok body_str ->
      match Ollama_parser.parse_chat_result body_str with
      | Error e -> TurnError e
      | Ok (Ollama_parser.TextResponse (text, thinking)) ->
          let response = match thinking with
            | Some t -> sprintf "[Thinking]\n%s\n\n[Response]\n%s" t text
            | None -> text
          in
          TurnDone response
      | Ok (Ollama_parser.ToolCalls (calls, _thinking)) -> TurnContinue calls
      | Ok (Ollama_parser.TextWithTools (text, calls, thinking)) ->
          if calls = [] then
            let response = match thinking with
              | Some t -> sprintf "[Thinking]\n%s\n\n[Response]\n%s" t text
              | None -> text
            in
            TurnDone response
          else TurnContinue calls

let execute_ollama_agentic
    ~sw ~proc_mgr ~clock
    ~(model : string)
    ~(prompt : string)
    ~(system_prompt : string option)
    ~(temperature : float)
    ~(timeout : int)
    ~(tools : Types.tool_schema list)
    ?external_mcp_url
    ?(on_turn : int -> string -> unit = fun _ _ -> ())
    () : tool_result =
  let max_turns = 10 in
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
  let system = match system_prompt with
    | Some s -> s
    | None -> "You are a helpful assistant with access to tools."
  in
  let initial_messages = [
    { role = "system"; content = system; tool_calls = None; name = None }
  ] in
  let rec process_tool_calls messages turn tool_calls =
    if turn >= max_turns then
      Error (sprintf "Max turns (%d) reached" max_turns)
    else begin
      let tool_results = List.map (fun tc ->
        execute_tool_call_eio ~sw ~proc_mgr ~clock ~timeout ~external_mcp_url tc
      ) tool_calls in
      let assistant_msg = {
        role = "assistant";
        content = "";
        tool_calls = Some tool_calls;
        name = None;
      } in
      let tool_msgs = List.map (fun (name, result) ->
        { role = "tool"; content = result; tool_calls = None; name = Some name }
      ) tool_results in
      let new_messages = messages @ [assistant_msg] @ tool_msgs in
      let request = build_agentic_request ~model ~temperature ~tools:ollama_tools new_messages in
      match execute_agentic_turn ~sw ~proc_mgr ~clock ~timeout request with
      | TurnDone text -> Ok text
      | TurnError err -> Error err
      | TurnContinue more_calls ->
          process_tool_calls new_messages (turn + 1) more_calls
    end
  in
  let run_loop () =
    on_turn 0 prompt;
    let user_msg = { role = "user"; content = prompt; tool_calls = None; name = None } in
    let messages = initial_messages @ [user_msg] in
    let request = build_agentic_request ~model ~temperature ~tools:ollama_tools messages in
    match execute_agentic_turn ~sw ~proc_mgr ~clock ~timeout request with
    | TurnDone text -> Ok text
    | TurnError err -> Error err
    | TurnContinue tool_calls ->
        process_tool_calls messages 1 tool_calls
  in
  match run_loop () with
  | Ok response ->
      {
        model = sprintf "ollama (%s) [agentic]" model;
        returncode = 0;
        response;
        extra = [
          ("temperature", sprintf "%.1f" temperature);
          ("local", "true");
          ("agentic", "true");
          ("tools_count", string_of_int (List.length tools));
        ];
      }
  | Error err ->
      {
        model = sprintf "ollama (%s) [agentic]" model;
        returncode = -1;
        response = sprintf "Agent loop error: %s" err;
        extra = [
          ("temperature", sprintf "%.1f" temperature);
          ("local", "true");
          ("agentic", "true");
          ("error", "true");
        ];
      }

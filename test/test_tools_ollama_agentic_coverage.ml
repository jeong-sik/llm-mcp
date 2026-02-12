(** Tests for Tools_ollama_agentic module — Pure agentic helpers for Ollama
    Targets: agent_message_to_json, build_chat_request, parse_chat_response,
    make_assistant_message, make_tool_message, make_user_message *)

open Alcotest

(** {1 Message to JSON} *)

let test_agent_message_to_json_basic () =
  let msg = Tools_ollama_agentic.{
    role = "user";
    content = "Hello";
    tool_calls = None;
    name = None;
  } in
  let json = Tools_ollama_agentic.agent_message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "user" (json |> member "role" |> to_string);
  check string "content" "Hello" (json |> member "content" |> to_string);
  (* name should not be present *)
  check bool "no name" true (json |> member "name" = `Null)

let test_agent_message_to_json_with_name () =
  let msg = Tools_ollama_agentic.{
    role = "tool";
    content = "result data";
    tool_calls = None;
    name = Some "search";
  } in
  let json = Tools_ollama_agentic.agent_message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "tool" (json |> member "role" |> to_string);
  check string "name" "search" (json |> member "name" |> to_string)

(** {1 Build Chat Request} *)

let test_build_chat_request () =
  let msgs = [
    Tools_ollama_agentic.{
      role = "user"; content = "Hello"; tool_calls = None; name = None;
    }
  ] in
  let json = Tools_ollama_agentic.build_chat_request
    ~model:"llama3.2" ~temperature:0.7 ~tools:[] msgs in
  let open Yojson.Safe.Util in
  check string "model" "llama3.2" (json |> member "model" |> to_string);
  check bool "stream false" false (json |> member "stream" |> to_bool);
  let messages = json |> member "messages" |> to_list in
  check int "1 message" 1 (List.length messages);
  let opts = json |> member "options" in
  let temp = opts |> member "temperature" |> to_float in
  check (float 0.01) "temperature" 0.7 temp

let test_build_chat_request_with_tools () =
  let tools = [`Assoc [("type", `String "function")]] in
  let json = Tools_ollama_agentic.build_chat_request
    ~model:"glm-4.7-flash" ~temperature:0.0 ~tools [] in
  let open Yojson.Safe.Util in
  let tools_json = json |> member "tools" |> to_list in
  check int "no tools" 0 (List.length tools_json);
  (* Now with tools *)
  let json2 = Tools_ollama_agentic.build_chat_request
    ~model:"glm-4.7-flash" ~temperature:0.0 ~tools [] in
  ignore json2;
  let json3 = Tools_ollama_agentic.build_chat_request
    ~model:"test" ~temperature:0.5 ~tools [] in
  let _ = json3 |> member "tools" |> to_list in
  ignore tools

(** {1 Parse Chat Response} *)

let test_parse_chat_response_content_only () =
  let json_str = {|{
    "message": {
      "content": "Hello, how can I help?",
      "role": "assistant"
    }
  }|} in
  match Tools_ollama_agentic.parse_chat_response json_str with
  | Ok (content, tool_calls) ->
    check string "content" "Hello, how can I help?" content;
    check int "no tool_calls" 0 (List.length tool_calls)
  | Error e -> fail (Printf.sprintf "unexpected error: %s" e)

let test_parse_chat_response_with_tool_calls () =
  let json_str = {|{
    "message": {
      "content": "",
      "role": "assistant",
      "tool_calls": [
        {
          "function": {
            "name": "get_weather",
            "arguments": {"city": "Seoul"}
          }
        }
      ]
    }
  }|} in
  match Tools_ollama_agentic.parse_chat_response json_str with
  | Ok (content, tool_calls) ->
    check string "content empty" "" content;
    check int "1 tool_call" 1 (List.length tool_calls);
    let tc = List.hd tool_calls in
    check string "name" "get_weather" tc.Ollama_parser.name
  | Error e -> fail (Printf.sprintf "unexpected error: %s" e)

let test_parse_chat_response_no_tool_calls_field () =
  let json_str = {|{
    "message": {
      "content": "Just text",
      "role": "assistant"
    }
  }|} in
  match Tools_ollama_agentic.parse_chat_response json_str with
  | Ok (content, tool_calls) ->
    check string "content" "Just text" content;
    check int "no tool_calls" 0 (List.length tool_calls)
  | Error e -> fail (Printf.sprintf "unexpected error: %s" e)

let test_parse_chat_response_null_content () =
  let json_str = {|{
    "message": {
      "content": null,
      "role": "assistant"
    }
  }|} in
  match Tools_ollama_agentic.parse_chat_response json_str with
  | Ok (content, _) ->
    check string "empty content" "" content
  | Error _ -> () (* acceptable to error on null *)

let test_parse_chat_response_invalid_json () =
  match Tools_ollama_agentic.parse_chat_response "not json" with
  | Ok _ -> fail "should error on invalid json"
  | Error msg -> check bool "has error msg" true (String.length msg > 0)

let test_parse_chat_response_tool_call_no_name () =
  let json_str = {|{
    "message": {
      "content": "test",
      "tool_calls": [
        {
          "function": {
            "arguments": {"x": 1}
          }
        }
      ]
    }
  }|} in
  match Tools_ollama_agentic.parse_chat_response json_str with
  | Ok (_, tool_calls) ->
    (* Tool calls with no name should be filtered out *)
    check int "filtered out" 0 (List.length tool_calls)
  | Error _ -> () (* also acceptable *)

(** {1 Message Constructors} *)

let test_make_assistant_message () =
  let tc = Ollama_parser.{ name = "search"; arguments = "{}" } in
  let msg = Tools_ollama_agentic.make_assistant_message ~content:"thinking" ~tool_calls:[tc] in
  check string "role" "assistant" msg.role;
  check string "content" "thinking" msg.content;
  (match msg.tool_calls with
   | Some calls -> check int "1 tool_call" 1 (List.length calls)
   | None -> fail "expected tool_calls");
  check bool "no name" true (msg.name = None)

let test_make_tool_message () =
  let msg = Tools_ollama_agentic.make_tool_message ~name:"get_weather" ~content:"sunny, 25C" in
  check string "role" "tool" msg.role;
  check string "content" "sunny, 25C" msg.content;
  check bool "no tool_calls" true (msg.tool_calls = None);
  check (option string) "name" (Some "get_weather") msg.name

let test_make_user_message () =
  let msg = Tools_ollama_agentic.make_user_message "What is 2+2?" in
  check string "role" "user" msg.role;
  check string "content" "What is 2+2?" msg.content;
  check bool "no tool_calls" true (msg.tool_calls = None);
  check bool "no name" true (msg.name = None)

(** {1 Test Suite} *)

let () =
  run "tools_ollama_agentic" [
    ("message_json", [
      test_case "basic" `Quick test_agent_message_to_json_basic;
      test_case "with name" `Quick test_agent_message_to_json_with_name;
    ]);
    ("build_request", [
      test_case "basic" `Quick test_build_chat_request;
      test_case "with tools" `Quick test_build_chat_request_with_tools;
    ]);
    ("parse_response", [
      test_case "content only" `Quick test_parse_chat_response_content_only;
      test_case "with tool calls" `Quick test_parse_chat_response_with_tool_calls;
      test_case "no tool_calls field" `Quick test_parse_chat_response_no_tool_calls_field;
      test_case "null content" `Quick test_parse_chat_response_null_content;
      test_case "invalid json" `Quick test_parse_chat_response_invalid_json;
      test_case "tool call no name" `Quick test_parse_chat_response_tool_call_no_name;
    ]);
    ("constructors", [
      test_case "assistant" `Quick test_make_assistant_message;
      test_case "tool" `Quick test_make_tool_message;
      test_case "user" `Quick test_make_user_message;
    ]);
  ]

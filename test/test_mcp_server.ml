(** Tests for MCP Server module *)

open Alcotest
open Lwt.Syntax
open Llm_mcp.Mcp_server

(** Helper to run Lwt test *)
let lwt_run f () =
  Lwt_main.run (f ())

(** Helper to call handle_request with defaults *)
let handle_request_simple request =
  let+ (_session_opt, response) = handle_request ~session_opt:None ~wants_stream:false request in
  response

(** Test initialize request handling *)
let test_handle_initialize () =
  let request = {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|} in
  let+ response = handle_request_simple request in
  match response with
  | JsonResponse json ->
      let open Yojson.Safe.Util in
      let id = json |> member "id" |> to_int in
      let result = json |> member "result" in
      let protocol_version = result |> member "protocolVersion" |> to_string in
      let server_name = result |> member "serverInfo" |> member "name" |> to_string in
      Alcotest.(check int) "id matches" 1 id;
      Alcotest.(check string) "protocol version" "2025-11-25" protocol_version;
      Alcotest.(check string) "server name" "llm-mcp" server_name
  | _ -> Alcotest.fail "Expected JsonResponse"

(** Test initialized notification (no response) *)
let test_handle_initialized () =
  let request = {|{"jsonrpc":"2.0","method":"initialized"}|} in
  let+ response = handle_request_simple request in
  Alcotest.(check bool) "NoResponse for notification" true (response = NoResponse)

(** Test tools/list request *)
let test_handle_list_tools () =
  let request = {|{"jsonrpc":"2.0","id":2,"method":"tools/list"}|} in
  let+ response = handle_request_simple request in
  match response with
  | JsonResponse json ->
      let open Yojson.Safe.Util in
      let id = json |> member "id" |> to_int in
      let tools = json |> member "result" |> member "tools" |> to_list in
      Alcotest.(check int) "id matches" 2 id;
      Alcotest.(check int) "has 14 tools" 14 (List.length tools);  (* gemini, claude-cli, codex, ollama, ollama_list, chain.*, gh_pr_diff *)
      let tool_names = List.map (fun t -> t |> member "name" |> to_string) tools in
      Alcotest.(check bool) "gemini exists" true (List.mem "gemini" tool_names);
      Alcotest.(check bool) "claude-cli exists" true (List.mem "claude-cli" tool_names);
      Alcotest.(check bool) "codex exists" true (List.mem "codex" tool_names);
      Alcotest.(check bool) "ollama exists" true (List.mem "ollama" tool_names);
      Alcotest.(check bool) "ollama_list exists" true (List.mem "ollama_list" tool_names);
      Alcotest.(check bool) "chain.run exists" true (List.mem "chain.run" tool_names);
      Alcotest.(check bool) "chain.validate exists" true (List.mem "chain.validate" tool_names);
      Alcotest.(check bool) "chain.to_mermaid exists" true (List.mem "chain.to_mermaid" tool_names)
  | _ -> Alcotest.fail "Expected JsonResponse"

(** Test unknown method error *)
let test_handle_unknown_method () =
  let request = {|{"jsonrpc":"2.0","id":3,"method":"unknown/method"}|} in
  let+ response = handle_request_simple request in
  match response with
  | JsonResponse json ->
      let open Yojson.Safe.Util in
      let id = json |> member "id" |> to_int in
      let error = json |> member "error" in
      let code = error |> member "code" |> to_int in
      let message = error |> member "message" |> to_string in
      Alcotest.(check int) "id matches" 3 id;
      Alcotest.(check int) "error code -32601" (-32601) code;
      Alcotest.(check bool) "message contains method name" true
        (String.sub message 0 16 = "Method not found")
  | _ -> Alcotest.fail "Expected JsonResponse"

(** Test invalid JSON error (caught as internal error) *)
let test_handle_invalid_json () =
  let request = "not valid json" in
  let+ response = handle_request_simple request in
  match response with
  | JsonResponse json ->
      let open Yojson.Safe.Util in
      let error = json |> member "error" in
      let code = error |> member "code" |> to_int in
      (* Invalid JSON returns parse error -32700 *)
      Alcotest.(check int) "error code -32700 (parse error)" (-32700) code
  | _ -> Alcotest.fail "Expected JsonResponse"

(** Test missing params error for tools/call *)
let test_handle_call_tool_no_params () =
  let request = {|{"jsonrpc":"2.0","id":4,"method":"tools/call"}|} in
  let+ response = handle_request_simple request in
  match response with
  | JsonResponse json ->
      let open Yojson.Safe.Util in
      let id = json |> member "id" |> to_int in
      let error = json |> member "error" in
      let code = error |> member "code" |> to_int in
      let message = error |> member "message" |> to_string in
      Alcotest.(check int) "id matches" 4 id;
      Alcotest.(check int) "error code -32602 (invalid params)" (-32602) code;
      Alcotest.(check string) "message" "Missing params" message
  | _ -> Alcotest.fail "Expected JsonResponse"

(** Test health_response *)
let test_health_response () =
  let json_str = health_response () in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "status" "ok" (json |> member "status" |> to_string);
  Alcotest.(check string) "server" "llm-mcp" (json |> member "server" |> to_string);
  Alcotest.(check string) "transport" "http" (json |> member "transport" |> to_string);
  Alcotest.(check string) "language" "ocaml" (json |> member "language" |> to_string)

(** Test make_response *)
let test_make_response () =
  let response = make_response ~id:(`Int 42) (`String "result") in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "jsonrpc" "2.0" (response |> member "jsonrpc" |> to_string);
  Alcotest.(check int) "id" 42 (response |> member "id" |> to_int);
  Alcotest.(check string) "result" "result" (response |> member "result" |> to_string)

(** Test make_error *)
let test_make_error () =
  let response = make_error ~id:(`Int 99) (-32600) "Invalid Request" in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "jsonrpc" "2.0" (response |> member "jsonrpc" |> to_string);
  Alcotest.(check int) "id" 99 (response |> member "id" |> to_int);
  let error = response |> member "error" in
  Alcotest.(check int) "code" (-32600) (error |> member "code" |> to_int);
  Alcotest.(check string) "message" "Invalid Request" (error |> member "message" |> to_string)

let () =
  run "MCP Server" [
    "initialize", [
      test_case "handle initialize" `Quick (lwt_run test_handle_initialize);
      test_case "handle initialized notification" `Quick (lwt_run test_handle_initialized);
    ];
    "tools/list", [
      test_case "returns all tools" `Quick (lwt_run test_handle_list_tools);
    ];
    "error handling", [
      test_case "unknown method" `Quick (lwt_run test_handle_unknown_method);
      test_case "invalid JSON" `Quick (lwt_run test_handle_invalid_json);
      test_case "missing params" `Quick (lwt_run test_handle_call_tool_no_params);
    ];
    "helpers", [
      test_case "health_response" `Quick test_health_response;
      test_case "make_response" `Quick test_make_response;
      test_case "make_error" `Quick test_make_error;
    ];
  ]

(** Tests for MCP Server Eio

    Unit tests for session management, authentication, and request handling.
    Uses Eio_main.run for effect handling and structured concurrency.
*)

open Alcotest

module Server = Mcp_server_eio

(** {1 Test Helpers} *)

let run_eio f =
  Eio_main.run @@ fun env ->
  f env

let temp_jsonl_path prefix =
  let path = Filename.temp_file prefix ".jsonl" in
  Unix.unlink path;
  path

let write_text_file path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let with_env key value f =
  let previous = Sys.getenv_opt key in
  Unix.putenv key value;
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some old -> Unix.putenv key old
      | None -> Unix.putenv key "")
    f

(** {1 Session Store Tests} *)

(** Test session creation and retrieval *)
let test_session_create_and_get () =
  run_eio @@ fun _env ->
  let store = Server.create_session_store () in

  (* Create a session *)
  let now = Unix.gettimeofday () in
  let session : Server.session = {
    id = "test-session-001";
    protocol_version = "2025-11-25";
    created_at = now;
    last_accessed = now;
  } in

  (* Put session in store *)
  Server.put_session store session;

  (* Retrieve it *)
  let retrieved = Server.get_session store "test-session-001" in
  check (option string) "session found" (Some "test-session-001")
    (Option.map (fun s -> s.Server.id) retrieved)

(** Test session isolation - multiple sessions don't interfere *)
let test_session_isolation () =
  run_eio @@ fun _env ->
  let store = Server.create_session_store () in
  let now = Unix.gettimeofday () in

  (* Create two sessions *)
  let session1 : Server.session = {
    id = "session-A";
    protocol_version = "2025-11-25";
    created_at = now;
    last_accessed = now;
  } in
  let session2 : Server.session = {
    id = "session-B";
    protocol_version = "2024-01-01";
    created_at = now -. 100.0;
    last_accessed = now -. 50.0;
  } in

  (* Store both *)
  Server.put_session store session1;
  Server.put_session store session2;

  (* Verify isolation *)
  let retrieved1 = Server.get_session store "session-A" in
  let retrieved2 = Server.get_session store "session-B" in

  check (option string) "session A exists" (Some "session-A")
    (Option.map (fun s -> s.Server.id) retrieved1);
  check (option string) "session B exists" (Some "session-B")
    (Option.map (fun s -> s.Server.id) retrieved2);

  (* Check protocol versions are correct *)
  check (option string) "session A protocol" (Some "2025-11-25")
    (Option.map (fun s -> s.Server.protocol_version) retrieved1);
  check (option string) "session B protocol" (Some "2024-01-01")
    (Option.map (fun s -> s.Server.protocol_version) retrieved2)

(** Test session touch updates last_accessed *)
let test_session_touch () =
  run_eio @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = Server.create_session_store () in
  let now = Unix.gettimeofday () in

  (* Create session with old last_accessed *)
  let session : Server.session = {
    id = "touch-test";
    protocol_version = "2025-11-25";
    created_at = now -. 1000.0;
    last_accessed = now -. 500.0;
  } in
  Server.put_session store session;

  (* Sleep a bit *)
  Eio.Time.sleep clock 0.1;

  (* Touch the session *)
  Server.touch_session store "touch-test";

  (* Verify last_accessed was updated *)
  let updated = Server.get_session store "touch-test" in
  match updated with
  | Some s ->
      check bool "last_accessed updated" true
        (s.Server.last_accessed > now -. 500.0)
  | None ->
      fail "session not found after touch"

(** Test session removal *)
let test_session_remove () =
  run_eio @@ fun _env ->
  let store = Server.create_session_store () in
  let now = Unix.gettimeofday () in

  (* Create and store session *)
  let session : Server.session = {
    id = "remove-test";
    protocol_version = "2025-11-25";
    created_at = now;
    last_accessed = now;
  } in
  Server.put_session store session;

  (* Verify it exists *)
  check (option string) "session exists before removal" (Some "remove-test")
    (Option.map (fun s -> s.Server.id) (Server.get_session store "remove-test"));

  (* Remove it *)
  Server.remove_session store "remove-test";

  (* Verify it's gone *)
  check (option unit) "session removed" None
    (Option.map (fun _ -> ()) (Server.get_session store "remove-test"))

(** Test stale session cleanup *)
let test_session_cleanup () =
  run_eio @@ fun _env ->
  let store = Server.create_session_store () in
  let now = Unix.gettimeofday () in

  (* Create fresh session (should NOT be removed) *)
  let fresh : Server.session = {
    id = "fresh-session";
    protocol_version = "2025-11-25";
    created_at = now;
    last_accessed = now;
  } in

  (* Create stale session (last accessed > 1 hour ago, SHOULD be removed) *)
  let stale : Server.session = {
    id = "stale-session";
    protocol_version = "2025-11-25";
    created_at = now -. 7200.0;  (* 2 hours ago *)
    last_accessed = now -. 3700.0;  (* 1 hour 1 minute ago *)
  } in

  (* Store both *)
  Server.put_session store fresh;
  Server.put_session store stale;

  (* Run cleanup *)
  Server.cleanup_stale_sessions store;

  (* Verify fresh session still exists *)
  check (option string) "fresh session remains" (Some "fresh-session")
    (Option.map (fun s -> s.Server.id) (Server.get_session store "fresh-session"));

  (* Verify stale session was removed *)
  check (option unit) "stale session removed" None
    (Option.map (fun _ -> ()) (Server.get_session store "stale-session"))

(** Test concurrent access to session store *)
let test_concurrent_session_access () =
  run_eio @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let store = Server.create_session_store () in
  let now = Unix.gettimeofday () in

  (* Launch 10 concurrent fibers that create sessions *)
  let fibers = List.init 10 (fun i ->
    Eio.Fiber.fork_promise ~sw (fun () ->
      let session : Server.session = {
        id = Printf.sprintf "concurrent-%d" i;
        protocol_version = "2025-11-25";
        created_at = now;
        last_accessed = now;
      } in
      Server.put_session store session;
      i
    )
  ) in

  (* Wait for all fibers *)
  let results = List.map Eio.Promise.await fibers in
  check int "all fibers completed" 10 (List.length results);

  (* Verify all sessions were created *)
  List.iteri (fun i _ ->
    let session_id = Printf.sprintf "concurrent-%d" i in
    let found = Server.get_session store session_id in
    check (option string) (Printf.sprintf "session %d exists" i) (Some session_id)
      (Option.map (fun s -> s.Server.id) found)
  ) results

(** {1 Authentication Tests} *)

(** Test auth with no env var requires key by default *)
let test_auth_no_env_requires_key () =
  let original_key = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  let headers = [("content-type", "application/json")] in
  let result = Server.auth_middleware headers in

  (match original_key with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> ());
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> ());

  match result with
  | Error _ -> check bool "missing key rejected" true true
  | Ok () -> fail "auth should require key by default"

(** Test auth allows no-auth when explicit env override is set *)
let test_auth_allow_no_auth_env () =
  let original_key = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "1";

  let headers = [("content-type", "application/json")] in
  let result = Server.auth_middleware headers in

  (match original_key with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> ());
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> ());

  match result with
  | Ok () -> check bool "allow-no-auth accepted" true true
  | Error _ -> fail "auth should pass when allow-no-auth is set"

(** Test auth with env var requires valid token *)
let test_auth_with_env_requires_token () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "test-secret-123";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  (* Test with valid Bearer token *)
  let headers = [("authorization", "Bearer test-secret-123")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "");

  match result with
  | Ok () -> check bool "valid token accepted" true true
  | Error msg -> fail (Printf.sprintf "auth should pass with valid token: %s" msg)

(** Test auth accepts MCP_API_KEY fallback *)
let test_auth_with_mcp_api_key_env () =
  let original_llm = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_mcp = Sys.getenv_opt "MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "";
  Unix.putenv "MCP_API_KEY" "mcp-secret-123";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  let headers = [("x-mcp-api-key", "mcp-secret-123")] in
  let result = Server.auth_middleware headers in

  (match original_llm with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");
  (match original_mcp with
   | Some v -> Unix.putenv "MCP_API_KEY" v
   | None -> Unix.putenv "MCP_API_KEY" "");
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "");

  match result with
  | Ok () -> check bool "MCP_API_KEY accepted" true true
  | Error msg -> fail (Printf.sprintf "auth should accept MCP_API_KEY: %s" msg)

(** Test auth rejects wrong token *)
let test_auth_wrong_token_rejected () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "correct-token";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  (* Test with wrong Bearer token *)
  let headers = [("authorization", "Bearer wrong-token")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "");

  match result with
  | Error _ -> check bool "wrong token rejected" true true
  | Ok () -> fail "auth should reject wrong token"

(** Test auth rejects missing Authorization header *)
let test_auth_missing_header_rejected () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "secret-key";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  (* Test with no Authorization header *)
  let headers = [("content-type", "application/json")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "");

  match result with
  | Error _ -> check bool "missing header rejected" true true
  | Ok () -> fail "auth should reject missing header"

(** Test auth rejects invalid Authorization format *)
let test_auth_invalid_format_rejected () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "secret-key";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  (* Test with invalid format (no "Bearer " prefix) *)
  let headers = [("authorization", "secret-key")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "");

  match result with
  | Error _ -> check bool "invalid format rejected" true true
  | Ok () -> fail "auth should reject invalid format"

(** Test auth accepts X-API-Key header *)
let test_auth_x_api_key_header () =
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "secret-key";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  let headers = [("x-api-key", "secret-key")] in
  let result = Server.auth_middleware headers in

  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "");

  match result with
  | Ok () -> check bool "x-api-key accepted" true true
  | Error msg -> fail (Printf.sprintf "auth should accept x-api-key: %s" msg)

(** Test auth accepts X-MCP-API-Key header *)
let test_auth_x_mcp_api_key_header () =
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let original_allow = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "secret-key";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "";

  let headers = [("x-mcp-api-key", "secret-key")] in
  let result = Server.auth_middleware headers in

  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");
  (match original_allow with
   | Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v
   | None -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "");

  match result with
  | Ok () -> check bool "x-mcp-api-key accepted" true true
  | Error msg -> fail (Printf.sprintf "auth should accept x-mcp-api-key: %s" msg)

(** {1 Header Extraction Tests} *)

(** Test extracting session ID from X-Session-Id header *)
let test_extract_session_from_header () =
  let headers = [
    ("content-type", "application/json");
    ("x-session-id", "my-session-123");
  ] in
  let session_id = Server.extract_session_id None headers in
  check (option string) "session from header" (Some "my-session-123") session_id

(** Test extracting session ID from params *)
let test_extract_session_from_param () =
  let params = Some (`Assoc [
    ("sessionId", `String "param-session-456");
    ("other", `String "value");
  ]) in
  let headers = [] in
  let session_id = Server.extract_session_id params headers in
  check (option string) "session from param" (Some "param-session-456") session_id

(** Test header takes precedence over param *)
let test_extract_session_header_precedence () =
  let params = Some (`Assoc [
    ("sessionId", `String "param-session");
  ]) in
  let headers = [("x-session-id", "header-session")] in
  let session_id = Server.extract_session_id params headers in
  check (option string) "header takes precedence" (Some "header-session") session_id

(** Test case-insensitive header matching *)
let test_extract_session_case_insensitive () =
  let headers = [
    ("Content-Type", "application/json");
    ("X-SESSION-ID", "uppercase-session");
  ] in
  let session_id = Server.extract_session_id None headers in
  check (option string) "case insensitive match" (Some "uppercase-session") session_id

(** Test returns None when no session ID found *)
let test_extract_session_not_found () =
  let headers = [("content-type", "application/json")] in
  let params = Some (`Assoc [("other", `String "value")]) in
  let session_id = Server.extract_session_id params headers in
  check (option unit) "no session found" None
    (Option.map (fun _ -> ()) session_id)

(** {1 Session ID Generation Tests} *)

(** Test session ID format *)
let test_generate_session_id_format () =
  let id = Server.generate_session_id () in
  (* Format: eio-{pid}-{timestamp}-{random8} *)
  check bool "starts with eio-" true (String.starts_with ~prefix:"eio-" id);
  check bool "has reasonable length" true (String.length id > 15)

(** Test session IDs are unique *)
let test_generate_session_id_unique () =
  let id1 = Server.generate_session_id () in
  let id2 = Server.generate_session_id () in
  check bool "ids are unique" true (id1 <> id2)

(** {1 tools/list Tests} *)

let tools_list_response ?(params = `Assoc []) () =
  run_eio @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let store = Server.create_session_store () in
  let request =
    Yojson.Safe.to_string
      (`Assoc
        [
          ("jsonrpc", `String "2.0");
          ("id", `Int 1);
          ("method", `String "tools/list");
          ("params", params);
        ])
  in
  let (_session_id, response) =
    Server.handle_request ~sw ~proc_mgr ~clock ~store ~headers:[] request
  in
  response

let tools_from_response response =
  let open Yojson.Safe.Util in
  response |> member "result" |> member "tools" |> to_list

let find_tool tools name =
  List.find_map
    (function
      | `Assoc fields as tool -> (
          match List.assoc_opt "name" fields with
          | Some (`String actual) when String.equal actual name -> Some tool
          | _ -> None)
      | _ -> None)
    tools

let tool_string_field tool field =
  match tool with
  | `Assoc fields -> (
      match List.assoc_opt field fields with
      | Some (`String value) -> value
      | _ -> fail (Printf.sprintf "missing field %s" field))
  | _ -> fail "tool entry is not an object"

let test_tools_list_default_hidden_utilities () =
  let tools = tools_from_response (tools_list_response ()) in
  let names =
    List.filter_map
      (function
        | `Assoc fields -> (
            match List.assoc_opt "name" fields with
            | Some (`String name) -> Some name
            | _ -> None)
        | _ -> None)
      tools
  in
  check bool "gh_pr_diff hidden" false (List.mem "gh_pr_diff" names);
  check bool "slack_post hidden" false (List.mem "slack_post" names);
  check bool "set_stream_delta hidden" false (List.mem "set_stream_delta" names);
  check bool "get_stream_delta hidden" false (List.mem "get_stream_delta" names);
  check bool "gemini visible" true (List.mem "gemini" names);
  check bool "chain.run visible" true (List.mem "chain.run" names);
  check bool "prompt.register visible" true (List.mem "prompt.register" names)

let test_tools_list_include_hidden_metadata () =
  let tools =
    tools_from_response
      (tools_list_response ~params:(`Assoc [ ("include_hidden", `Bool true) ]) ())
  in
  let gh_pr_diff =
    match find_tool tools "gh_pr_diff" with
    | Some tool -> tool
    | None -> fail "gh_pr_diff missing"
  in
  check string "gh_pr_diff visibility" "hidden"
    (tool_string_field gh_pr_diff "visibility");
  check string "gh_pr_diff lifecycle" "active"
    (tool_string_field gh_pr_diff "lifecycle");
  let stream_toggle =
    match find_tool tools "set_stream_delta" with
    | Some tool -> tool
    | None -> fail "set_stream_delta missing"
  in
  check string "set_stream_delta visibility" "hidden"
    (tool_string_field stream_toggle "visibility");
  check string "set_stream_delta lifecycle" "active"
    (tool_string_field stream_toggle "lifecycle")

let test_tools_list_include_usage_metadata () =
  let telemetry_path = temp_jsonl_path "llm_tool_usage_" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists telemetry_path then Unix.unlink telemetry_path)
    (fun () ->
      write_text_file telemetry_path
        {|{"timestamp":100.0,"event":["Tool_called",{"tool_name":"gemini","success":true,"duration_ms":10,"agent_id":null,"url":"http://localhost","error":null}]}
{"timestamp":125.0,"event":["Tool_called",{"tool_name":"gemini","success":false,"duration_ms":20,"agent_id":null,"url":"http://localhost","error":"boom"}]}
{"timestamp":130.0,"event":["Tool_called",{"tool_name":"chain.run","success":true,"duration_ms":30,"agent_id":null,"url":"http://localhost","error":null}]}
|}
      ;
      with_env "LLM_MCP_TELEMETRY_FILE" telemetry_path @@ fun () ->
      let response =
        tools_list_response
          ~params:(`Assoc [ ("include_usage", `Bool true) ]) ()
      in
      let open Yojson.Safe.Util in
      check bool "telemetry available" true
        (response |> member "result" |> member "usageTelemetryAvailable" |> to_bool);
      check string "telemetry path" telemetry_path
        (response |> member "result" |> member "usageTelemetryPath" |> to_string);
      check int "usage total calls" 3
        (response |> member "result" |> member "usageTotalCalls" |> to_int);
      let tools = tools_from_response response in
      let gemini =
        match find_tool tools "gemini" with
        | Some tool -> tool
        | None -> fail "gemini missing"
      in
      check int "gemini usageCount" 2
        (gemini |> member "usageCount" |> to_int);
      check int "gemini successCount" 1
        (gemini |> member "usageSuccessCount" |> to_int);
      check int "gemini failureCount" 1
        (gemini |> member "usageFailureCount" |> to_int);
      check (float 0.001) "gemini lastUsedAt" 125.0
        (gemini |> member "usageLastUsedAt" |> to_float);
      let prompt_register =
        match find_tool tools "prompt.register" with
        | Some tool -> tool
        | None -> fail "prompt.register missing"
      in
      check int "unused tool usageCount" 0
        (prompt_register |> member "usageCount" |> to_int))

(** {1 Test Suite} *)

let () =
  Alcotest.run "MCP Server Eio" [
    "session_store", [
      test_case "create and get" `Quick test_session_create_and_get;
      test_case "session isolation" `Quick test_session_isolation;
      test_case "touch updates last_accessed" `Quick test_session_touch;
      test_case "remove session" `Quick test_session_remove;
      test_case "cleanup stale sessions" `Quick test_session_cleanup;
      test_case "concurrent access" `Quick test_concurrent_session_access;
    ];

    "authentication", [
      test_case "no env requires key" `Quick test_auth_no_env_requires_key;
      test_case "allow-no-auth env" `Quick test_auth_allow_no_auth_env;
      test_case "valid token accepted" `Quick test_auth_with_env_requires_token;
      test_case "MCP_API_KEY fallback accepted" `Quick test_auth_with_mcp_api_key_env;
      test_case "wrong token rejected" `Quick test_auth_wrong_token_rejected;
      test_case "missing header rejected" `Quick test_auth_missing_header_rejected;
      test_case "invalid format rejected" `Quick test_auth_invalid_format_rejected;
      test_case "x-api-key accepted" `Quick test_auth_x_api_key_header;
      test_case "x-mcp-api-key accepted" `Quick test_auth_x_mcp_api_key_header;
    ];

    "header_extraction", [
      test_case "from X-Session-Id header" `Quick test_extract_session_from_header;
      test_case "from params" `Quick test_extract_session_from_param;
      test_case "header precedence" `Quick test_extract_session_header_precedence;
      test_case "case insensitive" `Quick test_extract_session_case_insensitive;
      test_case "not found" `Quick test_extract_session_not_found;
    ];

    "session_id_generation", [
      test_case "format check" `Quick test_generate_session_id_format;
      test_case "uniqueness" `Quick test_generate_session_id_unique;
    ];

    "tools_list", [
      test_case "default hidden utilities" `Quick test_tools_list_default_hidden_utilities;
      test_case "include hidden metadata" `Quick test_tools_list_include_hidden_metadata;
      test_case "include usage metadata" `Quick test_tools_list_include_usage_metadata;
    ];
  ]

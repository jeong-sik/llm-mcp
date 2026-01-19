(** Tests for MCP Server Eio

    Unit tests for session management, authentication, and request handling.
    Uses Eio_main.run for effect handling and structured concurrency.
*)

open Alcotest
open Llm_mcp

module Server = Mcp_server_eio

(** {1 Test Helpers} *)

let run_eio f =
  Eio_main.run @@ fun env ->
  f env

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

(** Test auth with no env var allows all requests *)
let test_auth_no_env_allows_all () =
  (* Ensure LLM_MCP_API_KEY is not set *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  (match original with
   | Some _ -> Unix.putenv "LLM_MCP_API_KEY" ""
   | None -> ());

  (* Test with no Authorization header *)
  let headers = [("content-type", "application/json")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> ());

  match result with
  | Ok () -> check bool "auth passed" true true
  | Error _ -> fail "auth should pass in dev mode"

(** Test auth with env var requires valid token *)
let test_auth_with_env_requires_token () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  Unix.putenv "LLM_MCP_API_KEY" "test-secret-123";

  (* Test with valid Bearer token *)
  let headers = [("authorization", "Bearer test-secret-123")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");

  match result with
  | Ok () -> check bool "valid token accepted" true true
  | Error msg -> fail (Printf.sprintf "auth should pass with valid token: %s" msg)

(** Test auth rejects wrong token *)
let test_auth_wrong_token_rejected () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  Unix.putenv "LLM_MCP_API_KEY" "correct-token";

  (* Test with wrong Bearer token *)
  let headers = [("authorization", "Bearer wrong-token")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");

  match result with
  | Error _ -> check bool "wrong token rejected" true true
  | Ok () -> fail "auth should reject wrong token"

(** Test auth rejects missing Authorization header *)
let test_auth_missing_header_rejected () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  Unix.putenv "LLM_MCP_API_KEY" "secret-key";

  (* Test with no Authorization header *)
  let headers = [("content-type", "application/json")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");

  match result with
  | Error _ -> check bool "missing header rejected" true true
  | Ok () -> fail "auth should reject missing header"

(** Test auth rejects invalid Authorization format *)
let test_auth_invalid_format_rejected () =
  (* Set API key *)
  let original = Sys.getenv_opt "LLM_MCP_API_KEY" in
  Unix.putenv "LLM_MCP_API_KEY" "secret-key";

  (* Test with invalid format (no "Bearer " prefix) *)
  let headers = [("authorization", "secret-key")] in
  let result = Server.auth_middleware headers in

  (* Restore original env *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_API_KEY" v
   | None -> Unix.putenv "LLM_MCP_API_KEY" "");

  match result with
  | Error _ -> check bool "invalid format rejected" true true
  | Ok () -> fail "auth should reject invalid format"

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
      test_case "no env allows all" `Quick test_auth_no_env_allows_all;
      test_case "valid token accepted" `Quick test_auth_with_env_requires_token;
      test_case "wrong token rejected" `Quick test_auth_wrong_token_rejected;
      test_case "missing header rejected" `Quick test_auth_missing_header_rejected;
      test_case "invalid format rejected" `Quick test_auth_invalid_format_rejected;
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
  ]

(** Tests for MCP Session management *)

open Alcotest

(** Test session ID generation *)
let test_generate_session_id () =
  let id1 = Llm_mcp.Mcp_session.generate_session_id () in
  let id2 = Llm_mcp.Mcp_session.generate_session_id () in
  (* 16 bytes * 2 hex chars = 32 chars *)
  check int "id length" 32 (String.length id1);
  check bool "ids are unique" true (id1 <> id2);
  (* All hex chars *)
  check bool "all hex chars" true
    (String.for_all (fun c ->
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) id1)

(** Test session ID validation *)
let test_is_valid_session_id () =
  check bool "valid hex" true
    (Llm_mcp.Mcp_session.is_valid_session_id "abc123def456");
  check bool "valid with uppercase" true
    (Llm_mcp.Mcp_session.is_valid_session_id "ABC123");
  check bool "empty is invalid" false
    (Llm_mcp.Mcp_session.is_valid_session_id "");
  check bool "space is invalid" false
    (Llm_mcp.Mcp_session.is_valid_session_id "abc 123");
  check bool "newline is invalid" false
    (Llm_mcp.Mcp_session.is_valid_session_id "abc\n123");
  (* Visible ASCII only: 0x21-0x7E *)
  check bool "tab is invalid" false
    (Llm_mcp.Mcp_session.is_valid_session_id "abc\t123")

(** Test session creation *)
let test_create_session () =
  let session = Llm_mcp.Mcp_session.create_session () in
  check int "session id length" 32 (String.length session.id);
  check bool "not initialized" false session.initialized;
  check string "default protocol" "2025-11-25" session.negotiated_protocol

(** Test session creation with custom ID *)
let test_create_session_with_id () =
  let custom_id = "my-custom-session-id-12345" in
  let session = Llm_mcp.Mcp_session.create_session ~id:custom_id () in
  check string "custom id used" custom_id session.id

(** Test session creation with invalid ID falls back to generated *)
let test_create_session_invalid_id () =
  let invalid_id = "has space here" in
  let session = Llm_mcp.Mcp_session.create_session ~id:invalid_id () in
  check bool "generated new id" true (session.id <> invalid_id);
  check int "generated id length" 32 (String.length session.id)

(** Test session retrieval *)
let test_get_session () =
  let session = Llm_mcp.Mcp_session.create_session () in
  let retrieved = Llm_mcp.Mcp_session.get_session session.id in
  check bool "found session" true (Option.is_some retrieved);
  match retrieved with
  | Some s -> check string "same id" session.id s.id
  | None -> fail "session should exist"

(** Test session not found *)
let test_get_session_not_found () =
  let result = Llm_mcp.Mcp_session.get_session "nonexistent-session" in
  check bool "not found" true (Option.is_none result)

(** Test mark initialized *)
let test_mark_initialized () =
  let session = Llm_mcp.Mcp_session.create_session () in
  check bool "initially not initialized" false session.initialized;
  Llm_mcp.Mcp_session.mark_initialized session;
  check bool "now initialized" true session.initialized

(** Test set negotiated protocol *)
let test_set_negotiated_protocol () =
  let session = Llm_mcp.Mcp_session.create_session () in
  Llm_mcp.Mcp_session.set_negotiated_protocol session "2024-11-05";
  check string "protocol updated" "2024-11-05" session.negotiated_protocol

(** Test delete session *)
let test_delete_session () =
  let session = Llm_mcp.Mcp_session.create_session () in
  let id = session.id in
  check bool "exists before delete" true
    (Option.is_some (Llm_mcp.Mcp_session.get_session id));
  Llm_mcp.Mcp_session.delete_session id;
  check bool "gone after delete" true
    (Option.is_none (Llm_mcp.Mcp_session.get_session id))

(** Test is_valid_session *)
let test_is_valid_session () =
  let session = Llm_mcp.Mcp_session.create_session () in
  check bool "valid session" true
    (Llm_mcp.Mcp_session.is_valid_session session.id);
  check bool "invalid session id" false
    (Llm_mcp.Mcp_session.is_valid_session "nonexistent")

(** Test protocol version constant *)
let test_protocol_version () =
  check string "protocol version format" "2025-11-25"
    Llm_mcp.Mcp_session.protocol_version

let () =
  run "Llm_mcp.Mcp_session" [
    "generate_session_id", [
      test_case "unique hex ids" `Quick test_generate_session_id;
    ];
    "is_valid_session_id", [
      test_case "validation rules" `Quick test_is_valid_session_id;
    ];
    "create_session", [
      test_case "default creation" `Quick test_create_session;
      test_case "with custom id" `Quick test_create_session_with_id;
      test_case "invalid id fallback" `Quick test_create_session_invalid_id;
    ];
    "get_session", [
      test_case "retrieve existing" `Quick test_get_session;
      test_case "not found" `Quick test_get_session_not_found;
    ];
    "mark_initialized", [
      test_case "sets flag" `Quick test_mark_initialized;
    ];
    "set_negotiated_protocol", [
      test_case "updates version" `Quick test_set_negotiated_protocol;
    ];
    "delete_session", [
      test_case "removes session" `Quick test_delete_session;
    ];
    "is_valid_session", [
      test_case "checks validity" `Quick test_is_valid_session;
    ];
    "protocol_version", [
      test_case "constant value" `Quick test_protocol_version;
    ];
  ]

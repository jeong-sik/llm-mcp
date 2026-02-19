(** Tests for exported pure helpers in Mcp_server_eio.

    Only functions in the .mli are testable:
    make_error, health_response, supported_protocol_versions,
    extract_session_id (additional edge cases),
    session store ops (additional coverage). *)

open Alcotest

module Server = Mcp_server_eio

(* --- make_error --- *)

let test_make_error_basic () =
  let result = Server.make_error ~id:(`Int 1) (-32600) "Invalid Request" in
  let open Yojson.Safe.Util in
  check string "jsonrpc" "2.0" (result |> member "jsonrpc" |> to_string);
  check int "id" 1 (result |> member "id" |> to_int);
  let err = result |> member "error" in
  check int "code" (-32600) (err |> member "code" |> to_int);
  check string "message" "Invalid Request" (err |> member "message" |> to_string)

let test_make_error_null_id () =
  let result = Server.make_error ~id:`Null (-32700) "Parse error" in
  let open Yojson.Safe.Util in
  check bool "id is null" true (result |> member "id" = `Null);
  check int "code" (-32700) (result |> member "error" |> member "code" |> to_int)

let test_make_error_string_id () =
  let result = Server.make_error ~id:(`String "req-42") (-32601) "Method not found" in
  let open Yojson.Safe.Util in
  check string "id" "req-42" (result |> member "id" |> to_string);
  check string "msg" "Method not found"
    (result |> member "error" |> member "message" |> to_string)

(* --- health_response --- *)

let test_health_response_fields () =
  let resp = Server.health_response () in
  let json = Yojson.Safe.from_string resp in
  let open Yojson.Safe.Util in
  check string "status" "ok" (json |> member "status" |> to_string);
  check string "server" "llm-mcp" (json |> member "server" |> to_string);
  check string "transport" "http" (json |> member "transport" |> to_string);
  check string "language" "ocaml" (json |> member "language" |> to_string)

let test_health_response_valid_json () =
  let resp = Server.health_response () in
  let _ = Yojson.Safe.from_string resp in
  check pass "valid JSON" () ()

let test_health_response_has_version () =
  let resp = Server.health_response () in
  let json = Yojson.Safe.from_string resp in
  let open Yojson.Safe.Util in
  let v = json |> member "version" |> to_string in
  check bool "non-empty version" true (String.length v > 0)

(* --- supported_protocol_versions --- *)

let test_supported_versions_non_empty () =
  check bool "at least one" true
    (List.length Server.supported_protocol_versions > 0)

let test_supported_versions_format () =
  let is_date s = String.length s = 10 && s.[4] = '-' && s.[7] = '-' in
  List.iter (fun v ->
    check bool (Printf.sprintf "'%s' is date-like" v) true (is_date v)
  ) Server.supported_protocol_versions

let test_supported_versions_includes_2024 () =
  check bool "has 2024-11-05" true
    (List.mem "2024-11-05" Server.supported_protocol_versions)

(* --- extract_session_id additional edge cases --- *)

let test_extract_session_mcp_header () =
  let headers = [("Mcp-Session-Id", "sess-abc")] in
  check (option string) "mcp header" (Some "sess-abc")
    (Server.extract_session_id None headers)

let test_extract_session_snake_param () =
  let params = Some (`Assoc [("session_id", `String "sess-snake")]) in
  check (option string) "snake param" (Some "sess-snake")
    (Server.extract_session_id params [])

let test_extract_session_non_string () =
  let params = Some (`Assoc [("sessionId", `Int 123)]) in
  check (option string) "non-string" None
    (Server.extract_session_id params [])

let test_extract_session_non_assoc () =
  let params = Some (`List [`String "nope"]) in
  check (option string) "non-assoc" None
    (Server.extract_session_id params [])

(* --- session store edge cases --- *)

let test_session_put_overwrites () =
  Eio_main.run (fun _env ->
    let store = Server.create_session_store () in
    let s1 : Server.session = {
      id = "dup"; protocol_version = "v1";
      created_at = 1000.0; last_accessed = 1000.0;
    } in
    Server.put_session store s1;
    Server.put_session store { s1 with protocol_version = "v2" };
    match Server.get_session store "dup" with
    | Some s -> check string "overwritten" "v2" s.protocol_version
    | None -> fail "should exist"
  )

let test_session_remove_nonexistent () =
  Eio_main.run (fun _env ->
    let store = Server.create_session_store () in
    Server.remove_session store "nope";
    check pass "no crash" () ()
  )

let test_session_touch_nonexistent () =
  Eio_main.run (fun _env ->
    let store = Server.create_session_store () in
    Server.touch_session store "nope";
    check pass "no crash" () ()
  )

let test_session_cleanup_old () =
  Eio_main.run (fun _env ->
    let store = Server.create_session_store () in
    let old : Server.session = {
      id = "old"; protocol_version = "v1";
      created_at = 0.0; last_accessed = 0.0;
    } in
    Server.put_session store old;
    Server.cleanup_stale_sessions store;
    check (option string) "removed" None
      (Option.map (fun (s : Server.session) -> s.id)
        (Server.get_session store "old"))
  )

let test_generate_session_id () =
  let id = Server.generate_session_id () in
  check bool "non-empty" true (String.length id > 0)

(* --- auth_middleware --- *)

let test_auth_no_env_no_allow () =
  let saved_key = Sys.getenv_opt "LLM_MCP_API_KEY" in
  let saved_mcp = Sys.getenv_opt "MCP_API_KEY" in
  let saved_a1 = Sys.getenv_opt "LLM_MCP_ALLOW_NO_AUTH" in
  let saved_a2 = Sys.getenv_opt "MCP_ALLOW_NO_AUTH" in
  Unix.putenv "LLM_MCP_API_KEY" "";
  Unix.putenv "MCP_API_KEY" "";
  Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" "0";
  Unix.putenv "MCP_ALLOW_NO_AUTH" "0";
  let result = Server.auth_middleware [] in
  (match saved_key with Some v -> Unix.putenv "LLM_MCP_API_KEY" v | None -> ());
  (match saved_mcp with Some v -> Unix.putenv "MCP_API_KEY" v | None -> ());
  (match saved_a1 with Some v -> Unix.putenv "LLM_MCP_ALLOW_NO_AUTH" v | None -> ());
  (match saved_a2 with Some v -> Unix.putenv "MCP_ALLOW_NO_AUTH" v | None -> ());
  check bool "error" true (match result with Error _ -> true | Ok () -> false)

(* --- Test Suite --- *)

let () =
  run "mcp_server_helpers" [
    ("make_error", [
      test_case "basic" `Quick test_make_error_basic;
      test_case "null id" `Quick test_make_error_null_id;
      test_case "string id" `Quick test_make_error_string_id;
    ]);
    ("health_response", [
      test_case "fields" `Quick test_health_response_fields;
      test_case "valid json" `Quick test_health_response_valid_json;
      test_case "has version" `Quick test_health_response_has_version;
    ]);
    ("supported_versions", [
      test_case "non-empty" `Quick test_supported_versions_non_empty;
      test_case "format" `Quick test_supported_versions_format;
      test_case "includes 2024" `Quick test_supported_versions_includes_2024;
    ]);
    ("extract_session_id", [
      test_case "mcp header" `Quick test_extract_session_mcp_header;
      test_case "snake param" `Quick test_extract_session_snake_param;
      test_case "non-string" `Quick test_extract_session_non_string;
      test_case "non-assoc" `Quick test_extract_session_non_assoc;
    ]);
    ("session_store", [
      test_case "put overwrites" `Quick test_session_put_overwrites;
      test_case "remove nonexistent" `Quick test_session_remove_nonexistent;
      test_case "touch nonexistent" `Quick test_session_touch_nonexistent;
      test_case "cleanup old" `Quick test_session_cleanup_old;
      test_case "generate id" `Quick test_generate_session_id;
    ]);
    ("auth_middleware", [
      test_case "no env no allow" `Quick test_auth_no_env_no_allow;
    ]);
  ]

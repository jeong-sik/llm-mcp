(* Compact Protocol v2.0 - Input Compression Tests *)

open Alcotest
open Llm_mcp.Types

(* =========================================================================
   Tool Caching Tests
   ========================================================================= *)

let test_tool_cache_roundtrip () =
  let name = "search_web" in
  let schema = {|{"type":"object","properties":{"query":{"type":"string"}}}|} in

  (* Cache the tool *)
  let ref_id = cache_tool_def ~name ~schema in
  check bool "ref starts with t_" true (String.sub ref_id 0 2 = "t_");
  check bool "ref length" true (String.length ref_id > 2);

  (* Lookup should return the entry *)
  match lookup_tool ref_id with
  | Some entry ->
      check string "tool_name matches" name entry.tool_name;
      check string "tool_id matches" ref_id entry.tool_id;
      check bool "schema_hash not empty" true (String.length entry.schema_hash > 0)
  | None -> fail "Tool lookup failed"

let test_tool_cache_unknown () =
  match lookup_tool "t_nonexistent" with
  | Some _ -> fail "Should not find unknown tool"
  | None -> check pass "unknown tool returns None" () ()

let test_tool_cache_idempotent () =
  let name = "get_weather" in
  let schema = {|{"city":"string"}|} in

  let ref1 = cache_tool_def ~name ~schema in
  let ref2 = cache_tool_def ~name ~schema in
  check string "same input = same ref" ref1 ref2

(* =========================================================================
   System Prompt Caching Tests
   ========================================================================= *)

let test_system_prompt_cache_roundtrip () =
  let prompt = "You are a helpful assistant that writes code." in

  let ref_id = cache_system_prompt prompt in
  check bool "ref starts with s_" true (String.sub ref_id 0 2 = "s_");

  match lookup_system_prompt ref_id with
  | Some p -> check string "prompt matches" prompt p
  | None -> fail "System prompt lookup failed"

let test_system_prompt_unknown () =
  match lookup_system_prompt "s_nonexistent" with
  | Some _ -> fail "Should not find unknown prompt"
  | None -> check pass "unknown prompt returns None" () ()

(* =========================================================================
   Delta Operations Tests
   ========================================================================= *)

let test_delta_full () =
  let delta = Full "Hello, world!" in
  let encoded = encode_delta delta in
  check string "full prefix" "D|F|Hello, world!" encoded;

  match decode_delta encoded with
  | Some (Full s) -> check string "decoded" "Hello, world!" s
  | _ -> fail "Full decode failed"

let test_delta_append () =
  let delta = Append ", more text" in
  let encoded = encode_delta delta in
  check string "append prefix" "D|+|, more text" encoded;

  match decode_delta encoded with
  | Some (Append s) -> check string "decoded" ", more text" s
  | _ -> fail "Append decode failed"

let test_delta_replace () =
  let delta = Replace (7, "World") in
  let encoded = encode_delta delta in
  check string "replace prefix" "D|R|7|World" encoded;

  match decode_delta encoded with
  | Some (Replace (pos, s)) ->
      check int "position" 7 pos;
      check string "content" "World" s
  | _ -> fail "Replace decode failed"

let test_apply_delta_full () =
  let result = apply_delta "old content" (Full "new content") in
  check string "full replaces" "new content" result

let test_apply_delta_append () =
  let result = apply_delta "Hello" (Append ", World!") in
  check string "append works" "Hello, World!" result

let test_apply_delta_replace () =
  let result = apply_delta "Hello, world!" (Replace (7, "World")) in
  check string "replace works" "Hello, World" result

let test_compute_delta_empty_old () =
  let delta = compute_delta ~old_content:"" ~new_content:"Hello" in
  match delta with
  | Full s -> check string "empty old = Full" "Hello" s
  | _ -> fail "Expected Full for empty old content"

let test_compute_delta_append () =
  let delta = compute_delta ~old_content:"Hello" ~new_content:"Hello, World!" in
  match delta with
  | Append s -> check string "append detected" ", World!" s
  | _ -> fail "Expected Append when new starts with old"

let test_compute_delta_full_fallback () =
  let delta = compute_delta ~old_content:"Hello" ~new_content:"Goodbye" in
  match delta with
  | Full s -> check string "full fallback" "Goodbye" s
  | _ -> fail "Expected Full when no common prefix"

(* =========================================================================
   Compact Request Tests
   ========================================================================= *)

let test_compact_request_encode_minimal () =
  let req = {
    req_version = 1;
    tool_ref = None;
    tool_def = Some {|{"name":"test"}|};
    args = "hello";
    context_deltas = [];
    system_ref = None;
    system_prompt = Some "You are helpful.";
  } in

  let encoded = encode_compact_request req in
  check bool "encoded not empty" true (String.length encoded > 0);

  (* Decode and verify *)
  match decode_compact_request encoded with
  | Some decoded ->
      check int "version" 1 decoded.req_version;
      check bool "tool_ref" true (decoded.tool_ref = None);
      check string "args" "hello" decoded.args
  | None -> fail "Request decode failed"

let test_compact_request_with_ref () =
  (* First cache a tool *)
  let ref_id = cache_tool_def ~name:"cached_tool" ~schema:"{}" in

  let req = {
    req_version = 1;
    tool_ref = Some ref_id;
    tool_def = None;
    args = "cached args";
    context_deltas = [];
    system_ref = None;
    system_prompt = None;
  } in

  let encoded = encode_compact_request req in
  match decode_compact_request encoded with
  | Some decoded ->
      check bool "has tool_ref" true (decoded.tool_ref = Some ref_id);
      check bool "no tool_def" true (decoded.tool_def = None)
  | None -> fail "Request with ref decode failed"

let test_compact_request_with_deltas () =
  let req = {
    req_version = 1;
    tool_ref = None;
    tool_def = Some "{}";
    args = "";
    context_deltas = [Full "Hello"; Append ", World!"];
    system_ref = None;
    system_prompt = None;
  } in

  let encoded = encode_compact_request req in
  match decode_compact_request encoded with
  | Some decoded ->
      check int "delta count" 2 (List.length decoded.context_deltas)
  | None -> fail "Request with deltas decode failed"

(* =========================================================================
   Test Runner
   ========================================================================= *)

let () =
  run "Input Compression" [
    "tool_cache", [
      "roundtrip", `Quick, test_tool_cache_roundtrip;
      "unknown", `Quick, test_tool_cache_unknown;
      "idempotent", `Quick, test_tool_cache_idempotent;
    ];
    "system_prompt_cache", [
      "roundtrip", `Quick, test_system_prompt_cache_roundtrip;
      "unknown", `Quick, test_system_prompt_unknown;
    ];
    "delta_ops", [
      "full", `Quick, test_delta_full;
      "append", `Quick, test_delta_append;
      "replace", `Quick, test_delta_replace;
      "apply_full", `Quick, test_apply_delta_full;
      "apply_append", `Quick, test_apply_delta_append;
      "apply_replace", `Quick, test_apply_delta_replace;
      "compute_empty", `Quick, test_compute_delta_empty_old;
      "compute_append", `Quick, test_compute_delta_append;
      "compute_full", `Quick, test_compute_delta_full_fallback;
    ];
    "compact_request", [
      "minimal", `Quick, test_compact_request_encode_minimal;
      "with_ref", `Quick, test_compact_request_with_ref;
      "with_deltas", `Quick, test_compact_request_with_deltas;
    ]
  ]

(** Tests for Compact/Encoding functions in Types module

    Pure function tests:
    - short_hash: MD5-based hash generation
    - cache_tool_def/lookup_tool: tool caching
    - cache_system_prompt/lookup_system_prompt: prompt caching
    - encode_delta/apply_delta: delta encoding roundtrip
    - compute_delta: delta computation
    - encode_base85/decode_base85: Base85 codec roundtrip
    - encode_msgpack_response/decode_msgpack_response: MsgPack roundtrip
    - encode_compact_response/decode_compact_response: compact DSL roundtrip
*)

open Alcotest
open Types

(** {1 Short Hash Tests} *)

let test_short_hash_deterministic () =
  let h1 = short_hash "hello" in
  let h2 = short_hash "hello" in
  check string "deterministic" h1 h2

let test_short_hash_length () =
  let h = short_hash "test input" in
  check int "8 chars" 8 (String.length h)

let test_short_hash_different () =
  let h1 = short_hash "hello" in
  let h2 = short_hash "world" in
  check bool "different inputs" true (h1 <> h2)

(** {1 Tool Cache Tests} *)

let test_cache_tool_def () =
  let id = cache_tool_def ~name:"my_tool" ~schema:"{}" in
  check bool "starts with t_" true (String.sub id 0 2 = "t_");
  check int "id length" 10 (String.length id)  (* t_ + 8 hash chars *)

let test_cache_tool_lookup () =
  let id = cache_tool_def ~name:"test_tool_unique" ~schema:"{\"type\":\"object\"}" in
  match lookup_tool id with
  | Some entry ->
      check string "tool_name" "test_tool_unique" entry.tool_name;
      check string "tool_id" id entry.tool_id
  | None -> fail "entry not found"

let test_cache_tool_not_found () =
  match lookup_tool "t_nonexist" with
  | Some _ -> fail "should not find"
  | None -> ()  (* expected *)

(** {1 System Prompt Cache Tests} *)

let test_cache_system_prompt () =
  let hash = cache_system_prompt "You are helpful." in
  check bool "starts with s_" true (String.sub hash 0 2 = "s_");
  check int "hash length" 10 (String.length hash)

let test_cache_system_prompt_lookup () =
  let hash = cache_system_prompt "Be concise and clear." in
  match lookup_system_prompt hash with
  | Some prompt -> check string "prompt" "Be concise and clear." prompt
  | None -> fail "prompt not found"

let test_cache_system_prompt_dedup () =
  let h1 = cache_system_prompt "Same prompt text" in
  let h2 = cache_system_prompt "Same prompt text" in
  check string "same hash" h1 h2

(** {1 Delta Encoding Tests} *)

let test_encode_delta_full () =
  let encoded = encode_delta (Full "hello world") in
  check string "full" "D|F|hello world" encoded

let test_encode_delta_append () =
  let encoded = encode_delta (Append " more text") in
  check string "append" "D|+| more text" encoded

let test_encode_delta_replace () =
  let encoded = encode_delta (Replace (5, "new")) in
  check string "replace" "D|R|5|new" encoded

let test_apply_delta_full () =
  let result = apply_delta "ignored" (Full "replaced") in
  check string "full apply" "replaced" result

let test_apply_delta_append () =
  let result = apply_delta "hello" (Append " world") in
  check string "append apply" "hello world" result

let test_apply_delta_replace () =
  let result = apply_delta "hello world" (Replace (6, "there")) in
  check string "replace apply" "hello there" result

(** {1 Compute Delta Tests} *)

let test_compute_delta_identical () =
  let delta = compute_delta ~old_content:"same" ~new_content:"same" in
  match delta with
  | Full s -> check string "identical = full" "same" s
  | _ -> ()  (* Either Full or no delta is acceptable *)

let test_compute_delta_append () =
  let delta = compute_delta ~old_content:"hello" ~new_content:"hello world" in
  match delta with
  | Append s -> check string "appended" " world" s
  | Full s -> check string "full fallback" "hello world" s
  | _ -> ()

let test_compute_delta_completely_different () =
  let delta = compute_delta ~old_content:"abc" ~new_content:"xyz" in
  match delta with
  | Full s -> check string "full" "xyz" s
  | _ -> fail "expected Full for different content"

(** {1 Base85 Codec Tests} *)

let test_base85_encode_decode_empty () =
  let encoded = encode_base85 "" in
  check string "empty encodes to empty" "" encoded;
  match decode_base85 encoded with
  | Ok decoded -> check string "empty roundtrip" "" decoded
  | Error _ -> fail "decode failed"

let test_base85_encode_decode_4bytes () =
  let data = "test" in  (* Exactly 4 bytes *)
  let encoded = encode_base85 data in
  check int "4 bytes -> 5 chars" 5 (String.length encoded);
  match decode_base85 encoded with
  | Ok decoded -> check string "4-byte roundtrip" data decoded
  | Error _ -> fail "decode failed"

let test_base85_encode_decode_8bytes () =
  let data = "testdata" in  (* 8 bytes = 2 blocks *)
  let encoded = encode_base85 data in
  check int "8 bytes -> 10 chars" 10 (String.length encoded);
  match decode_base85 encoded with
  | Ok decoded -> check string "8-byte roundtrip" data decoded
  | Error _ -> fail "decode failed"

let test_base85_encode_decode_partial () =
  let data = "hi" in  (* 2 bytes - partial block *)
  let encoded = encode_base85 data in
  match decode_base85 encoded with
  | Ok decoded -> check string "partial roundtrip" data decoded
  | Error _ -> fail "decode failed"

let test_base85_invalid_char () =
  match decode_base85 "~~~" with  (* ~ is not in base85 alphabet *)
  | Ok _ -> fail "should fail on invalid char"
  | Error (InvalidBase85Char (c, pos)) ->
      check char "invalid char" '~' c;
      check int "position" 0 pos
  | Error _ -> ()  (* Any error is acceptable *)

let test_base85_invalid_length () =
  match decode_base85 "A" with  (* Length 1 is invalid *)
  | Ok _ -> fail "should fail on length 1"
  | Error (InvalidBase85Length 1) -> ()
  | Error _ -> ()  (* Any error is acceptable *)

(** {1 MsgPack Response Tests} *)

let test_msgpack_v1_roundtrip () =
  let resp = {
    version = 1;
    status = OK;
    model = G3;
    tokens = 100;
    result = "test result";
  } in
  let encoded = encode_msgpack_response resp in
  match decode_msgpack_response encoded with
  | Some decoded ->
      check int "version" 1 decoded.version;
      check int "tokens" 100 decoded.tokens;
      check string "result" "test result" decoded.result
  | None -> fail "decode failed"

let test_msgpack_v2_roundtrip () =
  let resp = {
    version = 2;
    status = ERR;
    model = C4;
    tokens = 50;
    result = "error message";
  } in
  let encoded = encode_msgpack_response resp in
  match decode_msgpack_response encoded with
  | Some decoded ->
      check int "version" 2 decoded.version;
      check int "tokens" 50 decoded.tokens
  | None -> fail "decode failed"

let test_msgpack_v3_with_tokens () =
  let resp = {
    version = 3;
    status = OK;
    model = G3;
    tokens = 200;
    result = "result";
  } in
  let encoded = encode_msgpack_response resp in
  match decode_msgpack_response encoded with
  | Some decoded ->
      check int "version" 3 decoded.version;
      check int "tokens" 200 decoded.tokens
  | None -> fail "decode failed"

let test_msgpack_v3_zero_tokens () =
  let resp = {
    version = 3;
    status = OK;
    model = G3;
    tokens = 0;  (* Should be omitted in encoding *)
    result = "no tokens";
  } in
  let encoded = encode_msgpack_response resp in
  match decode_msgpack_response encoded with
  | Some decoded ->
      check int "version" 3 decoded.version;
      check int "tokens 0" 0 decoded.tokens
  | None -> fail "decode failed"

(** {1 Compact Response DSL Tests} *)

let test_compact_response_encode () =
  let resp = {
    version = 1;
    status = OK;
    model = G3;
    tokens = 150;
    result = "success";
  } in
  let encoded = encode_compact_response resp in
  check string "encoded" "RES|OK|G3|150|success" encoded

let test_compact_response_roundtrip () =
  let resp = {
    version = 1;
    status = ERR;
    model = C4;
    tokens = 0;
    result = "error message with|pipe";
  } in
  let encoded = encode_compact_response resp in
  match decode_compact_response encoded with
  | Some decoded ->
      check int "tokens" 0 decoded.tokens;
      check string "result" "error message with|pipe" decoded.result
  | None -> fail "decode failed"

let test_compact_response_invalid () =
  match decode_compact_response "INVALID" with
  | Some _ -> fail "should fail"
  | None -> ()  (* expected *)

(** {1 String Conversion Tests} *)

let test_string_of_decode_error () =
  let e1 = InvalidBase85Char ('~', 5) in
  let s1 = string_of_decode_error e1 in
  check bool "has char" true (Common.contains ~substring:"'~'" s1);
  check bool "has pos" true (Common.contains ~substring:"5" s1);

  let e2 = InvalidBase85Length 1 in
  let s2 = string_of_decode_error e2 in
  check bool "has length" true (Common.contains ~substring:"1" s2)

(** {1 Test Suite} *)

let hash_tests = [
  test_case "deterministic" `Quick test_short_hash_deterministic;
  test_case "length" `Quick test_short_hash_length;
  test_case "different" `Quick test_short_hash_different;
]

let tool_cache_tests = [
  test_case "cache_tool_def" `Quick test_cache_tool_def;
  test_case "lookup" `Quick test_cache_tool_lookup;
  test_case "not found" `Quick test_cache_tool_not_found;
]

let prompt_cache_tests = [
  test_case "cache" `Quick test_cache_system_prompt;
  test_case "lookup" `Quick test_cache_system_prompt_lookup;
  test_case "dedup" `Quick test_cache_system_prompt_dedup;
]

let delta_tests = [
  test_case "encode full" `Quick test_encode_delta_full;
  test_case "encode append" `Quick test_encode_delta_append;
  test_case "encode replace" `Quick test_encode_delta_replace;
  test_case "apply full" `Quick test_apply_delta_full;
  test_case "apply append" `Quick test_apply_delta_append;
  test_case "apply replace" `Quick test_apply_delta_replace;
  test_case "compute identical" `Quick test_compute_delta_identical;
  test_case "compute append" `Quick test_compute_delta_append;
  test_case "compute different" `Quick test_compute_delta_completely_different;
]

let base85_tests = [
  test_case "empty" `Quick test_base85_encode_decode_empty;
  test_case "4 bytes" `Quick test_base85_encode_decode_4bytes;
  test_case "8 bytes" `Quick test_base85_encode_decode_8bytes;
  test_case "partial" `Quick test_base85_encode_decode_partial;
  test_case "invalid char" `Quick test_base85_invalid_char;
  test_case "invalid length" `Quick test_base85_invalid_length;
]

let msgpack_tests = [
  test_case "v1 roundtrip" `Quick test_msgpack_v1_roundtrip;
  test_case "v2 roundtrip" `Quick test_msgpack_v2_roundtrip;
  test_case "v3 with tokens" `Quick test_msgpack_v3_with_tokens;
  test_case "v3 zero tokens" `Quick test_msgpack_v3_zero_tokens;
]

let compact_tests = [
  test_case "encode" `Quick test_compact_response_encode;
  test_case "roundtrip" `Quick test_compact_response_roundtrip;
  test_case "invalid" `Quick test_compact_response_invalid;
]

let error_tests = [
  test_case "decode_error to string" `Quick test_string_of_decode_error;
]

let () =
  run "compact_encoding" [
    ("short_hash", hash_tests);
    ("tool_cache", tool_cache_tests);
    ("prompt_cache", prompt_cache_tests);
    ("delta", delta_tests);
    ("base85", base85_tests);
    ("msgpack", msgpack_tests);
    ("compact_response", compact_tests);
    ("errors", error_tests);
  ]

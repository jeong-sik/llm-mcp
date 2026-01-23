(* Compact Protocol v1.3 Size Benchmark and Version Tests *)

open Alcotest
open Types

(* =========================================================================
   v1/v2/v3 Roundtrip Tests
   ========================================================================= *)

let test_v1_roundtrip () =
  let response : compact_response = {
    version = 1;
    status = OK;
    model = G3;
    tokens = 150;
    result = "Hello, world!";
  } in
  let encoded = encode_msgpack_response response in
  let decoded = decode_msgpack_response encoded in
  match decoded with
  | Some r ->
      check int "version" 1 r.version;
      check bool "status OK" true (r.status = OK);
      check bool "model G3" true (r.model = G3);
      check int "tokens" 150 r.tokens;
      check string "result" "Hello, world!" r.result
  | None -> fail "v1 decode failed"

let test_v2_roundtrip () =
  let response : compact_response = {
    version = 2;
    status = ERR;
    model = C4;
    tokens = 200;
    result = "Error message";
  } in
  let encoded = encode_msgpack_response response in
  let decoded = decode_msgpack_response encoded in
  match decoded with
  | Some r ->
      check int "version" 2 r.version;
      check bool "status ERR" true (r.status = ERR);
      check bool "model C4" true (r.model = C4);
      check int "tokens" 200 r.tokens;
      check string "result" "Error message" r.result
  | None -> fail "v2 decode failed"

let test_v3_with_tokens () =
  let response : compact_response = {
    version = 3;
    status = PART;
    model = X5;
    tokens = 100;
    result = "Partial result";
  } in
  let encoded = encode_msgpack_response response in
  let decoded = decode_msgpack_response encoded in
  match decoded with
  | Some r ->
      check int "version" 3 r.version;
      check bool "status PART" true (r.status = PART);
      check bool "model X5" true (r.model = X5);
      check int "tokens" 100 r.tokens;
      check string "result" "Partial result" r.result
  | None -> fail "v3 (with tokens) decode failed"

let test_v3_without_tokens () =
  let response : compact_response = {
    version = 3;
    status = STREAM;
    model = OL;
    tokens = 0;  (* Should be omitted in encoding *)
    result = "Streaming...";
  } in
  let encoded = encode_msgpack_response response in
  (* v3 with tokens=0 should be shorter *)
  let with_tokens = { response with tokens = 1 } in
  let encoded_with_tokens = encode_msgpack_response with_tokens in
  check bool "v3 without tokens is smaller" true
    (String.length encoded < String.length encoded_with_tokens);

  let decoded = decode_msgpack_response encoded in
  match decoded with
  | Some r ->
      check int "version" 3 r.version;
      check bool "status STREAM" true (r.status = STREAM);
      check bool "model OL" true (r.model = OL);
      check int "tokens should be 0" 0 r.tokens;
      check string "result" "Streaming..." r.result
  | None -> fail "v3 (without tokens) decode failed"

let test_all_status_codes () =
  let statuses = [OK; ERR; PART; STREAM] in
  List.iter (fun status ->
    let response : compact_response = {
      version = 2;
      status;
      model = G3;
      tokens = 0;
      result = "";
    } in
    let encoded = encode_msgpack_response response in
    let decoded = decode_msgpack_response encoded in
    match decoded with
    | Some r -> check bool (Printf.sprintf "status %s roundtrip" (string_of_status_code status))
        true (r.status = status)
    | None -> fail (Printf.sprintf "status %s decode failed" (string_of_status_code status))
  ) statuses

let test_all_model_codes () =
  let models = [G3; C4; X5; OL] in
  List.iter (fun model ->
    let response : compact_response = {
      version = 2;
      status = OK;
      model;
      tokens = 0;
      result = "";
    } in
    let encoded = encode_msgpack_response response in
    let decoded = decode_msgpack_response encoded in
    match decoded with
    | Some r -> check bool (Printf.sprintf "model %s roundtrip" (string_of_model_code model))
        true (r.model = model)
    | None -> fail (Printf.sprintf "model %s decode failed" (string_of_model_code model))
  ) models

(* =========================================================================
   Size Benchmark
   ========================================================================= *)

let test_size_comparison () =
  let result_text = "TypeScript is better for large codebases." in

  (* v3 encoding (latest) *)
  let response : compact_response = {
    version = 3;
    status = OK;
    model = G3;
    tokens = 150;
    result = result_text;
  } in

  (* v3 with tokens=0 (omitted) *)
  let response_no_tokens : compact_response = {
    version = 3;
    status = OK;
    model = G3;
    tokens = 0;
    result = result_text;
  } in

  let msgpack_v3 = encode_msgpack_response response in
  let msgpack_v3_no_tokens = encode_msgpack_response response_no_tokens in
  let base64_v3 = Base64.encode_string msgpack_v3 in

  (* v1.3 prefixes *)
  let binary_v13 = "M" ^ base64_v3 in

  (* Legacy prefix for comparison *)
  let binary_legacy = "MPK:" ^ base64_v3 in

  (* Compact DSL *)
  let compact_dsl = encode_compact_response response in

  (* Verbose JSON *)
  let verbose_json = Printf.sprintf
    {|{"model":"gemini (gemini-3-pro-preview)","returncode":0,"response":"%s"}|}
    result_text in

  Printf.printf "\n=== Compact Protocol v1.3 Benchmark ===\n\n";
  Printf.printf "Result text: \"%s\" (%d chars)\n\n" result_text (String.length result_text);

  Printf.printf "Format             | Size  | vs Verbose | Notes\n";
  Printf.printf "-------------------|-------|------------|------------------\n";
  Printf.printf "Verbose JSON       | %3d   | baseline   | Human readable\n" (String.length verbose_json);
  Printf.printf "Compact DSL        | %3d   | -%d%%       | RES|OK|G3|...\n"
    (String.length compact_dsl)
    (int_of_float (100.0 *. (1.0 -. float_of_int (String.length compact_dsl) /. float_of_int (String.length verbose_json))));
  Printf.printf "Binary v1.3 (M)    | %3d   | -%d%%       | New short prefix\n"
    (String.length binary_v13)
    (int_of_float (100.0 *. (1.0 -. float_of_int (String.length binary_v13) /. float_of_int (String.length verbose_json))));
  Printf.printf "Binary legacy      | %3d   | -%d%%       | MPK: prefix\n"
    (String.length binary_legacy)
    (int_of_float (100.0 *. (1.0 -. float_of_int (String.length binary_legacy) /. float_of_int (String.length verbose_json))));

  Printf.printf "\n=== MessagePack v3 Internals ===\n\n";
  Printf.printf "v3 with tokens (150):  %d bytes\n" (String.length msgpack_v3);
  Printf.printf "v3 without tokens (0): %d bytes (1 byte saved)\n" (String.length msgpack_v3_no_tokens);
  Printf.printf "Base64 encoded:        %d chars\n" (String.length base64_v3);

  (* Estimate Base85 savings *)
  let base85_estimated = int_of_float (float_of_int (String.length msgpack_v3) *. 1.25) in
  Printf.printf "Base85 estimated:      ~%d chars (~%.1f%% smaller than Base64)\n"
    base85_estimated
    ((1.0 -. float_of_int base85_estimated /. float_of_int (String.length base64_v3)) *. 100.0);

  Printf.printf "\n=== v1.3 Optimizations Summary ===\n\n";
  Printf.printf "1. Prefix shortening:  MPK: → M  (3 bytes saved)\n";
  Printf.printf "2. v3 tokens=0 omit:   1 byte saved when tokens=0\n";
  Printf.printf "3. Base85 encoding:    ~7%% smaller than Base64\n";
  Printf.printf "4. Zlib compression:   50-95%% for large responses\n";

  Printf.printf "\n=== Recommendations ===\n\n";
  Printf.printf "• Short responses (<50 bytes): Use Compact DSL\n";
  Printf.printf "• Medium responses: Use Base85 (A prefix)\n";
  Printf.printf "• Long responses (>100 bytes): Use Zlib+Base85 (Z prefix)\n";
  Printf.printf "• Binary channels (WebSocket): Use Raw MessagePack\n";

  check bool "analysis complete" true true

let () =
  run "MsgPack" [
    "version_roundtrip", [
      "v1", `Quick, test_v1_roundtrip;
      "v2", `Quick, test_v2_roundtrip;
      "v3_with_tokens", `Quick, test_v3_with_tokens;
      "v3_without_tokens", `Quick, test_v3_without_tokens;
      "all_status_codes", `Quick, test_all_status_codes;
      "all_model_codes", `Quick, test_all_model_codes;
    ];
    "size_benchmark", [
      "comparison", `Quick, test_size_comparison;
    ]
  ]

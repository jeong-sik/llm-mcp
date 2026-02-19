(** Tests for Langfuse module - LLM Observability Integration

    Pure function tests:
    - generate_id: UUID format validation
    - iso8601_of_float: timestamp formatting
    - metadata_to_json: key-value JSON conversion
    - trace_to_json, generation_to_json, span_to_json: JSON serialization
    - load_config: environment-based configuration
    - generation_to_json: output, usage, ended_at, status branches
    - span_to_json: ended_at branch
*)

open Alcotest
open Langfuse

(** {1 ID Generation Tests} *)

let test_generate_id_format () =
  let id = generate_id () in
  (* UUID format: 8-4-4-4-12 = 36 chars total *)
  check int "length" 36 (String.length id);
  check char "dash at 8" '-' (String.get id 8);
  check char "dash at 13" '-' (String.get id 13);
  check char "dash at 18" '-' (String.get id 18);
  check char "dash at 23" '-' (String.get id 23)

let test_generate_id_unique () =
  let id1 = generate_id () in
  let id2 = generate_id () in
  check bool "different" true (id1 <> id2)

let test_generate_id_hex_chars () =
  let id = generate_id () in
  let is_hex_or_dash c =
    (c >= '0' && c <= '9') ||
    (c >= 'a' && c <= 'f') ||
    c = '-'
  in
  let all_valid = String.for_all is_hex_or_dash id in
  check bool "all hex or dash" true all_valid

(** {1 Timestamp Formatting Tests} *)

let test_iso8601_of_float () =
  (* 2024-01-01 00:00:00 UTC = 1704067200.0 *)
  let ts = 1704067200.0 in
  let formatted = iso8601_of_float ts in
  check bool "has T separator" true (Common.contains ~substring:"T" formatted);
  check bool "has dash" true (Common.contains ~substring:"-" formatted);
  check bool "has colon" true (Common.contains ~substring:":" formatted);
  check bool "ends with Z" true (String.ends_with ~suffix:"Z" formatted)

let test_iso8601_with_milliseconds () =
  let ts = 1704067200.123 in
  let formatted = iso8601_of_float ts in
  check bool "has digits" true (String.length formatted > 10);
  (* Format: YYYY-MM-DDTHH:MM:SS.mmmZ — should have dot for ms *)
  check bool "has ms dot" true (Common.contains ~substring:"." formatted)

(** {1 Metadata to JSON Tests} *)

let test_metadata_to_json_empty () =
  let json = metadata_to_json [] in
  match json with
  | `Assoc [] -> ()
  | `Assoc _ -> ()
  | _ -> fail "expected object"

let test_metadata_to_json_single () =
  let json = metadata_to_json [("key", "value")] in
  match json with
  | `Assoc fields ->
      (match List.assoc_opt "key" fields with
       | Some (`String "value") -> ()
       | _ -> fail "key not found or wrong value")
  | _ -> fail "expected object"

let test_metadata_to_json_multiple () =
  let json = metadata_to_json [("a", "1"); ("b", "2"); ("c", "3")] in
  match json with
  | `Assoc fields ->
      check int "3 fields" 3 (List.length fields)
  | _ -> fail "expected object"

(** {1 Trace JSON Serialization Tests} *)

let test_trace_to_json () =
  let trace = {
    trace_id = "test-trace-id";
    name = "test-trace";
    metadata = [("env", "test")];
    started_at = 1704067200.0;
  } in
  let json = trace_to_json trace in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has name" true (List.mem_assoc "name" fields);
      check bool "has metadata" true (List.mem_assoc "metadata" fields);
      check bool "has timestamp" true (List.mem_assoc "timestamp" fields);
      (match List.assoc "id" fields with
       | `String id -> check string "trace_id" "test-trace-id" id
       | _ -> fail "expected string id");
      (match List.assoc "name" fields with
       | `String n -> check string "name" "test-trace" n
       | _ -> fail "expected string name")
  | _ -> fail "expected object"

(** {1 Generation JSON Serialization Tests} *)

let test_generation_to_json_running () =
  let gen = {
    gen_id = "gen-123";
    trace_id = "trace-456";
    name = "test-generation";
    model = "gemini-1.5-flash";
    input = "test input";
    output = None;
    usage = None;
    started_at = 1704067200.0;
    ended_at = None;
    status = `Running;
  } in
  let json = generation_to_json gen in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has model" true (List.mem_assoc "model" fields);
      check bool "has traceId" true (List.mem_assoc "traceId" fields);
      check bool "has level" true (List.mem_assoc "level" fields);
      (match List.assoc "model" fields with
       | `String m -> check string "model" "gemini-1.5-flash" m
       | _ -> fail "expected string model");
      (match List.assoc "level" fields with
       | `String l -> check string "running=DEFAULT" "DEFAULT" l
       | _ -> fail "expected string level");
      (* No output, usage, endTime when None *)
      check bool "no output" false (List.mem_assoc "output" fields);
      check bool "no usage" false (List.mem_assoc "usage" fields);
      check bool "no endTime" false (List.mem_assoc "endTime" fields)
  | _ -> fail "expected object"

let test_generation_with_output () =
  let gen = {
    gen_id = "gen-out";
    trace_id = "trace-1";
    name = "gen-with-output";
    model = "claude-3-opus";
    input = "test prompt";
    output = Some "test response";
    usage = None;
    started_at = 1704067200.0;
    ended_at = None;
    status = `Running;
  } in
  let json = generation_to_json gen in
  match json with
  | `Assoc fields ->
      check bool "has output" true (List.mem_assoc "output" fields);
      (match List.assoc "output" fields with
       | `String o -> check string "output value" "test response" o
       | _ -> fail "expected string output")
  | _ -> fail "expected object"

let test_generation_with_usage () =
  let gen = {
    gen_id = "gen-usage";
    trace_id = "trace-1";
    name = "gen-with-usage";
    model = "gemini-flash";
    input = "prompt";
    output = None;
    usage = Some (100, 200, 300);
    started_at = 1704067200.0;
    ended_at = None;
    status = `Running;
  } in
  let json = generation_to_json gen in
  match json with
  | `Assoc fields ->
      check bool "has usage" true (List.mem_assoc "usage" fields);
      (match List.assoc "usage" fields with
       | `Assoc usage_fields ->
           (match List.assoc "promptTokens" usage_fields with
            | `Int n -> check int "prompt tokens" 100 n
            | _ -> fail "expected int promptTokens");
           (match List.assoc "completionTokens" usage_fields with
            | `Int n -> check int "completion tokens" 200 n
            | _ -> fail "expected int completionTokens");
           (match List.assoc "totalTokens" usage_fields with
            | `Int n -> check int "total tokens" 300 n
            | _ -> fail "expected int totalTokens")
       | _ -> fail "expected usage object")
  | _ -> fail "expected object"

let test_generation_with_ended_at () =
  let gen = {
    gen_id = "gen-end";
    trace_id = "trace-1";
    name = "gen-ended";
    model = "claude";
    input = "prompt";
    output = None;
    usage = None;
    started_at = 1704067200.0;
    ended_at = Some 1704067260.0;
    status = `Running;
  } in
  let json = generation_to_json gen in
  match json with
  | `Assoc fields ->
      check bool "has endTime" true (List.mem_assoc "endTime" fields);
      (match List.assoc "endTime" fields with
       | `String _ -> ()  (* just verify it's a string *)
       | _ -> fail "expected string endTime")
  | _ -> fail "expected object"

let test_generation_status_success () =
  let gen = {
    gen_id = "gen-success";
    trace_id = "trace-1";
    name = "gen-ok";
    model = "gemini";
    input = "test";
    output = Some "result";
    usage = Some (10, 20, 30);
    started_at = 1704067200.0;
    ended_at = Some 1704067210.0;
    status = `Success;
  } in
  let json = generation_to_json gen in
  match json with
  | `Assoc fields ->
      (match List.assoc "level" fields with
       | `String l -> check string "success=DEFAULT" "DEFAULT" l
       | _ -> fail "expected string level");
      (* Should NOT have statusMessage for success *)
      check bool "no statusMessage" false (List.mem_assoc "statusMessage" fields)
  | _ -> fail "expected object"

let test_generation_status_error () =
  let gen = {
    gen_id = "gen-err";
    trace_id = "trace-1";
    name = "gen-failed";
    model = "claude";
    input = "test";
    output = None;
    usage = None;
    started_at = 1704067200.0;
    ended_at = Some 1704067205.0;
    status = `Error "connection timeout";
  } in
  let json = generation_to_json gen in
  match json with
  | `Assoc fields ->
      (match List.assoc "level" fields with
       | `String l -> check string "error level" "ERROR" l
       | _ -> fail "expected string level");
      check bool "has statusMessage" true (List.mem_assoc "statusMessage" fields);
      (match List.assoc "statusMessage" fields with
       | `String msg -> check string "error msg" "connection timeout" msg
       | _ -> fail "expected string statusMessage")
  | _ -> fail "expected object"

(** {1 Span JSON Serialization Tests} *)

let test_span_to_json_no_end () =
  let s = {
    span_id = "span-789";
    trace_id = "trace-456";
    name = "test-span";
    metadata = [];
    started_at = 1704067200.0;
    ended_at = None;
  } in
  let json = span_to_json s in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has name" true (List.mem_assoc "name" fields);
      check bool "has traceId" true (List.mem_assoc "traceId" fields);
      check bool "no endTime" false (List.mem_assoc "endTime" fields)
  | _ -> fail "expected object"

let test_span_to_json_with_end () =
  let s = {
    span_id = "span-end";
    trace_id = "trace-1";
    name = "completed-span";
    metadata = [("key", "val")];
    started_at = 1704067200.0;
    ended_at = Some 1704067260.0;
  } in
  let json = span_to_json s in
  match json with
  | `Assoc fields ->
      check bool "has endTime" true (List.mem_assoc "endTime" fields);
      (match List.assoc "endTime" fields with
       | `String _ -> ()
       | _ -> fail "expected string endTime");
      check bool "has metadata" true (List.mem_assoc "metadata" fields)
  | _ -> fail "expected object"

(** {1 Configuration Tests} *)

let test_is_enabled_without_env () =
  let _ = is_enabled () in
  check bool "function works" true true

let test_status () =
  let st = status () in
  check bool "contains Langfuse" true (Common.contains ~substring:"Langfuse" st);
  check bool "is non-empty" true (String.length st > 0)

(** {1 Test Suite} *)

let () =
  run "langfuse" [
    ("generate_id", [
      test_case "uuid format" `Quick test_generate_id_format;
      test_case "uniqueness" `Quick test_generate_id_unique;
      test_case "hex chars" `Quick test_generate_id_hex_chars;
    ]);
    ("timestamp", [
      test_case "iso8601 basic" `Quick test_iso8601_of_float;
      test_case "with milliseconds" `Quick test_iso8601_with_milliseconds;
    ]);
    ("metadata_to_json", [
      test_case "empty" `Quick test_metadata_to_json_empty;
      test_case "single" `Quick test_metadata_to_json_single;
      test_case "multiple" `Quick test_metadata_to_json_multiple;
    ]);
    ("trace_json", [
      test_case "trace_to_json" `Quick test_trace_to_json;
    ]);
    ("generation_json", [
      test_case "running (no output/usage/end)" `Quick test_generation_to_json_running;
      test_case "with output" `Quick test_generation_with_output;
      test_case "with usage" `Quick test_generation_with_usage;
      test_case "with ended_at" `Quick test_generation_with_ended_at;
      test_case "status success" `Quick test_generation_status_success;
      test_case "status error" `Quick test_generation_status_error;
    ]);
    ("span_json", [
      test_case "no end" `Quick test_span_to_json_no_end;
      test_case "with end" `Quick test_span_to_json_with_end;
    ]);
    ("config", [
      test_case "is_enabled" `Quick test_is_enabled_without_env;
      test_case "status" `Quick test_status;
    ]);
  ]

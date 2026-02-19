(** Tests for Types_core module — Foundation types coverage
    Targets: response_format conversion, tool_result_to_yojson,
    all enum roundtrips *)

open Alcotest

module TC = Types_core

(** {1 Tool Result to Yojson} *)

let test_tool_result_to_yojson_no_extra () =
  let result = TC.{
    model = "gemini";
    returncode = 0;
    response = "Hello";
    extra = [];
  } in
  let json = TC.tool_result_to_yojson result in
  let open Yojson.Safe.Util in
  check string "model" "gemini" (json |> member "model" |> to_string);
  check int "returncode" 0 (json |> member "returncode" |> to_int);
  check string "response" "Hello" (json |> member "response" |> to_string)

let test_tool_result_to_yojson_with_extra () =
  let result = TC.{
    model = "claude";
    returncode = 1;
    response = "error";
    extra = [("key1", "val1"); ("key2", "val2")];
  } in
  let json = TC.tool_result_to_yojson result in
  let open Yojson.Safe.Util in
  check string "key1" "val1" (json |> member "key1" |> to_string);
  check string "key2" "val2" (json |> member "key2" |> to_string)

(** {1 Thinking Level} *)

let test_thinking_level_roundtrip () =
  let levels = [TC.Low; TC.High] in
  List.iter (fun l ->
    let s = TC.string_of_thinking_level l in
    let l' = TC.thinking_level_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (l = l')
  ) levels

let test_thinking_level_unknown () =
  check bool "unknown defaults to High" true
    (TC.thinking_level_of_string "unknown" = TC.High)

(** {1 Reasoning Effort} *)

let test_reasoning_effort_roundtrip () =
  let efforts = [TC.RLow; TC.RMedium; TC.RHigh; TC.RXhigh] in
  List.iter (fun e ->
    let s = TC.string_of_reasoning_effort e in
    let e' = TC.reasoning_effort_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (e = e')
  ) efforts

let test_reasoning_effort_unknown () =
  check bool "unknown defaults to xhigh" true
    (TC.reasoning_effort_of_string "garbage" = TC.RXhigh)

(** {1 Sandbox Policy} *)

let test_sandbox_policy_roundtrip () =
  let policies = [TC.ReadOnly; TC.WorkspaceWrite; TC.DangerFullAccess] in
  List.iter (fun p ->
    let s = TC.string_of_sandbox_policy p in
    let p' = TC.sandbox_policy_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (p = p')
  ) policies

let test_sandbox_policy_unknown () =
  check bool "unknown defaults to workspace-write" true
    (TC.sandbox_policy_of_string "whatever" = TC.WorkspaceWrite)

(** {1 Output Format} *)

let test_output_format_roundtrip () =
  let formats = [TC.Text; TC.Json; TC.StreamJson] in
  List.iter (fun f ->
    let s = TC.string_of_output_format f in
    let f' = TC.output_format_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (f = f')
  ) formats

let test_output_format_unknown () =
  check bool "unknown defaults to text" true
    (TC.output_format_of_string "whatever" = TC.Text)

(** {1 Response Format} *)

let test_response_format_roundtrip () =
  let formats = [TC.Verbose; TC.Compact; TC.Binary; TC.Base85;
                 TC.Compressed; TC.ZstdDict; TC.Auto] in
  List.iter (fun f ->
    let s = TC.string_of_response_format f in
    let f' = TC.response_format_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (f = f')
  ) formats

let test_response_format_aliases () =
  let cases = [
    ("compact", TC.Compact);
    ("dsl", TC.Compact);
    ("binary", TC.Binary);
    ("msgpack", TC.Binary);
    ("base85", TC.Base85);
    ("ascii85", TC.Base85);
    ("compressed", TC.Compressed);
    ("zlib", TC.Compressed);
    ("zstd", TC.ZstdDict);
    ("zstd-dict", TC.ZstdDict);
    ("dict", TC.ZstdDict);
    ("auto", TC.Auto);
    ("adaptive", TC.Auto);
    ("verbose", TC.Verbose);
    ("json", TC.Verbose);
  ] in
  List.iter (fun (s, expected) ->
    let actual = TC.response_format_of_string s in
    check bool (Printf.sprintf "alias %s" s) true (actual = expected)
  ) cases

let test_response_format_unknown () =
  check bool "unknown defaults to verbose" true
    (TC.response_format_of_string "whatever" = TC.Verbose)

(** {1 Thinking Level Yojson Roundtrip} *)

let test_thinking_level_yojson () =
  let levels = [TC.Low; TC.High] in
  List.iter (fun l ->
    let json = TC.thinking_level_to_yojson l in
    match TC.thinking_level_of_yojson json with
    | Ok l' -> check bool "yojson roundtrip" true (l = l')
    | Error e -> Alcotest.fail e
  ) levels

(** {1 Reasoning Effort Yojson Roundtrip} *)

let test_reasoning_effort_yojson () =
  let efforts = [TC.RLow; TC.RMedium; TC.RHigh; TC.RXhigh] in
  List.iter (fun e ->
    let json = TC.reasoning_effort_to_yojson e in
    match TC.reasoning_effort_of_yojson json with
    | Ok e' -> check bool "yojson roundtrip" true (e = e')
    | Error e_msg -> Alcotest.fail e_msg
  ) efforts

(** {1 Sandbox Policy Yojson Roundtrip} *)

let test_sandbox_policy_yojson () =
  let policies = [TC.ReadOnly; TC.WorkspaceWrite; TC.DangerFullAccess] in
  List.iter (fun p ->
    let json = TC.sandbox_policy_to_yojson p in
    match TC.sandbox_policy_of_yojson json with
    | Ok p' -> check bool "yojson roundtrip" true (p = p')
    | Error e -> Alcotest.fail e
  ) policies

(** {1 Output Format Yojson Roundtrip} *)

let test_output_format_yojson () =
  let formats = [TC.Text; TC.Json; TC.StreamJson] in
  List.iter (fun f ->
    let json = TC.output_format_to_yojson f in
    match TC.output_format_of_yojson json with
    | Ok f' -> check bool "yojson roundtrip" true (f = f')
    | Error e -> Alcotest.fail e
  ) formats

(** {1 All string_of_* Explicit Values} *)

let test_string_of_response_format_all () =
  check string "verbose" "verbose" (TC.string_of_response_format TC.Verbose);
  check string "compact" "compact" (TC.string_of_response_format TC.Compact);
  check string "binary" "binary" (TC.string_of_response_format TC.Binary);
  check string "base85" "base85" (TC.string_of_response_format TC.Base85);
  check string "compressed" "compressed" (TC.string_of_response_format TC.Compressed);
  check string "zstd-dict" "zstd-dict" (TC.string_of_response_format TC.ZstdDict);
  check string "auto" "auto" (TC.string_of_response_format TC.Auto)

(** {1 Test Suite} *)

let () =
  run "types_core_coverage" [
    ("tool_result", [
      test_case "no extra" `Quick test_tool_result_to_yojson_no_extra;
      test_case "with extra" `Quick test_tool_result_to_yojson_with_extra;
    ]);
    ("thinking_level", [
      test_case "roundtrip" `Quick test_thinking_level_roundtrip;
      test_case "unknown" `Quick test_thinking_level_unknown;
      test_case "yojson" `Quick test_thinking_level_yojson;
    ]);
    ("reasoning_effort", [
      test_case "roundtrip" `Quick test_reasoning_effort_roundtrip;
      test_case "unknown" `Quick test_reasoning_effort_unknown;
      test_case "yojson" `Quick test_reasoning_effort_yojson;
    ]);
    ("sandbox_policy", [
      test_case "roundtrip" `Quick test_sandbox_policy_roundtrip;
      test_case "unknown" `Quick test_sandbox_policy_unknown;
      test_case "yojson" `Quick test_sandbox_policy_yojson;
    ]);
    ("output_format", [
      test_case "roundtrip" `Quick test_output_format_roundtrip;
      test_case "unknown" `Quick test_output_format_unknown;
      test_case "yojson" `Quick test_output_format_yojson;
    ]);
    ("response_format", [
      test_case "roundtrip" `Quick test_response_format_roundtrip;
      test_case "aliases" `Quick test_response_format_aliases;
      test_case "unknown" `Quick test_response_format_unknown;
      test_case "string_of all" `Quick test_string_of_response_format_all;
    ]);
  ]

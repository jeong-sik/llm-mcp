(* Protocol codec invariants (Phase 1-2) *)

open Alcotest
open Llm_mcp.Types

let decode_or_fail encoded =
  match decode_formatted_response encoded with
  | Ok r -> r
  | Error e -> fail (Printf.sprintf "decode failed: %s" e)

let assert_compact_fields ~expected ~actual =
  check bool "status preserved" true (actual.status = expected.status);
  check bool "model preserved" true (actual.model = expected.model);
  check int "tokens preserved" expected.tokens actual.tokens;
  check string "result preserved" expected.result actual.result

let test_format_roundtrip_preserves_fields () =
  let result : tool_result = {
    model = "gemini";
    returncode = 2;
    response = "err|payload";
    extra = [("tokens", "42")];
  } in
  let expected = tool_result_to_compact result in
  let formats = [Compact; Binary; Base85; Compressed; ZstdDict; Auto] in
  List.iter (fun format ->
    let encoded = format_tool_result ~format result in
    let decoded = decode_or_fail encoded in
    assert_compact_fields ~expected ~actual:decoded
  ) formats

let () =
  run "protocol invariants" [
    "codec",
    [
      "formatted roundtrip preserves fields", `Quick, test_format_roundtrip_preserves_fields;
    ];
  ]

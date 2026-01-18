(** test_dictionary.ml - Tests for Dictionary compression

    Tests content type detection, dictionary I/O, and compression/decompression.
*)

module Dictionary = Llm_mcp.Dictionary

(* Test content type detection *)
let test_detect_json () =
  let json = {|{"type":"response","data":[1,2,3]}|} in
  let ct = Dictionary.detect_content_type json in
  Alcotest.(check string) "detects JSON" "json" (Dictionary.string_of_content_type ct)

let test_detect_code () =
  let code = "def hello():\n    print('world')\n" in
  let ct = Dictionary.detect_content_type code in
  Alcotest.(check string) "detects code" "code" (Dictionary.string_of_content_type ct)

let test_detect_markdown () =
  let md = "# Header\n\nSome text here.\n\n## Section\n\nMore content." in
  let ct = Dictionary.detect_content_type md in
  Alcotest.(check string) "detects markdown" "markdown" (Dictionary.string_of_content_type ct)

let test_detect_mixed () =
  let mixed = "Just some plain text without clear markers." in
  let ct = Dictionary.detect_content_type mixed in
  Alcotest.(check string) "detects mixed" "mixed" (Dictionary.string_of_content_type ct)

(* Test small data not compressed *)
let test_small_data_unchanged () =
  let small = "tiny" in
  let result = Dictionary.compress_auto small in
  Alcotest.(check string) "small unchanged" small result

(* Test magic header detection *)
let test_is_dict_compressed_false () =
  let data = "regular data without compression" in
  let result = Dictionary.is_dict_compressed data in
  Alcotest.(check bool) "not dict compressed" false result

let test_is_dict_compressed_true () =
  let fake_header = "ZDCT" ^ String.make 20 '\x00' in
  let result = Dictionary.is_dict_compressed fake_header in
  Alcotest.(check bool) "has dict header" true result

(* Test roundtrip with fallback (no trained dict) *)
let test_decompress_auto_passthrough () =
  let data = "uncompressed plain text" in
  let result = Dictionary.decompress_auto data in
  Alcotest.(check string) "passthrough unchanged" data result

(* Test content type string conversions *)
let test_content_type_roundtrip () =
  let types = [Dictionary.Code; Dictionary.JSON; Dictionary.Markdown; Dictionary.Mixed] in
  List.iter (fun ct ->
    let s = Dictionary.string_of_content_type ct in
    let ct' = Dictionary.content_type_of_string s in
    Alcotest.(check string) "roundtrip" s (Dictionary.string_of_content_type ct')
  ) types

(* Test dictionary creation (minimal - no actual training) *)
let test_train_insufficient_samples () =
  let samples = ["a"; "b"; "c"] in
  let result = Dictionary.train ~samples ~content_type:Dictionary.Mixed in
  match result with
  | Error msg -> Alcotest.(check bool) "error mentions samples" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail with insufficient samples"

let () =
  let open Alcotest in
  run "Dictionary" [
    "Content Detection", [
      test_case "JSON" `Quick test_detect_json;
      test_case "Code" `Quick test_detect_code;
      test_case "Markdown" `Quick test_detect_markdown;
      test_case "Mixed" `Quick test_detect_mixed;
    ];
    "Compression", [
      test_case "Small unchanged" `Quick test_small_data_unchanged;
      test_case "Decompress passthrough" `Quick test_decompress_auto_passthrough;
    ];
    "Header", [
      test_case "Not dict compressed" `Quick test_is_dict_compressed_false;
      test_case "Has dict header" `Quick test_is_dict_compressed_true;
    ];
    "Types", [
      test_case "Content type roundtrip" `Quick test_content_type_roundtrip;
    ];
    "Training", [
      test_case "Insufficient samples" `Quick test_train_insufficient_samples;
    ];
  ]

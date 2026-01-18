(** Tests for Common module - shared utilities *)

open Alcotest

(** Test me_path builds correct paths *)
let test_me_path () =
  let path = Llm_mcp.Common.me_path ["logs"; "test.log"] in
  check bool "contains logs" true (String.length path > 0);
  check bool "ends with test.log" true
    (Filename.basename path = "test.log");
  check bool "has logs dir" true
    (String.sub (Filename.dirname path)
       (String.length (Filename.dirname path) - 4) 4 = "logs")

(** Test take function *)
let test_take () =
  let lst = [1; 2; 3; 4; 5] in
  check (list int) "take 3" [1; 2; 3] (Llm_mcp.Common.take 3 lst);
  check (list int) "take 0" [] (Llm_mcp.Common.take 0 lst);
  check (list int) "take 10" [1; 2; 3; 4; 5] (Llm_mcp.Common.take 10 lst);
  check (list int) "take from empty" [] (Llm_mcp.Common.take 3 [])

(** Test contains substring *)
let test_contains () =
  check bool "contains hello" true
    (Llm_mcp.Common.contains ~substring:"hello" "hello world");
  check bool "contains world" true
    (Llm_mcp.Common.contains ~substring:"world" "hello world");
  check bool "not contains foo" false
    (Llm_mcp.Common.contains ~substring:"foo" "hello world");
  check bool "empty substring" true
    (Llm_mcp.Common.contains ~substring:"" "anything")

(** Test assoc_opt *)
let test_assoc_opt () =
  let lst = [("a", 1); ("b", 2); ("c", 3)] in
  check (option int) "found a" (Some 1) (Llm_mcp.Common.assoc_opt "a" lst);
  check (option int) "found c" (Some 3) (Llm_mcp.Common.assoc_opt "c" lst);
  check (option int) "not found" None (Llm_mcp.Common.assoc_opt "z" lst)

(** Test json_escape *)
let test_json_escape () =
  check string "no escape needed" "hello" (Llm_mcp.Common.json_escape "hello");
  check string "escape quotes" "say \\\"hi\\\"" (Llm_mcp.Common.json_escape "say \"hi\"");
  check string "escape newline" "line1\\nline2" (Llm_mcp.Common.json_escape "line1\nline2");
  check string "escape backslash" "path\\\\to" (Llm_mcp.Common.json_escape "path\\to");
  check string "escape tab" "col1\\tcol2" (Llm_mcp.Common.json_escape "col1\tcol2")

(** Test date formatting *)
let test_date_str () =
  let date = Llm_mcp.Common.date_str () in
  (* Format: YYYY-MM-DD *)
  check int "length 10" 10 (String.length date);
  check bool "has dashes" true (date.[4] = '-' && date.[7] = '-');
  (* Year should start with 20 *)
  check bool "valid year" true (String.sub date 0 2 = "20")

(** Test ISO timestamp *)
let test_iso_timestamp () =
  let ts = Llm_mcp.Common.iso_timestamp () in
  (* Format: YYYY-MM-DDTHH:MM:SS *)
  check int "length 19" 19 (String.length ts);
  check bool "has T separator" true (ts.[10] = 'T');
  check bool "has colons" true (ts.[13] = ':' && ts.[16] = ':')

(** Test timestamp is integer *)
let test_timestamp () =
  let ts = Llm_mcp.Common.timestamp () in
  (* Should be a reasonable Unix timestamp (> 2020-01-01) *)
  check bool "reasonable timestamp" true (ts > 1577836800)

(** Test date_n_days_ago *)
let test_date_n_days_ago () =
  let today = Llm_mcp.Common.date_str () in
  let zero_days = Llm_mcp.Common.date_n_days_ago 0 in
  check string "0 days ago is today" today zero_days;
  let yesterday = Llm_mcp.Common.date_n_days_ago 1 in
  check int "yesterday length" 10 (String.length yesterday)

(** Test read_file_opt on non-existent file *)
let test_read_file_opt_missing () =
  let result = Llm_mcp.Common.read_file_opt "/nonexistent/path/file.txt" in
  check (option string) "missing file returns None" None result

(** Test read_file_opt and write_file round-trip *)
let test_file_roundtrip () =
  let tmp = Filename.temp_file "test_common_" ".txt" in
  let content = "test content with unicode: 한글 🎉" in
  let wrote = Llm_mcp.Common.write_file tmp content in
  check bool "write succeeded" true wrote;
  let read = Llm_mcp.Common.read_file_opt tmp in
  check (option string) "read matches written" (Some content) read;
  Sys.remove tmp

let () =
  run "Llm_mcp.Common" [
    "me_path", [
      test_case "builds path" `Quick test_me_path;
    ];
    "take", [
      test_case "list operations" `Quick test_take;
    ];
    "contains", [
      test_case "substring search" `Quick test_contains;
    ];
    "assoc_opt", [
      test_case "safe assoc" `Quick test_assoc_opt;
    ];
    "json_escape", [
      test_case "escape special chars" `Quick test_json_escape;
    ];
    "date_str", [
      test_case "format" `Quick test_date_str;
    ];
    "iso_timestamp", [
      test_case "format" `Quick test_iso_timestamp;
    ];
    "timestamp", [
      test_case "integer" `Quick test_timestamp;
    ];
    "date_n_days_ago", [
      test_case "calculates past dates" `Quick test_date_n_days_ago;
    ];
    "file_io", [
      test_case "missing file" `Quick test_read_file_opt_missing;
      test_case "roundtrip" `Quick test_file_roundtrip;
    ];
  ]

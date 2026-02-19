(** Tests for Common module — coverage push for utility functions *)

open Alcotest

(* ============================================================
   ensure_dir / rm_rf
   ============================================================ *)

let test_ensure_dir_creates_nested () =
  let base = Filename.concat (Filename.get_temp_dir_name ()) "llm_mcp_test_ensure" in
  let nested = Filename.concat (Filename.concat base "a") "b" in
  Common.rm_rf base;
  Common.ensure_dir nested;
  check bool "nested exists" true (Sys.file_exists nested);
  Common.rm_rf base

let test_rm_rf_removes_tree () =
  let base = Filename.concat (Filename.get_temp_dir_name ()) "llm_mcp_test_rmrf" in
  Common.ensure_dir (Filename.concat base "sub");
  let f = Filename.concat base "sub/file.txt" in
  ignore (Common.write_file f "test");
  Common.rm_rf base;
  check bool "removed" false (Sys.file_exists base)

let test_rm_rf_nonexistent () =
  Common.rm_rf "/tmp/llm_mcp_absolutely_no_such_dir_999";
  check bool "no error" true true

(* ============================================================
   which / command_exists
   ============================================================ *)

let test_which_found () =
  match Common.which "sh" with
  | Some _ -> check bool "found" true true
  | None -> fail "sh should be found in PATH"

let test_which_not_found () =
  check (option string) "not found" None (Common.which "llm_mcp_no_such_binary_999")

let test_command_exists_true () =
  check bool "sh exists" true (Common.command_exists "sh")

let test_command_exists_false () =
  check bool "missing" false (Common.command_exists "llm_mcp_no_such_binary_999")

(* ============================================================
   read_file_opt / write_file / read_json_opt / write_json
   ============================================================ *)

let test_read_file_opt_exists () =
  let p = Filename.temp_file "llm_mcp_test_" ".txt" in
  ignore (Common.write_file p "hello");
  (match Common.read_file_opt p with
   | Some s -> check string "content" "hello" s
   | None -> fail "should read file");
  Sys.remove p

let test_read_file_opt_missing () =
  check (option reject) "missing" None (Common.read_file_opt "/tmp/llm_mcp_no_such_99999.txt")

let test_read_json_opt_valid () =
  let p = Filename.temp_file "llm_mcp_test_" ".json" in
  ignore (Common.write_file p {|{"a":1}|});
  (match Common.read_json_opt p with
   | Some _ -> check bool "parsed" true true
   | None -> fail "should parse json");
  Sys.remove p

let test_read_json_opt_invalid () =
  let p = Filename.temp_file "llm_mcp_test_" ".json" in
  ignore (Common.write_file p "not json{{{");
  check (option reject) "invalid json" None (Common.read_json_opt p);
  Sys.remove p

let test_read_json_opt_missing () =
  check (option reject) "missing" None (Common.read_json_opt "/tmp/llm_mcp_no_such_99999.json")

let test_write_json_roundtrip () =
  let p = Filename.temp_file "llm_mcp_test_" ".json" in
  let json = `Assoc [("a", `Int 1)] in
  let ok = Common.write_json p json in
  check bool "write ok" true ok;
  (match Common.read_json_opt p with
   | Some j -> check int "roundtrip" 1 (Yojson.Safe.Util.member "a" j |> Yojson.Safe.Util.to_int)
   | None -> fail "should read back");
  Sys.remove p

(* ============================================================
   take / contains / assoc_opt
   ============================================================ *)

let test_take_normal () =
  check (list int) "take 2" [1; 2] (Common.take 2 [1; 2; 3])

let test_take_zero () =
  check (list int) "take 0" [] (Common.take 0 [1; 2; 3])

let test_take_more () =
  check (list int) "take 5" [1; 2] (Common.take 5 [1; 2])

let test_take_empty () =
  check (list int) "take empty" [] (Common.take 3 [])

let test_contains_true () =
  check bool "found" true (Common.contains ~substring:"world" "hello world")

let test_contains_false () =
  check bool "not found" false (Common.contains ~substring:"xyz" "hello world")

let test_contains_empty_substring () =
  check bool "empty sub" true (Common.contains ~substring:"" "hello")

let test_assoc_opt_found () =
  check (option int) "found" (Some 2) (Common.assoc_opt "b" [("a", 1); ("b", 2)])

let test_assoc_opt_missing () =
  check (option int) "missing" None (Common.assoc_opt "c" [("a", 1); ("b", 2)])

(* ============================================================
   env_true
   ============================================================ *)

let test_env_true_missing () =
  check bool "missing is false" false (Common.env_true "LLM_MCP_FAKE_ENV_99999")

let test_env_true_set () =
  Unix.putenv "LLM_MCP_TEST_ENV_TRUE" "1";
  check bool "1 is true" true (Common.env_true "LLM_MCP_TEST_ENV_TRUE");
  Unix.putenv "LLM_MCP_TEST_ENV_TRUE" "true";
  check bool "true is true" true (Common.env_true "LLM_MCP_TEST_ENV_TRUE");
  Unix.putenv "LLM_MCP_TEST_ENV_TRUE" "yes";
  check bool "yes is true" true (Common.env_true "LLM_MCP_TEST_ENV_TRUE");
  Unix.putenv "LLM_MCP_TEST_ENV_TRUE" "on";
  check bool "on is true" true (Common.env_true "LLM_MCP_TEST_ENV_TRUE")

let test_env_true_false_values () =
  Unix.putenv "LLM_MCP_TEST_ENV_TRUE" "0";
  check bool "0 is false" false (Common.env_true "LLM_MCP_TEST_ENV_TRUE");
  Unix.putenv "LLM_MCP_TEST_ENV_TRUE" "false";
  check bool "false is false" false (Common.env_true "LLM_MCP_TEST_ENV_TRUE");
  Unix.putenv "LLM_MCP_TEST_ENV_TRUE" "no";
  check bool "no is false" false (Common.env_true "LLM_MCP_TEST_ENV_TRUE")

(* ============================================================
   read_lines / read_lines_tail
   ============================================================ *)

let test_read_lines () =
  let p = Filename.temp_file "llm_mcp_test_" ".txt" in
  ignore (Common.write_file p "a\nb\nc\n");
  let lines = Common.read_lines p in
  check (list string) "lines" ["a"; "b"; "c"] lines;
  Sys.remove p

let test_read_lines_empty () =
  let p = Filename.temp_file "llm_mcp_test_" ".txt" in
  ignore (Common.write_file p "");
  let lines = Common.read_lines p in
  check (list string) "empty" [] lines;
  Sys.remove p

let test_read_lines_missing () =
  let lines = Common.read_lines "/tmp/llm_mcp_no_such_99999.txt" in
  check (list string) "missing" [] lines

let test_read_lines_tail () =
  let p = Filename.temp_file "llm_mcp_test_" ".txt" in
  ignore (Common.write_file p "line1\nline2\nline3\nline4\nline5\n");
  let lines = Common.read_lines_tail ~max_bytes:1000 ~max_lines:2 p in
  check (list string) "last 2" ["line4"; "line5"] lines;
  Sys.remove p

let test_read_lines_tail_zero_bytes () =
  check (list string) "zero bytes" [] (Common.read_lines_tail ~max_bytes:0 ~max_lines:10 "/dev/null")

let test_read_lines_tail_zero_lines () =
  check (list string) "zero lines" [] (Common.read_lines_tail ~max_bytes:1000 ~max_lines:0 "/dev/null")

let test_read_lines_tail_not_regular () =
  check (list string) "not regular" [] (Common.read_lines_tail ~max_bytes:1000 ~max_lines:10 "/dev")

(* ============================================================
   list_files_with_suffix
   ============================================================ *)

let test_list_files_with_suffix () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "llm_mcp_test_suffix" in
  Common.ensure_dir dir;
  ignore (Common.write_file (Filename.concat dir "a.ml") "");
  ignore (Common.write_file (Filename.concat dir "b.txt") "");
  ignore (Common.write_file (Filename.concat dir "c.ml") "");
  let files = Common.list_files_with_suffix dir ".ml" |> List.sort String.compare in
  check int "count" 2 (List.length files);
  Common.rm_rf dir

let test_list_files_with_suffix_missing_dir () =
  let files = Common.list_files_with_suffix "/tmp/llm_mcp_no_such_dir_999" ".ml" in
  check (list string) "empty" [] files

(* ============================================================
   json_escape
   ============================================================ *)

let test_json_escape_special () =
  let s = "a\\b\"c\nd\re\tf" in
  let escaped = Common.json_escape s in
  check string "escaped" "a\\\\b\\\"c\\nd\\re\\tf" escaped

let test_json_escape_empty () =
  check string "empty" "" (Common.json_escape "")

let test_json_escape_plain () =
  check string "plain" "hello" (Common.json_escape "hello")

(* ============================================================
   format_result_output
   ============================================================ *)

let test_format_result_output_with_warnings () =
  let json = `Assoc [
    ("context", `List [`String "ctx1"]);
    ("warnings", `List [`String "warn1"]);
  ] in
  let out = Common.format_result_output json in
  check bool "has context" true (Common.contains ~substring:"ctx1" out);
  check bool "has warning" true (Common.contains ~substring:"warn1" out)

let test_format_result_output_empty () =
  let json = `Assoc [] in
  let out = Common.format_result_output json in
  check string "empty" "" out

(* ============================================================
   protect
   ============================================================ *)

let test_protect_success () =
  let finally_ran = ref false in
  let r = Common.protect ~module_name:"Test" ~finally_label:"test"
    ~finally:(fun () -> finally_ran := true) (fun () -> 42) in
  check int "result" 42 r;
  check bool "finally ran" true !finally_ran

let test_protect_exception () =
  let finally_ran = ref false in
  let raised = ref false in
  (try
    ignore (Common.protect ~module_name:"Test" ~finally_label:"test"
      ~finally:(fun () -> finally_ran := true)
      (fun () -> failwith "boom"))
  with Failure _ -> raised := true);
  check bool "raised" true !raised;
  check bool "finally ran" true !finally_ran

(* ============================================================
   Runner
   ============================================================ *)

let () =
  run "common_coverage" [
    "dirs", [
      test_case "ensure_dir nested" `Quick test_ensure_dir_creates_nested;
      test_case "rm_rf tree" `Quick test_rm_rf_removes_tree;
      test_case "rm_rf nonexistent" `Quick test_rm_rf_nonexistent;
    ];
    "which", [
      test_case "found" `Quick test_which_found;
      test_case "not found" `Quick test_which_not_found;
      test_case "command_exists true" `Quick test_command_exists_true;
      test_case "command_exists false" `Quick test_command_exists_false;
    ];
    "file_io", [
      test_case "read_file_opt exists" `Quick test_read_file_opt_exists;
      test_case "read_file_opt missing" `Quick test_read_file_opt_missing;
      test_case "read_json valid" `Quick test_read_json_opt_valid;
      test_case "read_json invalid" `Quick test_read_json_opt_invalid;
      test_case "read_json missing" `Quick test_read_json_opt_missing;
      test_case "write_json roundtrip" `Quick test_write_json_roundtrip;
    ];
    "collections", [
      test_case "take normal" `Quick test_take_normal;
      test_case "take zero" `Quick test_take_zero;
      test_case "take more" `Quick test_take_more;
      test_case "take empty" `Quick test_take_empty;
      test_case "contains true" `Quick test_contains_true;
      test_case "contains false" `Quick test_contains_false;
      test_case "contains empty" `Quick test_contains_empty_substring;
      test_case "assoc_opt found" `Quick test_assoc_opt_found;
      test_case "assoc_opt missing" `Quick test_assoc_opt_missing;
    ];
    "env", [
      test_case "env_true missing" `Quick test_env_true_missing;
      test_case "env_true set" `Quick test_env_true_set;
      test_case "env_true false" `Quick test_env_true_false_values;
    ];
    "read_lines", [
      test_case "read_lines" `Quick test_read_lines;
      test_case "read_lines empty" `Quick test_read_lines_empty;
      test_case "read_lines missing" `Quick test_read_lines_missing;
      test_case "tail" `Quick test_read_lines_tail;
      test_case "tail zero bytes" `Quick test_read_lines_tail_zero_bytes;
      test_case "tail zero lines" `Quick test_read_lines_tail_zero_lines;
      test_case "tail not regular" `Quick test_read_lines_tail_not_regular;
    ];
    "list_files", [
      test_case "with suffix" `Quick test_list_files_with_suffix;
      test_case "missing dir" `Quick test_list_files_with_suffix_missing_dir;
    ];
    "json_escape", [
      test_case "special chars" `Quick test_json_escape_special;
      test_case "empty" `Quick test_json_escape_empty;
      test_case "plain" `Quick test_json_escape_plain;
    ];
    "format_output", [
      test_case "with warnings" `Quick test_format_result_output_with_warnings;
      test_case "empty" `Quick test_format_result_output_empty;
    ];
    "protect", [
      test_case "success" `Quick test_protect_success;
      test_case "exception" `Quick test_protect_exception;
    ];
  ]

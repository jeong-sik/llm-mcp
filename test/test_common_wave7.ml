(** Wave 7: Additional common.ml coverage tests.
    Focus on timestamp functions, read_lines_tail, list_files_with_suffix,
    me_path, format_result_output, json_escape, write_file, protect. *)

open Alcotest
open Common

let check_str = check string
let check_int = check int
let check_bool = check bool

let tmp_dir () =
  let d = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "test_common_%d" (Random.int 100000)) in
  ensure_dir d;
  d

(* ---- timestamp ---- *)
let test_timestamp_positive () =
  let ts = timestamp () in
  check_bool "positive" true (ts > 0)

(* ---- time_str format ---- *)
let test_time_str_format () =
  let s = time_str () in
  (* Format: YYYY-MM-DD HH:MM:SS = 19 chars *)
  check_int "length 19" 19 (String.length s);
  check_bool "has dash" true (String.contains s '-');
  check_bool "has colon" true (String.contains s ':')

(* ---- date_str format ---- *)
let test_date_str_format () =
  let s = date_str () in
  (* Format: YYYY-MM-DD = 10 chars *)
  check_int "length 10" 10 (String.length s);
  check_bool "has dash" true (String.contains s '-')

(* ---- iso_timestamp format ---- *)
let test_iso_timestamp_format () =
  let s = iso_timestamp () in
  (* Format: YYYY-MM-DDTHH:MM:SS = 19 chars *)
  check_int "length 19" 19 (String.length s);
  check_bool "has T" true (String.contains s 'T')

(* ---- date_n_days_ago ---- *)
let test_date_n_days_ago_today () =
  let today = date_str () in
  let ago0 = date_n_days_ago 0 in
  check_str "0 days ago = today" today ago0

let test_date_n_days_ago_1 () =
  let ago = date_n_days_ago 1 in
  check_int "length 10" 10 (String.length ago);
  check_bool "has dash" true (String.contains ago '-')

let test_date_n_days_ago_30 () =
  let ago = date_n_days_ago 30 in
  check_int "length 10" 10 (String.length ago)

(* ---- month_dir format ---- *)
let test_month_dir_format () =
  let s = month_dir () in
  (* Format: YYYY-MM = 7 chars *)
  check_int "length 7" 7 (String.length s);
  check_bool "has dash" true (String.contains s '-')

(* ---- day_str format ---- *)
let test_day_str_format () =
  let s = day_str () in
  (* Format: DD = 2 chars *)
  check_int "length 2" 2 (String.length s)

(* ---- me_root ---- *)
let test_me_root_nonempty () =
  check_bool "nonempty" true (String.length me_root > 0)

(* ---- me_path ---- *)
let test_me_path_simple () =
  let p = me_path ["a"; "b"] in
  check_bool "contains a/b" true (contains ~substring:"a/b" p);
  check_bool "starts with me_root" true
    (String.length p >= String.length me_root &&
     String.sub p 0 (String.length me_root) = me_root)

let test_me_path_empty () =
  let p = me_path [] in
  check_str "empty parts = me_root" me_root p

(* ---- write_file / read_file_opt ---- *)
let test_write_read_roundtrip () =
  let d = tmp_dir () in
  let path = Filename.concat d "test.txt" in
  let ok = write_file path "hello world" in
  check_bool "write ok" true ok;
  (match read_file_opt path with
   | Some s -> check_str "content" "hello world" s
   | None -> fail "expected Some");
  rm_rf d

let test_write_file_bad_path () =
  let ok = write_file "/nonexistent/dir/file.txt" "data" in
  check_bool "write fails" false ok

(* ---- read_lines_tail with large content ---- *)
let test_read_lines_tail_large () =
  let d = tmp_dir () in
  let path = Filename.concat d "big.txt" in
  let lines = List.init 100 (fun i -> Printf.sprintf "line %d" i) in
  let content = String.concat "\n" lines ^ "\n" in
  ignore (write_file path content);
  (* Read last 5 lines from last 256 bytes *)
  let result = read_lines_tail ~max_bytes:256 ~max_lines:5 path in
  check_int "5 lines" 5 (List.length result);
  check_bool "has line 99" true
    (List.exists (fun l -> contains ~substring:"99" l) result);
  rm_rf d

let test_read_lines_tail_small_file () =
  let d = tmp_dir () in
  let path = Filename.concat d "small.txt" in
  ignore (write_file path "a\nb\nc\n");
  let result = read_lines_tail ~max_bytes:10000 ~max_lines:10 path in
  check_int "3 lines" 3 (List.length result);
  rm_rf d

let test_read_lines_tail_missing () =
  let result = read_lines_tail ~max_bytes:100 ~max_lines:5 "/nonexistent/file" in
  check_int "empty" 0 (List.length result)

(* ---- list_files_with_suffix ---- *)
let test_list_files_suffix () =
  let d = tmp_dir () in
  ignore (write_file (Filename.concat d "a.ml") "");
  ignore (write_file (Filename.concat d "b.ml") "");
  ignore (write_file (Filename.concat d "c.txt") "");
  let result = list_files_with_suffix d ".ml" in
  check_int "2 ml files" 2 (List.length result);
  rm_rf d

let test_list_files_suffix_empty_dir () =
  let d = tmp_dir () in
  let result = list_files_with_suffix d ".ml" in
  check_int "0 files" 0 (List.length result);
  rm_rf d

let test_list_files_suffix_no_match () =
  let d = tmp_dir () in
  ignore (write_file (Filename.concat d "a.txt") "");
  let result = list_files_with_suffix d ".ml" in
  check_int "no match" 0 (List.length result);
  rm_rf d

(* ---- read_lines ---- *)
let test_read_lines_normal () =
  let d = tmp_dir () in
  let path = Filename.concat d "lines.txt" in
  ignore (write_file path "one\ntwo\nthree\n");
  let result = read_lines path in
  check_int "3 lines" 3 (List.length result);
  check_str "first" "one" (List.hd result);
  rm_rf d

(* ---- json_escape ---- *)
let test_json_escape_all_special () =
  let s = json_escape "a\tb\nc\\d\"e\r" in
  check_bool "tab" true (contains ~substring:"\\t" s);
  check_bool "newline" true (contains ~substring:"\\n" s);
  check_bool "backslash" true (contains ~substring:"\\\\" s);
  check_bool "quote" true (contains ~substring:"\\\"" s);
  check_bool "cr" true (contains ~substring:"\\r" s)

(* ---- format_result_output ---- *)
let test_format_result_with_context () =
  let j = `Assoc [
    "context", `List [`String "ctx1"; `String "ctx2"];
    "warnings", `List [`String "warn1"]
  ] in
  let out = format_result_output j in
  check_bool "has context" true (contains ~substring:"Context" out);
  check_bool "has ctx1" true (contains ~substring:"ctx1" out);
  check_bool "has warnings" true (contains ~substring:"Warnings" out);
  check_bool "has warn1" true (contains ~substring:"warn1" out)

let test_format_result_empty_json () =
  let j = `Assoc [] in
  let out = format_result_output j in
  check_str "empty output" "" out

let test_format_result_no_context () =
  let j = `Assoc ["warnings", `List [`String "w1"]] in
  let out = format_result_output j in
  check_bool "no context section" false (contains ~substring:"Context" out);
  check_bool "has warnings" true (contains ~substring:"w1" out)

let test_format_result_no_warnings () =
  let j = `Assoc ["context", `List [`String "c1"]] in
  let out = format_result_output j in
  check_bool "has context" true (contains ~substring:"c1" out);
  check_bool "no warnings section" false (contains ~substring:"Warnings" out)

(* ---- which / command_exists ---- *)
let test_which_absolute_path () =
  (* /bin/ls exists on macOS *)
  (match which "/bin/ls" with
   | Some p -> check_str "absolute path" "/bin/ls" p
   | None -> check_bool "ls should exist" true false)

let test_which_nonexistent_absolute () =
  let result = which "/nonexistent/program" in
  check_bool "None" true (Option.is_none result)

(* ---- env_true ---- *)
let test_env_true_on () =
  Unix.putenv "TEST_ENV_TRUE" "on";
  check_bool "on is true" true (env_true "TEST_ENV_TRUE");
  Unix.putenv "TEST_ENV_TRUE" ""

let test_env_true_yes () =
  Unix.putenv "TEST_ENV_TRUE" "yes";
  check_bool "yes is true" true (env_true "TEST_ENV_TRUE");
  Unix.putenv "TEST_ENV_TRUE" ""

let test_env_true_1 () =
  Unix.putenv "TEST_ENV_TRUE" "1";
  check_bool "1 is true" true (env_true "TEST_ENV_TRUE");
  Unix.putenv "TEST_ENV_TRUE" ""

let test_env_true_TRUE () =
  Unix.putenv "TEST_ENV_TRUE" "TRUE";
  check_bool "TRUE is true" true (env_true "TEST_ENV_TRUE");
  Unix.putenv "TEST_ENV_TRUE" ""

let test_env_true_no () =
  Unix.putenv "TEST_ENV_TRUE" "no";
  check_bool "no is false" false (env_true "TEST_ENV_TRUE");
  Unix.putenv "TEST_ENV_TRUE" ""

(* ---- protect: success case ---- *)
let test_protect_success () =
  let cleaned = ref false in
  let v = protect ~module_name:"test" ~finally_label:"cleanup"
    ~finally:(fun () -> cleaned := true)
    (fun () -> 42) in
  check_int "value" 42 v;
  check_bool "cleaned" true !cleaned

(* ---- protect: exception case ---- *)
let test_protect_exception () =
  let cleaned = ref false in
  (try
    let _ = protect ~module_name:"test" ~finally_label:"cleanup"
      ~finally:(fun () -> cleaned := true)
      (fun () -> failwith "boom") in
    ()
  with Failure _ -> ());
  check_bool "cleaned after exception" true !cleaned

(* ---- ensure_dir nested ---- *)
let test_ensure_dir_deeply_nested () =
  let d = tmp_dir () in
  let deep = Filename.concat (Filename.concat (Filename.concat d "a") "b") "c" in
  ensure_dir deep;
  check_bool "exists" true (Sys.file_exists deep && Sys.is_directory deep);
  rm_rf d

(* ---- rm_rf on file ---- *)
let test_rm_rf_file () =
  let d = tmp_dir () in
  let f = Filename.concat d "file.txt" in
  ignore (write_file f "data");
  rm_rf f;
  check_bool "removed" false (Sys.file_exists f);
  rm_rf d

(* ---- read_json_opt invalid ---- *)
let test_read_json_opt_invalid_json () =
  let d = tmp_dir () in
  let f = Filename.concat d "bad.json" in
  ignore (write_file f "not json{");
  (match read_json_opt f with
   | None -> ()
   | Some _ -> fail "expected None for invalid json");
  rm_rf d

(* ---- write_json / read_json roundtrip ---- *)
let test_write_json_read_json () =
  let d = tmp_dir () in
  let f = Filename.concat d "test.json" in
  let j = `Assoc ["key", `String "value"; "num", `Int 42] in
  ignore (write_json f j);
  (match read_json_opt f with
   | Some j2 ->
     let open Yojson.Safe.Util in
     check_str "key" "value" (j2 |> member "key" |> to_string);
     check_int "num" 42 (j2 |> member "num" |> to_int)
   | None -> fail "expected Some");
  rm_rf d

(* ---- run_command ---- *)
let test_run_command_echo () =
  let result = run_command "echo" ["hello"] in
  check_bool "has output" true (List.length result > 0);
  check_str "content" "hello" (List.hd result)

let test_run_command_failure () =
  let result = run_command "false" [] in
  check_int "empty on failure" 0 (List.length result)

(* ---- run_cmd ---- *)
let test_run_cmd_echo () =
  let result = run_cmd "echo" ["world"] in
  check_bool "has world" true (contains ~substring:"world" result)

let () =
  run "common_wave7" [
    "timestamps", [
      test_case "timestamp positive" `Quick test_timestamp_positive;
      test_case "time_str format" `Quick test_time_str_format;
      test_case "date_str format" `Quick test_date_str_format;
      test_case "iso_timestamp format" `Quick test_iso_timestamp_format;
      test_case "date_n_days_ago 0" `Quick test_date_n_days_ago_today;
      test_case "date_n_days_ago 1" `Quick test_date_n_days_ago_1;
      test_case "date_n_days_ago 30" `Quick test_date_n_days_ago_30;
      test_case "month_dir format" `Quick test_month_dir_format;
      test_case "day_str format" `Quick test_day_str_format;
    ];
    "paths", [
      test_case "me_root nonempty" `Quick test_me_root_nonempty;
      test_case "me_path simple" `Quick test_me_path_simple;
      test_case "me_path empty" `Quick test_me_path_empty;
    ];
    "file_io", [
      test_case "write/read roundtrip" `Quick test_write_read_roundtrip;
      test_case "write bad path" `Quick test_write_file_bad_path;
      test_case "read_lines normal" `Quick test_read_lines_normal;
      test_case "read_lines_tail large" `Quick test_read_lines_tail_large;
      test_case "read_lines_tail small" `Quick test_read_lines_tail_small_file;
      test_case "read_lines_tail missing" `Quick test_read_lines_tail_missing;
      test_case "list_files suffix" `Quick test_list_files_suffix;
      test_case "list_files empty" `Quick test_list_files_suffix_empty_dir;
      test_case "list_files no match" `Quick test_list_files_suffix_no_match;
      test_case "read_json invalid" `Quick test_read_json_opt_invalid_json;
      test_case "write/read json" `Quick test_write_json_read_json;
    ];
    "json", [
      test_case "escape all special" `Quick test_json_escape_all_special;
    ];
    "format", [
      test_case "result with context" `Quick test_format_result_with_context;
      test_case "result empty" `Quick test_format_result_empty_json;
      test_case "result no context" `Quick test_format_result_no_context;
      test_case "result no warnings" `Quick test_format_result_no_warnings;
    ];
    "which", [
      test_case "absolute path" `Quick test_which_absolute_path;
      test_case "nonexistent absolute" `Quick test_which_nonexistent_absolute;
    ];
    "env", [
      test_case "env_true on" `Quick test_env_true_on;
      test_case "env_true yes" `Quick test_env_true_yes;
      test_case "env_true 1" `Quick test_env_true_1;
      test_case "env_true TRUE" `Quick test_env_true_TRUE;
      test_case "env_true no" `Quick test_env_true_no;
    ];
    "protect", [
      test_case "success" `Quick test_protect_success;
      test_case "exception" `Quick test_protect_exception;
    ];
    "fs_ops", [
      test_case "ensure_dir deep" `Quick test_ensure_dir_deeply_nested;
      test_case "rm_rf file" `Quick test_rm_rf_file;
    ];
    "subprocess", [
      test_case "run_command echo" `Quick test_run_command_echo;
      test_case "run_command failure" `Quick test_run_command_failure;
      test_case "run_cmd echo" `Quick test_run_cmd_echo;
    ];
  ]

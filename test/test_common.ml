(** Tests for Common module - shared utilities *)

open Alcotest

let set_env name value = Unix.putenv name value
let unset_env name = Unix.putenv name ""

let with_env name value f =
  let prev = Sys.getenv_opt name in
  (match value with
   | Some v -> set_env name v
   | None -> unset_env name);
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> set_env name v
      | None -> unset_env name)
    f

(** Test me_path builds correct paths *)
let test_me_path () =
  let path = Common.me_path ["logs"; "test.log"] in
  check bool "contains logs" true (String.length path > 0);
  check bool "ends with test.log" true
    (Filename.basename path = "test.log");
  check bool "has logs dir" true
    (String.sub (Filename.dirname path)
       (String.length (Filename.dirname path) - 4) 4 = "logs")

(** Test take function *)
let test_take () =
  let lst = [1; 2; 3; 4; 5] in
  check (list int) "take 3" [1; 2; 3] (Common.take 3 lst);
  check (list int) "take 0" [] (Common.take 0 lst);
  check (list int) "take 10" [1; 2; 3; 4; 5] (Common.take 10 lst);
  check (list int) "take from empty" [] (Common.take 3 [])

(** Test contains substring *)
let test_contains () =
  check bool "contains hello" true
    (Common.contains ~substring:"hello" "hello world");
  check bool "contains world" true
    (Common.contains ~substring:"world" "hello world");
  check bool "not contains foo" false
    (Common.contains ~substring:"foo" "hello world");
  check bool "empty substring" true
    (Common.contains ~substring:"" "anything")

(** Test assoc_opt *)
let test_assoc_opt () =
  let lst = [("a", 1); ("b", 2); ("c", 3)] in
  check (option int) "found a" (Some 1) (Common.assoc_opt "a" lst);
  check (option int) "found c" (Some 3) (Common.assoc_opt "c" lst);
  check (option int) "not found" None (Common.assoc_opt "z" lst)

(** Test json_escape *)
let test_json_escape () =
  check string "no escape needed" "hello" (Common.json_escape "hello");
  check string "escape quotes" "say \\\"hi\\\"" (Common.json_escape "say \"hi\"");
  check string "escape newline" "line1\\nline2" (Common.json_escape "line1\nline2");
  check string "escape backslash" "path\\\\to" (Common.json_escape "path\\to");
  check string "escape tab" "col1\\tcol2" (Common.json_escape "col1\tcol2")

(** Test date formatting *)
let test_date_str () =
  let date = Common.date_str () in
  (* Format: YYYY-MM-DD *)
  check int "length 10" 10 (String.length date);
  check bool "has dashes" true (date.[4] = '-' && date.[7] = '-');
  (* Year should start with 20 *)
  check bool "valid year" true (String.sub date 0 2 = "20")

(** Test ISO timestamp *)
let test_iso_timestamp () =
  let ts = Common.iso_timestamp () in
  (* Format: YYYY-MM-DDTHH:MM:SS *)
  check int "length 19" 19 (String.length ts);
  check bool "has T separator" true (ts.[10] = 'T');
  check bool "has colons" true (ts.[13] = ':' && ts.[16] = ':')

(** Test timestamp is integer *)
let test_timestamp () =
  let ts = Common.timestamp () in
  (* Should be a reasonable Unix timestamp (> 2020-01-01) *)
  check bool "reasonable timestamp" true (ts > 1577836800)

(** Test date_n_days_ago *)
let test_date_n_days_ago () =
  let today = Common.date_str () in
  let zero_days = Common.date_n_days_ago 0 in
  check string "0 days ago is today" today zero_days;
  let yesterday = Common.date_n_days_ago 1 in
  check int "yesterday length" 10 (String.length yesterday)

(** Test read_file_opt on non-existent file *)
let test_read_file_opt_missing () =
  let result = Common.read_file_opt "/nonexistent/path/file.txt" in
  check (option string) "missing file returns None" None result

(** Test read_file_opt and write_file round-trip *)
let test_file_roundtrip () =
  let tmp = Filename.temp_file "test_common_" ".txt" in
  let content = "test content with ascii only" in
  let wrote = Common.write_file tmp content in
  check bool "write succeeded" true wrote;
  let read = Common.read_file_opt tmp in
  check (option string) "read matches written" (Some content) read;
  Sys.remove tmp

let test_protect_finally_runs () =
  let called = ref false in
  let value =
    Common.protect
      ~module_name:"test_common"
      ~finally_label:"finally"
      ~finally:(fun () -> called := true)
      (fun () -> 42)
  in
  check int "value" 42 value;
  check bool "finally called" true !called

let test_protect_finally_error_no_raise () =
  with_env "LLM_MCP_STRICT_FINALIZERS" (Some "0") (fun () ->
    let called = ref false in
    let value =
      Common.protect
        ~module_name:"test_common"
        ~finally_label:"finally"
        ~finally:(fun () -> called := true; failwith "boom")
        (fun () -> 7)
    in
    check int "value" 7 value;
    check bool "finally called" true !called)

let test_protect_finally_error_raise () =
  with_env "LLM_MCP_STRICT_FINALIZERS" (Some "1") (fun () ->
    let raised =
      try
        let _ =
          Common.protect
            ~module_name:"test_common"
            ~finally_label:"finally"
            ~finally:(fun () -> failwith "boom")
            (fun () -> 1)
        in
        false
      with Failure _ -> true
    in
    check bool "raises in strict mode" true raised)

let test_protect_preserves_exception () =
  let raised =
    try
      let _ =
        Common.protect
          ~module_name:"test_common"
          ~finally_label:"finally"
          ~finally:(fun () -> failwith "finalizer")
          (fun () -> failwith "main")
      in
      None
    with Failure msg -> Some msg
  in
  check (option string) "main exception preserved" (Some "main") raised

(** {1 New Coverage Tests} *)

(** Test me_root defaults to ~/me *)
let test_me_root () =
  let root = Common.me_root in
  check bool "me_root is non-empty" true (String.length root > 0)

(** Test ensure_dir creates nested directories *)
let test_ensure_dir () =
  let tmp_base = Filename.get_temp_dir_name () in
  let nested = Filename.concat tmp_base
    (Printf.sprintf "test_ensure_%d/a/b" (Common.timestamp ())) in
  Common.ensure_dir nested;
  check bool "nested dir exists" true (Sys.file_exists nested);
  (* Cleanup *)
  Common.rm_rf (Filename.concat tmp_base
    (Printf.sprintf "test_ensure_%d" (Common.timestamp () )))

(** Test ensure_dir is idempotent *)
let test_ensure_dir_idempotent () =
  let tmp_base = Filename.get_temp_dir_name () in
  let dir = Filename.concat tmp_base
    (Printf.sprintf "test_ensure_idem_%d" (Common.timestamp ())) in
  Common.ensure_dir dir;
  Common.ensure_dir dir;  (* second call should not fail *)
  check bool "dir still exists" true (Sys.file_exists dir);
  Common.rm_rf dir

(** Test rm_rf removes directories recursively *)
let test_rm_rf () =
  let tmp_base = Filename.get_temp_dir_name () in
  let dir = Filename.concat tmp_base
    (Printf.sprintf "test_rmrf_%d" (Common.timestamp ())) in
  Common.ensure_dir (Filename.concat dir "sub");
  let _ = Common.write_file (Filename.concat dir "sub/file.txt") "data" in
  Common.rm_rf dir;
  check bool "dir removed" false (Sys.file_exists dir)

(** Test rm_rf on nonexistent path is a no-op *)
let test_rm_rf_nonexistent () =
  Common.rm_rf "/nonexistent/path/test_rm_rf_ne";
  check bool "no error" true true

(** Test which finds known commands *)
let test_which_known () =
  let result = Common.which "ls" in
  check bool "ls found" true (Option.is_some result)

(** Test which returns None for nonexistent *)
let test_which_nonexistent () =
  let result = Common.which "nonexistent_command_xyz_abc_123" in
  check bool "not found" true (Option.is_none result)

(** Test which with absolute path *)
let test_which_absolute () =
  let result = Common.which "/bin/ls" in
  check bool "absolute ls found" true (Option.is_some result)

(** Test which with non-existent absolute path *)
let test_which_absolute_missing () =
  let result = Common.which "/nonexistent/binary" in
  check bool "absolute not found" true (Option.is_none result)

(** Test command_exists *)
let test_command_exists () =
  check bool "ls exists" true (Common.command_exists "ls");
  check bool "nonexistent" false
    (Common.command_exists "nonexistent_xyz_abc_def")

(** Test env_true *)
let test_env_true () =
  with_env "TEST_ENV_TRUE_VAR" (Some "1") (fun () ->
    check bool "1 is true" true (Common.env_true "TEST_ENV_TRUE_VAR"));
  with_env "TEST_ENV_TRUE_VAR" (Some "true") (fun () ->
    check bool "true is true" true (Common.env_true "TEST_ENV_TRUE_VAR"));
  with_env "TEST_ENV_TRUE_VAR" (Some "yes") (fun () ->
    check bool "yes is true" true (Common.env_true "TEST_ENV_TRUE_VAR"));
  with_env "TEST_ENV_TRUE_VAR" (Some "on") (fun () ->
    check bool "on is true" true (Common.env_true "TEST_ENV_TRUE_VAR"));
  with_env "TEST_ENV_TRUE_VAR" (Some "0") (fun () ->
    check bool "0 is false" false (Common.env_true "TEST_ENV_TRUE_VAR"));
  with_env "TEST_ENV_TRUE_VAR" (Some "false") (fun () ->
    check bool "false is false" false (Common.env_true "TEST_ENV_TRUE_VAR"));
  with_env "TEST_ENV_TRUE_VAR" (Some "") (fun () ->
    check bool "empty is false" false (Common.env_true "TEST_ENV_TRUE_VAR"))

(** Test env_true with unset var *)
let test_env_true_unset () =
  with_env "TEST_ENV_UNSET_XYZ" None (fun () ->
    check bool "unset is false" false (Common.env_true "TEST_ENV_UNSET_XYZ"))

(** Test read_lines_tail *)
let test_read_lines_tail () =
  let tmp = Filename.temp_file "test_tail_" ".txt" in
  let content = String.concat "\n"
    (List.init 100 (fun i -> Printf.sprintf "line%d" i)) ^ "\n" in
  let _ = Common.write_file tmp content in
  let lines = Common.read_lines_tail ~max_bytes:1024 ~max_lines:5 tmp in
  check bool "at most 5 lines" true (List.length lines <= 5);
  check bool "non-empty" true (List.length lines > 0);
  Sys.remove tmp

(** Test read_lines_tail edge cases *)
let test_read_lines_tail_zero () =
  check (list string) "zero max_bytes" [] (Common.read_lines_tail ~max_bytes:0 ~max_lines:5 "/any");
  check (list string) "zero max_lines" [] (Common.read_lines_tail ~max_bytes:1024 ~max_lines:0 "/any")

(** Test read_lines_tail on missing file *)
let test_read_lines_tail_missing () =
  let lines = Common.read_lines_tail ~max_bytes:1024 ~max_lines:10 "/nonexistent.txt" in
  check (list string) "empty" [] lines

(** Test list_files_with_suffix *)
let test_list_files_with_suffix () =
  let tmp_dir = Filename.get_temp_dir_name () in
  let test_dir = Filename.concat tmp_dir
    (Printf.sprintf "test_suffix_%d" (Common.timestamp ())) in
  Common.ensure_dir test_dir;
  let _ = Common.write_file (Filename.concat test_dir "a.txt") "a" in
  let _ = Common.write_file (Filename.concat test_dir "b.txt") "b" in
  let _ = Common.write_file (Filename.concat test_dir "c.ml") "c" in
  let txt_files = Common.list_files_with_suffix test_dir ".txt" in
  check int "2 txt files" 2 (List.length txt_files);
  let ml_files = Common.list_files_with_suffix test_dir ".ml" in
  check int "1 ml file" 1 (List.length ml_files);
  Common.rm_rf test_dir

(** Test list_files_with_suffix on nonexistent dir *)
let test_list_files_with_suffix_missing () =
  let files = Common.list_files_with_suffix "/nonexistent_dir_xyz" ".txt" in
  check (list string) "empty" [] files

(** Test month_dir format *)
let test_month_dir () =
  let md = Common.month_dir () in
  check int "YYYY-MM length" 7 (String.length md);
  check bool "has dash" true (md.[4] = '-')

(** Test day_str format *)
let test_day_str () =
  let d = Common.day_str () in
  check int "DD length" 2 (String.length d);
  let day = int_of_string d in
  check bool "valid day" true (day >= 1 && day <= 31)

(** Test read_json_opt and write_json roundtrip *)
let test_json_file_roundtrip () =
  let tmp = Filename.temp_file "test_json_" ".json" in
  let json = `Assoc [("key", `String "value"); ("num", `Int 42)] in
  let wrote = Common.write_json tmp json in
  check bool "write_json succeeded" true wrote;
  let read = Common.read_json_opt tmp in
  (match read with
   | Some j ->
     let open Yojson.Safe.Util in
     check string "key" "value" (j |> member "key" |> to_string);
     check int "num" 42 (j |> member "num" |> to_int)
   | None -> Alcotest.fail "read_json_opt returned None");
  Sys.remove tmp

(** Test read_json_opt on missing file *)
let test_read_json_opt_missing () =
  let result = Common.read_json_opt "/nonexistent/file.json" in
  check (option (of_pp Yojson.Safe.pp)) "missing returns None" None result

(** Test read_json_opt on invalid JSON *)
let test_read_json_opt_invalid () =
  let tmp = Filename.temp_file "test_badjson_" ".json" in
  let _ = Common.write_file tmp "not valid json {{{" in
  let result = Common.read_json_opt tmp in
  check (option (of_pp Yojson.Safe.pp)) "invalid returns None" None result;
  Sys.remove tmp

(** Test Subprocess.env_with_overrides *)
let test_env_with_overrides () =
  let env = Common.Subprocess.env_with_overrides [("TEST_OVERRIDE_KEY", "value123")] in
  let found = Array.exists (fun s -> s = "TEST_OVERRIDE_KEY=value123") env in
  check bool "override present" true found

(** Test format_result_output with context and warnings *)
let test_format_result_output () =
  let json = `Assoc [
    ("context", `List [`String "ctx1"; `String "ctx2"]);
    ("warnings", `List [`String "warn1"]);
  ] in
  let output = Common.format_result_output json in
  check bool "has Context" true (Common.contains ~substring:"Context" output);
  check bool "has ctx1" true (Common.contains ~substring:"ctx1" output);
  check bool "has Warnings" true (Common.contains ~substring:"Warnings" output);
  check bool "has warn1" true (Common.contains ~substring:"warn1" output)

(** Test format_result_output with empty sections *)
let test_format_result_output_empty () =
  let json = `Assoc [("other", `String "data")] in
  let output = Common.format_result_output json in
  check string "empty output" "" output

(** Test emoji_print and friends just produce output without crashing *)
let test_print_helpers () =
  Common.print_success "test success msg";
  Common.print_error "test error msg";
  Common.print_info "TEST" "test info msg";
  Common.print_warning "test warning msg";
  Common.emoji_print "T" "TAG" "message";
  check bool "no crash" true true

(** Test time_str format *)
let test_time_str () =
  let ts = Common.time_str () in
  (* Format: YYYY-MM-DD HH:MM:SS = 19 chars *)
  check int "length 19" 19 (String.length ts);
  check bool "has space" true (ts.[10] = ' ');
  check bool "has colons" true (ts.[13] = ':' && ts.[16] = ':')

(** Test take negative n *)
let test_take_negative () =
  check (list int) "take -1" [] (Common.take (-1) [1; 2; 3])

(** Test json_escape with carriage return *)
let test_json_escape_cr () =
  check string "escape cr" "a\\rb" (Common.json_escape "a\rb")

let () =
  run "Llm_mcp.Common" [
    "me_path", [
      test_case "builds path" `Quick test_me_path;
    ];
    "me_root", [
      test_case "default value" `Quick test_me_root;
    ];
    "take", [
      test_case "list operations" `Quick test_take;
      test_case "negative n" `Quick test_take_negative;
    ];
    "contains", [
      test_case "substring search" `Quick test_contains;
    ];
    "assoc_opt", [
      test_case "safe assoc" `Quick test_assoc_opt;
    ];
    "json_escape", [
      test_case "escape special chars" `Quick test_json_escape;
      test_case "escape carriage return" `Quick test_json_escape_cr;
    ];
    "date_str", [
      test_case "format" `Quick test_date_str;
    ];
    "time_str", [
      test_case "format" `Quick test_time_str;
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
    "month_dir", [
      test_case "format" `Quick test_month_dir;
    ];
    "day_str", [
      test_case "format" `Quick test_day_str;
    ];
    "ensure_dir", [
      test_case "creates nested dirs" `Quick test_ensure_dir;
      test_case "idempotent" `Quick test_ensure_dir_idempotent;
    ];
    "rm_rf", [
      test_case "removes recursively" `Quick test_rm_rf;
      test_case "nonexistent no-op" `Quick test_rm_rf_nonexistent;
    ];
    "which", [
      test_case "finds known command" `Quick test_which_known;
      test_case "returns None for nonexistent" `Quick test_which_nonexistent;
      test_case "absolute path found" `Quick test_which_absolute;
      test_case "absolute path missing" `Quick test_which_absolute_missing;
    ];
    "command_exists", [
      test_case "ls exists, fake does not" `Quick test_command_exists;
    ];
    "env_true", [
      test_case "various true/false values" `Quick test_env_true;
      test_case "unset is false" `Quick test_env_true_unset;
    ];
    "file_io", [
      test_case "missing file" `Quick test_read_file_opt_missing;
      test_case "roundtrip" `Quick test_file_roundtrip;
    ];
    "read_lines_tail", [
      test_case "reads tail" `Quick test_read_lines_tail;
      test_case "zero params" `Quick test_read_lines_tail_zero;
      test_case "missing file" `Quick test_read_lines_tail_missing;
    ];
    "list_files_with_suffix", [
      test_case "filters by suffix" `Quick test_list_files_with_suffix;
      test_case "missing dir" `Quick test_list_files_with_suffix_missing;
    ];
    "json_file_io", [
      test_case "json roundtrip" `Quick test_json_file_roundtrip;
      test_case "json missing" `Quick test_read_json_opt_missing;
      test_case "json invalid" `Quick test_read_json_opt_invalid;
    ];
    "subprocess", [
      test_case "env_with_overrides" `Quick test_env_with_overrides;
    ];
    "format_result_output", [
      test_case "with context and warnings" `Quick test_format_result_output;
      test_case "empty sections" `Quick test_format_result_output_empty;
    ];
    "print_helpers", [
      test_case "no crash" `Quick test_print_helpers;
    ];
    "finalizer_guard", [
      test_case "runs finally" `Quick test_protect_finally_runs;
      test_case "finalizer error no raise" `Quick test_protect_finally_error_no_raise;
      test_case "finalizer error raise" `Quick test_protect_finally_error_raise;
      test_case "preserve main exception" `Quick test_protect_preserves_exception;
    ];
  ]

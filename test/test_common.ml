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
    "finalizer_guard", [
      test_case "runs finally" `Quick test_protect_finally_runs;
      test_case "finalizer error no raise" `Quick test_protect_finally_error_no_raise;
      test_case "finalizer error raise" `Quick test_protect_finally_error_raise;
      test_case "preserve main exception" `Quick test_protect_preserves_exception;
    ];
  ]

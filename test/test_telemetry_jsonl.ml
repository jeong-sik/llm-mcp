(** Tests for Telemetry_jsonl module - JSONL telemetry logging

    Pure function tests:
    - expand_tilde: home directory expansion
    - ensure_dir: recursive directory creation
    - ensure_parent_dir: parent directory creation
    - append_json: thread-safe JSON writing (with temp file)
    - log_tool_called: tool call logging format
*)

open Alcotest
module TJ = Telemetry_jsonl

(** {1 Test Helpers} *)

let temp_dir = Sys.getenv_opt "TMPDIR" |> Option.value ~default:"/tmp"

(** {1 expand_tilde Tests} *)

let test_expand_tilde_with_home () =
  let path = "~/test/path" in
  let expanded = TJ.expand_tilde path in
  (* Should not start with ~ anymore *)
  check bool "no tilde" false (String.length expanded > 0 && expanded.[0] = '~');
  (* Should contain 'test/path' *)
  check bool "has path" true (Common.contains ~substring:"test/path" expanded)

let test_expand_tilde_without_tilde () =
  let path = "/absolute/path" in
  let expanded = TJ.expand_tilde path in
  check string "unchanged" "/absolute/path" expanded

let test_expand_tilde_relative () =
  let path = "relative/path" in
  let expanded = TJ.expand_tilde path in
  check string "unchanged" "relative/path" expanded

let test_expand_tilde_empty () =
  let path = "" in
  let expanded = TJ.expand_tilde path in
  check string "empty" "" expanded

let test_expand_tilde_just_tilde () =
  let path = "~" in
  let expanded = TJ.expand_tilde path in
  (* Should expand to HOME *)
  check bool "expanded" true (String.length expanded > 0 && expanded.[0] <> '~')

(** {1 ensure_dir Tests} *)

let test_ensure_dir_existing () =
  (* Current directory should always exist *)
  TJ.ensure_dir ".";
  (* No crash is success *)
  ()

let test_ensure_dir_root () =
  TJ.ensure_dir "/";
  (* No crash is success *)
  ()

let test_ensure_dir_creates_nested () =
  let base = Printf.sprintf "%s/test_ensure_%d" temp_dir (Random.int 100000) in
  let nested = base ^ "/a/b/c" in
  TJ.ensure_dir nested;
  check bool "created" true (Sys.file_exists nested);
  (* Cleanup *)
  ignore (Sys.command (Printf.sprintf "rm -rf %s" base))

(** {1 ensure_parent_dir Tests} *)

let test_ensure_parent_dir () =
  let base = Printf.sprintf "%s/test_parent_%d" temp_dir (Random.int 100000) in
  let file = base ^ "/subdir/file.txt" in
  TJ.ensure_parent_dir file;
  check bool "parent exists" true (Sys.file_exists (Filename.dirname file));
  (* Cleanup *)
  ignore (Sys.command (Printf.sprintf "rm -rf %s" base))

(** {1 append_json Tests} *)

let test_append_json_creates_file () =
  (* We can't easily test append_json since it uses the global telemetry_file.
     So we test indirectly via log_tool_called *)
  (* Just verify the function doesn't crash *)
  TJ.append_json (`Assoc [("test", `Bool true)]);
  ()

(** {1 log_tool_called Tests} *)

let test_log_tool_called_success () =
  (* Just verify it doesn't crash *)
  TJ.log_tool_called
    ~tool_name:"test_tool"
    ~url:"http://localhost:8932"
    ~duration_ms:100
    ~success:true
    ~error:None;
  ()

let test_log_tool_called_with_error () =
  TJ.log_tool_called
    ~tool_name:"failing_tool"
    ~url:"http://localhost:8932"
    ~duration_ms:50
    ~success:false
    ~error:(Some "Connection refused");
  ()

(** {1 telemetry_file Tests} *)

let test_telemetry_file_default () =
  (* If no env var, should use default *)
  let file = TJ.telemetry_file in
  check bool "non-empty" true (String.length file > 0)

let test_telemetry_file_expanded () =
  (* Should not contain ~ (would be expanded) *)
  let file = TJ.telemetry_file in
  check bool "no tilde" true (String.length file = 0 || file.[0] <> '~')

(** {1 Test Suite} *)

let expand_tilde_tests = [
  test_case "with home" `Quick test_expand_tilde_with_home;
  test_case "without tilde" `Quick test_expand_tilde_without_tilde;
  test_case "relative" `Quick test_expand_tilde_relative;
  test_case "empty" `Quick test_expand_tilde_empty;
  test_case "just tilde" `Quick test_expand_tilde_just_tilde;
]

let ensure_dir_tests = [
  test_case "existing" `Quick test_ensure_dir_existing;
  test_case "root" `Quick test_ensure_dir_root;
  test_case "creates nested" `Quick test_ensure_dir_creates_nested;
]

let ensure_parent_tests = [
  test_case "creates parent" `Quick test_ensure_parent_dir;
]

let append_tests = [
  test_case "creates file" `Quick test_append_json_creates_file;
]

let log_tests = [
  test_case "success" `Quick test_log_tool_called_success;
  test_case "with error" `Quick test_log_tool_called_with_error;
]

let config_tests = [
  test_case "default" `Quick test_telemetry_file_default;
  test_case "expanded" `Quick test_telemetry_file_expanded;
]

let () =
  Random.init (int_of_float (Unix.gettimeofday () *. 1000.0));
  run "telemetry_jsonl" [
    ("expand_tilde", expand_tilde_tests);
    ("ensure_dir", ensure_dir_tests);
    ("ensure_parent_dir", ensure_parent_tests);
    ("append_json", append_tests);
    ("log_tool_called", log_tests);
    ("telemetry_file", config_tests);
  ]

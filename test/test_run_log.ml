(** Tests for Run_log module - LLM run logging *)

open Alcotest
open Lwt.Syntax

(** Helper to run Lwt tests *)
let lwt_test f () = Lwt_main.run (f ())

(** Test log_path with default *)
let test_log_path_default () =
  (* Temporarily unset env var if set *)
  let original = Sys.getenv_opt "LLM_MCP_RUN_LOG_PATH" in
  Unix.putenv "LLM_MCP_RUN_LOG_PATH" "";
  let path = Llm_mcp.Run_log.log_path () in
  check bool "ends with jsonl" true
    (Filename.check_suffix path ".jsonl");
  check bool "contains llm_mcp" true
    (Llm_mcp.Common.contains ~substring:"llm_mcp" path);
  (* Restore *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" v
   | None -> ())

(** Test log_path with custom env var *)
let test_log_path_custom () =
  let original = Sys.getenv_opt "LLM_MCP_RUN_LOG_PATH" in
  Unix.putenv "LLM_MCP_RUN_LOG_PATH" "/custom/path/log.jsonl";
  let path = Llm_mcp.Run_log.log_path () in
  check string "custom path" "/custom/path/log.jsonl" path;
  (* Restore *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" v
   | None -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" "")

(** Test default_log_path *)
let test_default_log_path () =
  let path = Llm_mcp.Run_log.default_log_path () in
  check bool "has logs dir" true
    (Llm_mcp.Common.contains ~substring:"logs" path);
  check bool "has llm_mcp_runs" true
    (Llm_mcp.Common.contains ~substring:"llm_mcp_runs" path)

(** Test int_field helper *)
let test_int_field () =
  let json = `Assoc [("count", `Int 42); ("name", `String "test")] in
  check int "found int" 42
    (Llm_mcp.Run_log.int_field json "count" ~default:0);
  check int "default for missing" 99
    (Llm_mcp.Run_log.int_field json "missing" ~default:99);
  check int "default for wrong type" 99
    (Llm_mcp.Run_log.int_field json "name" ~default:99)

(** Test string_field helper *)
let test_string_field () =
  let json = `Assoc [("name", `String "hello"); ("count", `Int 42)] in
  check string "found string" "hello"
    (Llm_mcp.Run_log.string_field json "name" ~default:"");
  check string "default for missing" "fallback"
    (Llm_mcp.Run_log.string_field json "missing" ~default:"fallback");
  check string "default for wrong type" "fallback"
    (Llm_mcp.Run_log.string_field json "count" ~default:"fallback")

(** Test take_last helper *)
let test_take_last () =
  let lst = [1; 2; 3; 4; 5] in
  check (list int) "take last 3" [3; 4; 5]
    (Llm_mcp.Run_log.take_last 3 lst);
  check (list int) "take last 0" []
    (Llm_mcp.Run_log.take_last 0 lst);
  check (list int) "take last 10" [1; 2; 3; 4; 5]
    (Llm_mcp.Run_log.take_last 10 lst);
  check (list int) "take from empty" []
    (Llm_mcp.Run_log.take_last 3 [])

(** Test read_events on non-existent file *)
let test_read_events_missing () =
  let original = Sys.getenv_opt "LLM_MCP_RUN_LOG_PATH" in
  Unix.putenv "LLM_MCP_RUN_LOG_PATH" "/nonexistent/path/log.jsonl";
  let events = Llm_mcp.Run_log.read_events () in
  check (list (of_pp Yojson.Safe.pp)) "empty list" [] events;
  (* Restore *)
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" v
   | None -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" "")

(** Test record and read_recent roundtrip *)
let test_record_roundtrip () =
  let tmp = Filename.temp_file "test_run_log_" ".jsonl" in
  let original = Sys.getenv_opt "LLM_MCP_RUN_LOG_PATH" in
  Unix.putenv "LLM_MCP_RUN_LOG_PATH" tmp;
  let result : Llm_mcp.Types.tool_result = {
    response = "test response";
    returncode = 0;
    model = "test-model";
    extra = [("key", "value")];
  } in
  let* () = Llm_mcp.Run_log.record
    ~tool:"test-tool"
    ~streamed:false
    ~prompt_chars:100
    ~duration_ms:500
    result
  in
  let events = Llm_mcp.Run_log.read_events () in
  check bool "has one event" true (List.length events >= 1);
  let last = List.hd (List.rev events) in
  check string "tool name" "test-tool"
    (Llm_mcp.Run_log.string_field last "tool" ~default:"");
  check string "model name" "test-model"
    (Llm_mcp.Run_log.string_field last "model" ~default:"");
  check int "returncode" 0
    (Llm_mcp.Run_log.int_field last "returncode" ~default:(-1));
  check int "duration_ms" 500
    (Llm_mcp.Run_log.int_field last "duration_ms" ~default:0);
  (* Cleanup *)
  Sys.remove tmp;
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" v
   | None -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" "");
  Lwt.return_unit

(** Test stats function *)
let test_stats () =
  let tmp = Filename.temp_file "test_run_log_stats_" ".jsonl" in
  let original = Sys.getenv_opt "LLM_MCP_RUN_LOG_PATH" in
  Unix.putenv "LLM_MCP_RUN_LOG_PATH" tmp;
  (* Write a test event directly *)
  let ts = int_of_float (Unix.time ()) in
  let line = Printf.sprintf {|{"ts":%d,"tool":"gemini","returncode":0,"duration_ms":100}|} ts in
  let oc = open_out tmp in
  output_string oc (line ^ "\n");
  close_out oc;
  let stats = Llm_mcp.Run_log.stats ~since_ts:(ts - 10) ~until_ts:0 in
  let open Yojson.Safe.Util in
  check int "total 1" 1 (stats |> member "total" |> to_int);
  check int "success 1" 1 (stats |> member "success" |> to_int);
  (* Cleanup *)
  Sys.remove tmp;
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" v
   | None -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" "")

(** Test read_recent with time filter *)
let test_read_recent () =
  let tmp = Filename.temp_file "test_run_log_recent_" ".jsonl" in
  let original = Sys.getenv_opt "LLM_MCP_RUN_LOG_PATH" in
  Unix.putenv "LLM_MCP_RUN_LOG_PATH" tmp;
  let ts = int_of_float (Unix.time ()) in
  (* Write events with different timestamps *)
  let oc = open_out tmp in
  output_string oc (Printf.sprintf {|{"ts":%d,"tool":"old"}|} (ts - 1000) ^ "\n");
  output_string oc (Printf.sprintf {|{"ts":%d,"tool":"new"}|} ts ^ "\n");
  close_out oc;
  let recent = Llm_mcp.Run_log.read_recent ~since_ts:(ts - 10) ~limit:10 in
  check int "one recent event" 1 (List.length recent);
  (* Cleanup *)
  Sys.remove tmp;
  (match original with
   | Some v -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" v
   | None -> Unix.putenv "LLM_MCP_RUN_LOG_PATH" "")

let () =
  run "Llm_mcp.Run_log" [
    "log_path", [
      test_case "default" `Quick test_log_path_default;
      test_case "custom env" `Quick test_log_path_custom;
      test_case "default_log_path" `Quick test_default_log_path;
    ];
    "helpers", [
      test_case "int_field" `Quick test_int_field;
      test_case "string_field" `Quick test_string_field;
      test_case "take_last" `Quick test_take_last;
    ];
    "read_events", [
      test_case "missing file" `Quick test_read_events_missing;
    ];
    "record", [
      test_case "roundtrip" `Quick (lwt_test test_record_roundtrip);
    ];
    "stats", [
      test_case "aggregation" `Quick test_stats;
    ];
    "read_recent", [
      test_case "time filter" `Quick test_read_recent;
    ];
  ]

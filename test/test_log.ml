(** Tests for Log module - Structured logging

    Pure function tests:
    - level_to_int: level ordering
    - level_to_string: level formatting
    - level_of_string: level parsing with aliases
    - timestamp: format validation
    - level_emoji: ANSI color codes
    - config operations: set_level, set_format
*)

open Alcotest
open Log

(** {1 level_to_int Tests} *)

let test_level_to_int_debug () =
  check int "debug" 0 (level_to_int Debug)

let test_level_to_int_info () =
  check int "info" 1 (level_to_int Info)

let test_level_to_int_warn () =
  check int "warn" 2 (level_to_int Warn)

let test_level_to_int_error () =
  check int "error" 3 (level_to_int Error)

let test_level_to_int_critical () =
  check int "critical" 4 (level_to_int Critical)

let test_level_to_int_ordering () =
  (* Verify ordering: Debug < Info < Warn < Error < Critical *)
  check bool "debug < info" true (level_to_int Debug < level_to_int Info);
  check bool "info < warn" true (level_to_int Info < level_to_int Warn);
  check bool "warn < error" true (level_to_int Warn < level_to_int Error);
  check bool "error < critical" true (level_to_int Error < level_to_int Critical)

(** {1 level_to_string Tests} *)

let test_level_to_string_debug () =
  check string "debug" "DEBUG" (level_to_string Debug)

let test_level_to_string_info () =
  check string "info" "INFO" (level_to_string Info)

let test_level_to_string_warn () =
  check string "warn" "WARN" (level_to_string Warn)

let test_level_to_string_error () =
  check string "error" "ERROR" (level_to_string Error)

let test_level_to_string_critical () =
  check string "critical" "CRITICAL" (level_to_string Critical)

let test_level_to_string_uppercase () =
  (* All level strings should be uppercase *)
  let levels = [Debug; Info; Warn; Error; Critical] in
  List.iter (fun level ->
    let s = level_to_string level in
    check string "uppercase" s (String.uppercase_ascii s)
  ) levels

(** {1 level_of_string Tests} *)

let test_level_of_string_debug () =
  check int "debug" 0 (level_to_int (level_of_string "debug"))

let test_level_of_string_info () =
  check int "info" 1 (level_to_int (level_of_string "info"))

let test_level_of_string_warn () =
  check int "warn" 2 (level_to_int (level_of_string "warn"))

let test_level_of_string_warning_alias () =
  check int "warning" 2 (level_to_int (level_of_string "warning"))

let test_level_of_string_error () =
  check int "error" 3 (level_to_int (level_of_string "error"))

let test_level_of_string_critical () =
  check int "critical" 4 (level_to_int (level_of_string "critical"))

let test_level_of_string_fatal_alias () =
  check int "fatal" 4 (level_to_int (level_of_string "fatal"))

let test_level_of_string_unknown_defaults_to_info () =
  check int "unknown" 1 (level_to_int (level_of_string "unknown"));
  check int "garbage" 1 (level_to_int (level_of_string "garbage"));
  check int "empty" 1 (level_to_int (level_of_string ""))

(** {1 timestamp Tests} *)

let test_timestamp_format () =
  let ts = timestamp () in
  (* Format: YYYY-MM-DD HH:MM:SS *)
  check int "length" 19 (String.length ts);
  check char "dash at 4" '-' (String.get ts 4);
  check char "dash at 7" '-' (String.get ts 7);
  check char "space at 10" ' ' (String.get ts 10);
  check char "colon at 13" ':' (String.get ts 13);
  check char "colon at 16" ':' (String.get ts 16)

let test_timestamp_year_reasonable () =
  let ts = timestamp () in
  let year = int_of_string (String.sub ts 0 4) in
  check bool "year >= 2024" true (year >= 2024);
  check bool "year <= 2100" true (year <= 2100)

let test_timestamp_month_valid () =
  let ts = timestamp () in
  let month = int_of_string (String.sub ts 5 2) in
  check bool "month >= 1" true (month >= 1);
  check bool "month <= 12" true (month <= 12)

let test_timestamp_day_valid () =
  let ts = timestamp () in
  let day = int_of_string (String.sub ts 8 2) in
  check bool "day >= 1" true (day >= 1);
  check bool "day <= 31" true (day <= 31)

let test_timestamp_hour_valid () =
  let ts = timestamp () in
  let hour = int_of_string (String.sub ts 11 2) in
  check bool "hour >= 0" true (hour >= 0);
  check bool "hour <= 23" true (hour <= 23)

let test_timestamp_minute_valid () =
  let ts = timestamp () in
  let minute = int_of_string (String.sub ts 14 2) in
  check bool "minute >= 0" true (minute >= 0);
  check bool "minute <= 59" true (minute <= 59)

let test_timestamp_second_valid () =
  let ts = timestamp () in
  let second = int_of_string (String.sub ts 17 2) in
  check bool "second >= 0" true (second >= 0);
  check bool "second <= 59" true (second <= 59)

(** {1 level_emoji Tests} *)

let test_level_emoji_debug_cyan () =
  let emoji = level_emoji Debug in
  check bool "has [D]" true (Common.contains ~substring:"[D]" emoji);
  check bool "has ANSI" true (Common.contains ~substring:"\027[" emoji)

let test_level_emoji_info_green () =
  let emoji = level_emoji Info in
  check bool "has [I]" true (Common.contains ~substring:"[I]" emoji);
  check bool "has 32m (green)" true (Common.contains ~substring:"32m" emoji)

let test_level_emoji_warn_yellow () =
  let emoji = level_emoji Warn in
  check bool "has [W]" true (Common.contains ~substring:"[W]" emoji);
  check bool "has 33m (yellow)" true (Common.contains ~substring:"33m" emoji)

let test_level_emoji_error_red () =
  let emoji = level_emoji Error in
  check bool "has [E]" true (Common.contains ~substring:"[E]" emoji);
  check bool "has 31m (red)" true (Common.contains ~substring:"31m" emoji)

let test_level_emoji_critical_magenta () =
  let emoji = level_emoji Critical in
  check bool "has [!]" true (Common.contains ~substring:"[!]" emoji);
  check bool "has 35m (magenta)" true (Common.contains ~substring:"35m" emoji)

let test_level_emoji_reset_codes () =
  (* All emojis should end with reset code *)
  let levels = [Debug; Info; Warn; Error; Critical] in
  List.iter (fun level ->
    let emoji = level_emoji level in
    check bool "ends with reset" true (Common.contains ~substring:"\027[0m" emoji)
  ) levels

(** {1 Config Tests} *)

let test_set_level () =
  let original = config.min_level in
  set_level Debug;
  check int "set to debug" 0 (level_to_int config.min_level);
  set_level Error;
  check int "set to error" 3 (level_to_int config.min_level);
  set_level original  (* Restore *)

let test_set_format_text () =
  let original = config.format in
  set_format Text;
  check bool "text format" true (config.format = Text);
  set_format original

let test_set_format_json () =
  let original = config.format in
  set_format Json;
  check bool "json format" true (config.format = Json);
  set_format original

(** {1 Scoped Logger Tests (Make Functor)} *)

module TestLogger = Make(struct let name = "test_module" end)

let test_scoped_logger_exists () =
  (* Just verify the functor creates a valid module *)
  let _ = TestLogger.debug in
  let _ = TestLogger.info in
  let _ = TestLogger.warn in
  let _ = TestLogger.error in
  let _ = TestLogger.critical in
  ()

(** {1 Test Suite} *)

let level_to_int_tests = [
  test_case "debug" `Quick test_level_to_int_debug;
  test_case "info" `Quick test_level_to_int_info;
  test_case "warn" `Quick test_level_to_int_warn;
  test_case "error" `Quick test_level_to_int_error;
  test_case "critical" `Quick test_level_to_int_critical;
  test_case "ordering" `Quick test_level_to_int_ordering;
]

let level_to_string_tests = [
  test_case "debug" `Quick test_level_to_string_debug;
  test_case "info" `Quick test_level_to_string_info;
  test_case "warn" `Quick test_level_to_string_warn;
  test_case "error" `Quick test_level_to_string_error;
  test_case "critical" `Quick test_level_to_string_critical;
  test_case "uppercase" `Quick test_level_to_string_uppercase;
]

let level_of_string_tests = [
  test_case "debug" `Quick test_level_of_string_debug;
  test_case "info" `Quick test_level_of_string_info;
  test_case "warn" `Quick test_level_of_string_warn;
  test_case "warning alias" `Quick test_level_of_string_warning_alias;
  test_case "error" `Quick test_level_of_string_error;
  test_case "critical" `Quick test_level_of_string_critical;
  test_case "fatal alias" `Quick test_level_of_string_fatal_alias;
  test_case "unknown defaults" `Quick test_level_of_string_unknown_defaults_to_info;
]

let timestamp_tests = [
  test_case "format" `Quick test_timestamp_format;
  test_case "year" `Quick test_timestamp_year_reasonable;
  test_case "month" `Quick test_timestamp_month_valid;
  test_case "day" `Quick test_timestamp_day_valid;
  test_case "hour" `Quick test_timestamp_hour_valid;
  test_case "minute" `Quick test_timestamp_minute_valid;
  test_case "second" `Quick test_timestamp_second_valid;
]

let emoji_tests = [
  test_case "debug cyan" `Quick test_level_emoji_debug_cyan;
  test_case "info green" `Quick test_level_emoji_info_green;
  test_case "warn yellow" `Quick test_level_emoji_warn_yellow;
  test_case "error red" `Quick test_level_emoji_error_red;
  test_case "critical magenta" `Quick test_level_emoji_critical_magenta;
  test_case "reset codes" `Quick test_level_emoji_reset_codes;
]

let config_tests = [
  test_case "set_level" `Quick test_set_level;
  test_case "set_format text" `Quick test_set_format_text;
  test_case "set_format json" `Quick test_set_format_json;
]

let scoped_tests = [
  test_case "functor" `Quick test_scoped_logger_exists;
]

let () =
  run "log" [
    ("level_to_int", level_to_int_tests);
    ("level_to_string", level_to_string_tests);
    ("level_of_string", level_of_string_tests);
    ("timestamp", timestamp_tests);
    ("level_emoji", emoji_tests);
    ("config", config_tests);
    ("scoped", scoped_tests);
  ]

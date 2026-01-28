(** Tests for Version module - Version constant

    Simple validation tests:
    - version format (semver-like)
    - version non-empty
*)

open Alcotest

(** {1 Version Tests} *)

let test_version_non_empty () =
  check bool "non-empty" true (String.length Version.version > 0)

let test_version_semver_format () =
  let version = Version.version in
  (* Should contain at least one dot (major.minor) *)
  check bool "has dot" true (String.contains version '.')

let test_version_starts_with_digit () =
  let version = Version.version in
  let first_char = String.get version 0 in
  check bool "starts with digit" true (first_char >= '0' && first_char <= '9')

let test_version_no_v_prefix () =
  let version = Version.version in
  let first_char = String.get version 0 in
  check bool "no v prefix" true (first_char <> 'v' && first_char <> 'V')

let test_version_reasonable_length () =
  let version = Version.version in
  (* Version should be between 3 and 20 characters (e.g., "0.1" to "10.20.30-beta.123") *)
  check bool "length >= 3" true (String.length version >= 3);
  check bool "length <= 20" true (String.length version <= 20)

(** {1 Test Suite} *)

let version_tests = [
  test_case "non-empty" `Quick test_version_non_empty;
  test_case "semver format" `Quick test_version_semver_format;
  test_case "starts with digit" `Quick test_version_starts_with_digit;
  test_case "no v prefix" `Quick test_version_no_v_prefix;
  test_case "reasonable length" `Quick test_version_reasonable_length;
]

let () =
  run "version" [
    ("version", version_tests);
  ]

(** Tests for Chain_utils module - Safe helper functions

    Coverage targets:
    - list_nth_opt: negative index, empty list, valid index, out of bounds
    - list_hd_opt: empty, non-empty
    - list_tl_safe: empty, non-empty
    - list_uncons: empty, non-empty
    - list_last_opt: empty, single, multiple
    - starts_with / ends_with: match, no match, empty prefix/suffix, exact match
    - string_sub_opt: valid, out of bounds (negative start, negative len, overflow)
    - truncate_with_ellipsis: short, exact, long, custom max_len
    - strip_prefix / strip_suffix: present, absent, empty, full match
    - max_empty_retries: value check
    - is_empty_response: empty, whitespace, non-empty
    - empty_retry_suffix: non-empty string
    - is_complex_prompt: short, long, code block, steps
    - is_glm_model: exact "glm", prefix "glm-4", non-glm
    - string_contains: found, not found, empty substring
*)

open Alcotest
open Chain_utils

(* ── List helpers ───────────────────────────────────────── *)

let test_list_nth_opt_negative () =
  check (option int) "negative index" None (list_nth_opt [1;2;3] (-1))

let test_list_nth_opt_empty () =
  check (option int) "empty list" None (list_nth_opt [] 0)

let test_list_nth_opt_valid () =
  check (option int) "index 0" (Some 10) (list_nth_opt [10;20;30] 0);
  check (option int) "index 1" (Some 20) (list_nth_opt [10;20;30] 1);
  check (option int) "index 2" (Some 30) (list_nth_opt [10;20;30] 2)

let test_list_nth_opt_oob () =
  check (option int) "out of bounds" None (list_nth_opt [1;2] 5)

let test_list_hd_opt_empty () =
  check (option int) "empty" None (list_hd_opt [])

let test_list_hd_opt_nonempty () =
  check (option int) "head" (Some 42) (list_hd_opt [42;99])

let test_list_tl_safe_empty () =
  check (list int) "empty" [] (list_tl_safe [])

let test_list_tl_safe_nonempty () =
  check (list int) "tail" [2;3] (list_tl_safe [1;2;3])

let test_list_uncons_empty () =
  check (option (pair int (list int))) "empty" None (list_uncons [])

let test_list_uncons_nonempty () =
  check (option (pair int (list int))) "uncons" (Some (1, [2;3])) (list_uncons [1;2;3])

let test_list_uncons_single () =
  check (option (pair int (list int))) "single" (Some (5, [])) (list_uncons [5])

let test_list_last_opt_empty () =
  check (option int) "empty" None (list_last_opt [])

let test_list_last_opt_single () =
  check (option int) "single" (Some 7) (list_last_opt [7])

let test_list_last_opt_many () =
  check (option int) "last" (Some 99) (list_last_opt [1;2;3;99])

(* ── String helpers ─────────────────────────────────────── *)

let test_starts_with_match () =
  check bool "match" true (starts_with ~prefix:"hel" "hello")

let test_starts_with_no_match () =
  check bool "no match" false (starts_with ~prefix:"xyz" "hello")

let test_starts_with_empty_prefix () =
  check bool "empty prefix" true (starts_with ~prefix:"" "hello")

let test_starts_with_exact () =
  check bool "exact" true (starts_with ~prefix:"hello" "hello")

let test_starts_with_longer_prefix () =
  check bool "prefix longer than string" false (starts_with ~prefix:"hello world" "hello")

let test_ends_with_match () =
  check bool "match" true (ends_with ~suffix:"llo" "hello")

let test_ends_with_no_match () =
  check bool "no match" false (ends_with ~suffix:"xyz" "hello")

let test_ends_with_empty_suffix () =
  check bool "empty suffix" true (ends_with ~suffix:"" "hello")

let test_ends_with_exact () =
  check bool "exact" true (ends_with ~suffix:"hello" "hello")

let test_ends_with_longer_suffix () =
  check bool "suffix longer" false (ends_with ~suffix:"hello world" "hello")

let test_string_sub_opt_valid () =
  check (option string) "valid" (Some "ell") (string_sub_opt "hello" 1 3)

let test_string_sub_opt_negative_start () =
  check (option string) "negative start" None (string_sub_opt "hello" (-1) 2)

let test_string_sub_opt_negative_len () =
  check (option string) "negative len" None (string_sub_opt "hello" 0 (-1))

let test_string_sub_opt_overflow () =
  check (option string) "overflow" None (string_sub_opt "hello" 3 5)

let test_string_sub_opt_zero_len () =
  check (option string) "zero len" (Some "") (string_sub_opt "hello" 2 0)

let test_string_sub_opt_full () =
  check (option string) "full string" (Some "hello") (string_sub_opt "hello" 0 5)

let test_truncate_short () =
  check string "short" "hi" (truncate_with_ellipsis "hi")

let test_truncate_exact () =
  let s = String.make 160 'a' in
  check string "exact 160" s (truncate_with_ellipsis s)

let test_truncate_long () =
  let s = String.make 200 'b' in
  let result = truncate_with_ellipsis s in
  check int "truncated length" 163 (String.length result);
  check bool "ends with ellipsis" true (ends_with ~suffix:"..." result)

let test_truncate_custom_max () =
  check string "custom" "hel..." (truncate_with_ellipsis ~max_len:3 "hello world")

let test_strip_prefix_present () =
  check string "stripped" "world" (strip_prefix ~prefix:"hello " "hello world")

let test_strip_prefix_absent () =
  check string "unchanged" "hello" (strip_prefix ~prefix:"xyz" "hello")

let test_strip_prefix_empty () =
  check string "empty prefix" "hello" (strip_prefix ~prefix:"" "hello")

let test_strip_prefix_full_match () =
  check string "full" "" (strip_prefix ~prefix:"hello" "hello")

let test_strip_suffix_present () =
  check string "stripped" "hello" (strip_suffix ~suffix:" world" "hello world")

let test_strip_suffix_absent () =
  check string "unchanged" "hello" (strip_suffix ~suffix:"xyz" "hello")

let test_strip_suffix_empty () =
  check string "empty suffix" "hello" (strip_suffix ~suffix:"" "hello")

let test_strip_suffix_full_match () =
  check string "full" "" (strip_suffix ~suffix:"hello" "hello")

(* ── Empty response ─────────────────────────────────────── *)

let test_max_empty_retries () =
  check bool "positive" true (max_empty_retries > 0)

let test_is_empty_response_empty () =
  check bool "empty" true (is_empty_response "")

let test_is_empty_response_whitespace () =
  check bool "spaces" true (is_empty_response "   ");
  check bool "newlines" true (is_empty_response "\n\t  \n")

let test_is_empty_response_content () =
  check bool "content" false (is_empty_response "hello")

let test_is_empty_response_padded () =
  check bool "padded" false (is_empty_response "  hello  ")

let test_empty_retry_suffix_nonempty () =
  check bool "non-empty" true (String.length empty_retry_suffix > 0)

(* ── Prompt analysis ────────────────────────────────────── *)

let test_is_complex_prompt_short () =
  check bool "short simple" false (is_complex_prompt "hello")

let test_is_complex_prompt_long () =
  let long = String.make 501 'a' in
  check bool "long" true (is_complex_prompt long)

let test_is_complex_prompt_code_backtick () =
  check bool "backtick" true (is_complex_prompt "run `ls -la` please")

let test_is_complex_prompt_code_block () =
  check bool "code block" true (is_complex_prompt "here is code:\n```python\nprint(1)\n```")

let test_is_complex_prompt_steps () =
  check bool "step keyword" true (is_complex_prompt "step 1: do something");
  check bool "numbered" true (is_complex_prompt "1. first thing 2. second");
  check bool "then keyword" true (is_complex_prompt "first do this then do that");
  check bool "finally keyword" true (is_complex_prompt "finally wrap up the task")

(* ── GLM model detection ───────────────────────────────── *)

let test_is_glm_model_exact () =
  check bool "glm" true (is_glm_model "glm")

let test_is_glm_model_prefix () =
  check bool "glm-4" true (is_glm_model "glm-4-flash");
  check bool "GLM uppercase" true (is_glm_model "GLM-4.7")

let test_is_glm_model_not () =
  check bool "gemini" false (is_glm_model "gemini");
  check bool "claude" false (is_glm_model "claude")

let test_is_glm_model_short () =
  check bool "gl (too short)" false (is_glm_model "gl")

let test_is_glm_model_empty () =
  check bool "empty" false (is_glm_model "")

(* ── string_contains ───────────────────────────────────── *)

let test_string_contains_found () =
  check bool "found" true (string_contains ~substring:"ell" "hello")

let test_string_contains_not_found () =
  check bool "not found" false (string_contains ~substring:"xyz" "hello")

let test_string_contains_empty_sub () =
  check bool "empty substring" true (string_contains ~substring:"" "hello")

let test_string_contains_full () =
  check bool "full match" true (string_contains ~substring:"hello" "hello")

let test_string_contains_at_end () =
  check bool "at end" true (string_contains ~substring:"world" "hello world")

(* ── Test suite ─────────────────────────────────────────── *)

let () =
  run "chain_utils" [
    ("list_nth_opt", [
      test_case "negative index" `Quick test_list_nth_opt_negative;
      test_case "empty list" `Quick test_list_nth_opt_empty;
      test_case "valid indices" `Quick test_list_nth_opt_valid;
      test_case "out of bounds" `Quick test_list_nth_opt_oob;
    ]);
    ("list_hd_opt", [
      test_case "empty" `Quick test_list_hd_opt_empty;
      test_case "non-empty" `Quick test_list_hd_opt_nonempty;
    ]);
    ("list_tl_safe", [
      test_case "empty" `Quick test_list_tl_safe_empty;
      test_case "non-empty" `Quick test_list_tl_safe_nonempty;
    ]);
    ("list_uncons", [
      test_case "empty" `Quick test_list_uncons_empty;
      test_case "non-empty" `Quick test_list_uncons_nonempty;
      test_case "single" `Quick test_list_uncons_single;
    ]);
    ("list_last_opt", [
      test_case "empty" `Quick test_list_last_opt_empty;
      test_case "single" `Quick test_list_last_opt_single;
      test_case "multiple" `Quick test_list_last_opt_many;
    ]);
    ("starts_with", [
      test_case "match" `Quick test_starts_with_match;
      test_case "no match" `Quick test_starts_with_no_match;
      test_case "empty prefix" `Quick test_starts_with_empty_prefix;
      test_case "exact" `Quick test_starts_with_exact;
      test_case "longer prefix" `Quick test_starts_with_longer_prefix;
    ]);
    ("ends_with", [
      test_case "match" `Quick test_ends_with_match;
      test_case "no match" `Quick test_ends_with_no_match;
      test_case "empty suffix" `Quick test_ends_with_empty_suffix;
      test_case "exact" `Quick test_ends_with_exact;
      test_case "longer suffix" `Quick test_ends_with_longer_suffix;
    ]);
    ("string_sub_opt", [
      test_case "valid" `Quick test_string_sub_opt_valid;
      test_case "negative start" `Quick test_string_sub_opt_negative_start;
      test_case "negative len" `Quick test_string_sub_opt_negative_len;
      test_case "overflow" `Quick test_string_sub_opt_overflow;
      test_case "zero len" `Quick test_string_sub_opt_zero_len;
      test_case "full string" `Quick test_string_sub_opt_full;
    ]);
    ("truncate_with_ellipsis", [
      test_case "short" `Quick test_truncate_short;
      test_case "exact 160" `Quick test_truncate_exact;
      test_case "long" `Quick test_truncate_long;
      test_case "custom max_len" `Quick test_truncate_custom_max;
    ]);
    ("strip_prefix", [
      test_case "present" `Quick test_strip_prefix_present;
      test_case "absent" `Quick test_strip_prefix_absent;
      test_case "empty" `Quick test_strip_prefix_empty;
      test_case "full match" `Quick test_strip_prefix_full_match;
    ]);
    ("strip_suffix", [
      test_case "present" `Quick test_strip_suffix_present;
      test_case "absent" `Quick test_strip_suffix_absent;
      test_case "empty" `Quick test_strip_suffix_empty;
      test_case "full match" `Quick test_strip_suffix_full_match;
    ]);
    ("empty_response", [
      test_case "max retries positive" `Quick test_max_empty_retries;
      test_case "empty string" `Quick test_is_empty_response_empty;
      test_case "whitespace" `Quick test_is_empty_response_whitespace;
      test_case "content" `Quick test_is_empty_response_content;
      test_case "padded content" `Quick test_is_empty_response_padded;
      test_case "suffix non-empty" `Quick test_empty_retry_suffix_nonempty;
    ]);
    ("is_complex_prompt", [
      test_case "short simple" `Quick test_is_complex_prompt_short;
      test_case "long" `Quick test_is_complex_prompt_long;
      test_case "backtick" `Quick test_is_complex_prompt_code_backtick;
      test_case "code block" `Quick test_is_complex_prompt_code_block;
      test_case "step keywords" `Quick test_is_complex_prompt_steps;
    ]);
    ("is_glm_model", [
      test_case "exact" `Quick test_is_glm_model_exact;
      test_case "prefix" `Quick test_is_glm_model_prefix;
      test_case "not glm" `Quick test_is_glm_model_not;
      test_case "too short" `Quick test_is_glm_model_short;
      test_case "empty" `Quick test_is_glm_model_empty;
    ]);
    ("string_contains", [
      test_case "found" `Quick test_string_contains_found;
      test_case "not found" `Quick test_string_contains_not_found;
      test_case "empty substring" `Quick test_string_contains_empty_sub;
      test_case "full match" `Quick test_string_contains_full;
      test_case "at end" `Quick test_string_contains_at_end;
    ]);
  ]

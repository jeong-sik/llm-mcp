(** Tests for Fewshot_injector module

    Pure function tests:
    - contains_ci: case-insensitive substring search
    - detect_fewshot: keyword detection in prompts
    - has_fewshot_marker: marker detection
*)

open Alcotest
open Fewshot_injector

(** {1 contains_ci Tests} *)

let test_contains_ci_exact () =
  check bool "exact match" true (contains_ci "hello world" "hello");
  check bool "end match" true (contains_ci "hello world" "world")

let test_contains_ci_case_insensitive () =
  check bool "lowercase in uppercase" true (contains_ci "HELLO" "hello");
  check bool "uppercase in lowercase" true (contains_ci "hello" "HELLO");
  check bool "mixed case" true (contains_ci "HeLLo WoRLD" "world")

let test_contains_ci_not_found () =
  check bool "not found" false (contains_ci "hello" "xyz");
  check bool "empty haystack" false (contains_ci "" "test");
  check bool "needle longer" false (contains_ci "hi" "hello")

let test_contains_ci_empty_needle () =
  check bool "empty needle" true (contains_ci "hello" "")

let test_contains_ci_special_chars () =
  check bool "with special" true (contains_ci "tokio::spawn" "tokio");
  check bool "brackets" true (contains_ci "[#tokio::main]" "tokio")

(** {1 detect_fewshot Tests} *)

let test_detect_fewshot_ocaml () =
  check (option string) "ocaml 5 detected"
    (Some "ocaml-5x-effects.md")
    (detect_fewshot "I need help with OCaml 5 effects");
  check (option string) "effect handler detected"
    (Some "ocaml-5x-effects.md")
    (detect_fewshot "How to use effect handler?")

let test_detect_fewshot_rust () =
  check (option string) "tokio detected"
    (Some "rust-async.md")
    (detect_fewshot "Using tokio for async");
  check (option string) "tokio::spawn detected"
    (Some "rust-async.md")
    (detect_fewshot "How does tokio::spawn work?")

let test_detect_fewshot_react () =
  check (option string) "server component"
    (Some "react-server.md")
    (detect_fewshot "Create a server component");
  check (option string) "use server"
    (Some "react-server.md")
    (detect_fewshot "'use server' directive")

let test_detect_fewshot_typescript () =
  check (option string) "satisfies keyword"
    (Some "typescript-5x.md")
    (detect_fewshot "Using TypeScript satisfies operator")

let test_detect_fewshot_mcp () =
  check (option string) "mcp protocol"
    (Some "mcp-protocol.md")
    (detect_fewshot "Implement MCP server");
  check (option string) "tools/call"
    (Some "mcp-protocol.md")
    (detect_fewshot "How to use tools/call?")

let test_detect_fewshot_anthropic () =
  check (option string) "anthropic sdk"
    (Some "anthropic-messages-api.md")
    (detect_fewshot "Using Anthropic SDK");
  check (option string) "claude api"
    (Some "anthropic-messages-api.md")
    (detect_fewshot "Claude API integration")

let test_detect_fewshot_none () =
  check (option string) "generic question"
    None
    (detect_fewshot "How do I sort a list?");
  check (option string) "empty prompt"
    None
    (detect_fewshot "")

(** {1 has_fewshot_marker Tests} *)

let test_has_marker_positive () =
  check bool "syntax examples marker" true
    (has_fewshot_marker "Here are correct syntax examples for you");
  check bool "syntax above marker" true
    (has_fewshot_marker "using the syntax shown above")

let test_has_marker_negative () =
  check bool "no marker" false
    (has_fewshot_marker "How do I write a function?");
  check bool "similar but not exact" false
    (has_fewshot_marker "here is some syntax")

(** {1 Test Suite} *)

let contains_ci_tests = [
  test_case "exact match" `Quick test_contains_ci_exact;
  test_case "case insensitive" `Quick test_contains_ci_case_insensitive;
  test_case "not found" `Quick test_contains_ci_not_found;
  test_case "empty needle" `Quick test_contains_ci_empty_needle;
  test_case "special chars" `Quick test_contains_ci_special_chars;
]

let detect_fewshot_tests = [
  test_case "OCaml" `Quick test_detect_fewshot_ocaml;
  test_case "Rust" `Quick test_detect_fewshot_rust;
  test_case "React" `Quick test_detect_fewshot_react;
  test_case "TypeScript" `Quick test_detect_fewshot_typescript;
  test_case "MCP" `Quick test_detect_fewshot_mcp;
  test_case "Anthropic" `Quick test_detect_fewshot_anthropic;
  test_case "None" `Quick test_detect_fewshot_none;
]

let marker_tests = [
  test_case "has marker" `Quick test_has_marker_positive;
  test_case "no marker" `Quick test_has_marker_negative;
]

let () =
  run "fewshot_injector" [
    ("contains_ci", contains_ci_tests);
    ("detect_fewshot", detect_fewshot_tests);
    ("has_fewshot_marker", marker_tests);
  ]

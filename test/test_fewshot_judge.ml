(** Tests for Fewshot_judge module

    Pure function tests for:
    - category_to_file: category → filename option
    - parse_category: string → category
    - build_judge_request: prompt → (model, full_prompt)
*)

open Alcotest
open Fewshot_judge

(** {1 Test Helpers} *)

let category = testable
  (fun ppf c ->
    let s = match c with
      | OCaml_5x_Effects -> "OCaml_5x_Effects"
      | Rust_Async -> "Rust_Async"
      | React_Server -> "React_Server"
      | Haskell_Modern -> "Haskell_Modern"
      | None_Needed -> "None_Needed"
    in
    Fmt.pf ppf "%s" s)
  (fun a b -> a = b)

(** {1 category_to_file Tests} *)

let test_category_to_file_ocaml () =
  check (option string) "OCaml maps to file"
    (Some "ocaml-5x-effects.md")
    (category_to_file OCaml_5x_Effects)

let test_category_to_file_rust () =
  check (option string) "Rust maps to file"
    (Some "rust-async.md")
    (category_to_file Rust_Async)

let test_category_to_file_react () =
  check (option string) "React maps to file"
    (Some "react-server.md")
    (category_to_file React_Server)

let test_category_to_file_haskell () =
  check (option string) "Haskell maps to file"
    (Some "haskell-modern.md")
    (category_to_file Haskell_Modern)

let test_category_to_file_none () =
  check (option string) "None_Needed maps to None"
    None
    (category_to_file None_Needed)

(** {1 parse_category Tests} *)

let test_parse_category_ocaml () =
  check category "ocaml keyword"
    OCaml_5x_Effects (parse_category "ocaml");
  check category "OCAML uppercase"
    OCaml_5x_Effects (parse_category "OCAML");
  check category "effect keyword"
    OCaml_5x_Effects (parse_category "effect handlers")

let test_parse_category_rust () =
  check category "rust keyword"
    Rust_Async (parse_category "rust");
  check category "tokio keyword"
    Rust_Async (parse_category "tokio async")

let test_parse_category_react () =
  check category "react keyword"
    React_Server (parse_category "react");
  check category "server keyword"
    React_Server (parse_category "server components")

let test_parse_category_haskell () =
  check category "haskell keyword"
    Haskell_Modern (parse_category "haskell");
  check category "lens keyword"
    Haskell_Modern (parse_category "lens library")

let test_parse_category_none () =
  check category "generic response"
    None_Needed (parse_category "none");
  check category "empty response"
    None_Needed (parse_category "");
  check category "unrelated text"
    None_Needed (parse_category "python programming")

let test_parse_category_too_long () =
  let long_response = String.make 250 'x' in
  check category "too long response defaults to None"
    None_Needed (parse_category long_response)

(** {1 build_judge_request Tests} *)

let test_build_judge_request_model () =
  let (model, _prompt) = build_judge_request "test prompt" in
  check string "uses fast model" "qwen3:0.6b" model

let test_build_judge_request_prompt () =
  let user_input = "Write OCaml code" in
  let (_model, prompt) = build_judge_request user_input in
  check bool "contains judge prompt prefix" true
    (String.length prompt > 100);
  (* Check prompt ends with user input *)
  let suffix_start = String.length prompt - String.length user_input in
  check string "ends with user prompt"
    user_input
    (String.sub prompt suffix_start (String.length user_input))

(** {1 Test Suite} *)

let category_to_file_tests = [
  test_case "OCaml" `Quick test_category_to_file_ocaml;
  test_case "Rust" `Quick test_category_to_file_rust;
  test_case "React" `Quick test_category_to_file_react;
  test_case "Haskell" `Quick test_category_to_file_haskell;
  test_case "None" `Quick test_category_to_file_none;
]

let parse_category_tests = [
  test_case "OCaml keywords" `Quick test_parse_category_ocaml;
  test_case "Rust keywords" `Quick test_parse_category_rust;
  test_case "React keywords" `Quick test_parse_category_react;
  test_case "Haskell keywords" `Quick test_parse_category_haskell;
  test_case "None/default" `Quick test_parse_category_none;
  test_case "too long" `Quick test_parse_category_too_long;
]

let build_judge_tests = [
  test_case "model selection" `Quick test_build_judge_request_model;
  test_case "prompt construction" `Quick test_build_judge_request_prompt;
]

let () =
  run "fewshot_judge" [
    ("category_to_file", category_to_file_tests);
    ("parse_category", parse_category_tests);
    ("build_judge_request", build_judge_tests);
  ]

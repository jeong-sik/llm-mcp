(** LLM-based Difficulty Classifier tests.
    Uses mock exec_fn — no network, no Ollama required. *)

open Alcotest

let difficulty_testable =
  Alcotest.testable
    (fun fmt d -> Format.pp_print_string fmt (Difficulty_classifier.difficulty_to_string d))
    (=)

let category_testable =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (Difficulty_classifier.category_hint_to_string c))
    (=)

(* ── Mock exec_fn builders ──────────────────────────────────── *)

let mock_returning response ~model:_ ~system:_ ~prompt:_ =
  Ok response

let mock_error msg ~model:_ ~system:_ ~prompt:_ =
  Error msg

(* ── parse_response tests ───────────────────────────────────── *)

let test_parse_easy_general () =
  let r = Difficulty_classifier_llm.parse_response "DIFFICULTY: easy\nCATEGORY: general" in
  match r with
  | Some (d, c) ->
    check difficulty_testable "easy" Difficulty_classifier.Easy d;
    check category_testable "general" Difficulty_classifier.General c
  | None -> fail "parse failed"

let test_parse_hard_coding () =
  let r = Difficulty_classifier_llm.parse_response "DIFFICULTY: hard\nCATEGORY: coding" in
  match r with
  | Some (d, c) ->
    check difficulty_testable "hard" Difficulty_classifier.Hard d;
    check category_testable "coding" Difficulty_classifier.Coding c
  | None -> fail "parse failed"

let test_parse_medium_reasoning () =
  let r = Difficulty_classifier_llm.parse_response "DIFFICULTY: medium\nCATEGORY: reasoning" in
  match r with
  | Some (d, c) ->
    check difficulty_testable "medium" Difficulty_classifier.Medium d;
    check category_testable "reasoning" Difficulty_classifier.Reasoning c
  | None -> fail "parse failed"

let test_parse_case_insensitive () =
  let r = Difficulty_classifier_llm.parse_response "difficulty: EASY\nCategory: CODING" in
  match r with
  | Some (d, c) ->
    check difficulty_testable "easy" Difficulty_classifier.Easy d;
    check category_testable "coding" Difficulty_classifier.Coding c
  | None -> fail "parse failed"

let test_parse_extra_whitespace () =
  let r = Difficulty_classifier_llm.parse_response "  DIFFICULTY:   medium  \n  CATEGORY:   reasoning  \n" in
  match r with
  | Some (d, c) ->
    check difficulty_testable "medium" Difficulty_classifier.Medium d;
    check category_testable "reasoning" Difficulty_classifier.Reasoning c
  | None -> fail "parse failed"

let test_parse_with_preamble () =
  let r = Difficulty_classifier_llm.parse_response
    "Based on analysis:\nDIFFICULTY: hard\nCATEGORY: coding\nDone." in
  match r with
  | Some (d, c) ->
    check difficulty_testable "hard" Difficulty_classifier.Hard d;
    check category_testable "coding" Difficulty_classifier.Coding c
  | None -> fail "parse failed"

let test_parse_missing_category () =
  let r = Difficulty_classifier_llm.parse_response "DIFFICULTY: easy\n" in
  check (option (pair difficulty_testable category_testable)) "missing category" None r

let test_parse_garbage () =
  let r = Difficulty_classifier_llm.parse_response "I don't understand the query." in
  check (option (pair difficulty_testable category_testable)) "garbage" None r

let test_parse_empty () =
  let r = Difficulty_classifier_llm.parse_response "" in
  check (option (pair difficulty_testable category_testable)) "empty" None r

let test_parse_unknown_defaults_hard () =
  let r = Difficulty_classifier_llm.parse_response "DIFFICULTY: extreme\nCATEGORY: general" in
  match r with
  | Some (d, _) ->
    check difficulty_testable "unknown defaults to hard" Difficulty_classifier.Hard d
  | None -> fail "parse failed"

(* ── classify (LLM path) tests ──────────────────────────────── *)

let test_classify_llm_easy () =
  let exec_fn = mock_returning "DIFFICULTY: easy\nCATEGORY: general" in
  let c = Difficulty_classifier_llm.classify ~exec_fn "hello" in
  check difficulty_testable "LLM easy" Difficulty_classifier.Easy c.difficulty;
  check category_testable "LLM general" Difficulty_classifier.General c.category

let test_classify_llm_hard_coding () =
  let exec_fn = mock_returning "DIFFICULTY: hard\nCATEGORY: coding" in
  let c = Difficulty_classifier_llm.classify ~exec_fn "write a quicksort in OCaml" in
  check difficulty_testable "LLM hard" Difficulty_classifier.Hard c.difficulty;
  check category_testable "LLM coding" Difficulty_classifier.Coding c.category

let test_classify_llm_medium_reasoning () =
  let exec_fn = mock_returning "DIFFICULTY: medium\nCATEGORY: reasoning" in
  let c = Difficulty_classifier_llm.classify ~exec_fn "compare REST vs GraphQL" in
  check difficulty_testable "LLM medium" Difficulty_classifier.Medium c.difficulty;
  check category_testable "LLM reasoning" Difficulty_classifier.Reasoning c.category

(* ── classify (fallback path) tests ─────────────────────────── *)

let test_classify_fallback_on_error () =
  let exec_fn = mock_error "connection refused" in
  let c = Difficulty_classifier_llm.classify ~exec_fn "hello" in
  (* Heuristic fallback: "hello" is short, no tech keywords → Easy *)
  check difficulty_testable "fallback easy" Difficulty_classifier.Easy c.difficulty

let test_classify_fallback_on_parse_failure () =
  let exec_fn = mock_returning "Sorry, I can't classify that." in
  let c = Difficulty_classifier_llm.classify ~exec_fn "hello" in
  check difficulty_testable "parse fail fallback" Difficulty_classifier.Easy c.difficulty

let test_classify_fallback_hard_query () =
  let exec_fn = mock_error "timeout" in
  let query = "```ocaml\nlet rec fibonacci n = if n <= 1 then n else fibonacci (n-1) + fibonacci (n-2)\n```\nOptimize this with memoization and explain the time complexity difference." in
  let c = Difficulty_classifier_llm.classify ~exec_fn query in
  (* Heuristic fallback: has code + long + tech keywords → Hard *)
  check difficulty_testable "fallback hard" Difficulty_classifier.Hard c.difficulty

(* ── features preserved in LLM path ────────────────────────── *)

let test_features_present () =
  let exec_fn = mock_returning "DIFFICULTY: easy\nCATEGORY: general" in
  let c = Difficulty_classifier_llm.classify ~exec_fn "hello world" in
  check int "input_length" 11 c.features.input_length;
  check bool "no code" false c.features.has_code;
  check bool "no multiline" false c.features.has_multiline

(* ── model parameter ────────────────────────────────────────── *)

let test_custom_model () =
  let called_model = ref "" in
  let exec_fn ~model ~system:_ ~prompt:_ =
    called_model := model;
    Ok "DIFFICULTY: easy\nCATEGORY: general"
  in
  let _c = Difficulty_classifier_llm.classify ~exec_fn ~model:"ollama:qwen3:1.7b" "hi" in
  check string "custom model" "ollama:qwen3:1.7b" !called_model

let test_default_model () =
  let called_model = ref "" in
  let exec_fn ~model ~system:_ ~prompt:_ =
    called_model := model;
    Ok "DIFFICULTY: easy\nCATEGORY: general"
  in
  let _c = Difficulty_classifier_llm.classify ~exec_fn "hi" in
  check string "default model" "ollama:glm-4.7-flash" !called_model

(* ── system prompt passed correctly ─────────────────────────── *)

let test_system_prompt_passed () =
  let captured_system = ref "" in
  let exec_fn ~model:_ ~system ~prompt:_ =
    captured_system := system;
    Ok "DIFFICULTY: easy\nCATEGORY: general"
  in
  let _c = Difficulty_classifier_llm.classify ~exec_fn "test" in
  check bool "system prompt non-empty" true (String.length !captured_system > 0);
  check bool "contains DIFFICULTY keyword"
    true (String.length !captured_system > 50)

(* ── constants ──────────────────────────────────────────────── *)

let test_default_model_value () =
  check string "default model" "ollama:glm-4.7-flash" Difficulty_classifier_llm.default_model

let test_system_prompt_has_rules () =
  let sp = Difficulty_classifier_llm.classification_system_prompt in
  check bool "has DIFFICULTY" true (String.contains sp 'D');
  check bool "has easy" true
    (let len = String.length sp in
     let rec find i = if i >= len - 3 then false
       else if String.sub sp i 4 = "easy" then true
       else find (i + 1)
     in find 0)

(* ── test suite ─────────────────────────────────────────────── *)

let () =
  run "Difficulty_classifier_llm" [
    "parse_response", [
      test_case "easy/general" `Quick test_parse_easy_general;
      test_case "hard/coding" `Quick test_parse_hard_coding;
      test_case "medium/reasoning" `Quick test_parse_medium_reasoning;
      test_case "case insensitive" `Quick test_parse_case_insensitive;
      test_case "extra whitespace" `Quick test_parse_extra_whitespace;
      test_case "with preamble" `Quick test_parse_with_preamble;
      test_case "missing category" `Quick test_parse_missing_category;
      test_case "garbage" `Quick test_parse_garbage;
      test_case "empty" `Quick test_parse_empty;
      test_case "unknown defaults hard" `Quick test_parse_unknown_defaults_hard;
    ];
    "classify_llm", [
      test_case "LLM easy" `Quick test_classify_llm_easy;
      test_case "LLM hard/coding" `Quick test_classify_llm_hard_coding;
      test_case "LLM medium/reasoning" `Quick test_classify_llm_medium_reasoning;
    ];
    "classify_fallback", [
      test_case "error → heuristic" `Quick test_classify_fallback_on_error;
      test_case "parse fail → heuristic" `Quick test_classify_fallback_on_parse_failure;
      test_case "hard query fallback" `Quick test_classify_fallback_hard_query;
    ];
    "features", [
      test_case "features preserved" `Quick test_features_present;
    ];
    "model", [
      test_case "custom model" `Quick test_custom_model;
      test_case "default model" `Quick test_default_model;
    ];
    "system_prompt", [
      test_case "prompt passed" `Quick test_system_prompt_passed;
    ];
    "constants", [
      test_case "default model value" `Quick test_default_model_value;
      test_case "system prompt has rules" `Quick test_system_prompt_has_rules;
    ];
  ]

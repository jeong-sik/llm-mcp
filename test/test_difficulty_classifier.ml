(** Difficulty Classifier tests
    Tests for heuristic query difficulty classification.
    Pure tests — no network, no runtime dependencies. *)

open Alcotest

let check = Alcotest.check

(** Custom testable for difficulty *)
let difficulty_testable =
  Alcotest.testable
    (fun fmt d -> Format.pp_print_string fmt (Difficulty_classifier.difficulty_to_string d))
    (=)

let category_testable =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (Difficulty_classifier.category_hint_to_string c))
    (=)

(** {1 classify — Easy} *)

let test_classify_short_query () =
  let d = Difficulty_classifier.classify "hello" in
  check difficulty_testable "short=easy" Difficulty_classifier.Easy d

let test_classify_simple_question () =
  let d = Difficulty_classifier.classify "what time is it?" in
  check difficulty_testable "simple question=easy" Difficulty_classifier.Easy d

let test_classify_greeting () =
  let d = Difficulty_classifier.classify "hi there" in
  check difficulty_testable "greeting=easy" Difficulty_classifier.Easy d

let test_classify_empty () =
  let d = Difficulty_classifier.classify "" in
  check difficulty_testable "empty=easy" Difficulty_classifier.Easy d

(** {1 classify — Hard} *)

let test_classify_long_code () =
  let code = String.concat "\n" [
    "```typescript";
    "interface UserProfile {";
    "  id: string;";
    "  name: string;";
    "  email: string;";
    "  preferences: Record<string, unknown>;";
    "}";
    "";
    "async function fetchUser(id: string): Promise<UserProfile> {";
    "  const response = await fetch(`/api/users/${id}`);";
    "  if (!response.ok) throw new Error('Failed');";
    "  return response.json();";
    "}";
    "```";
    "This code has a bug with error handling. The function should ";
    "implement retry logic with exponential backoff and also validate ";
    "the response schema before returning. How should I refactor this ";
    "to handle network failures gracefully while maintaining type safety?";
  ] in
  let d = Difficulty_classifier.classify code in
  check difficulty_testable "long code=hard" Difficulty_classifier.Hard d

let test_classify_many_keywords () =
  let query = "Analyze the TypeScript React component architecture using \
               functional programming patterns with proper error handling, \
               implement the algorithm with async/await, add comprehensive tests" in
  let d = Difficulty_classifier.classify query in
  check difficulty_testable "many keywords=hard" Difficulty_classifier.Hard d

let test_classify_multiline_with_code () =
  let query = "def foo(x):\n  return x * 2\n\nHow does this work?" in
  let d = Difficulty_classifier.classify query in
  (* Has code pattern and multiline — at least Medium *)
  let is_not_easy = d <> Difficulty_classifier.Easy in
  check bool "multiline code not easy" true is_not_easy

(** {1 classify — Medium} *)

let test_classify_moderate_query () =
  let query = "Explain how the TypeScript type system handles union types \
               and type narrowing in conditional branches" in
  let d = Difficulty_classifier.classify query in
  check difficulty_testable "moderate=medium" Difficulty_classifier.Medium d

(** {1 classify_full} *)

let test_classify_full_coding () =
  let result = Difficulty_classifier.classify_full
    "```python\ndef sort(arr):\n  pass\n```\nImplement quicksort" in
  check category_testable "coding category"
    Difficulty_classifier.Coding result.category;
  check bool "has_code" true result.features.has_code;
  check bool "has_multiline" true result.features.has_multiline

let test_classify_full_reasoning () =
  let result = Difficulty_classifier.classify_full
    "Compare the tradeoffs between microservices and monolith architecture. \
     Analyze the performance implications and scalability differences." in
  check category_testable "reasoning category"
    Difficulty_classifier.Reasoning result.category

let test_classify_full_general () =
  let result = Difficulty_classifier.classify_full "hello world" in
  check category_testable "general category"
    Difficulty_classifier.General result.category

(** {1 extract_features} *)

let test_features_code_detection () =
  let f = Difficulty_classifier.extract_features "use `map` and `filter`" in
  check bool "backticks=code" true f.has_code

let test_features_arrow_detection () =
  let f = Difficulty_classifier.extract_features "const fn = (x) => x + 1" in
  check bool "arrow=code" true f.has_code

let test_features_multiline () =
  let f = Difficulty_classifier.extract_features "line1\nline2\nline3" in
  check bool "multiline" true f.has_multiline

let test_features_no_code () =
  let f = Difficulty_classifier.extract_features "what is the weather today" in
  check bool "no code" false f.has_code

let test_features_length () =
  let f = Difficulty_classifier.extract_features "hello" in
  check int "length=5" 5 f.input_length

(** {1 difficulty_to_string / difficulty_of_string roundtrip} *)

let test_difficulty_roundtrip () =
  let open Difficulty_classifier in
  List.iter (fun d ->
    let s = difficulty_to_string d in
    let d' = difficulty_of_string s in
    check difficulty_testable (Printf.sprintf "roundtrip %s" s) d d'
  ) [Easy; Medium; Hard]

let test_difficulty_of_string_unknown () =
  let d = Difficulty_classifier.difficulty_of_string "unknown_value" in
  check difficulty_testable "unknown=hard" Difficulty_classifier.Hard d

(** {1 category_to_model_registry} *)

let model_category_testable =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (Model_registry.category_to_string c))
    (=)

let test_category_to_model_registry () =
  let open Difficulty_classifier in
  check model_category_testable "coding"
    Model_registry.Coding (category_to_model_registry Coding);
  check model_category_testable "reasoning"
    Model_registry.Reasoning (category_to_model_registry Reasoning);
  check model_category_testable "general"
    Model_registry.General (category_to_model_registry General)

(** {1 Yojson roundtrip} *)

let test_difficulty_yojson_roundtrip () =
  let open Difficulty_classifier in
  List.iter (fun d ->
    let json = difficulty_to_yojson d in
    match difficulty_of_yojson json with
    | Ok d' -> check difficulty_testable "yojson roundtrip" d d'
    | Error e -> fail (Printf.sprintf "yojson roundtrip failed: %s" e)
  ) [Easy; Medium; Hard]

let test_classification_yojson_roundtrip () =
  let result = Difficulty_classifier.classify_full
    "Implement a binary search algorithm in OCaml" in
  let json = Difficulty_classifier.classification_to_yojson result in
  match Difficulty_classifier.classification_of_yojson json with
  | Ok result' ->
    check difficulty_testable "difficulty" result.difficulty result'.difficulty;
    check category_testable "category" result.category result'.category;
    check int "input_length" result.features.input_length result'.features.input_length
  | Error e -> fail (Printf.sprintf "classification yojson failed: %s" e)

(** {1 Boundary cases} *)

let test_boundary_80_chars () =
  (* Exactly 80 chars with no keywords — should be Easy *)
  let query = String.make 79 'a' in
  let d = Difficulty_classifier.classify query in
  check difficulty_testable "79 chars no keywords=easy" Difficulty_classifier.Easy d

let test_boundary_81_chars () =
  (* 81 chars with no keywords — not Easy, but also not Hard *)
  let query = String.make 81 'a' in
  let d = Difficulty_classifier.classify query in
  let not_easy = d <> Difficulty_classifier.Easy in
  check bool "81 chars not easy" true not_easy

let test_boundary_500_chars () =
  (* Over 500 chars — should be Hard regardless *)
  let query = String.make 501 'a' in
  let d = Difficulty_classifier.classify query in
  check difficulty_testable "501 chars=hard" Difficulty_classifier.Hard d

(** {1 Stats integration} *)

let test_cascade_stats_difficulty_tracking () =
  Eio_main.run @@ fun _env ->
  Chain_stats.reset ();
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0
    ~difficulty:Difficulty_classifier.Easy;
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0
    ~difficulty:Difficulty_classifier.Easy;
  Chain_stats.track_cascade ~resolved_tier:1 ~escalations:1 ~hard_failures:0
    ~difficulty:Difficulty_classifier.Medium;
  Chain_stats.track_cascade ~resolved_tier:2 ~escalations:2 ~hard_failures:0
    ~difficulty:Difficulty_classifier.Hard;
  let stats = Chain_stats.cascade_snapshot () in
  check int "easy_count" 2 stats.easy_count;
  check int "medium_count" 1 stats.medium_count;
  check int "hard_count" 1 stats.hard_count;
  check int "total" 4 stats.total_cascades

(** {1 Test Registration} *)

let () =
  Alcotest.run "Difficulty Classifier" [
    "classify", [
      test_case "short query → Easy" `Quick test_classify_short_query;
      test_case "simple question → Easy" `Quick test_classify_simple_question;
      test_case "greeting → Easy" `Quick test_classify_greeting;
      test_case "empty → Easy" `Quick test_classify_empty;
      test_case "long code → Hard" `Quick test_classify_long_code;
      test_case "many keywords → Hard" `Quick test_classify_many_keywords;
      test_case "multiline with code" `Quick test_classify_multiline_with_code;
      test_case "moderate → Medium" `Quick test_classify_moderate_query;
    ];
    "classify_full", [
      test_case "coding category" `Quick test_classify_full_coding;
      test_case "reasoning category" `Quick test_classify_full_reasoning;
      test_case "general category" `Quick test_classify_full_general;
    ];
    "extract_features", [
      test_case "code detection (backticks)" `Quick test_features_code_detection;
      test_case "code detection (arrow)" `Quick test_features_arrow_detection;
      test_case "multiline detection" `Quick test_features_multiline;
      test_case "no code" `Quick test_features_no_code;
      test_case "length" `Quick test_features_length;
    ];
    "roundtrip", [
      test_case "difficulty string roundtrip" `Quick test_difficulty_roundtrip;
      test_case "unknown → Hard" `Quick test_difficulty_of_string_unknown;
      test_case "category → model_registry" `Quick test_category_to_model_registry;
      test_case "difficulty yojson" `Quick test_difficulty_yojson_roundtrip;
      test_case "classification yojson" `Quick test_classification_yojson_roundtrip;
    ];
    "boundary", [
      test_case "79 chars → Easy" `Quick test_boundary_80_chars;
      test_case "81 chars → not Easy" `Quick test_boundary_81_chars;
      test_case "501 chars → Hard" `Quick test_boundary_500_chars;
    ];
    "stats_integration", [
      test_case "difficulty tracking" `Quick test_cascade_stats_difficulty_tracking;
    ];
  ]

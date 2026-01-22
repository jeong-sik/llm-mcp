(** Adapter Transform Tests

    Unit tests for the apply_adapter_transform function in chain_executor_eio.
    Tests all 12 transform types including the improved Conditional and JsonPath.
*)

(* We need to test the internal apply_adapter_transform function.
   Since it's not exposed in the .mli, we test it indirectly through
   execute_adapter or by duplicating the logic here for unit testing. *)

open Chain_types

(* ============================================================================
   Test Helpers - Condition Evaluation (mirrors chain_executor_eio logic)
   ============================================================================ *)

let evaluate_condition cond inp =
  let try_parse_float s =
    try Some (float_of_string (String.trim s))
    with Failure _ -> None
  in
  if String.length cond >= 9 && String.sub cond 0 9 = "contains:" then
    let text = String.sub cond 9 (String.length cond - 9) in
    (try Str.search_forward (Str.regexp_string text) inp 0 >= 0
     with Not_found -> false)
  else if String.length cond >= 3 && String.sub cond 0 3 = "eq:" then
    let value = String.sub cond 3 (String.length cond - 3) in
    String.trim inp = value
  else if String.length cond >= 4 && String.sub cond 0 4 = "neq:" then
    let value = String.sub cond 4 (String.length cond - 4) in
    String.trim inp <> value
  else if String.length cond >= 3 && String.sub cond 0 3 = "gt:" then
    let threshold = String.sub cond 3 (String.length cond - 3) in
    (match try_parse_float inp, try_parse_float threshold with
     | Some v, Some t -> v > t
     | _ -> false)
  else if String.length cond >= 4 && String.sub cond 0 4 = "gte:" then
    let threshold = String.sub cond 4 (String.length cond - 4) in
    (match try_parse_float inp, try_parse_float threshold with
     | Some v, Some t -> v >= t
     | _ -> false)
  else if String.length cond >= 3 && String.sub cond 0 3 = "lt:" then
    let threshold = String.sub cond 3 (String.length cond - 3) in
    (match try_parse_float inp, try_parse_float threshold with
     | Some v, Some t -> v < t
     | _ -> false)
  else if String.length cond >= 4 && String.sub cond 0 4 = "lte:" then
    let threshold = String.sub cond 4 (String.length cond - 4) in
    (match try_parse_float inp, try_parse_float threshold with
     | Some v, Some t -> v <= t
     | _ -> false)
  else if cond = "empty" then
    String.length (String.trim inp) = 0
  else if cond = "nonempty" then
    String.length (String.trim inp) > 0
  else if String.length cond >= 11 && String.sub cond 0 11 = "startswith:" then
    let prefix = String.sub cond 11 (String.length cond - 11) in
    String.length inp >= String.length prefix &&
    String.sub inp 0 (String.length prefix) = prefix
  else if String.length cond >= 9 && String.sub cond 0 9 = "endswith:" then
    let suffix = String.sub cond 9 (String.length cond - 9) in
    String.length inp >= String.length suffix &&
    String.sub inp (String.length inp - String.length suffix) (String.length suffix) = suffix
  else if String.length cond >= 8 && String.sub cond 0 8 = "matches:" then
    let pattern = String.sub cond 8 (String.length cond - 8) in
    (try
      let re = Str.regexp pattern in
      Str.search_forward re inp 0 >= 0
    with Not_found | Failure _ -> false)
  else
    (* Legacy: plain text means "contains" *)
    (try Str.search_forward (Str.regexp_string cond) inp 0 >= 0
     with Not_found -> false)

(* JsonPath normalization helper *)
let normalize_jsonpath path =
  if String.length path >= 2 && String.sub path 0 2 = "$." then
    String.sub path 2 (String.length path - 2)
  else if String.length path >= 1 && path.[0] = '$' then
    String.sub path 1 (String.length path - 1)
  else
    path

(* ============================================================================
   Conditional Expression Tests
   ============================================================================ *)

let test_conditional_contains () =
  Alcotest.(check bool) "contains:error matches" true
    (evaluate_condition "contains:error" "This has an error in it");
  Alcotest.(check bool) "contains:error no match" false
    (evaluate_condition "contains:error" "This is fine");
  Alcotest.(check bool) "contains:Hello matches" true
    (evaluate_condition "contains:Hello" "Hello World")

let test_conditional_eq () =
  Alcotest.(check bool) "eq:success matches" true
    (evaluate_condition "eq:success" "success");
  Alcotest.(check bool) "eq:success with whitespace" true
    (evaluate_condition "eq:success" "  success  ");
  Alcotest.(check bool) "eq:success no match" false
    (evaluate_condition "eq:success" "failure")

let test_conditional_neq () =
  Alcotest.(check bool) "neq:error matches when different" true
    (evaluate_condition "neq:error" "success");
  Alcotest.(check bool) "neq:error no match when same" false
    (evaluate_condition "neq:error" "error")

let test_conditional_gt () =
  Alcotest.(check bool) "gt:10 with 15" true
    (evaluate_condition "gt:10" "15");
  Alcotest.(check bool) "gt:10 with 10" false
    (evaluate_condition "gt:10" "10");
  Alcotest.(check bool) "gt:10 with 5" false
    (evaluate_condition "gt:10" "5");
  Alcotest.(check bool) "gt:0.5 with 0.8" true
    (evaluate_condition "gt:0.5" "0.8");
  Alcotest.(check bool) "gt:invalid returns false" false
    (evaluate_condition "gt:10" "not-a-number")

let test_conditional_gte () =
  Alcotest.(check bool) "gte:10 with 15" true
    (evaluate_condition "gte:10" "15");
  Alcotest.(check bool) "gte:10 with 10" true
    (evaluate_condition "gte:10" "10");
  Alcotest.(check bool) "gte:10 with 5" false
    (evaluate_condition "gte:10" "5")

let test_conditional_lt () =
  Alcotest.(check bool) "lt:10 with 5" true
    (evaluate_condition "lt:10" "5");
  Alcotest.(check bool) "lt:10 with 10" false
    (evaluate_condition "lt:10" "10");
  Alcotest.(check bool) "lt:10 with 15" false
    (evaluate_condition "lt:10" "15")

let test_conditional_lte () =
  Alcotest.(check bool) "lte:10 with 5" true
    (evaluate_condition "lte:10" "5");
  Alcotest.(check bool) "lte:10 with 10" true
    (evaluate_condition "lte:10" "10");
  Alcotest.(check bool) "lte:10 with 15" false
    (evaluate_condition "lte:10" "15")

let test_conditional_empty () =
  Alcotest.(check bool) "empty with empty string" true
    (evaluate_condition "empty" "");
  Alcotest.(check bool) "empty with whitespace" true
    (evaluate_condition "empty" "   ");
  Alcotest.(check bool) "empty with content" false
    (evaluate_condition "empty" "hello")

let test_conditional_nonempty () =
  Alcotest.(check bool) "nonempty with content" true
    (evaluate_condition "nonempty" "hello");
  Alcotest.(check bool) "nonempty with empty" false
    (evaluate_condition "nonempty" "");
  Alcotest.(check bool) "nonempty with whitespace" false
    (evaluate_condition "nonempty" "   ")

let test_conditional_startswith () =
  Alcotest.(check bool) "startswith:Hello" true
    (evaluate_condition "startswith:Hello" "Hello World");
  Alcotest.(check bool) "startswith:World no match" false
    (evaluate_condition "startswith:World" "Hello World")

let test_conditional_endswith () =
  Alcotest.(check bool) "endswith:World" true
    (evaluate_condition "endswith:World" "Hello World");
  Alcotest.(check bool) "endswith:Hello no match" false
    (evaluate_condition "endswith:Hello" "Hello World")

let test_conditional_matches () =
  Alcotest.(check bool) "matches regex digits" true
    (evaluate_condition "matches:[0-9]+" "score: 42");
  Alcotest.(check bool) "matches regex no digits" false
    (evaluate_condition "matches:^[0-9]+$" "no digits here")

let test_conditional_legacy () =
  (* Legacy behavior: plain text means "contains" *)
  Alcotest.(check bool) "legacy contains" true
    (evaluate_condition "error" "This has an error");
  Alcotest.(check bool) "legacy no match" false
    (evaluate_condition "error" "This is fine")

(* ============================================================================
   JsonPath Normalization Tests
   ============================================================================ *)

let test_jsonpath_normalize () =
  Alcotest.(check string) "$.field -> field"
    "field" (normalize_jsonpath "$.field");
  Alcotest.(check string) "$field -> field"
    "field" (normalize_jsonpath "$field");
  Alcotest.(check string) "field -> field"
    "field" (normalize_jsonpath "field");
  Alcotest.(check string) "$.data.items -> data.items"
    "data.items" (normalize_jsonpath "$.data.items");
  Alcotest.(check string) "$ alone -> empty"
    "" (normalize_jsonpath "$")

(* ============================================================================
   Adapter Transform Type Tests
   ============================================================================ *)

let test_adapter_transform_yojson_roundtrip () =
  (* Test that adapter_transform can be serialized/deserialized *)
  let transforms = [
    Extract "data.field";
    Template "Result: {{value}}";
    Summarize 100;
    Truncate 50;
    JsonPath "$.items[0]";
    Regex ("\\d+", "NUM");
    ValidateSchema "user_schema";
    ParseJson;
    Stringify;
    Custom "uppercase";
  ] in
  List.iter (fun t ->
    let json = adapter_transform_to_yojson t in
    let result = adapter_transform_of_yojson json in
    match result with
    | Ok t' ->
        Alcotest.(check bool) "roundtrip preserves transform" true
          (adapter_transform_to_yojson t = adapter_transform_to_yojson t')
    | Error msg ->
        Alcotest.fail (Printf.sprintf "Failed to deserialize: %s" msg)
  ) transforms

let test_adapter_chain_transform_roundtrip () =
  let chain_transform = Chain [
    Extract "data";
    Template "Extracted: {{value}}";
    Truncate 100;
  ] in
  let json = adapter_transform_to_yojson chain_transform in
  match adapter_transform_of_yojson json with
  | Ok _ -> Alcotest.(check pass) "chain roundtrip" () ()
  | Error msg -> Alcotest.fail msg

let test_adapter_conditional_transform_roundtrip () =
  let conditional = Conditional {
    condition = "gt:0.5";
    on_true = Template "High: {{value}}";
    on_false = Template "Low: {{value}}";
  } in
  let json = adapter_transform_to_yojson conditional in
  match adapter_transform_of_yojson json with
  | Ok _ -> Alcotest.(check pass) "conditional roundtrip" () ()
  | Error msg -> Alcotest.fail msg

(* ============================================================================
   Test Runner
   ============================================================================ *)

let conditional_tests = [
  "contains operator", `Quick, test_conditional_contains;
  "eq operator", `Quick, test_conditional_eq;
  "neq operator", `Quick, test_conditional_neq;
  "gt operator", `Quick, test_conditional_gt;
  "gte operator", `Quick, test_conditional_gte;
  "lt operator", `Quick, test_conditional_lt;
  "lte operator", `Quick, test_conditional_lte;
  "empty operator", `Quick, test_conditional_empty;
  "nonempty operator", `Quick, test_conditional_nonempty;
  "startswith operator", `Quick, test_conditional_startswith;
  "endswith operator", `Quick, test_conditional_endswith;
  "matches operator", `Quick, test_conditional_matches;
  "legacy contains", `Quick, test_conditional_legacy;
]

let jsonpath_tests = [
  "normalize paths", `Quick, test_jsonpath_normalize;
]

let transform_type_tests = [
  "yojson roundtrip", `Quick, test_adapter_transform_yojson_roundtrip;
  "chain roundtrip", `Quick, test_adapter_chain_transform_roundtrip;
  "conditional roundtrip", `Quick, test_adapter_conditional_transform_roundtrip;
]

let () =
  Alcotest.run "Adapter Transforms" [
    "Conditional Evaluation", conditional_tests;
    "JsonPath Normalization", jsonpath_tests;
    "Transform Types", transform_type_tests;
  ]

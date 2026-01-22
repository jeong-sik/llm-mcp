(** Chain Parser Safety Tests

    Tests for error handling improvements and placeholder validation.
    Ensures robust parsing with explicit error messages.
*)

open Chain_types
open Chain_parser

(* ============================================================================
   Test Fixtures - Error Cases
   ============================================================================ *)

(** Threshold with missing required 'value' field *)
let threshold_missing_value_json = {|
{
  "id": "bad_threshold",
  "nodes": [
    {
      "id": "check",
      "type": "threshold",
      "metric": "confidence",
      "operator": "gt"
    }
  ],
  "output": "check"
}
|}

(** GoalDriven with missing required 'goal_value' field *)
let goaldriven_missing_value_json = {|
{
  "id": "bad_goaldriven",
  "nodes": [
    {
      "id": "optimize",
      "type": "goal_driven",
      "goal_metric": "score",
      "goal_operator": "gte",
      "measure_func": "eval",
      "action_node": {
        "id": "action",
        "type": "llm",
        "model": "gemini",
        "prompt": "test"
      }
    }
  ],
  "output": "optimize"
}
|}

(** Valid threshold for comparison *)
let valid_threshold_json = {|
{
  "id": "good_threshold",
  "nodes": [
    {
      "id": "check",
      "type": "threshold",
      "metric": "confidence",
      "operator": "gt",
      "value": 0.8,
      "input_node": {
        "id": "inner",
        "type": "llm",
        "model": "gemini",
        "prompt": "test"
      }
    }
  ],
  "output": "check"
}
|}

(** Chain with placeholder that should fail validation *)
let chain_with_placeholder_json = {|
{
  "id": "placeholder_test",
  "nodes": [
    {
      "id": "gate",
      "type": "gate",
      "condition": "x > 0",
      "then": {
        "id": "_placeholder",
        "type": "chain_ref",
        "ref": "_"
      }
    }
  ],
  "output": "gate"
}
|}

(** Valid chain without placeholders *)
let valid_chain_no_placeholder_json = {|
{
  "id": "valid_chain",
  "nodes": [
    {
      "id": "gate",
      "type": "gate",
      "condition": "x > 0",
      "then": {
        "id": "action",
        "type": "llm",
        "model": "gemini",
        "prompt": "test"
      }
    }
  ],
  "output": "gate"
}
|}

(* ============================================================================
   Test Helpers
   ============================================================================ *)

let parse_json_string s =
  Yojson.Safe.from_string s

let test_parse_chain json_str expected_result name =
  let json = parse_json_string json_str in
  match parse_chain json with
  | Ok _ when expected_result = `Ok ->
      Printf.printf "[OK] %s\n%!" name;
      true
  | Error msg when expected_result = `Error ->
      Printf.printf "[OK] %s (got expected error: %s)\n%!" name (String.sub msg 0 (min 50 (String.length msg)));
      true
  | Ok _ ->
      Printf.printf "[FAIL] %s: Expected error but got Ok\n%!" name;
      false
  | Error msg ->
      Printf.printf "[FAIL] %s: Expected Ok but got Error: %s\n%!" name msg;
      false

let test_validate_chain json_str expected_result name =
  let json = parse_json_string json_str in
  match parse_chain json with
  | Error msg ->
      Printf.printf "[SKIP] %s: Parse failed: %s\n%!" name msg;
      expected_result = `ParseError
  | Ok chain ->
      match validate_chain chain with
      | Ok () when expected_result = `Ok ->
          Printf.printf "[OK] %s (validation passed)\n%!" name;
          true
      | Error msg when expected_result = `Error ->
          Printf.printf "[OK] %s (got expected validation error: %s)\n%!" name
            (String.sub msg 0 (min 60 (String.length msg)));
          true
      | Ok () ->
          Printf.printf "[FAIL] %s: Expected validation error but got Ok\n%!" name;
          false
      | Error msg ->
          Printf.printf "[FAIL] %s: Expected Ok but got Error: %s\n%!" name msg;
          false

(* ============================================================================
   Test: Error Handling for Required Fields
   ============================================================================ *)

let test_required_fields () =
  Printf.printf "\n=== Testing Required Field Error Handling ===\n%!";
  let results = [
    test_parse_chain threshold_missing_value_json `Error
      "Threshold missing 'value' returns error";
    test_parse_chain goaldriven_missing_value_json `Error
      "GoalDriven missing 'goal_value' returns error";
    test_parse_chain valid_threshold_json `Ok
      "Valid threshold parses successfully";
  ] in
  List.for_all Fun.id results

(* ============================================================================
   Test: Placeholder Validation
   ============================================================================ *)

let test_placeholder_validation () =
  Printf.printf "\n=== Testing Placeholder Validation ===\n%!";
  let results = [
    test_validate_chain chain_with_placeholder_json `Error
      "Chain with _placeholder fails validation";
    test_validate_chain valid_chain_no_placeholder_json `Ok
      "Chain without placeholder passes validation";
  ] in
  List.for_all Fun.id results

(* ============================================================================
   Test: Safe JSON Helper Functions
   ============================================================================ *)

let test_json_helpers () =
  Printf.printf "\n=== Testing Safe JSON Helpers ===\n%!";

  (* Test require_float *)
  let json_with_float = `Assoc [("value", `Float 3.14)] in
  let json_with_int = `Assoc [("value", `Int 42)] in
  let json_missing = `Assoc [] in
  let json_wrong_type = `Assoc [("value", `String "not a number")] in

  let test1 = match require_float json_with_float "value" with
    | Ok f when f = 3.14 -> Printf.printf "[OK] require_float with float\n%!"; true
    | _ -> Printf.printf "[FAIL] require_float with float\n%!"; false
  in

  let test2 = match require_float json_with_int "value" with
    | Ok f when f = 42.0 -> Printf.printf "[OK] require_float with int\n%!"; true
    | _ -> Printf.printf "[FAIL] require_float with int\n%!"; false
  in

  let test3 = match require_float json_missing "value" with
    | Error msg when String.length msg > 0 ->
        Printf.printf "[OK] require_float missing returns error\n%!"; true
    | _ -> Printf.printf "[FAIL] require_float missing\n%!"; false
  in

  let test4 = match require_float json_wrong_type "value" with
    | Error msg when String.length msg > 0 ->
        Printf.printf "[OK] require_float wrong type returns error\n%!"; true
    | _ -> Printf.printf "[FAIL] require_float wrong type\n%!"; false
  in

  (* Test require_string *)
  let json_with_string = `Assoc [("name", `String "test")] in

  let test5 = match require_string json_with_string "name" with
    | Ok s when s = "test" -> Printf.printf "[OK] require_string with string\n%!"; true
    | _ -> Printf.printf "[FAIL] require_string with string\n%!"; false
  in

  let test6 = match require_string json_missing "name" with
    | Error msg when String.length msg > 0 ->
        Printf.printf "[OK] require_string missing returns error\n%!"; true
    | _ -> Printf.printf "[FAIL] require_string missing\n%!"; false
  in

  test1 && test2 && test3 && test4 && test5 && test6

(* ============================================================================
   Test: Operator Parsing Edge Cases
   ============================================================================ *)

let test_operator_parsing () =
  Printf.printf "\n=== Testing Operator Parsing ===\n%!";

  let test_op op_str expected name =
    match parse_threshold_op op_str with
    | Ok op when op = expected ->
        Printf.printf "[OK] %s\n%!" name; true
    | Ok _ ->
        Printf.printf "[FAIL] %s: wrong operator\n%!" name; false
    | Error _ ->
        Printf.printf "[FAIL] %s: parse error\n%!" name; false
  in

  let test_invalid op_str name =
    match parse_threshold_op op_str with
    | Error _ ->
        Printf.printf "[OK] %s (invalid operator rejected)\n%!" name; true
    | Ok _ ->
        Printf.printf "[FAIL] %s: should have been rejected\n%!" name; false
  in

  let results = [
    test_op "gt" Gt "gt parses correctly";
    test_op ">" Gt "> alias parses correctly";
    test_op "gte" Gte "gte parses correctly";
    test_op ">=" Gte ">= alias parses correctly";
    test_op "lt" Lt "lt parses correctly";
    test_op "<" Lt "< alias parses correctly";
    test_op "eq" Eq "eq parses correctly";
    test_op "==" Eq "== alias parses correctly";
    test_op "neq" Neq "neq parses correctly";
    test_op "!=" Neq "!= alias parses correctly";
    test_invalid "invalid" "invalid operator rejected";
    test_invalid "" "empty operator rejected";
  ] in
  List.for_all Fun.id results

(* ============================================================================
   Main Test Runner
   ============================================================================ *)

let () =
  Printf.printf "Chain Parser Safety Tests\n";
  Printf.printf "=========================\n%!";

  let all_passed = ref true in

  if not (test_json_helpers ()) then all_passed := false;
  if not (test_required_fields ()) then all_passed := false;
  if not (test_placeholder_validation ()) then all_passed := false;
  if not (test_operator_parsing ()) then all_passed := false;

  Printf.printf "\n";
  if !all_passed then begin
    Printf.printf "✅ All Chain Parser Safety tests passed!\n%!";
    exit 0
  end else begin
    Printf.printf "❌ Some tests failed!\n%!";
    exit 1
  end

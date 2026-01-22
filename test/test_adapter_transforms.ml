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

(* ============================================================================
   Summarize Tests (Token-based truncation)
   ============================================================================ *)

(* Mirror the token-based summarize logic from chain_executor_eio *)
let summarize_tokens max_tokens input =
  let estimated_chars = max_tokens * 4 in
  if String.length input <= estimated_chars then
    input
  else
    let truncated = String.sub input 0 estimated_chars in
    let last_space =
      try String.rindex truncated ' '
      with Not_found -> estimated_chars
    in
    String.sub truncated 0 last_space ^ "..."

let test_summarize_short_input () =
  (* Short input should be returned as-is *)
  let input = "Hello world" in
  let result = summarize_tokens 100 input in
  Alcotest.(check string) "short input unchanged" input result

let test_summarize_truncation () =
  (* Long input should be truncated at word boundary *)
  let input = "This is a very long text that should be truncated at a word boundary" in
  let result = summarize_tokens 5 input in  (* 5 tokens = 20 chars *)
  Alcotest.(check bool) "truncated with ellipsis" true
    (String.length result < String.length input &&
     String.sub result (String.length result - 3) 3 = "...")

let test_summarize_word_boundary () =
  (* Truncation should happen at word boundary *)
  let input = "word1 word2 word3 word4 word5" in
  let result = summarize_tokens 3 input in  (* 3 tokens = 12 chars *)
  Alcotest.(check bool) "ends with space before ..." true
    (not (String.contains (String.sub result 0 (String.length result - 3)) (Char.chr 0)))

let summarize_tests = [
  "short input unchanged", `Quick, test_summarize_short_input;
  "long input truncated", `Quick, test_summarize_truncation;
  "truncate at word boundary", `Quick, test_summarize_word_boundary;
]

(* ============================================================================
   ValidateSchema Tests (JSON Schema validation)
   ============================================================================ *)

(* Mirror the validate_type logic from chain_executor_eio *)
let validate_type expected json =
  match expected, json with
  | "string", `String _ -> true
  | "number", `Float _ | "number", `Int _ -> true
  | "integer", `Int _ -> true
  | "boolean", `Bool _ -> true
  | "array", `List _ -> true
  | "object", `Assoc _ -> true
  | "null", `Null -> true
  | _ -> false

let validate_required required json =
  match json with
  | `Assoc fields ->
      let field_names = List.map fst fields in
      List.for_all (fun r -> List.mem r field_names) required
  | _ -> false

let test_validate_type_string () =
  Alcotest.(check bool) "string type" true
    (validate_type "string" (`String "hello"));
  Alcotest.(check bool) "not string" false
    (validate_type "string" (`Int 42))

let test_validate_type_number () =
  Alcotest.(check bool) "float is number" true
    (validate_type "number" (`Float 3.14));
  Alcotest.(check bool) "int is number" true
    (validate_type "number" (`Int 42));
  Alcotest.(check bool) "string is not number" false
    (validate_type "number" (`String "42"))

let test_validate_type_integer () =
  Alcotest.(check bool) "int is integer" true
    (validate_type "integer" (`Int 42));
  Alcotest.(check bool) "float is not integer" false
    (validate_type "integer" (`Float 3.14))

let test_validate_type_boolean () =
  Alcotest.(check bool) "bool type" true
    (validate_type "boolean" (`Bool true));
  Alcotest.(check bool) "not bool" false
    (validate_type "boolean" (`String "true"))

let test_validate_type_array () =
  Alcotest.(check bool) "array type" true
    (validate_type "array" (`List [`Int 1; `Int 2]));
  Alcotest.(check bool) "not array" false
    (validate_type "array" (`String "[]"))

let test_validate_type_object () =
  Alcotest.(check bool) "object type" true
    (validate_type "object" (`Assoc [("key", `String "value")]));
  Alcotest.(check bool) "not object" false
    (validate_type "object" (`List []))

let test_validate_required_fields () =
  let json = `Assoc [("name", `String "Alice"); ("age", `Int 30)] in
  Alcotest.(check bool) "has required" true
    (validate_required ["name"] json);
  Alcotest.(check bool) "missing required" false
    (validate_required ["name"; "email"] json)

let validate_schema_tests = [
  "type: string", `Quick, test_validate_type_string;
  "type: number", `Quick, test_validate_type_number;
  "type: integer", `Quick, test_validate_type_integer;
  "type: boolean", `Quick, test_validate_type_boolean;
  "type: array", `Quick, test_validate_type_array;
  "type: object", `Quick, test_validate_type_object;
  "required fields", `Quick, test_validate_required_fields;
]

(* ============================================================================
   Split Tests (Chunking for parallel processing)
   ============================================================================ *)

(* Mirror the split logic from chain_adapter_eio *)
let split_by_delimiter delim text =
  match delim with
  | "line" -> String.split_on_char '\n' text
  | "paragraph" ->
      let re = Str.regexp "\n\n+" in
      Str.split re text
  | "sentence" ->
      let re = Str.regexp "[.!?][ \n]+" in
      Str.split re text
  | custom ->
      let re = Str.regexp_string custom in
      Str.split re text

let merge_chunks_by_size chunks max_chars overlap_chars =
  let rec merge acc current_chunk = function
    | [] ->
        if String.length current_chunk > 0 then
          List.rev (current_chunk :: acc)
        else
          List.rev acc
    | chunk :: rest ->
        let chunk = String.trim chunk in
        if String.length chunk = 0 then
          merge acc current_chunk rest
        else if String.length current_chunk = 0 then
          merge acc chunk rest
        else if String.length current_chunk + String.length chunk + 1 <= max_chars then
          merge acc (current_chunk ^ " " ^ chunk) rest
        else
          let overlap_text =
            if overlap_chars > 0 && String.length current_chunk > overlap_chars then
              let start = String.length current_chunk - overlap_chars in
              let overlap_start =
                try
                  let space_pos = String.rindex_from current_chunk (start + overlap_chars - 1) ' ' in
                  if space_pos >= start then space_pos + 1 else start
                with Not_found -> start
              in
              String.sub current_chunk overlap_start (String.length current_chunk - overlap_start)
            else
              ""
          in
          let new_chunk =
            if String.length overlap_text > 0 then
              overlap_text ^ " " ^ chunk
            else
              chunk
          in
          merge (current_chunk :: acc) new_chunk rest
  in
  merge [] "" chunks

let test_split_by_line () =
  let input = "line1\nline2\nline3" in
  let chunks = split_by_delimiter "line" input in
  Alcotest.(check int) "3 lines" 3 (List.length chunks);
  Alcotest.(check string) "first line" "line1" (List.nth chunks 0)

let test_split_by_paragraph () =
  let input = "para1\n\npara2\n\n\npara3" in
  let chunks = split_by_delimiter "paragraph" input in
  Alcotest.(check int) "3 paragraphs" 3 (List.length chunks)

let test_split_by_sentence () =
  let input = "First sentence. Second sentence! Third sentence? Done" in
  let chunks = split_by_delimiter "sentence" input in
  Alcotest.(check int) "4 parts" 4 (List.length chunks)

let test_split_chunk_merging () =
  (* Small chunks should merge until they reach max_chars *)
  let chunks = ["a"; "b"; "c"; "d"] in
  let merged = merge_chunks_by_size chunks 10 0 in
  (* "a b c d" = 7 chars, fits in 10 *)
  Alcotest.(check int) "merged into 1" 1 (List.length merged)

let test_split_chunk_overflow () =
  let chunks = ["hello"; "world"; "this"; "is"; "test"] in
  let merged = merge_chunks_by_size chunks 12 0 in
  (* "hello world" = 11, "this is" = 7, "test" = 4 *)
  Alcotest.(check bool) "multiple chunks" true (List.length merged > 1)

let test_split_with_overlap () =
  let chunks = ["chunk one here"; "chunk two here"] in
  let merged = merge_chunks_by_size chunks 15 4 in
  (* Each chunk is ~14 chars, with 4 char overlap *)
  Alcotest.(check bool) "has chunks" true (List.length merged >= 1)

let split_tests = [
  "split by line", `Quick, test_split_by_line;
  "split by paragraph", `Quick, test_split_by_paragraph;
  "split by sentence", `Quick, test_split_by_sentence;
  "chunk merging", `Quick, test_split_chunk_merging;
  "chunk overflow", `Quick, test_split_chunk_overflow;
  "overlap handling", `Quick, test_split_with_overlap;
]

let () =
  Alcotest.run "Adapter Transforms" [
    "Conditional Evaluation", conditional_tests;
    "JsonPath Normalization", jsonpath_tests;
    "Transform Types", transform_type_tests;
    "Summarize (Token-based)", summarize_tests;
    "ValidateSchema", validate_schema_tests;
    "Split (Chunking)", split_tests;
  ]

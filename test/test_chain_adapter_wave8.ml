(** Wave 8: chain_adapter_eio coverage — apply_adapter_transform branches *)

open Chain_types

let check_ok msg expected result =
  match result with
  | Ok v -> Alcotest.(check string) msg expected v
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)

let check_error_contains msg substring result =
  match result with
  | Ok v -> Alcotest.fail (Printf.sprintf "%s: Expected Error, got Ok: %s" msg v)
  | Error e ->
    if not (String.length e >= String.length substring &&
            (try ignore (Str.search_forward (Str.regexp_string substring) e 0); true
             with Not_found -> false))
    then Alcotest.fail (Printf.sprintf "%s: Error '%s' does not contain '%s'" msg e substring)

(* ── Extract ── *)
let test_extract_simple () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "name") {|{"name":"alice"}|} in
  check_ok "extract simple" {|"alice"|} r

let test_extract_nested () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "a.b.c") {|{"a":{"b":{"c":42}}}|} in
  check_ok "extract nested" "42" r

let test_extract_missing_key () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "missing") {|{"name":"alice"}|} in
  check_error_contains "missing key" "not found" r

let test_extract_invalid_json () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "x") "not json" in
  check_error_contains "invalid json" "JSON parse error" r

let test_extract_non_object () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "x") {|"just a string"|} in
  check_error_contains "non-object" "non-object" r

let test_extract_array_index () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "[1]") {|[10,20,30]|} in
  check_ok "array index" "20" r

let test_extract_array_index_oob () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "[99]") {|[10,20,30]|} in
  check_error_contains "oob" "out of bounds" r

let test_extract_array_invalid_index () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "[abc]") {|[10,20,30]|} in
  check_error_contains "invalid index" "Invalid index" r

let test_extract_nested_array () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Extract "items.[0].name") {|{"items":[{"name":"first"}]}|} in
  check_ok "nested array" {|"first"|} r

(* ── Template ── *)
let test_template_basic () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Template "Hello {{value}}!") "world" in
  check_ok "template" "Hello world!" r

let test_template_multiple () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Template "{{value}} and {{value}}") "X" in
  check_ok "template multi" "X and X" r

(* ── Summarize ── *)
let test_summarize_short () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Summarize 100) "short text" in
  check_ok "short text passes through" "short text" r

let test_summarize_truncate () =
  let long = String.make 500 'x' in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Summarize 10) long in
  (match r with
   | Ok v ->
     Alcotest.(check bool) "ends with ..." true (String.length v < 500);
     Alcotest.(check bool) "has ellipsis" true
       (String.length v >= 3 && String.sub v (String.length v - 3) 3 = "...")
   | Error e -> Alcotest.fail e)

let test_summarize_word_boundary () =
  let text = "hello world this is a test of summarization" in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Summarize 3) text in
  (match r with
   | Ok v ->
     Alcotest.(check bool) "truncated" true (String.length v < String.length text)
   | Error e -> Alcotest.fail e)

(* ── Truncate ── *)
let test_truncate_short () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Truncate 100) "short" in
  check_ok "no truncation" "short" r

let test_truncate_long () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Truncate 5) "hello world" in
  check_ok "truncated" "hello..." r

(* ── JsonPath ── *)
let test_jsonpath_dollar_dot () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (JsonPath "$.name") {|{"name":"test"}|} in
  check_ok "jsonpath $." {|"test"|} r

let test_jsonpath_dollar_only () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (JsonPath "$") {|{"name":"test"}|} in
  check_ok "jsonpath $ root" {|{"name":"test"}|} r

let test_jsonpath_no_prefix () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (JsonPath "name") {|{"name":"test"}|} in
  check_ok "jsonpath no prefix" {|"test"|} r

let test_jsonpath_dollar_no_dot () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (JsonPath "$name") {|{"name":"test"}|} in
  check_ok "jsonpath $ no dot" {|"test"|} r

(* ── Regex ── *)
let test_regex_replace () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Regex ("world", "earth")) "hello world" in
  check_ok "regex replace" "hello earth" r

let test_regex_invalid () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Regex ("[invalid", "x")) "test" in
  check_error_contains "invalid regex" "Invalid regex" r

(* ── ValidateSchema ── *)
let test_validate_schema_string () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "string") {|"hello"|} in
  check_ok "valid string" {|"hello"|} r

let test_validate_schema_object () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "object") {|{"a":1}|} in
  check_ok "valid object" {|{"a":1}|} r

let test_validate_schema_type_mismatch () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "number") {|"hello"|} in
  check_error_contains "type mismatch" "expected type" r

let test_validate_schema_required () =
  let schema = {|{"type":"object","required":["name","age"]}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) {|{"name":"alice"}|} in
  check_error_contains "missing required" "missing required" r

let test_validate_schema_required_pass () =
  let schema = {|{"type":"object","required":["name"]}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) {|{"name":"alice"}|} in
  check_ok "required present" {|{"name":"alice"}|} r

let test_validate_schema_enum () =
  let schema = {|{"enum":["red","green","blue"]}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) {|"red"|} in
  check_ok "enum valid" {|"red"|} r

let test_validate_schema_enum_fail () =
  let schema = {|{"enum":["red","green","blue"]}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) {|"purple"|} in
  check_error_contains "enum fail" "not in enum" r

let test_validate_schema_minlength () =
  let schema = {|{"type":"string","minLength":5}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) {|"hi"|} in
  check_error_contains "minLength" "string length" r

let test_validate_schema_maxlength () =
  let schema = {|{"type":"string","maxLength":3}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) {|"hello world"|} in
  check_error_contains "maxLength" "string length" r

let test_validate_schema_minimum () =
  let schema = {|{"type":"number","minimum":10}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) "5" in
  check_error_contains "minimum" "number range" r

let test_validate_schema_maximum () =
  let schema = {|{"type":"number","maximum":10}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) "15" in
  check_error_contains "maximum" "number range" r

let test_validate_schema_number_pass () =
  let schema = {|{"type":"number","minimum":1,"maximum":100}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) "50" in
  check_ok "number in range" "50" r

let test_validate_schema_integer () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "integer") "42" in
  check_ok "valid integer" "42" r

let test_validate_schema_boolean () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "boolean") "true" in
  check_ok "valid boolean" "true" r

let test_validate_schema_array () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "array") {|[1,2,3]|} in
  check_ok "valid array" {|[1,2,3]|} r

let test_validate_schema_null () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "null") "null" in
  check_ok "valid null" "null" r

let test_validate_schema_invalid_json () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema "string") "not json" in
  check_error_contains "invalid json input" "Invalid JSON" r

let test_validate_schema_int_range () =
  let schema = {|{"type":"integer","minimum":5,"maximum":10}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) "7" in
  check_ok "int in range" "7" r

let test_validate_schema_int_range_fail () =
  let schema = {|{"type":"integer","minimum":5,"maximum":10}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) "3" in
  check_error_contains "int out of range" "number range" r

let test_validate_schema_float_range () =
  let schema = {|{"type":"number","minimum":1.5,"maximum":3.5}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) "2.5" in
  check_ok "float in range" "2.5" r

let test_validate_schema_required_non_object () =
  let schema = {|{"required":["x"]}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (ValidateSchema schema) {|[1,2]|} in
  check_error_contains "required non-obj" "missing required" r

(* ── ParseJson ── *)
let test_parsejson_valid () =
  let r = Chain_adapter_eio.apply_adapter_transform
    ParseJson {|{"a":1}|} in
  check_ok "valid json" {|{"a":1}|} r

let test_parsejson_invalid () =
  let r = Chain_adapter_eio.apply_adapter_transform
    ParseJson "not json" in
  check_error_contains "invalid json" "Not valid JSON" r

(* ── Stringify ── *)
let test_stringify_json () =
  let r = Chain_adapter_eio.apply_adapter_transform
    Stringify {|{"a":1}|} in
  check_ok "already json" {|{"a":1}|} r

let test_stringify_plain () =
  let r = Chain_adapter_eio.apply_adapter_transform
    Stringify "plain text" in
  check_ok "stringified" {|"plain text"|} r

(* ── Chain ── *)
let test_chain_transforms () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Chain [Custom "uppercase"; Custom "trim"])
    "  hello  " in
  check_ok "chain" "HELLO" r

let test_chain_error_stops () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Chain [ParseJson; Custom "uppercase"])
    "not json" in
  check_error_contains "chain stops on error" "Not valid JSON" r

let test_chain_empty () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Chain []) "input" in
  check_ok "empty chain" "input" r

(* ── Conditional ── *)
let test_conditional_contains_true () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "contains:hello"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "say hello" in
  check_ok "contains true" "SAY HELLO" r

let test_conditional_contains_false () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "contains:hello"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "say goodbye" in
  check_ok "contains false" "say goodbye" r

let test_conditional_eq () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "eq:yes"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "yes" in
  check_ok "eq true" "YES" r

let test_conditional_eq_trimmed () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "eq:yes"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "  yes  " in
  check_ok "eq with trim" "  YES  " r

let test_conditional_neq () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "neq:no"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "yes" in
  check_ok "neq true" "YES" r

let test_conditional_gt () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "gt:5"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "10" in
  check_ok "gt true" "10" r

let test_conditional_gt_false () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "gt:5"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "3" in
  check_ok "gt false" "3" r

let test_conditional_gt_non_numeric () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "gt:5"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "abc" in
  check_ok "gt non-numeric" "abc" r

let test_conditional_gte () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "gte:5"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "5" in
  check_ok "gte true" "5" r

let test_conditional_lt () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "lt:5"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "3" in
  check_ok "lt true" "3" r

let test_conditional_lte () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "lte:5"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "5" in
  check_ok "lte true" "5" r

let test_conditional_empty () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "empty"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "   " in
  check_ok "empty true" "   " r

let test_conditional_nonempty () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "nonempty"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "hello" in
  check_ok "nonempty true" "HELLO" r

let test_conditional_startswith () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "startswith:he"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "hello" in
  check_ok "startswith true" "HELLO" r

let test_conditional_startswith_false () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "startswith:xx"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "hello" in
  check_ok "startswith false" "hello" r

let test_conditional_endswith () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "endswith:lo"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "hello" in
  check_ok "endswith true" "HELLO" r

let test_conditional_endswith_false () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "endswith:xx"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "hello" in
  check_ok "endswith false" "hello" r

let test_conditional_matches () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "matches:[0-9]+"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "abc123" in
  check_ok "matches true" "ABC123" r

let test_conditional_matches_no_match () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "matches:^[0-9]+$"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "abc" in
  check_ok "matches false" "abc" r

let test_conditional_matches_too_long () =
  let long_pattern = "matches:" ^ String.make 110 'a' in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = long_pattern; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "hello" in
  check_ok "matches too long -> false" "hello" r

let test_conditional_legacy_contains () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "hello"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "say hello world" in
  check_ok "legacy contains" "SAY HELLO WORLD" r

let test_conditional_legacy_not_found () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "zzz"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "hello" in
  check_ok "legacy not found" "hello" r

(* ── Split ── *)
let test_split_line () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Split { delimiter = "line"; chunk_size = 1; overlap = 0 })
    "line1\nline2\nline3" in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     (match json with
      | `List items -> Alcotest.(check int) "3 chunks" 3 (List.length items)
      | _ -> Alcotest.fail "expected list")
   | Error e -> Alcotest.fail e)

let test_split_paragraph () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Split { delimiter = "paragraph"; chunk_size = 1; overlap = 0 })
    "para1\n\npara2\n\npara3" in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     (match json with
      | `List items -> Alcotest.(check int) "3 paragraphs" 3 (List.length items)
      | _ -> Alcotest.fail "expected list")
   | Error e -> Alcotest.fail e)

let test_split_sentence () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Split { delimiter = "sentence"; chunk_size = 1; overlap = 0 })
    "First sentence. Second sentence! Third sentence? Done" in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     (match json with
      | `List items ->
        Alcotest.(check bool) "at least 3" true (List.length items >= 3)
      | _ -> Alcotest.fail "expected list")
   | Error e -> Alcotest.fail e)

let test_split_custom_delimiter () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Split { delimiter = "||"; chunk_size = 1; overlap = 0 })
    "item_one||item_two||item_three" in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     (match json with
      | `List items -> Alcotest.(check int) "3 parts" 3 (List.length items)
      | _ -> Alcotest.fail "expected list")
   | Error e -> Alcotest.fail e)

let test_split_with_chunk_merge () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Split { delimiter = "line"; chunk_size = 100; overlap = 0 })
    "a\nb\nc" in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     (match json with
      | `List items ->
        (* Small chunks should merge into 1 *)
        Alcotest.(check bool) "merged" true (List.length items <= 3)
      | _ -> Alcotest.fail "expected list")
   | Error e -> Alcotest.fail e)

let test_split_with_overlap () =
  (* Create content large enough to need splitting *)
  let chunk1 = String.make 200 'a' in
  let chunk2 = String.make 200 'b' in
  let input = chunk1 ^ "\n" ^ chunk2 in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Split { delimiter = "line"; chunk_size = 60; overlap = 10 })
    input in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     (match json with
      | `List items ->
        Alcotest.(check bool) "split happened" true (List.length items >= 2)
      | _ -> Alcotest.fail "expected list")
   | Error e -> Alcotest.fail e)

(* ── Custom functions ── *)
let test_custom_identity () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "identity") "hello" in
  check_ok "identity" "hello" r

let test_custom_uppercase () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "uppercase") "hello" in
  check_ok "uppercase" "HELLO" r

let test_custom_lowercase () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "lowercase") "HELLO" in
  check_ok "lowercase" "hello" r

let test_custom_trim () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "trim") "  hello  " in
  check_ok "trim" "hello" r

let test_custom_extract_json_object () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_json") {|Some text {"key":"val"} more text|} in
  check_ok "extract json obj" {|{"key":"val"}|} r

let test_custom_extract_json_array () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_json") {|prefix [1,2,3] suffix|} in
  check_ok "extract json arr" "[1,2,3]" r

let test_custom_extract_json_not_found () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_json") "no json here" in
  check_error_contains "no json" "No JSON" r

let test_custom_extract_json_unbalanced () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_json") {|prefix {"a": "b"|} in
  check_error_contains "unbalanced" "Unbalanced" r

let test_custom_extract_json_nested () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_json") {|text {"a":{"b":"c"}} end|} in
  check_ok "nested json" {|{"a":{"b":"c"}}|} r

let test_custom_extract_json_with_string () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_json") {|text {"a":"val with } brace"} end|} in
  check_ok "json with brace in string" {|{"a":"val with } brace"}|} r

let test_custom_extract_json_with_escape () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_json") {|text {"a":"val with \" quote"} end|} in
  check_ok "json with escape" {|{"a":"val with \" quote"}|} r

let test_custom_extract_html () =
  let html = {|Some preamble <!doctype html><html><body>hello</body></html> trailer|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_html") html in
  (match r with
   | Ok v ->
     Alcotest.(check bool) "starts with doctype" true
       (String.length v >= 15 && String.lowercase_ascii (String.sub v 0 15) = "<!doctype html>")
   | Error e -> Alcotest.fail e)

let test_custom_extract_html_no_html () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_html") "just plain text" in
  check_ok "no html -> passthrough" "just plain text" r

let test_custom_extract_html_no_closing () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_html") "<html><body>no closing" in
  check_ok "no closing -> passthrough" "<html><body>no closing" r

let test_custom_extract_html_field_json () =
  let input = {|{"html":"<div>content</div>","other":"data"}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_html_field") input in
  check_ok "html field" "<div>content</div>" r

let test_custom_extract_html_field_no_html () =
  let input = {|{"other":"data"}|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_html_field") input in
  (* Falls through to extract_html which passes through *)
  (match r with
   | Ok _ -> () (* Any Ok is fine *)
   | Error e -> Alcotest.fail e)

let test_custom_extract_html_field_embedded_json () =
  let input = {|Some text {"html":"<p>hello</p>"} more|} in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "extract_html_field") input in
  check_ok "embedded json html field" "<p>hello</p>" r

let test_custom_unescape_json_string () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "unescape_json_string") {|hello\nworld\ttab\"quote\\back|} in
  check_ok "unescape" "hello\nworld\ttab\"quote\\back" r

let test_custom_unescape_unknown_escape () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "unescape_json_string") {|hello\xworld|} in
  (* Unknown escapes: backslash kept, next char processed normally *)
  check_ok "unknown escape" "hello\\xworld" r

let test_custom_reverse () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "reverse") "abc" in
  check_ok "reverse" "cba" r

let test_custom_unknown () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "nonexistent") "test" in
  check_error_contains "unknown" "Unknown custom function" r

let test_custom_figma_summary () =
  let input = Yojson.Safe.to_string (`Assoc [
    ("name", `String "Button");
    ("type", `String "FRAME");
    ("children_count", `Int 2);
    ("truncated", `Bool false);
    ("hint", `String "");
    ("children", `List [
      `Assoc [("name", `String "Label"); ("type", `String "TEXT")];
      `Assoc [("name", `String "Icon"); ("type", `String "VECTOR")];
    ]);
  ]) in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "figma_summary_to_spec") input in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     let open Yojson.Safe.Util in
     let name = json |> member "component_name" |> to_string in
     Alcotest.(check string) "component name" "Button" name
   | Error e -> Alcotest.fail e)

let test_custom_figma_summary_truncated () =
  let input = Yojson.Safe.to_string (`Assoc [
    ("name", `String "Page");
    ("type", `String "FRAME");
    ("children_count", `Int 100);
    ("truncated", `Bool true);
    ("hint", `String "Use figma_get_node_chunk");
    ("children", `List []);
  ]) in
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "figma_summary_to_spec") input in
  (match r with
   | Ok v ->
     let json = Yojson.Safe.from_string v in
     let open Yojson.Safe.Util in
     let notes = json |> member "implementation_notes" |> to_list |> List.map to_string in
     Alcotest.(check bool) "has truncation note" true
       (List.exists (fun s ->
          try ignore (Str.search_forward (Str.regexp_string "truncated") s 0); true
          with Not_found -> false) notes)
   | Error e -> Alcotest.fail e)

let test_custom_figma_summary_invalid () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Custom "figma_summary_to_spec") "not json" in
  check_error_contains "invalid figma" "Failed to parse" r

(* ── Conditional: matches with ReDoS pattern ── *)
let test_conditional_matches_redos () =
  let r = Chain_adapter_eio.apply_adapter_transform
    (Conditional { condition = "matches:(a+)+"; on_true = Custom "uppercase"; on_false = Custom "lowercase" })
    "aaa" in
  (* ReDoS pattern rejected -> evaluates to false -> lowercase *)
  check_ok "redos rejected" "aaa" r

let () =
  let open Alcotest in
  run "Chain_adapter_eio" [
    "extract", [
      test_case "simple" `Quick test_extract_simple;
      test_case "nested" `Quick test_extract_nested;
      test_case "missing key" `Quick test_extract_missing_key;
      test_case "invalid json" `Quick test_extract_invalid_json;
      test_case "non-object" `Quick test_extract_non_object;
      test_case "array index" `Quick test_extract_array_index;
      test_case "array oob" `Quick test_extract_array_index_oob;
      test_case "array invalid idx" `Quick test_extract_array_invalid_index;
      test_case "nested array" `Quick test_extract_nested_array;
    ];
    "template", [
      test_case "basic" `Quick test_template_basic;
      test_case "multiple" `Quick test_template_multiple;
    ];
    "summarize", [
      test_case "short passthrough" `Quick test_summarize_short;
      test_case "truncate" `Quick test_summarize_truncate;
      test_case "word boundary" `Quick test_summarize_word_boundary;
    ];
    "truncate", [
      test_case "short passthrough" `Quick test_truncate_short;
      test_case "long truncate" `Quick test_truncate_long;
    ];
    "jsonpath", [
      test_case "dollar dot" `Quick test_jsonpath_dollar_dot;
      test_case "dollar only" `Quick test_jsonpath_dollar_only;
      test_case "no prefix" `Quick test_jsonpath_no_prefix;
      test_case "dollar no dot" `Quick test_jsonpath_dollar_no_dot;
    ];
    "regex", [
      test_case "replace" `Quick test_regex_replace;
      test_case "invalid" `Quick test_regex_invalid;
    ];
    "validate_schema", [
      test_case "string" `Quick test_validate_schema_string;
      test_case "object" `Quick test_validate_schema_object;
      test_case "type mismatch" `Quick test_validate_schema_type_mismatch;
      test_case "required fail" `Quick test_validate_schema_required;
      test_case "required pass" `Quick test_validate_schema_required_pass;
      test_case "enum pass" `Quick test_validate_schema_enum;
      test_case "enum fail" `Quick test_validate_schema_enum_fail;
      test_case "minLength" `Quick test_validate_schema_minlength;
      test_case "maxLength" `Quick test_validate_schema_maxlength;
      test_case "minimum" `Quick test_validate_schema_minimum;
      test_case "maximum" `Quick test_validate_schema_maximum;
      test_case "number pass" `Quick test_validate_schema_number_pass;
      test_case "integer" `Quick test_validate_schema_integer;
      test_case "boolean" `Quick test_validate_schema_boolean;
      test_case "array" `Quick test_validate_schema_array;
      test_case "null" `Quick test_validate_schema_null;
      test_case "invalid json input" `Quick test_validate_schema_invalid_json;
      test_case "int range pass" `Quick test_validate_schema_int_range;
      test_case "int range fail" `Quick test_validate_schema_int_range_fail;
      test_case "float range" `Quick test_validate_schema_float_range;
      test_case "required non-obj" `Quick test_validate_schema_required_non_object;
    ];
    "parsejson", [
      test_case "valid" `Quick test_parsejson_valid;
      test_case "invalid" `Quick test_parsejson_invalid;
    ];
    "stringify", [
      test_case "json passthrough" `Quick test_stringify_json;
      test_case "plain text" `Quick test_stringify_plain;
    ];
    "chain", [
      test_case "sequential" `Quick test_chain_transforms;
      test_case "error stops" `Quick test_chain_error_stops;
      test_case "empty" `Quick test_chain_empty;
    ];
    "conditional", [
      test_case "contains true" `Quick test_conditional_contains_true;
      test_case "contains false" `Quick test_conditional_contains_false;
      test_case "eq" `Quick test_conditional_eq;
      test_case "eq trimmed" `Quick test_conditional_eq_trimmed;
      test_case "neq" `Quick test_conditional_neq;
      test_case "gt true" `Quick test_conditional_gt;
      test_case "gt false" `Quick test_conditional_gt_false;
      test_case "gt non-numeric" `Quick test_conditional_gt_non_numeric;
      test_case "gte" `Quick test_conditional_gte;
      test_case "lt" `Quick test_conditional_lt;
      test_case "lte" `Quick test_conditional_lte;
      test_case "empty" `Quick test_conditional_empty;
      test_case "nonempty" `Quick test_conditional_nonempty;
      test_case "startswith true" `Quick test_conditional_startswith;
      test_case "startswith false" `Quick test_conditional_startswith_false;
      test_case "endswith true" `Quick test_conditional_endswith;
      test_case "endswith false" `Quick test_conditional_endswith_false;
      test_case "matches true" `Quick test_conditional_matches;
      test_case "matches false" `Quick test_conditional_matches_no_match;
      test_case "matches too long" `Quick test_conditional_matches_too_long;
      test_case "matches redos" `Quick test_conditional_matches_redos;
      test_case "legacy contains" `Quick test_conditional_legacy_contains;
      test_case "legacy not found" `Quick test_conditional_legacy_not_found;
    ];
    "split", [
      test_case "line" `Quick test_split_line;
      test_case "paragraph" `Quick test_split_paragraph;
      test_case "sentence" `Quick test_split_sentence;
      test_case "custom delimiter" `Quick test_split_custom_delimiter;
      test_case "chunk merge" `Quick test_split_with_chunk_merge;
      test_case "overlap" `Quick test_split_with_overlap;
    ];
    "custom", [
      test_case "identity" `Quick test_custom_identity;
      test_case "uppercase" `Quick test_custom_uppercase;
      test_case "lowercase" `Quick test_custom_lowercase;
      test_case "trim" `Quick test_custom_trim;
      test_case "extract_json obj" `Quick test_custom_extract_json_object;
      test_case "extract_json arr" `Quick test_custom_extract_json_array;
      test_case "extract_json not found" `Quick test_custom_extract_json_not_found;
      test_case "extract_json unbalanced" `Quick test_custom_extract_json_unbalanced;
      test_case "extract_json nested" `Quick test_custom_extract_json_nested;
      test_case "extract_json with string" `Quick test_custom_extract_json_with_string;
      test_case "extract_json with escape" `Quick test_custom_extract_json_with_escape;
      test_case "extract_html" `Quick test_custom_extract_html;
      test_case "extract_html none" `Quick test_custom_extract_html_no_html;
      test_case "extract_html no close" `Quick test_custom_extract_html_no_closing;
      test_case "extract_html_field json" `Quick test_custom_extract_html_field_json;
      test_case "extract_html_field no html" `Quick test_custom_extract_html_field_no_html;
      test_case "extract_html_field embedded" `Quick test_custom_extract_html_field_embedded_json;
      test_case "unescape" `Quick test_custom_unescape_json_string;
      test_case "unescape unknown" `Quick test_custom_unescape_unknown_escape;
      test_case "reverse" `Quick test_custom_reverse;
      test_case "unknown" `Quick test_custom_unknown;
      test_case "figma summary" `Quick test_custom_figma_summary;
      test_case "figma truncated" `Quick test_custom_figma_summary_truncated;
      test_case "figma invalid" `Quick test_custom_figma_summary_invalid;
    ];
  ]

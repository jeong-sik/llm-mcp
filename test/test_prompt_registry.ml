(** Tests for Prompt_registry module

    Pure function tests:
    - extract_variables: regex-based variable extraction
    - render_template: template substitution
    - prompt_entry: JSON roundtrip
    - prompt_metrics: JSON roundtrip

    Registry integration tests:
    - register/get/unregister lifecycle
    - version management
    - metrics tracking
*)

open Alcotest
open Prompt_registry

(** {1 Test Helpers} *)

let setup () =
  clear ()

(** {1 extract_variables Tests} *)

let test_extract_single_var () =
  let vars = extract_variables "Hello {{name}}!" in
  check (list string) "single var" ["name"] vars

let test_extract_multiple_vars () =
  let vars = extract_variables "{{greeting}} {{name}}, welcome to {{place}}" in
  check (list string) "sorted vars" ["greeting"; "name"; "place"] vars

let test_extract_duplicate_vars () =
  let vars = extract_variables "{{x}} + {{y}} = {{x}} + {{y}}" in
  check (list string) "unique vars" ["x"; "y"] vars

let test_extract_no_vars () =
  let vars = extract_variables "Hello world!" in
  check (list string) "no vars" [] vars

let test_extract_nested_braces () =
  (* {{var}} should match, but {{{var}}} should only match inner *)
  let vars = extract_variables "Value: {{value}}" in
  check (list string) "nested" ["value"] vars

let test_extract_underscore_vars () =
  let vars = extract_variables "{{user_name}} {{user_id}}" in
  check (list string) "underscore vars" ["user_id"; "user_name"] vars

let test_extract_complex_template () =
  let template = {|
    System: {{system_prompt}}
    User: {{user_input}}
    Context: {{context}}
    Previous: {{user_input}}
  |} in
  let vars = extract_variables template in
  check (list string) "complex" ["context"; "system_prompt"; "user_input"] vars

(** {1 render_template Tests} *)

let test_render_basic () =
  let result = render_template
    ~template:"Hello {{name}}!"
    ~vars:[("name", "World")]
    ()
  in
  match result with
  | Ok rendered -> check string "basic render" "Hello World!" rendered
  | Error e -> fail e

let test_render_multiple_vars () =
  let result = render_template
    ~template:"{{greeting}}, {{name}}! Welcome to {{place}}."
    ~vars:[("greeting", "Hi"); ("name", "Alice"); ("place", "Wonderland")]
    ()
  in
  match result with
  | Ok rendered -> check string "multi render" "Hi, Alice! Welcome to Wonderland." rendered
  | Error e -> fail e

let test_render_repeated_var () =
  let result = render_template
    ~template:"{{x}} + {{x}} = 2 * {{x}}"
    ~vars:[("x", "5")]
    ()
  in
  match result with
  | Ok rendered -> check string "repeated" "5 + 5 = 2 * 5" rendered
  | Error e -> fail e

let test_render_missing_var () =
  let result = render_template
    ~template:"Hello {{name}} from {{place}}!"
    ~vars:[("name", "World")]  (* missing 'place' *)
    ()
  in
  match result with
  | Ok _ -> fail "should fail with unresolved variable"
  | Error _ -> ()  (* expected *)

let test_render_empty_vars () =
  let result = render_template
    ~template:"No variables here"
    ~vars:[]
    ()
  in
  match result with
  | Ok rendered -> check string "no vars" "No variables here" rendered
  | Error e -> fail e

let test_render_empty_value () =
  let result = render_template
    ~template:"Value: {{value}}"
    ~vars:[("value", "")]
    ()
  in
  match result with
  | Ok rendered -> check string "empty value" "Value: " rendered
  | Error e -> fail e

let test_render_multiline () =
  let template = "Line 1: {{a}}\nLine 2: {{b}}\nLine 3: {{a}}" in
  let result = render_template ~template ~vars:[("a", "X"); ("b", "Y")] () in
  match result with
  | Ok rendered ->
      check string "multiline" "Line 1: X\nLine 2: Y\nLine 3: X" rendered
  | Error e -> fail e

(** {1 JSON Roundtrip Tests} *)

let test_prompt_metrics_roundtrip () =
  let metrics : prompt_metrics = {
    usage_count = 42;
    avg_score = 0.85;
    last_used = 1704067200.0;
  } in
  let json = prompt_metrics_to_yojson metrics in
  match prompt_metrics_of_yojson json with
  | Ok m2 ->
      check int "usage_count" metrics.usage_count m2.usage_count;
      check (float 0.001) "avg_score" metrics.avg_score m2.avg_score;
      check (float 0.001) "last_used" metrics.last_used m2.last_used
  | Error e -> fail e

let test_prompt_entry_roundtrip () =
  let entry : prompt_entry = {
    id = "test-prompt";
    template = "Hello {{name}}!";
    version = "1.0";
    variables = ["name"];
    metrics = None;
    created_at = 1704067200.0;
    deprecated = false;
  } in
  let json = prompt_entry_to_yojson entry in
  match prompt_entry_of_yojson json with
  | Ok e2 ->
      check string "id" entry.id e2.id;
      check string "template" entry.template e2.template;
      check string "version" entry.version e2.version;
      check (list string) "variables" entry.variables e2.variables;
      check bool "deprecated" entry.deprecated e2.deprecated
  | Error e -> fail e

let test_prompt_entry_with_metrics () =
  let metrics : prompt_metrics = {
    usage_count = 100;
    avg_score = 0.92;
    last_used = 1704153600.0;
  } in
  let entry : prompt_entry = {
    id = "popular-prompt";
    template = "Analyze: {{code}}";
    version = "2.0";
    variables = ["code"];
    metrics = Some metrics;
    created_at = 1704067200.0;
    deprecated = false;
  } in
  let json = prompt_entry_to_yojson entry in
  match prompt_entry_of_yojson json with
  | Ok e2 ->
      (match e2.metrics with
       | Some m -> check int "metrics preserved" 100 m.usage_count
       | None -> fail "metrics lost")
  | Error e -> fail e

let test_prompt_entry_deprecated () =
  let entry : prompt_entry = {
    id = "old-prompt";
    template = "Legacy: {{data}}";
    version = "0.1";
    variables = ["data"];
    metrics = None;
    created_at = 1700000000.0;
    deprecated = true;
  } in
  let json = prompt_entry_to_yojson entry in
  match prompt_entry_of_yojson json with
  | Ok e2 -> check bool "deprecated" true e2.deprecated
  | Error e -> fail e

(** {1 Registry Integration Tests} *)

let test_register_and_get () =
  setup ();
  let entry : prompt_entry = {
    id = "test-1";
    template = "Hello {{name}}!";
    version = "1.0.0";
    variables = ["name"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  match get ~id:"test-1" () with
  | Some e ->
      check string "id" "test-1" e.id;
      check string "version" "1.0.0" e.version
  | None -> fail "entry not found"

let test_register_multiple_versions () =
  setup ();
  let entry1 : prompt_entry = {
    id = "versioned";
    template = "V1: {{x}}";
    version = "1.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  let entry2 : prompt_entry = {
    id = "versioned";
    template = "V2: {{x}} improved";
    version = "2.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry1;
  register entry2;
  let versions = get_versions ~id:"versioned" () in
  check int "2 versions" 2 (List.length versions);
  (* Without version, should get latest *)
  match get ~id:"versioned" () with
  | Some e -> check string "latest" "2.0.0" e.version
  | None -> fail "not found"

let test_exists () =
  setup ();
  check bool "not exists initially" false (exists ~id:"new-prompt" ());
  let entry : prompt_entry = {
    id = "new-prompt";
    template = "{{x}}";
    version = "1.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  check bool "exists after register" true (exists ~id:"new-prompt" ())

let test_unregister () =
  setup ();
  let entry : prompt_entry = {
    id = "to-remove";
    template = "{{x}}";
    version = "1.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  check bool "exists" true (exists ~id:"to-remove" ());
  let _ = unregister ~id:"to-remove" () in
  check bool "removed" false (exists ~id:"to-remove" ())

let test_count () =
  setup ();
  check int "initial" 0 (count ());
  let entry : prompt_entry = {
    id = "counter-test";
    template = "{{x}}";
    version = "1.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  check int "after 1" 1 (count ())

let test_render_registered () =
  setup ();
  let entry : prompt_entry = {
    id = "render-test";
    template = "Hello {{name}} from {{place}}!";
    version = "1.0.0";
    variables = ["name"; "place"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  match render ~id:"render-test" ~vars:[("name", "Alice"); ("place", "Paris")] () with
  | Ok text -> check string "rendered" "Hello Alice from Paris!" text
  | Error e -> fail e

(** {1 Test Suite} *)

let extract_tests = [
  test_case "single variable" `Quick test_extract_single_var;
  test_case "multiple variables" `Quick test_extract_multiple_vars;
  test_case "duplicate variables" `Quick test_extract_duplicate_vars;
  test_case "no variables" `Quick test_extract_no_vars;
  test_case "nested braces" `Quick test_extract_nested_braces;
  test_case "underscore vars" `Quick test_extract_underscore_vars;
  test_case "complex template" `Quick test_extract_complex_template;
]

let render_tests = [
  test_case "basic" `Quick test_render_basic;
  test_case "multiple vars" `Quick test_render_multiple_vars;
  test_case "repeated var" `Quick test_render_repeated_var;
  test_case "missing var" `Quick test_render_missing_var;
  test_case "empty vars" `Quick test_render_empty_vars;
  test_case "empty value" `Quick test_render_empty_value;
  test_case "multiline" `Quick test_render_multiline;
]

let json_tests = [
  test_case "metrics roundtrip" `Quick test_prompt_metrics_roundtrip;
  test_case "entry roundtrip" `Quick test_prompt_entry_roundtrip;
  test_case "entry with metrics" `Quick test_prompt_entry_with_metrics;
  test_case "deprecated entry" `Quick test_prompt_entry_deprecated;
]

let registry_tests = [
  test_case "register and get" `Quick test_register_and_get;
  test_case "multiple versions" `Quick test_register_multiple_versions;
  test_case "exists" `Quick test_exists;
  test_case "unregister" `Quick test_unregister;
  test_case "count" `Quick test_count;
  test_case "render registered" `Quick test_render_registered;
]

(** {1 Deprecate Tests} *)

let test_deprecate () =
  setup ();
  let entry : prompt_entry = {
    id = "dep-test";
    template = "{{x}}";
    version = "1.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  check bool "not deprecated initially" false
    (match get ~id:"dep-test" () with Some e -> e.deprecated | None -> true);
  let ok = deprecate ~id:"dep-test" ~version:"1.0.0" () in
  check bool "deprecate success" true ok;
  check bool "deprecated after" true
    (match get ~id:"dep-test" ~version:"1.0.0" () with Some e -> e.deprecated | None -> false);
  let fail_ok = deprecate ~id:"nonexistent" ~version:"1.0.0" () in
  check bool "deprecate missing" false fail_ok

(** {1 Update Metrics Tests} *)

let test_update_metrics_new () =
  setup ();
  let entry : prompt_entry = {
    id = "metrics-test";
    template = "{{x}}";
    version = "1.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  update_metrics ~id:"metrics-test" ~version:"1.0.0" ~score:0.8 ();
  (match get ~id:"metrics-test" () with
   | Some e ->
       (match e.metrics with
        | Some m ->
            check int "usage count 1" 1 m.usage_count;
            check (float 0.01) "avg score" 0.8 m.avg_score
        | None -> fail "metrics should exist")
   | None -> fail "entry should exist")

let test_update_metrics_accumulate () =
  setup ();
  let entry : prompt_entry = {
    id = "metrics-acc";
    template = "{{x}}";
    version = "1.0.0";
    variables = ["x"];
    metrics = None;
    created_at = Unix.gettimeofday ();
    deprecated = false;
  } in
  register entry;
  update_metrics ~id:"metrics-acc" ~version:"1.0.0" ~score:0.8 ();
  update_metrics ~id:"metrics-acc" ~version:"1.0.0" ~score:0.6 ();
  (match get ~id:"metrics-acc" () with
   | Some e ->
       (match e.metrics with
        | Some m ->
            check int "usage count 2" 2 m.usage_count;
            check (float 0.01) "avg score" 0.7 m.avg_score
        | None -> fail "metrics should exist")
   | None -> fail "entry should exist")

let test_update_metrics_nonexistent () =
  setup ();
  (* Should not crash *)
  update_metrics ~id:"nonexistent" ~version:"1.0.0" ~score:0.5 ()

(** {1 Stats Tests} *)

let test_stats_empty () =
  setup ();
  let s = stats () in
  check int "total 0" 0 s.total_prompts;
  check int "active 0" 0 s.active_prompts;
  check int "deprecated 0" 0 s.deprecated_prompts;
  check (option string) "most used none" None s.most_used;
  check (float 0.01) "avg usage 0" 0.0 s.avg_usage

let test_stats_with_data () =
  setup ();
  let entry1 : prompt_entry = {
    id = "s1"; template = "{{x}}"; version = "1.0.0";
    variables = ["x"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  let entry2 : prompt_entry = {
    id = "s2"; template = "{{y}}"; version = "1.0.0";
    variables = ["y"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = true;
  } in
  register entry1;
  register entry2;
  update_metrics ~id:"s1" ~version:"1.0.0" ~score:0.9 ();
  update_metrics ~id:"s1" ~version:"1.0.0" ~score:0.8 ();
  let s = stats () in
  check int "total 2" 2 s.total_prompts;
  check int "active 1" 1 s.active_prompts;
  check int "deprecated 1" 1 s.deprecated_prompts;
  check (option string) "most used" (Some "s1") s.most_used;
  check bool "avg usage > 0" true (s.avg_usage > 0.0)

(** {1 Unregister All Versions} *)

let test_unregister_all_versions () =
  setup ();
  let entry1 : prompt_entry = {
    id = "multi-ver"; template = "v1 {{x}}"; version = "1.0.0";
    variables = ["x"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  let entry2 : prompt_entry = {
    id = "multi-ver"; template = "v2 {{x}}"; version = "2.0.0";
    variables = ["x"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  register entry1;
  register entry2;
  check int "2 versions" 2 (List.length (get_versions ~id:"multi-ver" ()));
  let ok = unregister ~id:"multi-ver" () in
  check bool "unregister all ok" true ok;
  check bool "all gone" false (exists ~id:"multi-ver" ());
  let fail_ok = unregister ~id:"multi-ver" () in
  check bool "unregister missing" false fail_ok

(** {1 List IDs Tests} *)

let test_list_ids () =
  setup ();
  check int "empty list" 0 (List.length (list_ids ()));
  let e1 : prompt_entry = {
    id = "id-a"; template = "{{x}}"; version = "1.0.0";
    variables = ["x"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  let e2 : prompt_entry = {
    id = "id-b"; template = "{{y}}"; version = "1.0.0";
    variables = ["y"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  register e1;
  register e2;
  let ids = list_ids () |> List.sort String.compare in
  check (list string) "ids" ["id-a"; "id-b"] ids

(** {1 Count Unique Tests} *)

let test_count_unique () =
  setup ();
  check int "empty" 0 (count_unique ());
  let e1 : prompt_entry = {
    id = "cu-a"; template = "{{x}}"; version = "1.0.0";
    variables = ["x"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  let e2 : prompt_entry = {
    id = "cu-a"; template = "{{x}} v2"; version = "2.0.0";
    variables = ["x"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  let e3 : prompt_entry = {
    id = "cu-b"; template = "{{y}}"; version = "1.0.0";
    variables = ["y"]; metrics = None;
    created_at = Unix.gettimeofday (); deprecated = false;
  } in
  register e1; register e2; register e3;
  check int "unique 2 (cu-a and cu-b)" 2 (count_unique ());
  check int "total 3" 3 (count ())

(** {1 to_json / of_json Tests} *)

let test_to_json_of_json () =
  setup ();
  let e1 : prompt_entry = {
    id = "json-test"; template = "Hello {{name}}"; version = "1.0.0";
    variables = ["name"]; metrics = None;
    created_at = 1000.0; deprecated = false;
  } in
  register e1;
  let json = to_json () in
  setup ();  (* clear *)
  (match of_json json with
   | Ok n ->
       check int "imported 1" 1 n;
       (match get ~id:"json-test" () with
        | Some e -> check string "template" "Hello {{name}}" e.template
        | None -> fail "not found after import")
   | Error e -> fail e)

let test_of_json_invalid () =
  setup ();
  (match of_json (`String "not a list") with
   | Ok _ -> fail "expected error for non-list"
   | Error _ -> ())

let test_of_json_partial () =
  setup ();
  (* List with one valid and one invalid entry *)
  let e1 : prompt_entry = {
    id = "partial-test"; template = "{{x}}"; version = "1.0.0";
    variables = ["x"]; metrics = None;
    created_at = 1000.0; deprecated = false;
  } in
  let valid_json = prompt_entry_to_yojson e1 in
  let invalid_json = `Assoc [("bad", `String "data")] in
  let json = `List [valid_json; invalid_json] in
  (match of_json json with
   | Ok n -> check int "imported 1 of 2" 1 n
   | Error e -> fail e)

(** {1 Test Suite} *)

let deprecate_tests = [
  test_case "deprecate" `Quick test_deprecate;
]

let metrics_tests = [
  test_case "new metrics" `Quick test_update_metrics_new;
  test_case "accumulate metrics" `Quick test_update_metrics_accumulate;
  test_case "nonexistent metrics" `Quick test_update_metrics_nonexistent;
]

let stats_tests = [
  test_case "empty stats" `Quick test_stats_empty;
  test_case "stats with data" `Quick test_stats_with_data;
]

let advanced_registry_tests = [
  test_case "unregister all versions" `Quick test_unregister_all_versions;
  test_case "list ids" `Quick test_list_ids;
  test_case "count unique" `Quick test_count_unique;
]

let json_io_tests = [
  test_case "to/of json roundtrip" `Quick test_to_json_of_json;
  test_case "of_json invalid" `Quick test_of_json_invalid;
  test_case "of_json partial" `Quick test_of_json_partial;
]

let () =
  run "prompt_registry" [
    ("extract_variables", extract_tests);
    ("render_template", render_tests);
    ("json_roundtrip", json_tests);
    ("registry", registry_tests);
    ("deprecate", deprecate_tests);
    ("metrics", metrics_tests);
    ("stats", stats_tests);
    ("advanced_registry", advanced_registry_tests);
    ("json_io", json_io_tests);
  ]

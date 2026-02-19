(** Extended tests for Handler_gemini.parse_models_response

    Coverage targets (internal functions exercised through parse_models_response):
    - starts_with / strip_prefix: via "models/" prefix stripping
    - string_contains_ci: via filter matching (case insensitive)
    - parse_methods: supportedGenerationMethods extraction
    - to_opt_string / to_opt_int: optional field extraction
    - filter + include_all combinations
    - Edge cases: missing fields, malformed JSON, empty models list
*)

open Alcotest

let ids (models : Yojson.Safe.t list) : string list =
  let open Yojson.Safe.Util in
  List.filter_map (fun m ->
    try Some (m |> member "id" |> to_string) with _ -> None
  ) models

let get_field (m : Yojson.Safe.t) (field : string) : Yojson.Safe.t =
  Yojson.Safe.Util.member field m

(* ── Basic parsing ──────────────────────────────────────── *)

let sample_full = Yojson.Safe.from_string {|
{
  "models": [
    {
      "name": "models/gemini-2.5-pro",
      "displayName": "Gemini 2.5 Pro",
      "description": "Advanced reasoning model",
      "inputTokenLimit": 1048576,
      "outputTokenLimit": 8192,
      "supportedGenerationMethods": ["generateContent", "countTokens"]
    },
    {
      "name": "models/gemini-2.5-flash",
      "displayName": "Gemini 2.5 Flash",
      "description": "Fast model",
      "inputTokenLimit": 1048576,
      "outputTokenLimit": 65536,
      "supportedGenerationMethods": ["generateContent"]
    },
    {
      "name": "models/text-embedding-004",
      "displayName": "Text Embedding",
      "description": "Embedding model",
      "inputTokenLimit": 2048,
      "outputTokenLimit": 1,
      "supportedGenerationMethods": ["embedContent"]
    }
  ]
}
|}

let test_parse_all_generate () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_full in
  check int "2 generate models" 2 (List.length models);
  check (list string) "ids" ["gemini-2.5-pro"; "gemini-2.5-flash"] (ids models)

let test_parse_include_all_with_embedding () =
  let models = Handler_gemini.parse_models_response ~include_all:true ~filter:None sample_full in
  check int "3 total models" 3 (List.length models);
  check (list string) "includes embedding" ["gemini-2.5-pro"; "gemini-2.5-flash"; "text-embedding-004"] (ids models)

let test_parse_filter_pro () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:(Some "pro") sample_full in
  check (list string) "only pro" ["gemini-2.5-pro"] (ids models)

let test_parse_filter_case_insensitive () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:(Some "FLASH") sample_full in
  check (list string) "flash" ["gemini-2.5-flash"] (ids models)

let test_parse_filter_no_match () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:(Some "nonexistent") sample_full in
  check int "no match" 0 (List.length models)

(* ── Field extraction ───────────────────────────────────── *)

let test_parse_fields_complete () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:(Some "2.5-pro") sample_full in
  match models with
  | [m] ->
    let open Yojson.Safe.Util in
    check string "id" "gemini-2.5-pro" (m |> member "id" |> to_string);
    check string "full_name" "models/gemini-2.5-pro" (m |> member "full_name" |> to_string);
    check string "display_name" "Gemini 2.5 Pro" (m |> member "display_name" |> to_string);
    check string "description" "Advanced reasoning model" (m |> member "description" |> to_string);
    check int "input_token_limit" 1048576 (m |> member "input_token_limit" |> to_int);
    check int "output_token_limit" 8192 (m |> member "output_token_limit" |> to_int);
    check bool "supports_generate_content" true (m |> member "supports_generate_content" |> to_bool);
    (match m |> member "supported_generation_methods" with
     | `List methods ->
       let method_strs = List.map to_string methods in
       check bool "has generateContent" true (List.mem "generateContent" method_strs);
       check bool "has countTokens" true (List.mem "countTokens" method_strs)
     | _ -> fail "expected list of methods")
  | _ -> fail "expected exactly 1 model"

(* ── Edge cases: missing/null fields ────────────────────── *)

let sample_missing_fields = Yojson.Safe.from_string {|
{
  "models": [
    {
      "name": "models/minimal-model",
      "supportedGenerationMethods": ["generateContent"]
    }
  ]
}
|}

let test_parse_missing_optional_fields () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_missing_fields in
  match models with
  | [m] ->
    check string "id" "minimal-model" (Yojson.Safe.Util.(m |> member "id" |> to_string));
    (* Optional fields should be null *)
    check bool "display_name null" true (get_field m "display_name" = `Null);
    check bool "description null" true (get_field m "description" = `Null);
    check bool "input_token_limit null" true (get_field m "input_token_limit" = `Null);
    check bool "output_token_limit null" true (get_field m "output_token_limit" = `Null)
  | _ -> fail "expected 1 model"

(* ── Edge case: no "name" field ─────────────────────────── *)

let sample_no_name = Yojson.Safe.from_string {|
{
  "models": [
    {
      "displayName": "Nameless",
      "supportedGenerationMethods": ["generateContent"]
    }
  ]
}
|}

let test_parse_no_name_skipped () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_no_name in
  check int "skipped" 0 (List.length models)

(* ── Edge case: empty models list ───────────────────────── *)

let sample_empty = Yojson.Safe.from_string {|{ "models": [] }|}

let test_parse_empty_models () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_empty in
  check int "empty" 0 (List.length models)

(* ── Edge case: no "models" key ─────────────────────────── *)

let sample_no_models = Yojson.Safe.from_string {|{ "error": "not found" }|}

let test_parse_no_models_key () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_no_models in
  check int "no models key" 0 (List.length models)

(* ── Edge case: models not a list ───────────────────────── *)

let sample_models_string = Yojson.Safe.from_string {|{ "models": "not a list" }|}

let test_parse_models_not_list () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_models_string in
  check int "not a list" 0 (List.length models)

(* ── Edge case: malformed supportedGenerationMethods ────── *)

let sample_bad_methods = Yojson.Safe.from_string {|
{
  "models": [
    {
      "name": "models/test-model",
      "supportedGenerationMethods": "not a list"
    }
  ]
}
|}

let test_parse_bad_methods () =
  (* Methods not a list → empty methods → include_all=false should skip (no generateContent) *)
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_bad_methods in
  check int "skipped (no generate)" 0 (List.length models);
  (* include_all=true should include *)
  let models_all = Handler_gemini.parse_models_response ~include_all:true ~filter:None sample_bad_methods in
  check int "included with all" 1 (List.length models_all)

(* ── Edge case: methods list with non-string values ─────── *)

let sample_mixed_methods = Yojson.Safe.from_string {|
{
  "models": [
    {
      "name": "models/mixed-model",
      "supportedGenerationMethods": ["generateContent", 42, null, "countTokens"]
    }
  ]
}
|}

let test_parse_mixed_methods () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_mixed_methods in
  match models with
  | [m] ->
    (match get_field m "supported_generation_methods" with
     | `List methods ->
       (* Non-string values should be filtered out *)
       let strs = List.filter_map (fun x ->
         match x with `String s -> Some s | _ -> None
       ) methods in
       check bool "has generateContent" true (List.mem "generateContent" strs);
       check bool "has countTokens" true (List.mem "countTokens" strs)
     | _ -> fail "expected list")
  | _ -> fail "expected 1 model"

(* ── Edge case: name without models/ prefix ─────────────── *)

let sample_no_prefix = Yojson.Safe.from_string {|
{
  "models": [
    {
      "name": "bare-model-name",
      "supportedGenerationMethods": ["generateContent"]
    }
  ]
}
|}

let test_parse_no_prefix () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_no_prefix in
  match models with
  | [m] ->
    check string "id without prefix" "bare-model-name" (Yojson.Safe.Util.(m |> member "id" |> to_string))
  | _ -> fail "expected 1 model"

(* ── Filter with include_all combinations ───────────────── *)

let test_filter_with_include_all () =
  (* Filter "embedding" with include_all=true should find the embedding model *)
  let models = Handler_gemini.parse_models_response ~include_all:true ~filter:(Some "embedding") sample_full in
  check (list string) "embedding model" ["text-embedding-004"] (ids models)

let test_filter_with_include_all_false () =
  (* Filter "embedding" with include_all=false should NOT find embedding (no generateContent) *)
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:(Some "embedding") sample_full in
  check int "no generate embedding" 0 (List.length models)

(* ── Large model list ───────────────────────────────────── *)

let sample_many = Yojson.Safe.from_string {|
{
  "models": [
    {"name": "models/a", "supportedGenerationMethods": ["generateContent"]},
    {"name": "models/b", "supportedGenerationMethods": ["generateContent"]},
    {"name": "models/c", "supportedGenerationMethods": ["embedContent"]},
    {"name": "models/d", "supportedGenerationMethods": ["generateContent", "embedContent"]},
    {"name": "models/e", "supportedGenerationMethods": []}
  ]
}
|}

let test_parse_many_models () =
  let gen_only = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample_many in
  check (list string) "generate models" ["a"; "b"; "d"] (ids gen_only);
  let all = Handler_gemini.parse_models_response ~include_all:true ~filter:None sample_many in
  check (list string) "all models" ["a"; "b"; "c"; "d"; "e"] (ids all)

(* ── Test suite ─────────────────────────────────────────── *)

(* ── NEW: tests for aliases_to_json and static_models_to_json ─── *)
(* These require the updated .mli that exports them *)

let test_aliases_to_json_structure () =
  let json = Handler_gemini.aliases_to_json () in
  match json with
  | `Assoc fields ->
    check bool "non-empty" true (List.length fields > 0);
    (* Every alias should map to a string *)
    List.iter (fun (key, value) ->
      (match value with
       | `String _ -> ()
       | _ -> fail (Printf.sprintf "alias %s should map to string" key))
    ) fields
  | _ -> fail "expected object"

let test_aliases_to_json_known_alias () =
  let json = Handler_gemini.aliases_to_json () in
  match json with
  | `Assoc fields ->
    (* "gemini" should be a known alias *)
    (match List.assoc_opt "gemini" fields with
     | Some (`String _) -> ()
     | _ -> fail "expected gemini alias");
    (* "pro" should be a known alias *)
    (match List.assoc_opt "pro" fields with
     | Some (`String _) -> ()
     | _ -> fail "expected pro alias")
  | _ -> fail "expected object"

let test_aliases_matches_constant () =
  let json = Handler_gemini.aliases_to_json () in
  match json with
  | `Assoc fields ->
    let alias_count = List.length fields in
    let constant_count = List.length Handler_gemini.gemini_model_aliases in
    check int "same count" constant_count alias_count
  | _ -> fail "expected object"

let test_static_models_no_filter () =
  let models = Handler_gemini.static_models_to_json ~filter:None in
  check bool "non-empty" true (List.length models > 0);
  (* Each model should have "id" and "source" fields *)
  List.iter (fun m ->
    match m with
    | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      (match List.assoc "source" fields with
       | `String "static" -> ()
       | _ -> fail "expected source=static")
    | _ -> fail "expected object"
  ) models

let test_static_models_unique () =
  let models = Handler_gemini.static_models_to_json ~filter:None in
  let ids = List.filter_map (fun m ->
    match m with
    | `Assoc fields ->
      (match List.assoc_opt "id" fields with
       | Some (`String s) -> Some s
       | _ -> None)
    | _ -> None
  ) models in
  let unique_ids = List.sort_uniq String.compare ids in
  check int "all unique" (List.length ids) (List.length unique_ids)

let test_static_models_with_filter () =
  let models = Handler_gemini.static_models_to_json ~filter:(Some "pro") in
  check bool "has results" true (List.length models > 0);
  List.iter (fun m ->
    match m with
    | `Assoc fields ->
      (match List.assoc_opt "id" fields with
       | Some (`String id) ->
         let lower_id = String.lowercase_ascii id in
         check bool "contains pro" true
           (try let _ = Str.search_forward (Str.regexp_string "pro") lower_id 0 in true
            with Not_found -> false)
       | _ -> fail "expected string id")
    | _ -> fail "expected object"
  ) models

let test_static_models_filter_no_match () =
  let models = Handler_gemini.static_models_to_json ~filter:(Some "zzz_nonexistent") in
  check int "no matches" 0 (List.length models)

let test_static_models_filter_case_insensitive () =
  let lower = Handler_gemini.static_models_to_json ~filter:(Some "flash") in
  let upper = Handler_gemini.static_models_to_json ~filter:(Some "FLASH") in
  check int "same results" (List.length lower) (List.length upper)

let test_gemini_model_aliases_non_empty () =
  check bool "non-empty" true (List.length Handler_gemini.gemini_model_aliases > 0)

let test_gemini_model_aliases_all_pairs () =
  List.iter (fun (alias, target) ->
    check bool (Printf.sprintf "alias %s non-empty" alias) true (String.length alias > 0);
    check bool (Printf.sprintf "target for %s non-empty" alias) true (String.length target > 0)
  ) Handler_gemini.gemini_model_aliases

(* ── Test suite ─────────────────────────────────────────── *)

let () =
  run "handler_gemini_coverage" [
    ("parse_basic", [
      test_case "generate only" `Quick test_parse_all_generate;
      test_case "include all with embedding" `Quick test_parse_include_all_with_embedding;
      test_case "filter pro" `Quick test_parse_filter_pro;
      test_case "filter case insensitive" `Quick test_parse_filter_case_insensitive;
      test_case "filter no match" `Quick test_parse_filter_no_match;
    ]);
    ("field_extraction", [
      test_case "complete fields" `Quick test_parse_fields_complete;
      test_case "missing optional fields" `Quick test_parse_missing_optional_fields;
    ]);
    ("edge_cases", [
      test_case "no name field" `Quick test_parse_no_name_skipped;
      test_case "empty models" `Quick test_parse_empty_models;
      test_case "no models key" `Quick test_parse_no_models_key;
      test_case "models not list" `Quick test_parse_models_not_list;
      test_case "bad methods" `Quick test_parse_bad_methods;
      test_case "mixed methods" `Quick test_parse_mixed_methods;
      test_case "no models/ prefix" `Quick test_parse_no_prefix;
    ]);
    ("filter_combinations", [
      test_case "filter + include_all=true" `Quick test_filter_with_include_all;
      test_case "filter + include_all=false" `Quick test_filter_with_include_all_false;
      test_case "many models" `Quick test_parse_many_models;
    ]);
    ("aliases_to_json", [
      test_case "structure" `Quick test_aliases_to_json_structure;
      test_case "known alias" `Quick test_aliases_to_json_known_alias;
      test_case "matches constant" `Quick test_aliases_matches_constant;
    ]);
    ("static_models", [
      test_case "no filter" `Quick test_static_models_no_filter;
      test_case "unique ids" `Quick test_static_models_unique;
      test_case "with filter" `Quick test_static_models_with_filter;
      test_case "no match" `Quick test_static_models_filter_no_match;
      test_case "case insensitive" `Quick test_static_models_filter_case_insensitive;
    ]);
    ("gemini_model_aliases", [
      test_case "non-empty" `Quick test_gemini_model_aliases_non_empty;
      test_case "all valid pairs" `Quick test_gemini_model_aliases_all_pairs;
    ]);
  ]

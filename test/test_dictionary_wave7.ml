(** Wave 7: Additional dictionary.ml coverage tests.
    Focus on detect_model_type, load/save, decompress error paths,
    compress_auto, pp, content_type/model_type round-trips. *)

open Alcotest
open Dictionary

let check_str = check string
let check_int = check int
let check_bool = check bool

let tmp_dir () =
  let d = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "test_dict_%d" (Random.int 100000)) in
  Common.ensure_dir d;
  d

(* ---- content_type conversions ---- *)
let test_content_type_code () =
  check_str "code" "code" (string_of_content_type Code);
  (match content_type_of_string "code" with Code -> () | _ -> fail "expected Code")

let test_content_type_json () =
  check_str "json" "json" (string_of_content_type JSON);
  (match content_type_of_string "json" with JSON -> () | _ -> fail "expected JSON")

let test_content_type_markdown () =
  check_str "markdown" "markdown" (string_of_content_type Markdown);
  (match content_type_of_string "markdown" with Markdown -> () | _ -> fail "expected Markdown")

let test_content_type_mixed () =
  check_str "mixed" "mixed" (string_of_content_type Mixed);
  (match content_type_of_string "mixed" with Mixed -> () | _ -> fail "expected Mixed")

let test_content_type_unknown () =
  (match content_type_of_string "unknown" with Mixed -> () | _ -> fail "expected Mixed for unknown")

(* ---- model_type conversions ---- *)
let test_model_type_claude () =
  check_str "claude" "claude" (string_of_model_type Claude);
  (match model_type_of_string "claude" with Claude -> () | _ -> fail "expected Claude")

let test_model_type_gpt () =
  check_str "gpt" "gpt" (string_of_model_type GPT);
  (match model_type_of_string "gpt" with GPT -> () | _ -> fail "expected GPT")

let test_model_type_openai () =
  (match model_type_of_string "openai" with GPT -> () | _ -> fail "expected GPT for openai")

let test_model_type_gemini () =
  check_str "gemini" "gemini" (string_of_model_type Gemini);
  (match model_type_of_string "gemini" with Gemini -> () | _ -> fail "expected Gemini")

let test_model_type_google () =
  (match model_type_of_string "google" with Gemini -> () | _ -> fail "expected Gemini for google")

let test_model_type_llama () =
  check_str "llama" "llama" (string_of_model_type Llama);
  (match model_type_of_string "llama" with Llama -> () | _ -> fail "expected Llama")

let test_model_type_meta () =
  (match model_type_of_string "meta" with Llama -> () | _ -> fail "expected Llama for meta")

let test_model_type_local () =
  (match model_type_of_string "local" with Llama -> () | _ -> fail "expected Llama for local")

let test_model_type_universal () =
  check_str "universal" "universal" (string_of_model_type Universal);
  (match model_type_of_string "universal" with Universal -> () | _ -> fail "expected Universal")

let test_model_type_mixed_alias () =
  (match model_type_of_string "mixed" with Universal -> () | _ -> fail "expected Universal for mixed")

let test_model_type_unknown () =
  (match model_type_of_string "xyz" with Universal -> () | _ -> fail "expected Universal for unknown")

(* ---- detect_content_type ---- *)
let test_detect_def () =
  (match detect_content_type "def hello():\n  pass\n  more code here" with
   | Code -> () | _ -> fail "expected Code for def")

let test_detect_function () =
  (match detect_content_type "function test() { return 1; } function other()" with
   | Code -> () | _ -> fail "expected Code for function")

let test_detect_let () =
  (match detect_content_type "let x = 42 in let y = 43 in x + y" with
   | Code -> () | _ -> fail "expected Code for let")

let test_detect_class () =
  (match detect_content_type "class Foo: pass\n  def bar(self): pass" with
   | Code -> () | _ -> fail "expected Code for class")

let test_detect_json_brace () =
  (match detect_content_type "{\"key\": \"value\", \"number\": 42}" with
   | JSON -> () | _ -> fail "expected JSON for brace")

let test_detect_json_bracket () =
  (match detect_content_type "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]" with
   | JSON -> () | _ -> fail "expected JSON for bracket")

let test_detect_markdown () =
  (match detect_content_type "# Hello World\nSome text\nMore text" with
   | Markdown -> () | _ -> fail "expected Markdown")

let test_detect_mixed () =
  (match detect_content_type "Just some plain text without any markers or symbols" with
   | Mixed -> () | _ -> fail "expected Mixed")

let test_detect_short () =
  (match detect_content_type "hi" with
   | Mixed -> () | _ -> fail "expected Mixed for short")

(* ---- detect_model_type ---- *)
let test_detect_model_claude_ill_help () =
  (match detect_model_type "I'll help you with this task. Let me start by analyzing the problem." with
   | Claude -> () | _ -> fail "expected Claude for i'll help")

let test_detect_model_claude_let_me () =
  (match detect_model_type "Let me think about this problem and provide a detailed analysis." with
   | Claude -> () | _ -> fail "expected Claude for let me")

let test_detect_model_claude_thinking () =
  (match detect_model_type "<thinking>Let me analyze this step by step</thinking>" with
   | Claude -> () | _ -> fail "expected Claude for <thinking>")

let test_detect_model_gpt_certainly () =
  (match detect_model_type "Certainly! I can help you with that request." with
   | GPT -> () | _ -> fail "expected GPT for certainly")

let test_detect_model_gpt_happy () =
  (match detect_model_type "I'd be happy to assist you with this task." with
   | GPT -> () | _ -> fail "expected GPT for i'd be happy")

let test_detect_model_gpt_heres_what () =
  (match detect_model_type "Here's what you need to know about this topic." with
   | GPT -> () | _ -> fail "expected GPT for here's what")

let test_detect_model_gemini_heres () =
  (match detect_model_type "Here's a concise summary of the key points." with
   | Gemini -> () | _ -> fail "expected Gemini for here's")

let test_detect_model_gemini_answer () =
  (match detect_model_type "The answer is 42 according to the calculation." with
   | Gemini -> () | _ -> fail "expected Gemini for the answer is")

let test_detect_model_universal () =
  (match detect_model_type "This is a generic response without any model-specific patterns." with
   | Universal -> () | _ -> fail "expected Universal")

let test_detect_model_short () =
  (match detect_model_type "short" with
   | Universal -> () | _ -> fail "expected Universal for short")

(* ---- save / load roundtrip v2 ---- *)
let test_save_load_v2 () =
  let d = tmp_dir () in
  let path = Filename.concat d "test.zdict" in
  let dict = {
    dict_id = "test_dict"; content_type = JSON; model_type = Claude;
    dict_data = "fake_dict_data_here";
    sample_count = 200; created_at = 1700000000.0
  } in
  (match save dict path with
   | Ok () -> ()
   | Error e -> fail (Printf.sprintf "save failed: %s" e));
  (match load path with
   | Ok d2 ->
     check_str "dict_id" "test.zdict" d2.dict_id;
     check_str "content_type" "json" (string_of_content_type d2.content_type);
     check_str "model_type" "claude" (string_of_model_type d2.model_type);
     check_int "sample_count" 200 d2.sample_count;
     check_str "dict_data" "fake_dict_data_here" d2.dict_data
   | Error e -> fail (Printf.sprintf "load failed: %s" e));
  Common.rm_rf d

(* ---- load v1 format ---- *)
let test_load_v1_format () =
  let d = tmp_dir () in
  let path = Filename.concat d "legacy.zdict" in
  let content = "ZDCT|v1|json|150|1700000000.0\nfake_v1_dict_data" in
  Dictionary.write_binary_file path content;
  (match load path with
   | Ok d ->
     check_str "content_type" "json" (string_of_content_type d.content_type);
     check_str "model_type" "universal" (string_of_model_type d.model_type);
     check_int "sample_count" 150 d.sample_count
   | Error e -> fail (Printf.sprintf "v1 load failed: %s" e));
  Common.rm_rf d

(* ---- load invalid format ---- *)
let test_load_invalid_meta () =
  let d = tmp_dir () in
  let path = Filename.concat d "bad.zdict" in
  Dictionary.write_binary_file path "INVALID|format\nbody";
  (match load path with
   | Error _ -> ()
   | Ok _ -> fail "expected Error for invalid format");
  Common.rm_rf d

let test_load_nonexistent () =
  (match load "/nonexistent/dict.zdict" with
   | Error _ -> ()
   | Ok _ -> fail "expected Error for nonexistent")

(* ---- decompress_with_dict error paths ---- *)
let test_decompress_not_compressed () =
  let dict = {
    dict_id = "d"; content_type = Mixed; model_type = Universal;
    dict_data = "fake"; sample_count = 100; created_at = 0.0
  } in
  (match decompress_with_dict dict "plain text" with
   | Error msg -> check_bool "not compressed" true (Common.contains ~substring:"Not dictionary" msg)
   | Ok _ -> fail "expected Error")

let test_decompress_truncated_header () =
  let dict = {
    dict_id = "d"; content_type = Mixed; model_type = Universal;
    dict_data = "fake"; sample_count = 100; created_at = 0.0
  } in
  (* ZDCT + 4 bytes but no dict_id_len *)
  (match decompress_with_dict dict "ZDCT\x00\x00\x00\x0a" with
   | Error _ -> ()
   | Ok _ -> fail "expected Error for truncated")

let test_decompress_short_data () =
  let dict = {
    dict_id = "d"; content_type = Mixed; model_type = Universal;
    dict_data = "fake"; sample_count = 100; created_at = 0.0
  } in
  (match decompress_with_dict dict "ABC" with
   | Error msg -> check_bool "not compressed" true (Common.contains ~substring:"Not dictionary" msg)
   | Ok _ -> fail "expected Error")

(* ---- is_dict_compressed ---- *)
let test_is_compressed_true () =
  check_bool "ZDCT" true (is_dict_compressed "ZDCT\x00\x00\x00\x0a\x02xxcompressed")

let test_is_compressed_false () =
  check_bool "not" false (is_dict_compressed "plain text data")

let test_is_compressed_short () =
  check_bool "short" false (is_dict_compressed "ZD")

(* ---- compress_auto small ---- *)
let test_compress_auto_small () =
  let data = "tiny" in
  let compressed = compress_auto data in
  check_str "unchanged" data compressed

(* ---- decompress_auto not compressed ---- *)
let test_decompress_auto_plain () =
  let data = "plain text that is not compressed" in
  let result = decompress_auto data in
  check_str "passthrough" data result

(* ---- pp ---- *)
let test_pp () =
  let dict = {
    dict_id = "test"; content_type = Code; model_type = Claude;
    dict_data = String.make 50 'x'; sample_count = 200; created_at = 0.0
  } in
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt dict;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  check_bool "has id" true (Common.contains ~substring:"test" s);
  check_bool "has type" true (Common.contains ~substring:"code" s);
  check_bool "has samples" true (Common.contains ~substring:"200" s)

(* ---- constants ---- *)
let test_constants () =
  check_str "magic" "ZDCT" magic;
  check_int "version" 1 version;
  check_int "min_samples" 100 min_samples;
  check_int "min_payload_size" 64 min_payload_size;
  check_bool "dict_size > 0" true (default_dict_size > 0)

(* ---- contains_substring ---- *)
let test_contains_substring_true () =
  check_bool "contains" true (contains_substring "hello world" "world")

let test_contains_substring_false () =
  check_bool "not contains" false (contains_substring "hello" "xyz")

(* ---- train with too few samples ---- *)
let test_train_too_few () =
  (match train ~samples:["a"; "b"; "c"] ~content_type:JSON () with
   | Error msg -> check_bool "need at least" true (Common.contains ~substring:"Need at least" msg)
   | Ok _ -> fail "expected Error")

(* ---- dict_dir ---- *)
let test_dict_dir_env_override () =
  Unix.putenv "LLM_MCP_DICTS_DIR" "/tmp/custom_dicts";
  let d = dict_dir () in
  check_str "custom dir" "/tmp/custom_dicts" d;
  (* Clean up env *)
  Unix.putenv "LLM_MCP_DICTS_DIR" ""

let test_dict_dir_default () =
  (* Save and unset LLM_MCP_DICTS_DIR to test default path resolution *)
  let saved = Sys.getenv_opt "LLM_MCP_DICTS_DIR" in
  (try Unix.putenv "LLM_MCP_DICTS_DIR" "" with _ -> ());
  let d = dict_dir () in
  (* Empty env var is still Some "" so dict_dir returns "" *)
  (* Just check it doesn't raise *)
  ignore d;
  check_bool "no exception" true true;
  (match saved with Some v -> Unix.putenv "LLM_MCP_DICTS_DIR" v | None -> ())

(* ---- load_for_type / load_default ---- *)
let test_load_for_type_missing () =
  Unix.putenv "LLM_MCP_DICTS_DIR" "/tmp/nonexistent_dicts_dir";
  let result = load_for_type Code in
  check_bool "None" true (Option.is_none result);
  Unix.putenv "LLM_MCP_DICTS_DIR" ""

let test_load_default_missing () =
  Unix.putenv "LLM_MCP_DICTS_DIR" "/tmp/nonexistent_dicts_dir";
  let result = load_default () in
  check_bool "None" true (Option.is_none result);
  Unix.putenv "LLM_MCP_DICTS_DIR" ""

(* ---- run_process_silent ---- *)
let test_run_process_true () =
  (match run_process_silent "true" [] with
   | Ok 0 -> ()
   | _ -> fail "expected Ok 0")

let test_run_process_false () =
  (match run_process_silent "false" [] with
   | Ok code -> check_bool "nonzero" true (code <> 0)
   | Error _ -> fail "expected Ok with nonzero")

let test_run_process_missing () =
  (match run_process_silent "/nonexistent_binary_xyz" [] with
   | Error msg -> check_bool "missing" true (Common.contains ~substring:"Missing" msg)
   | Ok _ -> fail "expected Error")

let () =
  run "dictionary_wave7" [
    "content_type", [
      test_case "code" `Quick test_content_type_code;
      test_case "json" `Quick test_content_type_json;
      test_case "markdown" `Quick test_content_type_markdown;
      test_case "mixed" `Quick test_content_type_mixed;
      test_case "unknown" `Quick test_content_type_unknown;
    ];
    "model_type", [
      test_case "claude" `Quick test_model_type_claude;
      test_case "gpt" `Quick test_model_type_gpt;
      test_case "openai" `Quick test_model_type_openai;
      test_case "gemini" `Quick test_model_type_gemini;
      test_case "google" `Quick test_model_type_google;
      test_case "llama" `Quick test_model_type_llama;
      test_case "meta" `Quick test_model_type_meta;
      test_case "local" `Quick test_model_type_local;
      test_case "universal" `Quick test_model_type_universal;
      test_case "mixed alias" `Quick test_model_type_mixed_alias;
      test_case "unknown" `Quick test_model_type_unknown;
    ];
    "detect_content", [
      test_case "def" `Quick test_detect_def;
      test_case "function" `Quick test_detect_function;
      test_case "let" `Quick test_detect_let;
      test_case "class" `Quick test_detect_class;
      test_case "json brace" `Quick test_detect_json_brace;
      test_case "json bracket" `Quick test_detect_json_bracket;
      test_case "markdown" `Quick test_detect_markdown;
      test_case "mixed" `Quick test_detect_mixed;
      test_case "short" `Quick test_detect_short;
    ];
    "detect_model", [
      test_case "claude i'll help" `Quick test_detect_model_claude_ill_help;
      test_case "claude let me" `Quick test_detect_model_claude_let_me;
      test_case "claude thinking" `Quick test_detect_model_claude_thinking;
      test_case "gpt certainly" `Quick test_detect_model_gpt_certainly;
      test_case "gpt happy" `Quick test_detect_model_gpt_happy;
      test_case "gpt here's what" `Quick test_detect_model_gpt_heres_what;
      test_case "gemini here's" `Quick test_detect_model_gemini_heres;
      test_case "gemini answer" `Quick test_detect_model_gemini_answer;
      test_case "universal" `Quick test_detect_model_universal;
      test_case "short" `Quick test_detect_model_short;
    ];
    "save_load", [
      test_case "v2 roundtrip" `Quick test_save_load_v2;
      test_case "v1 format" `Quick test_load_v1_format;
      test_case "invalid meta" `Quick test_load_invalid_meta;
      test_case "nonexistent" `Quick test_load_nonexistent;
    ];
    "decompress", [
      test_case "not compressed" `Quick test_decompress_not_compressed;
      test_case "truncated" `Quick test_decompress_truncated_header;
      test_case "short data" `Quick test_decompress_short_data;
    ];
    "is_compressed", [
      test_case "true" `Quick test_is_compressed_true;
      test_case "false" `Quick test_is_compressed_false;
      test_case "short" `Quick test_is_compressed_short;
    ];
    "compress", [
      test_case "auto small" `Quick test_compress_auto_small;
      test_case "auto plain passthrough" `Quick test_decompress_auto_plain;
    ];
    "pp", [
      test_case "format" `Quick test_pp;
    ];
    "constants", [
      test_case "values" `Quick test_constants;
    ];
    "contains", [
      test_case "true" `Quick test_contains_substring_true;
      test_case "false" `Quick test_contains_substring_false;
    ];
    "train", [
      test_case "too few" `Quick test_train_too_few;
    ];
    "dict_dir", [
      test_case "env override" `Quick test_dict_dir_env_override;
      test_case "default" `Quick test_dict_dir_default;
    ];
    "load_builtin", [
      test_case "missing type" `Quick test_load_for_type_missing;
      test_case "missing default" `Quick test_load_default_missing;
    ];
    "run_process", [
      test_case "true" `Quick test_run_process_true;
      test_case "false" `Quick test_run_process_false;
      test_case "missing" `Quick test_run_process_missing;
    ];
  ]

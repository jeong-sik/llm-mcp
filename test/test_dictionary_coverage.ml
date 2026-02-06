(** Tests for Dictionary module — Additional coverage
    Targets: model_type detection/conversion, detect_model_type,
    is_dict_compressed, contains_substring, constants, pp, save/load *)

open Alcotest

module D = Dictionary

(** {1 Model Type Conversion} *)

let test_string_of_model_type () =
  check string "claude" "claude" (D.string_of_model_type D.Claude);
  check string "gpt" "gpt" (D.string_of_model_type D.GPT);
  check string "gemini" "gemini" (D.string_of_model_type D.Gemini);
  check string "llama" "llama" (D.string_of_model_type D.Llama);
  check string "universal" "universal" (D.string_of_model_type D.Universal)

let test_model_type_of_string () =
  check bool "claude" true (D.model_type_of_string "claude" = D.Claude);
  check bool "gpt" true (D.model_type_of_string "gpt" = D.GPT);
  check bool "openai" true (D.model_type_of_string "openai" = D.GPT);
  check bool "gemini" true (D.model_type_of_string "gemini" = D.Gemini);
  check bool "google" true (D.model_type_of_string "google" = D.Gemini);
  check bool "llama" true (D.model_type_of_string "llama" = D.Llama);
  check bool "meta" true (D.model_type_of_string "meta" = D.Llama);
  check bool "local" true (D.model_type_of_string "local" = D.Llama);
  check bool "universal" true (D.model_type_of_string "universal" = D.Universal);
  check bool "mixed" true (D.model_type_of_string "mixed" = D.Universal);
  check bool "unknown defaults" true (D.model_type_of_string "anything" = D.Universal)

let test_model_type_roundtrip () =
  let types = [D.Claude; D.GPT; D.Gemini; D.Llama; D.Universal] in
  List.iter (fun t ->
    let s = D.string_of_model_type t in
    let t' = D.model_type_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (t = t')
  ) types

(** {1 Content Type Detection — Additional Cases} *)

let test_detect_content_type_short () =
  (* Very short strings default to Mixed *)
  check bool "short string" true (D.detect_content_type "hi" = D.Mixed)

let test_detect_content_type_json_array () =
  let arr = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]" in
  check bool "json array" true (D.detect_content_type arr = D.JSON)

let test_detect_content_type_code_function () =
  let code = "function hello() {\n  console.log('world')\n}" in
  check bool "function keyword" true (D.detect_content_type code = D.Code)

let test_detect_content_type_code_let () =
  let code = "let x = 42 in\nlet y = x + 1 in\nprint_int y" in
  check bool "let keyword" true (D.detect_content_type code = D.Code)

let test_detect_content_type_code_class () =
  let code = "class MyClass:\n    def __init__(self):\n        pass" in
  check bool "class keyword" true (D.detect_content_type code = D.Code)

(** {1 Model Type Detection} *)

let test_detect_model_type_short () =
  check bool "short defaults to Universal" true
    (D.detect_model_type "tiny" = D.Universal)

let test_detect_model_type_claude () =
  let text = "I'll help you with that. Let me analyze the code and provide feedback." in
  check bool "claude pattern" true (D.detect_model_type text = D.Claude)

let test_detect_model_type_claude_thinking () =
  let text = "Here is my analysis: <thinking>Let me consider this carefully</thinking>" in
  check bool "claude thinking" true (D.detect_model_type text = D.Claude)

let test_detect_model_type_claude_let_me () =
  let text = "Let me review this code for you and provide suggestions." in
  check bool "claude let me" true (D.detect_model_type text = D.Claude)

let test_detect_model_type_gpt () =
  let text = "Certainly! I'd be happy to help you with your question." in
  check bool "gpt pattern" true (D.detect_model_type text = D.GPT)

let test_detect_model_type_gpt_heres_what () =
  let text = "Here's what I found regarding your query about the code." in
  check bool "gpt here's what" true (D.detect_model_type text = D.GPT)

let test_detect_model_type_gemini () =
  let text = "Here's a breakdown of the analysis. The answer is straightforward." in
  check bool "gemini pattern" true (D.detect_model_type text = D.Gemini)

let test_detect_model_type_universal () =
  let text = "The result of the computation is forty-two, which was expected." in
  check bool "universal fallback" true (D.detect_model_type text = D.Universal)

(** {1 Dictionary Constants} *)

let test_constants () =
  check string "magic" "ZDCT" D.magic;
  check int "version" 1 D.version;
  check bool "default_dict_size > 0" true (D.default_dict_size > 0);
  check int "min_samples" 100 D.min_samples;
  check int "min_payload_size" 64 D.min_payload_size

(** {1 is_dict_compressed} *)

let test_is_dict_compressed_too_short () =
  check bool "empty" false (D.is_dict_compressed "");
  check bool "short" false (D.is_dict_compressed "ZDC")

let test_is_dict_compressed_wrong_magic () =
  check bool "wrong magic" false (D.is_dict_compressed "ABCD data")

let test_is_dict_compressed_correct_magic () =
  check bool "correct magic" true (D.is_dict_compressed ("ZDCT" ^ String.make 100 '\x00'))

(** {1 Contains Substring} *)

let test_contains_substring () =
  check bool "found" true (D.contains_substring "hello world" "world");
  check bool "not found" false (D.contains_substring "hello world" "xyz");
  check bool "empty needle" true (D.contains_substring "hello" "");
  check bool "at start" true (D.contains_substring "hello" "hel")

(** {1 Content Type String Conversion} *)

let test_content_type_of_string_extra () =
  check bool "unknown defaults" true (D.content_type_of_string "unknown" = D.Mixed);
  check bool "empty defaults" true (D.content_type_of_string "" = D.Mixed)

(** {1 Dict Save/Load Roundtrip} *)

let test_save_load_roundtrip () =
  let dict = D.{
    dict_id = "test_dict";
    content_type = JSON;
    model_type = Claude;
    dict_data = "fake dictionary data here for testing";
    sample_count = 200;
    created_at = 1700000000.0;
  } in
  let tmp = Filename.temp_file "test_dict" ".zdict" in
  match D.save dict tmp with
  | Error e -> fail (Printf.sprintf "save error: %s" e)
  | Ok () ->
    match D.load tmp with
    | Error e ->
      Sys.remove tmp;
      fail (Printf.sprintf "load error: %s" e)
    | Ok loaded ->
      Sys.remove tmp;
      check string "dict_data" dict.dict_data loaded.dict_data;
      check bool "content_type" true (loaded.content_type = D.JSON);
      check bool "model_type" true (loaded.model_type = D.Claude);
      check int "sample_count" 200 loaded.sample_count

let test_load_invalid_format () =
  let tmp = Filename.temp_file "test_bad" ".zdict" in
  let oc = open_out tmp in
  output_string oc "INVALID|FORMAT|DATA\nstuff";
  close_out oc;
  match D.load tmp with
  | Error msg ->
    Sys.remove tmp;
    check bool "has error" true (String.length msg > 0)
  | Ok _ ->
    Sys.remove tmp;
    fail "should fail on invalid format"

let test_load_nonexistent () =
  match D.load "/nonexistent/path/dict.zdict" with
  | Error _ -> () (* expected *)
  | Ok _ -> fail "should fail on nonexistent file"

(** {1 pp formatter} *)

let test_pp () =
  let dict = D.{
    dict_id = "test_pp";
    content_type = Code;
    model_type = Universal;
    dict_data = "data";
    sample_count = 50;
    created_at = 0.0;
  } in
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  D.pp fmt dict;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  check bool "has id" true
    (try let _ = Str.search_forward (Str.regexp_string "test_pp") s 0 in true
     with Not_found -> false);
  check bool "has type" true
    (try let _ = Str.search_forward (Str.regexp_string "code") s 0 in true
     with Not_found -> false)

(** {1 Train Errors} *)

let test_train_too_few_samples () =
  let samples = List.init 10 (fun i -> Printf.sprintf "sample %d" i) in
  match D.train ~samples ~content_type:D.Mixed () with
  | Error msg -> check bool "mentions min samples" true (String.length msg > 0)
  | Ok _ -> fail "should fail with too few samples"

(** {1 Decompress Auto Passthrough} *)

let test_decompress_auto_not_compressed () =
  let data = "regular text not compressed" in
  let result = D.decompress_auto data in
  check string "passthrough" data result

(** {1 Compress Auto Small} *)

let test_compress_auto_small () =
  let data = "tiny" in
  let result = D.compress_auto data in
  check string "small unchanged" data result

(** {1 Test Suite} *)

let () =
  run "dictionary_coverage" [
    ("model_type", [
      test_case "to_string" `Quick test_string_of_model_type;
      test_case "of_string" `Quick test_model_type_of_string;
      test_case "roundtrip" `Quick test_model_type_roundtrip;
    ]);
    ("content_type_extra", [
      test_case "short" `Quick test_detect_content_type_short;
      test_case "json array" `Quick test_detect_content_type_json_array;
      test_case "function" `Quick test_detect_content_type_code_function;
      test_case "let" `Quick test_detect_content_type_code_let;
      test_case "class" `Quick test_detect_content_type_code_class;
      test_case "of_string defaults" `Quick test_content_type_of_string_extra;
    ]);
    ("detect_model_type", [
      test_case "short" `Quick test_detect_model_type_short;
      test_case "claude" `Quick test_detect_model_type_claude;
      test_case "claude thinking" `Quick test_detect_model_type_claude_thinking;
      test_case "claude let_me" `Quick test_detect_model_type_claude_let_me;
      test_case "gpt" `Quick test_detect_model_type_gpt;
      test_case "gpt heres_what" `Quick test_detect_model_type_gpt_heres_what;
      test_case "gemini" `Quick test_detect_model_type_gemini;
      test_case "universal" `Quick test_detect_model_type_universal;
    ]);
    ("constants", [
      test_case "values" `Quick test_constants;
    ]);
    ("is_dict_compressed", [
      test_case "too short" `Quick test_is_dict_compressed_too_short;
      test_case "wrong magic" `Quick test_is_dict_compressed_wrong_magic;
      test_case "correct magic" `Quick test_is_dict_compressed_correct_magic;
    ]);
    ("contains_substring", [
      test_case "basic" `Quick test_contains_substring;
    ]);
    ("save_load", [
      test_case "roundtrip" `Quick test_save_load_roundtrip;
      test_case "invalid format" `Quick test_load_invalid_format;
      test_case "nonexistent" `Quick test_load_nonexistent;
    ]);
    ("pp", [
      test_case "format" `Quick test_pp;
    ]);
    ("train", [
      test_case "too few samples" `Quick test_train_too_few_samples;
    ]);
    ("decompress_auto", [
      test_case "not compressed" `Quick test_decompress_auto_not_compressed;
    ]);
    ("compress_auto", [
      test_case "small" `Quick test_compress_auto_small;
    ]);
  ]

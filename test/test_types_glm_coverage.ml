(** Tests for Types_glm module — GLM function calling types
    Targets: glm_tool_type conversion, json_schema_type, function_parameter_to_json,
    glm_function_schema_to_json, glm_tool constructors, glm_tool_to_json,
    parse_glm_tool_calls, translation_strategy conversion *)

open Alcotest

(** {1 GLM Tool Type Conversion} *)

let test_glm_tool_type_to_string () =
  check string "web_search" "web_search"
    (Types_glm.string_of_glm_tool_type Types_glm.GlmWebSearch);
  check string "function" "function"
    (Types_glm.string_of_glm_tool_type Types_glm.GlmFunction);
  check string "code_interpreter" "code_interpreter"
    (Types_glm.string_of_glm_tool_type Types_glm.GlmCodeInterpreter)

let test_glm_tool_type_of_string () =
  check bool "web_search" true
    (Types_glm.glm_tool_type_of_string "web_search" = Types_glm.GlmWebSearch);
  check bool "function" true
    (Types_glm.glm_tool_type_of_string "function" = Types_glm.GlmFunction);
  check bool "code_interpreter" true
    (Types_glm.glm_tool_type_of_string "code_interpreter" = Types_glm.GlmCodeInterpreter)

let test_glm_tool_type_of_string_unknown () =
  try
    let _ = Types_glm.glm_tool_type_of_string "unknown" in
    fail "should raise"
  with Failure msg ->
    check bool "error msg contains unknown" true
      (String.length msg > 0)

let test_glm_tool_type_roundtrip () =
  let types = [Types_glm.GlmWebSearch; Types_glm.GlmFunction; Types_glm.GlmCodeInterpreter] in
  List.iter (fun t ->
    let s = Types_glm.string_of_glm_tool_type t in
    let t' = Types_glm.glm_tool_type_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (t = t')
  ) types

(** {1 JSON Schema Type} *)

let test_json_schema_type_to_string () =
  check string "string" "string" (Types_glm.string_of_json_schema_type Types_glm.SchemaString);
  check string "number" "number" (Types_glm.string_of_json_schema_type Types_glm.SchemaNumber);
  check string "integer" "integer" (Types_glm.string_of_json_schema_type Types_glm.SchemaInteger);
  check string "boolean" "boolean" (Types_glm.string_of_json_schema_type Types_glm.SchemaBoolean);
  check string "array" "array" (Types_glm.string_of_json_schema_type (Types_glm.SchemaArray Types_glm.SchemaString));
  check string "object" "object" (Types_glm.string_of_json_schema_type Types_glm.SchemaObject);
  check string "null" "null" (Types_glm.string_of_json_schema_type Types_glm.SchemaNull)

(** {1 Function Parameter to JSON} *)

let test_function_parameter_to_json_basic () =
  let param = Types_glm.{
    param_name = "query";
    param_type = SchemaString;
    param_description = None;
    param_required = true;
    param_enum = None;
  } in
  let json = Types_glm.function_parameter_to_json param in
  let open Yojson.Safe.Util in
  check string "type" "string" (json |> member "type" |> to_string)

let test_function_parameter_to_json_with_desc () =
  let param = Types_glm.{
    param_name = "query";
    param_type = SchemaString;
    param_description = Some "Search query";
    param_required = true;
    param_enum = None;
  } in
  let json = Types_glm.function_parameter_to_json param in
  let open Yojson.Safe.Util in
  check string "description" "Search query" (json |> member "description" |> to_string)

let test_function_parameter_to_json_with_enum () =
  let param = Types_glm.{
    param_name = "lang";
    param_type = SchemaString;
    param_description = None;
    param_required = false;
    param_enum = Some ["en"; "ko"; "ja"];
  } in
  let json = Types_glm.function_parameter_to_json param in
  let open Yojson.Safe.Util in
  let enums = json |> member "enum" |> to_list |> List.map to_string in
  check int "enum count" 3 (List.length enums);
  check string "first enum" "en" (List.hd enums)

(** {1 GLM Function Schema to JSON} *)

let test_glm_function_schema_to_json () =
  let schema = Types_glm.{
    func_name = "search";
    func_description = "Search the web";
    func_parameters = [
      { param_name = "query"; param_type = SchemaString;
        param_description = Some "Search query"; param_required = true;
        param_enum = None };
      { param_name = "limit"; param_type = SchemaInteger;
        param_description = None; param_required = false;
        param_enum = None };
    ];
  } in
  let json = Types_glm.glm_function_schema_to_json schema in
  let open Yojson.Safe.Util in
  check string "type" "function" (json |> member "type" |> to_string);
  let func = json |> member "function" in
  check string "name" "search" (func |> member "name" |> to_string);
  check string "description" "Search the web" (func |> member "description" |> to_string);
  let params = func |> member "parameters" in
  check string "params type" "object" (params |> member "type" |> to_string);
  let props = params |> member "properties" in
  check string "query type" "string" (props |> member "query" |> member "type" |> to_string);
  let required = params |> member "required" |> to_list |> List.map to_string in
  check int "required count" 1 (List.length required);
  check string "required field" "query" (List.hd required)

(** {1 GLM Tool Constructors} *)

let test_glm_web_search_tool () =
  let tool = Types_glm.glm_web_search_tool () in
  check bool "type is web_search" true (tool.tool_type = Types_glm.GlmWebSearch);
  check bool "no function_schema" true (tool.function_schema = None);
  check bool "has web_search_config" true (tool.web_search_config <> None);
  check bool "no code_interpreter" true (tool.code_interpreter_config = None)

let test_glm_web_search_tool_no_result () =
  let tool = Types_glm.glm_web_search_tool ~search_result:false () in
  match tool.web_search_config with
  | Some (_, search_result) -> check bool "search_result false" false search_result
  | None -> fail "expected web_search_config"

let test_glm_function_tool () =
  let schema = Types_glm.{
    func_name = "test_fn";
    func_description = "A test function";
    func_parameters = [];
  } in
  let tool = Types_glm.glm_function_tool schema in
  check bool "type is function" true (tool.tool_type = Types_glm.GlmFunction);
  check bool "has function_schema" true (tool.function_schema <> None)

let test_glm_code_interpreter_tool () =
  let tool = Types_glm.glm_code_interpreter_tool () in
  check bool "type is code_interpreter" true (tool.tool_type = Types_glm.GlmCodeInterpreter);
  match tool.code_interpreter_config with
  | Some sandbox -> check string "default sandbox" "auto" sandbox
  | None -> fail "expected code_interpreter_config"

let test_glm_code_interpreter_tool_custom () =
  let tool = Types_glm.glm_code_interpreter_tool ~sandbox:"restricted" () in
  match tool.code_interpreter_config with
  | Some sandbox -> check string "custom sandbox" "restricted" sandbox
  | None -> fail "expected code_interpreter_config"

(** {1 GLM Tool to JSON} *)

let test_glm_tool_to_json_web_search () =
  let tool = Types_glm.glm_web_search_tool () in
  let json = Types_glm.glm_tool_to_json tool in
  let open Yojson.Safe.Util in
  check string "type" "web_search" (json |> member "type" |> to_string);
  let ws = json |> member "web_search" in
  check bool "enable" true (ws |> member "enable" |> to_bool);
  check bool "search_result" true (ws |> member "search_result" |> to_bool)

let test_glm_tool_to_json_web_search_no_config () =
  (* Web search with no config should default to true, true *)
  let tool = Types_glm.{
    tool_type = GlmWebSearch;
    function_schema = None;
    web_search_config = None;
    code_interpreter_config = None;
  } in
  let json = Types_glm.glm_tool_to_json tool in
  let open Yojson.Safe.Util in
  check bool "default enable" true (json |> member "web_search" |> member "enable" |> to_bool)

let test_glm_tool_to_json_function () =
  let schema = Types_glm.{
    func_name = "get_weather";
    func_description = "Get weather info";
    func_parameters = [
      { param_name = "city"; param_type = SchemaString;
        param_description = Some "City name"; param_required = true;
        param_enum = None }
    ];
  } in
  let tool = Types_glm.glm_function_tool schema in
  let json = Types_glm.glm_tool_to_json tool in
  let open Yojson.Safe.Util in
  check string "type" "function" (json |> member "type" |> to_string)

let test_glm_tool_to_json_function_no_schema () =
  let tool = Types_glm.{
    tool_type = GlmFunction;
    function_schema = None;
    web_search_config = None;
    code_interpreter_config = None;
  } in
  try
    let _ = Types_glm.glm_tool_to_json tool in
    fail "should raise"
  with Failure msg ->
    check bool "error msg" true (String.length msg > 0)

let test_glm_tool_to_json_code_interpreter () =
  let tool = Types_glm.glm_code_interpreter_tool ~sandbox:"secure" () in
  let json = Types_glm.glm_tool_to_json tool in
  let open Yojson.Safe.Util in
  check string "type" "code_interpreter" (json |> member "type" |> to_string);
  check string "sandbox" "secure"
    (json |> member "code_interpreter" |> member "sandbox" |> to_string)

let test_glm_tool_to_json_code_interpreter_no_config () =
  let tool = Types_glm.{
    tool_type = GlmCodeInterpreter;
    function_schema = None;
    web_search_config = None;
    code_interpreter_config = None;
  } in
  let json = Types_glm.glm_tool_to_json tool in
  let open Yojson.Safe.Util in
  check string "default sandbox" "auto"
    (json |> member "code_interpreter" |> member "sandbox" |> to_string)

(** {1 GLM Tools to JSON (list)} *)

let test_glm_tools_to_json () =
  let tools = [
    Types_glm.glm_web_search_tool ();
    Types_glm.glm_code_interpreter_tool ();
  ] in
  let json = Types_glm.glm_tools_to_json tools in
  match json with
  | `List items -> check int "2 tools" 2 (List.length items)
  | _ -> fail "expected list"

(** {1 Parse GLM Tool Calls} *)

let test_parse_glm_tool_calls_success () =
  let json = Yojson.Safe.from_string {|{
    "choices": [{
      "message": {
        "tool_calls": [{
          "id": "call_123",
          "type": "function",
          "function": {
            "name": "search",
            "arguments": "{\"query\": \"test\"}"
          }
        }]
      }
    }]
  }|} in
  let calls = Types_glm.parse_glm_tool_calls json in
  check int "1 call" 1 (List.length calls);
  let call = List.hd calls in
  check string "id" "call_123" call.call_id;
  check string "type" "function" call.call_type;
  check string "name" "search" call.function_name;
  check bool "has arguments" true (String.length call.function_arguments > 0)

let test_parse_glm_tool_calls_no_tool_calls () =
  let json = Yojson.Safe.from_string {|{
    "choices": [{
      "message": {
        "content": "Hello",
        "tool_calls": null
      }
    }]
  }|} in
  let calls = Types_glm.parse_glm_tool_calls json in
  check int "no calls" 0 (List.length calls)

let test_parse_glm_tool_calls_empty_choices () =
  let json = Yojson.Safe.from_string {|{"choices": []}|} in
  let calls = Types_glm.parse_glm_tool_calls json in
  check int "no calls" 0 (List.length calls)

let test_parse_glm_tool_calls_invalid_json () =
  let json = `String "not valid" in
  let calls = Types_glm.parse_glm_tool_calls json in
  check int "no calls" 0 (List.length calls)

let test_parse_glm_tool_calls_multiple () =
  let json = Yojson.Safe.from_string {|{
    "choices": [{
      "message": {
        "tool_calls": [
          {"id": "c1", "type": "function", "function": {"name": "f1", "arguments": "{}"}},
          {"id": "c2", "type": "function", "function": {"name": "f2", "arguments": "{}"}}
        ]
      }
    }]
  }|} in
  let calls = Types_glm.parse_glm_tool_calls json in
  check int "2 calls" 2 (List.length calls)

(** {1 Translation Strategy} *)

let test_translation_strategy_to_string () =
  check string "general" "general"
    (Types_glm.string_of_translation_strategy Types_glm.TransGeneral);
  check string "paraphrased" "paraphrased"
    (Types_glm.string_of_translation_strategy Types_glm.TransParaphrased);
  check string "two_step" "two_step"
    (Types_glm.string_of_translation_strategy Types_glm.TransTwoStep);
  check string "three_stage" "three_stage"
    (Types_glm.string_of_translation_strategy Types_glm.TransThreeStage);
  check string "reflective" "reflective"
    (Types_glm.string_of_translation_strategy Types_glm.TransReflective);
  check string "chain_of_thought" "chain_of_thought"
    (Types_glm.string_of_translation_strategy Types_glm.TransChainOfThought)

let test_translation_strategy_of_string () =
  check bool "general" true
    (Types_glm.translation_strategy_of_string "general" = Types_glm.TransGeneral);
  check bool "paraphrased" true
    (Types_glm.translation_strategy_of_string "paraphrased" = Types_glm.TransParaphrased);
  check bool "two_step" true
    (Types_glm.translation_strategy_of_string "two_step" = Types_glm.TransTwoStep);
  check bool "three_stage" true
    (Types_glm.translation_strategy_of_string "three_stage" = Types_glm.TransThreeStage);
  check bool "reflective" true
    (Types_glm.translation_strategy_of_string "reflective" = Types_glm.TransReflective);
  check bool "chain_of_thought" true
    (Types_glm.translation_strategy_of_string "chain_of_thought" = Types_glm.TransChainOfThought)

let test_translation_strategy_of_string_unknown () =
  try
    let _ = Types_glm.translation_strategy_of_string "unknown" in
    fail "should raise"
  with Failure msg ->
    check bool "error msg" true (String.length msg > 0)

let test_translation_strategy_roundtrip () =
  let strategies = [
    Types_glm.TransGeneral; Types_glm.TransParaphrased;
    Types_glm.TransTwoStep; Types_glm.TransThreeStage;
    Types_glm.TransReflective; Types_glm.TransChainOfThought;
  ] in
  List.iter (fun s ->
    let str = Types_glm.string_of_translation_strategy s in
    let s' = Types_glm.translation_strategy_of_string str in
    check bool (Printf.sprintf "roundtrip %s" str) true (s = s')
  ) strategies

(** {1 Test Suite} *)

let () =
  run "types_glm" [
    ("glm_tool_type", [
      test_case "to_string" `Quick test_glm_tool_type_to_string;
      test_case "of_string" `Quick test_glm_tool_type_of_string;
      test_case "of_string unknown" `Quick test_glm_tool_type_of_string_unknown;
      test_case "roundtrip" `Quick test_glm_tool_type_roundtrip;
    ]);
    ("json_schema_type", [
      test_case "to_string" `Quick test_json_schema_type_to_string;
    ]);
    ("function_parameter_json", [
      test_case "basic" `Quick test_function_parameter_to_json_basic;
      test_case "with description" `Quick test_function_parameter_to_json_with_desc;
      test_case "with enum" `Quick test_function_parameter_to_json_with_enum;
    ]);
    ("function_schema_json", [
      test_case "full schema" `Quick test_glm_function_schema_to_json;
    ]);
    ("glm_tool_constructors", [
      test_case "web_search" `Quick test_glm_web_search_tool;
      test_case "web_search no result" `Quick test_glm_web_search_tool_no_result;
      test_case "function" `Quick test_glm_function_tool;
      test_case "code_interpreter" `Quick test_glm_code_interpreter_tool;
      test_case "code_interpreter custom" `Quick test_glm_code_interpreter_tool_custom;
    ]);
    ("glm_tool_to_json", [
      test_case "web_search" `Quick test_glm_tool_to_json_web_search;
      test_case "web_search no config" `Quick test_glm_tool_to_json_web_search_no_config;
      test_case "function" `Quick test_glm_tool_to_json_function;
      test_case "function no schema" `Quick test_glm_tool_to_json_function_no_schema;
      test_case "code_interpreter" `Quick test_glm_tool_to_json_code_interpreter;
      test_case "code_interpreter no config" `Quick test_glm_tool_to_json_code_interpreter_no_config;
    ]);
    ("glm_tools_list", [
      test_case "to_json" `Quick test_glm_tools_to_json;
    ]);
    ("parse_tool_calls", [
      test_case "success" `Quick test_parse_glm_tool_calls_success;
      test_case "no tool_calls" `Quick test_parse_glm_tool_calls_no_tool_calls;
      test_case "empty choices" `Quick test_parse_glm_tool_calls_empty_choices;
      test_case "invalid json" `Quick test_parse_glm_tool_calls_invalid_json;
      test_case "multiple calls" `Quick test_parse_glm_tool_calls_multiple;
    ]);
    ("translation_strategy", [
      test_case "to_string" `Quick test_translation_strategy_to_string;
      test_case "of_string" `Quick test_translation_strategy_of_string;
      test_case "of_string unknown" `Quick test_translation_strategy_of_string_unknown;
      test_case "roundtrip" `Quick test_translation_strategy_roundtrip;
    ]);
  ]

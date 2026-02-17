(* ============================================================================
   GLM Function Calling Types (Phase 1.1)

   GLM 5 / GLM 4.7 supports three tool types:
   - web_search: Real-time web search
   - function: Custom function calling (OpenAI-compatible)
   - code_interpreter: Execute Python code in sandbox
   ============================================================================ *)

(** GLM parsing errors *)
type glm_parse_error =
  | Unknown_tool_type of string
  | Missing_function_schema
  | Unknown_translation_strategy of string

(** GLM Tool type - discriminated union for tool kinds *)
type glm_tool_type =
  | GlmWebSearch      (** Web search for real-time information *)
  | GlmFunction       (** Custom function calling *)
  | GlmCodeInterpreter  (** Python code execution sandbox *)
[@@deriving yojson]

let string_of_glm_tool_type = function
  | GlmWebSearch -> "web_search"
  | GlmFunction -> "function"
  | GlmCodeInterpreter -> "code_interpreter"

let glm_tool_type_of_string = function
  | "web_search" -> Ok GlmWebSearch
  | "function" -> Ok GlmFunction
  | "code_interpreter" -> Ok GlmCodeInterpreter
  | s -> Error (Unknown_tool_type s)

(** [glm_tool_type_of_string_unsafe] is legacy - returns default on error *)
let glm_tool_type_of_string_unsafe = function
  | "web_search" -> GlmWebSearch
  | "function" -> GlmFunction
  | "code_interpreter" -> GlmCodeInterpreter
  | _ -> GlmFunction  (* Default fallback *)

(** JSON Schema for function parameters *)
type json_schema_type =
  | SchemaString
  | SchemaNumber
  | SchemaInteger
  | SchemaBoolean
  | SchemaArray of json_schema_type
  | SchemaObject
  | SchemaNull
[@@deriving yojson]

let string_of_json_schema_type = function
  | SchemaString -> "string"
  | SchemaNumber -> "number"
  | SchemaInteger -> "integer"
  | SchemaBoolean -> "boolean"
  | SchemaArray _ -> "array"
  | SchemaObject -> "object"
  | SchemaNull -> "null"

(** Function parameter definition *)
type function_parameter = {
  param_name : string;
  param_type : json_schema_type;
  param_description : string option;
  param_required : bool;
  param_enum : string list option;  (** Allowed values for enum types *)
}

(** Function schema for GLM Function Calling *)
type glm_function_schema = {
  func_name : string;
  func_description : string;
  func_parameters : function_parameter list;
}

(** Convert function_parameter to JSON Schema *)
let function_parameter_to_json param =
  let type_str = string_of_json_schema_type param.param_type in
  let base = [("type", `String type_str)] in
  let with_desc = match param.param_description with
    | Some d -> base @ [("description", `String d)]
    | None -> base
  in
  let with_enum = match param.param_enum with
    | Some values -> with_desc @ [("enum", `List (List.map (fun v -> `String v) values))]
    | None -> with_desc
  in
  `Assoc with_enum

(** Convert glm_function_schema to GLM API format *)
let glm_function_schema_to_json schema =
  let properties = List.map (fun p ->
    (p.param_name, function_parameter_to_json p)
  ) schema.func_parameters in
  let required = List.filter_map (fun p ->
    if p.param_required then Some (`String p.param_name) else None
  ) schema.func_parameters in
  `Assoc [
    ("type", `String "function");
    ("function", `Assoc [
      ("name", `String schema.func_name);
      ("description", `String schema.func_description);
      ("parameters", `Assoc [
        ("type", `String "object");
        ("properties", `Assoc properties);
        ("required", `List required);
      ]);
    ])
  ]

(** GLM Tool definition - generic wrapper for all tool types *)
type glm_tool = {
  tool_type : glm_tool_type;
  function_schema : glm_function_schema option;  (** Only for GlmFunction *)
  web_search_config : (bool * bool) option;  (** (enable, search_result) for GlmWebSearch *)
  code_interpreter_config : string option;  (** sandbox mode for GlmCodeInterpreter *)
}

(** Create web_search tool *)
let glm_web_search_tool ?(search_result=true) () = {
  tool_type = GlmWebSearch;
  function_schema = None;
  web_search_config = Some (true, search_result);
  code_interpreter_config = None;
}

(** Create function tool *)
let glm_function_tool schema = {
  tool_type = GlmFunction;
  function_schema = Some schema;
  web_search_config = None;
  code_interpreter_config = None;
}

(** Create code_interpreter tool *)
let glm_code_interpreter_tool ?(sandbox="auto") () = {
  tool_type = GlmCodeInterpreter;
  function_schema = None;
  web_search_config = None;
  code_interpreter_config = Some sandbox;
}

(** Convert glm_tool to GLM API JSON format *)
let glm_tool_to_json tool =
  match tool.tool_type with
  | GlmWebSearch ->
      let enable, search_result = match tool.web_search_config with
        | Some (e, r) -> (e, r)
        | None -> (true, true)
      in
      Ok (`Assoc [
        ("type", `String "web_search");
        ("web_search", `Assoc [
          ("enable", `Bool enable);
          ("search_result", `Bool search_result);
        ])
      ])
  | GlmFunction ->
      (match tool.function_schema with
       | Some schema -> Ok (glm_function_schema_to_json schema)
       | None -> Error Missing_function_schema)
  | GlmCodeInterpreter ->
      let sandbox = match tool.code_interpreter_config with
        | Some s -> s
        | None -> "auto"
      in
      Ok (`Assoc [
        ("type", `String "code_interpreter");
        ("code_interpreter", `Assoc [
          ("sandbox", `String sandbox);
        ])
      ])

(** Convert list of glm_tools to JSON *)
let glm_tools_to_json tools =
  let rec loop acc errors = function
    | [] -> (match List.rev errors, List.rev acc with
        | [], ok -> Ok (`List ok)
        | err :: _, _ -> Error err)
    | x :: xs ->
        (match glm_tool_to_json x with
         | Ok json -> loop (json :: acc) errors xs
         | Error err -> loop acc (err :: errors) xs)
  in
  loop [] [] tools

(** Tool call result from GLM response *)
type glm_tool_call = {
  call_id : string;
  call_type : string;  (** "function" *)
  function_name : string;
  function_arguments : string;  (** JSON string *)
}

(** Parse tool_calls from GLM response JSON *)
let parse_glm_tool_calls json =
  let open Yojson.Safe.Util in
  try
    let choices = json |> member "choices" |> to_list in
    if List.length choices = 0 then []
    else
      let message = (List.hd choices) |> member "message" in
      let tool_calls = message |> member "tool_calls" in
      if tool_calls = `Null then []
      else
        tool_calls |> to_list |> List.map (fun tc ->
          let func = tc |> member "function" in
          {
            call_id = tc |> member "id" |> to_string;
            call_type = tc |> member "type" |> to_string;
            function_name = func |> member "name" |> to_string;
            function_arguments = func |> member "arguments" |> to_string;
          }
        )
  with _ -> []

(** GLM Translation Strategy (Phase 3) *)
type glm_translation_strategy =
  | TransGeneral           (** Direct translation *)
  | TransParaphrased       (** Free/paraphrased translation *)
  | TransTwoStep           (** Literal → Free *)
  | TransThreeStage        (** Literal → Free → Review *)
  | TransReflective        (** Draft → Critique → Revise *)
  | TransChainOfThought    (** Step-by-step reasoning translation *)
[@@deriving yojson]

let string_of_translation_strategy = function
  | TransGeneral -> "general"
  | TransParaphrased -> "paraphrased"
  | TransTwoStep -> "two_step"
  | TransThreeStage -> "three_stage"
  | TransReflective -> "reflective"
  | TransChainOfThought -> "chain_of_thought"

let translation_strategy_of_string = function
  | "general" -> Ok TransGeneral
  | "paraphrased" -> Ok TransParaphrased
  | "two_step" -> Ok TransTwoStep
  | "three_stage" -> Ok TransThreeStage
  | "reflective" -> Ok TransReflective
  | "chain_of_thought" -> Ok TransChainOfThought
  | s -> Error (Unknown_translation_strategy s)

(** [translation_strategy_of_string_unsafe] is legacy - returns default on error *)
let translation_strategy_of_string_unsafe = function
  | "general" -> TransGeneral
  | "paraphrased" -> TransParaphrased
  | "two_step" -> TransTwoStep
  | "three_stage" -> TransThreeStage
  | "reflective" -> TransReflective
  | "chain_of_thought" -> TransChainOfThought
  | _ -> TransGeneral  (* Default fallback *)


open Types_core

type visibility =
  | Default
  | Hidden

type lifecycle =
  | Active
  | Deprecated

type entry = {
  schema : tool_schema;
  parse : Yojson.Safe.t -> Types_llm.tool_args;
  visibility : visibility;
  lifecycle : lifecycle;
  canonical_name : string option;
  replacement : string option;
}

let default ?canonical_name ?replacement schema parse =
  {
    schema;
    parse;
    visibility = Default;
    lifecycle = Active;
    canonical_name;
    replacement;
  }

let hidden ?canonical_name ?replacement schema parse =
  {
    schema;
    parse;
    visibility = Hidden;
    lifecycle = Active;
    canonical_name;
    replacement;
  }

let entries =
  [
    default Types_schema.gemini_schema Tool_parsers.parse_gemini_args;
    default Types_schema.gemini_list_schema Tool_parsers.parse_gemini_list_args;
    default Types_schema.claude_schema Tool_parsers.parse_claude_args;
    default Types_schema.codex_schema Tool_parsers.parse_codex_args;
    default Types_schema.ollama_schema Tool_parsers.parse_ollama_args;
    default Types_schema.ollama_list_schema Tool_parsers.parse_ollama_list_args;
    default Types_schema.glm_schema Tool_parsers.parse_glm_args;
    default Types_schema.glm_ocr_schema Tool_parsers.parse_glm_ocr_args;
    default Types_schema.glm_image_schema Tool_parsers.parse_glm_image_args;
    default Types_schema.glm_video_schema Tool_parsers.parse_glm_video_args;
    default Types_schema.glm_stt_schema Tool_parsers.parse_glm_stt_args;
    default Types_schema.glm_translate_schema Tool_parsers.parse_glm_translate_args;
    hidden Types_schema.set_stream_delta_schema Tool_parsers.parse_set_stream_delta_args;
    hidden Types_schema.get_stream_delta_schema Tool_parsers.parse_get_stream_delta_args;
    default Types_schema.chain_run_schema Tool_parsers.parse_chain_run_args;
    default Types_schema.chain_validate_schema Tool_parsers.parse_chain_validate_args;
    default Types_schema.chain_convert_schema Tool_parsers.parse_chain_convert_args;
    default Types_schema.chain_list_schema (fun _ -> Types_llm.ChainList);
    default Types_schema.chain_checkpoints_schema Tool_parsers.parse_chain_checkpoints_args;
    default Types_schema.chain_resume_schema Tool_parsers.parse_chain_resume_args;
    default Types_schema.chain_to_mermaid_schema Tool_parsers.parse_chain_to_mermaid_args;
    default Types_schema.chain_visualize_schema Tool_parsers.parse_chain_visualize_args;
    default Types_schema.chain_orchestrate_schema Tool_parsers.parse_chain_orchestrate_args;
    default Types_schema.prompt_register_schema Tool_parsers.parse_prompt_register_args;
    default Types_schema.prompt_list_schema (fun _ -> Types_llm.PromptList);
    default Types_schema.prompt_get_schema Tool_parsers.parse_prompt_get_args;
    hidden Types_schema.gh_pr_diff_schema Tool_parsers.parse_gh_pr_diff_args;
    hidden Types_schema.slack_post_schema Tool_parsers.parse_slack_post_args;
  ]

let all_entries = entries

let all_schemas = List.map (fun entry -> entry.schema) entries

let find_entry name =
  List.find_opt (fun entry -> String.equal entry.schema.name name) entries

let is_known_tool name = Option.is_some (find_entry name)

let should_list ?(include_hidden = false) ?(include_deprecated = false) entry =
  match entry.visibility, entry.lifecycle with
  | Hidden, _ -> include_hidden
  | Default, Deprecated -> include_deprecated
  | Default, Active -> true

let list_entries ?(include_hidden = false) ?(include_deprecated = false) () =
  List.filter (should_list ~include_hidden ~include_deprecated) entries

let list_schemas ?(include_hidden = false) ?(include_deprecated = false) () =
  list_entries ~include_hidden ~include_deprecated ()
  |> List.map (fun entry -> entry.schema)

let visibility_to_string = function
  | Default -> "default"
  | Hidden -> "hidden"

let lifecycle_to_string = function
  | Active -> "active"
  | Deprecated -> "deprecated"

let metadata_fields entry =
  let base =
    [
      ("visibility", `String (visibility_to_string entry.visibility));
      ("lifecycle", `String (lifecycle_to_string entry.lifecycle));
    ]
  in
  let with_canonical =
    match entry.canonical_name with
    | Some canonical_name -> ("canonicalName", `String canonical_name) :: base
    | None -> base
  in
  match entry.replacement with
  | Some replacement -> ("replacement", `String replacement) :: with_canonical
  | None -> with_canonical

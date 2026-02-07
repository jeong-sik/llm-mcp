(** Tools Tracer - Langfuse Tracing Helpers for LLM Tools

    Pure functions for extracting tracing metadata from tool arguments.
    No Eio dependencies - can be used anywhere.
*)

open Types

(** Extract model name from tool_args for tracing *)
let get_model_name = function
  | Gemini { model; _ } -> Printf.sprintf "gemini:%s" model
  | Claude { model; _ } -> Printf.sprintf "claude:%s" model
  | Codex { model; _ } -> Printf.sprintf "codex:%s" model
  | Ollama { model; _ } -> Printf.sprintf "ollama:%s" model
  | OllamaList -> "ollama:list"
  | Glm { model; _ } -> Printf.sprintf "glm:%s" model
  | GlmTranslate { model; _ } -> Printf.sprintf "glm.translate:%s" model
  | ChainRun _ -> "chain:run"
  | ChainValidate _ -> "chain:validate"
  | ChainList -> "chain:list"
  | ChainToMermaid _ -> "chain:to_mermaid"
  | ChainVisualize _ -> "chain:visualize"
  | ChainConvert _ -> "chain:convert"
  | ChainOrchestrate _ -> "chain:orchestrate"
  | ChainCheckpoints _ -> "chain:checkpoints"
  | ChainResume _ -> "chain:resume"
  | PromptRegister _ -> "prompt:register"
  | PromptList -> "prompt:list"
  | PromptGet _ -> "prompt:get"
  | GhPrDiff _ -> "tool:gh_pr_diff"
  | SlackPost _ -> "tool:slack_post"
  | SetStreamDelta _ -> "config:set_stream_delta"
  | GetStreamDelta -> "config:get_stream_delta"

(** Extract input/prompt from tool_args for tracing *)
let get_input = function
  | Gemini { prompt; _ } -> prompt
  | Claude { prompt; _ } -> prompt
  | Codex { prompt; _ } -> prompt
  | Ollama { prompt; _ } -> prompt
  | OllamaList -> "(list models)"
  | Glm { prompt; _ } -> prompt
  | GlmTranslate { text; _ } -> Printf.sprintf "(translate: %s)" (String.sub text 0 (min 50 (String.length text)))
  | ChainRun { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainValidate { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainOrchestrate { chain; _ } ->
      (match chain with
       | Some j -> Printf.sprintf "(orchestrate: %s)" (Yojson.Safe.to_string j)
       | None -> "(orchestrate: preset)")
  | _ -> "(non-llm operation)"

(** Get tool name for logging (dot-separated format) *)
let get_tool_name = function
  | ChainRun _ -> "chain.run"
  | ChainValidate _ -> "chain.validate"
  | ChainList -> "chain.list"
  | ChainToMermaid _ -> "chain.to_mermaid"
  | ChainVisualize _ -> "chain.visualize"
  | ChainConvert _ -> "chain.convert"
  | ChainOrchestrate _ -> "chain.orchestrate"
  | ChainCheckpoints _ -> "chain.checkpoints"
  | ChainResume _ -> "chain.resume"
  | OllamaList -> "ollama.list"
  | PromptRegister _ -> "prompt.register"
  | PromptList -> "prompt.list"
  | PromptGet _ -> "prompt.get"
  | GhPrDiff _ -> "gh.pr.diff"
  | SlackPost _ -> "slack.post"
  | SetStreamDelta _ -> "config.set_stream_delta"
  | GetStreamDelta -> "config.get_stream_delta"
  | _ -> "llm"

(** Classify error type from tool result *)
let classify_error (r : tool_result) : string option =
  if r.returncode = 0 then None
  else if String.length r.response >= 7 && String.sub r.response 0 7 = "Timeout" then
    Some "timeout"
  else if String.length r.response >= 6 && String.sub r.response 0 6 = "Error:" then
    Some "tool_error"
  else
    Some "llm_error"

(** Check if result was streamed *)
let was_streamed (r : tool_result) : bool =
  match List.assoc_opt "streamed" r.extra with
  | Some "true" -> true
  | _ -> false

(** Known aliases for API key env vars. *)
let api_key_aliases = function
  | "GEMINI_API_KEY" -> [ "GOOGLE_AI_API_KEY" ]
  | _ -> []

(** Get API key from environment, returns empty string if not set.
    Accepts aliases for some providers (e.g. GEMINI_API_KEY ↔ GOOGLE_AI_API_KEY). *)
let get_api_key env_var =
  let get_nonempty k =
    match Sys.getenv_opt k with
    | Some v when String.length v > 0 -> Some v
    | _ -> None
  in
  match get_nonempty env_var with
  | Some v -> v
  | None ->
      api_key_aliases env_var
      |> List.find_map get_nonempty
      |> Option.value ~default:""

(** Check if API key is set, returns Some error_result if missing *)
let require_api_key ~env_var ~model ~extra : tool_result option =
  let api_key = get_api_key env_var in
  if String.length api_key = 0 then
    let names = env_var :: api_key_aliases env_var in
    let name_str =
      match names with
      | [] -> env_var
      | [one] -> one
      | _ -> String.concat " or " names
    in
    Some { model; returncode = -1;
           response = Printf.sprintf "Error: %s environment variable not set" name_str;
           extra = extra @ [("error", "missing_api_key")] }
  else
    None

(** Create success result *)
let success_result ~model ~extra response : tool_result =
  { model; returncode = 0; response; extra }

(** Create timeout error result *)
let timeout_result ~model ~extra timeout_sec : tool_result =
  { model; returncode = -1; response = Printf.sprintf "Timeout after %ds" timeout_sec; extra }

(** Create process error result *)
let process_error_result ~model ~extra msg : tool_result =
  { model; returncode = -1; response = Printf.sprintf "Error: %s" msg; extra }

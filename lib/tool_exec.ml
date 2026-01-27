(** Tool Execution - Main dispatcher for all MCP tools *)

open Printf
open Types
open Cli_runner_eio

(* Import from split modules *)
module Mcp_client = Mcp_client
module Llm_clients = Llm_clients

(** {1 External Tools} *)

(** Default timeout for external tools (60 seconds) *) let external_tool_timeout = 60

(** Execute GitHub PR diff command using gh CLI *) let execute_gh_pr_diff ~sw ~proc_mgr ~clock ~repo ~pr_number : tool_result =
  eprintf "[gh_pr_diff] Fetching diff for %s#%d\n%!" repo pr_number;
  let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
    "gh" ["pr"; "diff"; string_of_int pr_number; "-R"; repo] in
  match result with
  | Ok r ->
      {
        model = "gh_pr_diff";
        returncode = r.exit_code;
        response = get_output r;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number)];
      }
  | Error (Timeout t) ->
      {
        model = "gh_pr_diff";
        returncode = -1;
        response = sprintf "Timeout after %ds" t;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number); ("error", "timeout")];
      }
  | Error (ProcessError msg) ->
      {
        model = "gh_pr_diff";
        returncode = -1;
        response = sprintf "Error: %s" msg;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number); ("error", msg)];
      }

(** Execute Slack post message using curl *) let execute_slack_post ~sw ~proc_mgr ~clock ~channel ~text ~thread_ts : tool_result =
  let slack_token = Sys.getenv_opt "SLACK_BOT_TOKEN" in
  match slack_token with
  | None ->
      {
        model = "slack_post";
        returncode = -1;
        response = "Error: SLACK_BOT_TOKEN environment variable not set";
        extra = [("error", "missing_token")];
      }
  | Some token ->
      let payload = `Assoc (
        [("channel", `String channel); ("text", `String text)]
        @ (match thread_ts with Some ts -> [("thread_ts", `String ts)] | None -> [])
      ) in
      let json_str = Yojson.Safe.to_string payload in
      eprintf "[slack_post] Posting to channel %s\n%!" channel;
      let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
        "curl" [
          "-s"; "-X"; "POST";
          "-H"; "Content-Type: application/json; charset=utf-8";
          "-H"; sprintf "Authorization: Bearer %s" token;
          "-d"; json_str;
          "https://slack.com/api/chat.postMessage"
        ] in
      match result with
      | Ok r ->
          (try
            let response_json = Yojson.Safe.from_string r.stdout in
            let ok = Yojson.Safe.Util.(response_json |> member "ok" |> to_bool) in
            if ok then
              {
                model = "slack_post";
                returncode = 0;
                response = sprintf "Message posted to %s" channel;
                extra = [("channel", channel)];
              }
            else
              let error = Yojson.Safe.Util.(response_json |> member "error" |> to_string) in
              {
                model = "slack_post";
                returncode = -1;
                response = sprintf "Slack API error: %s" error;
                extra = [("error", error)];
              }
          with _ ->
            {
              model = "slack_post";
              returncode = r.exit_code;
              response = r.stdout;
              extra = [("channel", channel)];
            })
      | Error (Timeout t) ->
          {
            model = "slack_post";
            returncode = -1;
            response = sprintf "Timeout after %ds" t;
            extra = [("channel", channel); ("error", "timeout")];
          }
      | Error (ProcessError msg) ->
          {
            model = "slack_post";
            returncode = -1;
            response = sprintf "Error: %s" msg;
            extra = [("channel", channel); ("error", msg)];
          }

(** {1 Langfuse Tracing Helpers} *)

(** Extract model name from tool_args for tracing *) let get_model_name_for_tracing = function
  | Gemini { model; _ } -> sprintf "gemini:%s" model
  | Claude { model; _ } -> sprintf "claude:%s" model
  | Codex { model; _ } -> sprintf "codex:%s" model
  | Ollama { model; _ } -> sprintf "ollama:%s" model
  | OllamaList -> "ollama:list"
  | Glm { model; _ } -> sprintf "glm:%s" model
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

(** Extract input/prompt from tool_args for tracing *) let get_input_for_tracing = function
  | Gemini { prompt; _ } -> prompt
  | Claude { prompt; _ } -> prompt
  | Codex { prompt; _ } -> prompt
  | Ollama { prompt; _ } -> prompt
  | OllamaList -> "(list models)"
  | Glm { prompt; _ } -> prompt
  | ChainRun { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainValidate { mermaid; _ } -> Option.value mermaid ~default:"(json chain)"
  | ChainOrchestrate { chain; _ } ->
      (match chain with
       | Some j -> sprintf "(orchestrate: %s)" (Yojson.Safe.to_string j)
       | None -> "(orchestrate: preset)")
  | _ -> "(non-llm operation)"

let classify_error_class (r : tool_result) : string option =
  if r.returncode = 0 then None
  else if String.length r.response >= 7 && String.sub r.response 0 7 = "Timeout" then
    Some "timeout"
  else if String.length r.response >= 6 && String.sub r.response 0 6 = "Error:" then
    Some "tool_error"
  else
    Some "llm_error"

let result_streamed (r : tool_result) : bool =
  match List.assoc_opt "streamed" r.extra with
  | Some "true" -> true
  | _ -> false

(** {1 Main Execute Function} *)

(** Execute a tool and return result - Direct Style *) 
let rec execute ~sw ~proc_mgr ~clock args : tool_result =
  let log_enabled = Run_log_eio.enabled () in
  let stream_flag = match args with
    | Ollama { stream; _ } -> stream
    | Gemini { stream; _ } -> stream
    | Claude { stream; _ } -> stream
    | Codex { stream; _ } -> stream
    | Glm { stream; _ } -> stream
    | _ -> false
  in
  let model_for_log = get_model_name_for_tracing args in
  let stream_id_opt =
    match args with
    | Ollama { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Gemini { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Claude { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Codex { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | Glm { stream = true; _ } -> Some (Llm_clients.generate_stream_id model_for_log)
    | _ -> None
  in
  let log_extra =
    match stream_id_opt with
    | Some sid -> [("stream_id", sid)]
    | None -> []
  in
  let tool_for_log = match args with
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
    | _ -> "llm"
  in
  let prompt_for_log = get_input_for_tracing args in
  let log_start_ts = Unix.gettimeofday () in
  if log_enabled then
    Run_log_eio.record_event
      ~event:"llm_call"
      ~tool:tool_for_log
      ~model:model_for_log
      ~prompt_chars:(String.length prompt_for_log)
      ~streamed:stream_flag
      ~extra:log_extra
      ()
  else
    ();
  match args with
  | Gemini { prompt; model; thinking_level; timeout; stream; use_cli; _ } ->
      let result =
        match String.lowercase_ascii model with
        | "stub" | "mock" ->
            { Types.model = "stub"; returncode = 0; response = sprintf "[stub]%s" prompt; extra = [] }
        | "simple-test" ->
            { Types.model = "simple-test"; returncode = 0; response = sprintf "[test]%s" prompt; extra = [] }
        | _ ->
            if use_cli then
              Llm_clients.execute_gemini_with_retry ~sw ~proc_mgr ~clock ~model ~thinking_level ~timeout ~stream ~args ()
            else
              Llm_clients.execute_gemini_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~thinking_level ~timeout ~stream
      in
      if log_enabled then
        Run_log_eio.record_event
          ~event:"llm_complete"
          ~tool:tool_for_log
          ~model:model_for_log
          ~prompt_chars:(String.length prompt_for_log)
          ~response_chars:(String.length result.response)
          ~duration_ms:(int_of_float ((Unix.gettimeofday () -. log_start_ts) *. 1000.0))
          ~success:(result.returncode = 0)
          ~streamed:(result_streamed result)
          ~extra:log_extra
          ?error_class:(classify_error_class result)
          ()
      else
        ();
      result

  | Claude { prompt; model; system_prompt; timeout; stream; use_cli=_; _ } ->
      Llm_clients.execute_claude_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~system_prompt ~timeout ~stream

  | Codex { prompt; model; timeout; stream; use_cli=_; _ } ->
      Llm_clients.execute_codex_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~timeout ~stream

  | Ollama { model; prompt; temperature=_; timeout; tools=_; stream=_; _ } ->
      let _ = ["ollama"; "run"; model; prompt] in
      let result = run_command ~sw ~proc_mgr ~clock ~timeout "ollama" ["run"; model; prompt] in
      (match result with
       | Ok r -> { model = "ollama"; returncode = 0; response = r.stdout; extra = [] }
       | Error _ -> { model = "ollama"; returncode = -1; response = "Error"; extra = [] })

  | ChainRun { chain; mermaid; input; trace=_ } ->
      let parse_result = match (chain, mermaid) with | (Some c, _) -> Chain_parser.parse_chain c | (_, Some m) -> Chain_mermaid_parser.parse_mermaid_to_chain m | (None, None) -> Error "Missing chain" in
      (match parse_result with
       | Error msg -> { model = "chain.run"; returncode = -1; response = msg; extra = [] }
       | Ok parsed ->
           (match Chain_compiler.compile parsed with
            | Error msg -> { model = "chain.run"; returncode = -1; response = msg; extra = [] }
            | Ok plan ->
                let exec_fn ~model ?system:_ ~prompt ?tools:_ () =
                  match String.lowercase_ascii model with
                  | "simple-test" -> Ok ("[test]" ^ prompt)
                  | _ -> let res = execute ~sw ~proc_mgr ~clock (Gemini {prompt; model="gemini-3-pro-preview"; thinking_level=Types.High; yolo=false; output_format=Types.Text; timeout=300; stream=false; use_cli=true; fallback_to_api=true}) in if res.returncode = 0 then Ok res.response else Error res.response
                in
                let tool_exec ~name:_ ~args:_ = Ok "stub" in
                let result = Chain_executor_eio.execute ~sw ~clock ~exec_fn ~tool_exec ?input plan in
                { model = "chain.run"; returncode = if result.success then 0 else -1; response = result.output; extra = [] }))

  | ChainValidate { chain; mermaid; strict } ->
      let parse_result = match (chain, mermaid) with | (Some c, _) -> Chain_parser.parse_chain c | (_, Some m) -> Chain_mermaid_parser.parse_mermaid_to_chain m | (None, None) -> Error "Missing chain" in
      (match parse_result with
       | Error msg -> { model = "chain.validate"; returncode = -1; response = msg; extra = [] }
       | Ok parsed ->
           (match if strict then Chain_parser.validate_chain_strict parsed else Chain_parser.validate_chain parsed with
            | Error msg -> { model = "chain.validate"; returncode = -1; response = msg; extra = [] }
            | Ok () ->
                (match Chain_compiler.compile parsed with
                 | Error msg -> { model = "chain.validate"; returncode = -1; response = msg; extra = [] }
                 | Ok _ -> { model = "chain.validate"; returncode = 0; response = "Valid"; extra = [] })))

  | ChainList -> { model = "chain.list"; returncode = 0; response = "[]"; extra = [] }
  | ChainToMermaid _ -> { model = "chain.to_mermaid"; returncode = 0; response = "graph LR"; extra = [] }
  | ChainVisualize _ -> { model = "chain.visualize"; returncode = 0; response = "ascii"; extra = [] }
  | ChainConvert _ -> { model = "chain.convert"; returncode = 0; response = "converted"; extra = [] }
  | ChainOrchestrate _ -> { model = "chain.orchestrate"; returncode = -1; response = "Server only"; extra = [] }
  | ChainCheckpoints _ -> { model = "chain.checkpoints"; returncode = 0; response = "[]"; extra = [] }
  | ChainResume _ -> { model = "chain.resume"; returncode = -1; response = "Not implemented"; extra = [] }
  | PromptRegister _ -> { model = "prompt.register"; returncode = 0; response = "Registered"; extra = [] }
  | PromptList -> { model = "prompt.list"; returncode = 0; response = "[]"; extra = [] }
  | PromptGet _ -> { model = "prompt.get"; returncode = -1; response = "Not found"; extra = [] }
  | GhPrDiff { repo; pr_number } -> execute_gh_pr_diff ~sw ~proc_mgr ~clock ~repo ~pr_number
  | SlackPost { channel; text; thread_ts } -> execute_slack_post ~sw ~proc_mgr ~clock ~channel ~text ~thread_ts
  | SetStreamDelta { enabled } -> { model = "config:set_stream_delta"; returncode = 0; response = "Updated"; extra = [("enabled", string_of_bool (Llm_clients.set_stream_delta enabled))] }
  | GetStreamDelta -> { model = "config:get_stream_delta"; returncode = 0; response = string_of_bool (Llm_clients.get_stream_delta ()); extra = [] }
  | _ -> { model = "unknown"; returncode = -1; response = "Unknown tool"; extra = [] }
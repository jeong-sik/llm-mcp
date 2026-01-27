(** Tool Execution - Main dispatcher for all MCP tools *)

open Printf
open Types
open Cli_runner_eio

module Mcp_client = Mcp_client
module Llm_clients = Llm_clients

let external_tool_timeout = 60

let execute_gh_pr_diff ~sw ~proc_mgr ~clock ~repo ~pr_number : tool_result =
  eprintf "[gh_pr_diff] Fetching diff for %s#%d\n%!" repo pr_number;
  let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
    "gh" ["pr"; "diff"; string_of_int pr_number; "-R"; repo] in
  match result with
  | Ok r -> { model = "gh_pr_diff"; returncode = r.exit_code; response = get_output r; extra = [("repo", repo); ("pr_number", string_of_int pr_number)]; }
  | Error _ -> { model = "gh_pr_diff"; returncode = -1; response = "Error"; extra = [] }

let execute_slack_post ~sw ~proc_mgr ~clock ~channel ~text ~thread_ts:_ : tool_result =
  let slack_token = Sys.getenv_opt "SLACK_BOT_TOKEN" in
  match slack_token with
  | None -> { model = "slack_post"; returncode = -1; response = "Error: Token missing"; extra = [] }
  | Some token ->
      let payload = `Assoc [("channel", `String channel); ("text", `String text)] in
      let json_str = Yojson.Safe.to_string payload in
      let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
        "curl" ["-s"; "-X"; "POST"; "-H"; "Authorization: Bearer " ^ token; "-d"; json_str; "https://slack.com/api/chat.postMessage"] in
      match result with
      | Ok _ -> { model = "slack_post"; returncode = 0; response = "Posted"; extra = [] }
      | Error _ -> { model = "slack_post"; returncode = -1; response = "Error"; extra = [] }

let get_model_name_for_tracing = function
  | Gemini { model; _ } -> sprintf "gemini:%s" model
  | Claude { model; _ } -> sprintf "claude:%s" model
  | Codex { model; _ } -> sprintf "codex:%s" model
  | Ollama { model; _ } -> sprintf "ollama:%s" model
  | ChainRun _ -> "chain:run"
  | _ -> "llm"

let execute ~sw ~proc_mgr ~clock args : tool_result =
  match args with
  | Gemini { prompt; model; thinking_level; timeout; stream; use_cli; _ } ->
      (match String.lowercase_ascii model with
       | "simple-test" -> { model = "simple-test"; returncode = 0; response = "[test]" ^ prompt; extra = [] }
       | _ -> 
           if use_cli then Llm_clients.execute_gemini_with_retry ~sw ~proc_mgr ~clock ~model ~thinking_level ~timeout ~stream ~args ()
           else Llm_clients.execute_gemini_direct_api ~sw ~proc_mgr ~clock ~model ~prompt ~thinking_level ~timeout ~stream)
  | ChainRun { chain; mermaid; input; trace=_ } ->
      let parse_result = match (chain, mermaid) with | (Some c, _) -> Chain_parser.parse_chain c | (_, Some m) -> Chain_mermaid_parser.parse_mermaid_to_chain m | (None, None) -> Error "Missing chain" in
      (match parse_result with
       | Error msg -> { model = "chain.run"; returncode = -1; response = msg; extra = [] }
       | Ok parsed ->
           (match Chain_compiler.compile parsed with
            | Error msg -> { model = "chain.run"; returncode = -1; response = msg; extra = [] }
            | Ok plan ->
                let exec_fn ~model:_ ?system:_ ~prompt:_ ?tools:_ () = 
                  Ok "[test]Hello"
                in
                let tool_exec ~name:_ ~args:_ = Ok "stub" in
                let result = Chain_executor_eio.execute ~sw ~clock ~exec_fn ~tool_exec ?input plan in
                { model = "chain.run"; returncode = if result.success then 0 else -1; response = result.output; extra = [] }))
  | _ -> { model = "stub"; returncode = 0; response = "Not implemented in test version"; extra = [] }
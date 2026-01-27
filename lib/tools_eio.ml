(** MAGI Trinity tool implementations - Eio Direct-Style

    Pure Eio version of tools.ml using:
    - Direct-style (no monadic bind)
    - Cli_runner_eio for subprocess execution
    - Eio.Time for delays
    - Structured concurrency

    This module has been refactored to delegate to submodules in lib/tools/:
    - Mcp_client: MCP client logic
    - Llm_clients: LLM API integrations
    - Tool_exec: Main tool execution dispatcher
*)

(** {1 Re-exports from Tool_parsers (Pure, no Lwt)} *)

(* Pure functions - from Tool_parsers module *)
let budget_mode_value = Tool_parsers.budget_mode_value
let parse_gemini_args = Tool_parsers.parse_gemini_args
let parse_claude_args = Tool_parsers.parse_claude_args
let parse_codex_args = Tool_parsers.parse_codex_args
let parse_ollama_args = Tool_parsers.parse_ollama_args
let parse_ollama_list_args = Tool_parsers.parse_ollama_list_args
let parse_glm_args = Tool_parsers.parse_glm_args
let parse_chain_run_args = Tool_parsers.parse_chain_run_args
let parse_chain_validate_args = Tool_parsers.parse_chain_validate_args
let parse_chain_to_mermaid_args = Tool_parsers.parse_chain_to_mermaid_args
let parse_chain_visualize_args = Tool_parsers.parse_chain_visualize_args
let parse_chain_convert_args = Tool_parsers.parse_chain_convert_args
let parse_chain_orchestrate_args = Tool_parsers.parse_chain_orchestrate_args
let parse_gh_pr_diff_args = Tool_parsers.parse_gh_pr_diff_args
let parse_slack_post_args = Tool_parsers.parse_slack_post_args
let parse_chain_checkpoints_args = Tool_parsers.parse_chain_checkpoints_args
let parse_chain_resume_args = Tool_parsers.parse_chain_resume_args
let parse_prompt_register_args = Tool_parsers.parse_prompt_register_args
let parse_prompt_get_args = Tool_parsers.parse_prompt_get_args
let parse_set_stream_delta_args = Tool_parsers.parse_set_stream_delta_args
let parse_get_stream_delta_args = Tool_parsers.parse_get_stream_delta_args
let build_gemini_cmd = Tool_parsers.build_gemini_cmd
let build_claude_cmd = Tool_parsers.build_claude_cmd
let build_codex_cmd = Tool_parsers.build_codex_cmd
let build_ollama_curl_cmd = Tool_parsers.build_ollama_curl_cmd
let parse_ollama_response = Tool_parsers.parse_ollama_response
let parse_ollama_chunk = Tool_parsers.parse_ollama_chunk
let clean_codex_output = Tool_parsers.clean_codex_output
let exponential_backoff ~base_delay attempt =
  base_delay *. (2. ** float_of_int attempt)

(* MCP config helpers *)
let get_mcp_server_url = Tool_config.get_mcp_server_url
let get_mcp_server_config = Tool_config.get_mcp_server_config

(* Re-export from Mcp_client *)
let parse_mcp_http_response = Mcp_client.parse_mcp_http_response
let call_external_mcp = Mcp_client.call_external_mcp
let call_stdio_mcp = Mcp_client.call_stdio_mcp
let call_mcp = Mcp_client.call_mcp
let masc_enabled = Mcp_client.masc_enabled
let masc_agent_base = Mcp_client.masc_agent_base
let masc_heartbeat_interval = Mcp_client.masc_heartbeat_interval
let masc_available = Mcp_client.masc_available
let call_masc_tool = Mcp_client.call_masc_tool
let with_masc_hook = Mcp_client.with_masc_hook

(* Re-export from Llm_clients *)
let execute_gemini_direct_api = Llm_clients.execute_gemini_direct_api
let execute_claude_direct_api = Llm_clients.execute_claude_direct_api
let execute_codex_direct_api = Llm_clients.execute_codex_direct_api
let execute_cli_streaming = Llm_clients.execute_cli_streaming
let execute_ollama_streaming = Llm_clients.execute_ollama_streaming
let execute_gemini_with_retry = Llm_clients.execute_gemini_with_retry
let get_stream_delta = Llm_clients.get_stream_delta
let set_stream_delta = Llm_clients.set_stream_delta

(* Re-export from Tool_exec *)
let execute = Tool_exec.execute
let execute_with_tracing ~sw ~proc_mgr ~clock args =
  (* Add Langfuse tracing if enabled *)
  execute ~sw ~proc_mgr ~clock args

(* Convenience wrappers *)
let execute_with_env ~sw ~env args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  execute ~sw ~proc_mgr ~clock args

let execute_chain ~sw ~proc_mgr ~clock ~chain_json ~trace ~timeout:_timeout =
  execute ~sw ~proc_mgr ~clock (ChainRun { chain=Some chain_json; mermaid=None; input=None; trace })
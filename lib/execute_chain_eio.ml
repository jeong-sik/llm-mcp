(** Chain Tool Execution - Eio Direct-Style

    Extracted from tools_eio.ml for maintainability.
    Contains: ChainRun, ChainValidate, ChainList, ChainToMermaid,
              ChainVisualize, ChainConvert, ChainOrchestrate

    Each function receives an ~execute parameter to avoid circular dependency
    with the main execute function in tools_eio.ml.
*)

open Printf
open Types

(** Type alias for the execute function *)
type execute_fn =
  sw:Eio.Switch.t ->
  proc_mgr:[`Proc] Eio.Process.mgr_ty Eio.Resource.t ->
  clock:[`Clock] Eio.Time.clock_ty Eio.Resource.t ->
  tool_args ->
  tool_result

(** Type alias for MCP call function *)
type mcp_call_fn =
  sw:Eio.Switch.t ->
  proc_mgr:[`Proc] Eio.Process.mgr_ty Eio.Resource.t ->
  clock:[`Clock] Eio.Time.clock_ty Eio.Resource.t ->
  server_name:string ->
  tool_name:string ->
  arguments:Yojson.Safe.t ->
  timeout:int ->
  string

(* ============================================================================
   Helper Functions
   ============================================================================ *)

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let split_tool_name name =
  let delim_idx =
    match String.index_opt name '.' with
    | Some idx -> Some idx
    | None -> String.index_opt name ':'
  in
  match delim_idx with
  | None -> None
  | Some idx ->
      let server = String.sub name 0 idx in
      let tool_len = String.length name - idx - 1 in
      if server = "" || tool_len <= 0 then None
      else Some (server, String.sub name (idx + 1) tool_len)

(* ============================================================================
   ChainRun
   ============================================================================ *)

let execute_chain_run
    ~sw ~proc_mgr ~clock ~execute ~call_mcp
    ~chain ~mermaid ~input ~trace ~node_timeout =
  (* Parse from either JSON or Mermaid (WYSIWYE) *)
  let parse_result = match (chain, mermaid) with
    | (Some c, _) -> Chain_parser.parse_chain c
    | (_, Some m) -> Chain_mermaid_parser.parse_mermaid_to_chain m
    | (None, None) -> Error "Either 'chain' (JSON) or 'mermaid' (string) is required"
  in
  match parse_result with
  | Error msg ->
      { model = "chain.run";
        returncode = -1;
        response = sprintf "Parse error: %s" msg;
        extra = [("stage", "parse")]; }
  | Ok parsed_chain ->
      match Chain_compiler.compile parsed_chain with
      | Error msg ->
          { model = "chain.run";
            returncode = -1;
            response = sprintf "Compile error: %s" msg;
            extra = [("stage", "compile")]; }
  | Ok plan ->
      if Run_log_eio.enabled () then
        Run_log_eio.record_event
          ~event:"chain_start"
          ~chain_id:plan.chain.Chain_types.id
          ~model:"chain.run"
          ~tool:"chain.run"
          ~prompt_chars:(String.length (Option.value mermaid ~default:""))
          ()
      else
        ();
          (* Convert Yojson.Safe.t tools to tool_schema list option *)
          let parse_tools tools =
            match tools with
            | None -> None
            | Some json ->
                let open Yojson.Safe.Util in
                match json with
                | `List items ->
                    let schemas = List.filter_map (fun item ->
                      try
                        Some {
                          Types.name = item |> member "name" |> to_string;
                          description = item |> member "description" |> to_string;
                          input_schema = item |> member "input_schema";
                        }
                      with _ -> None
                    ) items in
                    if schemas = [] then None else Some schemas
                | _ -> None
          in
          (* Create exec_fn that routes to appropriate LLM *)
          let exec_fn ~model ?system ~prompt ?tools () =
            let _ = system in  (* Unused for now *)
            let parsed_tools = parse_tools tools in
            let args = match String.lowercase_ascii model with
              | "stub" | "mock" ->
                  Types.Gemini {
                    prompt;
                    model = "stub";
                    thinking_level = Types.Low;
                    yolo = false;
                    output_format = Types.Text;
                    timeout = node_timeout;
                    stream = false;
                  }
              | "gemini" | "gemini-3-pro-preview" | "gemini-2.5-pro" ->
                  Types.Gemini {
                    prompt;
                    model = "gemini-3-pro-preview";
                    thinking_level = Types.High;
                    yolo = false;
                    output_format = Types.Text;
                    timeout = node_timeout;
                    stream = false;
                  }
              | "claude" | "opus" | "opus-4" | "sonnet" | "haiku" | "haiku-4.5" ->
                  Types.Claude {
                    prompt;
                    model;
                    long_context = true;
                    system_prompt = None;
                    output_format = Types.Text;
                    allowed_tools = [];
                    working_directory = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp";
                    timeout = node_timeout;
                    stream = false;
                  }
              | "codex" | "gpt-5.2" ->
                  Types.Codex {
                    prompt;
                    model = "gpt-5.2";
                    reasoning_effort = Types.RXhigh;
                    sandbox = Types.WorkspaceWrite;
                    working_directory = None;
                    timeout = node_timeout;
                    stream = false;
                  }
              | "ollama" ->
                  Types.Ollama {
                    prompt;
                    model = "qwen3:1.7b";
                    system_prompt = None;
                    temperature = 0.7;
                    timeout = node_timeout;
                    stream = false;
                    tools = parsed_tools;
                  }
              | m when String.length m > 7 && String.sub m 0 7 = "ollama:" ->
                  let ollama_model = String.sub m 7 (String.length m - 7) in
                  Types.Ollama {
                    prompt;
                    model = ollama_model;
                    system_prompt = None;
                    temperature = 0.7;
                    timeout = node_timeout;
                    stream = false;
                    tools = parsed_tools;
                  }
              | _ ->
                  Types.Gemini {
                    prompt;
                    model = "gemini-3-pro-preview";
                    thinking_level = Types.High;
                    yolo = false;
                    output_format = Types.Text;
                    timeout = node_timeout;
                    stream = false;
                  }
            in
            match args with
            | Types.Gemini { model = "stub"; _ } ->
                Ok (Printf.sprintf "[stub]%s" prompt)
            | _ ->
                let result = execute ~sw ~proc_mgr ~clock args in
                if result.returncode = 0 then Ok result.response
                else Error result.response
          in
          let tool_exec ~name ~args =
            match split_tool_name name with
            | Some (server_name, tool_name) ->
                let output =
                  call_mcp ~sw ~proc_mgr ~clock
                    ~server_name ~tool_name ~arguments:args ~timeout:node_timeout
                in
                if starts_with ~prefix:"Error:" output then Error output else Ok output
            | None ->
                let result =
                  match name with
                  | "gemini" ->
                      let args = Tool_parsers.parse_gemini_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "claude-cli" ->
                      let args = Tool_parsers.parse_claude_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "codex" ->
                      let args = Tool_parsers.parse_codex_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "ollama" ->
                      let args = Tool_parsers.parse_ollama_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "ollama_list" ->
                      let args = Tool_parsers.parse_ollama_list_args args in
                      execute ~sw ~proc_mgr ~clock args
                  | "echo" ->
                      let input = try args |> Yojson.Safe.Util.member "input" |> Yojson.Safe.Util.to_string
                                  with _ -> Yojson.Safe.to_string args in
                      { Types.model = "echo"; returncode = 0; response = input; extra = [] }
                  | "identity" ->
                      { Types.model = "identity"; returncode = 0; response = Yojson.Safe.to_string args; extra = [] }
                  | _ ->
                      { Types.model = "chain.tool";
                        returncode = -1;
                        response = sprintf "Unknown tool: %s" name;
                        extra = [("tool", name)];
                      }
                in
                if result.returncode = 0 then Ok result.response else Error result.response
          in
          let _ = input in
          let result = Chain_executor_eio.execute
            ~sw ~clock ~timeout:node_timeout ~trace ~exec_fn ~tool_exec ?input plan
          in
          if Run_log_eio.enabled () then
            Run_log_eio.record_event
              ~event:"chain_complete"
              ~chain_id:result.Chain_types.chain_id
              ~model:"chain.run"
              ~tool:"chain.run"
              ~duration_ms:result.Chain_types.duration_ms
              ~success:result.Chain_types.success
              ?error_class:(if result.Chain_types.success then None else Some "chain_error")
              ()
          else
            ();
          { model = "chain.run";
            returncode = if result.Chain_types.success then 0 else -1;
            response = result.Chain_types.output;
            extra = [
              ("chain_id", result.Chain_types.chain_id);
              ("duration_ms", string_of_int result.Chain_types.duration_ms);
              ("trace_count", string_of_int (List.length result.Chain_types.trace));
            ]; }

(* ============================================================================
   ChainValidate
   ============================================================================ *)

let execute_chain_validate ~chain ~mermaid ~strict =
  let parse_result = match (chain, mermaid) with
    | (Some c, _) -> Chain_parser.parse_chain c
    | (_, Some m) -> Chain_mermaid_parser.parse_mermaid_to_chain m
    | (None, None) -> Error "Either 'chain' (JSON) or 'mermaid' (string) is required"
  in
  match parse_result with
  | Error msg ->
      { model = "chain.validate";
        returncode = -1;
        response = sprintf "Parse error: %s" msg;
        extra = [("stage", "parse"); ("valid", "false")]; }
  | Ok parsed_chain ->
      let validation =
        if strict then Chain_parser.validate_chain_strict parsed_chain
        else Chain_parser.validate_chain parsed_chain
      in
      match validation with
      | Error msg ->
          { model = "chain.validate";
            returncode = -1;
            response = sprintf "Validation error: %s" msg;
            extra = [("stage", "validate"); ("valid", "false")]; }
      | Ok () ->
          match Chain_compiler.compile parsed_chain with
          | Error msg ->
              { model = "chain.validate";
                returncode = -1;
                response = sprintf "Compile error: %s" msg;
                extra = [("stage", "compile"); ("valid", "false")]; }
          | Ok plan ->
              let node_count = List.length parsed_chain.Chain_types.nodes in
              let depth = plan.Chain_types.depth in
              let parallel_groups = List.length plan.Chain_types.parallel_groups in
              { model = "chain.validate";
                returncode = 0;
                response = sprintf "Chain '%s' is valid: %d nodes, depth %d, %d parallel groups"
                  parsed_chain.Chain_types.id node_count depth parallel_groups;
                extra = [
                  ("valid", "true");
                  ("strict", if strict then "true" else "false");
                  ("chain_id", parsed_chain.Chain_types.id);
                  ("node_count", string_of_int node_count);
                  ("depth", string_of_int depth);
                  ("parallel_groups", string_of_int parallel_groups);
                ]; }

(* ============================================================================
   ChainList
   ============================================================================ *)

let execute_chain_list () =
  let ids = Chain_registry.list_ids () in
  let response = String.concat ", " ids in
  { model = "chain.list";
    returncode = 0;
    response = "Registered chains: " ^ response;
    extra = [("count", string_of_int (List.length ids))];
  }

(* ============================================================================
   ChainToMermaid
   ============================================================================ *)

let execute_chain_to_mermaid ~chain =
  match Chain_parser.parse_chain chain with
  | Error msg ->
      { model = "chain.to_mermaid";
        returncode = -1;
        response = sprintf "Parse error: %s" msg;
        extra = [("stage", "parse")]; }
  | Ok parsed_chain ->
      let mermaid_text = Chain_mermaid_parser.chain_to_mermaid parsed_chain in
      { model = "chain.to_mermaid";
        returncode = 0;
        response = mermaid_text;
        extra = [
          ("chain_id", parsed_chain.Chain_types.id);
          ("node_count", string_of_int (List.length parsed_chain.Chain_types.nodes));
        ]; }

(* ============================================================================
   ChainVisualize
   ============================================================================ *)

let execute_chain_visualize ~chain =
  match Chain_parser.parse_chain chain with
  | Error msg ->
      { model = "chain.visualize";
        returncode = -1;
        response = sprintf "Parse error: %s" msg;
        extra = [("stage", "parse")]; }
  | Ok parsed_chain ->
      let ascii_text = Chain_mermaid_parser.chain_to_ascii parsed_chain in
      { model = "chain.visualize";
        returncode = 0;
        response = ascii_text;
        extra = [
          ("chain_id", parsed_chain.Chain_types.id);
          ("node_count", string_of_int (List.length parsed_chain.Chain_types.nodes));
          ("output", parsed_chain.Chain_types.output);
        ]; }

(* ============================================================================
   ChainConvert
   ============================================================================ *)

let execute_chain_convert ~from_format ~to_format ~input ~pretty =
  match (from_format, to_format) with
  | ("json", "mermaid") ->
      (match Chain_parser.parse_chain input with
       | Error msg ->
           { model = "chain.convert";
             returncode = -1;
             response = sprintf "JSON parse error: %s" msg;
             extra = [("from", "json"); ("to", "mermaid"); ("stage", "parse")]; }
       | Ok chain ->
           let mermaid = Chain_mermaid_parser.chain_to_mermaid chain in
           { model = "chain.convert";
             returncode = 0;
             response = mermaid;
             extra = [
               ("from", "json"); ("to", "mermaid");
               ("chain_id", chain.Chain_types.id);
               ("node_count", string_of_int (List.length chain.Chain_types.nodes));
             ]; })

  | ("mermaid", "json") ->
      let mermaid_text = match input with
        | `String s -> s
        | _ -> Yojson.Safe.to_string input
      in
      (match Chain_mermaid_parser.parse_mermaid_to_chain mermaid_text with
       | Error msg ->
           { model = "chain.convert";
             returncode = -1;
             response = sprintf "Mermaid parse error: %s" msg;
             extra = [("from", "mermaid"); ("to", "json"); ("stage", "parse")]; }
       | Ok chain ->
           let json_str = Chain_parser.chain_to_json_string ~pretty chain in
           { model = "chain.convert";
             returncode = 0;
             response = json_str;
             extra = [
               ("from", "mermaid"); ("to", "json");
               ("chain_id", chain.Chain_types.id);
               ("node_count", string_of_int (List.length chain.Chain_types.nodes));
             ]; })

  | (f, t) when f = t ->
      { model = "chain.convert";
        returncode = -1;
        response = sprintf "Same format conversion (from=%s, to=%s) is not meaningful" f t;
        extra = [("from", f); ("to", t)]; }

  | (f, t) ->
      { model = "chain.convert";
        returncode = -1;
        response = sprintf "Unsupported conversion: %s → %s (supported: json↔mermaid)" f t;
        extra = [("from", f); ("to", t)]; }

(* ============================================================================
   ChainOrchestrate
   ============================================================================ *)

let execute_chain_orchestrate
    ~sw ~proc_mgr ~clock ~execute ~call_mcp
    ~goal ~chain ~tasks ~max_replans ~timeout ~trace ~verify_on_complete ~orchestrator_model =
  let config : Chain_orchestrator_eio.orchestration_config = {
    max_replans;
    timeout_ms = timeout * 1000;
    trace_enabled = trace;
    verify_on_complete;
  } in

  (* Create llm_call adapter *)
  let llm_call ~prompt =
    match orchestrator_model with
    | "stub" ->
        Printf.sprintf {|Here is a chain to accomplish the goal:

```json
{
  "id": "stub_chain_%d",
  "nodes": [
    {"id": "step1", "type": "llm", "model": "stub", "prompt": "Process: %s"}
  ],
  "output": "step1",
  "config": {"timeout": 30, "trace": true}
}
```

This chain will execute the goal using a stub model.|}
          (Random.int 10000)
          (String.escaped (String.sub prompt 0 (min 50 (String.length prompt))))
    | "claude" | "claude-cli" ->
        let args = Types.Claude {
          prompt;
          model = "sonnet";
          long_context = false;
          system_prompt = Some "You are a chain orchestrator. Design, analyze, and verify workflows.";
          output_format = Types.Text;
          allowed_tools = [];
          working_directory = Sys.getcwd ();
          timeout = 120;
          stream = false;
        } in
        let result = execute ~sw ~proc_mgr ~clock args in
        if result.returncode = 0 then result.response
        else failwith (Printf.sprintf "Claude call failed: %s" result.response)
    | "codex" ->
        let args = Types.Codex {
          prompt;
          model = "gpt-5.2";
          reasoning_effort = Types.RHigh;
          sandbox = Types.WorkspaceWrite;
          working_directory = Some (Sys.getcwd ());
          timeout = 120;
          stream = false;
        } in
        let result = execute ~sw ~proc_mgr ~clock args in
        if result.returncode = 0 then result.response
        else failwith (Printf.sprintf "Codex call failed: %s" result.response)
    | "ollama" ->
        let args = Types.Ollama {
          prompt;
          model = "devstral";
          system_prompt = Some "You are a chain orchestrator. Design, analyze, and verify workflows.";
          temperature = 0.7;
          timeout = 120;
          stream = false;
          tools = None;
        } in
        let result = execute ~sw ~proc_mgr ~clock args in
        if result.returncode = 0 then result.response
        else failwith (Printf.sprintf "Ollama call failed: %s" result.response)
    | "gemini" | _ ->
        let args = Types.Gemini {
          prompt;
          model = "gemini-3-pro-preview";
          thinking_level = Types.High;
          yolo = false;
          output_format = Types.Text;
          timeout = 120;
          stream = false;
        } in
        let result = execute ~sw ~proc_mgr ~clock args in
        if result.returncode = 0 then result.response
        else failwith (Printf.sprintf "Gemini call failed: %s" result.response)
  in

  (* Create tool_exec adapter *)
  let tool_exec ~name ~args:tool_args =
    let node_timeout = timeout in
    match split_tool_name name with
    | Some (server_name, tool_name) ->
        let output = call_mcp ~sw ~proc_mgr ~clock ~server_name ~tool_name ~arguments:tool_args ~timeout:node_timeout in
        (try Yojson.Safe.from_string output with _ -> `String output)
    | None ->
        let result = match name with
          | "gemini" ->
              let parsed = Tool_parsers.parse_gemini_args tool_args in
              execute ~sw ~proc_mgr ~clock parsed
          | "claude-cli" | "claude" ->
              let parsed = Tool_parsers.parse_claude_args tool_args in
              execute ~sw ~proc_mgr ~clock parsed
          | "codex" ->
              let parsed = Tool_parsers.parse_codex_args tool_args in
              execute ~sw ~proc_mgr ~clock parsed
          | "ollama" ->
              let parsed = Tool_parsers.parse_ollama_args tool_args in
              execute ~sw ~proc_mgr ~clock parsed
          | "echo" ->
              let input = try tool_args |> Yojson.Safe.Util.member "input" |> Yojson.Safe.Util.to_string
                          with _ -> Yojson.Safe.to_string tool_args in
              { model = "echo"; returncode = 0; response = input; extra = [] }
          | "identity" ->
              { model = "identity"; returncode = 0; response = Yojson.Safe.to_string tool_args; extra = [] }
          | _ ->
              { model = name; returncode = 1; response = Printf.sprintf "Unknown tool: %s" name; extra = [] }
        in
        if result.returncode = 0 then
          (try Yojson.Safe.from_string result.response with _ -> `String result.response)
        else
          `Assoc [("error", `String result.response)]
  in

  let parse_tasks_from_json (json: Yojson.Safe.t) =
    match json with
    | `List items ->
        List.filter_map (fun item ->
          match Chain_composer.masc_task_of_yojson item with
          | Ok t -> Some t
          | Error _ -> None
        ) items
    | _ -> []
  in

  (* Create tasks from explicit tasks list or from chain if provided *)
  let tasks_from_input = match tasks with
    | Some t -> parse_tasks_from_json t
    | None -> []
  in
  let tasks_from_chain = match chain with
    | Some chain_json ->
        (try
          let open Yojson.Safe.Util in
          let nodes = chain_json |> member "nodes" |> to_list in
          List.mapi (fun i node ->
            let title = node |> member "id" |> to_string_option |> Option.value ~default:(Printf.sprintf "task-%d" i) in
            Chain_composer.{
              task_id = Printf.sprintf "task-%03d" (i + 1);
              title;
              description = Some (Yojson.Safe.to_string node);
              priority = i + 1;
              status = "todo";
              assignee = None;
              metadata = [];
            }
          ) nodes
        with _ -> [])
    | None -> []
  in
  let tasks = if tasks_from_input <> [] then tasks_from_input else tasks_from_chain in

  (* Run orchestration *)
  match Chain_orchestrator_eio.orchestrate ~sw ~clock ~config ~llm_call ~tool_exec ~goal ~tasks with
  | Ok result ->
      let metrics_json = match result.final_metrics with
        | Some m -> Chain_evaluator.chain_metrics_to_yojson m |> Yojson.Safe.to_string
        | None -> "null"
      in
      let verification_json = match result.verification with
        | Some v -> Chain_evaluator.verification_result_to_yojson v |> Yojson.Safe.to_string
        | None -> "null"
      in
      { model = "chain.orchestrate";
        returncode = if result.success then 0 else 1;
        response = result.summary;
        extra = [
          ("success", string_of_bool result.success);
          ("total_replans", string_of_int result.total_replans);
          ("metrics", metrics_json);
          ("verification", verification_json);
        ]; }
  | Error err ->
      let error_msg = match err with
        | Chain_orchestrator_eio.DesignFailed msg -> Printf.sprintf "Design failed: %s" msg
        | Chain_orchestrator_eio.CompileFailed msg -> Printf.sprintf "Compile failed: %s" msg
        | Chain_orchestrator_eio.ExecutionFailed msg -> Printf.sprintf "Execution failed: %s" msg
        | Chain_orchestrator_eio.VerificationFailed msg -> Printf.sprintf "Verification failed: %s" msg
        | Chain_orchestrator_eio.MaxReplansExceeded -> "Max replans exceeded"
        | Chain_orchestrator_eio.Timeout -> "Orchestration timeout"
      in
      { model = "chain.orchestrate";
        returncode = 1;
        response = error_msg;
        extra = [("error_type", Yojson.Safe.to_string (Chain_orchestrator_eio.orchestration_error_to_yojson err))]; }

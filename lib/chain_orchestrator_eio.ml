(** Chain Orchestrator - Integration Layer (Eio)

    This module bridges the Neural Layer (Composer) with the Symbolic Layer (Conductor).
    It orchestrates the full lifecycle:

    1. Design: Composer analyzes tasks → generates Chain DSL
    2. Compile: Parser + Compiler → Execution Plan
    3. Execute: Conductor runs the plan with Eio fibers
    4. Verify: Composer evaluates completion
    5. Decide: Continue / Replan / Complete / Abort

    Architecture:
    ┌─────────────────────────────────────────────────────────────┐
    │                 ORCHESTRATOR (Integration Layer)            │
    │  ┌─────────────────────────────────────────────────────┐   │
    │  │                 orchestrate_session                  │   │
    │  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌────────┐ │   │
    │  │  │ Design  │→ │ Compile │→ │ Execute │→ │ Verify │ │   │
    │  │  │(Composer)│  │(Compiler)│  │(Conduct)│  │(Compos)│ │   │
    │  │  └─────────┘  └─────────┘  └─────────┘  └────────┘ │   │
    │  │        ↑                                     │       │   │
    │  │        └──────── Replan Loop ←───────────────┘       │   │
    │  └─────────────────────────────────────────────────────┘   │
    └─────────────────────────────────────────────────────────────┘
*)

open Chain_types
open Chain_composer
open Chain_evaluator

(** LLM call function type - provided by caller *)
type llm_call = prompt:string -> string

(** Tool execution function type *)
type tool_exec = name:string -> args:Yojson.Safe.t -> Yojson.Safe.t

(** Orchestration result *)
type orchestration_result = {
  success: bool;
  final_metrics: chain_metrics option;
  verification: verification_result option;
  total_replans: int;
  summary: string;
}
[@@deriving yojson]

(** Orchestration error *)
type orchestration_error =
  | DesignFailed of string
  | CompileFailed of string
  | ExecutionFailed of string
  | VerificationFailed of string
  | MaxReplansExceeded
  | Timeout
[@@deriving yojson]

(** Orchestration configuration *)
type orchestration_config = {
  max_replans: int;           (** Maximum re-planning attempts *)
  timeout_ms: int;            (** Overall timeout *)
  trace_enabled: bool;        (** Enable execution tracing *)
  verify_on_complete: bool;   (** Run LLM verification on completion *)
}

let default_config = {
  max_replans = 3;
  timeout_ms = 300_000;  (* 5 minutes *)
  trace_enabled = true;
  verify_on_complete = true;
}

(** Parse chain design from LLM response *)
let parse_chain_design (response: string) : (chain, string) result =
  (* Try to extract Mermaid graph or JSON from response anywhere in the text *)
  (* NOTE: Must use regular string for \n to be interpreted as newline *)
  let mermaid_pattern = Str.regexp "```mermaid\n\\(graph[^`]+\\)```" in
  let json_pattern = Str.regexp "```json\n\\([^`]+\\)```" in

  try
    let _ = Str.search_forward mermaid_pattern response 0 in
    let mermaid_code = Str.matched_group 1 response in
    match Chain_mermaid_parser.parse_chain mermaid_code with
    | Ok chain -> Ok chain
    | Error e -> Error (Printf.sprintf "Mermaid parse error: %s" e)
  with Not_found ->
    try
      let _ = Str.search_forward json_pattern response 0 in
      let json_str = Str.matched_group 1 response in
      match Yojson.Safe.from_string json_str |> Chain_parser.parse_chain with
      | Ok chain -> Ok chain
      | Error e -> Error (Printf.sprintf "JSON parse error: %s" e)
      | exception Yojson.Json_error e -> Error (Printf.sprintf "Invalid JSON: %s" e)
    with Not_found ->
      Error "No valid chain format found in LLM response"

(** Convert chain execution result to metrics *)
let result_to_metrics ~(chain_id: string) ~(goal: string) ~(started_at: float)
    ~(max_depth: int)
    (result: Chain_executor_eio.chain_result) : chain_metrics =
  let now = Unix.gettimeofday () in
  let node_metrics = List.map (fun (entry: Chain_types.trace_entry) ->
    {
      node_id = entry.node_id;
      node_type = entry.node_type_name;
      status = (match entry.status with
        | `Success -> Succeeded
        | `Failure -> Failed
        | `Skipped -> Skipped);
      started_at = Some entry.start_time;
      completed_at = Some entry.end_time;
      duration_ms = int_of_float ((entry.end_time -. entry.start_time) *. 1000.0);
      estimated_duration_ms = None;
      retry_count = 0;  (* trace_entry doesn't track retries *)
      error_message = entry.error;
      output_preview = (match entry.output_preview with
        | Some o -> Some (String.sub o 0 (min 200 (String.length o)))
        | None -> None);
    }
  ) result.trace in

  let succeeded = List.length (List.filter (fun n -> n.status = Succeeded) node_metrics) in
  let failed = List.length (List.filter (fun n -> n.status = Failed) node_metrics) in
  let skipped = List.length (List.filter (fun n -> n.status = Skipped) node_metrics) in
  let total = List.length node_metrics in

  {
    chain_id;
    goal;
    started_at;
    completed_at = Some now;
    total_duration_ms = int_of_float ((now -. started_at) *. 1000.0);
    total_nodes = total;
    nodes_succeeded = succeeded;
    nodes_failed = failed;
    nodes_skipped = skipped;
    nodes_pending = 0;
    parallel_groups = 1;  (* TODO: calculate from chain structure *)
    max_depth;
    success_rate = if total > 0 then float_of_int succeeded /. float_of_int total else 0.0;
    parallelization_efficiency = 1.0;  (* TODO: calculate *)
    estimation_accuracy = 1.0;
    node_metrics;
    verification = None;
  }

(** Main orchestration function *)
let orchestrate
    ~sw
    ~clock
    ~(config: orchestration_config)
    ~(llm_call: llm_call)
    ~(tool_exec: tool_exec)
    ~(goal: string)
    ~(tasks: masc_task list)
    : (orchestration_result, orchestration_error) result =

  let session_id = Printf.sprintf "orch-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) in
  let state = ref (init_state ~session_id ~goal ~tasks ~max_replans:config.max_replans) in
  let started_at = Unix.gettimeofday () in

  (* Design phase: Get chain from LLM *)
  let design_chain () =
    let context = get_design_context !state in
    let response = llm_call ~prompt:context in
    parse_chain_design response
  in

  (* Execute phase: Run chain with Conductor *)
  let execute_chain (chain: chain) : (Chain_executor_eio.chain_result, string) result =
    match Chain_compiler.compile chain with
    | Error e -> Error (Printf.sprintf "Compile error: %s" e)
    | Ok plan ->
      (* Create execution function for LLM nodes *)
      let exec_fn ~model:_ ~prompt =
        let result = llm_call ~prompt in
        Ok result
      in

      (* Create tool execution wrapper *)
      let tool_exec_fn ~name ~args =
        let result = tool_exec ~name ~args in
        Ok (Yojson.Safe.to_string result)
      in

      Ok (Chain_executor_eio.execute
        ~sw ~clock
        ~timeout:(config.timeout_ms / 1000)
        ~trace:config.trace_enabled
        ~exec_fn
        ~tool_exec:tool_exec_fn
        plan)
  in

  (* Verify phase: Check completion with LLM *)
  let verify_completion (metrics: chain_metrics) =
    if not config.verify_on_complete then
      None
    else begin
      let context = get_verification_context !state metrics in
      let response = llm_call ~prompt:context in
      Some (parse_verification_response response)
    end
  in

  (* Main orchestration loop *)
  let rec loop () =
    (* Check timeout *)
    let elapsed = Unix.gettimeofday () -. started_at in
    if elapsed *. 1000.0 > float_of_int config.timeout_ms then
      Error Timeout
    else begin
      (* Design *)
      match design_chain () with
      | Error e -> Error (DesignFailed e)
      | Ok chain ->
        state := set_chain !state chain;

        (* Execute *)
        match execute_chain chain with
        | Error e -> Error (ExecutionFailed e)
        | Ok exec_result ->
          let max_depth = chain.config.max_depth in
          let metrics = result_to_metrics ~chain_id:session_id ~goal ~started_at ~max_depth exec_result in

          if not exec_result.success && metrics.nodes_failed > 0 then begin
          (* Execution had failures - check if we should replan *)
          let verification = verify_completion metrics in
          let decision = decide_next_action ~state:!state ~metrics ~verification in

          match decision with
          | Replan reason ->
            state := increment_replan !state;
            state := add_checkpoint !state
              ~trigger:OnFailure
              ~metrics
              ~decision:`Replan
              ~reason:(Printf.sprintf "Replanning due to: %s"
                (match reason with
                 | TaskFailed id -> Printf.sprintf "Task %s failed" id
                 | GoalNotAchieved -> "Goal not achieved"
                 | NewTaskAdded id -> Printf.sprintf "New task %s" id
                 | ContextChanged -> "Context changed"
                 | TimeoutApproaching -> "Timeout approaching"));
            loop ()  (* Retry with new design *)

          | Abort reason ->
            Error (ExecutionFailed reason)

          | Complete final_metrics ->
            Ok {
              success = true;
              final_metrics = Some final_metrics;
              verification;
              total_replans = !state.replan_count;
              summary = generate_summary !state;
            }

          | Continue ->
            (* Shouldn't happen after failure, but handle gracefully *)
            loop ()
        end
        else begin
          (* Success or no failures - verify completion *)
          let verification = verify_completion metrics in
          let decision = decide_next_action ~state:!state ~metrics ~verification in

          match decision with
          | Complete final_metrics ->
            state := finalize !state final_metrics;
            Ok {
              success = true;
              final_metrics = Some final_metrics;
              verification;
              total_replans = !state.replan_count;
              summary = generate_summary !state;
            }

          | Replan reason when !state.replan_count < config.max_replans ->
            state := increment_replan !state;
            state := add_checkpoint !state
              ~trigger:OnChainComplete
              ~metrics
              ~decision:`Replan
              ~reason:(Printf.sprintf "Replanning: %s"
                (match reason with
                 | TaskFailed id -> Printf.sprintf "Task %s failed" id
                 | GoalNotAchieved -> "Goal not achieved despite execution success"
                 | _ -> "Unknown reason"));
            loop ()

          | Replan _ ->
            Error MaxReplansExceeded

          | Abort reason ->
            Error (ExecutionFailed reason)

          | Continue ->
            (* Partial completion - continue monitoring *)
            Ok {
              success = metrics.success_rate >= 0.8;
              final_metrics = Some metrics;
              verification;
              total_replans = !state.replan_count;
              summary = generate_summary !state;
            }
        end
    end
  in

  loop ()

(** Simplified orchestration for quick tasks *)
let orchestrate_quick
    ~sw
    ~clock
    ~(llm_call: llm_call)
    ~(tool_exec: tool_exec)
    ~(goal: string)
    ~(tasks: masc_task list)
    : (orchestration_result, orchestration_error) result =
  orchestrate ~sw ~clock ~config:default_config ~llm_call ~tool_exec ~goal ~tasks

(** Create MASC tasks from simple string descriptions *)
let tasks_from_strings (descriptions: string list) : masc_task list =
  List.mapi (fun i desc ->
    {
      task_id = Printf.sprintf "task-%03d" (i + 1);
      title = desc;
      description = None;
      priority = i + 1;
      status = "todo";
      assignee = None;
      metadata = [];
    }
  ) descriptions

(** Pretty print orchestration result *)
let pp_result (result: orchestration_result) : string =
  Printf.sprintf {|
╔══════════════════════════════════════════════════════════════╗
║                 ORCHESTRATION RESULT                          ║
╠══════════════════════════════════════════════════════════════╣
║ Success: %s                                                   ║
║ Replans: %d                                                   ║
╠══════════════════════════════════════════════════════════════╣
%s
╠══════════════════════════════════════════════════════════════╣
%s
╚══════════════════════════════════════════════════════════════╝
|}
    (if result.success then "YES ✅" else "NO ❌")
    result.total_replans
    (match result.final_metrics with
     | Some m -> Printf.sprintf "║ Nodes: %d total | %d ✅ | %d ❌ | %d ⏭️"
         m.total_nodes m.nodes_succeeded m.nodes_failed m.nodes_skipped
     | None -> "║ (no metrics)")
    (match result.verification with
     | Some v -> Printf.sprintf "║ Verified: %s (%.0f%% confidence)\n║ %s"
         (if v.is_complete then "YES" else "NO")
         (v.confidence *. 100.0)
         v.reason
     | None -> "║ (not verified)")

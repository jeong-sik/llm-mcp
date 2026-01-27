(** Chain Executor - Eio-based Parallel Execution Engine (Refactored)

    Executes compiled Chain DSL plans using Eio fibers for concurrency.
    Supports recursive subgraph execution and trace generation.

    This module delegates state management to Chain_context and
    node execution to Chain_node_runner.
*)

open Chain_types
include Chain_context
open Chain_node_runner

(** {1 Type Aliases for External Modules} *)

type chain_result = Chain_types.chain_result
type execution_plan = Chain_types.execution_plan
type trace_entry = Chain_types.trace_entry

(** {1 Execution Entry Point} *)

(** Execute a compiled chain plan *)
let execute
    ~sw
    ~clock
    ?(timeout = 300)
    ?(trace = true)
    ?(exec_fn = fun ~model:_ ?system:_ ~prompt:_ ?tools:_ () -> Ok "stub")
    ?(tool_exec = fun ~name:_ ~args:_ -> Ok "stub")
    ?(input = "")
    ?checkpoint
    (plan : execution_plan) : chain_result =

  let start_time = Unix.gettimeofday () in
  let chain_id = plan.chain.id in
  
  (* Initialize Langfuse trace if enabled *)
  let langfuse_trace =
    if trace then Some (Langfuse.create_trace ~name:chain_id ())
    else None
  in

  (* Create execution context *)
  let ctx = make_context ~start_time ~trace_enabled:trace ~timeout ~chain_id ?langfuse_trace ?checkpoint () in
  
  (* Store initial input *)
  Hashtbl.add ctx.outputs "input" input;

  (* Handle resume from checkpoint *)
  let start_node_id = restore_from_checkpoint ctx ~chain_id in
  
  add_trace ctx chain_id (ChainStart { chain_id; mermaid_dsl = None });

  (* Build node lookup map *)
  let node_map = Hashtbl.create (List.length plan.chain.nodes) in
  List.iter (fun (n : node) -> Hashtbl.add node_map n.id n) plan.chain.nodes;

  (* Execute parallel groups sequentially *)
  let rec execute_groups groups =
    match groups with
    | [] -> Ok ""
    | group :: rest ->
        let result = execute_parallel_group ctx sw clock exec_fn tool_exec group node_map in
        match result with
        | Ok _ -> execute_groups rest
        | Error msg -> Error msg
  in

  (* Find starting point in groups if resuming *)
  let remaining_groups =
    match start_node_id with
    | Some node_id ->
        (* Find group containing this node and skip all groups before it *)
        let rec skip_to_node = function
          | [] -> []
          | group :: rest ->
              if List.mem node_id group then group :: rest
              else skip_to_node rest
        in
        skip_to_node plan.parallel_groups
    | None -> plan.parallel_groups
  in

  let execution_result = execute_groups remaining_groups in
  
  let end_time = Unix.gettimeofday () in
  let duration_ms = int_of_float ((end_time -. start_time) *. 1000.0) in
  let success = Result.is_ok execution_result in
  
  add_trace ctx chain_id (ChainComplete { chain_id; success });

  (* Record to Langfuse if enabled *)
  (match langfuse_trace with
   | Some t -> Langfuse.end_trace t
   | None -> ());

  (* Build final result *)
  let final_output =
    match execution_result with
    | Ok _ ->
        (match Hashtbl.find_opt ctx.outputs plan.chain.output with
         | Some out -> out
         | None -> "No output produced")
    | Error msg -> msg
  in

  {
    chain_id;
    success;
    output = final_output;
    duration_ms;
    token_usage = {
      Chain_types.prompt_tokens = ctx.total_tokens.prompt_tokens;
      completion_tokens = ctx.total_tokens.completion_tokens;
      total_tokens = ctx.total_tokens.total_tokens;
      estimated_cost_usd = ctx.total_tokens.estimated_cost_usd;
    };
    trace = List.rev !(ctx.traces) |> List.map (fun t -> 
      (* Use a generic node type name for the trace as we don't have node_map here *)
      Chain_context.trace_to_entry t "node"
    );
    metadata = [
      ("run_id", ctx.checkpoint.run_id);
    ];
  }
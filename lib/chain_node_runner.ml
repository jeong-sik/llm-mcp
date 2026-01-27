(** Chain Node Runner - Core execution logic for specific node types *)

open Printf
open Chain_types
open Chain_context

(** {1 Helper Types for Node Execution} *)

type exec_fn = model:string -> ?system:string -> prompt:string -> ?tools:Yojson.Safe.t -> unit -> (string, string) result
type tool_exec = name:string -> args:Yojson.Safe.t -> (string, string) result

(** {1 Recursive Execution} *)

let rec execute_node ctx sw clock exec_fn tool_exec (node : Chain_types.node) : (string, string) result =
  match node.node_type with
  | Llm _ -> execute_llm_node ctx exec_fn node node.node_type
  | Tool _ -> execute_tool_node ctx tool_exec node node.node_type
  | Pipeline nodes -> execute_pipeline ctx sw clock exec_fn tool_exec node nodes
  | Fanout nodes -> execute_fanout ctx sw clock exec_fn tool_exec node nodes
  | Quorum { required; nodes } -> execute_quorum ctx sw clock exec_fn tool_exec node required nodes
  | Gate { condition; then_node; else_node } ->
      execute_gate ctx sw clock exec_fn tool_exec node condition then_node else_node
  | Subgraph chain -> execute_subgraph ctx sw clock exec_fn tool_exec node chain
  | ChainRef ref_id ->
      (match Chain_registry.lookup ref_id with
       | Some referenced_chain -> execute_subgraph ctx sw clock exec_fn tool_exec node referenced_chain
       | None -> Error (sprintf "ChainRef '%s' not found" ref_id))
  | Map { func; inner } -> execute_map ctx sw clock exec_fn tool_exec node func inner
  | Bind { func; inner } -> execute_bind ctx sw clock exec_fn tool_exec node func inner
  | Merge { strategy; nodes } -> execute_merge ctx sw clock exec_fn tool_exec node strategy nodes
  | Threshold { metric; operator; value; input_node; on_pass; on_fail } ->
      execute_threshold ctx sw clock exec_fn tool_exec node metric operator value input_node on_pass on_fail
  | GoalDriven gd -> execute_goal_driven ctx sw clock exec_fn tool_exec node gd.goal_metric gd.goal_operator gd.goal_value gd.action_node gd.measure_func gd.max_iterations gd.strategy_hints gd.conversational gd.relay_models
  | Evaluator e -> execute_evaluator ctx sw clock exec_fn tool_exec node e.candidates e.scoring_func e.scoring_prompt e.select_strategy e.min_score
  | Retry r -> execute_retry ctx sw clock exec_fn tool_exec node r.node r.max_attempts r.backoff r.retry_on
  | Fallback f -> execute_fallback ctx sw clock exec_fn tool_exec node f.primary f.fallbacks
  | Race r -> execute_race ctx sw clock exec_fn tool_exec node r.nodes r.timeout
  | ChainExec c -> execute_chain_exec ctx sw clock exec_fn tool_exec node c.chain_source c.validate c.max_depth c.sandbox c.context_inject c.pass_outputs
  | Adapter a -> execute_adapter ctx node a.input_ref a.transform a.on_error
  | Cache c -> execute_cache ctx sw clock exec_fn tool_exec node c.key_expr c.ttl_seconds c.inner
  | Batch b -> execute_batch ctx sw clock exec_fn tool_exec node b.batch_size b.parallel b.collect_strategy b.inner
  | Spawn s -> execute_spawn ctx sw clock exec_fn tool_exec node s.clean s.pass_vars s.inherit_cache s.inner
  | Mcts m -> execute_mcts ctx sw clock exec_fn tool_exec node m.strategies m.simulation m.evaluator m.evaluator_prompt m.policy m.max_iterations m.max_depth m.expansion_threshold m.early_stop m.parallel_sims
  | StreamMerge s -> execute_stream_merge ctx sw clock exec_fn tool_exec node s.nodes s.reducer s.initial s.min_results s.timeout
  | FeedbackLoop f -> execute_feedback_loop ctx sw clock exec_fn tool_exec node f.generator f.evaluator_config f.improver_prompt f.max_iterations f.score_threshold f.score_operator f.conversational f.relay_models
  | Masc_broadcast m -> execute_masc_broadcast ctx tool_exec node m.message m.room m.mention
  | Masc_listen l -> execute_masc_listen ctx clock tool_exec node l.filter l.timeout_sec l.room
  | Masc_claim c -> execute_masc_claim ctx tool_exec node c.task_id c.room

and add_trace ctx node_id event =
  if ctx.trace_enabled then
    let entry = { timestamp = Unix.gettimeofday () -. ctx.start_time; node_id; event } in
    ctx.traces := entry :: !(ctx.traces)

and record_start ?(node_type = "unknown") ctx node_id =
  let attempt = next_attempt ctx node_id in
  set_node_status ctx node_id Running;
  add_trace ctx node_id (NodeStart { node_type; attempt })

and record_complete ?(node_type = "unknown") ctx node_id ~duration_ms ~success =
  let attempt = match Hashtbl.find_opt ctx.node_attempts node_id with Some n -> n | None -> 1 in
  set_node_status ctx node_id (if success then Completed else Failed);
  add_trace ctx node_id (NodeComplete { duration_ms; success; node_type; attempt })

and record_error ?(node_type = "unknown") ctx node_id msg =
  let attempt = match Hashtbl.find_opt ctx.node_attempts node_id with Some n -> n | None -> 1 in
  add_trace ctx node_id (NodeError { message = msg; error_class = None; node_type; attempt })

and execute_llm_node ctx (exec_fn : exec_fn) node (llm : node_type) =
  match llm with
  | Llm { model; system = _; prompt; _ } ->
      let inputs = resolve_inputs ctx node.input_mapping in
      let resolved_prompt = Chain_context.substitute_prompt prompt inputs in
      record_start ctx node.id ~node_type:"llm";
      let start = Unix.gettimeofday () in
      let result = exec_fn ~model ~prompt:resolved_prompt () in
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      (match result with
      | Ok output ->
          record_complete ctx node.id ~duration_ms ~success:true ~node_type:"llm";
          Hashtbl.add ctx.outputs node.id output; Ok output
      | Error msg ->
          record_complete ctx node.id ~duration_ms ~success:false ~node_type:"llm"; Error msg)
  | _ -> Error "execute_llm_node error"

and execute_tool_node ctx tool_exec node (tool : node_type) =
  match tool with
  | Tool { name; args } ->
      record_start ctx node.id ~node_type:"tool";
      let start = Unix.gettimeofday () in
      let result = tool_exec ~name ~args in
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      (match result with
      | Ok output -> record_complete ctx node.id ~duration_ms ~success:true ~node_type:"tool"; Hashtbl.add ctx.outputs node.id output; Ok output
      | Error msg -> record_complete ctx node.id ~duration_ms ~success:false ~node_type:"tool"; Error msg)
  | _ -> Error "execute_tool_node error"

and execute_adapter ctx node input_ref transform _on_error =
  record_start ctx node.id;
  let input_value = resolve_single_input ctx input_ref in
  match Chain_adapter_eio.apply_adapter_transform transform input_value with
  | Ok output -> Hashtbl.add ctx.outputs node.id output; Ok output
  | Error msg -> Error msg

and execute_sequential ctx sw clock exec_fn tool_exec (nodes : Chain_types.node list) : (string, string) result =
  let rec loop last_output = function
    | [] -> Ok last_output
    | node :: rest ->
        match execute_node ctx sw clock exec_fn tool_exec node with
        | Ok output -> loop output rest
        | Error msg -> Error msg
  in loop "" nodes

and execute_pipeline ctx sw clock exec_fn tool_exec parent (nodes : Chain_types.node list) =
  record_start ctx parent.id;
  let res = execute_sequential ctx sw clock exec_fn tool_exec nodes in
  (match res with Ok o -> Hashtbl.add ctx.outputs parent.id o | _ -> ()); res

and execute_fanout ctx sw clock exec_fn tool_exec parent (nodes : Chain_types.node list) =
  record_start ctx parent.id;
  Eio.Fiber.all (List.map (fun node -> fun () -> ignore (execute_node ctx sw clock exec_fn tool_exec node)) nodes);
  Ok "fanout_done"

and execute_quorum ctx _sw _clock _exec_fn _tool_exec parent required (nodes : Chain_types.node list) =
  record_start ctx parent.id;
  let successes = List.filter (fun (n : Chain_types.node) -> Hashtbl.mem ctx.outputs n.id) nodes in
  if List.length successes >= required then Ok "quorum_met" else Error "quorum_failed"

and execute_gate ctx sw clock exec_fn tool_exec parent _condition then_node _else_node =
  record_start ctx parent.id;
  execute_node ctx sw clock exec_fn tool_exec then_node

and execute_merge ctx _sw _clock _exec_fn _tool_exec parent _strategy _nodes =
  record_start ctx parent.id;
  Ok "merged"

and execute_threshold ctx sw clock exec_fn tool_exec parent _metric _op _val input_node on_pass _on_fail =
  record_start ctx parent.id;
  match execute_node ctx sw clock exec_fn tool_exec input_node with
  | Ok _ -> (match on_pass with Some n -> execute_node ctx sw clock exec_fn tool_exec n | None -> Ok "passed")
  | Error m -> Error m

and execute_goal_driven ctx sw clock exec_fn tool_exec parent _metric _op _val action_node _measure _max _hints _conv _models =
  record_start ctx parent.id;
  execute_node ctx sw clock exec_fn tool_exec action_node

and execute_subgraph ctx sw clock exec_fn tool_exec parent (chain : Chain_types.chain) =
  record_start ctx parent.id;
  execute_sequential ctx sw clock exec_fn tool_exec chain.nodes

and execute_parallel_group ctx sw clock exec_fn tool_exec group node_map =
  Eio.Fiber.all (List.map (fun nid -> fun () -> match Hashtbl.find_opt node_map nid with Some n -> ignore (execute_node ctx sw clock exec_fn tool_exec n) | None -> ()) group);
  Ok "group_done"

and execute_map ctx sw clock exec_fn tool_exec parent _func inner =
  record_start ctx parent.id;
  execute_node ctx sw clock exec_fn tool_exec inner

and execute_bind ctx sw clock exec_fn tool_exec parent _func inner =
  record_start ctx parent.id;
  execute_node ctx sw clock exec_fn tool_exec inner

and execute_evaluator ctx _sw _clock _exec_fn _tool_exec parent _c _s _sp _ss _m = record_start ctx parent.id; Ok "eval"
and execute_retry ctx sw clock _exec_fn tool_exec parent node _m _b _r = record_start ctx parent.id; execute_node ctx sw clock _exec_fn tool_exec node
and execute_fallback ctx sw clock _exec_fn tool_exec parent primary _f = record_start ctx parent.id; execute_node ctx sw clock _exec_fn tool_exec primary
and execute_race ctx sw clock _exec_fn tool_exec parent nodes _t = record_start ctx parent.id; (match nodes with h :: _ -> execute_node ctx sw clock _exec_fn tool_exec h | [] -> Error "empty")
and execute_chain_exec ctx _sw _clock _exec_fn _tool_exec parent _cs _v _md _s _ci _po = record_start ctx parent.id; Ok "exec"
and execute_cache ctx sw clock _exec_fn tool_exec node _k _t inner = record_start ctx node.id; execute_node ctx sw clock _exec_fn tool_exec inner
and execute_batch ctx sw clock _exec_fn tool_exec node _bs _p _cs inner = record_start ctx node.id; execute_node ctx sw clock _exec_fn tool_exec inner
and execute_spawn ctx sw clock _exec_fn tool_exec node clean _pv _ic inner = record_start ctx node.id; let spawn_ctx = make_context ~start_time:ctx.start_time ~trace_enabled:ctx.trace_enabled ~timeout:ctx.timeout ~chain_id:ctx.chain_id () in execute_node (if clean then spawn_ctx else ctx) sw clock _exec_fn tool_exec inner
and execute_mcts ctx _sw _clock _exec_fn _tool_exec parent _s _sim _e _ep _p _mi _md _et _es _ps = record_start ctx parent.id; Ok "mcts"
and execute_stream_merge ctx _sw _clock _exec_fn _tool_exec node _n _r _i _mr _t = record_start ctx node.id; Ok "stream"
and execute_feedback_loop ctx _sw _clock _exec_fn _tool_exec node _g _ec _ip _mi _st _so _c _rm = record_start ctx node.id; Ok "feedback"
and execute_masc_broadcast ctx _te node _m _r _men = record_start ctx node.id; Ok "broadcast"
and execute_masc_listen ctx _c _te node _f _t _r = record_start ctx node.id; Ok "listen"
and execute_masc_claim ctx _te node _tid _r = record_start ctx node.id; Ok "claim"

(** Helper functions *)
and maybe_summarize_and_rotate ~exec_fn:_ _conv = ()
and build_context_prompt conv = String.concat "\n" (List.map (fun (m : Chain_context.conv_message) -> m.content) conv.history)
and add_message conv ~role ~content ~iteration ~model = conv.history <- {role; content; model; iteration} :: conv.history; conv.total_tokens <- conv.total_tokens + (String.length content / 4)
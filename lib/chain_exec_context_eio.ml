type node = Chain_types.node
type trace_event = Chain_trace_types.trace_event =
  | NodeStart of { node_type : string; attempt : int }
  | NodeComplete of { duration_ms : int; success : bool; node_type : string; attempt : int }
  | NodeError of { message : string; error_class : string option; node_type : string; attempt : int }
  | ChainStart of { chain_id : string; mermaid_dsl : string option }
  | ChainComplete of { chain_id : string; success : bool }

type internal_trace = Chain_trace_types.internal_trace = {
  timestamp : float;
  node_id : string;
  event : trace_event;
}

type exec_phase = Chain_trace_types.exec_phase =
  | Planned | Running | Completed | Failed | Skipped

type iteration_ctx = Chain_iteration.iteration_ctx
type conversation_ctx = Chain_conversation.conversation_ctx

type checkpoint_config = {
  checkpoint_store: Checkpoint_store.checkpoint_store option;
  checkpoint_enabled: bool;
  resume_from: string option;
  run_id: string;
  fs: Eio.Fs.dir_ty Eio.Path.t option;
}

type exec_context = {
  outputs: (string, string) Hashtbl.t;
  traces: internal_trace list ref;
  start_time: float;
  trace_enabled: bool;
  timeout: int;
  mutable iteration_ctx: iteration_ctx option;
  mutable conversation: conversation_ctx option;
  cache: (string, string * float) Hashtbl.t;
  mutable total_tokens: Chain_category.token_usage;
  langfuse_trace: Langfuse.trace option;
  checkpoint: checkpoint_config;
  node_status: (string, exec_phase) Hashtbl.t;
  node_attempts: (string, int) Hashtbl.t;
  chain_id: string;
}

let default_checkpoint_config = {
  checkpoint_store = None;
  checkpoint_enabled = false;
  resume_from = None;
  run_id = Checkpoint_store.generate_run_id ();
  fs = None;
}

let make_context ~start_time ~trace_enabled ~timeout ~chain_id ?langfuse_trace ?checkpoint () = {
  outputs = Hashtbl.create 16;
  traces = ref [];
  start_time;
  trace_enabled;
  timeout;
  iteration_ctx = None;
  conversation = None;
  cache = Hashtbl.create 32;
  total_tokens = Chain_category.Token_monoid.empty;
  langfuse_trace;
  checkpoint = Option.value checkpoint ~default:default_checkpoint_config;
  node_status = Hashtbl.create 64;
  node_attempts = Hashtbl.create 64;
  chain_id;
}

let set_node_status ctx node_id status =
  Hashtbl.replace ctx.node_status node_id status

let next_attempt ctx node_id =
  let cur = match Hashtbl.find_opt ctx.node_attempts node_id with Some n -> n | None -> 0 in
  let next = cur + 1 in
  Hashtbl.replace ctx.node_attempts node_id next;
  next

let make_checkpoint_config ?fs ?store ?(enabled = false) ?resume_from () =
  let store = match store with
    | Some s -> Some s
    | None when enabled -> Some (Checkpoint_store.create ())
    | None -> None
  in
  {
    checkpoint_store = store;
    checkpoint_enabled = enabled;
    resume_from;
    run_id = Checkpoint_store.generate_run_id ();
    fs;
  }

let save_checkpoint ctx ~chain_id ~node_id =
  match ctx.checkpoint.checkpoint_store, ctx.checkpoint.fs with
  | Some store, Some fs when ctx.checkpoint.checkpoint_enabled ->
      let outputs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx.outputs [] in
      let traces = [] in
      let cp = Checkpoint_store.make_checkpoint
        ~run_id:ctx.checkpoint.run_id
        ~chain_id
        ~node_id
        ~outputs
        ~traces
        ~total_tokens:ctx.total_tokens
        ()
      in
      (match Checkpoint_store.save_eio ~fs store cp with
       | Ok () -> ()
       | Error msg -> Printf.eprintf "[checkpoint] Save failed: %s\n%!" msg)
  | Some store, None when ctx.checkpoint.checkpoint_enabled ->
      let outputs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx.outputs [] in
      let traces = [] in
      let cp = Checkpoint_store.make_checkpoint
        ~run_id:ctx.checkpoint.run_id
        ~chain_id
        ~node_id
        ~outputs
        ~traces
        ~total_tokens:ctx.total_tokens
        ()
      in
      (match Checkpoint_store.save store cp with
       | Ok () -> ()
       | Error msg -> Printf.eprintf "[checkpoint] Save failed: %s\n%!" msg)
  | _ -> ()

let restore_from_checkpoint ctx ~chain_id:_ =
  match ctx.checkpoint.checkpoint_store, ctx.checkpoint.resume_from with
  | Some store, Some run_id ->
      (match Checkpoint_store.load store ~run_id with
       | Ok cp ->
           List.iter (fun (k, v) -> Hashtbl.replace ctx.outputs k v) cp.outputs;
           (match cp.total_tokens with
            | Some tokens -> ctx.total_tokens <- tokens
            | None -> ());
           Ok cp.node_id
       | Error msg -> Error msg)
  | _ -> Error "No checkpoint to resume from"

let node_completed_in_checkpoint ctx node_id =
  Hashtbl.mem ctx.outputs node_id

let store_node_output ctx (node : node) (output : string) =
  Hashtbl.replace ctx.outputs node.id output;
  match node.output_key with
  | Some key ->
      let key = String.trim key in
      if key <> "" && key <> node.id then
        (match Hashtbl.find_opt ctx.outputs key with
         | Some existing ->
             if existing <> output then
               Printf.eprintf
                 "Warning: output_key '%s' for node '%s' ignored (already set)\n%!"
                 key
                 node.id
         | None ->
             Hashtbl.replace ctx.outputs key output)
  | None -> ()

let add_trace ctx node_id event =
  (if ctx.trace_enabled then
     let entry = {
       timestamp = Time_compat.now () -. ctx.start_time;
       node_id;
       event;
     } in
     ctx.traces := entry :: !(ctx.traces));
  (match event with
   | ChainStart { chain_id; mermaid_dsl } ->
       Chain_telemetry.emit (Chain_telemetry.chain_start ~chain_id ~nodes:0 ?mermaid_dsl ())
   | ChainComplete { chain_id; success = _ } ->
       let duration_ms = int_of_float ((Time_compat.now () -. ctx.start_time) *. 1000.0) in
       Chain_telemetry.emit (Chain_telemetry.ChainComplete {
         Chain_telemetry.complete_chain_id = chain_id;
         complete_duration_ms = duration_ms;
         complete_tokens = ctx.total_tokens;
         nodes_executed = 0;
         nodes_skipped = 0;
       })
   | NodeStart { node_type; _ } ->
       Chain_telemetry.emit (Chain_telemetry.node_start ~node_id ~node_type ())
   | NodeComplete { duration_ms; success; _ } ->
       Chain_telemetry.emit (Chain_telemetry.node_complete
         ~node_id
         ~duration_ms
         ~tokens:Chain_category.Token_monoid.empty
         ~verdict:(if success then Chain_category.Pass "" else Chain_category.Fail "")
         ~confidence:1.0
         ())
   | NodeError { message; _ } ->
       Chain_telemetry.emit (Chain_telemetry.Error {
         Chain_telemetry.error_node_id = node_id;
         error_message = message;
         error_retries = 0;
         error_timestamp = Time_compat.now ();
       }))

let record_start ?(node_type = "unknown") ctx node_id =
  let attempt = next_attempt ctx node_id in
  set_node_status ctx node_id Running;
  if Run_log_eio.enabled () then
    Run_log_eio.record_event
      ~event:"node_start"
      ~run_id:ctx.checkpoint.run_id
      ~chain_id:ctx.chain_id
      ~node_id
      ~node_type
      ~attempt
      ()
  else
    ();
  add_trace ctx node_id (NodeStart { node_type; attempt })

let record_complete ?(node_type = "unknown") ctx node_id ~duration_ms ~success =
  let attempt = match Hashtbl.find_opt ctx.node_attempts node_id with Some n -> n | None -> 1 in
  set_node_status ctx node_id (if success then Completed else Failed);
  if Run_log_eio.enabled () then
    Run_log_eio.record_event
      ~event:"node_complete"
      ~run_id:ctx.checkpoint.run_id
      ~chain_id:ctx.chain_id
      ~node_id
      ~node_type
      ~attempt
      ~duration_ms
      ~success
      ()
  else
    ();
  add_trace ctx node_id (NodeComplete { duration_ms; success; node_type; attempt })

let record_error ?(node_type = "unknown") ctx node_id msg =
  let attempt = match Hashtbl.find_opt ctx.node_attempts node_id with Some n -> n | None -> 1 in
  if Run_log_eio.enabled () then
    Run_log_eio.record_event
      ~event:"node_error"
      ~run_id:ctx.checkpoint.run_id
      ~chain_id:ctx.chain_id
      ~node_id
      ~node_type
      ~attempt
      ~error_class:"node_error"
      ~error:msg
      ()
  else
    ();
  add_trace ctx node_id (NodeError { message = msg; error_class = None; node_type; attempt })

let trace_to_entry = Chain_trace_types.trace_to_entry
let traces_to_entries = Chain_trace_types.traces_to_entries

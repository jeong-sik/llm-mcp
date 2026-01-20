(** Chain Executor - Eio-based Parallel Execution Engine

    Executes compiled Chain DSL plans using Eio fibers for concurrency.
    Supports recursive subgraph execution and trace generation.

    Key features:
    - Parallel execution of independent nodes (Fanout, Merge)
    - Sequential pipeline execution
    - N/K quorum consensus
    - Conditional gate execution
    - Recursive subgraph execution
    - Trace generation for debugging
*)

(** {1 Type Aliases from Chain_types} *)

(** Re-export Chain_types for local use *)
type node = Chain_types.node
type node_type = Chain_types.node_type
type chain = Chain_types.chain
type chain_config = Chain_types.chain_config
type chain_result = Chain_types.chain_result
type execution_plan = Chain_types.execution_plan
type trace_entry = Chain_types.trace_entry
type token_usage = Chain_types.token_usage
type merge_strategy = Chain_types.merge_strategy

(** {1 Local Trace Types} *)

(** Trace event types for execution logging *)
type trace_event =
  | NodeStart
  | NodeComplete of { duration_ms : int; success : bool }
  | NodeError of string
  | ChainStart of { chain_id : string }
  | ChainComplete of { chain_id : string; success : bool }

(** Internal trace entry for execution *)
type internal_trace = {
  timestamp : float;
  node_id : string;
  event : trace_event;
}

(** {1 Execution Context} *)

(** Context passed through execution *)
type exec_context = {
  outputs: (string, string) Hashtbl.t;      (** Node outputs by ID *)
  traces: internal_trace list ref;           (** Accumulated trace entries *)
  start_time: float;                         (** Execution start time *)
  trace_enabled: bool;                       (** Whether to record traces *)
  timeout: int;                              (** Overall timeout in seconds *)
}

(** Create a new execution context *)
let make_context ~start_time ~trace_enabled ~timeout = {
  outputs = Hashtbl.create 16;
  traces = ref [];
  start_time;
  trace_enabled;
  timeout;
}

(** {1 Trace Helpers} *)

let add_trace ctx node_id event =
  if ctx.trace_enabled then
    let entry = {
      timestamp = Unix.gettimeofday () -. ctx.start_time;
      node_id;
      event;
    } in
    ctx.traces := entry :: !(ctx.traces)

let record_start ctx node_id =
  add_trace ctx node_id NodeStart

let record_complete ctx node_id ~duration_ms ~success =
  add_trace ctx node_id (NodeComplete { duration_ms; success })

let record_error ctx node_id msg =
  add_trace ctx node_id (NodeError msg)

(** Convert internal_trace to Chain_types.trace_entry *)
let trace_to_entry (t : internal_trace) (node_type_name : string) : Chain_types.trace_entry =
  let status, error = match t.event with
    | NodeStart -> (`Success, None)  (* Will be updated by NodeComplete *)
    | NodeComplete { success; _ } ->
        if success then (`Success, None) else (`Failure, None)
    | NodeError msg -> (`Failure, Some msg)
    | ChainStart _ | ChainComplete _ -> (`Success, None)
  in
  {
    Chain_types.node_id = t.node_id;
    node_type_name;
    start_time = t.timestamp;
    end_time = t.timestamp;  (* Will be updated by pairing with NodeComplete *)
    status;
    output_preview = None;
    error;
  }

(** Convert internal traces to trace_entry list, pairing start/complete events *)
let traces_to_entries (traces : internal_trace list) : Chain_types.trace_entry list =
  (* Group traces by node_id and build proper entries *)
  let node_traces = Hashtbl.create 16 in
  List.iter (fun (t : internal_trace) ->
    let existing = try Hashtbl.find node_traces t.node_id with Not_found -> [] in
    Hashtbl.replace node_traces t.node_id (t :: existing)
  ) traces;

  Hashtbl.fold (fun node_id events acc ->
    (* Find start and complete events *)
    let start_time = List.fold_left (fun acc t ->
      match t.event with NodeStart -> min acc t.timestamp | _ -> acc
    ) max_float events in
    let end_time, status, error = List.fold_left (fun (et, st, err) t ->
      match t.event with
      | NodeComplete { duration_ms = _; success } ->
          (t.timestamp, (if success then `Success else `Failure), err)
      | NodeError msg -> (et, `Failure, Some msg)
      | _ -> (et, st, err)
    ) (start_time, `Success, None) events in

    let entry : Chain_types.trace_entry = {
      node_id;
      node_type_name = "unknown";  (* Would need node lookup to get this *)
      start_time;
      end_time;
      status;
      output_preview = None;
      error;
    } in
    entry :: acc
  ) node_traces []

(** {1 Input Resolution} *)

(** Resolve input mappings to actual values *)
let resolve_inputs ctx (mappings : (string * string) list) : (string * string) list =
  List.filter_map (fun (key, ref_str) ->
    (* Parse "node_id.output" or just "node_id" *)
    let node_id = match String.split_on_char '.' ref_str with
      | id :: _ -> id
      | [] -> ref_str
    in
    match Hashtbl.find_opt ctx.outputs node_id with
    | Some value -> Some (key, value)
    | None -> None
  ) mappings

(** Substitute {{var}} in prompt with resolved inputs *)
let substitute_prompt prompt (inputs : (string * string) list) : string =
  List.fold_left (fun acc (key, value) ->
    (* Replace {{key}} with value *)
    let pattern = "{{" ^ key ^ "}}" in
    let buf = Buffer.create (String.length acc) in
    let rec replace start =
      match String.index_from_opt acc start '{' with
      | None -> Buffer.add_substring buf acc start (String.length acc - start)
      | Some i ->
          if i + String.length pattern <= String.length acc &&
             String.sub acc i (String.length pattern) = pattern then begin
            Buffer.add_substring buf acc start (i - start);
            Buffer.add_string buf value;
            replace (i + String.length pattern)
          end else begin
            Buffer.add_char buf acc.[i];
            replace (i + 1)
          end
    in
    replace 0;
    Buffer.contents buf
  ) prompt inputs

(** Substitute placeholders inside JSON values *)
let substitute_json ctx (json : Yojson.Safe.t) : Yojson.Safe.t =
  let rec map = function
    | `String s ->
        let mappings = Chain_parser.extract_input_mappings s in
        if mappings = [] then `String s
        else
          let inputs = resolve_inputs ctx mappings in
          `String (substitute_prompt s inputs)
    | `Assoc fields ->
        `Assoc (List.map (fun (k, v) -> (k, map v)) fields)
    | `List items ->
        `List (List.map map items)
    | other -> other
  in
  map json

(** {1 Node Execution} *)

(** Type of execution function callback *)
type exec_fn = model:string -> prompt:string -> (string, string) result

(** Type of tool execution callback *)
type tool_exec = name:string -> args:Yojson.Safe.t -> (string, string) result

(** Execute a single LLM node *)
let execute_llm_node ctx ~exec_fn ~(node : node) (llm : node_type) : (string, string) result =
  match llm with
  | Llm { model; prompt; timeout = _ } ->
      let inputs = resolve_inputs ctx node.input_mapping in
      let resolved_prompt = substitute_prompt prompt inputs in
      record_start ctx node.id;
      let start = Unix.gettimeofday () in
      let result = exec_fn ~model ~prompt:resolved_prompt in
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      (match result with
      | Ok output ->
          record_complete ctx node.id ~duration_ms ~success:true;
          Hashtbl.add ctx.outputs node.id output;
          Ok output
  | Error msg ->
      record_complete ctx node.id ~duration_ms ~success:false;
      record_error ctx node.id msg;
      Error msg)
  | _ -> Error "execute_llm_node called with non-LLM node"

(** Execute a tool node *)
let execute_tool_node ctx ~tool_exec ~(node : node) (tool : node_type) : (string, string) result =
  match tool with
  | Tool { name; args } ->
      record_start ctx node.id;
      let start = Unix.gettimeofday () in
      let resolved_args = substitute_json ctx args in
      let result =
        try tool_exec ~name ~args:resolved_args
        with exn -> Error (Printexc.to_string exn)
      in
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      (match result with
      | Ok output ->
          record_complete ctx node.id ~duration_ms ~success:true;
          Hashtbl.add ctx.outputs node.id output;
          Ok output
      | Error msg ->
          record_complete ctx node.id ~duration_ms ~success:false;
          record_error ctx node.id msg;
          Error msg)
  | _ -> Error "execute_tool_node called with non-Tool node"

(** {1 Recursive Execution} *)

(** Forward declaration for recursive execution *)
let rec execute_node ctx ~sw ~clock ~exec_fn ~tool_exec (node : node) : (string, string) result =
  match node.node_type with
  | Llm _ -> execute_llm_node ctx ~exec_fn ~node node.node_type
  | Tool _ -> execute_tool_node ctx ~tool_exec ~node node.node_type
  | Pipeline nodes -> execute_pipeline ctx ~sw ~clock ~exec_fn ~tool_exec nodes
  | Fanout nodes -> execute_fanout ctx ~sw ~clock ~exec_fn ~tool_exec node nodes
  | Quorum { required; nodes } -> execute_quorum ctx ~sw ~clock ~exec_fn ~tool_exec node ~required nodes
  | Gate { condition; then_node; else_node } ->
      execute_gate ctx ~sw ~clock ~exec_fn ~tool_exec node ~condition ~then_node ~else_node
  | Subgraph chain -> execute_subgraph ctx ~sw ~clock ~exec_fn ~tool_exec node chain
  | ChainRef ref_id ->
      (* Look up chain in registry and execute as subgraph *)
      (match Chain_registry.lookup ref_id with
       | Some referenced_chain ->
           execute_subgraph ctx ~sw ~clock ~exec_fn ~tool_exec node referenced_chain
       | None ->
           record_error ctx node.id (Printf.sprintf "ChainRef '%s' not found in registry" ref_id);
           Error (Printf.sprintf "ChainRef '%s' not found in registry" ref_id))
  | Map { func; inner } -> execute_map ctx ~sw ~clock ~exec_fn ~tool_exec node ~func inner
  | Bind { func; inner } -> execute_bind ctx ~sw ~clock ~exec_fn ~tool_exec node ~func inner
  | Merge { strategy; nodes } -> execute_merge ctx ~sw ~clock ~exec_fn ~tool_exec node ~strategy nodes

(** Execute nodes in sequence (Pipeline) *)
and execute_pipeline ctx ~sw ~clock ~exec_fn ~tool_exec (nodes : node list) : (string, string) result =
  let rec loop last_output = function
    | [] -> Ok last_output
    | node :: rest ->
        match execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node with
        | Ok output -> loop output rest
        | Error msg -> Error msg
  in
  loop "" nodes

(** Execute nodes in parallel (Fanout) *)
and execute_fanout ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) (nodes : node list) : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* Collect results from parallel execution *)
  let results = ref [] in
  let mutex = Eio.Mutex.create () in

  Eio.Fiber.all (List.map (fun node ->
    fun () ->
      let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node in
      Eio.Mutex.use_rw mutex ~protect:true (fun () ->
        results := (node.id, result) :: !results
      )
  ) nodes);

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in

  (* Check if all succeeded *)
  let outputs = List.filter_map (fun (id, r) ->
    match r with Ok o -> Some (id, o) | Error _ -> None
  ) !results in

  if List.length outputs = List.length nodes then begin
    let combined = String.concat "\n---\n"
      (List.map (fun (id, o) -> Printf.sprintf "[%s]: %s" id o) outputs) in
    record_complete ctx parent.id ~duration_ms ~success:true;
    Hashtbl.add ctx.outputs parent.id combined;
    Ok combined
  end else begin
    let errors = List.filter_map (fun (id, r) ->
      match r with Error e -> Some (Printf.sprintf "%s: %s" id e) | Ok _ -> None
    ) !results in
    let msg = String.concat "; " errors in
    record_complete ctx parent.id ~duration_ms ~success:false;
    record_error ctx parent.id msg;
    Error msg
  end

(** Execute N/K quorum (require N successes from K nodes) *)
and execute_quorum ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) ~required (nodes : node list) : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* Track successes and failures *)
  let successes = ref [] in
  let failures = ref [] in
  let mutex = Eio.Mutex.create () in
  let done_promise, done_resolver = Eio.Promise.create () in

  (* Early termination when quorum reached *)
  Eio.Fiber.all (List.map (fun node ->
    fun () ->
      if Eio.Promise.is_resolved done_promise then () else
      let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node in
      Eio.Mutex.use_rw mutex ~protect:true (fun () ->
        match result with
        | Ok output -> successes := (node.id, output) :: !successes
        | Error msg -> failures := (node.id, msg) :: !failures
      );
      (* Check if quorum reached *)
      if List.length !successes >= required then
        (try Eio.Promise.resolve done_resolver () with _ -> ())
  ) nodes);

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in

  if List.length !successes >= required then begin
    let combined = String.concat "\n---\n"
      (List.map (fun (id, o) -> Printf.sprintf "[%s]: %s" id o) (List.rev !successes)) in
    record_complete ctx parent.id ~duration_ms ~success:true;
    Hashtbl.add ctx.outputs parent.id combined;
    Ok combined
  end else begin
    let msg = Printf.sprintf "Quorum not met: needed %d, got %d successes"
      required (List.length !successes) in
    record_complete ctx parent.id ~duration_ms ~success:false;
    record_error ctx parent.id msg;
    Error msg
  end

(** Execute conditional gate *)
and execute_gate ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) ~condition ~then_node ~else_node : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* Simple condition evaluation: check if referenced output is truthy *)
  let condition_met =
    let inputs = resolve_inputs ctx parent.input_mapping in
    match List.assoc_opt "condition" inputs with
    | Some "true" | Some "1" | Some "yes" -> true
    | Some "false" | Some "0" | Some "no" -> false
    | Some s when String.length s > 0 -> true
    | _ ->
        (* Fallback: evaluate condition string *)
        condition = "true" || String.length condition = 0
  in

  let result =
    if condition_met then
      execute_node ctx ~sw ~clock ~exec_fn ~tool_exec then_node
    else
      match else_node with
      | Some node -> execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node
      | None -> Ok ""  (* No else branch, return empty *)
  in

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
  (match result with
  | Ok output ->
      record_complete ctx parent.id ~duration_ms ~success:true;
      Hashtbl.add ctx.outputs parent.id output;
      Ok output
  | Error msg ->
      record_complete ctx parent.id ~duration_ms ~success:false;
      record_error ctx parent.id msg;
      Error msg)

(** Execute inline subgraph (recursive) *)
and execute_subgraph ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) (chain : chain) : (string, string) result =
  record_start ctx parent.id;
  add_trace ctx parent.id (ChainStart { chain_id = chain.id });
  let start = Unix.gettimeofday () in

  (* Execute subgraph nodes sequentially for now *)
  (* TODO: Use compiled plan for proper parallel execution *)
  let result = execute_pipeline ctx ~sw ~clock ~exec_fn ~tool_exec chain.nodes in

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
  let success = Result.is_ok result in
  add_trace ctx parent.id (ChainComplete { chain_id = chain.id; success });
  record_complete ctx parent.id ~duration_ms ~success;

  (match result with
  | Ok output ->
      (* Get output from specified output node *)
      let final = match Hashtbl.find_opt ctx.outputs chain.output with
        | Some o -> o
        | None -> output
      in
      Hashtbl.add ctx.outputs parent.id final;
      Ok final
  | Error msg -> Error msg)

(** Execute map (transform output) *)
and execute_map ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) ~func (inner : node) : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec inner in

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
  (match result with
  | Ok output ->
      (* Apply transformation function *)
      let transformed = match func with
        | "uppercase" -> String.uppercase_ascii output
        | "lowercase" -> String.lowercase_ascii output
        | "trim" -> String.trim output
        | "identity" | _ -> output
      in
      record_complete ctx parent.id ~duration_ms ~success:true;
      Hashtbl.add ctx.outputs parent.id transformed;
      Ok transformed
  | Error msg ->
      record_complete ctx parent.id ~duration_ms ~success:false;
      record_error ctx parent.id msg;
      Error msg)

(** Execute bind (dynamic routing based on output) *)
and execute_bind ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) ~func:_ (inner : node) : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* Bind executes inner, then could route to another node based on output *)
  (* For now, just execute inner and return *)
  let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec inner in

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
  (match result with
  | Ok output ->
      record_complete ctx parent.id ~duration_ms ~success:true;
      Hashtbl.add ctx.outputs parent.id output;
      Ok output
  | Error msg ->
      record_complete ctx parent.id ~duration_ms ~success:false;
      record_error ctx parent.id msg;
      Error msg)

(** Execute merge (combine parallel results) *)
and execute_merge ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) ~strategy (nodes : node list) : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* Execute all nodes in parallel *)
  let results = ref [] in
  let mutex = Eio.Mutex.create () in

  Eio.Fiber.all (List.map (fun node ->
    fun () ->
      let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node in
      Eio.Mutex.use_rw mutex ~protect:true (fun () ->
        results := (node.id, result) :: !results
      )
  ) nodes);

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in

  (* Merge results based on strategy *)
  let outputs = List.filter_map (fun (id, r) ->
    match r with Ok o -> Some (id, o) | Error _ -> None
  ) !results in

  if List.length outputs = 0 then begin
    record_complete ctx parent.id ~duration_ms ~success:false;
    Error "All merge inputs failed"
  end else begin
    let merged = match strategy with
      | First -> snd (List.hd outputs)
      | Last -> snd (List.hd (List.rev outputs))
      | Concat -> String.concat "\n" (List.map snd outputs)
      | WeightedAvg ->
          (* Weighted average - for now just concatenate with equal weights *)
          String.concat "\n---\n" (List.map (fun (id, o) ->
            Printf.sprintf "[%s]:\n%s" id o
          ) outputs)
      | Custom func_name ->
          (* Custom merge - for now just annotate with function name *)
          Printf.sprintf "Custom merge '%s':\n%s" func_name
            (String.concat "\n---\n" (List.map snd outputs))
    in
    record_complete ctx parent.id ~duration_ms ~success:true;
    Hashtbl.add ctx.outputs parent.id merged;
    Ok merged
  end

(** {1 Execution Steps} *)

(** Execution step - either single node or parallel group *)
type execution_step =
  | Sequential of node
  | Parallel of node list

(** Convert parallel_groups to execution steps *)
let plan_to_steps (plan : execution_plan) : execution_step list =
  let get_node id =
    List.find_opt (fun (n : node) -> n.id = id) plan.chain.Chain_types.nodes
  in
  List.filter_map (fun group ->
    let nodes = List.filter_map get_node group in
    match nodes with
    | [] -> None
    | [node] -> Some (Sequential node)
    | nodes -> Some (Parallel nodes)
  ) plan.parallel_groups

(** {1 Main Execution Entry Point} *)

(** Execute a compiled execution plan *)
let execute ~sw ~clock ~timeout ~trace ~exec_fn ~tool_exec (plan : execution_plan) : chain_result =
  let start_time = Unix.gettimeofday () in
  let ctx = make_context ~start_time ~trace_enabled:trace ~timeout in

  (* Record chain start *)
  add_trace ctx plan.chain.Chain_types.id (ChainStart { chain_id = plan.chain.Chain_types.id });

  (* Helper to build chain_result *)
  let make_result ~success ~output =
    let duration_ms = int_of_float ((Unix.gettimeofday () -. start_time) *. 1000.0) in
    let trace = traces_to_entries (List.rev !(ctx.traces)) in
    {
      Chain_types.chain_id = plan.chain.Chain_types.id;
      output;
      success;
      trace;
      token_usage = Chain_types.empty_token_usage;
      duration_ms;
      metadata = [];
    }
  in

  (* Execute each step in the plan *)
  let rec execute_steps () = function
    | [] ->
        (* Get output from the designated output node *)
        let output = match Hashtbl.find_opt ctx.outputs plan.chain.Chain_types.output with
          | Some o -> o
          | None -> ""
        in
        add_trace ctx plan.chain.Chain_types.id (ChainComplete { chain_id = plan.chain.Chain_types.id; success = true });
        make_result ~success:true ~output
    | step :: rest ->
        let result = match step with
          | Sequential node ->
              execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node
          | Parallel nodes ->
              (* Execute all nodes in parallel *)
              let results = ref [] in
              let mutex = Eio.Mutex.create () in
              Eio.Fiber.all (List.map (fun node ->
                fun () ->
                  let r = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node in
                  Eio.Mutex.use_rw mutex ~protect:true (fun () ->
                    results := (node.Chain_types.id, r) :: !results
                  )
              ) nodes);
              (* Check all succeeded *)
              let errors = List.filter_map (fun (id, r) ->
                match r with Error e -> Some (id ^ ": " ^ e) | Ok _ -> None
              ) !results in
              if List.length errors = 0 then Ok ""
              else Error (String.concat "; " errors)
        in
        match result with
        | Ok _ ->
            execute_steps () rest
        | Error msg ->
            add_trace ctx plan.chain.Chain_types.id (ChainComplete { chain_id = plan.chain.Chain_types.id; success = false });
            make_result ~success:false ~output:msg
  in

  (* Execute with timeout using Eio.Time.with_timeout *)
  let timeout_secs = Float.of_int timeout in
  match Eio.Time.with_timeout clock timeout_secs (fun () ->
    Ok (execute_steps () (plan_to_steps plan))
  ) with
  | Ok result -> result
  | Error `Timeout ->
      add_trace ctx plan.chain.Chain_types.id (ChainComplete { chain_id = plan.chain.Chain_types.id; success = false });
      make_result ~success:false ~output:(Printf.sprintf "Execution timeout after %d seconds" timeout)

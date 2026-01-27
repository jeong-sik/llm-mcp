(** Chain Execution Context - State management for workflows *)

(** {1 Execution Phase} *)

type exec_phase =
  | Planned
  | Running
  | Completed
  | Failed
  | Skipped

(** {1 Iteration Context} *)

type iteration_ctx = {
  iteration: int;
  max_iterations: int;
  progress: float;
  last_value: float;
  goal_value: float;
  strategy: string option;
}

(** {1 Conversation Context} *)

type conv_message = {
  role: string;
  content: string;
  model: string;
  iteration: int;
}

type conversation_ctx = {
  mutable history: conv_message list;
  mutable current_model: string;
  mutable model_index: int;
  models: string list;
  token_threshold: int;
  window_size: int;
  mutable total_tokens: int;
  mutable summaries: string list;
}

(** {1 Trace Types} *)

type trace_event =
  | NodeStart of { node_type : string; attempt : int }
  | NodeComplete of { duration_ms : int; success : bool; node_type : string; attempt : int }
  | NodeError of { message : string; error_class : string option; node_type : string; attempt : int }
  | ChainStart of { chain_id : string; mermaid_dsl : string option }
  | ChainComplete of { chain_id : string; success : bool }

type internal_trace = {
  timestamp : float;
  node_id : string;
  event : trace_event;
}

(** {1 Checkpoint Context} *)

type checkpoint_config = {
  checkpoint_store: Checkpoint_store.checkpoint_store option;
  checkpoint_enabled: bool;
  resume_from: string option;
  run_id: string;
  fs: Eio.Fs.dir_ty Eio.Path.t option;
}

let default_checkpoint_config = {
  checkpoint_store = None;
  checkpoint_enabled = false;
  resume_from = None;
  run_id = Checkpoint_store.generate_run_id ();
  fs = None;
}

(** {1 Main Execution Context} *)

type exec_context = {
  outputs: (string, string) Hashtbl.t;
  traces: internal_trace list ref;
  start_time: float;
  trace_enabled: bool;
  timeout: int;
  mutable iteration_ctx: iteration_ctx option;
  mutable conversation: conversation_ctx option;
  cache: (string, (string * float)) Hashtbl.t;
  mutable total_tokens: Chain_category.token_usage;
  langfuse_trace: Langfuse.trace option;
  checkpoint: checkpoint_config;
  node_status: (string, exec_phase) Hashtbl.t;
  node_attempts: (string, int) Hashtbl.t;
  chain_id: string;
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

let make_conversation_ctx ?(models=["gemini"; "claude"; "codex"])
                          ?(token_threshold=6000)
                          ?(window_size=10) () : conversation_ctx = {
  history = [];
  current_model = (match models with m :: _ -> m | [] -> "gemini");
  model_index = 0;
  models;
  token_threshold;
  window_size;
  total_tokens = 0;
  summaries = [];
}

let add_message (conv : conversation_ctx) ~role ~content ~iteration ~model =
  conv.history <- {role; content; model; iteration} :: conv.history;
  conv.total_tokens <- conv.total_tokens + (String.length content / 4)

let rotate_model (conv : conversation_ctx) =
  if List.length conv.models > 1 then begin
    conv.model_index <- (conv.model_index + 1) mod List.length conv.models;
    conv.current_model <- List.nth conv.models conv.model_index
  end

let needs_summarization (conv : conversation_ctx) = conv.total_tokens > conv.token_threshold

let build_context_prompt (conv : conversation_ctx) =
  String.concat "\n" (List.map (fun m -> m.content) (List.rev conv.history))

let estimate_tokens s = (String.length s + 3) / 4

let set_node_status ctx node_id status =
  Hashtbl.replace ctx.node_status node_id status

let next_attempt ctx node_id =
  let cur = match Hashtbl.find_opt ctx.node_attempts node_id with Some n -> n | None -> 0 in
  let next = cur + 1 in
  Hashtbl.replace ctx.node_attempts node_id next;
  next

(** {1 Variable Resolution} *)

let resolve_single_input ctx ref_str =
  match Hashtbl.find_opt ctx.outputs ref_str with
  | Some v -> v
  | None ->
      match Hashtbl.find_opt ctx.outputs ("parent." ^ ref_str) with
      | Some v -> v
      | None -> ref_str

let resolve_inputs ctx mapping =
  List.map (fun (param, source) -> (param, resolve_single_input ctx source)) mapping

let resolve_variables ctx (text : string) : string =
  let regex = Str.regexp "{{\\([^{}]+\\)}}" in
  let rec replace s =
    try
      let _ = Str.search_forward regex s 0 in
      let key = Str.matched_group 1 s |> String.trim in
      let replacement =
        match Hashtbl.find_opt ctx.outputs key with
        | Some v -> v
        | None ->
            match Hashtbl.find_opt ctx.outputs ("parent." ^ key) with
            | Some v -> v
            | None -> "{{" ^ key ^ "}}"
      in
      Str.replace_first regex replacement s |> replace
    with Not_found -> s
  in replace text

let substitute_prompt prompt inputs =
  List.fold_left (fun acc (key, value) ->
    let pattern = "{{" ^ key ^ "}}" in
    Str.global_replace (Str.regexp_string pattern) value acc
  ) prompt inputs

let substitute_json ctx json =
  let rec map = function
    | `String s ->
        let mappings = Chain_parser.extract_input_mappings s in
        if mappings = [] then `String s
        else
          let inputs = resolve_inputs ctx mappings in
          `String (substitute_prompt s inputs)
    | `Assoc fields -> `Assoc (List.map (fun (k, v) -> (k, map v)) fields)
    | `List items -> `List (List.map map items)
    | other -> other
  in map json

let substitute_iteration_vars prompt (iter_ctx : iteration_ctx option) =
  match iter_ctx with
  | None -> prompt
  | Some (it_ctx : iteration_ctx) ->
      let result = prompt in
      let result = Str.global_replace (Str.regexp_string "{{iteration}}") (string_of_int it_ctx.iteration) result in
      let result = Str.global_replace (Str.regexp_string "{{progress}}") (Printf.sprintf "%.2f" it_ctx.progress) result in
      result

(** {1 Checkpoint Helpers} *)

let save_checkpoint ctx ~chain_id ~node_id =
  match ctx.checkpoint.checkpoint_store, ctx.checkpoint.fs with
  | Some store, Some fs when ctx.checkpoint.checkpoint_enabled ->
      let outputs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx.outputs [] in
      let cp = Checkpoint_store.make_checkpoint ~run_id:ctx.checkpoint.run_id ~chain_id ~node_id ~outputs ~traces:[] ~total_tokens:ctx.total_tokens () in
      ignore (Checkpoint_store.save_eio ~fs store cp)
  | _ -> ()

let restore_from_checkpoint ctx ~chain_id:_ =
  match ctx.checkpoint.checkpoint_store, ctx.checkpoint.resume_from with
  | Some store, Some run_id ->
      (match Checkpoint_store.load store ~run_id with
       | Ok cp ->
           List.iter (fun (k, v) -> Hashtbl.replace ctx.outputs k v) cp.outputs;
           (match cp.total_tokens with Some t -> ctx.total_tokens <- t | None -> ());
           Some cp.node_id
       | Error _ -> None)
  | _ -> None

let trace_to_entry (t : internal_trace) _node_type_name =
  let status = match t.event with NodeComplete { success = true; _ } -> `Success | NodeError _ -> `Failure | _ -> `Success in
  { Chain_types.node_id = t.node_id; node_type_name = "node"; start_time = t.timestamp; end_time = t.timestamp; status; output_preview = None; error = None }
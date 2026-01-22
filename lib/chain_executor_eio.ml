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
type adapter_transform = Chain_types.adapter_transform

(** {1 Local Trace Types} *)

(** Trace event types for execution logging *)
type trace_event =
  | NodeStart
  | NodeComplete of { duration_ms : int; success : bool }
  | NodeError of string
  | ChainStart of { chain_id : string; mermaid_dsl : string option }
  | ChainComplete of { chain_id : string; success : bool }

(** Internal trace entry for execution *)
type internal_trace = {
  timestamp : float;
  node_id : string;
  event : trace_event;
}

(** {1 Execution Context} *)

(** Iteration context for GoalDriven loops - enables dynamic prompt variables *)
type iteration_ctx = {
  iteration: int;           (** Current iteration number (1-based) *)
  max_iterations: int;      (** Maximum iterations allowed *)
  progress: float;          (** Current progress toward goal (0.0 to 1.0+) *)
  last_value: float;        (** Last measured metric value *)
  goal_value: float;        (** Target goal value *)
  strategy: string option;  (** Current strategy hint if any *)
}

(** A single message in conversation history *)
type conv_message = {
  role: string;       (** "user" | "assistant" | "system" *)
  content: string;    (** Message content *)
  model: string;      (** Model that generated this (for assistant messages) *)
  iteration: int;     (** Which iteration this belongs to *)
}

(** Conversation context for maintaining history across iterations *)
type conversation_ctx = {
  mutable history: conv_message list;   (** Accumulated messages (newest first) *)
  mutable current_model: string;        (** Currently active model *)
  mutable model_index: int;             (** Index in model rotation *)
  models: string list;                  (** Available models for rotation *)
  token_threshold: int;                 (** Threshold to trigger summarization *)
  window_size: int;                     (** Keep last N messages without summarizing *)
  mutable total_tokens: int;            (** Estimated total tokens used *)
  mutable summaries: string list;       (** Previous conversation summaries *)
}

(** Context passed through execution *)
type exec_context = {
  outputs: (string, string) Hashtbl.t;      (** Node outputs by ID *)
  traces: internal_trace list ref;           (** Accumulated trace entries *)
  start_time: float;                         (** Execution start time *)
  trace_enabled: bool;                       (** Whether to record traces *)
  timeout: int;                              (** Overall timeout in seconds *)
  mutable iteration_ctx: iteration_ctx option;  (** Iteration context for GoalDriven *)
  mutable conversation: conversation_ctx option; (** Conversation context for conversational mode *)
  cache: (string, string * float) Hashtbl.t;  (** Node cache: key -> (result, timestamp) *)
}

(** Create a new execution context *)
let make_context ~start_time ~trace_enabled ~timeout = {
  outputs = Hashtbl.create 16;
  traces = ref [];
  start_time;
  trace_enabled;
  timeout;
  iteration_ctx = None;
  conversation = None;
  cache = Hashtbl.create 32;
}

(** {1 Trace Helpers} *)

let add_trace ctx node_id event =
  (* Record to local trace *)
  (if ctx.trace_enabled then
    let entry = {
      timestamp = Unix.gettimeofday () -. ctx.start_time;
      node_id;
      event;
    } in
    ctx.traces := entry :: !(ctx.traces));
  (* Also emit to global telemetry for stats collection - ALWAYS, not just when tracing *)
  (match event with
   | ChainStart { chain_id; mermaid_dsl } ->
       Chain_telemetry.emit (Chain_telemetry.chain_start ~chain_id ~nodes:0 ?mermaid_dsl ())
   | ChainComplete { chain_id; success = _ } ->
       let duration_ms = int_of_float ((Unix.gettimeofday () -. ctx.start_time) *. 1000.0) in
       Chain_telemetry.emit (Chain_telemetry.ChainComplete {
         Chain_telemetry.complete_chain_id = chain_id;
         complete_duration_ms = duration_ms;
         complete_tokens = Chain_category.Token_monoid.empty;
         nodes_executed = 0;
         nodes_skipped = 0;
       })
   | NodeStart ->
       Chain_telemetry.emit (Chain_telemetry.node_start ~node_id ~node_type:"unknown" ())
   | NodeComplete { duration_ms; success } ->
       Chain_telemetry.emit (Chain_telemetry.node_complete
         ~node_id
         ~duration_ms
         ~tokens:Chain_category.Token_monoid.empty
         ~verdict:(if success then Chain_category.Pass "" else Chain_category.Fail "")
         ~confidence:1.0
         ())
   | NodeError msg ->
       Chain_telemetry.emit (Chain_telemetry.Error {
         Chain_telemetry.error_node_id = node_id;
         error_message = msg;
         error_retries = 0;
         error_timestamp = Unix.gettimeofday ();
       }))

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

(** Resolve a single input reference to its value

    Supports:
    - {{node_id}} - get output from node
    - {{node_id.output}} - same, with explicit .output suffix
    - literal string - returns as-is
*)
let resolve_single_input ctx (ref_str : string) : string =
  (* Check if it's a {{variable}} reference *)
  let trimmed = String.trim ref_str in
  if String.length trimmed > 4 &&
     String.sub trimmed 0 2 = "{{" &&
     String.sub trimmed (String.length trimmed - 2) 2 = "}}" then
    (* Extract variable name from {{var}} *)
    let var = String.sub trimmed 2 (String.length trimmed - 4) in
    let node_id = match String.split_on_char '.' var with
      | id :: _ -> id
      | [] -> var
    in
    match Hashtbl.find_opt ctx.outputs node_id with
    | Some value -> value
    | None -> ref_str  (* Return original if not found *)
  else
    (* Direct node_id reference or literal *)
    match Hashtbl.find_opt ctx.outputs trimmed with
    | Some value -> value
    | None -> ref_str  (* Return as literal *)

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

(** {1 Iteration Variable Substitution} *)

(** Substitute iteration-aware variables in prompt
    Supports:
    - {{iteration}} - current iteration number (1-based)
    - {{max_iterations}} - maximum iterations allowed
    - {{progress}} - current progress toward goal (0.0 to 1.0+)
    - {{last_value}} - last measured metric value
    - {{goal_value}} - target goal value
    - {{strategy}} - current strategy hint (or "default")
    - {{linear:start,end}} - linear interpolation based on progress
    - {{step:v1,v2,v3,...}} - step function based on iteration
*)
let substitute_iteration_vars (prompt : string) (iter_ctx : iteration_ctx option) : string =
  match iter_ctx with
  | None -> prompt
  | Some ctx ->
      let replace_var s var_name replacement =
        let pattern = "{{" ^ var_name ^ "}}" in
        let buf = Buffer.create (String.length s) in
        let rec replace start =
          match String.index_from_opt s start '{' with
          | None -> Buffer.add_substring buf s start (String.length s - start)
          | Some i ->
              if i + String.length pattern <= String.length s &&
                 String.sub s i (String.length pattern) = pattern then begin
                Buffer.add_substring buf s start (i - start);
                Buffer.add_string buf replacement;
                replace (i + String.length pattern)
              end else begin
                (* Pattern didn't match - add content from start to i+1 and continue *)
                Buffer.add_substring buf s start (i - start + 1);
                replace (i + 1)
              end
        in
        replace 0;
        Buffer.contents buf
      in
      (* Basic variable substitution *)
      let result = prompt in
      let result = replace_var result "iteration" (string_of_int ctx.iteration) in
      let result = replace_var result "max_iterations" (string_of_int ctx.max_iterations) in
      let result = replace_var result "progress" (Printf.sprintf "%.2f" ctx.progress) in
      let result = replace_var result "last_value" (Printf.sprintf "%.2f" ctx.last_value) in
      let result = replace_var result "goal_value" (Printf.sprintf "%.2f" ctx.goal_value) in
      let result = replace_var result "strategy" (Option.value ctx.strategy ~default:"default") in

      (* Linear interpolation: {{linear:start,end}} *)
      let linear_regex = Str.regexp "{{linear:\\([0-9.]+\\),\\([0-9.]+\\)}}" in
      let result = Str.global_substitute linear_regex (fun s ->
        try
          let start_val = float_of_string (Str.matched_group 1 s) in
          let end_val = float_of_string (Str.matched_group 2 s) in
          let t = float_of_int (ctx.iteration - 1) /. float_of_int (max 1 (ctx.max_iterations - 1)) in
          let interpolated = start_val +. (end_val -. start_val) *. t in
          Printf.sprintf "%.2f" interpolated
        with Failure _ | Not_found -> Str.matched_string s
      ) result in

      (* Step function: {{step:v1,v2,v3,...}} *)
      let step_regex = Str.regexp "{{step:\\([^}]+\\)}}" in
      let result = Str.global_substitute step_regex (fun s ->
        try
          let values_str = Str.matched_group 1 s in
          let values = String.split_on_char ',' values_str in
          let idx = min (ctx.iteration - 1) (List.length values - 1) in
          List.nth values (max 0 idx) |> String.trim
        with Not_found | Failure _ | Invalid_argument _ -> Str.matched_string s
      ) result in

      result

(** {1 Conversational Mode Helpers} *)

(** Estimate token count from string (rough: ~4 chars per token) *)
let estimate_tokens (s : string) : int = (String.length s + 3) / 4

(** Estimate total tokens in conversation *)
let estimate_conversation_tokens (conv : conversation_ctx) : int =
  List.fold_left (fun acc msg -> acc + estimate_tokens msg.content) 0 conv.history
  + List.fold_left (fun acc s -> acc + estimate_tokens s) 0 conv.summaries

(** Create default conversation context *)
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

(** Add a message to conversation history *)
let add_message (conv : conversation_ctx) ~role ~content ~iteration : unit =
  let msg = { role; content; model = conv.current_model; iteration } in
  conv.history <- msg :: conv.history;
  conv.total_tokens <- conv.total_tokens + estimate_tokens content

(** Rotate to next model in the list *)
let rotate_model (conv : conversation_ctx) : unit =
  if List.length conv.models > 1 then begin
    conv.model_index <- (conv.model_index + 1) mod List.length conv.models;
    conv.current_model <- List.nth conv.models conv.model_index
  end

(** Check if summarization is needed *)
let needs_summarization (conv : conversation_ctx) : bool =
  conv.total_tokens > conv.token_threshold &&
  List.length conv.history > conv.window_size

(** Build context prompt from conversation history *)
let build_context_prompt (conv : conversation_ctx) : string =
  let summary_section = match conv.summaries with
    | [] -> ""
    | sums -> "## Previous Context Summary\n" ^ String.concat "\n---\n" sums ^ "\n\n"
  in
  let history_section =
    let recent = List.rev conv.history in
    String.concat "\n" (List.map (fun msg ->
      Printf.sprintf "[%s (%s, iter %d)]: %s"
        msg.role msg.model msg.iteration msg.content
    ) recent)
  in
  summary_section ^ "## Recent History\n" ^ history_section

(** Summarize history using LLM and compress context *)
let summarize_history ~exec_fn (conv : conversation_ctx) : string =
  (* Keep only recent messages for window *)
  let to_summarize, to_keep =
    let rec split n acc = function
      | [] -> (List.rev acc, [])
      | rest when n <= 0 -> (List.rev acc, rest)
      | h :: t -> split (n - 1) (h :: acc) t
    in
    split (List.length conv.history - conv.window_size) [] (List.rev conv.history)
  in
  let history_text = String.concat "\n" (List.map (fun msg ->
    Printf.sprintf "[%s]: %s" msg.role msg.content
  ) to_summarize) in

  let summary_prompt = Printf.sprintf
    "Summarize this conversation context concisely, preserving key decisions, progress, and important information:\n\n%s\n\nProvide a brief summary (under 500 words):"
    history_text
  in
  (* Use current model for summarization *)
  let summary = match exec_fn ~model:conv.current_model ?system:None ~prompt:summary_prompt ?tools:None () with
    | Ok s -> s
    | Error _ -> "Previous context (summarization failed)"
  in
  (* Update conversation state *)
  conv.summaries <- conv.summaries @ [summary];
  conv.history <- to_keep;
  conv.total_tokens <- estimate_conversation_tokens conv;
  summary

(** Maybe summarize and rotate model if needed *)
let maybe_summarize_and_rotate ~exec_fn (conv : conversation_ctx) : unit =
  if needs_summarization conv then begin
    let _ = summarize_history ~exec_fn conv in
    rotate_model conv
  end

(** {1 Node Execution} *)

(** Type of execution function callback *)
type exec_fn = model:string -> ?system:string -> prompt:string -> ?tools:Yojson.Safe.t -> unit -> (string, string) result

(** Type of tool execution callback *)
type tool_exec = name:string -> args:Yojson.Safe.t -> (string, string) result

(** Execute a single LLM node *)
let execute_llm_node ctx ~exec_fn ~(node : node) (llm : node_type) : (string, string) result =
  match llm with
  | Llm { model; system; prompt; timeout = _; tools } ->
      let inputs = resolve_inputs ctx node.input_mapping in
      let resolved_prompt = substitute_prompt prompt inputs in
      (* Apply iteration variable substitution if in GoalDriven context *)
      let final_prompt = substitute_iteration_vars resolved_prompt ctx.iteration_ctx in
      (* Also resolve system instruction if present *)
      let resolved_system = Option.map (fun s -> substitute_prompt s inputs) system in
      let final_system = Option.map (fun s -> substitute_iteration_vars s ctx.iteration_ctx) resolved_system in
      record_start ctx node.id;
      let start = Unix.gettimeofday () in
      let result = exec_fn ~model ?system:final_system ~prompt:final_prompt ?tools () in
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

(** Apply adapter transformation to input value *)
let rec apply_adapter_transform (transform : adapter_transform) (input : string) : (string, string) result =
  let open Chain_types in
  match transform with
  | Extract path ->
      (* Simple dot-path extraction from JSON *)
      (try
        let json = Yojson.Safe.from_string input in
        let parts = String.split_on_char '.' path in
        let rec extract_path j = function
          | [] -> Ok (Yojson.Safe.to_string j)
          | key :: rest ->
              (match j with
               | `Assoc fields ->
                   (match List.assoc_opt key fields with
                    | Some v -> extract_path v rest
                    | None -> Error (Printf.sprintf "Key '%s' not found in path '%s'" key path))
               | `List items when String.length key > 0 && key.[0] = '[' ->
                   (* Handle array index: [0], [1], etc. *)
                   let idx_str = String.sub key 1 (String.length key - 2) in
                   (try
                     let idx = int_of_string idx_str in
                     if idx >= 0 && idx < List.length items then
                       extract_path (List.nth items idx) rest
                     else
                       Error (Printf.sprintf "Index %d out of bounds" idx)
                   with Failure _ -> Error (Printf.sprintf "Invalid index: %s" key))
               | _ -> Error (Printf.sprintf "Cannot extract '%s' from non-object" key))
        in
        extract_path json parts
      with Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg))

  | Template tpl ->
      (* Simple {{value}} substitution *)
      let result = Str.global_replace (Str.regexp "{{value}}") input tpl in
      Ok result

  | Summarize max_tokens ->
      (* Simple truncation-based summarization (placeholder for LLM summarization) *)
      let words = String.split_on_char ' ' input in
      let truncated = List.filteri (fun i _ -> i < max_tokens) words in
      Ok (String.concat " " truncated)

  | Truncate max_chars ->
      if String.length input <= max_chars then Ok input
      else Ok (String.sub input 0 max_chars ^ "...")

  | JsonPath path ->
      (* Simplified JSONPath - supports $.field.subfield and $[0].field syntax *)
      let normalized_path =
        (* Strip $. or $ prefix if present *)
        if String.length path >= 2 && String.sub path 0 2 = "$." then
          String.sub path 2 (String.length path - 2)
        else if String.length path >= 1 && path.[0] = '$' then
          String.sub path 1 (String.length path - 1)
        else
          path
      in
      if String.length normalized_path = 0 then
        Ok input  (* $ alone means root *)
      else
        apply_adapter_transform (Extract normalized_path) input

  | Regex (pattern, replacement) ->
      (try
        let re = Str.regexp pattern in
        Ok (Str.global_replace re replacement input)
      with Failure _ -> Error (Printf.sprintf "Invalid regex pattern: %s" pattern))

  | ValidateSchema schema_name ->
      (* Schema validation - basic type checking.
         TODO: Implement full JSON Schema draft-07 validation.
         Options: 1) Use jsonschema2atd codegen 2) Minimal runtime validator
         Current: Parse JSON and verify non-empty - basic sanity check *)
      (try
        let json = Yojson.Safe.from_string input in
        match json with
        | `Null -> Error (Printf.sprintf "Schema '%s': null value not allowed" schema_name)
        | _ -> Ok input
      with Yojson.Json_error msg ->
        Error (Printf.sprintf "Schema '%s' validation failed: %s" schema_name msg))

  | ParseJson ->
      (* Validate input is valid JSON and return as-is *)
      (try
        let _ = Yojson.Safe.from_string input in
        Ok input
      with Yojson.Json_error msg -> Error (Printf.sprintf "Not valid JSON: %s" msg))

  | Stringify ->
      (* Wrap in JSON string if not already *)
      (try
        let _ = Yojson.Safe.from_string input in
        Ok input  (* Already JSON *)
      with Yojson.Json_error _ ->
        Ok (Yojson.Safe.to_string (`String input)))

  | Chain transforms ->
      (* Apply transforms sequentially *)
      List.fold_left
        (fun acc t ->
          match acc with
          | Error _ -> acc
          | Ok v -> apply_adapter_transform t v)
        (Ok input)
        transforms

  | Conditional { condition; on_true; on_false } ->
      (* Expression-based condition evaluation
         Supported operators:
         - "contains:text" - input contains text (no trimming)
         - "eq:value" - input equals value (input trimmed, value not trimmed)
         - "neq:value" - input not equals value (input trimmed)
         - "gt:number" - input > number (both trimmed for parsing)
         - "gte:number" - input >= number
         - "lt:number" - input < number
         - "lte:number" - input <= number
         - "empty" - input is empty or whitespace only
         - "nonempty" - input has non-whitespace content
         - "startswith:prefix" - input starts with prefix (no trimming)
         - "endswith:suffix" - input ends with suffix (no trimming)
         - "matches:regex" - input matches regex pattern (max 100 chars, ReDoS-protected)
         - Plain text - input contains the text (legacy behavior)

         Whitespace handling:
         - eq/neq: Input is trimmed before comparison
         - gt/gte/lt/lte: Both sides trimmed for numeric parsing
         - contains/startswith/endswith/matches: No trimming (exact match)
         - empty/nonempty: Checks after trimming

         Security:
         - matches: patterns limited to 100 chars
         - matches: catastrophic backtracking patterns (e.g., (a+)+) are rejected
      *)
      let evaluate_condition cond inp =
        let try_parse_float s =
          try Some (float_of_string (String.trim s))
          with Failure _ -> None
        in
        if String.length cond >= 9 && String.sub cond 0 9 = "contains:" then
          let text = String.sub cond 9 (String.length cond - 9) in
          try Str.search_forward (Str.regexp_string text) inp 0 >= 0
          with Not_found -> false
        else if String.length cond >= 3 && String.sub cond 0 3 = "eq:" then
          let value = String.sub cond 3 (String.length cond - 3) in
          String.trim inp = value
        else if String.length cond >= 4 && String.sub cond 0 4 = "neq:" then
          let value = String.sub cond 4 (String.length cond - 4) in
          String.trim inp <> value
        else if String.length cond >= 3 && String.sub cond 0 3 = "gt:" then
          let threshold = String.sub cond 3 (String.length cond - 3) in
          (match try_parse_float inp, try_parse_float threshold with
           | Some v, Some t -> v > t
           | _ -> false)
        else if String.length cond >= 4 && String.sub cond 0 4 = "gte:" then
          let threshold = String.sub cond 4 (String.length cond - 4) in
          (match try_parse_float inp, try_parse_float threshold with
           | Some v, Some t -> v >= t
           | _ -> false)
        else if String.length cond >= 3 && String.sub cond 0 3 = "lt:" then
          let threshold = String.sub cond 3 (String.length cond - 3) in
          (match try_parse_float inp, try_parse_float threshold with
           | Some v, Some t -> v < t
           | _ -> false)
        else if String.length cond >= 4 && String.sub cond 0 4 = "lte:" then
          let threshold = String.sub cond 4 (String.length cond - 4) in
          (match try_parse_float inp, try_parse_float threshold with
           | Some v, Some t -> v <= t
           | _ -> false)
        else if cond = "empty" then
          String.length (String.trim inp) = 0
        else if cond = "nonempty" then
          String.length (String.trim inp) > 0
        else if String.length cond >= 11 && String.sub cond 0 11 = "startswith:" then
          let prefix = String.sub cond 11 (String.length cond - 11) in
          String.length inp >= String.length prefix &&
          String.sub inp 0 (String.length prefix) = prefix
        else if String.length cond >= 9 && String.sub cond 0 9 = "endswith:" then
          let suffix = String.sub cond 9 (String.length cond - 9) in
          String.length inp >= String.length suffix &&
          String.sub inp (String.length inp - String.length suffix) (String.length suffix) = suffix
        else if String.length cond >= 8 && String.sub cond 0 8 = "matches:" then
          let pattern = String.sub cond 8 (String.length cond - 8) in
          (* ReDoS protection: limit pattern length and block catastrophic patterns *)
          let max_pattern_len = 100 in
          let has_redos_pattern p =
            (* Detect patterns that can cause exponential backtracking:
               - Nested quantifiers: (a+)+, (a[*])+, (a+)[*], (a[*])[*]
               - Overlapping alternations with quantifiers *)
            try
              let redos_re = Str.regexp "\\([+*]\\)[+*]\\|[+*])\\+\\|[+*])\\*" in
              Str.search_forward redos_re p 0 >= 0
            with Not_found -> false
          in
          if String.length pattern > max_pattern_len then
            false  (* Pattern too long - reject for safety *)
          else if has_redos_pattern pattern then
            false  (* Potentially catastrophic pattern - reject *)
          else
            (try
              let re = Str.regexp pattern in
              (* Use search_forward for contains-like matching *)
              Str.search_forward re inp 0 >= 0
            with Not_found | Failure _ -> false)
        else
          (* Legacy: plain text means "contains" *)
          try Str.search_forward (Str.regexp_string cond) inp 0 >= 0
          with Not_found -> false
      in
      let result = evaluate_condition condition input in
      let transform = if result then on_true else on_false in
      apply_adapter_transform transform input

  | Custom func_name ->
      (* Custom function placeholder *)
      (match func_name with
       | "identity" -> Ok input
       | "uppercase" -> Ok (String.uppercase_ascii input)
       | "lowercase" -> Ok (String.lowercase_ascii input)
       | "trim" -> Ok (String.trim input)
       | "reverse" ->
           let chars = String.to_seq input |> List.of_seq |> List.rev in
           Ok (String.of_seq (List.to_seq chars))
       | _ -> Error (Printf.sprintf "Unknown custom function: %s" func_name))

(** Execute an adapter node *)
let execute_adapter ctx (node : node) ~input_ref ~transform ~on_error : (string, string) result =
  record_start ctx node.id;
  let start = Unix.gettimeofday () in
  (* Resolve input reference *)
  let input_value = resolve_single_input ctx input_ref in
  if input_value = "" then begin
    let msg = Printf.sprintf "Adapter: empty input from '%s'" input_ref in
    record_error ctx node.id msg;
    match on_error with
    | `Fail -> Error msg
    | `Passthrough -> Ok ""
    | `Default d -> Ok d
  end
  else
    (* Apply transformation *)
    let result = apply_adapter_transform transform input_value in
    let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
    match result with
    | Ok output ->
        record_complete ctx node.id ~duration_ms ~success:true;
        Hashtbl.add ctx.outputs node.id output;
        Ok output
    | Error msg ->
        record_complete ctx node.id ~duration_ms ~success:false;
        record_error ctx node.id msg;
        (match on_error with
         | `Fail -> Error msg
         | `Passthrough -> Ok input_value  (* Return original on error *)
         | `Default d -> Ok d)

(** {1 Recursive Execution} *)

(** Forward declaration for recursive execution *)
let rec execute_node ctx ~sw ~clock ~exec_fn ~tool_exec (node : node) : (string, string) result =
  match node.node_type with
  | Llm _ -> execute_llm_node ctx ~exec_fn ~node node.node_type
  | Tool _ -> execute_tool_node ctx ~tool_exec ~node node.node_type
  | Pipeline nodes -> execute_pipeline ctx ~sw ~clock ~exec_fn ~tool_exec node nodes
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
  | Threshold { metric; operator; value; input_node; on_pass; on_fail } ->
      execute_threshold ctx ~sw ~clock ~exec_fn ~tool_exec node
        ~metric ~operator ~value ~input_node ~on_pass ~on_fail
  | GoalDriven { goal_metric; goal_operator; goal_value; action_node; measure_func; max_iterations; strategy_hints; conversational; relay_models } ->
      execute_goal_driven ctx ~sw ~clock ~exec_fn ~tool_exec node
        ~goal_metric ~goal_operator ~goal_value ~action_node ~measure_func ~max_iterations ~strategy_hints ~conversational ~relay_models
  | Evaluator { candidates; scoring_func; scoring_prompt; select_strategy; min_score } ->
      execute_evaluator ctx ~sw ~clock ~exec_fn ~tool_exec node
        ~candidates ~scoring_func ~scoring_prompt ~select_strategy ~min_score
  (* Resilience Nodes *)
  | Retry { node = inner_node; max_attempts; backoff; retry_on } ->
      execute_retry ctx ~sw ~clock ~exec_fn ~tool_exec node
        ~inner_node ~max_attempts ~backoff ~retry_on
  | Fallback { primary; fallbacks } ->
      execute_fallback ctx ~sw ~clock ~exec_fn ~tool_exec node
        ~primary ~fallbacks
  | Race { nodes = race_nodes; timeout } ->
      execute_race ctx ~sw ~clock ~exec_fn ~tool_exec node
        ~nodes:race_nodes ~timeout
  (* Meta-Chain: Execute a dynamically generated chain *)
  | ChainExec { chain_source; validate; max_depth; sandbox = _; context_inject; pass_outputs } ->
      execute_chain_exec ctx ~sw ~clock ~exec_fn ~tool_exec node
        ~chain_source ~validate ~max_depth ~context_inject ~pass_outputs
  (* Data Transformation Node *)
  | Adapter { input_ref; transform; on_error } ->
      execute_adapter ctx node ~input_ref ~transform ~on_error
  (* Caching Node *)
  | Cache { key_expr; ttl_seconds; inner } ->
      execute_cache ctx ~sw ~clock ~exec_fn ~tool_exec node ~key_expr ~ttl_seconds inner
  (* Batch Processing Node *)
  | Batch { batch_size; parallel; inner; collect_strategy } ->
      execute_batch ctx ~sw ~clock ~exec_fn ~tool_exec node ~batch_size ~parallel ~collect_strategy inner

(** Execute cache node - check cache first, execute inner if miss *)
and execute_cache ctx ~sw ~clock ~exec_fn ~tool_exec (node : node)
    ~key_expr ~ttl_seconds (inner : node) : (string, string) result =
  record_start ctx node.id;
  let start = Unix.gettimeofday () in

  (* Generate cache key by resolving the key expression *)
  let cache_key = resolve_single_input ctx key_expr in
  let full_key = Printf.sprintf "%s:%s" inner.id cache_key in

  (* Check cache *)
  let cached = match Hashtbl.find_opt ctx.cache full_key with
    | Some (result, timestamp) ->
        if ttl_seconds = 0 || Unix.gettimeofday () -. timestamp < float_of_int ttl_seconds then
          Some result
        else begin
          (* Expired - remove from cache *)
          Hashtbl.remove ctx.cache full_key;
          None
        end
    | None -> None
  in

  let result = match cached with
    | Some cached_result ->
        (* Cache hit - return cached value *)
        Ok cached_result
    | None ->
        (* Cache miss - execute inner node *)
        let inner_result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec inner in
        (match inner_result with
        | Ok output ->
            (* Store in cache *)
            Hashtbl.replace ctx.cache full_key (output, Unix.gettimeofday ());
            Ok output
        | Error _ as e -> e)
  in

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
  let success = Result.is_ok result in
  record_complete ctx node.id ~duration_ms ~success;
  (match result with
  | Ok output -> Hashtbl.add ctx.outputs node.id output
  | Error msg -> record_error ctx node.id msg);
  result

(** Execute batch node - process list items in batches *)
and execute_batch ctx ~sw ~clock ~exec_fn ~tool_exec (node : node)
    ~batch_size ~parallel ~collect_strategy (inner : node) : (string, string) result =
  record_start ctx node.id;
  let start = Unix.gettimeofday () in

  (* Get input as JSON array *)
  let input_str = resolve_single_input ctx (Printf.sprintf "{{%s}}" node.id) in
  let items = try
    match Yojson.Safe.from_string input_str with
    | `List items -> Ok (List.map Yojson.Safe.to_string items)
    | `String s ->
        (* Try to parse as newline-separated items *)
        Ok (String.split_on_char '\n' s |> List.filter (fun s -> String.trim s <> ""))
    | _ -> Error "Batch input must be a JSON array or newline-separated text"
  with Yojson.Json_error _ ->
    (* Treat as newline-separated *)
    Ok (String.split_on_char '\n' input_str |> List.filter (fun s -> String.trim s <> ""))
  in

  match items with
  | Error msg ->
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      record_complete ctx node.id ~duration_ms ~success:false;
      record_error ctx node.id msg;
      Error msg
  | Ok item_list ->
      (* Process items in batches - manual chunking *)
      let rec chunk_list n lst =
        if n <= 0 then [[]]
        else match lst with
        | [] -> []
        | _ ->
            let rec take n acc = function
              | [] -> (List.rev acc, [])
              | h :: t -> if n <= 0 then (List.rev acc, h :: t) else take (n - 1) (h :: acc) t
            in
            let (chunk, rest) = take n [] lst in
            chunk :: chunk_list n rest
      in
      let batches = chunk_list batch_size item_list in
      let all_results = ref [] in

      let process_batch batch_list =
        if parallel then begin
          (* Parallel execution within batch *)
          let mutex = Eio.Mutex.create () in
          Eio.Fiber.all (List.mapi (fun i item ->
            fun () ->
              (* Set item as input for inner node *)
              Hashtbl.replace ctx.outputs (Printf.sprintf "%s_item" node.id) item;
              let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec inner in
              Eio.Mutex.use_rw mutex ~protect:true (fun () ->
                all_results := (i, result) :: !all_results
              )
          ) batch_list)
        end else begin
          (* Sequential execution within batch *)
          List.iteri (fun i item ->
            Hashtbl.replace ctx.outputs (Printf.sprintf "%s_item" node.id) item;
            let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec inner in
            all_results := (i, result) :: !all_results
          ) batch_list
        end
      in

      List.iter process_batch batches;

      (* Sort results by index and collect *)
      let sorted_results = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) !all_results in
      let outputs = List.filter_map (fun (_, r) ->
        match r with Ok o -> Some o | Error _ -> None
      ) sorted_results in

      let final_result = match collect_strategy with
        | `List -> Ok (Printf.sprintf "[%s]" (String.concat "," outputs))
        | `Concat -> Ok (String.concat "\n" outputs)
        | `First -> (match outputs with h :: _ -> Ok h | [] -> Error "No successful results")
        | `Last -> (match List.rev outputs with h :: _ -> Ok h | [] -> Error "No successful results")
      in

      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      let success = Result.is_ok final_result in
      record_complete ctx node.id ~duration_ms ~success;
      (match final_result with
      | Ok output -> Hashtbl.add ctx.outputs node.id output
      | Error msg -> record_error ctx node.id msg);
      final_result

(** Execute a dynamically generated chain (ChainExec node)

    Context Injection allows parent chain to pass data to generated chain:
    - pass_outputs: if true, all parent outputs are available as {{parent.node_id}}
    - context_inject: explicit mapping [(child_var, parent_source)] for {{var}} in child

    Depth tracking uses __chain_depth in outputs hashtable.
*)
and execute_chain_exec ctx ~sw ~clock ~exec_fn ~tool_exec (node : node)
    ~chain_source ~validate ~max_depth
    ~context_inject ~pass_outputs : (string, string) result =
  (* Check depth limit - stored in outputs table *)
  let current_depth = try
    int_of_string (Hashtbl.find ctx.outputs "__chain_depth")
  with Not_found | Failure _ -> 0
  in
  if current_depth >= max_depth then
    Error (Printf.sprintf "ChainExec depth limit exceeded: %d >= %d" current_depth max_depth)
  else begin
    (* Get chain JSON from source *)
    let chain_json_str = resolve_single_input ctx chain_source in
    if chain_json_str = "" then
      Error (Printf.sprintf "ChainExec: empty chain source from '%s'" chain_source)
    else
      (* Parse the chain JSON *)
      let chain_json = try
        Ok (Yojson.Safe.from_string chain_json_str)
      with exn ->
        Error (Printf.sprintf "ChainExec: invalid JSON from '%s': %s" chain_source (Printexc.to_string exn))
      in
      match chain_json with
      | Error msg -> Error msg
      | Ok json ->
          (* Parse chain *)
          (match Chain_parser.parse_chain json with
          | Error msg ->
              Error (Printf.sprintf "ChainExec: parse error: %s" msg)
          | Ok generated_chain ->
              (* Validate if required *)
              let validation = if validate then Chain_parser.validate_chain generated_chain else Ok () in
              (match validation with
              | Error msg -> Error (Printf.sprintf "ChainExec: validation error: %s" msg)
              | Ok () ->
                  (* Create new outputs table for child chain with incremented depth *)
                  let new_outputs = Hashtbl.create 16 in
                  Hashtbl.replace new_outputs "__chain_depth" (string_of_int (current_depth + 1));

                  (* Context Injection: pass_outputs - copy parent outputs with "parent." prefix *)
                  if pass_outputs then
                    Hashtbl.iter (fun k v ->
                      if not (String.equal k "__chain_depth") then
                        Hashtbl.replace new_outputs ("parent." ^ k) v
                    ) ctx.outputs;

                  (* Context Injection: explicit mappings - resolve and inject *)
                  List.iter (fun (child_var, parent_source) ->
                    let resolved = resolve_single_input ctx parent_source in
                    Hashtbl.replace new_outputs child_var resolved
                  ) context_inject;

                  let new_ctx = { ctx with outputs = new_outputs } in
                  (* Compile and execute the generated chain *)
                  (match Chain_compiler.compile generated_chain with
                  | Error msg -> Error (Printf.sprintf "ChainExec: compile error: %s" msg)
                  | Ok plan ->
                      (* Execute nodes in order using compiled plan *)
                      let rec exec_nodes = function
                        | [] ->
                            (* Get final output *)
                            (match Hashtbl.find_opt new_ctx.outputs generated_chain.Chain_types.output with
                            | Some output ->
                                Hashtbl.replace ctx.outputs node.id output;
                                Ok output
                            | None ->
                                Error (Printf.sprintf "ChainExec: output node '%s' not found" generated_chain.Chain_types.output))
                        | node_id :: rest ->
                            (match Chain_compiler.get_node generated_chain node_id with
                            | None -> Error (Printf.sprintf "ChainExec: node '%s' not found" node_id)
                            | Some child_node ->
                                (match execute_node new_ctx ~sw ~clock ~exec_fn ~tool_exec child_node with
                                | Ok _ -> exec_nodes rest
                                | Error msg -> Error msg))
                      in
                      exec_nodes plan.Chain_types.execution_order)))
  end

(** Execute nodes in sequence (internal helper, no output storage) *)
and execute_sequential ctx ~sw ~clock ~exec_fn ~tool_exec (nodes : node list) : (string, string) result =
  let rec loop last_output = function
    | [] -> Ok last_output
    | node :: rest ->
        match execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node with
        | Ok output -> loop output rest
        | Error msg -> Error msg
  in
  loop "" nodes

(** Execute nodes in sequence (Pipeline container node) *)
and execute_pipeline ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) (nodes : node list) : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in
  let result = execute_sequential ctx ~sw ~clock ~exec_fn ~tool_exec nodes in
  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
  (match result with
   | Ok output ->
       Hashtbl.add ctx.outputs parent.id output;
       record_complete ctx parent.id ~duration_ms ~success:true
   | Error msg ->
       record_complete ctx parent.id ~duration_ms ~success:false;
       record_error ctx parent.id msg);
  result

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

(** Execute N/K quorum (require N successes from K nodes)

    In Mermaid DAG: J1 --> V{Quorum:2}, J2 --> V, J3 --> V
    - J1, J2, J3 execute BEFORE V (topological order)
    - V aggregates already-computed outputs from ctx.outputs
    - If nodes are ChainRef placeholders, look up in ctx.outputs instead of executing
*)
and execute_quorum ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node) ~required (nodes : node list) : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in
  let _ = (sw, clock, exec_fn, tool_exec) in  (* suppress unused warnings *)

  (* Collect results from already-executed nodes or ChainRef lookups *)
  let successes = ref [] in
  let failures = ref [] in

  List.iter (fun (node : node) ->
    (* For ChainRef nodes (created by mermaid parser for Quorum inputs),
       look up the referenced node's output in ctx.outputs *)
    let result = match node.node_type with
      | ChainRef ref_id ->
          (match Hashtbl.find_opt ctx.outputs ref_id with
           | Some output -> Ok output
           | None -> Error (Printf.sprintf "Input node '%s' not yet executed" ref_id))
      | _ ->
          (* For non-ChainRef nodes, try to execute (backward compat) *)
          execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node
    in
    match result with
    | Ok output -> successes := (node.id, output) :: !successes
    | Error msg -> failures := (node.id, msg) :: !failures
  ) nodes;

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
  let mermaid_dsl = Some (Chain_mermaid_parser.chain_to_mermaid chain) in
  add_trace ctx parent.id (ChainStart { chain_id = chain.id; mermaid_dsl });
  let start = Unix.gettimeofday () in

  (* Execute subgraph nodes sequentially for now.
     TODO: Implement dependency-based parallel execution:
     1. Analyze node dependencies via Chain_compiler.analyze_dependencies
     2. Group independent nodes into parallel batches
     3. Execute batches with Eio.Fiber.all, sequential between batches
     4. Currently Fanout/Quorum/Race handle parallelism at their level *)
  let result = execute_sequential ctx ~sw ~clock ~exec_fn ~tool_exec chain.nodes in

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

  (* Execute all nodes in parallel, handling ChainRef nodes specially *)
  let results = ref [] in
  let mutex = Eio.Mutex.create () in

  Eio.Fiber.all (List.map (fun (node : node) ->
    fun () ->
      (* For ChainRef nodes, look up in ctx.outputs instead of executing *)
      let result = match node.node_type with
        | ChainRef ref_id ->
            (match Hashtbl.find_opt ctx.outputs ref_id with
             | Some output -> Ok output
             | None -> Error (Printf.sprintf "Input node '%s' not yet executed" ref_id))
        | _ ->
            (* For non-ChainRef nodes, execute normally *)
            execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node
      in
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

(** Execute threshold node - conditional branching based on metric value *)
and execute_threshold ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node)
    ~metric ~operator ~value ~input_node ~on_pass ~on_fail : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* First, execute the input node to get the value *)
  match execute_node ctx ~sw ~clock ~exec_fn ~tool_exec input_node with
  | Error msg ->
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      record_complete ctx parent.id ~duration_ms ~success:false;
      Error (Printf.sprintf "Threshold input node failed: %s" msg)
  | Ok input_output ->
      (* Extract numeric value from output based on metric *)
      let extracted_value = match metric with
        | "confidence" | "score" | "coverage" | "latency" ->
            (* Try to parse a float from the output *)
            (try Some (float_of_string (String.trim input_output))
             with Failure _ -> None)
        | _ -> None
      in
      (match extracted_value with
       | None ->
           let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
           record_complete ctx parent.id ~duration_ms ~success:false;
           Error (Printf.sprintf "Could not extract metric '%s' from output" metric)
       | Some v ->
           (* Compare value against threshold *)
           let passes = match operator with
             | Gt -> v > value
             | Gte -> v >= value
             | Lt -> v < value
             | Lte -> v <= value
             | Eq -> v = value
             | Neq -> v <> value
           in
           (* Execute appropriate branch *)
           let branch_result =
             if passes then
               match on_pass with
               | Some n -> execute_node ctx ~sw ~clock ~exec_fn ~tool_exec n
               | None -> Ok input_output  (* No pass branch, return input *)
             else
               match on_fail with
               | Some n -> execute_node ctx ~sw ~clock ~exec_fn ~tool_exec n
               | None -> Ok input_output  (* No fail branch, return input *)
           in
           let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
           (match branch_result with
            | Ok output ->
                record_complete ctx parent.id ~duration_ms ~success:true;
                Hashtbl.add ctx.outputs parent.id output;
                Ok output
            | Error msg ->
                record_complete ctx parent.id ~duration_ms ~success:false;
                Error msg))

(** Execute goal-driven iterative node - repeat until goal is met *)
and execute_goal_driven ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node)
    ~goal_metric ~goal_operator ~goal_value ~action_node ~measure_func
    ~max_iterations ~strategy_hints ~conversational ~relay_models : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* Initialize conversation context if conversational mode is enabled *)
  let _conv_ctx =
    if conversational then
      let models = if relay_models = [] then ["gemini"; "claude"; "codex"] else relay_models in
      Some (make_conversation_ctx ~models ())
    else
      None
  in

  (* Get strategy hint based on current progress *)
  let get_strategy_hint current_value =
    (* strategy_hints format: [("below_50", "fast"), ("above_50", "accurate")] *)
    let pct = (current_value /. goal_value) *. 100.0 in
    List.find_opt (fun (condition, _) ->
      match String.split_on_char '_' condition with
      | ["below"; n] -> (try pct < float_of_string n with Failure _ -> false)
      | ["above"; n] -> (try pct >= float_of_string n with Failure _ -> false)
      | _ -> false
    ) strategy_hints
    |> Option.map snd
  in

  (* Measure metric from output using measure_func *)
  let measure output =
    match measure_func with
    | "parse_float" | "parse_json" ->
        (* Direct float parsing from output *)
        (try Some (float_of_string (String.trim output))
         with Failure _ ->
           (* Try JSON extraction *)
           try
             let json = Yojson.Safe.from_string output in
             let open Yojson.Safe.Util in
             Some (json |> member goal_metric |> to_float)
           with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> None)
    | "exec_test" ->
        (* For test execution: extract coverage/pass rate from output *)
        (* Expected format: "coverage: 0.85" or JSON with metric field *)
        let regex = Str.regexp (goal_metric ^ "[: ]+\\([0-9.]+\\)") in
        (try
          let _ = Str.search_forward regex output 0 in
          Some (float_of_string (Str.matched_group 1 output))
        with Not_found ->
          try Some (float_of_string (String.trim output))
          with Failure _ -> None)
    | "call_api" ->
        (* For API calls: expect JSON response with metric *)
        (try
          let json = Yojson.Safe.from_string output in
          let open Yojson.Safe.Util in
          Some (json |> member goal_metric |> to_float)
        with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> None)
    | "llm_judge" ->
        (* Use LLM to assess the metric *)
        let prompt = Printf.sprintf
          "Evaluate the following output for '%s' metric. Return ONLY a number between 0.0 and 1.0:\n\n%s"
          goal_metric output
        in
        let result = exec_fn ~model:"gemini" ?system:None ~prompt:prompt ?tools:None () in
        (match result with
         | Ok score_str ->
             (try Some (float_of_string (String.trim score_str))
              with Failure _ -> None)
         | Error _ -> None)
    | _ ->
        (* Default: try to extract any float *)
        (try Some (float_of_string (String.trim output))
         with Failure _ -> None)
  in

  let rec iterate iteration last_value =
    if iteration > max_iterations then begin
      ctx.iteration_ctx <- None;  (* Clear iteration context on completion *)
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      record_complete ctx parent.id ~duration_ms ~success:false;
      Error (Printf.sprintf "Goal not achieved after %d iterations (last value: %.2f, target: %.2f)"
               max_iterations last_value goal_value)
    end else begin
      (* Get current strategy hint *)
      let current_strategy = get_strategy_hint last_value in

      (* Calculate progress toward goal *)
      let progress = last_value /. (max 0.001 goal_value) in

      (* Set iteration context for variable substitution in prompts *)
      ctx.iteration_ctx <- Some {
        iteration;
        max_iterations;
        progress;
        last_value;
        goal_value;
        strategy = current_strategy;
      };

      (* Execute the action node *)
      match execute_node ctx ~sw ~clock ~exec_fn ~tool_exec action_node with
      | Error msg ->
          let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
          record_complete ctx parent.id ~duration_ms ~success:false;
          Error (Printf.sprintf "Iteration %d failed: %s" iteration msg)
      | Ok output ->
          (* Measure the metric *)
          (match measure output with
           | None ->
               (* Can't measure, keep trying with same last_value *)
               iterate (iteration + 1) last_value
           | Some v ->
               (* Check if goal is met *)
               let goal_met = match goal_operator with
                 | Gt -> v > goal_value
                 | Gte -> v >= goal_value
                 | Lt -> v < goal_value
                 | Lte -> v <= goal_value
                 | Eq -> abs_float (v -. goal_value) < 0.001
                 | Neq -> abs_float (v -. goal_value) >= 0.001
               in
               if goal_met then begin
                 ctx.iteration_ctx <- None;  (* Clear iteration context on completion *)
                 let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
                 record_complete ctx parent.id ~duration_ms ~success:true;
                 Hashtbl.add ctx.outputs parent.id output;
                 Ok output
               end else
                 iterate (iteration + 1) v)
    end
  in
  iterate 1 0.0

(** Execute evaluator node - score candidates and select based on strategy *)
and execute_evaluator ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node)
    ~candidates ~scoring_func ~scoring_prompt ~select_strategy ~min_score : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in

  (* Execute all candidates in parallel *)
  let results = ref [] in
  let mutex = Eio.Mutex.create () in

  Eio.Fiber.all (List.map (fun (candidate : node) ->
    fun () ->
      let result = execute_node ctx ~sw ~clock ~exec_fn ~tool_exec candidate in
      Eio.Mutex.use_rw mutex ~protect:true (fun () ->
        results := (candidate.id, result) :: !results
      )
  ) candidates);

  (* Helper: LLM-based scoring using exec_fn *)
  let llm_score output =
    let prompt = match scoring_prompt with
      | Some p -> Printf.sprintf "%s\n\nCandidate output:\n%s\n\nRespond with ONLY a number between 0.0 and 1.0" p output
      | None -> Printf.sprintf "Score this output from 0.0 to 1.0 for quality and correctness:\n\n%s\n\nRespond with ONLY a number between 0.0 and 1.0" output
    in
    let result = exec_fn ~model:"gemini" ?system:None ~prompt:prompt ?tools:None () in
    match result with
    | Ok score_str ->
        (* Extract float from response *)
        let cleaned = String.trim score_str in
        (try
          let score = float_of_string cleaned in
          min 1.0 (max 0.0 score)  (* Clamp to [0, 1] *)
        with Failure _ ->
          (* Try to find a number in the response *)
          let regex = Str.regexp "[0-9]+\\.[0-9]+" in
          try
            let _ = Str.search_forward regex cleaned 0 in
            let found = Str.matched_string cleaned in
            min 1.0 (max 0.0 (float_of_string found))
          with Not_found | Failure _ -> 0.5)  (* Fallback *)
    | Error _ -> 0.5  (* Fallback on error *)
  in

  (* Score each successful result *)
  let scored = List.filter_map (fun (id, r) ->
    match r with
    | Error _ -> None
    | Ok output ->
        (* Score based on scoring_func *)
        let score = match scoring_func with
          | "regex_match" ->
              (* Simple: longer output = higher score (placeholder) *)
              float_of_int (String.length output) /. 1000.0
          | "json_schema" ->
              (* Check if valid JSON, bonus for more complete structure *)
              (try
                let json = Yojson.Safe.from_string output in
                let depth = ref 0 in
                let rec count_depth = function
                  | `Assoc fields ->
                      incr depth;
                      List.iter (fun (_, v) -> count_depth v) fields
                  | `List items ->
                      incr depth;
                      List.iter count_depth items
                  | _ -> ()
                in
                count_depth json;
                min 1.0 (0.5 +. (float_of_int !depth *. 0.1))
               with Yojson.Json_error _ -> 0.0)
          | "llm_judge" ->
              (* Use LLM to score the output *)
              llm_score output
          | "custom" | _ ->
              (* For custom, try to parse score from output metadata *)
              (try
                let json = Yojson.Safe.from_string output in
                let open Yojson.Safe.Util in
                json |> member "score" |> to_float
               with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> 0.5)
        in
        Some (id, output, score)
  ) !results in

  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in

  if scored = [] then begin
    record_complete ctx parent.id ~duration_ms ~success:false;
    Error "No candidates succeeded"
  end else begin
    (* Filter by min_score if specified *)
    let filtered = match min_score with
      | None -> scored
      | Some threshold -> List.filter (fun (_, _, s) -> s >= threshold) scored
    in
    if filtered = [] then begin
      record_complete ctx parent.id ~duration_ms ~success:false;
      Error (Printf.sprintf "No candidates met minimum score %.2f" (Option.value min_score ~default:0.0))
    end else begin
      (* Select based on strategy *)
      let selected = match select_strategy with
        | Best ->
            List.fold_left (fun best (id, out, sc) ->
              match best with
              | None -> Some (id, out, sc)
              | Some (_, _, best_sc) -> if sc > best_sc then Some (id, out, sc) else best
            ) None filtered
        | Worst ->
            List.fold_left (fun worst (id, out, sc) ->
              match worst with
              | None -> Some (id, out, sc)
              | Some (_, _, worst_sc) -> if sc < worst_sc then Some (id, out, sc) else worst
            ) None filtered
        | AboveThreshold t ->
            List.find_opt (fun (_, _, sc) -> sc >= t) filtered
        | WeightedRandom ->
            (* Simplified: just pick first (proper impl would use weighted random) *)
            Some (List.hd filtered)
      in
      match selected with
      | None ->
          record_complete ctx parent.id ~duration_ms ~success:false;
          Error "Selection strategy returned no result"
      | Some (_, output, _) ->
          record_complete ctx parent.id ~duration_ms ~success:true;
          Hashtbl.add ctx.outputs parent.id output;
          Ok output
    end
  end

(* ════════════════════════════════════════════════════════════════════════════
   Resilience Nodes Implementation
   ════════════════════════════════════════════════════════════════════════════ *)

(** Calculate backoff delay in seconds *)
and calculate_backoff_delay (strategy : Chain_types.backoff_strategy) (attempt : int) : float =
  match strategy with
  | Chain_types.Constant secs -> secs
  | Chain_types.Exponential base -> base *. (2.0 ** float_of_int attempt)
  | Chain_types.Linear base -> base *. float_of_int (attempt + 1)
  | Chain_types.Jitter (min_sec, max_sec) ->
      min_sec +. Random.float (max_sec -. min_sec)

(** Check if error matches retry patterns *)
and should_retry (retry_on : string list) (error_msg : string) : bool =
  match retry_on with
  | [] -> true
  | patterns ->
      List.exists (fun pattern ->
        try
          let regex = Str.regexp_case_fold pattern in
          Str.search_forward regex error_msg 0 >= 0
        with Failure _ | Not_found | Invalid_argument _ ->
          (* Regex failed or pattern not found: try prefix match *)
          String.sub error_msg 0 (min (String.length pattern) (String.length error_msg)) = pattern
      ) patterns

(** Execute retry node - retry on failure with backoff *)
and execute_retry ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node)
    ~inner_node ~max_attempts ~backoff ~retry_on : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in
  let rec attempt n last_error =
    if n > max_attempts then begin
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
      record_complete ctx parent.id ~duration_ms ~success:false;
      record_error ctx parent.id (Printf.sprintf "Max retries (%d) exceeded: %s" max_attempts last_error);
      Error (Printf.sprintf "Max retries (%d) exceeded: %s" max_attempts last_error)
    end else begin
      if n > 1 then Eio.Time.sleep clock (calculate_backoff_delay backoff (n - 2));
      match execute_node ctx ~sw ~clock ~exec_fn ~tool_exec inner_node with
      | Ok output ->
          let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
          record_complete ctx parent.id ~duration_ms ~success:true;
          Hashtbl.add ctx.outputs parent.id output;
          Ok output
      | Error msg ->
          if should_retry retry_on msg then attempt (n + 1) msg
          else begin
            let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
            record_complete ctx parent.id ~duration_ms ~success:false;
            record_error ctx parent.id msg;
            Error msg
          end
    end
  in
  attempt 1 ""

(** Execute fallback node - try primary, then fallbacks in order *)
and execute_fallback ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node)
    ~primary ~fallbacks : (string, string) result =
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in
  let rec try_nodes nodes errors =
    match nodes with
    | [] ->
        let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
        let msg = Printf.sprintf "All fallbacks failed: %s" (String.concat "; " (List.rev errors)) in
        record_complete ctx parent.id ~duration_ms ~success:false;
        record_error ctx parent.id msg;
        Error msg
    | node :: rest ->
        match execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node with
        | Ok output ->
            let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
            record_complete ctx parent.id ~duration_ms ~success:true;
            Hashtbl.add ctx.outputs parent.id output;
            Ok output
        | Error msg ->
            try_nodes rest ((node.id ^ ": " ^ msg) :: errors)
  in
  try_nodes (primary :: fallbacks) []

(** Execute race node - run all in parallel, first result wins *)
and execute_race ctx ~sw ~clock ~exec_fn ~tool_exec (parent : node)
    ~nodes ~timeout : (string, string) result =
  ignore timeout;
  record_start ctx parent.id;
  let start = Unix.gettimeofday () in
  let winner = ref None in
  let winner_mutex = Eio.Mutex.create () in
  let all_errors = ref [] in
  Eio.Fiber.all (List.map (fun (node : node) ->
    fun () ->
      let already_won = Eio.Mutex.use_rw winner_mutex ~protect:true (fun () -> Option.is_some !winner) in
      if not already_won then begin
        match execute_node ctx ~sw ~clock ~exec_fn ~tool_exec node with
        | Ok output ->
            Eio.Mutex.use_rw winner_mutex ~protect:true (fun () ->
              if Option.is_none !winner then winner := Some (node.id, output))
        | Error msg ->
            Eio.Mutex.use_rw winner_mutex ~protect:true (fun () ->
              all_errors := (node.id ^ ": " ^ msg) :: !all_errors)
      end
  ) nodes);
  let duration_ms = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
  match !winner with
  | Some (winner_id, output) ->
      record_complete ctx parent.id ~duration_ms ~success:true;
      Hashtbl.add ctx.outputs parent.id (Printf.sprintf "[winner: %s] %s" winner_id output);
      Ok output
  | None ->
      let msg = Printf.sprintf "All racers failed: %s" (String.concat "; " !all_errors) in
      record_complete ctx parent.id ~duration_ms ~success:false;
      record_error ctx parent.id msg;
      Error msg

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

  (* Record chain start with mermaid visualization *)
  let mermaid_dsl = Some (Chain_mermaid_parser.chain_to_mermaid plan.chain) in
  add_trace ctx plan.chain.Chain_types.id (ChainStart { chain_id = plan.chain.Chain_types.id; mermaid_dsl });

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

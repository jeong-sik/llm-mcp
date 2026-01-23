(** Chain Parser - JSON to Chain AST conversion

    Parses JSON DSL into Chain_types structures.
    Handles:
    - Node type detection and parsing
    - Input mapping with {{node.output}} syntax
    - Validation (cycle detection, depth limits)
    - Config defaults and overrides
*)

open Chain_types

(** Helper: Result bind operator *)
let ( let* ) = Result.bind

(** {1 Safe JSON Parsing Helpers - Explicit Error Handling} *)

(** Parse float from JSON with explicit error message *)
let require_float json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `Float f -> Ok f
  | `Int i -> Ok (float_of_int i)
  | `Null -> Error (Printf.sprintf "Missing required field '%s'" field_name)
  | other -> Error (Printf.sprintf "Field '%s' must be a number, got: %s"
                      field_name (Yojson.Safe.to_string other))

(** Parse int with default, logging when fallback is used *)
let parse_int_with_default json field_name default =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `Int i -> i
  | `Null -> default  (* explicit null = use default *)
  | other ->
      (* Log unexpected type but continue with default *)
      Printf.eprintf "[WARN] chain_parser: Field '%s' expected int, got %s, using default %d\n%!"
        field_name (Yojson.Safe.to_string other) default;
      default

(** Parse bool with default *)
let parse_bool_with_default json field_name default =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `Bool b -> b
  | `Null -> default
  | _ -> default

(** Parse string list with default empty *)
let parse_string_list_opt json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
  | `Null -> []
  | _ -> []

(** Parse assoc list as string pairs *)
let parse_string_assoc_opt json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `Assoc pairs -> List.filter_map (fun (k, v) ->
      match v with `String s -> Some (k, s) | _ -> None) pairs
  | `Null -> []
  | _ -> []

(** Parse string with default value *)
let parse_string_with_default json field_name default =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `String s -> s
  | `Null -> default
  | _ -> default

(** Parse optional string from JSON *)
let parse_string_opt json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `String s -> Some s
  | _ -> None

(** Parse optional int from JSON *)
let parse_int_opt json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `Int i -> Some i
  | _ -> None

(** Parse list with default empty - handles various JSON types *)
let parse_list_with_default json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `List l -> l
  | `Null -> []
  | _ -> []

(** Parse optional float from JSON *)
let parse_float_opt json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | _ -> None

(** Parse MCTS policy from JSON *)
let parse_mcts_policy (json : Yojson.Safe.t) : (mcts_policy, string) result =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string_option with
  | Some "ucb1" ->
      let c = match json |> member "c" with
        | `Float f -> f | `Int i -> float_of_int i | _ -> 1.41
      in Ok (UCB1 c)
  | Some "greedy" -> Ok Greedy
  | Some "epsilon_greedy" ->
      let eps = match json |> member "epsilon" with
        | `Float f -> f | `Int i -> float_of_int i | _ -> 0.1
      in Ok (EpsilonGreedy eps)
  | Some "softmax" ->
      let temp = match json |> member "temperature" with
        | `Float f -> f | `Int i -> float_of_int i | _ -> 1.0
      in Ok (Softmax temp)
  | Some t -> Error (Printf.sprintf "Unknown MCTS policy type: %s" t)
  | None -> Ok (UCB1 1.41)  (* default policy *)

(** Parse backoff strategy from JSON with sensible defaults *)
let parse_backoff_strategy json =
  let open Yojson.Safe.Util in
  match json |> member "backoff" with
  | `Null -> Exponential 1.0  (* sensible default *)
  | `String s ->
      (match String.split_on_char ':' s with
       | ["exponential"; v] -> (match float_of_string_opt v with Some f -> Exponential f | None -> Exponential 1.0)
       | ["constant"; v] -> (match float_of_string_opt v with Some f -> Constant f | None -> Constant 1.0)
       | ["linear"; v] -> (match float_of_string_opt v with Some f -> Linear f | None -> Linear 1.0)
       | _ -> Exponential 1.0)
  | `Float f -> Exponential f
  | `Int i -> Exponential (float_of_int i)
  | `Assoc _ as obj ->
      let typ = match obj |> member "type" with `String s -> s | _ -> "exponential" in
      let get_float key default = match obj |> member key with
        | `Float f -> f | `Int i -> float_of_int i | _ -> default
      in
      (match typ with
       | "constant" -> Constant (get_float "seconds" 1.0)
       | "exponential" -> Exponential (get_float "base" 2.0)
       | "linear" -> Linear (get_float "base" 1.0)
       | "jitter" -> Jitter (get_float "min" 0.5, get_float "max" 2.0)
       | _ -> Exponential 2.0)
  | _ -> Exponential 1.0

(** Parse merge strategy from string *)
let parse_merge_strategy = function
  | "first" -> Ok First
  | "last" -> Ok Last
  | "concat" -> Ok Concat
  | "weighted_average" | "weighted_avg" -> Ok WeightedAvg
  | s when String.length s > 7 && String.sub s 0 7 = "custom:" ->
      Ok (Custom (String.sub s 7 (String.length s - 7)))
  | s -> Error (Printf.sprintf "Unknown merge strategy: %s" s)

(** Parse threshold comparison operator from string *)
let parse_threshold_op = function
  | "gt" | ">" -> Ok Gt
  | "gte" | ">=" -> Ok Gte
  | "lt" | "<" -> Ok Lt
  | "lte" | "<=" -> Ok Lte
  | "eq" | "=" | "==" -> Ok Eq
  | "neq" | "!=" | "<>" -> Ok Neq
  | s -> Error (Printf.sprintf "Unknown threshold operator: %s" s)

(** Parse selection strategy from string/JSON *)
let parse_select_strategy json =
  match json with
  | `String "best" -> Ok Best
  | `String "worst" -> Ok Worst
  | `String "weighted_random" -> Ok WeightedRandom
  | `Assoc [("above_threshold", `Float f)] -> Ok (AboveThreshold f)
  | `Assoc [("above_threshold", `Int i)] -> Ok (AboveThreshold (float_of_int i))
  | `String s when String.length s > 16 && String.sub s 0 16 = "above_threshold:" ->
      (try
        let threshold = float_of_string (String.sub s 16 (String.length s - 16)) in
        Ok (AboveThreshold threshold)
      with Failure _ -> Error (Printf.sprintf "Invalid threshold value in: %s" s))
  | other -> Error (Printf.sprintf "Unknown select strategy: %s" (Yojson.Safe.to_string other))

(** Extract input mappings from prompt template *)
let extract_input_mappings (prompt : string) : (string * string) list =
  let regex = Str.regexp "{{\\([^}]+\\)}}" in
  let rec find_all pos acc =
    try
      let _ = Str.search_forward regex prompt pos in
      let matched = Str.matched_group 1 prompt in
      let next_pos = Str.match_end () in
      find_all next_pos (matched :: acc)
    with Not_found -> List.rev acc
  in
  find_all 0 []
  |> List.map (fun ref ->
      (* Split "node.output" or just use as-is *)
      match String.split_on_char '.' ref with
      | [node_id; _field] -> (ref, node_id)
      | _ -> (ref, ref))

(** Extract input mappings from JSON arguments *)
let rec extract_json_mappings (json : Yojson.Safe.t) : (string * string) list =
  match json with
  | `String s -> extract_input_mappings s
  | `Assoc fields ->
      List.concat (List.map (fun (_k, v) -> extract_json_mappings v) fields)
  | `List items ->
      List.concat (List.map extract_json_mappings items)
  | _ -> []

(** Parse chain config from JSON *)
let parse_config (json : Yojson.Safe.t) : chain_config =
  let open Yojson.Safe.Util in
  let get_int_opt key default =
    try json |> member key |> to_int
    with Type_error _ -> default
  in
  let get_bool_opt key default =
    try json |> member key |> to_bool
    with Type_error _ -> default
  in
  let get_direction_opt key default =
    match json |> member key with
    | `String s -> direction_of_string s
    | _ -> default
  in
  {
    max_depth = get_int_opt "max_depth" default_config.max_depth;
    max_concurrency = get_int_opt "max_concurrency" default_config.max_concurrency;
    timeout = get_int_opt "timeout" default_config.timeout;
    trace = get_bool_opt "trace" default_config.trace;
    direction = get_direction_opt "direction" default_config.direction;
  }

(** Helper: Get required string field with better error messages *)
let require_string json field_name =
  let open Yojson.Safe.Util in
  match json |> member field_name with
  | `Null -> Error (Printf.sprintf "Missing required field '%s'" field_name)
  | `String s -> Ok s
  | other -> Error (Printf.sprintf "Field '%s' must be a string, got: %s"
                      field_name (Yojson.Safe.to_string other))

(** Parse adapter transform from JSON *)
let rec parse_adapter_transform (json : Yojson.Safe.t) : (adapter_transform, string) result =
  let open Yojson.Safe.Util in
  match json with
  | `String s ->
      (* Simple string format: "extract:data.field" or "truncate:100" *)
      (match String.split_on_char ':' s with
       | ["extract"; path] -> Ok (Extract path)
       | ["template"; tpl] -> Ok (Template tpl)
       | ["summarize"; n] -> (try Ok (Summarize (int_of_string n)) with _ -> Error "Invalid summarize value")
       | ["truncate"; n] -> (try Ok (Truncate (int_of_string n)) with _ -> Error "Invalid truncate value")
       | ["jsonpath"; path] -> Ok (JsonPath path)
       | ["parse_json"] | ["parse"] -> Ok ParseJson
       | ["stringify"] -> Ok Stringify
       | ["custom"; name] -> Ok (Custom name)
       | [simple] ->
           (* Handle simple keywords *)
           (match simple with
            | "parse_json" | "parse" -> Ok ParseJson
            | "stringify" -> Ok Stringify
            | _ -> Error (Printf.sprintf "Unknown simple transform: %s" simple))
       | _ -> Error (Printf.sprintf "Invalid transform string format: %s" s))
  | `Assoc _ ->
      (* Object format with "type" field *)
      let typ = parse_string_with_default json "type" "unknown" in
      (match typ with
       | "extract" ->
           let path = parse_string_with_default json "path" "." in
           Ok (Extract path)
       | "template" ->
           let tpl = parse_string_with_default json "template" "{{value}}" in
           Ok (Template tpl)
       | "summarize" ->
           let max_tokens = parse_int_with_default json "max_tokens" 500 in
           Ok (Summarize max_tokens)
       | "truncate" ->
           let max_chars = parse_int_with_default json "max_chars" 1000 in
           Ok (Truncate max_chars)
       | "jsonpath" ->
           let path = parse_string_with_default json "path" "$" in
           Ok (JsonPath path)
       | "regex" ->
           let pattern = parse_string_with_default json "pattern" ".*" in
           let replacement = parse_string_with_default json "replacement" "" in
           Ok (Regex (pattern, replacement))
       | "validate_schema" ->
           let schema = parse_string_with_default json "schema" "" in
           Ok (ValidateSchema schema)
       | "parse_json" | "parse" -> Ok ParseJson
       | "stringify" -> Ok Stringify
       | "chain" ->
           let transforms_json = parse_list_with_default json "transforms" in
           let* transforms = parse_adapter_transforms transforms_json in
           Ok (Chain transforms)
       | "conditional" ->
           let* condition =
             match parse_string_opt json "condition" with
             | Some s -> Ok s
             | None -> Error "Missing 'condition' in conditional transform"
           in
           let* on_true = parse_adapter_transform (json |> member "on_true") in
           let* on_false = parse_adapter_transform (json |> member "on_false") in
           Ok (Conditional { condition; on_true; on_false })
       | "custom" ->
           let name = parse_string_with_default json "name" "identity" in
           Ok (Custom name)
       | unknown -> Error (Printf.sprintf "Unknown transform type: %s" unknown))
  | _ -> Error "Transform must be a string or object"

(** Parse list of adapter transforms *)
and parse_adapter_transforms (json_list : Yojson.Safe.t list) : (adapter_transform list, string) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | json :: rest ->
        match parse_adapter_transform json with
        | Ok t -> aux (t :: acc) rest
        | Error e -> Error e
  in
  aux [] json_list

(** Parse a single node from JSON *)
let rec parse_node (json : Yojson.Safe.t) : (node, string) result =
  let open Yojson.Safe.Util in
  try
    let* id = require_string json "id" in
    let* node_type_str = require_string json "type" in

    let* node_type = parse_node_type json node_type_str in

    (* Parse explicit input_mapping if provided, otherwise extract from prompt/args *)
    (* Helper for auto-extracting input mappings from node content *)
    let auto_extract_mappings () =
      match node_type with
      | Llm { prompt; _ } -> extract_input_mappings prompt
      | Tool { args; _ } -> extract_json_mappings args
      | _ -> []
    in
    let input_mapping =
      match json |> member "input_mapping" with
      | `List pairs ->
          List.filter_map (fun pair ->
            match pair with
            | `List [`String k; `String v] -> Some (k, v)
            | _ -> None
          ) pairs
      | `Null -> auto_extract_mappings ()
      | _ -> []
    in

    Ok { id; node_type; input_mapping }
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "JSON type error: %s" msg)
  | exn ->
      Error (Printf.sprintf "Parse error: %s" (Printexc.to_string exn))

(** Parse node type based on type string *)
and parse_node_type (json : Yojson.Safe.t) (type_str : string) : (node_type, string) result =
  let open Yojson.Safe.Util in
  match type_str with
  | "llm" ->
      let* model = require_string json "model" in
      let* prompt = require_string json "prompt" in
      let system = parse_string_opt json "system" in
      let timeout = parse_int_opt json "timeout" in
      let tools =
        match json |> member "tools" with
        | `Null -> None
        | v -> Some v
      in
      Ok (Llm { model; system; prompt; timeout; tools })

  | "tool" ->
      let* name = require_string json "name" in
      let args =
        match json |> member "args" with
        | `Null -> `Assoc []
        | v -> v
      in
      Ok (Tool { name; args })

  | "pipeline" ->
      let nodes_json = json |> member "nodes" |> to_list in
      let* nodes = parse_nodes nodes_json in
      Ok (Pipeline nodes)

  | "fanout" ->
      (* Try "branches" first, fallback to "nodes" *)
      let branches_json =
        match json |> member "branches" with
        | `List l -> l
        | _ -> parse_list_with_default json "nodes"
      in
      let* nodes = parse_nodes branches_json in
      Ok (Fanout nodes)

  | "quorum" ->
      let required = json |> member "required" |> to_int in
      (* Try "nodes" first, fallback to "inputs" *)
      let nodes_json =
        match json |> member "nodes" with
        | `List l -> l
        | _ -> parse_list_with_default json "inputs"
      in
      let* nodes = parse_nodes nodes_json in
      Ok (Quorum { required; nodes })

  | "gate" ->
      let* condition = require_string json "condition" in
      let then_json = json |> member "then" in
      let* then_node = parse_node then_json in
      let else_node =
        match json |> member "else" with
        | `Null -> None
        | else_json ->
            (match parse_node else_json with
             | Ok n -> Some n
             | Error _ -> None)
      in
      Ok (Gate { condition; then_node; else_node })

  | "subgraph" ->
      let graph_json = json |> member "graph" in
      let* chain = parse_chain_inner graph_json in
      Ok (Subgraph chain)

  | "chain_ref" ->
      let* ref_id = require_string json "ref" in
      Ok (ChainRef ref_id)

  | "map" ->
      let* func = require_string json "func" in
      let inner_json = json |> member "inner" in
      let* inner = parse_node inner_json in
      Ok (Map { func; inner })

  | "bind" ->
      let* func = require_string json "func" in
      let inner_json = json |> member "inner" in
      let* inner = parse_node inner_json in
      Ok (Bind { func; inner })

  | "merge" ->
      let strategy_str = parse_string_with_default json "strategy" "concat" in
      let* strategy = parse_merge_strategy strategy_str in
      (* Try "nodes" first, fallback to "inputs" *)
      let nodes_json =
        match json |> member "nodes" with
        | `List l -> l
        | _ -> parse_list_with_default json "inputs"
      in
      let* nodes = parse_nodes nodes_json in
      Ok (Merge { strategy; nodes })

  | "threshold" ->
      let* metric = require_string json "metric" in
      let* operator_str = require_string json "operator" in
      let* operator = parse_threshold_op operator_str in
      let* value = require_float json "value" in  (* Now explicit error on missing/invalid *)
      let input_json = json |> member "input_node" in
      let* input_node = parse_node input_json in
      let on_pass =
        match json |> member "on_pass" with
        | `Null -> None
        | pass_json -> (match parse_node pass_json with Ok n -> Some n | Error _ -> None)
      in
      let on_fail =
        match json |> member "on_fail" with
        | `Null -> None
        | fail_json -> (match parse_node fail_json with Ok n -> Some n | Error _ -> None)
      in
      Ok (Threshold { metric; operator; value; input_node; on_pass; on_fail })

  | "goal_driven" ->
      let* goal_metric = require_string json "goal_metric" in
      let* goal_operator_str = require_string json "goal_operator" in
      let* goal_operator = parse_threshold_op goal_operator_str in
      let* goal_value = require_float json "goal_value" in  (* Explicit error on missing *)
      let action_json = json |> member "action_node" in
      let* action_node = parse_node action_json in
      let* measure_func = require_string json "measure_func" in
      let max_iterations = parse_int_with_default json "max_iterations" 10 in
      let strategy_hints = parse_string_assoc_opt json "strategy_hints" in
      let conversational = parse_bool_with_default json "conversational" false in
      let relay_models = parse_string_list_opt json "relay_models" in
      Ok (GoalDriven {
        goal_metric; goal_operator; goal_value;
        action_node; measure_func; max_iterations; strategy_hints;
        conversational; relay_models
      })

  | "evaluator" ->
      let candidates_json = match json |> member "candidates" with
        | `List l -> l | `Null -> [] | _ -> []
      in
      let* candidates = parse_nodes candidates_json in
      let* scoring_func = require_string json "scoring_func" in
      let scoring_prompt = match json |> member "scoring_prompt" with
        | `String s -> Some s | _ -> None
      in
      let select_strategy_json = match json |> member "select_strategy" with
        | `Null -> `String "best" | v -> v
      in
      let* select_strategy = parse_select_strategy select_strategy_json in
      let min_score = match json |> member "min_score" with
        | `Float f -> Some f
        | `Int i -> Some (float_of_int i)
        | _ -> None
      in
      Ok (Evaluator { candidates; scoring_func; scoring_prompt; select_strategy; min_score })

  (* Resilience Nodes *)
  | "retry" ->
      let* input_node = parse_node (json |> member "node") in
      let max_attempts = parse_int_with_default json "max_attempts" 3 in
      let backoff = parse_backoff_strategy json in
      let retry_on = parse_string_list_opt json "retry_on" in
      Ok (Retry { node = input_node; max_attempts; backoff; retry_on })

  | "fallback" ->
      let* primary = parse_node (json |> member "primary") in
      let fallbacks_json = match json |> member "fallbacks" with
        | `List l -> l | _ -> []
      in
      let* fallbacks = parse_nodes fallbacks_json in
      Ok (Fallback { primary; fallbacks })

  | "race" ->
      let nodes_json = match json |> member "nodes" with
        | `List l -> l | _ -> []
      in
      let* nodes = parse_nodes nodes_json in
      let timeout = match json |> member "timeout" with
        | `Float f -> Some f
        | `Int i -> Some (float_of_int i)
        | _ -> None
      in
      Ok (Race { nodes; timeout })

  | "chain_exec" | "chainexec" | "meta" ->
      (* Meta-chain: execute a dynamically generated chain *)
      let chain_source = match json |> member "chain_source" with
        | `String s -> s
        | `Null -> (match json |> member "source" with `String s -> s | _ -> "{{input}}")
        | _ -> "{{input}}"
      in
      let validate = parse_bool_with_default json "validate" true in
      let max_depth = parse_int_with_default json "max_depth" 3 in
      let sandbox = parse_bool_with_default json "sandbox" true in
      let context_inject = parse_string_assoc_opt json "context_inject" in
      let pass_outputs = parse_bool_with_default json "pass_outputs" true in
      Ok (ChainExec { chain_source; validate; max_depth; sandbox; context_inject; pass_outputs })

  (* Data Transformation Node *)
  | "adapter" ->
      let* input_ref = require_string json "input_ref" in
      let* transform = parse_adapter_transform (json |> member "transform") in
      let on_error =
        match parse_string_opt json "on_error" with
        | Some "fail" -> `Fail
        | Some "passthrough" -> `Passthrough
        | Some s when String.length s > 8 && String.sub s 0 8 = "default:" ->
            `Default (String.sub s 8 (String.length s - 8))
        | _ -> `Fail
      in
      Ok (Adapter { input_ref; transform; on_error })

  (* Performance Optimization Nodes *)
  | "cache" ->
      let key_expr = parse_string_with_default json "key_expr" "{{input}}" in
      let ttl_seconds = parse_int_with_default json "ttl_seconds" 0 in
      let* inner = parse_node (json |> member "inner") in
      Ok (Cache { key_expr; ttl_seconds; inner })

  | "batch" ->
      let batch_size = parse_int_with_default json "batch_size" 10 in
      let parallel = parse_bool_with_default json "parallel" false in
      let* inner = parse_node (json |> member "inner") in
      let collect_strategy = match parse_string_opt json "collect_strategy" with
        | Some "concat" -> `Concat
        | Some "first" -> `First
        | Some "last" -> `Last
        | _ -> `List
      in
      Ok (Batch { batch_size; parallel; inner; collect_strategy })

  | "spawn" ->
      (* Clean Context Spawn - execute inner with isolated context *)
      let clean = parse_bool_with_default json "clean" true in  (* default: clean *)
      let* inner = parse_node (json |> member "inner") in
      let pass_vars = match json |> member "pass_vars" with
        | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      let inherit_cache = parse_bool_with_default json "inherit_cache" true in
      Ok (Spawn { clean; inner; pass_vars; inherit_cache })

  | "mcts" ->
      (* MCTS - Monte Carlo Tree Search for multi-strategy exploration *)
      let strategies_json = parse_list_with_default json "strategies" in
      let* strategies = parse_nodes strategies_json in
      let* simulation = parse_node (json |> member "simulation") in
      let evaluator = parse_string_with_default json "evaluator" "llm_judge" in
      let evaluator_prompt = parse_string_opt json "evaluator_prompt" in
      let* policy = parse_mcts_policy (json |> member "policy") in
      let max_iterations = parse_int_with_default json "max_iterations" 10 in
      let max_depth = parse_int_with_default json "max_depth" 5 in
      let expansion_threshold = parse_int_with_default json "expansion_threshold" 3 in
      let early_stop = parse_float_opt json "early_stop" in
      let parallel_sims = parse_int_with_default json "parallel_sims" 1 in
      Ok (Mcts {
        strategies; simulation; evaluator; evaluator_prompt; policy;
        max_iterations; max_depth; expansion_threshold; early_stop; parallel_sims
      })

  | "stream_merge" ->
      (* StreamMerge - progressive result processing from parallel nodes *)
      let nodes_json = parse_list_with_default json "nodes" in
      let* nodes = parse_nodes nodes_json in
      let reducer = match json |> member "reducer" with
        | `String "first" -> First
        | `String "last" -> Last
        | `String "concat" -> Concat
        | `String "weighted_avg" -> WeightedAvg
        | `Assoc pairs -> (
            match List.assoc_opt "name" pairs with
            | Some (`String name) -> Custom name
            | _ -> Concat  (* default *)
          )
        | `String s -> Custom s  (* treat unknown as custom *)
        | _ -> Concat
      in
      let initial = parse_string_with_default json "initial" "" in
      let min_results = parse_int_opt json "min_results" in
      let timeout = parse_float_opt json "timeout" in
      Ok (StreamMerge { nodes; reducer; initial; min_results; timeout })

  | unknown ->
      Error (Printf.sprintf "Unknown node type: %s" unknown)

(** Parse a list of nodes *)
and parse_nodes (json_list : Yojson.Safe.t list) : (node list, string) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | json :: rest ->
        match parse_node json with
        | Ok node -> aux (node :: acc) rest
        | Error e -> Error e
  in
  aux [] json_list

(** Parse inner chain (for subgraph) *)
and parse_chain_inner (json : Yojson.Safe.t) : (chain, string) result =
  let open Yojson.Safe.Util in
  try
    let id =
      parse_string_with_default json "id"
        (Printf.sprintf "subgraph_%d" (Random.int 10000))
    in
    let nodes_json = json |> member "nodes" |> to_list in
    let* nodes = parse_nodes nodes_json in
    let output = json |> member "output" |> to_string in
    let config =
      match json |> member "config" with
      | `Null -> default_config
      | cfg -> parse_config cfg
    in
    Ok { id; nodes; output; config }
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "Chain JSON type error: %s" msg)
  | exn ->
      Error (Printf.sprintf "Chain parse error: %s" (Printexc.to_string exn))

(** Main entry point: Parse complete chain from JSON *)
let parse_chain (json : Yojson.Safe.t) : (chain, string) result =
  parse_chain_inner json

(** Check if a node_type contains unresolved placeholder references *)
let rec has_placeholder_in_node_type = function
  | ChainRef "_" -> true
  | Pipeline nodes | Fanout nodes -> List.exists has_placeholder_node nodes
  | Race { nodes; _ } -> List.exists has_placeholder_node nodes
  | Quorum { nodes; _ } -> List.exists has_placeholder_node nodes
  | Gate { then_node; else_node; _ } ->
      has_placeholder_node then_node ||
      (match else_node with Some n -> has_placeholder_node n | None -> false)
  | GoalDriven { action_node; _ } -> has_placeholder_node action_node
  | Retry { node; _ } -> has_placeholder_node node
  | Fallback { primary; fallbacks; _ } ->
      has_placeholder_node primary || List.exists has_placeholder_node fallbacks
  | Map { inner; _ } -> has_placeholder_node inner
  | Threshold { input_node; _ } -> has_placeholder_node input_node
  | Evaluator { candidates; _ } -> List.exists has_placeholder_node candidates
  | Mcts { strategies; simulation; _ } ->
      List.exists has_placeholder_node strategies || has_placeholder_node simulation
  | _ -> false

and has_placeholder_node (n : Chain_types.node) =
  n.id = "_placeholder" || has_placeholder_in_node_type n.node_type

(** Collect all placeholder node IDs for error reporting *)
let rec collect_placeholders_in_node_type acc = function
  | ChainRef "_" -> "_chainref" :: acc
  | Pipeline nodes | Fanout nodes ->
      List.fold_left collect_placeholders_in_node acc nodes
  | Race { nodes; _ } ->
      List.fold_left collect_placeholders_in_node acc nodes
  | Quorum { nodes; _ } ->
      List.fold_left collect_placeholders_in_node acc nodes
  | Gate { then_node; else_node; _ } ->
      let acc = collect_placeholders_in_node acc then_node in
      (match else_node with Some n -> collect_placeholders_in_node acc n | None -> acc)
  | GoalDriven { action_node; _ } ->
      collect_placeholders_in_node acc action_node
  | Retry { node; _ } ->
      collect_placeholders_in_node acc node
  | Fallback { primary; fallbacks; _ } ->
      let acc = collect_placeholders_in_node acc primary in
      List.fold_left collect_placeholders_in_node acc fallbacks
  | Map { inner; _ } ->
      collect_placeholders_in_node acc inner
  | Threshold { input_node; _ } ->
      collect_placeholders_in_node acc input_node
  | Evaluator { candidates; _ } ->
      List.fold_left collect_placeholders_in_node acc candidates
  | Mcts { strategies; simulation; _ } ->
      let acc = List.fold_left collect_placeholders_in_node acc strategies in
      collect_placeholders_in_node acc simulation
  | _ -> acc

and collect_placeholders_in_node acc (n : Chain_types.node) =
  let acc = if n.id = "_placeholder" then n.id :: acc else acc in
  collect_placeholders_in_node_type acc n.node_type

(** Validate chain structure *)
let validate_chain (c : Chain_types.chain) : (unit, string) result =
  (* Check output node exists *)
  let node_ids = List.map (fun (n : Chain_types.node) -> n.id) c.Chain_types.nodes in
  if not (List.mem c.Chain_types.output node_ids) then
    Error (Printf.sprintf "Output node '%s' not found in chain" c.Chain_types.output)
  (* Check for duplicate IDs *)
  else
    let* () =
      let rec check_dups seen = function
        | [] -> Ok ()
        | id :: rest ->
            if List.mem id seen then
              Error (Printf.sprintf "Duplicate node ID: %s" id)
            else
              check_dups (id :: seen) rest
      in
      check_dups [] node_ids
    in
    (* Check for unresolved placeholder nodes *)
    let placeholders = List.fold_left collect_placeholders_in_node [] c.Chain_types.nodes in
    if placeholders <> [] then
      Error (Printf.sprintf "Unresolved placeholder nodes found: %s. This usually indicates incomplete Mermaid edge resolution."
               (String.concat ", " (List.sort_uniq String.compare placeholders)))
    else
      Ok ()

(* ============================================================================
   Chain to JSON Serializer (for JSON <-> Mermaid round-trip)
   ============================================================================ *)

(** Serialize merge strategy to string *)
let merge_strategy_to_string = function
  | First -> "first"
  | Last -> "last"
  | Concat -> "concat"
  | WeightedAvg -> "weighted_average"
  | Custom s -> "custom:" ^ s

(** Serialize threshold operator to string *)
let threshold_op_to_string = function
  | Gt -> "gt" | Gte -> "gte" | Lt -> "lt" | Lte -> "lte" | Eq -> "eq" | Neq -> "neq"

(** Serialize select strategy to JSON *)
let select_strategy_to_json = function
  | Best -> `String "best"
  | Worst -> `String "worst"
  | WeightedRandom -> `String "weighted_random"
  | AboveThreshold f -> `Assoc [("above_threshold", `Float f)]

(** Serialize backoff strategy to JSON *)
let backoff_to_json = function
  | Constant s -> `Assoc [("type", `String "constant"); ("seconds", `Float s)]
  | Exponential b -> `Assoc [("type", `String "exponential"); ("base", `Float b)]
  | Linear b -> `Assoc [("type", `String "linear"); ("base", `Float b)]
  | Jitter (min_s, max_s) -> `Assoc [("type", `String "jitter"); ("min", `Float min_s); ("max", `Float max_s)]

(** Serialize adapter transform to JSON *)
let rec adapter_transform_to_json = function
  | Extract path -> `Assoc [("type", `String "extract"); ("path", `String path)]
  | Template tpl -> `Assoc [("type", `String "template"); ("template", `String tpl)]
  | Summarize tokens -> `Assoc [("type", `String "summarize"); ("max_tokens", `Int tokens)]
  | Truncate chars -> `Assoc [("type", `String "truncate"); ("max_chars", `Int chars)]
  | JsonPath path -> `Assoc [("type", `String "jsonpath"); ("path", `String path)]
  | Regex (pattern, replacement) ->
      `Assoc [("type", `String "regex"); ("pattern", `String pattern); ("replacement", `String replacement)]
  | ValidateSchema schema -> `Assoc [("type", `String "validate_schema"); ("schema", `String schema)]
  | ParseJson -> `String "parse_json"
  | Stringify -> `String "stringify"
  | Chain transforms ->
      `Assoc [("type", `String "chain"); ("transforms", `List (List.map adapter_transform_to_json transforms))]
  | Conditional { condition; on_true; on_false } ->
      `Assoc [
        ("type", `String "conditional");
        ("condition", `String condition);
        ("on_true", adapter_transform_to_json on_true);
        ("on_false", adapter_transform_to_json on_false);
      ]
  | Split { delimiter; chunk_size; overlap } ->
      `Assoc [
        ("type", `String "split");
        ("delimiter", `String delimiter);
        ("chunk_size", `Int chunk_size);
        ("overlap", `Int overlap);
      ]
  | Custom name -> `Assoc [("type", `String "custom"); ("func", `String name)]

(** Serialize on_error policy to JSON *)
let on_error_to_json = function
  | `Fail -> `String "fail"
  | `Passthrough -> `String "passthrough"
  | `Default s -> `Assoc [("default", `String s)]

(** Serialize config to JSON *)
let config_to_json (cfg : chain_config) : Yojson.Safe.t =
  `Assoc [
    ("max_depth", `Int cfg.max_depth);
    ("max_concurrency", `Int cfg.max_concurrency);
    ("timeout", `Int cfg.timeout);
    ("trace", `Bool cfg.trace);
  ]

(** Serialize node to JSON *)
let rec node_to_json (n : node) : Yojson.Safe.t =
  let base = [("id", `String n.id)] in
  let input_mapping =
    if n.input_mapping = [] then []
    else [("inputs", `Assoc (List.map (fun (k, v) -> (k, `String v)) n.input_mapping))]
  in
  let type_fields = match n.node_type with
    | Llm { model; system; prompt; timeout; tools } ->
        let fields = [
          ("type", `String "llm");
          ("model", `String model);
          ("prompt", `String prompt);
        ] in
        let fields = match system with
          | Some s -> fields @ [("system", `String s)]
          | None -> fields
        in
        let fields = match timeout with
          | Some t -> fields @ [("timeout", `Int t)]
          | None -> fields
        in
        let fields = match tools with
          | Some t -> fields @ [("tools", t)]
          | None -> fields
        in
        fields

    | Tool { name; args } ->
        [("type", `String "tool"); ("name", `String name); ("args", args)]

    | Pipeline nodes ->
        [("type", `String "pipeline"); ("nodes", `List (List.map node_to_json nodes))]

    | Fanout nodes ->
        [("type", `String "fanout"); ("nodes", `List (List.map node_to_json nodes))]

    | Quorum { required; nodes } ->
        [
          ("type", `String "quorum");
          ("required", `Int required);
          ("nodes", `List (List.map node_to_json nodes));
        ]

    | Gate { condition; then_node; else_node } ->
        let fields = [
          ("type", `String "gate");
          ("condition", `String condition);
          ("then", node_to_json then_node);
        ] in
        (match else_node with
         | Some en -> fields @ [("else", node_to_json en)]
         | None -> fields)

    | Subgraph c ->
        [("type", `String "subgraph"); ("graph", chain_to_json_inner c)]

    | ChainRef ref_id ->
        [("type", `String "chain_ref"); ("ref", `String ref_id)]

    | Map { func; inner } ->
        [("type", `String "map"); ("func", `String func); ("inner", node_to_json inner)]

    | Bind { func; inner } ->
        [("type", `String "bind"); ("func", `String func); ("inner", node_to_json inner)]

    | Merge { strategy; nodes } ->
        [
          ("type", `String "merge");
          ("strategy", `String (merge_strategy_to_string strategy));
          ("nodes", `List (List.map node_to_json nodes));
        ]

    | Threshold { metric; operator; value; input_node; on_pass; on_fail } ->
        let fields = [
          ("type", `String "threshold");
          ("metric", `String metric);
          ("operator", `String (threshold_op_to_string operator));
          ("value", `Float value);
          ("input", node_to_json input_node);
        ] in
        let fields = match on_pass with Some n -> fields @ [("on_pass", node_to_json n)] | None -> fields in
        let fields = match on_fail with Some n -> fields @ [("on_fail", node_to_json n)] | None -> fields in
        fields

    | GoalDriven { goal_metric; goal_operator; goal_value; action_node;
                    measure_func; max_iterations; strategy_hints; conversational; relay_models } ->
        let fields = [
          ("type", `String "goal_driven");
          ("goal_metric", `String goal_metric);
          ("goal_operator", `String (threshold_op_to_string goal_operator));
          ("goal_value", `Float goal_value);
          ("action_node", node_to_json action_node);
          ("measure_func", `String measure_func);
          ("max_iterations", `Int max_iterations);
          ("conversational", `Bool conversational);
        ] in
        let fields = if strategy_hints = [] then fields
          else fields @ [("strategy_hints", `Assoc (List.map (fun (k, v) -> (k, `String v)) strategy_hints))]
        in
        let fields = if relay_models = [] then fields
          else fields @ [("relay_models", `List (List.map (fun s -> `String s) relay_models))]
        in
        fields

    | Evaluator { candidates; scoring_func; scoring_prompt; select_strategy; min_score } ->
        let fields = [
          ("type", `String "evaluator");
          ("candidates", `List (List.map node_to_json candidates));
          ("scoring_func", `String scoring_func);
          ("select_strategy", select_strategy_to_json select_strategy);
        ] in
        let fields = match scoring_prompt with
          | Some p -> fields @ [("scoring_prompt", `String p)]
          | None -> fields
        in
        let fields = match min_score with
          | Some s -> fields @ [("min_score", `Float s)]
          | None -> fields
        in
        fields

    | Retry { node = inner; max_attempts; backoff; retry_on } ->
        [
          ("type", `String "retry");
          ("node", node_to_json inner);
          ("max_attempts", `Int max_attempts);
          ("backoff", backoff_to_json backoff);
          ("retry_on", `List (List.map (fun s -> `String s) retry_on));
        ]

    | Fallback { primary; fallbacks } ->
        [
          ("type", `String "fallback");
          ("primary", node_to_json primary);
          ("fallbacks", `List (List.map node_to_json fallbacks));
        ]

    | Race { nodes; timeout } ->
        let fields = [
          ("type", `String "race");
          ("nodes", `List (List.map node_to_json nodes));
        ] in
        (match timeout with
         | Some t -> fields @ [("timeout", `Float t)]
         | None -> fields)

    | ChainExec { chain_source; validate; max_depth; sandbox; context_inject; pass_outputs } ->
        let base_fields = [
          ("type", `String "chain_exec");
          ("chain_source", `String chain_source);
          ("validate", `Bool validate);
          ("max_depth", `Int max_depth);
          ("sandbox", `Bool sandbox);
          ("pass_outputs", `Bool pass_outputs);
        ] in
        let inject_fields =
          if context_inject = [] then []
          else [("context_inject", `Assoc (List.map (fun (k, v) -> (k, `String v)) context_inject))]
        in
        base_fields @ inject_fields

    | Adapter { input_ref; transform; on_error } ->
        [
          ("type", `String "adapter");
          ("input_ref", `String input_ref);
          ("transform", adapter_transform_to_json transform);
          ("on_error", on_error_to_json on_error);
        ]

    | Cache { key_expr; ttl_seconds; inner } ->
        [
          ("type", `String "cache");
          ("key_expr", `String key_expr);
          ("ttl_seconds", `Int ttl_seconds);
          ("inner", node_to_json inner);
        ]

    | Batch { batch_size; parallel; inner; collect_strategy } ->
        let strategy_str = match collect_strategy with
          | `List -> "list" | `Concat -> "concat" | `First -> "first" | `Last -> "last"
        in
        [
          ("type", `String "batch");
          ("batch_size", `Int batch_size);
          ("parallel", `Bool parallel);
          ("inner", node_to_json inner);
          ("collect_strategy", `String strategy_str);
        ]
    | Spawn { clean; inner; pass_vars; inherit_cache } ->
        [
          ("type", `String "spawn");
          ("clean", `Bool clean);
          ("inner", node_to_json inner);
          ("pass_vars", `List (List.map (fun v -> `String v) pass_vars));
          ("inherit_cache", `Bool inherit_cache);
        ]
    | Mcts { strategies; simulation; evaluator; evaluator_prompt; policy;
             max_iterations; max_depth; expansion_threshold; early_stop; parallel_sims } ->
        let policy_json = match policy with
          | UCB1 c -> `Assoc [("type", `String "ucb1"); ("c", `Float c)]
          | Greedy -> `Assoc [("type", `String "greedy")]
          | EpsilonGreedy e -> `Assoc [("type", `String "epsilon_greedy"); ("epsilon", `Float e)]
          | Softmax t -> `Assoc [("type", `String "softmax"); ("temperature", `Float t)]
        in
        [
          ("type", `String "mcts");
          ("strategies", `List (List.map node_to_json strategies));
          ("simulation", node_to_json simulation);
          ("evaluator", `String evaluator);
          ("evaluator_prompt", match evaluator_prompt with Some p -> `String p | None -> `Null);
          ("policy", policy_json);
          ("max_iterations", `Int max_iterations);
          ("max_depth", `Int max_depth);
          ("expansion_threshold", `Int expansion_threshold);
          ("early_stop", match early_stop with Some s -> `Float s | None -> `Null);
          ("parallel_sims", `Int parallel_sims);
        ]
    | StreamMerge { nodes; reducer; initial; min_results; timeout } ->
        let reducer_json = match reducer with
          | First -> `String "first"
          | Last -> `String "last"
          | Concat -> `String "concat"
          | WeightedAvg -> `String "weighted_avg"
          | Custom s -> `Assoc [("type", `String "custom"); ("name", `String s)]
        in
        [
          ("type", `String "stream_merge");
          ("nodes", `List (List.map node_to_json nodes));
          ("reducer", reducer_json);
          ("initial", `String initial);
          ("min_results", match min_results with Some n -> `Int n | None -> `Null);
          ("timeout", match timeout with Some t -> `Float t | None -> `Null);
        ]
  in
  `Assoc (base @ type_fields @ input_mapping)

(** Serialize chain to JSON (inner) *)
and chain_to_json_inner (c : chain) : Yojson.Safe.t =
  `Assoc [
    ("id", `String c.id);
    ("nodes", `List (List.map node_to_json c.nodes));
    ("output", `String c.output);
    ("config", config_to_json c.config);
  ]

(** Main entry point: Serialize chain to JSON *)
let chain_to_json (c : chain) : Yojson.Safe.t = chain_to_json_inner c

(** Serialize chain to JSON string (pretty-printed) *)
let chain_to_json_string ?(pretty=true) (c : chain) : string =
  let json = chain_to_json c in
  if pretty then Yojson.Safe.pretty_to_string json
  else Yojson.Safe.to_string json

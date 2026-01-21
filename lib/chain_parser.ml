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
      with _ -> Error (Printf.sprintf "Invalid threshold value in: %s" s))
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
    with _ -> default
  in
  let get_bool_opt key default =
    try json |> member key |> to_bool
    with _ -> default
  in
  let get_direction_opt key default =
    try
      let s = json |> member key |> to_string in
      direction_of_string s
    with _ -> default
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

(** Parse a single node from JSON *)
let rec parse_node (json : Yojson.Safe.t) : (node, string) result =
  let open Yojson.Safe.Util in
  try
    let* id = require_string json "id" in
    let* node_type_str = require_string json "type" in

    let* node_type = parse_node_type json node_type_str in

    (* Parse explicit input_mapping if provided, otherwise extract from prompt/args *)
    let input_mapping =
      try
        let mapping_json = json |> member "input_mapping" in
        match mapping_json with
        | `List pairs ->
            List.filter_map (fun pair ->
              match pair with
              | `List [`String k; `String v] -> Some (k, v)
              | _ -> None
            ) pairs
        | `Null ->
            (match node_type with
             | Llm { prompt; _ } -> extract_input_mappings prompt
             | Tool { args; _ } -> extract_json_mappings args
             | _ -> [])
        | _ -> []
      with _ ->
        match node_type with
        | Llm { prompt; _ } -> extract_input_mappings prompt
        | Tool { args; _ } -> extract_json_mappings args
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
      let timeout =
        try Some (json |> member "timeout" |> to_int)
        with _ -> None
      in
      let tools =
        try
          match json |> member "tools" with
          | `Null -> None
          | v -> Some v
        with _ -> None
      in
      Ok (Llm { model; prompt; timeout; tools })

  | "tool" ->
      let* name = require_string json "name" in
      let args =
        try json |> member "args"
        with _ -> `Assoc []
      in
      Ok (Tool { name; args })

  | "pipeline" ->
      let nodes_json = json |> member "nodes" |> to_list in
      let* nodes = parse_nodes nodes_json in
      Ok (Pipeline nodes)

  | "fanout" ->
      let branches_json =
        try json |> member "branches" |> to_list
        with _ -> json |> member "nodes" |> to_list
      in
      let* nodes = parse_nodes branches_json in
      Ok (Fanout nodes)

  | "quorum" ->
      let required = json |> member "required" |> to_int in
      let nodes_json =
        try json |> member "nodes" |> to_list
        with _ -> json |> member "inputs" |> to_list
      in
      let* nodes = parse_nodes nodes_json in
      Ok (Quorum { required; nodes })

  | "gate" ->
      let* condition = require_string json "condition" in
      let then_json = json |> member "then" in
      let* then_node = parse_node then_json in
      let else_node =
        try
          let else_json = json |> member "else" in
          match parse_node else_json with
          | Ok n -> Some n
          | Error _ -> None
        with _ -> None
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
      let strategy_str =
        try json |> member "strategy" |> to_string
        with _ -> "concat"
      in
      let* strategy = parse_merge_strategy strategy_str in
      let nodes_json =
        try json |> member "nodes" |> to_list
        with _ -> json |> member "inputs" |> to_list
      in
      let* nodes = parse_nodes nodes_json in
      Ok (Merge { strategy; nodes })

  | "threshold" ->
      let* metric = require_string json "metric" in
      let* operator_str = require_string json "operator" in
      let* operator = parse_threshold_op operator_str in
      let value =
        try json |> member "value" |> to_float
        with _ ->
          try float_of_int (json |> member "value" |> to_int)
          with _ -> 0.0
      in
      let input_json = json |> member "input_node" in
      let* input_node = parse_node input_json in
      let on_pass =
        try
          let pass_json = json |> member "on_pass" in
          match parse_node pass_json with
          | Ok n -> Some n
          | Error _ -> None
        with _ -> None
      in
      let on_fail =
        try
          let fail_json = json |> member "on_fail" in
          match parse_node fail_json with
          | Ok n -> Some n
          | Error _ -> None
        with _ -> None
      in
      Ok (Threshold { metric; operator; value; input_node; on_pass; on_fail })

  | "goal_driven" ->
      let* goal_metric = require_string json "goal_metric" in
      let* goal_operator_str = require_string json "goal_operator" in
      let* goal_operator = parse_threshold_op goal_operator_str in
      let goal_value =
        try json |> member "goal_value" |> to_float
        with _ ->
          try float_of_int (json |> member "goal_value" |> to_int)
          with _ -> 0.0
      in
      let action_json = json |> member "action_node" in
      let* action_node = parse_node action_json in
      let* measure_func = require_string json "measure_func" in
      let max_iterations =
        try json |> member "max_iterations" |> to_int
        with _ -> 10
      in
      let strategy_hints =
        try
          json |> member "strategy_hints" |> to_assoc
          |> List.map (fun (k, v) -> (k, to_string v))
        with _ -> []
      in
      let conversational =
        try json |> member "conversational" |> to_bool
        with _ -> false
      in
      let relay_models =
        try json |> member "relay_models" |> to_list |> List.map to_string
        with _ -> []
      in
      Ok (GoalDriven {
        goal_metric; goal_operator; goal_value;
        action_node; measure_func; max_iterations; strategy_hints;
        conversational; relay_models
      })

  | "evaluator" ->
      let candidates_json =
        try json |> member "candidates" |> to_list
        with _ -> []
      in
      let* candidates = parse_nodes candidates_json in
      let* scoring_func = require_string json "scoring_func" in
      let scoring_prompt =
        try Some (json |> member "scoring_prompt" |> to_string)
        with _ -> None
      in
      let select_strategy_json =
        try json |> member "select_strategy"
        with _ -> `String "best"
      in
      let* select_strategy = parse_select_strategy select_strategy_json in
      let min_score =
        try Some (json |> member "min_score" |> to_float)
        with _ ->
          try Some (float_of_int (json |> member "min_score" |> to_int))
          with _ -> None
      in
      Ok (Evaluator { candidates; scoring_func; scoring_prompt; select_strategy; min_score })

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
      try json |> member "id" |> to_string
      with _ -> Printf.sprintf "subgraph_%d" (Random.int 10000)
    in
    let nodes_json = json |> member "nodes" |> to_list in
    let* nodes = parse_nodes nodes_json in
    let output = json |> member "output" |> to_string in
    let config =
      try parse_config (json |> member "config")
      with _ -> default_config
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

(** Validate chain structure *)
let validate_chain (c : Chain_types.chain) : (unit, string) result =
  (* Check output node exists *)
  let node_ids = List.map (fun (n : Chain_types.node) -> n.id) c.Chain_types.nodes in
  if not (List.mem c.Chain_types.output node_ids) then
    Error (Printf.sprintf "Output node '%s' not found in chain" c.Chain_types.output)
  (* Check for duplicate IDs *)
  else
    let rec check_dups seen = function
      | [] -> Ok ()
      | id :: rest ->
          if List.mem id seen then
            Error (Printf.sprintf "Duplicate node ID: %s" id)
          else
            check_dups (id :: seen) rest
    in
    check_dups [] node_ids

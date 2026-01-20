(** Chain Mermaid Parser - Parse Mermaid flowcharts into Chain AST

    Enables "Executable Documentation" - the same Mermaid diagram that
    renders beautifully on GitHub can be executed as a real workflow.

    ═══════════════════════════════════════════════════════════════════
    FULL 1:1 MAPPING: Mermaid ↔ Chain AST
    ═══════════════════════════════════════════════════════════════════

    ┌─────────────────────────────┬─────────────────────────────────────┐
    │ Mermaid Syntax              │ Chain AST Type                      │
    ├─────────────────────────────┼─────────────────────────────────────┤
    │ [LLM:model "prompt"]        │ Llm { model; prompt }               │
    │ [Tool:name]                 │ Tool { name; args }                 │
    │ [[Ref:chain_id]]            │ ChainRef chain_id                   │
    │ {Quorum:N}                  │ Quorum { required = N; nodes }      │
    │ {Gate:condition}            │ Gate { condition; then; else }      │
    │ {Merge:strategy}            │ Merge { strategy; nodes }           │
    │ [[Pipeline:a,b,c]]          │ Pipeline [a; b; c]                  │
    │ [[Fanout:a,b,c]]            │ Fanout [a; b; c]                    │
    │ [[Map:func,node]]           │ Map { func; inner }                 │
    │ [[Bind:func,node]]          │ Bind { func; inner }                │
    └─────────────────────────────┴─────────────────────────────────────┘

    Merge strategies: weighted_avg, first, last, concat, or custom name

    ═══════════════════════════════════════════════════════════════════
    COMPOSABILITY: All types can compose with each other
    ═══════════════════════════════════════════════════════════════════

    Example: LLM -> Pipeline -> Map -> Quorum
    {[
      graph LR
          A[LLM:gemini "Parse"] --> P[[Pipeline:step1,step2]]
          P --> M[[Map:format,result]]
          M --> Q{Quorum:2}
    ]}

    ═══════════════════════════════════════════════════════════════════
    NODE SHAPES
    ═══════════════════════════════════════════════════════════════════

    - [...]   = Rectangle: LLM or Tool nodes
    - {...}   = Diamond: Quorum, Gate, or Merge nodes
    - [[...]] = Subroutine: Ref, Pipeline, Fanout, Map, Bind
*)

open Chain_types

(** Parsed node from Mermaid *)
type mermaid_node = {
  id : string;
  shape : [ `Rect | `Diamond | `Subroutine ];
  content : string;
}

(** Parsed edge from Mermaid *)
type mermaid_edge = {
  from_nodes : string list;  (* Multiple for merge: A & B --> C *)
  to_node : string;
}

(** Parsed Mermaid graph *)
type mermaid_graph = {
  direction : string;  (* LR, TB, etc. *)
  nodes : mermaid_node list;
  edges : mermaid_edge list;
}

(** Helper: trim whitespace *)
let trim s = String.trim s

(* Pre-compiled regexes for better performance and reliability *)
(* Support both plain [text] and quoted ["text"] syntax *)
let rect_re = Str.regexp {|\([A-Za-z_][A-Za-z0-9_]*\)\[\([^]]*\)\]|}
let diamond_re = Str.regexp {|\([A-Za-z_][A-Za-z0-9_]*\){\([^}]*\)}|}
let subroutine_re = Str.regexp {|\([A-Za-z_][A-Za-z0-9_]*\)\[\[\([^]]*\)\]\]|}
let arrow_re = Str.regexp {|[ ]*-->[ ]*|}
let ampersand_re = Str.regexp {|[ ]*&[ ]*|}
let quote_re = Str.regexp {|\([^ "]+\)[ ]*"\([^"]*\)"|}
let simple_model_re = Str.regexp {|\([^ ]+\)|}
let quorum_id_re = Str.regexp {|quorum_\([0-9]+\)|}
let consensus_id_re = Str.regexp {|consensus_\([0-9]+\)|}

(** Known LLM model names *)
let llm_models = ["gemini"; "claude"; "codex"; "gpt"; "gpt4"; "gpt5"; "o1"; "o3"; "sonnet"; "opus"; "haiku"]

(** Known tool names *)
let known_tools = ["eslint"; "tsc"; "prettier"; "jest"; "vitest"; "cargo"; "dune"; "make"; "npm"; "yarn"; "pnpm"]

(** Strip surrounding quotes from text if present *)
let strip_quotes s =
  let s = trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len-1] = '"' then
    String.sub s 1 (len - 2)
  else
    s

(** Infer node type from node ID and shape *)
let infer_type_from_id (id : string) (shape : [ `Rect | `Diamond | `Subroutine ]) (text : string)
    : (node_type, string) result =
  let id_lower = String.lowercase_ascii id in
  let text = strip_quotes text in

  match shape with
  | `Diamond ->
      (* Diamond nodes: Quorum, Gate, or Merge *)
      if Str.string_match quorum_id_re id_lower 0 then
        let n = int_of_string (Str.matched_group 1 id_lower) in
        Ok (Quorum { required = n; nodes = [] })
      else if Str.string_match consensus_id_re id_lower 0 then
        let n = int_of_string (Str.matched_group 1 id_lower) in
        Ok (Quorum { required = n; nodes = [] })
      else if String.length id_lower >= 5 && String.sub id_lower 0 5 = "gate_" then
        Ok (Gate { condition = text; then_node = { id = "_placeholder"; node_type = ChainRef "_"; input_mapping = [] }; else_node = None })
      else if String.length id_lower >= 6 && String.sub id_lower 0 6 = "merge_" then
        Ok (Merge { strategy = Concat; nodes = [] })
      else
        (* Default diamond: try to parse old syntax or default to Quorum *)
        (try
          let n = Scanf.sscanf text "Quorum:%d" (fun n -> n) in
          Ok (Quorum { required = n; nodes = [] })
        with _ ->
          Ok (Gate { condition = text; then_node = { id = "_placeholder"; node_type = ChainRef "_"; input_mapping = [] }; else_node = None }))

  | `Subroutine ->
      (* Subroutine nodes: Ref, Pipeline, Fanout, Map, Bind *)
      if String.length id_lower >= 4 && String.sub id_lower 0 4 = "ref_" then
        let ref_id = String.sub id (4) (String.length id - 4) in
        Ok (ChainRef ref_id)
      else if String.length id_lower >= 4 && String.sub id_lower 0 4 = "seq_" then
        Ok (Pipeline [])
      else if String.length id_lower >= 4 && String.sub id_lower 0 4 = "par_" then
        Ok (Fanout [])
      else if String.length id_lower >= 4 && String.sub id_lower 0 4 = "map_" then
        Ok (Map { func = text; inner = { id = "_placeholder"; node_type = ChainRef "_"; input_mapping = [] } })
      else
        (* Default subroutine: ChainRef using the content as chain ID *)
        let ref_id = if text = "" then id else text in
        Ok (ChainRef ref_id)

  | `Rect ->
      (* Rectangle nodes: LLM or Tool *)
      (* Check if ID starts with known LLM model *)
      let is_llm = List.exists (fun model ->
        String.length id_lower >= String.length model &&
        String.sub id_lower 0 (String.length model) = model
      ) llm_models in

      if is_llm then
        (* Extract model from ID (e.g., "gemini_parse" -> "gemini") *)
        let model = List.find (fun m ->
          String.length id_lower >= String.length m &&
          String.sub id_lower 0 (String.length m) = m
        ) llm_models in
        Ok (Llm { model; prompt = if text = "" then "{{input}}" else text; timeout = None })
      else if List.mem id_lower known_tools then
        Ok (Tool { name = id; args = `Null })
      else
        (* Default: treat as LLM with "gemini" default model, text as prompt *)
        (* If ID is short (1-2 chars) or generic, use text as prompt *)
        let prompt = if text = "" then id else text in
        Ok (Llm { model = "gemini"; prompt; timeout = None })

(** Parse node shape and extract content *)
let parse_node_definition (s : string) : (string * mermaid_node) option =
  let s = trim s in
  if s = "" then None
  (* Try subroutine first (more specific pattern: [[...]]) *)
  else if Str.string_match subroutine_re s 0 then
    let id = Str.matched_group 1 s in
    let content = Str.matched_group 2 s in
    Some (id, { id; shape = `Subroutine; content = trim content })
  (* Then diamond: {...} *)
  else if Str.string_match diamond_re s 0 then
    let id = Str.matched_group 1 s in
    let content = Str.matched_group 2 s in
    Some (id, { id; shape = `Diamond; content = trim content })
  (* Finally rectangle: [...] *)
  else if Str.string_match rect_re s 0 then
    let id = Str.matched_group 1 s in
    let content = Str.matched_group 2 s in
    Some (id, { id; shape = `Rect; content = trim content })
  else
    None

(** Parse node content into Chain node_type *)
let parse_node_content (shape : [ `Rect | `Diamond | `Subroutine ]) (content : string)
    : (node_type, string) result =
  let content = trim content in
  match shape with
  | `Subroutine ->
      (* [[Ref:chain_id]] or [[Pipeline:A,B,C]] or [[Fanout:A,B,C]] or [[Map:func,node]] or [[Bind:func,node]] *)
      if String.length content > 4 && String.sub content 0 4 = "Ref:" then
        let ref_id = trim (String.sub content 4 (String.length content - 4)) in
        Ok (ChainRef ref_id)
      else if String.length content > 9 && String.sub content 0 9 = "Pipeline:" then
        (* [[Pipeline:A,B,C]] - sequential execution *)
        let node_ids = String.sub content 9 (String.length content - 9)
          |> String.split_on_char ','
          |> List.map trim
          |> List.filter (fun s -> s <> "")
        in
        let placeholder_nodes = List.map (fun id ->
          { id; node_type = ChainRef id; input_mapping = [] }
        ) node_ids in
        Ok (Pipeline placeholder_nodes)
      else if String.length content > 7 && String.sub content 0 7 = "Fanout:" then
        (* [[Fanout:A,B,C]] - parallel execution *)
        let node_ids = String.sub content 7 (String.length content - 7)
          |> String.split_on_char ','
          |> List.map trim
          |> List.filter (fun s -> s <> "")
        in
        let placeholder_nodes = List.map (fun id ->
          { id; node_type = ChainRef id; input_mapping = [] }
        ) node_ids in
        Ok (Fanout placeholder_nodes)
      else if String.length content > 4 && String.sub content 0 4 = "Map:" then
        (* [[Map:func,node]] - transform output *)
        let parts = String.sub content 4 (String.length content - 4)
          |> String.split_on_char ','
          |> List.map trim
        in
        (match parts with
        | [func; node_id] ->
            Ok (Map { func; inner = { id = node_id; node_type = ChainRef node_id; input_mapping = [] } })
        | _ ->
            Error (Printf.sprintf "Map requires func,node format, got: %s" content))
      else if String.length content > 5 && String.sub content 0 5 = "Bind:" then
        (* [[Bind:func,node]] - dynamic routing *)
        let parts = String.sub content 5 (String.length content - 5)
          |> String.split_on_char ','
          |> List.map trim
        in
        (match parts with
        | [func; node_id] ->
            Ok (Bind { func; inner = { id = node_id; node_type = ChainRef node_id; input_mapping = [] } })
        | _ ->
            Error (Printf.sprintf "Bind requires func,node format, got: %s" content))
      else
        Error (Printf.sprintf "Subroutine node must be Ref/Pipeline/Fanout/Map/Bind, got: %s" content)

  | `Diamond ->
      (* {Quorum:N} or {Gate:condition} *)
      if String.length content > 7 && String.sub content 0 7 = "Quorum:" then
        let n_str = trim (String.sub content 7 (String.length content - 7)) in
        (try
          let required = int_of_string n_str in
          (* Quorum nodes need their inputs filled in later from edges *)
          Ok (Quorum { required; nodes = [] })
        with _ ->
          Error (Printf.sprintf "Invalid quorum count: %s" n_str))
      else if String.length content > 5 && String.sub content 0 5 = "Gate:" then
        let condition = trim (String.sub content 5 (String.length content - 5)) in
        (* Gate needs then/else filled in from edges *)
        Ok (Gate { condition; then_node = { id = "_placeholder"; node_type = ChainRef "_"; input_mapping = [] }; else_node = None })
      else if String.length content > 6 && String.sub content 0 6 = "Merge:" then
        (* {Merge:strategy} - e.g., {Merge:weighted_average} *)
        let strategy_str = trim (String.sub content 6 (String.length content - 6)) in
        let strategy = match strategy_str with
          | "weighted_avg" | "weighted" -> WeightedAvg
          | "first" -> First
          | "last" -> Last
          | "concat" -> Concat
          | s -> Custom s  (* custom strategy name *)
        in
        (* Merge nodes need their inputs filled in later from edges *)
        Ok (Merge { strategy; nodes = [] })
      else
        Error (Printf.sprintf "Diamond node must be Quorum:N, Gate:condition, or Merge:strategy, got: %s" content)

  | `Rect ->
      (* [LLM:model "prompt"] or [Tool:name] *)
      if String.length content > 4 && String.sub content 0 4 = "LLM:" then
        let rest = String.sub content 4 (String.length content - 4) in
        (* Parse: model "prompt" or just model *)
        if Str.string_match quote_re rest 0 then
          let model = Str.matched_group 1 rest in
          let prompt = Str.matched_group 2 rest in
          Ok (Llm { model = trim model; prompt = trim prompt; timeout = None })
        else if Str.string_match simple_model_re rest 0 then
          let model = Str.matched_group 1 rest in
          Ok (Llm { model = trim model; prompt = "{{input}}"; timeout = None })
        else
          Error (Printf.sprintf "Invalid LLM format: %s" content)
      else if String.length content > 5 && String.sub content 0 5 = "Tool:" then
        let name = trim (String.sub content 5 (String.length content - 5)) in
        Ok (Tool { name; args = `Assoc [] })
      else
        (* Default: treat as LLM with content as prompt, model = gemini *)
        Ok (Llm { model = "gemini"; prompt = content; timeout = None })

(** Parse edge line: A --> B or A & B --> C *)
let parse_edge_line (line : string) : mermaid_edge list =
  let line = trim line in
  (* Split by --> using pre-compiled regex *)
  let parts = Str.split arrow_re line in

  let rec build_edges acc = function
    | [] | [_] -> List.rev acc
    | from_part :: to_part :: rest ->
        (* Parse from_part: could be "A" or "A & B" *)
        let from_nodes =
          from_part
          |> Str.split ampersand_re
          |> List.map (fun s ->
              (* Extract just the ID, not the node definition *)
              let s = trim s in
              match parse_node_definition s with
              | Some (id, _) -> id
              | None -> s)
        in
        (* Extract to_node ID *)
        let to_node =
          let s = trim to_part in
          match parse_node_definition s with
          | Some (id, _) -> id
          | None -> s
        in
        let edge = { from_nodes; to_node } in
        build_edges (edge :: acc) (to_part :: rest)
  in
  build_edges [] parts

(** Parse full Mermaid graph text *)
let parse_mermaid_text (text : string) : (mermaid_graph, string) result =
  let lines = String.split_on_char '\n' text |> List.map trim in

  (* Find graph direction *)
  let direction = ref "LR" in
  let nodes = Hashtbl.create 16 in
  let edges = ref [] in

  List.iter (fun line ->
    let line = trim line in
    (* Skip empty lines and comments *)
    if line = "" || (String.length line > 0 && line.[0] = '%') then ()
    (* Parse graph direction *)
    else if String.length line >= 5 && String.sub line 0 5 = "graph" then begin
      let rest = trim (String.sub line 5 (String.length line - 5)) in
      if rest <> "" then direction := rest
    end
    else if String.length line >= 9 && String.sub line 0 9 = "flowchart" then begin
      let rest = trim (String.sub line 9 (String.length line - 9)) in
      if rest <> "" then direction := rest
    end
    (* Skip subgraph/end for now *)
    else if String.length line >= 8 && String.sub line 0 8 = "subgraph" then ()
    else if line = "end" then ()
    (* Parse edges and collect nodes *)
    else begin
      (* Extract all node definitions from the line *)
      let parts = Str.split arrow_re line in
      List.iter (fun part ->
        let part = trim part in
        (* Split by & for multiple nodes *)
        let sub_parts = Str.split ampersand_re part in
        List.iter (fun sub ->
          match parse_node_definition (trim sub) with
          | Some (id, node) -> Hashtbl.replace nodes id node
          | None -> ()
        ) sub_parts
      ) parts;
      (* Parse edges *)
      let new_edges = parse_edge_line line in
      edges := !edges @ new_edges
    end
  ) lines;

  Ok {
    direction = !direction;
    nodes = Hashtbl.fold (fun _ node acc -> node :: acc) nodes [];
    edges = !edges;
  }

(** Build dependency graph from edges *)
let build_dependency_graph (edges : mermaid_edge list) : (string, string list) Hashtbl.t =
  let deps = Hashtbl.create 16 in
  List.iter (fun edge ->
    let existing =
      match Hashtbl.find_opt deps edge.to_node with
      | Some l -> l
      | None -> []
    in
    Hashtbl.replace deps edge.to_node (existing @ edge.from_nodes)
  ) edges;
  deps

(** Find nodes with no outgoing edges (terminal nodes) *)
let find_output_nodes (graph : mermaid_graph) : string list =
  let has_outgoing = Hashtbl.create 16 in
  List.iter (fun edge ->
    List.iter (fun from_node ->
      Hashtbl.replace has_outgoing from_node true
    ) edge.from_nodes
  ) graph.edges;

  graph.nodes
  |> List.filter (fun node -> not (Hashtbl.mem has_outgoing node.id))
  |> List.map (fun node -> node.id)

(** Convert Mermaid graph to Chain AST *)
let mermaid_to_chain ?(id = "mermaid_chain") (graph : mermaid_graph) : (chain, string) result =
  let deps = build_dependency_graph graph.edges in

  (* Convert each mermaid node to chain node *)
  let node_map = Hashtbl.create 16 in

  let convert_result = ref (Ok ()) in

  List.iter (fun mnode ->
    match !convert_result with
    | Error _ -> ()
    | Ok () ->
        (* Try new inference-based parsing first, fall back to old explicit syntax *)
        let parse_result =
          (* Check if content uses old explicit syntax (LLM:, Tool:, Ref:, etc.) *)
          let content = trim mnode.content in
          let uses_old_syntax =
            (String.length content > 4 && String.sub content 0 4 = "LLM:") ||
            (String.length content > 5 && String.sub content 0 5 = "Tool:") ||
            (String.length content > 4 && String.sub content 0 4 = "Ref:") ||
            (String.length content > 7 && String.sub content 0 7 = "Quorum:") ||
            (String.length content > 5 && String.sub content 0 5 = "Gate:") ||
            (String.length content > 6 && String.sub content 0 6 = "Merge:") ||
            (String.length content > 9 && String.sub content 0 9 = "Pipeline:") ||
            (String.length content > 7 && String.sub content 0 7 = "Fanout:") ||
            (String.length content > 4 && String.sub content 0 4 = "Map:") ||
            (String.length content > 5 && String.sub content 0 5 = "Bind:")
          in
          if uses_old_syntax then
            parse_node_content mnode.shape mnode.content
          else
            infer_type_from_id mnode.id mnode.shape mnode.content
        in
        match parse_result with
        | Error e -> convert_result := Error e
        | Ok node_type ->
            (* For Quorum and Merge nodes, we need to fill in the child nodes *)
            let node_type = match node_type with
              | Quorum { required; nodes = _ } ->
                  let input_ids =
                    match Hashtbl.find_opt deps mnode.id with
                    | Some ids -> ids
                    | None -> []
                  in
                  (* Create placeholder nodes for inputs *)
                  let input_nodes = List.map (fun input_id ->
                    { id = input_id; node_type = ChainRef input_id; input_mapping = [] }
                  ) input_ids in
                  Quorum { required; nodes = input_nodes }
              | Merge { strategy; nodes = _ } ->
                  let input_ids =
                    match Hashtbl.find_opt deps mnode.id with
                    | Some ids -> ids
                    | None -> []
                  in
                  let input_nodes = List.map (fun input_id ->
                    { id = input_id; node_type = ChainRef input_id; input_mapping = [] }
                  ) input_ids in
                  Merge { strategy; nodes = input_nodes }
              | other -> other
            in
            let input_mapping =
              match Hashtbl.find_opt deps mnode.id with
              | Some inputs ->
                  List.map (fun inp -> (inp, Printf.sprintf "{{%s.output}}" inp)) inputs
              | None -> []
            in
            let node = { id = mnode.id; node_type; input_mapping } in
            Hashtbl.replace node_map mnode.id node
  ) graph.nodes;

  match !convert_result with
  | Error e -> Error e
  | Ok () ->
      (* Build node list *)
      let nodes = Hashtbl.fold (fun _ node acc -> node :: acc) node_map [] in

      (* Find output node *)
      let output_nodes = find_output_nodes graph in
      let output = match output_nodes with
        | [single] -> single
        | first :: _ -> first  (* Take first if multiple *)
        | [] ->
            (* No terminal node, use last defined *)
            match List.rev graph.nodes with
            | last :: _ -> last.id
            | [] -> "output"
      in

      Ok {
        id;
        nodes;
        output;
        config = { default_config with direction = direction_of_string graph.direction };
      }

(** Main entry point: Parse Mermaid text into Chain *)
let parse_chain (text : string) : (chain, string) result =
  match parse_mermaid_text text with
  | Error e -> Error e
  | Ok graph -> mermaid_to_chain graph

(** Parse with custom chain ID *)
let parse_chain_with_id ~id (text : string) : (chain, string) result =
  match parse_mermaid_text text with
  | Error e -> Error e
  | Ok graph -> mermaid_to_chain ~id graph

(* ═══════════════════════════════════════════════════════════════════
   REVERSE DIRECTION: Chain AST → Mermaid
   ═══════════════════════════════════════════════════════════════════ *)

(** Convert a node_type to Mermaid node ID suggestion *)
let node_type_to_id (nt : node_type) (fallback : string) : string =
  match nt with
  | Llm { model; _ } -> model
  | Tool { name; _ } -> name
  | Quorum { required; _ } -> Printf.sprintf "quorum_%d" required
  | Gate { condition; _ } ->
      let safe_cond = Str.global_replace (Str.regexp "[^a-zA-Z0-9_]") "_" condition in
      Printf.sprintf "gate_%s" (String.sub safe_cond 0 (min 10 (String.length safe_cond)))
  | Merge _ -> "merge"
  | Pipeline _ -> "seq"
  | Fanout _ -> "par"
  | Map { func; _ } -> Printf.sprintf "map_%s" func
  | Bind { func; _ } -> Printf.sprintf "bind_%s" func
  | ChainRef ref_id -> Printf.sprintf "ref_%s" ref_id
  | Subgraph _ -> fallback

(** Convert a node_type to Mermaid node text (prompt/description) *)
let node_type_to_text (nt : node_type) : string =
  match nt with
  | Llm { prompt; _ } -> if prompt = "{{input}}" then "" else prompt
  | Tool { name; _ } -> Printf.sprintf "Run %s" name
  | Quorum { required; nodes } ->
      Printf.sprintf "%d/%d must agree" required (max (List.length nodes) required)
  | Gate { condition; _ } -> condition
  | Merge { strategy; _ } ->
      (match strategy with
       | First -> "Take first"
       | Last -> "Take last"
       | Concat -> "Combine all"
       | WeightedAvg -> "Weighted average"
       | Custom s -> s)
  | Pipeline nodes -> Printf.sprintf "Sequence of %d" (List.length nodes)
  | Fanout nodes -> Printf.sprintf "Parallel %d" (List.length nodes)
  | Map { func; _ } -> func
  | Bind { func; _ } -> func
  | ChainRef ref_id -> ref_id
  | Subgraph sub -> sub.id

(** Convert a node_type to Mermaid shape *)
let node_type_to_shape (nt : node_type) : string * string =
  match nt with
  | Llm _ | Tool _ -> ("[", "]")
  | Quorum _ | Gate _ | Merge _ -> ("{", "}")
  | Pipeline _ | Fanout _ | Map _ | Bind _ | ChainRef _ | Subgraph _ -> ("[[", "]]")

(** Convert Chain AST to Mermaid text (standard-compliant, uses chain.config.direction) *)
let chain_to_mermaid (chain : chain) : string =
  let buf = Buffer.create 256 in
  let dir = direction_to_string chain.config.direction in
  Buffer.add_string buf (Printf.sprintf "graph %s\n" dir);

  (* Build edge map: target -> sources *)
  let edges = Hashtbl.create 16 in
  List.iter (fun (node : node) ->
    List.iter (fun (src, _) ->
      let existing = match Hashtbl.find_opt edges node.id with
        | Some l -> l
        | None -> []
      in
      Hashtbl.replace edges node.id (src :: existing)
    ) node.input_mapping
  ) chain.nodes;

  (* Output nodes *)
  List.iter (fun (node : node) ->
    let (shape_open, shape_close) = node_type_to_shape node.node_type in
    let text = node_type_to_text node.node_type in
    let text_escaped = Str.global_replace (Str.regexp {|"|}) {|'|} text in
    Buffer.add_string buf (Printf.sprintf "    %s%s\"%s\"%s\n"
      node.id shape_open text_escaped shape_close)
  ) chain.nodes;

  (* Output edges *)
  Hashtbl.iter (fun target sources ->
    List.iter (fun src ->
      Buffer.add_string buf (Printf.sprintf "    %s --> %s\n" src target)
    ) sources
  ) edges;

  Buffer.contents buf

(** Round-trip test: parse and re-serialize *)
let round_trip (text : string) : (string, string) result =
  match parse_chain text with
  | Error e -> Error e
  | Ok chain -> Ok (chain_to_mermaid chain)

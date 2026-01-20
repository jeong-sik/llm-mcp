(** Chain Mermaid Parser - Parse Mermaid flowcharts into Chain AST

    Enables "Executable Documentation" - the same Mermaid diagram that
    renders beautifully on GitHub can be executed as a real workflow.

    Supported Mermaid syntax:
    {[
      graph LR
          A[LLM:gemini "Classify input"] --> B[LLM:claude "Review"]
          A --> C[LLM:codex "Analyze"]
          B & C --> D{Quorum:2}
          D --> E[[Ref:summary_chain]]
    ]}

    Node shapes:
    - [...]   = LLM or Tool node (rectangle)
    - {...}   = Quorum or Gate node (diamond)
    - [[...]] = ChainRef node (subroutine shape)

    Node content format:
    - LLM:model "prompt"     → Llm { model; prompt }
    - Tool:name              → Tool { name; args = {} }
    - Quorum:N               → Quorum { required = N }
    - Gate:condition         → Gate { condition }
    - Ref:chain_id           → ChainRef chain_id
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
let rect_re = Str.regexp {|\([A-Za-z_][A-Za-z0-9_]*\)\[\([^]]*\)\]|}
let diamond_re = Str.regexp {|\([A-Za-z_][A-Za-z0-9_]*\){\([^}]*\)}|}
let subroutine_re = Str.regexp {|\([A-Za-z_][A-Za-z0-9_]*\)\[\[\([^]]*\)\]\]|}
let arrow_re = Str.regexp {|[ ]*-->[ ]*|}
let ampersand_re = Str.regexp {|[ ]*&[ ]*|}
let quote_re = Str.regexp {|\([^ "]+\)[ ]*"\([^"]*\)"|}
let simple_model_re = Str.regexp {|\([^ ]+\)|}

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
      (* [[Ref:chain_id]] *)
      if String.length content > 4 && String.sub content 0 4 = "Ref:" then
        let ref_id = trim (String.sub content 4 (String.length content - 4)) in
        Ok (ChainRef ref_id)
      else
        Error (Printf.sprintf "Subroutine node must be Ref:chain_id, got: %s" content)

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
      else
        Error (Printf.sprintf "Diamond node must be Quorum:N or Gate:condition, got: %s" content)

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
    else if String.length line >= 8 && String.sub line 0 8 = "flowchart" then begin
      let rest = trim (String.sub line 8 (String.length line - 8)) in
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
        match parse_node_content mnode.shape mnode.content with
        | Error e -> convert_result := Error e
        | Ok node_type ->
            (* For Quorum nodes, we need to fill in the child nodes *)
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
        config = default_config;
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

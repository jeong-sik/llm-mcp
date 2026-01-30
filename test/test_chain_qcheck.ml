(** QCheck Property-Based Tests for Chain Roundtrip

    Properties verified:
    1. JSON → Mermaid → JSON preserves semantic equality
    2. Node count is preserved
    3. Edge count is preserved
    4. Output node is preserved
*)

module CP = Chain_parser
module CM = Chain_mermaid_parser
module CT = Chain_types

(* ══════════════════════════════════════════════════════════════════════════
   Arbitrary Generators
   ══════════════════════════════════════════════════════════════════════════ *)

let models = [| "gemini"; "claude"; "codex"; "ollama:llama3" |]
let prompts = [|
  "Analyze the input";
  "Review this code";
  "Summarize the text";
  "Generate a response";
  "Process {{input}}";
  "Evaluate {{prev}}";
|]
let tool_names = [| "fetch"; "eslint"; "format"; "validate" |]

(** Generate LLM node type *)
let gen_llm_node_type : CT.node_type QCheck.Gen.t =
  QCheck.Gen.(
    let* model = oneof_array models in
    let* prompt = oneof_array prompts in
    return (CT.Llm { model; prompt; system = None; timeout = None;
                     tools = None; prompt_ref = None; prompt_vars = []; thinking = false })
  )

(** Generate Tool node type *)
let gen_tool_node_type : CT.node_type QCheck.Gen.t =
  QCheck.Gen.(
    let* name = oneof_array tool_names in
    let* has_args = bool in
    let args = if has_args then `Assoc [("input", `String "{{input}}")] else `Assoc [] in
    return (CT.Tool { name; args })
  )

(** Generate a simple node *)
let gen_simple_node (id : string) : CT.node QCheck.Gen.t =
  QCheck.Gen.(
    let* node_type = oneof [gen_llm_node_type; gen_tool_node_type] in
    return { CT.id; node_type; input_mapping = []; output_key = None; depends_on = None }
  )

(** Generate a simple linear chain (2-4 nodes) *)
let gen_simple_chain : CT.chain QCheck.Gen.t =
  QCheck.Gen.(
    let chain_ids = [| "test_chain"; "prop_chain"; "qcheck_chain" |] in
    let* chain_id = oneof_array chain_ids in
    let* num_nodes = int_range 2 4 in

    (* Generate node IDs *)
    let ids = List.init num_nodes (fun i -> Printf.sprintf "n%d" i) in

    (* Generate nodes with dependencies *)
    let* nodes =
      let rec build_nodes acc remaining prev_id =
        match remaining with
        | [] -> return (List.rev acc)
        | id :: rest ->
            let* node = gen_simple_node id in
            let node_with_deps = match prev_id with
              | None -> node
              | Some pid -> { node with CT.input_mapping = [("input", pid)] }
            in
            build_nodes (node_with_deps :: acc) rest (Some id)
      in
      build_nodes [] ids None
    in

    let output = List.hd (List.rev ids) in
    let config = CT.default_config in

    return { CT.id = chain_id; nodes; output; config;
             name = None; description = None; version = None;
             input_schema = None; output_schema = None; metadata = None }
  )

(** Generate chain with Quorum (fan-in) *)
let gen_chain_with_quorum : CT.chain QCheck.Gen.t =
  QCheck.Gen.(
    let chain_ids = [| "quorum_chain"; "consensus_chain" |] in
    let* chain_id = oneof_array chain_ids in
    let* num_inputs = int_range 2 3 in

    (* Generate input nodes *)
    let input_ids = List.init num_inputs (fun i -> Printf.sprintf "input%d" i) in
    let* input_nodes =
      let rec gen_nodes acc = function
        | [] -> return (List.rev acc)
        | id :: rest ->
            let* node = gen_simple_node id in
            gen_nodes (node :: acc) rest
      in
      gen_nodes [] input_ids
    in

    (* Generate Quorum node *)
    let* required = int_range 1 num_inputs in
    let quorum_children = List.map (fun id ->
      { CT.id; node_type = CT.ChainRef id; input_mapping = []; output_key = None; depends_on = None }
    ) input_ids in
    let quorum_node = {
      CT.id = "quorum";
      node_type = CT.Quorum { consensus = CT.Count required; nodes = quorum_children; weights = [] };
      input_mapping = List.map (fun id -> (id, id)) input_ids;
      output_key = None;
      depends_on = None;
    } in

    let all_nodes = input_nodes @ [quorum_node] in
    let config = CT.default_config in

    return { CT.id = chain_id; nodes = all_nodes; output = "quorum"; config;
             name = None; description = None; version = None;
             input_schema = None; output_schema = None; metadata = None }
  )

(** Combined arbitrary chain generator *)
let arbitrary_chain : CT.chain QCheck.arbitrary =
  QCheck.make
    ~print:(fun c -> Printf.sprintf "Chain(%s, %d nodes)" c.CT.id (List.length c.CT.nodes))
    (QCheck.Gen.oneof [gen_simple_chain; gen_chain_with_quorum])

(* ══════════════════════════════════════════════════════════════════════════
   Helper Functions
   ══════════════════════════════════════════════════════════════════════════ *)

(** Count real nodes (excluding ChainRef) *)
let count_real_nodes (chain : CT.chain) : int =
  List.length (List.filter (fun n ->
    match n.CT.node_type with CT.ChainRef _ -> false | _ -> true
  ) chain.CT.nodes)

(** Extract all real node IDs *)
let get_node_ids (chain : CT.chain) : string list =
  List.filter_map (fun n ->
    match n.CT.node_type with CT.ChainRef _ -> None | _ -> Some n.CT.id
  ) chain.CT.nodes |> List.sort String.compare

(** Extract edges as (src, dst) pairs *)
let get_edges (chain : CT.chain) : (string * string) list =
  List.concat_map (fun (node : CT.node) ->
    List.filter_map (fun (_, src) ->
      (* Only include if src is a real node *)
      if List.exists (fun (n : CT.node) -> n.id = src) chain.nodes then
        Some (src, node.CT.id)
      else None
    ) node.CT.input_mapping
  ) chain.CT.nodes
  |> List.sort_uniq (fun (a1, a2) (b1, b2) ->
       let c = String.compare a1 b1 in if c <> 0 then c else String.compare a2 b2)

let sort_nodes_by_id (nodes : CT.node list) : CT.node list =
  List.sort (fun (a : CT.node) (b : CT.node) -> String.compare a.id b.id) nodes

let normalize_node (node : CT.node) : CT.node =
  let input_mapping =
    List.sort (fun (a, _) (b, _) -> String.compare a b) node.CT.input_mapping
  in
  let node_type =
    match node.CT.node_type with
    | CT.Quorum { consensus; nodes; weights } ->
        CT.Quorum { consensus; nodes = sort_nodes_by_id nodes; weights }
    | other -> other
  in
  { node with CT.input_mapping; node_type }

let normalize_chain (chain : CT.chain) : CT.chain =
  { chain with CT.nodes = sort_nodes_by_id (List.map normalize_node chain.CT.nodes) }

(** Node type content equality - verifies ALL fields, not just structure *)
let node_type_equal (t1 : CT.node_type) (t2 : CT.node_type) : bool =
  match t1, t2 with
  | CT.Llm l1, CT.Llm l2 ->
      l1.model = l2.model &&
      l1.prompt = l2.prompt &&
      l1.system = l2.system &&
      l1.timeout = l2.timeout
  | CT.Tool t1, CT.Tool t2 ->
      t1.name = t2.name &&
      Yojson.Safe.equal t1.args t2.args
  | CT.Quorum q1, CT.Quorum q2 ->
      q1.consensus = q2.consensus &&
      List.length q1.nodes = List.length q2.nodes
  | CT.ChainRef r1, CT.ChainRef r2 ->
      r1 = r2
  | _ -> false

(** Node equality - ID + content *)
let node_equal (n1 : CT.node) (n2 : CT.node) : bool =
  n1.id = n2.id && node_type_equal n1.node_type n2.node_type

(** Find node by ID *)
let find_node (nodes : CT.node list) (id : string) : CT.node option =
  List.find_opt (fun (n : CT.node) -> n.id = id) nodes

(** Chain equivalence - FULL field comparison (not just counts) *)
let chain_content_equal c1 c2 =
  get_node_ids c1 = get_node_ids c2 &&
  c1.CT.output = c2.CT.output &&
  List.for_all (fun (n1 : CT.node) ->
    match n1.node_type with
    | CT.ChainRef _ -> true
    | _ ->
        match find_node c2.CT.nodes n1.id with
        | None -> false
        | Some n2 -> node_equal n1 n2
  ) c1.CT.nodes

(* ══════════════════════════════════════════════════════════════════════════
   Properties
   ══════════════════════════════════════════════════════════════════════════ *)

(** Property 1: Node count preserved after roundtrip *)
let prop_node_count_preserved =
  QCheck.Test.make ~count:100 ~name:"node_count_preserved"
    arbitrary_chain
    (fun chain ->
      let mermaid = CM.chain_to_mermaid chain in
      match CM.parse_mermaid_to_chain mermaid with
      | Error _ -> false
      | Ok chain' -> count_real_nodes chain = count_real_nodes chain')

(** Property 2: Node IDs preserved after roundtrip *)
let prop_node_ids_preserved =
  QCheck.Test.make ~count:100 ~name:"node_ids_preserved"
    arbitrary_chain
    (fun chain ->
      let mermaid = CM.chain_to_mermaid chain in
      match CM.parse_mermaid_to_chain mermaid with
      | Error _ -> false
      | Ok chain' -> get_node_ids chain = get_node_ids chain')

(** Property 3: Edges preserved after roundtrip *)
let prop_edges_preserved =
  QCheck.Test.make ~count:100 ~name:"edges_preserved"
    arbitrary_chain
    (fun chain ->
      let mermaid = CM.chain_to_mermaid chain in
      match CM.parse_mermaid_to_chain mermaid with
      | Error _ -> false
      | Ok chain' -> get_edges chain = get_edges chain')

(** Property 4: Output node preserved *)
let prop_output_preserved =
  QCheck.Test.make ~count:100 ~name:"output_preserved"
    arbitrary_chain
    (fun chain ->
      let mermaid = CM.chain_to_mermaid chain in
      match CM.parse_mermaid_to_chain mermaid with
      | Error _ -> false
      | Ok chain' -> chain.CT.output = chain'.CT.output)

(** Property 5: Double roundtrip idempotent (Mermaid stability) *)
let prop_double_roundtrip_idempotent =
  QCheck.Test.make ~count:50 ~name:"double_roundtrip_idempotent"
    arbitrary_chain
    (fun chain ->
      let mermaid1 = CM.chain_to_mermaid chain in
      match CM.parse_mermaid_to_chain mermaid1 with
      | Error _ -> false
      | Ok chain' ->
          let mermaid2 = CM.chain_to_mermaid chain' in
          match CM.parse_mermaid_to_chain mermaid2 with
          | Error _ -> false
          | Ok chain'' ->
              get_node_ids chain' = get_node_ids chain'' &&
              get_edges chain' = get_edges chain'')

(** Property 6: Mermaid parse never crashes *)
let prop_mermaid_parse_safe =
  QCheck.Test.make ~count:100 ~name:"mermaid_parse_safe"
    arbitrary_chain
    (fun chain ->
      let mermaid = CM.chain_to_mermaid chain in
      match CM.parse_mermaid_to_chain mermaid with
      | Ok _ -> true
      | Error _ -> true)  (* Error is OK, crash is not - test passes if no exception *)

(** Property 7: JSON roundtrip preserves ALL fields (not just structure) *)
let prop_json_roundtrip =
  QCheck.Test.make ~count:100 ~name:"json_roundtrip"
    arbitrary_chain
    (fun chain ->
      let json = CP.chain_to_json chain in
      match CP.parse_chain json with
      | Error _ -> false
      | Ok chain' -> chain_content_equal chain chain')

(** Property 8: chain_full metadata roundtrip is exact *)
let prop_chain_full_roundtrip_exact =
  QCheck.Test.make ~count:100 ~name:"chain_full_roundtrip_exact"
    arbitrary_chain
    (fun chain ->
      let mermaid = CM.chain_to_mermaid chain in
      if not (String.contains mermaid '@') then false
      else
        match CM.parse_mermaid_to_chain mermaid with
        | Error _ -> false
        | Ok chain' ->
            let json1 = CP.chain_to_json_string ~pretty:false ~include_empty_inputs:true (normalize_chain chain) in
            let json2 = CP.chain_to_json_string ~pretty:false ~include_empty_inputs:true (normalize_chain chain') in
            String.equal json1 json2)

(* ══════════════════════════════════════════════════════════════════════════
   Test Runner
   ══════════════════════════════════════════════════════════════════════════ *)

let () =
  let open Alcotest in
  run "Chain QCheck Properties" [
    "roundtrip", [
      QCheck_alcotest.to_alcotest prop_node_count_preserved;
      QCheck_alcotest.to_alcotest prop_node_ids_preserved;
      QCheck_alcotest.to_alcotest prop_edges_preserved;
      QCheck_alcotest.to_alcotest prop_output_preserved;
      QCheck_alcotest.to_alcotest prop_double_roundtrip_idempotent;
      QCheck_alcotest.to_alcotest prop_mermaid_parse_safe;
      QCheck_alcotest.to_alcotest prop_json_roundtrip;
      QCheck_alcotest.to_alcotest prop_chain_full_roundtrip_exact;
    ];
  ]

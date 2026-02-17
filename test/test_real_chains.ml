(** Real Chain Files Roundtrip Tests - Layer 5

    Tests roundtrip conversion for all chain files in data/chains/.
    Validates that JSON → Mermaid → JSON preserves Tool nodes correctly.
*)

open Alcotest
module Json_parser = Chain_parser
module Mermaid_parser = Chain_mermaid_parser
open Chain_types

(** Resolve repo root for test data *)
let repo_root =
  let rec find_root dir remaining =
    if remaining = 0 then dir
    else
      let candidate = Filename.concat dir "data/chains" in
      if Sys.file_exists candidate then dir
      else
        let parent = Filename.dirname dir in
        if parent = dir then dir else find_root parent (remaining - 1)
  in
  find_root (Sys.getcwd ()) 6

let repo_path rel = Filename.concat repo_root rel

let list_chain_files () =
  let dir = repo_path "data/chains" in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name ->
       Filename.check_suffix name ".json"
       && (try not (Sys.is_directory (Filename.concat dir name)) with _ -> true))
  |> List.sort String.compare

(** Read file contents *)
let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** Count nodes by type in a chain *)
let count_tool_nodes (chain : chain) : int =
  List.fold_left (fun acc (n : node) ->
    match n.node_type with
    | Tool _ -> acc + 1
    | _ -> acc
  ) 0 chain.nodes

(** Count LLM nodes in a chain *)
let count_llm_nodes (chain : chain) : int =
  List.fold_left (fun acc (n : node) ->
    match n.node_type with
    | Llm _ -> acc + 1
    | _ -> acc
  ) 0 chain.nodes

(** Find a node by ID *)
let find_node (chain : chain) (id : string) : node option =
  List.find_opt (fun (n : node) -> n.id = id) chain.nodes

(** Common roundtrip assertion *)
let assert_roundtrip path =
  let json_str = read_file (repo_path path) in
  let json = Yojson.Safe.from_string json_str in
  match Json_parser.parse_chain json with
  | Error e -> fail (Printf.sprintf "JSON parse failed for %s: %s" path e)
  | Ok chain1 ->
      let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
      (match Mermaid_parser.parse_mermaid_to_chain ~id:"roundtrip" mermaid with
       | Error e -> fail (Printf.sprintf "Mermaid parse failed for %s: %s\nMermaid:\n%s" path e mermaid)
       | Ok chain2 ->
           let json1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
           let json2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
           check string "json equality" json1 json2;
           (* Basic count checks *)
           check int "node count" (List.length chain1.nodes) (List.length chain2.nodes);
           check int "tool count" (count_tool_nodes chain1) (count_tool_nodes chain2);
           check int "llm count" (count_llm_nodes chain1) (count_llm_nodes chain2);
           check string "output node" chain1.output chain2.output)

(** 5.1 figma-to-web-component.json - KEY TEST (5+ nested Tool nodes) *)
let test_figma_to_web_component () =
  let path = "data/chains/figma-to-web-component.json" in
  assert_roundtrip path;
  (* Extra validation: verify Tool nodes have server prefix *)
  let json_str = read_file (repo_path path) in
  let json = Yojson.Safe.from_string json_str in
  let chain = Json_parser.parse_chain json |> Result.get_ok in
  (* Check parse-url node has figma: prefix *)
  (match find_node chain "parse-url" with
   | Some n ->
       (match n.node_type with
        | Tool { name; _ } ->
            check bool "parse-url has figma prefix" true
              (String.length name >= 6 && String.sub name 0 6 = "figma:")
        | _ -> fail "parse-url should be Tool node")
   | None -> fail "parse-url node not found")

(** 5.2 figma-to-prototype.json *)
let test_figma_to_prototype () =
  assert_roundtrip "data/chains/figma-to-prototype.json"

(** 5.3 code-migration.json *)
let test_code_migration () =
  assert_roundtrip "data/chains/code-migration.json"

(** 5.4 deep-research.json *)
let test_deep_research () =
  assert_roundtrip "data/chains/deep-research.json"

(** 5.5 design_to_tasks.json *)
let test_design_to_tasks () =
  assert_roundtrip "data/chains/design_to_tasks.json"

(** 5.6 incident-response.json *)
let test_incident_response () =
  assert_roundtrip "data/chains/incident-response.json"

(** 5.7 consensus-review.json *)
let test_consensus_review () =
  assert_roundtrip "data/chains/consensus-review.json"

(** 5.8 mcts-mantra-explore.json *)
let test_mcts_mantra_explore () =
  assert_roundtrip "data/chains/mcts-mantra-explore.json"

(** 5.9 mcts-mantra-hybrid.json *)
let test_mcts_mantra_hybrid () =
  assert_roundtrip "data/chains/mcts-mantra-hybrid.json"

(** 5.10 mcts-mantra-review.json *)
let test_mcts_mantra_review () =
  assert_roundtrip "data/chains/mcts-mantra-review.json"

(** 5.11 mermaid-to-chain.json *)
let test_mermaid_to_chain () =
  assert_roundtrip "data/chains/mermaid-to-chain.json"

(** 5.12 pr-review-pipeline.json *)
let test_pr_review_pipeline () =
  assert_roundtrip "data/chains/pr-review-pipeline.json"

(** 5.13 simple-test.json *)
let test_simple_test () =
  assert_roundtrip "data/chains/simple-test.json"

let test_all_chain_files_have_output () =
  let files = list_chain_files () in
  List.iter (fun name ->
    let path = repo_path ("data/chains/" ^ name) in
    let json = Yojson.Safe.from_string (read_file path) in
    let open Yojson.Safe.Util in
    match json |> member "output" with
    | `String _ -> ()
    | _ -> fail (Printf.sprintf "%s missing output field" name)
  ) files

let () =
  run "Real Chain Files" [
    "Sanity", [
      test_case "5.0 all chain files have output" `Quick test_all_chain_files_have_output;
    ];
    "Layer 5: Roundtrip", [
      test_case "5.1 figma-to-web-component" `Quick test_figma_to_web_component;
      test_case "5.2 figma-to-prototype" `Quick test_figma_to_prototype;
      test_case "5.3 code-migration" `Quick test_code_migration;
      test_case "5.4 deep-research" `Quick test_deep_research;
      test_case "5.5 design_to_tasks" `Quick test_design_to_tasks;
      test_case "5.6 incident-response" `Quick test_incident_response;
      test_case "5.7 consensus-review" `Quick test_consensus_review;
      test_case "5.8 mcts-mantra-explore" `Quick test_mcts_mantra_explore;
      test_case "5.9 mcts-mantra-hybrid" `Quick test_mcts_mantra_hybrid;
      test_case "5.10 mcts-mantra-review" `Quick test_mcts_mantra_review;
      test_case "5.11 mermaid-to-chain" `Quick test_mermaid_to_chain;
      test_case "5.12 pr-review-pipeline" `Quick test_pr_review_pipeline;
      test_case "5.13 simple-test" `Quick test_simple_test;
    ];
  ]

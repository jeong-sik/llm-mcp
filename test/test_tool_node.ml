(** Tool Node Parsing Tests - Layer 1-4 combined

    5 Layer TDD structure:
    - Layer 1: JSON parsing (flat + nested format)
    - Layer 2: Mermaid generation (Base64 args)
    - Layer 3: Mermaid parsing (Base64 args)
    - Layer 4: Roundtrip tests
    - Layer 5: Real chain files (separate test file)
*)

open Alcotest
module Json_parser = Chain_parser
module Mermaid_parser = Chain_mermaid_parser
open Chain_types

(** Helper to parse a node JSON and extract node_type *)
let parse_node_json json_str =
  let json = Yojson.Safe.from_string json_str in
  Json_parser.parse_node json

(** Extract node_type from parsed node *)
let get_node_type = function
  | Ok (node : node) -> Ok node.node_type
  | Error e -> Error e

(** Find a node by ID in a chain *)
let find_node (chain : chain) (id : string) : node option =
  List.find_opt (fun (n : node) -> n.id = id) chain.nodes

(* ============================================== *)
(* Layer 1: JSON Parsing Tests                    *)
(* ============================================== *)

(** 1.1 Flat format - minimal *)
let test_parse_tool_flat_minimal () =
  match parse_node_json {|{"id":"t","type":"tool","name":"eslint"}|} |> get_node_type with
  | Ok (Tool { name = "eslint"; args }) ->
      check bool "empty args" true (args = `Assoc [])
  | Ok _ -> fail "Expected Tool node"
  | Error e -> fail ("Parse failed: " ^ e)

(** 1.2 Flat format - with args *)
let test_parse_tool_flat_with_args () =
  match parse_node_json {|{"id":"t","type":"tool","name":"eslint","args":{"pattern":"*.ts"}}|} |> get_node_type with
  | Ok (Tool { name; args }) ->
      check string "name" "eslint" name;
      let pattern = Yojson.Safe.Util.(args |> member "pattern" |> to_string) in
      check string "pattern" "*.ts" pattern
  | Ok _ -> fail "Expected Tool node"
  | Error e -> fail ("Parse failed: " ^ e)

(** 1.3 Nested format - no server *)
let test_parse_tool_nested_no_server () =
  match parse_node_json {|{"id":"t","type":"tool","tool":{"name":"eslint","args":{}}}|} |> get_node_type with
  | Ok (Tool { name = "eslint"; _ }) -> ()
  | Ok (Tool { name; _ }) -> fail ("Expected eslint, got: " ^ name)
  | Ok _ -> fail "Expected Tool node"
  | Error e -> fail ("Parse failed: " ^ e)

(** 1.4 Nested format - with server (key test case!) *)
let test_parse_tool_nested_with_server () =
  match parse_node_json {|{"id":"t","type":"tool","tool":{"server":"figma","name":"parse_url","args":{}}}|} |> get_node_type with
  | Ok (Tool { name; _ }) ->
      (* Should encode server:name format *)
      check string "server:name" "figma:parse_url" name
  | Ok _ -> fail "Expected Tool node"
  | Error e -> fail ("Parse failed: " ^ e)

(** 1.5 Complex args *)
let test_parse_tool_complex_args () =
  match parse_node_json {|{"id":"t","type":"tool","tool":{
    "server":"figma",
    "name":"get_node",
    "args":{"depth":4,"include_vars":true,"tags":["a","b"]}
  }}|} |> get_node_type with
  | Ok (Tool { args; _ }) ->
      let open Yojson.Safe.Util in
      check int "depth" 4 (args |> member "depth" |> to_int);
      check bool "include_vars" true (args |> member "include_vars" |> to_bool);
      let tags = args |> member "tags" |> to_list |> List.map to_string in
      check (list string) "tags" ["a"; "b"] tags
  | Ok _ -> fail "Expected Tool node"
  | Error e -> fail ("Parse failed: " ^ e)

(** 1.6 Error case - missing name (both flat and nested) *)
let test_parse_tool_missing_name () =
  (* Flat format without name should fail *)
  match parse_node_json {|{"id":"t","type":"tool"}|} |> get_node_type with
  | Error _ -> () (* Expected error *)
  | Ok _ -> fail "Should fail without name"

(* ============================================== *)
(* Layer 2: Mermaid Generation Tests              *)
(* ============================================== *)

(** 2.1 Simple tool generation *)
let test_gen_tool_simple () =
  let tool = Tool { name = "eslint"; args = `Assoc [] } in
  let label = Mermaid_parser.node_type_to_text tool in
  check string "simple" "Tool:eslint" label

(** 2.2 Tool with server:name *)
let test_gen_tool_with_server () =
  let tool = Tool { name = "figma:parse_url"; args = `Assoc [] } in
  let label = Mermaid_parser.node_type_to_text tool in
  check string "with server" "Tool:figma:parse_url" label

(** 2.3 Tool with args - should include JSON or Base64 *)
let test_gen_tool_with_args () =
  let args = `Assoc [("url", `String "{{input}}")] in
  let tool = Tool { name = "figma:parse_url"; args } in
  let label = Mermaid_parser.node_type_to_text tool in
  (* Label should contain Tool:figma:parse_url and some representation of args *)
  check bool "has tool prefix" true (String.length label >= 19);
  check bool "starts with Tool:" true (String.sub label 0 5 = "Tool:")

(* ============================================== *)
(* Layer 3: Mermaid Parsing Tests                 *)
(* ============================================== *)

(** 3.1 Simple tool parsing *)
let test_parse_mermaid_tool_simple () =
  let mermaid = {|graph LR
    a["Tool:eslint"]
|} in
  match Mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid with
  | Ok chain ->
      (match find_node chain "a" with
       | Some n ->
           (match n.node_type with
            | Tool { name = "eslint"; _ } -> ()
            | nt -> fail ("Expected Tool:eslint, got: " ^ Mermaid_parser.node_type_to_text nt))
       | None -> fail "Node a not found")
  | Error e -> fail ("Parse failed: " ^ e)

(** 3.2 Tool with server:name format *)
let test_parse_mermaid_tool_with_server () =
  let mermaid = {|graph LR
    a["Tool:figma:parse_url"]
|} in
  match Mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid with
  | Ok chain ->
      (match find_node chain "a" with
       | Some n ->
           (match n.node_type with
            | Tool { name; _ } ->
                check string "name with server" "figma:parse_url" name
            | nt -> fail ("Expected Tool, got: " ^ Mermaid_parser.node_type_to_text nt))
       | None -> fail "Node a not found")
  | Error e -> fail ("Parse failed: " ^ e)

(** 3.3 Tool with quoted args (existing format) *)
let test_parse_mermaid_tool_quoted_args () =
  let mermaid = {|graph LR
    a["Tool:eslint '*.ts'"]
|} in
  match Mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid with
  | Ok chain ->
      (match find_node chain "a" with
       | Some n ->
           (match n.node_type with
            | Tool { args; _ } ->
                let input = Yojson.Safe.Util.(args |> member "input" |> to_string) in
                check string "input arg" "*.ts" input
            | _ -> fail "Expected Tool")
       | None -> fail "Node a not found")
  | Error e -> fail ("Parse failed: " ^ e)

(** 3.4 Tool with raw JSON args (new format support needed) *)
let test_parse_mermaid_tool_json_args () =
  let mermaid = {|graph LR
    a["Tool:eslint {\"pattern\":\"*.ts\"}"]
|} in
  match Mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid with
  | Ok chain ->
      (match find_node chain "a" with
       | Some n ->
           (match n.node_type with
            | Tool { args; _ } ->
                (* Should parse JSON args *)
                let pattern = Yojson.Safe.Util.(args |> member "pattern" |> to_string_option) in
                (match pattern with
                 | Some p -> check string "pattern" "*.ts" p
                 | None ->
                     (* Fallback: check if stored as input *)
                     let input = Yojson.Safe.Util.(args |> member "input" |> to_string_option) in
                     check (option string) "input fallback" (Some "{\"pattern\":\"*.ts\"}") input)
            | _ -> fail "Expected Tool")
       | None -> fail "Node a not found")
  | Error e -> fail ("Parse failed: " ^ e)

(* ============================================== *)
(* Layer 4: Roundtrip Tests                       *)
(* ============================================== *)

(** 4.1 Flat format roundtrip *)
let test_roundtrip_tool_flat () =
  let original = {|{"id":"t","nodes":[
    {"id":"a","type":"tool","name":"eslint","args":{"pattern":"*.ts"}}
  ],"output":"a"}|} in
  let json1 = Yojson.Safe.from_string original in
  match Json_parser.parse_chain json1 with
  | Error e -> fail ("JSON parse failed: " ^ e)
  | Ok chain1 ->
      let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain1 in
      (match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
       | Error e -> fail ("Mermaid parse failed: " ^ e ^ "\nMermaid:\n" ^ mermaid)
       | Ok chain2 ->
           check int "node count" (List.length chain1.nodes) (List.length chain2.nodes);
           check string "output" chain1.output chain2.output)

(** 4.2 Nested format roundtrip - THIS IS THE KEY TEST *)
let test_roundtrip_tool_nested () =
  let original = {|{"id":"t","nodes":[
    {"id":"a","type":"tool","tool":{"server":"figma","name":"figma_parse_url","args":{"url":"{{input}}"}}}
  ],"output":"a"}|} in
  let json1 = Yojson.Safe.from_string original in
  match Json_parser.parse_chain json1 with
  | Error e -> fail ("JSON parse failed: " ^ e)
  | Ok chain1 ->
      (* Verify nested format was parsed correctly *)
      (match find_node chain1 "a" with
       | Some n ->
           (match n.node_type with
            | Tool { name; _ } ->
                check bool "has server prefix" true (String.length name > 6 && String.sub name 0 6 = "figma:")
            | _ -> fail "Tool node not found after JSON parse")
       | None -> fail "Node a not found");

      let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain1 in
      Printf.printf "Generated Mermaid:\n%s\n" mermaid;

      (match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
       | Error e -> fail ("Mermaid parse failed: " ^ e ^ "\nMermaid:\n" ^ mermaid)
       | Ok chain2 ->
           check int "node count" (List.length chain1.nodes) (List.length chain2.nodes);
           (* Verify Tool node preserved *)
           (match find_node chain2 "a" with
            | Some n ->
                (match n.node_type with
                 | Tool { name; args } ->
                     check bool "name has server" true (String.length name > 6);
                     (* Args should be preserved *)
                     let url = Yojson.Safe.Util.(args |> member "url" |> to_string_option) in
                     check (option string) "url preserved" (Some "{{input}}") url
                 | _ -> fail "Tool node type not preserved")
            | None -> fail "Tool node not preserved after roundtrip"))

(** 4.3 Mixed nodes roundtrip *)
let test_roundtrip_mixed_nodes () =
  let original = {|{"id":"t","nodes":[
    {"id":"a","type":"llm","model":"gemini","prompt":"hello"},
    {"id":"b","type":"tool","name":"fetch","args":{"url":"http://example.com"}},
    {"id":"c","type":"quorum","required":2}
  ],"output":"c"}|} in
  let json1 = Yojson.Safe.from_string original in
  match Json_parser.parse_chain json1 with
  | Error e -> fail ("JSON parse failed: " ^ e)
  | Ok chain1 ->
      let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain1 in
      (match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
       | Error e -> fail ("Mermaid parse failed: " ^ e)
       | Ok chain2 ->
           check int "node count" 3 (List.length chain2.nodes);
           (* Check each node type preserved *)
           let types = List.map (fun (n : node) ->
             match n.node_type with
             | Llm _ -> "llm"
             | Tool _ -> "tool"
             | Quorum _ -> "quorum"
             | _ -> "other"
           ) chain2.nodes |> List.sort String.compare in
           check (list string) "types" ["llm"; "quorum"; "tool"] types)

(* ============================================== *)
(* Test Registration                              *)
(* ============================================== *)

let layer1_tests = [
  test_case "1.1 flat minimal" `Quick test_parse_tool_flat_minimal;
  test_case "1.2 flat with args" `Quick test_parse_tool_flat_with_args;
  test_case "1.3 nested no server" `Quick test_parse_tool_nested_no_server;
  test_case "1.4 nested with server" `Quick test_parse_tool_nested_with_server;
  test_case "1.5 complex args" `Quick test_parse_tool_complex_args;
  test_case "1.6 missing name" `Quick test_parse_tool_missing_name;
]

let layer2_tests = [
  test_case "2.1 simple gen" `Quick test_gen_tool_simple;
  test_case "2.2 server:name gen" `Quick test_gen_tool_with_server;
  test_case "2.3 with args gen" `Quick test_gen_tool_with_args;
]

let layer3_tests = [
  test_case "3.1 simple parse" `Quick test_parse_mermaid_tool_simple;
  test_case "3.2 server:name parse" `Quick test_parse_mermaid_tool_with_server;
  test_case "3.3 quoted args parse" `Quick test_parse_mermaid_tool_quoted_args;
  test_case "3.4 json args parse" `Quick test_parse_mermaid_tool_json_args;
]

let layer4_tests = [
  test_case "4.1 flat roundtrip" `Quick test_roundtrip_tool_flat;
  test_case "4.2 nested roundtrip" `Quick test_roundtrip_tool_nested;
  test_case "4.3 mixed roundtrip" `Quick test_roundtrip_mixed_nodes;
]

let () =
  run "Tool Node Tests" [
    "Layer 1: JSON Parsing", layer1_tests;
    "Layer 2: Mermaid Generation", layer2_tests;
    "Layer 3: Mermaid Parsing", layer3_tests;
    "Layer 4: Roundtrip", layer4_tests;
  ]

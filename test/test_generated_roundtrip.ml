(* Generated Chain Roundtrip Tests

   Property-style tests with randomized small chains to ensure
   JSON → Mermaid → JSON is identical when lossless metadata is present.
*)

open Alcotest
module Json_parser = Chain_parser
module Mermaid_parser = Chain_mermaid_parser

let gen_chain idx : Yojson.Safe.t =
  let node_count = 2 + Random.int 6 in
  let nodes = ref [] in
  for i = 0 to node_count - 1 do
    let id = Printf.sprintf "n%d" i in
    let base =
      if Random.bool () then
        `Assoc [
          ("id", `String id);
          ("type", `String "llm");
          ("model", `String "stub");
          ("prompt", `String (Printf.sprintf "P%d" i));
        ]
      else
        `Assoc [
          ("id", `String id);
          ("type", `String "tool");
          ("name", `String "echo");
          ("args", `Assoc []);
        ]
    in
    let inputs =
      if i = 0 then []
      else
        let max_inputs = min 3 i in
        let count = Random.int (max_inputs + 1) in
        let rec pick acc k =
          if k = 0 then acc
          else
            let src = Random.int i in
            let key = Printf.sprintf "in%d" k in
            pick ((key, Printf.sprintf "n%d" src) :: acc) (k - 1)
        in
        pick [] count
    in
    let node_json =
      if inputs = [] then base
      else
        let input_list =
          `List (List.map (fun (k, v) -> `List [`String k; `String v]) inputs)
        in
        match base with
        | `Assoc fields -> `Assoc (("input_mapping", input_list) :: fields)
        | other -> other
    in
    nodes := node_json :: !nodes
  done;
  `Assoc [
    ("id", `String (Printf.sprintf "gen_%d" idx));
    ("nodes", `List (List.rev !nodes));
    ("output", `String (Printf.sprintf "n%d" (node_count - 1)));
    ("config", `Assoc [
      ("timeout", `Int 30);
      ("trace", `Bool false);
      ("max_depth", `Int 3);
    ]);
  ]

let test_generated_roundtrip () =
  Random.init 42;
  for i = 0 to 49 do
    let json = gen_chain i in
    match Json_parser.parse_chain json with
    | Error e -> fail (Printf.sprintf "JSON parse failed (gen_%d): %s" i e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain mermaid with
        | Error e -> fail (Printf.sprintf "Mermaid parse failed (gen_%d): %s" i e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            check string (Printf.sprintf "generated_%d" i) j1 j2
  done

let () =
  run "Generated Chain Roundtrip" [
    "generated", [
      test_case "random small chains (50)" `Quick test_generated_roundtrip;
    ];
  ]

(* Lossless Roundtrip Test: JSON → Mermaid (lossless) → Chain → verify equality *)
open Chain_types
module Json_parser = Chain_parser
module Mermaid_parser = Chain_mermaid_parser

let original_json = {|{
  "id": "lossless_test",
  "nodes": [
    {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello"},
    {"id": "b", "type": "llm", "model": "claude", "prompt": "Process: {{input}}", "input_mapping": [["input", "a"], ["extra", "a"]]}
  ],
  "output": "b",
  "config": {
    "timeout": 120,
    "trace": true,
    "max_depth": 5
  }
}|}

let test_lossless_roundtrip () =
  Alcotest.(check bool) "JSON parse" true (
    let json1 = Yojson.Safe.from_string original_json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        (* Convert to Mermaid with lossless mode *)
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain1 in

        (* Parse back with metadata *)
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            (* Verify all fields *)
            let id_ok = chain1.id = chain2.id in
            let output_ok = chain1.output = chain2.output in
            let timeout_ok = chain1.config.timeout = chain2.config.timeout in
            let trace_ok = chain1.config.trace = chain2.config.trace in
            let depth_ok = chain1.config.max_depth = chain2.config.max_depth in

            (* Verify node b's input_mapping *)
            let find_b nodes = List.find_opt (fun (n:node) -> n.id = "b") nodes in
            let mapping_ok = match find_b chain1.nodes, find_b chain2.nodes with
              | Some n1, Some n2 ->
                  let m1 = List.sort compare n1.input_mapping in
                  let m2 = List.sort compare n2.input_mapping in
                  m1 = m2
              | _ -> false
            in

            if not id_ok then Printf.printf "FAIL: id mismatch: %s vs %s\n" chain1.id chain2.id;
            if not output_ok then Printf.printf "FAIL: output mismatch: %s vs %s\n" chain1.output chain2.output;
            if not timeout_ok then Printf.printf "FAIL: timeout mismatch: %d vs %d\n" chain1.config.timeout chain2.config.timeout;
            if not trace_ok then Printf.printf "FAIL: trace mismatch: %b vs %b\n" chain1.config.trace chain2.config.trace;
            if not depth_ok then Printf.printf "FAIL: max_depth mismatch: %d vs %d\n" chain1.config.max_depth chain2.config.max_depth;
            if not mapping_ok then Printf.printf "FAIL: input_mapping mismatch\n";

            id_ok && output_ok && timeout_ok && trace_ok && depth_ok && mapping_ok
  )

let test_metadata_parsing () =
  (* Test that metadata comments are correctly parsed *)
  let mermaid_with_meta = {|graph LR
    %% @chain {"id":"my_chain","output":"result","timeout":300,"trace":false,"max_depth":3}
    %% @node:b {"input_mapping":[["key1","a"],["key2","a"]]}
    a["LLM:gemini 'Hello'"]
    b["LLM:claude 'World'"]
    a --> b
|} in
  Alcotest.(check bool) "metadata parsing" true (
    match Mermaid_parser.parse_mermaid_to_chain ~id:"default" mermaid_with_meta with
    | Error e -> failwith ("Parse failed: " ^ e)
    | Ok chain ->
        chain.id = "my_chain" &&
        chain.output = "result" &&
        chain.config.timeout = 300 &&
        chain.config.trace = false &&
        chain.config.max_depth = 3 &&
        (match List.find_opt (fun (n:node) -> n.id = "b") chain.nodes with
         | Some n ->
             let mapping = List.sort compare n.input_mapping in
             mapping = [("key1", "a"); ("key2", "a")]
         | None -> false)
  )

(* Edge case: special characters in prompts *)
let test_special_chars_roundtrip () =
  let json = {|{
    "id": "special_chars",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello \"world\" with 'quotes' and \\backslash"},
      {"id": "b", "type": "llm", "model": "claude", "prompt": "Process: {{a}}", "input_mapping": [["a", "a"]]}
    ],
    "output": "b",
    "config": {"timeout": 60, "trace": false, "max_depth": 2}
  }|} in
  Alcotest.(check bool) "special chars" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            chain1.id = chain2.id &&
            chain1.config.timeout = chain2.config.timeout
  )

(* Edge case: empty input_mapping should not emit metadata *)
let test_empty_mapping () =
  let json = {|{
    "id": "empty_mapping",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello"}
    ],
    "output": "a",
    "config": {"timeout": 30, "trace": true, "max_depth": 1}
  }|} in
  Alcotest.(check bool) "empty mapping" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain1 in
        (* Should only have @chain comment, no @node comments *)
        let has_chain_meta = String.sub mermaid 0 100 |> fun s ->
          try let _ = Str.search_forward (Str.regexp "@chain") s 0 in true
          with Not_found -> false
        in
        let has_node_meta =
          try let _ = Str.search_forward (Str.regexp "@node:") mermaid 0 in true
          with Not_found -> false
        in
        has_chain_meta && not has_node_meta
  )

(* Edge case: complex multi-node chain with various mappings *)
let test_complex_chain () =
  let json = {|{
    "id": "complex_chain",
    "nodes": [
      {"id": "start", "type": "llm", "model": "gemini", "prompt": "Start"},
      {"id": "mid1", "type": "llm", "model": "claude", "prompt": "Mid1: {{x}}", "input_mapping": [["x", "start"]]},
      {"id": "mid2", "type": "llm", "model": "codex", "prompt": "Mid2: {{y}}", "input_mapping": [["y", "start"]]},
      {"id": "end", "type": "llm", "model": "gemini", "prompt": "End: {{a}} {{b}}", "input_mapping": [["a", "mid1"], ["b", "mid2"]]}
    ],
    "output": "end",
    "config": {"timeout": 180, "trace": true, "max_depth": 4}
  }|} in
  Alcotest.(check bool) "complex chain" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:true chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            (* Verify all config *)
            let config_ok =
              chain1.id = chain2.id &&
              chain1.output = chain2.output &&
              chain1.config.timeout = chain2.config.timeout &&
              chain1.config.trace = chain2.config.trace &&
              chain1.config.max_depth = chain2.config.max_depth
            in
            (* Verify end node's input_mapping *)
            let find_end nodes = List.find_opt (fun (n:node) -> n.id = "end") nodes in
            let mapping_ok = match find_end chain1.nodes, find_end chain2.nodes with
              | Some n1, Some n2 ->
                  let m1 = List.sort compare n1.input_mapping in
                  let m2 = List.sort compare n2.input_mapping in
                  m1 = m2
              | _ -> false
            in
            if not config_ok then Printf.printf "FAIL: config mismatch\n";
            if not mapping_ok then Printf.printf "FAIL: end node mapping mismatch\n";
            config_ok && mapping_ok
  )

(* Edge case: without lossless flag, metadata should be lost *)
let test_lossy_mode () =
  let json = {|{
    "id": "lossy_test",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello"},
      {"id": "b", "type": "llm", "model": "claude", "prompt": "{{input}}", "input_mapping": [["input", "a"], ["extra", "a"]]}
    ],
    "output": "b",
    "config": {"timeout": 999, "trace": true, "max_depth": 9}
  }|} in
  Alcotest.(check bool) "lossy mode loses metadata" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        (* Without lossless=true, no metadata comments *)
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false ~lossless:false chain1 in
        let has_meta =
          try let _ = Str.search_forward (Str.regexp "@chain") mermaid 0 in true
          with Not_found -> false
        in
        not has_meta  (* Should NOT have metadata *)
  )

(* Edge case: fallback id is used when no metadata *)
let test_fallback_id () =
  let mermaid_no_meta = {|graph LR
    a["LLM:gemini 'Hello'"]
    b["LLM:claude 'World'"]
    a --> b
|} in
  Alcotest.(check bool) "fallback id" true (
    match Mermaid_parser.parse_mermaid_to_chain ~id:"my_fallback_id" mermaid_no_meta with
    | Error e -> failwith ("Parse failed: " ^ e)
    | Ok chain ->
        (* Should use fallback id since no @chain metadata *)
        chain.id = "my_fallback_id"
  )

(* Edge case: partial metadata (only some fields) *)
let test_partial_metadata () =
  let mermaid_partial = {|graph LR
    %% @chain {"id":"partial_test","timeout":500}
    a["LLM:gemini 'Hello'"]
|} in
  Alcotest.(check bool) "partial metadata" true (
    match Mermaid_parser.parse_mermaid_to_chain ~id:"default" mermaid_partial with
    | Error e -> failwith ("Parse failed: " ^ e)
    | Ok chain ->
        (* id and timeout from metadata, others from defaults *)
        chain.id = "partial_test" &&
        chain.config.timeout = 500 &&
        chain.config.trace = Chain_types.default_config.trace &&  (* default *)
        chain.config.max_depth = Chain_types.default_config.max_depth  (* default *)
  )

let () =
  Alcotest.run "Lossless Roundtrip" [
    "roundtrip", [
      Alcotest.test_case "JSON → Mermaid → Chain equality" `Quick test_lossless_roundtrip;
      Alcotest.test_case "Metadata comment parsing" `Quick test_metadata_parsing;
    ];
    "edge_cases", [
      Alcotest.test_case "Special characters in prompts" `Quick test_special_chars_roundtrip;
      Alcotest.test_case "Empty input_mapping" `Quick test_empty_mapping;
      Alcotest.test_case "Complex multi-node chain" `Quick test_complex_chain;
      Alcotest.test_case "Lossy mode (no metadata)" `Quick test_lossy_mode;
      Alcotest.test_case "Fallback ID when no metadata" `Quick test_fallback_id;
      Alcotest.test_case "Partial metadata" `Quick test_partial_metadata;
    ];
  ]

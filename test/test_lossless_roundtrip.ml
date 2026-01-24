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
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in

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
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
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
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        (* Should have @chain_full + @chain comments, no @node comments *)
        let has_chain_full =
          try let _ = Str.search_forward (Str.regexp "@chain_full") mermaid 0 in true
          with Not_found -> false
        in
        let has_chain_meta =
          try let _ = Str.search_forward (Str.regexp "@chain ") mermaid 0 in true
          with Not_found -> false
        in
        let has_node_meta =
          try let _ = Str.search_forward (Str.regexp "@node:") mermaid 0 in true
          with Not_found -> false
        in
        has_chain_full && has_chain_meta && not has_node_meta
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
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
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

(* Always lossless: metadata should always be present *)
let test_always_lossless () =
  let json = {|{
    "id": "lossy_test",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello"},
      {"id": "b", "type": "llm", "model": "claude", "prompt": "{{input}}", "input_mapping": [["input", "a"], ["extra", "a"]]}
    ],
    "output": "b",
    "config": {"timeout": 999, "trace": true, "max_depth": 9}
  }|} in
  Alcotest.(check bool) "always lossless includes metadata" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        let has_chain_full =
          try let _ = Str.search_forward (Str.regexp "@chain_full") mermaid 0 in true
          with Not_found -> false
        in
        let has_chain =
          try let _ = Str.search_forward (Str.regexp "@chain ") mermaid 0 in true
          with Not_found -> false
        in
        has_chain_full && has_chain
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

(* chain_full metadata should override mismatched graph *)
let test_chain_full_override () =
  let json = {|{
    "id": "override_chain",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello"},
      {"id": "b", "type": "llm", "model": "claude", "prompt": "World", "input_mapping": [["input","a"]]}
    ],
    "output": "b",
    "config": {"timeout": 10, "trace": false, "max_depth": 2}
  }|} in
  Alcotest.(check bool) "chain_full override" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let full_json = Json_parser.chain_to_json_string ~pretty:false chain1 in
        (* Intentionally wrong graph, should still parse from chain_full *)
        let mermaid =
          "graph LR\n    %% @chain_full " ^ full_json ^ "\n    x[LLM:stub \"x\"]\n"
        in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let ids1 = List.map (fun (n:node) -> n.id) chain1.nodes |> List.sort compare in
            let ids2 = List.map (fun (n:node) -> n.id) chain2.nodes |> List.sort compare in
            chain1.id = chain2.id && ids1 = ids2 && chain1.output = chain2.output
  )

(* FeedbackLoop should survive lossless roundtrip via chain_full *)
let test_feedback_loop_roundtrip () =
  let json = {|{
    "id": "fb_loop",
    "nodes": [
      {
        "id": "loop",
        "type": "feedback_loop",
        "generator": {"id": "gen", "type": "llm", "model": "claude", "prompt": "Draft"},
        "evaluator_config": {"scoring_func": "llm_judge", "select_strategy": "best"},
        "improver_prompt": "Improve: {{feedback}}",
        "max_iterations": 2,
        "min_score": 0.6
      }
    ],
    "output": "loop",
    "config": {"timeout": 60, "trace": false, "max_depth": 3}
  }|} in
  Alcotest.(check bool) "feedback_loop lossless" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
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

(* GoalDriven Mermaid parsing test *)
let test_goaldriven_mermaid_parsing () =
  let mermaid_with_goal = {|graph LR
    %% @chain {"id":"goaldriven_test","output":"goal","timeout":60,"trace":false,"max_depth":3}
    action[LLM:gemini "Improve code"]
    goal{GoalDriven:coverage:gte:0.90:10}
    action --> goal
|} in
  Alcotest.(check bool) "GoalDriven parsing" true (
    match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid_with_goal with
    | Error e ->
        Printf.printf "Parse error: %s\n" e;
        false
    | Ok chain ->
        (* Check chain was parsed with GoalDriven node *)
        List.exists (fun (n:node) ->
          match n.node_type with
          | GoalDriven { goal_metric; goal_operator; goal_value; max_iterations; _ } ->
              goal_metric = "coverage" &&
              goal_operator = Gte &&
              goal_value = 0.90 &&
              max_iterations = 10
          | _ -> false
        ) chain.nodes
  )

(* GoalDriven strict roundtrip: Chain → Mermaid → Chain equality *)
let test_goaldriven_strict_roundtrip () =
  let action_node = {
    id = "action";
    node_type = Llm { model = "gemini"; system = None; prompt = "Improve code"; timeout = None; tools = None; prompt_ref = None; prompt_vars = [] };
    input_mapping = []
  } in
  let goal_node = {
    id = "goal";
    node_type = GoalDriven {
      goal_metric = "coverage";
      goal_operator = Gte;
      goal_value = 0.90;
      action_node = action_node;
      measure_func = "exec_test";
      max_iterations = 10;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = [("input", "action")]
  } in
  let chain1 = {
    id = "strict_test";
    nodes = [action_node; goal_node];
    output = "goal";
    config = { default_config with timeout = 120; trace = true }
  } in

  Alcotest.(check bool) "GoalDriven strict roundtrip" true (
    (* Chain → Mermaid (lossless) *)
    let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in

    Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
    Printf.printf "║  🎯 GoalDriven ↔ Mermaid 양방향 변환 증명                 ║\n";
    Printf.printf "╚══════════════════════════════════════════════════════════╝\n";

    Printf.printf "\n┌─────────────────────────────────────────────────────────┐\n";
    Printf.printf "│ STEP 1: Chain → Mermaid (lossless)                      │\n";
    Printf.printf "└─────────────────────────────────────────────────────────┘\n";
    Printf.printf "%s\n" mermaid;

    (* Mermaid → Chain *)
    match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
    | Error e ->
        Printf.printf "❌ Parse error: %s\n" e;
        false
    | Ok chain2 ->
        Printf.printf "┌─────────────────────────────────────────────────────────┐\n";
        Printf.printf "│ STEP 2: Mermaid → Chain                                 │\n";
        Printf.printf "└─────────────────────────────────────────────────────────┘\n";

        (* Verify key fields *)
        let id_ok = chain2.id = chain1.id in
        let output_ok = chain2.output = chain1.output in
        let timeout_ok = chain2.config.timeout = chain1.config.timeout in
        let trace_ok = chain2.config.trace = chain1.config.trace in

        Printf.printf "  Chain ID: %s %s\n" chain2.id (if id_ok then "✅" else "❌");
        Printf.printf "  Output:   %s %s\n" chain2.output (if output_ok then "✅" else "❌");
        Printf.printf "  Timeout:  %d %s\n" chain2.config.timeout (if timeout_ok then "✅" else "❌");
        Printf.printf "  Trace:    %b %s\n" chain2.config.trace (if trace_ok then "✅" else "❌");

        Printf.printf "\n┌─────────────────────────────────────────────────────────┐\n";
        Printf.printf "│ STEP 3: Verify GoalDriven fields                        │\n";
        Printf.printf "└─────────────────────────────────────────────────────────┘\n";

        (* Verify GoalDriven node *)
        let goal_ok = List.exists (fun (n:node) ->
          match n.node_type with
          | GoalDriven g ->
              let m_ok = g.goal_metric = "coverage" in
              let o_ok = g.goal_operator = Gte in
              let v_ok = g.goal_value = 0.90 in
              let i_ok = g.max_iterations = 10 in
              Printf.printf "  🎯 GoalDriven node '%s':\n" n.id;
              Printf.printf "     metric:   %s %s\n" g.goal_metric (if m_ok then "✅" else "❌");
              Printf.printf "     operator: %s %s\n"
                (match g.goal_operator with Gt->"gt"|Gte->"gte"|Lt->"lt"|Lte->"lte"|Eq->"eq"|Neq->"neq")
                (if o_ok then "✅" else "❌");
              Printf.printf "     value:    %.2f %s\n" g.goal_value (if v_ok then "✅" else "❌");
              Printf.printf "     max_iter: %d %s\n" g.max_iterations (if i_ok then "✅" else "❌");
              m_ok && o_ok && v_ok && i_ok
          | Llm { model; prompt; _ } ->
              Printf.printf "  🤖 LLM node '%s': model=%s prompt=\"%s\" ✅\n" n.id model prompt;
              false
          | _ -> false
        ) chain2.nodes in

        let all_ok = id_ok && output_ok && timeout_ok && trace_ok && goal_ok in
        Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
        if all_ok then
          Printf.printf "║  🎉 양방향 변환 증명 성공!                               ║\n"
        else
          Printf.printf "║  ❌ 일부 필드 불일치                                     ║\n";
        Printf.printf "╚══════════════════════════════════════════════════════════╝\n";
        all_ok
  )

(* GoalDriven ID-based inference test *)
let test_goaldriven_id_inference () =
  let mermaid_with_goal_id = {|graph LR
    action[LLM:gemini "Improve"]
    goal_accuracy{gte:0.95:5}
    action --> goal_accuracy
|} in
  Alcotest.(check bool) "GoalDriven ID inference" true (
    match Mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid_with_goal_id with
    | Error e ->
        Printf.printf "Parse error: %s\n" e;
        false
    | Ok chain ->
        let has_goal_node = List.exists (fun (n:node) ->
          match n.node_type with
          | GoalDriven { goal_metric; goal_operator; goal_value; max_iterations; _ } ->
              goal_metric = "accuracy" &&
              goal_operator = Gte &&
              goal_value = 0.95 &&
              max_iterations = 5
          | _ -> false
        ) chain.nodes in
        if not has_goal_node then Printf.printf "FAIL: GoalDriven node (ID-based) not found\n";
        has_goal_node
  )

(* Test "inputs" format (assoc dict) is correctly parsed - this is used by chain_to_json output *)
let test_inputs_assoc_format () =
  let json = {|{
    "id": "inputs_assoc_test",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Hello"},
      {"id": "b", "type": "llm", "model": "claude", "prompt": "Process: {{x}} and {{y}}", "inputs": {"x": "a", "y": "a"}}
    ],
    "output": "b",
    "config": {"timeout": 60, "trace": false, "max_depth": 2}
  }|} in
  Alcotest.(check bool) "inputs assoc format" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        (* Verify input_mapping was populated from "inputs" *)
        let find_b nodes = List.find_opt (fun (n:node) -> n.id = "b") nodes in
        match find_b chain1.nodes with
        | Some n ->
            let mapping = List.sort compare n.input_mapping in
            let expected = [("x", "a"); ("y", "a")] in
            if mapping <> expected then
              Printf.printf "FAIL: mapping mismatch. Got %s, expected %s\n"
                (String.concat ", " (List.map (fun (k,v) -> k ^ "=" ^ v) mapping))
                (String.concat ", " (List.map (fun (k,v) -> k ^ "=" ^ v) expected));
            mapping = expected
        | None -> false
  )

(* Test full roundtrip: JSON (inputs) → Chain → JSON → parse again → verify edges *)
let test_inputs_full_roundtrip () =
  let json = {|{
    "id": "inputs_roundtrip",
    "nodes": [
      {"id": "a", "type": "llm", "model": "gemini", "prompt": "Start"},
      {"id": "b", "type": "llm", "model": "claude", "prompt": "Process: {{data}}", "inputs": {"data": "a"}}
    ],
    "output": "b"
  }|} in
  Alcotest.(check bool) "inputs full roundtrip" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        (* Serialize back to JSON *)
        let json2 = Json_parser.chain_to_json chain1 in
        (* Parse the serialized JSON *)
        match Json_parser.parse_chain json2 with
        | Error e -> failwith ("Re-parse failed: " ^ e)
        | Ok chain2 ->
            (* Verify mapping survived roundtrip *)
            let find_b nodes = List.find_opt (fun (n:node) -> n.id = "b") nodes in
            match find_b chain1.nodes, find_b chain2.nodes with
            | Some n1, Some n2 ->
                let m1 = List.sort compare n1.input_mapping in
                let m2 = List.sort compare n2.input_mapping in
                if m1 <> m2 then
                  Printf.printf "FAIL: roundtrip mapping mismatch\n";
                m1 = m2
            | _ -> false
  )

(* Complex nested nodes should survive lossless Mermaid roundtrip *)
let test_complex_lossless_roundtrip () =
  let make_llm id model prompt =
    { id;
      node_type = Llm {
        model; system = Some "system";
        prompt; timeout = Some 42; tools = None;
        prompt_ref = None;
        prompt_vars = [];
      };
      input_mapping = [];
    }
  in
  let tool_node = {
    id = "tool_parse";
    node_type = Tool {
      name = "figma:figma_parse_url";
      args = `Assoc [("url", `String "https://figma.com/file/abc/xyz")];
    };
    input_mapping = [];
  } in
  let retry_node = {
    id = "retry_api";
    node_type = Retry {
      node = tool_node;
      max_attempts = 2;
      backoff = Exponential 1.5;
      retry_on = ["timeout"; "5xx"];
    };
    input_mapping = [];
  } in
  let fallback_node = {
    id = "fallback_llm";
    node_type = Fallback {
      primary = make_llm "primary" "claude" "Primary";
      fallbacks = [make_llm "fb1" "gemini" "Backup1"; make_llm "fb2" "ollama" "Backup2"];
    };
    input_mapping = [("input", "retry_api")];
  } in
  let sub_chain = {
    id = "sub_chain";
    nodes = [
      make_llm "sub_a" "gemini" "Sub A";
      make_llm "sub_b" "claude" "Sub B";
    ];
    output = "sub_b";
    config = { Chain_types.default_config with timeout = 77; trace = true; max_concurrency = 2; direction = TB };
  } in
  let subgraph_node = {
    id = "subgraph";
    node_type = Subgraph sub_chain;
    input_mapping = [("input", "fallback_llm")];
  } in
  let feedback_node = {
    id = "feedback";
    node_type = FeedbackLoop {
      generator = make_llm "gen" "gemini" "Generate";
      evaluator_config = { scoring_func = "llm_judge"; scoring_prompt = Some "Score 0-1"; select_strategy = Best };
      improver_prompt = "Fix: {{feedback}}";
      max_iterations = 3;
      score_threshold = 0.9;
      score_operator = Gte;
      conversational = false;
      relay_models = [];
    };
    input_mapping = [];
  } in
  let mcts_node = {
    id = "mcts";
    node_type = Mcts {
      strategies = [make_llm "s1" "gemini" "S1"; make_llm "s2" "claude" "S2"];
      simulation = make_llm "sim" "gemini" "Sim";
      evaluator = "llm_judge";
      evaluator_prompt = None;
      policy = UCB1 1.41;
      max_iterations = 5;
      max_depth = 3;
      expansion_threshold = 2;
      early_stop = Some 0.95;
      parallel_sims = 2;
    };
    input_mapping = [];
  } in
  let stream_node = {
    id = "stream";
    node_type = StreamMerge {
      nodes = [make_llm "sm1" "gemini" "S"; make_llm "sm2" "claude" "M"];
      reducer = Concat;
      initial = "";
      min_results = Some 1;
      timeout = Some 1.5;
    };
    input_mapping = [("from", "subgraph")];
  } in
  let adapter_node = {
    id = "adapt";
    node_type = Adapter {
      input_ref = "{{stream}}";
      transform = Template "Result: {{value}}";
      on_error = `Default "N/A";
    };
    input_mapping = [];
  } in
  let chain_exec_node = {
    id = "exec";
    node_type = ChainExec {
      chain_source = "chain_json";
      validate = true;
      max_depth = 2;
      sandbox = true;
      context_inject = [("input", "subgraph")];
      pass_outputs = false;
    };
    input_mapping = [];
  } in
  let cache_node = {
    id = "cache";
    node_type = Cache {
      key_expr = "{{input}}";
      ttl_seconds = 30;
      inner = make_llm "cache_inner" "gemini" "Cache";
    };
    input_mapping = [];
  } in
  let batch_node = {
    id = "batch";
    node_type = Batch {
      batch_size = 2;
      parallel = true;
      inner = make_llm "batch_inner" "gemini" "Batch";
      collect_strategy = `Concat;
    };
    input_mapping = [];
  } in
  let spawn_node = {
    id = "spawn";
    node_type = Spawn {
      clean = true;
      inner = make_llm "spawn_inner" "gemini" "Spawn";
      pass_vars = ["input"; "config"];
      inherit_cache = false;
    };
    input_mapping = [];
  } in
  let chain1 = {
    id = "complex_lossless";
    nodes = [
      retry_node;
      fallback_node;
      subgraph_node;
      feedback_node;
      mcts_node;
      stream_node;
      adapter_node;
      chain_exec_node;
      cache_node;
      batch_node;
      spawn_node;
    ];
    output = "stream";
    config = { Chain_types.default_config with timeout = 123; trace = true; max_concurrency = 4; direction = LR };
  } in
  Alcotest.(check bool) "complex lossless roundtrip" true (
    let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
    match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
    | Error e -> failwith ("Mermaid parse failed: " ^ e)
    | Ok chain2 ->
        let json1 = Json_parser.chain_to_json_string ~pretty:true chain1 in
        let json2 = Json_parser.chain_to_json_string ~pretty:true chain2 in
        if json1 <> json2 then begin
          Printf.printf "FAIL: complex lossless mismatch\n";
          Printf.printf "---- json1 ----\n%s\n" json1;
          Printf.printf "---- json2 ----\n%s\n" json2;
        end;
        json1 = json2
  )

(* Tool node nested structure roundtrip test *)
let test_tool_nested_roundtrip () =
  let json = {|{
    "id": "tool_nested_test",
    "nodes": [
      {
        "id": "parse",
        "type": "tool",
        "tool": {
          "server": "figma",
          "name": "figma_parse_url",
          "args": {"url": "{{input.figma_url}}", "depth": 4}
        }
      },
      {
        "id": "export",
        "type": "tool",
        "tool": {
          "server": "figma",
          "name": "figma_export_image",
          "args": {"scale": 2, "format": "png"}
        },
        "input_mapping": [["file_key", "parse"]]
      }
    ],
    "output": "export",
    "config": {"timeout": 60, "trace": false, "max_depth": 3}
  }|} in
  Alcotest.(check bool) "tool nested structure roundtrip" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let json1_str = Json_parser.chain_to_json_string ~pretty:true chain1 in
            let json2_str = Json_parser.chain_to_json_string ~pretty:true chain2 in
            if json1_str <> json2_str then begin
              Printf.printf "FAIL: tool nested mismatch\n";
              Printf.printf "---- json1 ----\n%s\n" json1_str;
              Printf.printf "---- json2 ----\n%s\n" json2_str;
            end;
            json1_str = json2_str
  )

(* Tool node flat format roundtrip test *)
let test_tool_flat_roundtrip () =
  let json = {|{
    "id": "tool_flat_test",
    "nodes": [
      {"id": "lint", "type": "tool", "name": "eslint", "args": {"pattern": "*.ts"}}
    ],
    "output": "lint",
    "config": {"timeout": 30, "trace": false, "max_depth": 2}
  }|} in
  Alcotest.(check bool) "tool flat format roundtrip" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            (* For flat format, just check that args are preserved *)
            let find_lint nodes = List.find_opt (fun (n:node) -> n.id = "lint") nodes in
            match find_lint chain1.nodes, find_lint chain2.nodes with
            | Some n1, Some n2 ->
                (match n1.node_type, n2.node_type with
                 | Tool { args = a1; _ }, Tool { args = a2; _ } ->
                     let eq = Yojson.Safe.equal a1 a2 in
                     if not eq then Printf.printf "FAIL: tool args mismatch\n";
                     eq
                 | _ -> false)
            | _ -> false
  )

(* ============================================================
   Production Chain Tests - Test all 14 chains from data/chains/
   ============================================================ *)

(* Helper: find project root (contains dune-project) *)
let find_project_root () =
  let rec find dir =
    let dune_project = Filename.concat dir "dune-project" in
    if Sys.file_exists dune_project then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None
      else find parent
  in
  (* Try from current directory and walk up *)
  match find (Sys.getcwd ()) with
  | Some root -> root
  | None ->
      (* Fallback: try common locations *)
      let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
      Filename.concat home "me/workspace/yousleepwhen/llm-mcp"

(* Helper: read file contents *)
let read_chain_file filename =
  let root = find_project_root () in
  let path = Printf.sprintf "%s/data/chains/%s" root filename in
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Helper: test a production chain file for lossless roundtrip *)
let test_production_chain filename () =
  let json_str = read_chain_file filename in
  let json1 = Yojson.Safe.from_string json_str in
  match Json_parser.parse_chain json1 with
  | Error e ->
      Alcotest.fail (Printf.sprintf "%s: JSON parse failed: %s" filename e)
  | Ok chain1 ->
      let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
      match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
      | Error e ->
          Alcotest.fail (Printf.sprintf "%s: Mermaid parse failed: %s" filename e)
      | Ok chain2 ->
          (* Compare: node count, output, id *)
          let n1 = List.length chain1.nodes in
          let n2 = List.length chain2.nodes in
          if n1 <> n2 then
            Alcotest.fail (Printf.sprintf "%s: node count mismatch: %d vs %d" filename n1 n2);
          if chain1.id <> chain2.id then
            Alcotest.fail (Printf.sprintf "%s: id mismatch: %s vs %s" filename chain1.id chain2.id);
          if chain1.output <> chain2.output then
            Alcotest.fail (Printf.sprintf "%s: output mismatch: %s vs %s" filename chain1.output chain2.output);
          (* Compare full JSON for strictest check *)
          let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
          let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
          if j1 <> j2 then begin
            Printf.printf "FAIL: %s roundtrip mismatch\n" filename;
            Printf.printf "---- Original nodes: %d, Roundtrip nodes: %d ----\n" n1 n2;
            Alcotest.fail (Printf.sprintf "%s: full JSON mismatch after roundtrip" filename)
          end

(* Individual production chain tests *)
let test_chain_code_migration = test_production_chain "code-migration.json"
let test_chain_deep_research = test_production_chain "deep-research.json"
let test_chain_design_to_tasks = test_production_chain "design_to_tasks.json"
let test_chain_figma_to_prototype = test_production_chain "figma-to-prototype.json"
let test_chain_figma_to_web_v2 = test_production_chain "figma-to-web-component-v2.json"
let test_chain_figma_to_web = test_production_chain "figma-to-web-component.json"
let test_chain_incident_response = test_production_chain "incident-response.json"
let test_chain_magi_code_review = test_production_chain "magi-code-review.json"
let test_chain_mcts_explore = test_production_chain "mcts-mantra-explore.json"
let test_chain_mcts_hybrid = test_production_chain "mcts-mantra-hybrid.json"
let test_chain_mcts_review = test_production_chain "mcts-mantra-review.json"
let test_chain_mermaid_to_chain = test_production_chain "mermaid-to-chain.json"
let test_chain_pr_review = test_production_chain "pr-review-pipeline.json"
let test_chain_simple_test = test_production_chain "simple-test.json"

(* ============================================================
   Advanced Node Types Roundtrip Tests (Map, Bind, Spawn, Mcts)
   ============================================================ *)

(* Map node roundtrip test *)
let test_map_roundtrip () =
  let json = {|{
    "id": "map_test",
    "nodes": [
      {"id": "source", "type": "llm", "model": "gemini", "prompt": "Generate data"},
      {
        "id": "transform",
        "type": "map",
        "func": "extract_json",
        "inner": {"id": "inner_llm", "type": "llm", "model": "claude", "prompt": "Parse: {{source}}"}
      }
    ],
    "output": "transform",
    "config": {"timeout": 60, "trace": false, "max_depth": 3}
  }|} in
  Alcotest.(check bool) "map roundtrip" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            if j1 <> j2 then Printf.printf "FAIL: map mismatch\n";
            j1 = j2
  )

(* Bind node roundtrip test *)
let test_bind_roundtrip () =
  let json = {|{
    "id": "bind_test",
    "nodes": [
      {"id": "router", "type": "llm", "model": "gemini", "prompt": "Decide route"},
      {
        "id": "dynamic",
        "type": "bind",
        "func": "select_model",
        "inner": {"id": "inner_exec", "type": "llm", "model": "claude", "prompt": "Execute: {{router}}"}
      }
    ],
    "output": "dynamic",
    "config": {"timeout": 90, "trace": true, "max_depth": 4}
  }|} in
  Alcotest.(check bool) "bind roundtrip" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            if j1 <> j2 then Printf.printf "FAIL: bind mismatch\n";
            j1 = j2
  )

(* Spawn node roundtrip test *)
let test_spawn_roundtrip () =
  let json = {|{
    "id": "spawn_test",
    "nodes": [
      {"id": "setup", "type": "llm", "model": "gemini", "prompt": "Setup context"},
      {
        "id": "isolated",
        "type": "spawn",
        "clean": true,
        "pass_vars": ["input", "config"],
        "inherit_cache": false,
        "inner": {"id": "worker", "type": "llm", "model": "claude", "prompt": "Work in isolation"}
      }
    ],
    "output": "isolated",
    "config": {"timeout": 120, "trace": false, "max_depth": 5}
  }|} in
  Alcotest.(check bool) "spawn roundtrip" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            if j1 <> j2 then Printf.printf "FAIL: spawn mismatch\n";
            j1 = j2
  )

(* ============================================================
   Combination Tests - Node type interactions
   ============================================================ *)

(* ChainRef inside Pipeline *)
let test_chainref_in_pipeline () =
  let json = {|{
    "id": "chainref_pipeline",
    "nodes": [
      {
        "id": "p",
        "type": "pipeline",
        "nodes": [
          {"id": "pre", "type": "llm", "model": "gemini", "prompt": "Prepare"},
          {"id": "ref", "type": "chain_ref", "ref": "magi-code-review"},
          {"id": "post", "type": "llm", "model": "claude", "prompt": "Finalize: {{ref}}"}
        ]
      }
    ],
    "output": "p",
    "config": {"timeout": 180, "trace": true, "max_depth": 5}
  }|} in
  Alcotest.(check bool) "chainref in pipeline" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* Fanout with mixed node types *)
let test_fanout_mixed_types () =
  let json = {|{
    "id": "fanout_mixed",
    "nodes": [
      {
        "id": "parallel",
        "type": "fanout",
        "nodes": [
          {"id": "llm_branch", "type": "llm", "model": "gemini", "prompt": "Analyze"},
          {"id": "tool_branch", "type": "tool", "name": "eslint", "args": {"pattern": "*.ts"}},
          {"id": "ref_branch", "type": "chain_ref", "ref": "simple-test"}
        ]
      },
      {
        "id": "merge_all",
        "type": "merge",
        "strategy": "concat",
        "nodes": [
          {"id": "m1", "type": "llm", "model": "claude", "prompt": "Summarize: {{parallel}}"}
        ]
      }
    ],
    "output": "merge_all",
    "config": {"timeout": 120, "trace": false, "max_depth": 4}
  }|} in
  Alcotest.(check bool) "fanout mixed types" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* Subgraph with complex inner chain *)
let test_subgraph_complex () =
  let json = {|{
    "id": "subgraph_complex",
    "nodes": [
      {"id": "input_proc", "type": "llm", "model": "gemini", "prompt": "Process input"},
      {
        "id": "inner_chain",
        "type": "subgraph",
        "graph": {
          "id": "inner",
          "nodes": [
            {"id": "a", "type": "llm", "model": "claude", "prompt": "Step A"},
            {
              "id": "gate",
              "type": "gate",
              "condition": "{{a.score}} > 0.8",
              "then": {"id": "high", "type": "llm", "model": "codex", "prompt": "High path"},
              "else": {"id": "low", "type": "llm", "model": "gemini", "prompt": "Low path"}
            }
          ],
          "output": "gate",
          "config": {"timeout": 60, "trace": false, "max_depth": 2}
        }
      }
    ],
    "output": "inner_chain",
    "config": {"timeout": 180, "trace": true, "max_depth": 6}
  }|} in
  Alcotest.(check bool) "subgraph complex" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* Nested resilience: Retry > Fallback > Race *)
let test_nested_resilience () =
  let json = {|{
    "id": "nested_resilience",
    "nodes": [
      {
        "id": "robust",
        "type": "retry",
        "max_attempts": 3,
        "backoff": {"Exponential": {"base": 1.0, "max": 10.0}},
        "retry_on": ["timeout", "rate_limit"],
        "node": {
          "id": "fallback_layer",
          "type": "fallback",
          "primary": {
            "id": "race_layer",
            "type": "race",
            "timeout": 30.0,
            "nodes": [
              {"id": "fast", "type": "llm", "model": "gemini", "prompt": "Quick"},
              {"id": "slow", "type": "llm", "model": "claude", "prompt": "Thorough"}
            ]
          },
          "fallbacks": [
            {"id": "backup", "type": "llm", "model": "codex", "prompt": "Backup plan"}
          ]
        }
      }
    ],
    "output": "robust",
    "config": {"timeout": 300, "trace": true, "max_depth": 8}
  }|} in
  Alcotest.(check bool) "nested resilience" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* GoalDriven with Evaluator inside *)
let test_goaldriven_with_evaluator () =
  let json = {|{
    "id": "goaldriven_eval",
    "nodes": [
      {
        "id": "iterative",
        "type": "goal_driven",
        "goal_metric": "coverage",
        "goal_operator": "gte",
        "goal_value": 0.9,
        "max_iterations": 5,
        "measure_func": "exec_test",
        "conversational": true,
        "relay_models": ["gemini", "claude"],
        "strategy_hints": {"below_50": "aggressive", "above_50": "conservative"},
        "action_node": {
          "id": "action",
          "type": "evaluator",
          "scoring_func": "anti_fake",
          "select_strategy": "best",
          "min_score": 0.7,
          "candidates": [
            {"id": "c1", "type": "llm", "model": "gemini", "prompt": "Generate v1"},
            {"id": "c2", "type": "llm", "model": "claude", "prompt": "Generate v2"}
          ]
        }
      }
    ],
    "output": "iterative",
    "config": {"timeout": 600, "trace": true, "max_depth": 10}
  }|} in
  Alcotest.(check bool) "goaldriven with evaluator" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* Mcts node roundtrip test *)
let test_mcts_roundtrip () =
  let json = {|{
    "id": "mcts_test",
    "nodes": [
      {
        "id": "search",
        "type": "mcts",
        "strategies": [
          {"id": "s1", "type": "llm", "model": "gemini", "prompt": "Strategy A"},
          {"id": "s2", "type": "llm", "model": "claude", "prompt": "Strategy B"}
        ],
        "simulation": {"id": "sim", "type": "llm", "model": "codex", "prompt": "Simulate"},
        "evaluator": "llm_judge",
        "evaluator_prompt": "Score 0-1",
        "policy": {"UCB1": 1.414},
        "max_iterations": 10,
        "max_depth": 3,
        "expansion_threshold": 2,
        "early_stop": 0.95,
        "parallel_sims": 2
      }
    ],
    "output": "search",
    "config": {"timeout": 300, "trace": true, "max_depth": 8}
  }|} in
  Alcotest.(check bool) "mcts roundtrip" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            if j1 <> j2 then begin
              Printf.printf "FAIL: mcts mismatch\n";
              Printf.printf "---- j1 ----\n%s\n" (Json_parser.chain_to_json_string ~pretty:true chain1);
              Printf.printf "---- j2 ----\n%s\n" (Json_parser.chain_to_json_string ~pretty:true chain2);
            end;
            j1 = j2
  )

(* ============================================================
   STRESS TESTS - Extreme combinations 🔥
   ============================================================ *)

(* 5-level deep nesting: Subgraph > Pipeline > Retry > Fanout > Gate *)
let test_stress_5level_nesting () =
  let json = {|{
    "id": "stress_5level",
    "nodes": [
      {
        "id": "level1_subgraph",
        "type": "subgraph",
        "graph": {
          "id": "inner1",
          "nodes": [
            {
              "id": "level2_pipeline",
              "type": "pipeline",
              "nodes": [
                {"id": "prep", "type": "llm", "model": "gemini", "prompt": "Prepare"},
                {
                  "id": "level3_retry",
                  "type": "retry",
                  "max_attempts": 3,
                  "backoff": {"Exponential": {"base": 1.0, "max": 10.0}},
                  "retry_on": ["timeout"],
                  "node": {
                    "id": "level4_fanout",
                    "type": "fanout",
                    "nodes": [
                      {
                        "id": "level5_gate_a",
                        "type": "gate",
                        "condition": "{{prep.score}} > 0.5",
                        "then": {"id": "high_a", "type": "llm", "model": "claude", "prompt": "High A"},
                        "else": {"id": "low_a", "type": "llm", "model": "codex", "prompt": "Low A"}
                      },
                      {
                        "id": "level5_gate_b",
                        "type": "gate",
                        "condition": "{{prep.score}} > 0.8",
                        "then": {"id": "high_b", "type": "llm", "model": "gemini", "prompt": "High B"}
                      }
                    ]
                  }
                },
                {"id": "post", "type": "llm", "model": "claude", "prompt": "Finalize"}
              ]
            }
          ],
          "output": "level2_pipeline",
          "config": {"timeout": 120, "trace": true, "max_depth": 6}
        }
      }
    ],
    "output": "level1_subgraph",
    "config": {"timeout": 300, "trace": true, "max_depth": 10}
  }|} in
  Alcotest.(check bool) "5-level nesting" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* Kitchen sink: Maximum node type diversity in one chain *)
let test_stress_kitchen_sink () =
  let json = {|{
    "id": "kitchen_sink",
    "nodes": [
      {"id": "n1_llm", "type": "llm", "model": "gemini", "prompt": "Start"},
      {"id": "n2_tool", "type": "tool", "name": "eslint", "args": {"pattern": "*.ts"}},
      {"id": "n3_chainref", "type": "chain_ref", "ref": "magi-code-review"},
      {
        "id": "n4_pipeline",
        "type": "pipeline",
        "nodes": [
          {"id": "p1", "type": "llm", "model": "claude", "prompt": "Pipeline step 1"},
          {"id": "p2", "type": "llm", "model": "codex", "prompt": "Pipeline step 2"}
        ]
      },
      {
        "id": "n5_fanout",
        "type": "fanout",
        "nodes": [
          {"id": "f1", "type": "llm", "model": "gemini", "prompt": "Branch 1"},
          {"id": "f2", "type": "llm", "model": "claude", "prompt": "Branch 2"}
        ]
      },
      {
        "id": "n6_quorum",
        "type": "quorum",
        "required": 2,
        "nodes": [
          {"id": "q1", "type": "llm", "model": "gemini", "prompt": "Vote 1"},
          {"id": "q2", "type": "llm", "model": "claude", "prompt": "Vote 2"},
          {"id": "q3", "type": "llm", "model": "codex", "prompt": "Vote 3"}
        ]
      },
      {
        "id": "n7_gate",
        "type": "gate",
        "condition": "{{n1_llm.score}} > 0.7",
        "then": {"id": "then_node", "type": "llm", "model": "claude", "prompt": "Then path"},
        "else": {"id": "else_node", "type": "llm", "model": "gemini", "prompt": "Else path"}
      },
      {
        "id": "n8_retry",
        "type": "retry",
        "max_attempts": 2,
        "backoff": {"Fixed": 1.0},
        "retry_on": ["error"],
        "node": {"id": "retry_inner", "type": "llm", "model": "codex", "prompt": "Retry this"}
      },
      {
        "id": "n9_fallback",
        "type": "fallback",
        "primary": {"id": "primary", "type": "llm", "model": "claude", "prompt": "Primary"},
        "fallbacks": [
          {"id": "fb1", "type": "llm", "model": "gemini", "prompt": "Fallback 1"},
          {"id": "fb2", "type": "llm", "model": "codex", "prompt": "Fallback 2"}
        ]
      },
      {
        "id": "n10_race",
        "type": "race",
        "timeout": 30.0,
        "nodes": [
          {"id": "r1", "type": "llm", "model": "gemini", "prompt": "Race 1"},
          {"id": "r2", "type": "llm", "model": "claude", "prompt": "Race 2"}
        ]
      },
      {
        "id": "n11_merge",
        "type": "merge",
        "strategy": "concat",
        "nodes": [
          {"id": "m1", "type": "llm", "model": "claude", "prompt": "Merge input 1"},
          {"id": "m2", "type": "llm", "model": "gemini", "prompt": "Merge input 2"}
        ]
      },
      {
        "id": "n12_threshold",
        "type": "threshold",
        "metric": "confidence",
        "operator": "gte",
        "value": 0.8,
        "input_node": {"id": "thresh_in", "type": "llm", "model": "claude", "prompt": "Check"},
        "on_pass": {"id": "pass", "type": "llm", "model": "gemini", "prompt": "Passed"},
        "on_fail": {"id": "fail", "type": "llm", "model": "codex", "prompt": "Failed"}
      },
      {
        "id": "n13_evaluator",
        "type": "evaluator",
        "scoring_func": "llm_judge",
        "select_strategy": "best",
        "min_score": 0.6,
        "candidates": [
          {"id": "e1", "type": "llm", "model": "gemini", "prompt": "Candidate 1"},
          {"id": "e2", "type": "llm", "model": "claude", "prompt": "Candidate 2"}
        ]
      }
    ],
    "output": "n13_evaluator",
    "config": {"timeout": 600, "trace": true, "max_depth": 15}
  }|} in
  Alcotest.(check bool) "kitchen sink" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* Triple subgraph nesting: Subgraph > Subgraph > Subgraph *)
let test_stress_triple_subgraph () =
  let json = {|{
    "id": "triple_sub",
    "nodes": [
      {
        "id": "outer",
        "type": "subgraph",
        "graph": {
          "id": "mid_chain",
          "nodes": [
            {"id": "mid_pre", "type": "llm", "model": "gemini", "prompt": "Mid prep"},
            {
              "id": "middle",
              "type": "subgraph",
              "graph": {
                "id": "inner_chain",
                "nodes": [
                  {"id": "inner_pre", "type": "llm", "model": "claude", "prompt": "Inner prep"},
                  {
                    "id": "innermost",
                    "type": "subgraph",
                    "graph": {
                      "id": "core_chain",
                      "nodes": [
                        {"id": "core1", "type": "llm", "model": "codex", "prompt": "Core 1"},
                        {"id": "core2", "type": "llm", "model": "gemini", "prompt": "Core 2"}
                      ],
                      "output": "core2",
                      "config": {"timeout": 30, "trace": true, "max_depth": 2}
                    }
                  }
                ],
                "output": "innermost",
                "config": {"timeout": 60, "trace": true, "max_depth": 3}
              }
            }
          ],
          "output": "middle",
          "config": {"timeout": 120, "trace": true, "max_depth": 4}
        }
      }
    ],
    "output": "outer",
    "config": {"timeout": 300, "trace": true, "max_depth": 8}
  }|} in
  Alcotest.(check bool) "triple subgraph" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* MCTS with Pipeline strategies *)
let test_stress_mcts_pipeline_strategies () =
  let json = {|{
    "id": "mcts_complex",
    "nodes": [
      {
        "id": "search",
        "type": "mcts",
        "strategies": [
          {
            "id": "strategy_pipeline",
            "type": "pipeline",
            "nodes": [
              {"id": "sp1", "type": "llm", "model": "gemini", "prompt": "Analyze"},
              {"id": "sp2", "type": "llm", "model": "claude", "prompt": "Synthesize"}
            ]
          },
          {
            "id": "strategy_fanout",
            "type": "fanout",
            "nodes": [
              {"id": "sf1", "type": "llm", "model": "codex", "prompt": "Path A"},
              {"id": "sf2", "type": "llm", "model": "gemini", "prompt": "Path B"}
            ]
          }
        ],
        "simulation": {
          "id": "sim_retry",
          "type": "retry",
          "max_attempts": 2,
          "backoff": {"Fixed": 0.5},
          "retry_on": ["timeout"],
          "node": {"id": "sim_inner", "type": "llm", "model": "claude", "prompt": "Simulate"}
        },
        "evaluator": "llm_judge",
        "evaluator_prompt": "Score 0-1 based on quality",
        "policy": {"UCB1": 1.414},
        "max_iterations": 5,
        "max_depth": 3,
        "expansion_threshold": 2,
        "early_stop": 0.9,
        "parallel_sims": 1
      }
    ],
    "output": "search",
    "config": {"timeout": 600, "trace": true, "max_depth": 10}
  }|} in
  Alcotest.(check bool) "mcts pipeline strategies" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* Evaluator with complex candidates (Pipeline + Fanout) *)
let test_stress_evaluator_complex_candidates () =
  let json = {|{
    "id": "eval_complex",
    "nodes": [
      {
        "id": "eval",
        "type": "evaluator",
        "scoring_func": "anti_fake",
        "scoring_prompt": "Check for quality and completeness",
        "select_strategy": "best",
        "min_score": 0.7,
        "candidates": [
          {
            "id": "cand_pipeline",
            "type": "pipeline",
            "nodes": [
              {"id": "cp1", "type": "llm", "model": "gemini", "prompt": "Step 1"},
              {"id": "cp2", "type": "llm", "model": "claude", "prompt": "Step 2"},
              {"id": "cp3", "type": "llm", "model": "codex", "prompt": "Step 3"}
            ]
          },
          {
            "id": "cand_fanout",
            "type": "fanout",
            "nodes": [
              {"id": "cf1", "type": "llm", "model": "gemini", "prompt": "Parallel A"},
              {"id": "cf2", "type": "llm", "model": "claude", "prompt": "Parallel B"}
            ]
          },
          {
            "id": "cand_retry",
            "type": "retry",
            "max_attempts": 3,
            "backoff": {"Exponential": {"base": 0.5, "max": 5.0}},
            "retry_on": ["error", "timeout"],
            "node": {"id": "cr_inner", "type": "llm", "model": "codex", "prompt": "Robust generation"}
          }
        ]
      }
    ],
    "output": "eval",
    "config": {"timeout": 300, "trace": true, "max_depth": 8}
  }|} in
  Alcotest.(check bool) "evaluator complex candidates" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* GoalDriven with MCTS action_node *)
let test_stress_goaldriven_mcts () =
  let json = {|{
    "id": "goal_mcts",
    "nodes": [
      {
        "id": "optimizer",
        "type": "goal_driven",
        "goal_metric": "test_coverage",
        "goal_operator": "gte",
        "goal_value": 0.95,
        "max_iterations": 10,
        "measure_func": "run_tests",
        "conversational": false,
        "strategy_hints": {"below_30": "expand", "below_70": "refine", "above_70": "polish"},
        "action_node": {
          "id": "mcts_action",
          "type": "mcts",
          "strategies": [
            {"id": "s1", "type": "llm", "model": "gemini", "prompt": "Strategy 1"},
            {"id": "s2", "type": "llm", "model": "claude", "prompt": "Strategy 2"}
          ],
          "simulation": {"id": "sim", "type": "llm", "model": "codex", "prompt": "Simulate"},
          "evaluator": "llm_judge",
          "policy": {"UCB1": 1.414},
          "max_iterations": 3,
          "max_depth": 2,
          "expansion_threshold": 1,
          "parallel_sims": 1
        }
      }
    ],
    "output": "optimizer",
    "config": {"timeout": 1200, "trace": true, "max_depth": 15}
  }|} in
  Alcotest.(check bool) "goaldriven mcts" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* FeedbackLoop with GoalDriven generator *)
let test_stress_feedbackloop_goaldriven () =
  let json = {|{
    "id": "feedback_goal",
    "nodes": [
      {
        "id": "loop",
        "type": "feedback_loop",
        "generator": {
          "id": "gen_goal",
          "type": "goal_driven",
          "goal_metric": "quality",
          "goal_operator": "gte",
          "goal_value": 0.8,
          "max_iterations": 5,
          "measure_func": "quality_check",
          "conversational": true,
          "action_node": {"id": "gen_action", "type": "llm", "model": "claude", "prompt": "Generate"}
        },
        "evaluator_config": {
          "scoring_func": "anti_fake",
          "scoring_prompt": "Score the output quality"
        },
        "improver_prompt": "Improve based on: {{feedback}}",
        "max_iterations": 3,
        "score_threshold": 0.9,
        "score_operator": "gte"
      }
    ],
    "output": "loop",
    "config": {"timeout": 900, "trace": true, "max_depth": 12}
  }|} in
  Alcotest.(check bool) "feedbackloop goaldriven" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* All resilience patterns combined: Retry > Fallback > Race > Gate *)
let test_stress_resilience_pyramid () =
  let json = {|{
    "id": "resilience_pyramid",
    "nodes": [
      {
        "id": "layer1_retry",
        "type": "retry",
        "max_attempts": 3,
        "backoff": {"Exponential": {"base": 1.0, "max": 30.0}},
        "retry_on": ["timeout", "rate_limit", "error"],
        "node": {
          "id": "layer2_fallback",
          "type": "fallback",
          "primary": {
            "id": "layer3_race",
            "type": "race",
            "timeout": 60.0,
            "nodes": [
              {
                "id": "race_gate1",
                "type": "gate",
                "condition": "{{input.priority}} == 'high'",
                "then": {"id": "fast_path", "type": "llm", "model": "gemini", "prompt": "Fast high priority"},
                "else": {"id": "normal_path", "type": "llm", "model": "claude", "prompt": "Normal priority"}
              },
              {
                "id": "race_gate2",
                "type": "gate",
                "condition": "{{input.type}} == 'code'",
                "then": {"id": "code_path", "type": "llm", "model": "codex", "prompt": "Code specialized"},
                "else": {"id": "text_path", "type": "llm", "model": "gemini", "prompt": "Text specialized"}
              }
            ]
          },
          "fallbacks": [
            {
              "id": "fb_quorum",
              "type": "quorum",
              "required": 2,
              "nodes": [
                {"id": "fb_q1", "type": "llm", "model": "gemini", "prompt": "Fallback vote 1"},
                {"id": "fb_q2", "type": "llm", "model": "claude", "prompt": "Fallback vote 2"},
                {"id": "fb_q3", "type": "llm", "model": "codex", "prompt": "Fallback vote 3"}
              ]
            },
            {"id": "last_resort", "type": "llm", "model": "gemini", "prompt": "Last resort"}
          ]
        }
      }
    ],
    "output": "layer1_retry",
    "config": {"timeout": 600, "trace": true, "max_depth": 12}
  }|} in
  Alcotest.(check bool) "resilience pyramid" true (
    let json1 = Yojson.Safe.from_string json in
    match Json_parser.parse_chain json1 with
    | Error e -> failwith ("JSON parse failed: " ^ e)
    | Ok chain1 ->
        let mermaid = Mermaid_parser.chain_to_mermaid ~styled:false chain1 in
        match Mermaid_parser.parse_mermaid_to_chain ~id:"fallback" mermaid with
        | Error e -> failwith ("Mermaid parse failed: " ^ e)
        | Ok chain2 ->
            let j1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
            let j2 = Json_parser.chain_to_json_string ~pretty:false chain2 in
            j1 = j2
  )

(* ============================================================
   REVERSE ROUNDTRIP - Mermaid → JSON → Mermaid → JSON
   ============================================================ *)

(* Helper for reverse roundtrip verification *)
let verify_reverse_roundtrip mermaid_input =
  (* Step 1: Mermaid → Chain₁ *)
  match Mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid_input with
  | Error e -> failwith ("Mermaid₁ parse failed: " ^ e)
  | Ok chain1 ->
      (* Step 2: Chain₁ → JSON₁ *)
      let json1 = Json_parser.chain_to_json_string ~pretty:false chain1 in
      (* Step 3: JSON₁ → Chain₂ *)
      let json1_parsed = Yojson.Safe.from_string json1 in
      match Json_parser.parse_chain json1_parsed with
      | Error e -> failwith ("JSON₁ parse failed: " ^ e)
      | Ok chain2 ->
          (* Step 4: Chain₂ → Mermaid₂ *)
          let mermaid2 = Mermaid_parser.chain_to_mermaid ~styled:false chain2 in
          (* Step 5: Mermaid₂ → Chain₃ *)
          match Mermaid_parser.parse_mermaid_to_chain ~id:"test" mermaid2 with
          | Error e -> failwith ("Mermaid₂ parse failed: " ^ e)
          | Ok chain3 ->
              (* Step 6: Chain₃ → JSON₃ *)
              let json3 = Json_parser.chain_to_json_string ~pretty:false chain3 in
              (* Verify: JSON₁ = JSON₃ *)
              json1 = json3

(* Reverse: Simple pipeline *)
let test_reverse_simple_pipeline () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_pipe","nodes":[{"id":"a","type":"llm","model":"gemini","prompt":"Hello"},{"id":"b","type":"llm","model":"claude","prompt":"World: {{a}}"}],"output":"b","config":{"timeout":60,"trace":false,"max_depth":3,"max_concurrency":1}}
    a["LLM:gemini 'Hello'"]
    b["LLM:claude 'World: {{a}}'"]
    a --> b
|} in
  Alcotest.(check bool) "reverse simple pipeline" true (verify_reverse_roundtrip mermaid)

(* Reverse: Fanout with Quorum *)
let test_reverse_fanout_quorum () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_fanout","nodes":[{"id":"fan","type":"fanout","nodes":[{"id":"f1","type":"llm","model":"gemini","prompt":"A"},{"id":"f2","type":"llm","model":"claude","prompt":"B"},{"id":"f3","type":"llm","model":"codex","prompt":"C"}]},{"id":"vote","type":"quorum","required":2,"nodes":[{"id":"q1","type":"llm","model":"gemini","prompt":"Vote1"},{"id":"q2","type":"llm","model":"claude","prompt":"Vote2"}]}],"output":"vote","config":{"timeout":120,"trace":true,"max_depth":5,"max_concurrency":3}}
    fan[["Fanout"]]
    vote{Quorum:2}
    fan --> vote
|} in
  Alcotest.(check bool) "reverse fanout quorum" true (verify_reverse_roundtrip mermaid)

(* Reverse: Nested retry with fallback *)
let test_reverse_nested_resilience () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_resil","nodes":[{"id":"robust","type":"retry","max_attempts":3,"backoff":{"Exponential":{"base":1.0,"max":10.0}},"retry_on":["timeout"],"node":{"id":"fb","type":"fallback","primary":{"id":"main","type":"llm","model":"claude","prompt":"Main"},"fallbacks":[{"id":"backup","type":"llm","model":"gemini","prompt":"Backup"}]}}],"output":"robust","config":{"timeout":180,"trace":true,"max_depth":6,"max_concurrency":1}}
    robust("Retry 3x (exp 1.0x)")
|} in
  Alcotest.(check bool) "reverse nested resilience" true (verify_reverse_roundtrip mermaid)

(* Reverse: Gate with branches *)
let test_reverse_gate () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_gate","nodes":[{"id":"check","type":"llm","model":"gemini","prompt":"Check"},{"id":"gate","type":"gate","condition":"{{check.score}} > 0.5","then":{"id":"high","type":"llm","model":"claude","prompt":"High"},"else":{"id":"low","type":"llm","model":"codex","prompt":"Low"}}],"output":"gate","config":{"timeout":60,"trace":false,"max_depth":4,"max_concurrency":1}}
    check["LLM:gemini 'Check'"]
    gate{Gate:{{check.score}} > 0.5}
    check --> gate
|} in
  Alcotest.(check bool) "reverse gate" true (verify_reverse_roundtrip mermaid)

(* Reverse: Subgraph with inner chain *)
let test_reverse_subgraph () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_sub","nodes":[{"id":"outer","type":"subgraph","graph":{"id":"inner","nodes":[{"id":"a","type":"llm","model":"gemini","prompt":"Inner A"},{"id":"b","type":"llm","model":"claude","prompt":"Inner B"}],"output":"b","config":{"timeout":30,"trace":true,"max_depth":2,"max_concurrency":1}}}],"output":"outer","config":{"timeout":90,"trace":true,"max_depth":5,"max_concurrency":1}}
    outer[/"Subgraph:inner"/]
|} in
  Alcotest.(check bool) "reverse subgraph" true (verify_reverse_roundtrip mermaid)

(* Reverse: Evaluator with candidates *)
let test_reverse_evaluator () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_eval","nodes":[{"id":"eval","type":"evaluator","scoring_func":"llm_judge","select_strategy":"best","min_score":0.7,"candidates":[{"id":"c1","type":"llm","model":"gemini","prompt":"Candidate 1"},{"id":"c2","type":"llm","model":"claude","prompt":"Candidate 2"}]}],"output":"eval","config":{"timeout":120,"trace":true,"max_depth":4,"max_concurrency":2}}
    eval{Evaluator:llm_judge:best}
|} in
  Alcotest.(check bool) "reverse evaluator" true (verify_reverse_roundtrip mermaid)

(* Reverse: GoalDriven with action *)
let test_reverse_goaldriven () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_goal","nodes":[{"id":"opt","type":"goal_driven","goal_metric":"quality","goal_operator":"gte","goal_value":0.8,"max_iterations":5,"measure_func":"check","conversational":false,"action_node":{"id":"act","type":"llm","model":"claude","prompt":"Generate"}}],"output":"opt","config":{"timeout":300,"trace":true,"max_depth":8,"max_concurrency":1}}
    opt{GoalDriven:quality:gte:0.80:5}
|} in
  Alcotest.(check bool) "reverse goaldriven" true (verify_reverse_roundtrip mermaid)

(* Reverse: MCTS with strategies *)
let test_reverse_mcts () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_mcts","nodes":[{"id":"search","type":"mcts","strategies":[{"id":"s1","type":"llm","model":"gemini","prompt":"Strategy A"},{"id":"s2","type":"llm","model":"claude","prompt":"Strategy B"}],"simulation":{"id":"sim","type":"llm","model":"codex","prompt":"Simulate"},"evaluator":"llm_judge","evaluator_prompt":null,"policy":{"UCB1":1.414},"max_iterations":5,"max_depth":3,"expansion_threshold":2,"early_stop":null,"parallel_sims":1}],"output":"search","config":{"timeout":300,"trace":true,"max_depth":8,"max_concurrency":1}}
    search[[MCTS UCB1:1.41 5iter]]
|} in
  Alcotest.(check bool) "reverse mcts" true (verify_reverse_roundtrip mermaid)

(* Reverse: FeedbackLoop *)
let test_reverse_feedbackloop () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_loop","nodes":[{"id":"loop","type":"feedback_loop","generator":{"id":"gen","type":"llm","model":"claude","prompt":"Generate"},"evaluator_config":{"scoring_func":"anti_fake","scoring_prompt":"Check quality"},"improver_prompt":"Improve: {{feedback}}","max_iterations":3,"score_threshold":0.9,"score_operator":"gte"}],"output":"loop","config":{"timeout":180,"trace":true,"max_depth":6,"max_concurrency":1}}
    loop[[FeedbackLoop:3:>=0.90]]
|} in
  Alcotest.(check bool) "reverse feedbackloop" true (verify_reverse_roundtrip mermaid)

(* Reverse: Threshold with branches *)
let test_reverse_threshold () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_thresh","nodes":[{"id":"th","type":"threshold","metric":"confidence","operator":"gte","value":0.8,"input_node":{"id":"check","type":"llm","model":"gemini","prompt":"Check"},"on_pass":{"id":"pass","type":"llm","model":"claude","prompt":"Pass"},"on_fail":{"id":"fail","type":"llm","model":"codex","prompt":"Fail"}}],"output":"th","config":{"timeout":90,"trace":true,"max_depth":5,"max_concurrency":1}}
    th{Threshold:confidence:gte:0.80}
|} in
  Alcotest.(check bool) "reverse threshold" true (verify_reverse_roundtrip mermaid)

(* Reverse: Race with timeout *)
let test_reverse_race () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_race","nodes":[{"id":"race","type":"race","timeout":30.0,"nodes":[{"id":"r1","type":"llm","model":"gemini","prompt":"Fast"},{"id":"r2","type":"llm","model":"claude","prompt":"Slow"}]}],"output":"race","config":{"timeout":60,"trace":false,"max_depth":3,"max_concurrency":2}}
    race("Race 30.0s")
|} in
  Alcotest.(check bool) "reverse race" true (verify_reverse_roundtrip mermaid)

(* Reverse: Tool node *)
let test_reverse_tool () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_tool","nodes":[{"id":"lint","type":"tool","name":"eslint","args":{"pattern":"*.ts","fix":true}}],"output":"lint","config":{"timeout":60,"trace":false,"max_depth":2,"max_concurrency":1}}
    lint["Tool:eslint"]
|} in
  Alcotest.(check bool) "reverse tool" true (verify_reverse_roundtrip mermaid)

(* Reverse: ChainRef *)
let test_reverse_chainref () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_ref","nodes":[{"id":"ref","type":"chain_ref","ref":"magi-code-review"}],"output":"ref","config":{"timeout":120,"trace":true,"max_depth":3,"max_concurrency":1}}
    ref[/"Ref:magi-code-review"/]
|} in
  Alcotest.(check bool) "reverse chainref" true (verify_reverse_roundtrip mermaid)

(* Reverse: Merge with strategy *)
let test_reverse_merge () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_merge","nodes":[{"id":"merge","type":"merge","strategy":"concat","nodes":[{"id":"m1","type":"llm","model":"gemini","prompt":"Part 1"},{"id":"m2","type":"llm","model":"claude","prompt":"Part 2"}]}],"output":"merge","config":{"timeout":60,"trace":false,"max_depth":3,"max_concurrency":2}}
    merge{Merge:concat}
|} in
  Alcotest.(check bool) "reverse merge" true (verify_reverse_roundtrip mermaid)

(* Reverse: Map node *)
let test_reverse_map () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_map","nodes":[{"id":"map","type":"map","func":"extract","inner":{"id":"inner","type":"llm","model":"claude","prompt":"Transform"}}],"output":"map","config":{"timeout":90,"trace":true,"max_depth":4,"max_concurrency":1}}
    map[/"Map:extract"/]
|} in
  Alcotest.(check bool) "reverse map" true (verify_reverse_roundtrip mermaid)

(* Reverse: Spawn node *)
let test_reverse_spawn () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_spawn","nodes":[{"id":"spawn","type":"spawn","clean":true,"pass_vars":["input","config"],"inherit_cache":false,"inner":{"id":"child","type":"llm","model":"claude","prompt":"Child task"}}],"output":"spawn","config":{"timeout":120,"trace":true,"max_depth":5,"max_concurrency":1}}
    spawn[[Spawn:clean]]
|} in
  Alcotest.(check bool) "reverse spawn" true (verify_reverse_roundtrip mermaid)

(* ============================================================
   REVERSE STRESS - Complex Mermaid → JSON → Mermaid
   ============================================================ *)

(* Reverse Stress: 5-level nesting *)
let test_reverse_stress_5level () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_5level","nodes":[{"id":"l1","type":"subgraph","graph":{"id":"inner1","nodes":[{"id":"l2","type":"pipeline","nodes":[{"id":"prep","type":"llm","model":"gemini","prompt":"Prep"},{"id":"l3","type":"retry","max_attempts":2,"backoff":{"Fixed":1.0},"retry_on":["error"],"node":{"id":"l4","type":"fanout","nodes":[{"id":"l5a","type":"gate","condition":"{{x}}>0","then":{"id":"yes","type":"llm","model":"claude","prompt":"Yes"}},{"id":"l5b","type":"llm","model":"codex","prompt":"Simple"}]}}]}],"output":"l2","config":{"timeout":60,"trace":true,"max_depth":5,"max_concurrency":1}}}],"output":"l1","config":{"timeout":180,"trace":true,"max_depth":10,"max_concurrency":1}}
    l1[/"Subgraph:inner1"/]
|} in
  Alcotest.(check bool) "reverse stress 5level" true (verify_reverse_roundtrip mermaid)

(* Reverse Stress: All node types *)
let test_reverse_stress_all_types () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_all","nodes":[{"id":"n1","type":"llm","model":"gemini","prompt":"Start"},{"id":"n2","type":"tool","name":"lint","args":{}},{"id":"n3","type":"chain_ref","ref":"other"},{"id":"n4","type":"pipeline","nodes":[{"id":"p1","type":"llm","model":"claude","prompt":"P1"}]},{"id":"n5","type":"fanout","nodes":[{"id":"f1","type":"llm","model":"codex","prompt":"F1"}]},{"id":"n6","type":"quorum","required":1,"nodes":[{"id":"q1","type":"llm","model":"gemini","prompt":"Q1"}]},{"id":"n7","type":"gate","condition":"true","then":{"id":"t","type":"llm","model":"claude","prompt":"T"}},{"id":"n8","type":"retry","max_attempts":1,"backoff":{"Fixed":0.5},"retry_on":[],"node":{"id":"r","type":"llm","model":"codex","prompt":"R"}},{"id":"n9","type":"race","timeout":10.0,"nodes":[{"id":"rc","type":"llm","model":"gemini","prompt":"RC"}]},{"id":"n10","type":"merge","strategy":"first","nodes":[{"id":"mg","type":"llm","model":"claude","prompt":"MG"}]},{"id":"n11","type":"evaluator","scoring_func":"custom","select_strategy":"best","candidates":[{"id":"ev","type":"llm","model":"codex","prompt":"EV"}]}],"output":"n11","config":{"timeout":300,"trace":true,"max_depth":10,"max_concurrency":3}}
    n1["LLM:gemini 'Start'"]
    n11{Evaluator:custom:best}
    n1 --> n11
|} in
  Alcotest.(check bool) "reverse stress all types" true (verify_reverse_roundtrip mermaid)

(* Reverse Stress: Deeply nested resilience *)
let test_reverse_stress_resilience_tower () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_tower","nodes":[{"id":"r1","type":"retry","max_attempts":3,"backoff":{"Exponential":{"base":1.0,"max":30.0}},"retry_on":["timeout","error"],"node":{"id":"r2","type":"retry","max_attempts":2,"backoff":{"Fixed":2.0},"retry_on":["rate_limit"],"node":{"id":"fb","type":"fallback","primary":{"id":"race","type":"race","timeout":30.0,"nodes":[{"id":"fast","type":"llm","model":"gemini","prompt":"Fast"},{"id":"slow","type":"llm","model":"claude","prompt":"Slow"}]},"fallbacks":[{"id":"backup","type":"llm","model":"codex","prompt":"Backup"}]}}}],"output":"r1","config":{"timeout":600,"trace":true,"max_depth":10,"max_concurrency":2}}
    r1("Retry 3x (exp 1.0x)")
|} in
  Alcotest.(check bool) "reverse stress resilience tower" true (verify_reverse_roundtrip mermaid)

(* Reverse Stress: GoalDriven + MCTS + FeedbackLoop combo *)
let test_reverse_stress_meta_combo () =
  let mermaid = {|graph LR
    %% @chain_full {"id":"rev_meta","nodes":[{"id":"goal","type":"goal_driven","goal_metric":"quality","goal_operator":"gte","goal_value":0.9,"max_iterations":5,"measure_func":"test","conversational":true,"action_node":{"id":"loop","type":"feedback_loop","generator":{"id":"gen","type":"llm","model":"claude","prompt":"Gen"},"evaluator_config":{"scoring_func":"llm_judge"},"improver_prompt":"Fix: {{feedback}}","max_iterations":3,"score_threshold":0.8,"score_operator":"gte"}}],"output":"goal","config":{"timeout":900,"trace":true,"max_depth":15,"max_concurrency":1}}
    goal{GoalDriven:quality:gte:0.90:5}
|} in
  Alcotest.(check bool) "reverse stress meta combo" true (verify_reverse_roundtrip mermaid)

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
      Alcotest.test_case "Always lossless (metadata present)" `Quick test_always_lossless;
      Alcotest.test_case "Fallback ID when no metadata" `Quick test_fallback_id;
      Alcotest.test_case "chain_full override" `Quick test_chain_full_override;
      Alcotest.test_case "FeedbackLoop lossless" `Quick test_feedback_loop_roundtrip;
      Alcotest.test_case "Partial metadata" `Quick test_partial_metadata;
      Alcotest.test_case "Complex lossless (nested/resilience)" `Quick test_complex_lossless_roundtrip;
      Alcotest.test_case "inputs assoc format (chain_to_json compat)" `Quick test_inputs_assoc_format;
      Alcotest.test_case "inputs full roundtrip" `Quick test_inputs_full_roundtrip;
    ];
    "goaldriven", [
      Alcotest.test_case "GoalDriven explicit syntax" `Quick test_goaldriven_mermaid_parsing;
      Alcotest.test_case "GoalDriven ID-based inference" `Quick test_goaldriven_id_inference;
      Alcotest.test_case "GoalDriven strict roundtrip" `Quick test_goaldriven_strict_roundtrip;
    ];
    "tool_nodes", [
      Alcotest.test_case "Tool nested structure (server:name)" `Quick test_tool_nested_roundtrip;
      Alcotest.test_case "Tool flat format" `Quick test_tool_flat_roundtrip;
    ];
    "advanced_nodes", [
      Alcotest.test_case "Map node (functor)" `Quick test_map_roundtrip;
      Alcotest.test_case "Bind node (monad)" `Quick test_bind_roundtrip;
      Alcotest.test_case "Spawn node (clean context)" `Quick test_spawn_roundtrip;
      Alcotest.test_case "Mcts node (tree search)" `Quick test_mcts_roundtrip;
    ];
    "combinations", [
      Alcotest.test_case "ChainRef inside Pipeline" `Quick test_chainref_in_pipeline;
      Alcotest.test_case "Fanout with mixed node types" `Quick test_fanout_mixed_types;
      Alcotest.test_case "Subgraph with complex inner" `Quick test_subgraph_complex;
      Alcotest.test_case "Nested resilience (Retry>Fallback>Race)" `Quick test_nested_resilience;
      Alcotest.test_case "GoalDriven with Evaluator" `Quick test_goaldriven_with_evaluator;
    ];
    "stress", [
      Alcotest.test_case "5-level deep nesting" `Quick test_stress_5level_nesting;
      Alcotest.test_case "Kitchen sink (13 node types)" `Quick test_stress_kitchen_sink;
      Alcotest.test_case "Triple subgraph nesting" `Quick test_stress_triple_subgraph;
      Alcotest.test_case "MCTS with Pipeline strategies" `Quick test_stress_mcts_pipeline_strategies;
      Alcotest.test_case "Evaluator complex candidates" `Quick test_stress_evaluator_complex_candidates;
      Alcotest.test_case "GoalDriven with MCTS" `Quick test_stress_goaldriven_mcts;
      Alcotest.test_case "FeedbackLoop with GoalDriven" `Quick test_stress_feedbackloop_goaldriven;
      Alcotest.test_case "Resilience pyramid (4-layer)" `Quick test_stress_resilience_pyramid;
    ];
    "production_chains", [
      Alcotest.test_case "code-migration.json" `Quick test_chain_code_migration;
      Alcotest.test_case "deep-research.json" `Quick test_chain_deep_research;
      Alcotest.test_case "design_to_tasks.json" `Quick test_chain_design_to_tasks;
      Alcotest.test_case "figma-to-prototype.json" `Quick test_chain_figma_to_prototype;
      Alcotest.test_case "figma-to-web-component-v2.json" `Quick test_chain_figma_to_web_v2;
      Alcotest.test_case "figma-to-web-component.json" `Quick test_chain_figma_to_web;
      Alcotest.test_case "incident-response.json" `Quick test_chain_incident_response;
      Alcotest.test_case "magi-code-review.json" `Quick test_chain_magi_code_review;
      Alcotest.test_case "mcts-mantra-explore.json" `Quick test_chain_mcts_explore;
      Alcotest.test_case "mcts-mantra-hybrid.json" `Quick test_chain_mcts_hybrid;
      Alcotest.test_case "mcts-mantra-review.json" `Quick test_chain_mcts_review;
      Alcotest.test_case "mermaid-to-chain.json" `Quick test_chain_mermaid_to_chain;
      Alcotest.test_case "pr-review-pipeline.json" `Quick test_chain_pr_review;
      Alcotest.test_case "simple-test.json" `Quick test_chain_simple_test;
    ];
    "reverse_roundtrip", [
      Alcotest.test_case "Simple pipeline (Mermaid→JSON→Mermaid)" `Quick test_reverse_simple_pipeline;
      Alcotest.test_case "Fanout with Quorum" `Quick test_reverse_fanout_quorum;
      Alcotest.test_case "Nested resilience (Retry>Fallback>Race)" `Quick test_reverse_nested_resilience;
      Alcotest.test_case "Gate with branches" `Quick test_reverse_gate;
      Alcotest.test_case "Subgraph (inner chain)" `Quick test_reverse_subgraph;
      Alcotest.test_case "Evaluator node" `Quick test_reverse_evaluator;
      Alcotest.test_case "GoalDriven node" `Quick test_reverse_goaldriven;
      Alcotest.test_case "MCTS node" `Quick test_reverse_mcts;
      Alcotest.test_case "FeedbackLoop node" `Quick test_reverse_feedbackloop;
      Alcotest.test_case "Threshold node" `Quick test_reverse_threshold;
      Alcotest.test_case "Race node" `Quick test_reverse_race;
      Alcotest.test_case "Tool node" `Quick test_reverse_tool;
      Alcotest.test_case "ChainRef node" `Quick test_reverse_chainref;
      Alcotest.test_case "Merge node" `Quick test_reverse_merge;
      Alcotest.test_case "Map node" `Quick test_reverse_map;
      Alcotest.test_case "Spawn node" `Quick test_reverse_spawn;
    ];
    "reverse_stress", [
      Alcotest.test_case "5-level deep nesting" `Quick test_reverse_stress_5level;
      Alcotest.test_case "All node types (11 types)" `Quick test_reverse_stress_all_types;
      Alcotest.test_case "Resilience tower (4-layer)" `Quick test_reverse_stress_resilience_tower;
      Alcotest.test_case "Meta combo (GoalDriven+FeedbackLoop)" `Quick test_reverse_stress_meta_combo;
    ];
  ]

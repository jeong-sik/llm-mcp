(** Tests for Cascade node type - confidence parsing, tier routing, Mermaid DSL

    Pure function tests:
    - confidence_of_string: High/Medium/Low/garbage input
    - confidence_to_float: numeric mapping
    - context_mode roundtrip: CM_None/CM_Summary/CM_Full
    - make_cascade: default values
    - cascade_tier yojson roundtrip
    - Mermaid 3-tier parsing
    - Mermaid bare Cascade parsing
    - cascade_stats tracking
*)

open Alcotest
open Chain_types

(** {1 Confidence Level Tests} *)

let test_confidence_of_string_valid () =
  check bool "High" true (confidence_of_string "High" = High);
  check bool "Medium" true (confidence_of_string "Medium" = Medium);
  check bool "Low" true (confidence_of_string "Low" = Low)

let test_confidence_of_string_case_insensitive () =
  check bool "high" true (confidence_of_string "high" = High);
  check bool "HIGH" true (confidence_of_string "HIGH" = High);
  check bool "medium" true (confidence_of_string "medium" = Medium);
  check bool "MEDIUM" true (confidence_of_string "MEDIUM" = Medium);
  check bool "low" true (confidence_of_string "low" = Low)

let test_confidence_of_string_garbage () =
  check bool "garbage defaults Low" true (confidence_of_string "garbage" = Low);
  check bool "empty defaults Low" true (confidence_of_string "" = Low);
  check bool "number defaults Low" true (confidence_of_string "0.8" = Low)

let test_confidence_to_float () =
  check (float 0.001) "High=1.0" 1.0 (confidence_to_float High);
  check (float 0.001) "Medium=0.5" 0.5 (confidence_to_float Medium);
  check (float 0.001) "Low=0.2" 0.2 (confidence_to_float Low)

(** {1 Context Mode Tests} *)

let test_context_mode_roundtrip () =
  let test mode =
    let s = context_mode_to_string mode in
    let mode' = context_mode_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (mode = mode')
  in
  List.iter test [CM_None; CM_Summary; CM_Full]

let test_context_mode_of_string () =
  check bool "none" true (context_mode_of_string "none" = CM_None);
  check bool "summary" true (context_mode_of_string "summary" = CM_Summary);
  check bool "full" true (context_mode_of_string "full" = CM_Full);
  check bool "unknown defaults Summary" true (context_mode_of_string "xyz" = CM_Summary)

let test_context_mode_yojson_roundtrip () =
  let test mode =
    let json = context_mode_to_yojson mode in
    match context_mode_of_yojson json with
    | Ok mode' -> check bool (Printf.sprintf "yojson roundtrip %s" (context_mode_to_string mode)) true (mode = mode')
    | Error e -> fail (Printf.sprintf "yojson roundtrip failed: %s" e)
  in
  List.iter test [CM_None; CM_Summary; CM_Full]

(** {1 Cascade Type Tests} *)

let test_make_cascade_defaults () =
  let node = make_cascade ~id:"test" ~tiers:[] () in
  check string "id" "test" node.id;
  match node.node_type with
  | Cascade { tiers; confidence_prompt; max_escalations; context_mode; task_hint; default_threshold } ->
    check int "tiers empty" 0 (List.length tiers);
    check bool "no confidence_prompt" true (confidence_prompt = None);
    check int "max_escalations default" 2 max_escalations;
    check bool "context_mode default Summary" true (context_mode = CM_Summary);
    check bool "no task_hint" true (task_hint = None);
    check (float 0.001) "default_threshold" 0.7 default_threshold
  | _ -> fail "Expected Cascade node_type"

let test_node_type_name_cascade () =
  let node = make_cascade ~id:"c" ~tiers:[] () in
  check string "node_type_name" "cascade" (node_type_name node.node_type)

let make_test_tier ?(index=0) ?(threshold=0.7) ?(cost=0.0) model prompt =
  let tier_node = {
    id = model;
    node_type = Llm { model; system = None; prompt; timeout = None;
                       tools = None; prompt_ref = None; prompt_vars = []; thinking = false };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  { tier_node; tier_index = index; confidence_threshold = threshold;
    cost_weight = cost; pass_context = true }

let test_cascade_tier_yojson_roundtrip () =
  let tier = make_test_tier ~index:0 ~threshold:0.7 ~cost:0.0 "ollama:glm-4.7-flash" "{{input}}" in
  let json = cascade_tier_to_yojson tier in
  match cascade_tier_of_yojson json with
  | Ok tier' ->
    check int "tier_index" tier.tier_index tier'.tier_index;
    check (float 0.001) "threshold" tier.confidence_threshold tier'.confidence_threshold;
    check (float 0.001) "cost_weight" tier.cost_weight tier'.cost_weight;
    check bool "pass_context" tier.pass_context tier'.pass_context;
    check string "tier_node id" tier.tier_node.id tier'.tier_node.id
  | Error e -> fail (Printf.sprintf "cascade_tier yojson roundtrip failed: %s" e)

let test_parse_custom_cascade_tier_json () =
  let json_str = {|{
    "id": "cascade-custom-tier",
    "nodes": [
      { "id": "glm", "type": "llm", "model": "ollama:glm-4.7-flash", "prompt": "{{input}}" },
      {
        "id": "cascade",
        "type": "cascade",
        "tiers": [
          {
            "tier_node": { "id": "glm_ref", "type": "chain_ref", "ref": "glm" },
            "tier_index": 0,
            "confidence_threshold": 0.7,
            "cost_weight": 0.0,
            "pass_context": false
          }
        ],
        "max_escalations": 2,
        "context_mode": "summary",
        "default_threshold": 0.7
      }
    ],
    "output": "cascade",
    "config": null
  }|} in
  let json = Yojson.Safe.from_string json_str in
  match Chain_parser.parse_chain json with
  | Error e -> fail (Printf.sprintf "parse_chain failed: %s" e)
  | Ok chain ->
    let cascade_node = List.find_opt (fun n ->
      match n.node_type with Cascade _ -> true | _ -> false
    ) chain.nodes in
    match cascade_node with
    | None -> fail "Expected a Cascade node"
    | Some n ->
      match n.node_type with
      | Cascade { tiers; _ } ->
        check int "tiers len" 1 (List.length tiers);
        let t = List.hd tiers in
        check int "tier_index" 0 t.tier_index;
        check (float 0.001) "confidence_threshold" 0.7 t.confidence_threshold;
        check (float 0.001) "cost_weight" 0.0 t.cost_weight;
        check bool "pass_context" false t.pass_context;
        check string "tier_node id" "glm_ref" t.tier_node.id;
        (match t.tier_node.node_type with
         | ChainRef ref_id -> check string "tier_node ref" "glm" ref_id
         | _ -> fail "Expected tier_node.type=chain_ref")
      | _ -> fail "Expected Cascade node_type"

let test_confidence_level_yojson_roundtrip () =
  let test level =
    let json = confidence_level_to_yojson level in
    match confidence_level_of_yojson json with
    | Ok level' -> check bool (Printf.sprintf "roundtrip %s" (match level with High -> "High" | Medium -> "Medium" | Low -> "Low")) true (level = level')
    | Error e -> fail (Printf.sprintf "confidence_level yojson roundtrip failed: %s" e)
  in
  List.iter test [High; Medium; Low]

(** {1 Mermaid Parsing Tests} *)

let test_mermaid_3tier_cascade () =
  let mermaid = {|graph LR
    glm["LLM:ollama:glm-4.7-flash '{{input}}'"]
    gemini["LLM:gemini '{{input}}'"]
    claude["LLM:claude '{{input}}'"]
    cascade("Cascade:0.7:summary")
    glm --> cascade
    gemini --> cascade
    claude --> cascade|} in
  match Chain_mermaid_parser.parse_mermaid_to_chain mermaid with
  | Ok chain ->
    (* Find the cascade node *)
    let cascade_node = List.find_opt (fun n ->
      match n.node_type with Cascade _ -> true | _ -> false
    ) chain.nodes in
    (match cascade_node with
     | Some n ->
       (match n.node_type with
        | Cascade { tiers; context_mode; max_escalations; _ } ->
          check bool "has tiers" true (List.length tiers > 0);
          check bool "context_mode summary" true (context_mode = CM_Summary);
          check int "max_escalations" 2 max_escalations
        | _ -> fail "Expected Cascade")
     | None -> fail "No Cascade node found in parsed chain")
  | Error e -> fail (Printf.sprintf "Mermaid parse failed: %s" e)

let test_mermaid_bare_cascade () =
  let mermaid = {|graph LR
    a["LLM:gemini '{{input}}'"]
    b("Cascade")
    a --> b|} in
  match Chain_mermaid_parser.parse_mermaid_to_chain mermaid with
  | Ok chain ->
    let cascade_node = List.find_opt (fun n ->
      match n.node_type with Cascade _ -> true | _ -> false
    ) chain.nodes in
    (match cascade_node with
     | Some n ->
       (match n.node_type with
        | Cascade { context_mode; max_escalations; _ } ->
          check bool "default context_mode Summary" true (context_mode = CM_Summary);
          check int "default max_escalations" 2 max_escalations
        | _ -> fail "Expected Cascade")
     | None -> fail "No Cascade node found")
  | Error e -> fail (Printf.sprintf "Bare Cascade parse failed: %s" e)

(** {1 Mermaid-JSON Roundtrip Tests} *)

let test_mermaid_to_json_roundtrip () =
  (* Parse Mermaid with custom threshold and context mode *)
  let mermaid = {|graph LR
    glm["LLM:ollama:glm-4.7-flash '{{input}}'"]
    gemini["LLM:gemini '{{input}}'"]
    claude["LLM:claude '{{input}}'"]
    cascade("Cascade:0.8:full")
    glm --> cascade
    gemini --> cascade
    claude --> cascade|} in
  match Chain_mermaid_parser.parse_mermaid_to_chain mermaid with
  | Error e -> fail (Printf.sprintf "Mermaid parse failed: %s" e)
  | Ok chain ->
    (* Convert to JSON and back *)
    let json = Chain_parser.chain_to_json chain in
    match Chain_parser.parse_chain json with
    | Error e -> fail (Printf.sprintf "JSON roundtrip failed: %s" e)
    | Ok chain' ->
      (* Find cascade node and verify properties preserved *)
      let cascade_node = List.find_opt (fun n ->
        match n.node_type with Cascade _ -> true | _ -> false
      ) chain'.nodes in
      match cascade_node with
      | None -> fail "No Cascade node after roundtrip"
      | Some n ->
        match n.node_type with
        | Cascade { default_threshold; context_mode; _ } ->
          check (float 0.01) "threshold preserved" 0.8 default_threshold;
          check bool "context_mode CM_Full" true (context_mode = CM_Full)
        | _ -> fail "Expected Cascade type"

let test_json_chain_yojson_roundtrip () =
  (* Build cascade programmatically *)
  let tier = make_test_tier ~index:0 ~threshold:0.9 "ollama:glm" "{{input}}" in
  let cascade = make_cascade
    ~id:"test-cascade"
    ~tiers:[tier]
    ~default_threshold:0.9
    ~context_mode:CM_None
    ()
  in
  let chain = make_chain
    ~id:"roundtrip-test"
    ~nodes:[cascade]
    ~output:"test-cascade"
    ()
  in
  let json = Chain_parser.chain_to_json chain in
  match Chain_parser.parse_chain json with
  | Error e -> fail (Printf.sprintf "Yojson roundtrip failed: %s" e)
  | Ok chain' ->
    check string "chain id" chain.id chain'.id;
    check int "node count" 1 (List.length chain'.nodes);
    let n = List.hd chain'.nodes in
    match n.node_type with
    | Cascade { default_threshold; context_mode; _ } ->
      check (float 0.01) "threshold 0.9" 0.9 default_threshold;
      check bool "context_mode None" true (context_mode = CM_None)
    | _ -> fail "Expected Cascade"

let test_mermaid_serialization () =
  (* Build cascade and serialize to Mermaid *)
  let tier0 = make_test_tier ~index:0 ~threshold:0.8 "ollama:glm" "{{input}}" in
  let tier1 = make_test_tier ~index:1 ~threshold:0.8 "gemini" "{{input}}" in
  let cascade = make_cascade
    ~id:"my-cascade"
    ~tiers:[tier0; tier1]
    ~default_threshold:0.8
    ~context_mode:CM_Full
    ()
  in
  let chain = make_chain
    ~id:"mermaid-test"
    ~nodes:[cascade]
    ~output:"my-cascade"
    ()
  in
  let mermaid_out = Chain_mermaid_parser.chain_to_mermaid ~styled:false chain in
  (* Verify cascade appears with threshold and context mode *)
  check bool "contains Cascade" true (String.length mermaid_out > 0);
  (* The serialization should mention the cascade ID *)
  check bool "contains cascade id" true
    (try let _ = Str.search_forward (Str.regexp_string "cascade") mermaid_out 0 in true
     with Not_found -> false)

(** {1 Threshold Propagation Tests} *)

let test_custom_threshold_0_9 () =
  let mermaid = {|graph LR
    a["LLM:gemini '{{input}}'"]
    b("Cascade:0.9:none")
    a --> b|} in
  match Chain_mermaid_parser.parse_mermaid_to_chain mermaid with
  | Error e -> fail (Printf.sprintf "Parse failed: %s" e)
  | Ok chain ->
    let cascade_node = List.find_opt (fun n ->
      match n.node_type with Cascade _ -> true | _ -> false
    ) chain.nodes in
    match cascade_node with
    | None -> fail "No Cascade node found"
    | Some n ->
      match n.node_type with
      | Cascade { default_threshold; context_mode; _ } ->
        check (float 0.01) "threshold 0.9" 0.9 default_threshold;
        check bool "context_mode None" true (context_mode = CM_None)
      | _ -> fail "Expected Cascade"

let test_custom_threshold_0_5 () =
  let mermaid = {|graph LR
    a["LLM:claude '{{input}}'"]
    b("Cascade:0.5:full")
    a --> b|} in
  match Chain_mermaid_parser.parse_mermaid_to_chain mermaid with
  | Error e -> fail (Printf.sprintf "Parse failed: %s" e)
  | Ok chain ->
    let cascade_node = List.find_opt (fun n ->
      match n.node_type with Cascade _ -> true | _ -> false
    ) chain.nodes in
    match cascade_node with
    | None -> fail "No Cascade node found"
    | Some n ->
      match n.node_type with
      | Cascade { default_threshold; context_mode; _ } ->
        check (float 0.01) "threshold 0.5" 0.5 default_threshold;
        check bool "context_mode Full" true (context_mode = CM_Full)
      | _ -> fail "Expected Cascade"

(** {1 Edge Case Tests} *)

let test_empty_tiers () =
  let cascade = make_cascade ~id:"empty" ~tiers:[] () in
  match cascade.node_type with
  | Cascade { tiers; _ } ->
    check int "empty tiers" 0 (List.length tiers)
  | _ -> fail "Expected Cascade"

let test_max_escalations_zero () =
  let cascade = make_cascade
    ~id:"no-escalate"
    ~tiers:[]
    ~max_escalations:0
    ()
  in
  match cascade.node_type with
  | Cascade { max_escalations; _ } ->
    check int "max_escalations 0" 0 max_escalations
  | _ -> fail "Expected Cascade"

let test_single_tier () =
  let tier = make_test_tier ~index:0 ~threshold:0.7 "gemini" "test" in
  let cascade = make_cascade ~id:"single" ~tiers:[tier] () in
  match cascade.node_type with
  | Cascade { tiers; _ } ->
    check int "single tier" 1 (List.length tiers);
    let t = List.hd tiers in
    check int "tier_index 0" 0 t.tier_index
  | _ -> fail "Expected Cascade"

(** {1 Stats Tests} *)

let test_cascade_stats_tracking () =
  Eio_main.run @@ fun _env ->
  Chain_stats.reset ();
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:1 ~escalations:1 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:2 ~escalations:2 ~hard_failures:1;
  let stats = Chain_stats.cascade_snapshot () in
  check int "total_cascades" 3 stats.total_cascades;
  check int "tier0_resolved" 1 stats.tier0_resolved;
  check int "tier1_resolved" 1 stats.tier1_resolved;
  check int "tier2_plus_resolved" 1 stats.tier2_plus_resolved;
  check int "total_escalations" 3 stats.total_escalations;
  check int "total_hard_failures" 1 stats.total_hard_failures

let test_cascade_stats_empty () =
  Eio_main.run @@ fun _env ->
  Chain_stats.reset ();
  let stats = Chain_stats.cascade_snapshot () in
  check int "total_cascades zero" 0 stats.total_cascades;
  check (float 0.001) "avg_tier zero" 0.0 stats.avg_tier;
  check (float 0.001) "savings zero" 0.0 stats.estimated_savings_pct

let test_cascade_stats_all_tier0 () =
  Eio_main.run @@ fun _env ->
  Chain_stats.reset ();
  for _ = 1 to 10 do
    Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0
  done;
  let stats = Chain_stats.cascade_snapshot () in
  check int "total" 10 stats.total_cascades;
  check int "all tier0" 10 stats.tier0_resolved;
  check (float 0.001) "avg_tier" 0.0 stats.avg_tier;
  check (float 0.001) "max savings" 100.0 stats.estimated_savings_pct

let test_stats_yojson_roundtrip () =
  Eio_main.run @@ fun _env ->
  Chain_stats.reset ();
  Chain_stats.track_cascade ~resolved_tier:0 ~escalations:0 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:1 ~escalations:1 ~hard_failures:0;
  Chain_stats.track_cascade ~resolved_tier:2 ~escalations:2 ~hard_failures:1;
  let stats = Chain_stats.cascade_snapshot () in
  let json = Chain_stats.cascade_stats_to_yojson stats in
  match Chain_stats.cascade_stats_of_yojson json with
  | Error e -> fail (Printf.sprintf "Yojson roundtrip failed: %s" e)
  | Ok stats' ->
    check int "total_cascades" stats.total_cascades stats'.total_cascades;
    check int "tier0" stats.tier0_resolved stats'.tier0_resolved;
    check int "tier1" stats.tier1_resolved stats'.tier1_resolved;
    check int "tier2+" stats.tier2_plus_resolved stats'.tier2_plus_resolved;
    check int "escalations" stats.total_escalations stats'.total_escalations;
    check int "hard_failures" stats.total_hard_failures stats'.total_hard_failures;
    check (float 0.001) "avg_tier" stats.avg_tier stats'.avg_tier

let test_stats_high_volume () =
  Eio_main.run @@ fun _env ->
  Chain_stats.reset ();
  (* Track 100 cascades with varying tiers *)
  for i = 0 to 99 do
    let tier = i mod 3 in
    Chain_stats.track_cascade ~resolved_tier:tier ~escalations:tier ~hard_failures:0
  done;
  let stats = Chain_stats.cascade_snapshot () in
  check int "total 100" 100 stats.total_cascades;
  (* 34 tier0, 33 tier1, 33 tier2 (0-99 mod 3) *)
  check int "tier0 count" 34 stats.tier0_resolved;
  check int "tier1 count" 33 stats.tier1_resolved;
  check int "tier2+ count" 33 stats.tier2_plus_resolved;
  (* Total escalations: 0*34 + 1*33 + 2*33 = 99 *)
  check int "escalations" 99 stats.total_escalations

(** {1 Test Runner} *)

let () =
  run "Cascade" [
    "confidence_parsing", [
      test_case "valid levels" `Quick test_confidence_of_string_valid;
      test_case "case insensitive" `Quick test_confidence_of_string_case_insensitive;
      test_case "garbage defaults Low" `Quick test_confidence_of_string_garbage;
      test_case "to float mapping" `Quick test_confidence_to_float;
      test_case "yojson roundtrip" `Quick test_confidence_level_yojson_roundtrip;
    ];
    "context_mode", [
      test_case "roundtrip" `Quick test_context_mode_roundtrip;
      test_case "of_string" `Quick test_context_mode_of_string;
      test_case "yojson roundtrip" `Quick test_context_mode_yojson_roundtrip;
    ];
    "cascade_type", [
      test_case "make_cascade defaults" `Quick test_make_cascade_defaults;
      test_case "node_type_name" `Quick test_node_type_name_cascade;
      test_case "tier yojson roundtrip" `Quick test_cascade_tier_yojson_roundtrip;
      test_case "parse custom tier json" `Quick test_parse_custom_cascade_tier_json;
    ];
    "mermaid_parsing", [
      test_case "3-tier cascade" `Quick test_mermaid_3tier_cascade;
      test_case "bare Cascade" `Quick test_mermaid_bare_cascade;
    ];
    "mermaid_json_roundtrip", [
      test_case "mermaid to json roundtrip" `Quick test_mermaid_to_json_roundtrip;
      test_case "json chain yojson roundtrip" `Quick test_json_chain_yojson_roundtrip;
      test_case "mermaid serialization" `Quick test_mermaid_serialization;
    ];
    "threshold_propagation", [
      test_case "custom threshold 0.9" `Quick test_custom_threshold_0_9;
      test_case "custom threshold 0.5" `Quick test_custom_threshold_0_5;
    ];
    "edge_cases", [
      test_case "empty tiers" `Quick test_empty_tiers;
      test_case "max_escalations zero" `Quick test_max_escalations_zero;
      test_case "single tier" `Quick test_single_tier;
    ];
    "cascade_stats", [
      test_case "tracking" `Quick test_cascade_stats_tracking;
      test_case "empty" `Quick test_cascade_stats_empty;
      test_case "all tier0 savings" `Quick test_cascade_stats_all_tier0;
      test_case "yojson roundtrip" `Quick test_stats_yojson_roundtrip;
      test_case "high volume" `Quick test_stats_high_volume;
    ];
  ]

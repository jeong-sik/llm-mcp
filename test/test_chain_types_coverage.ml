(** Tests for Chain_types module — Additional coverage for uncovered branches
    Targets: consensus_mode, confidence_level, context_mode, merge_strategy,
    threshold_op, select_strategy, backoff_strategy, adapter_transform,
    mcts_policy, count_parallel_groups, batch types, make_* helpers *)

open Alcotest
open Chain_types

(** {1 Consensus Mode} *)

let test_consensus_mode_to_string () =
  check string "count" "count:3" (consensus_mode_to_string (Count 3));
  check string "majority" "majority" (consensus_mode_to_string Majority);
  check string "unanimous" "unanimous" (consensus_mode_to_string Unanimous);
  check string "weighted" "weighted:0.75" (consensus_mode_to_string (Weighted 0.75))

let test_consensus_mode_of_string () =
  check bool "majority" true (consensus_mode_of_string "majority" = Majority);
  check bool "unanimous" true (consensus_mode_of_string "unanimous" = Unanimous);
  check bool "count" true (consensus_mode_of_string "3" = Count 3);
  check bool "count fallback" true (consensus_mode_of_string "not_a_number" = Count 1)

let test_consensus_mode_of_string_weighted () =
  match consensus_mode_of_string "weighted:0.8" with
  | Weighted t -> check (float 0.01) "threshold" 0.8 t
  | _ -> fail "expected Weighted"

let test_consensus_mode_of_string_weighted_invalid () =
  match consensus_mode_of_string "weighted:abc" with
  | Weighted t -> check (float 0.01) "fallback threshold" 0.5 t
  | _ -> fail "expected Weighted"

let test_consensus_mode_of_string_trim_case () =
  check bool "case insensitive" true
    (consensus_mode_of_string " MAJORITY " = Majority);
  check bool "case insensitive unanimous" true
    (consensus_mode_of_string " Unanimous " = Unanimous)

(** {1 Confidence Level} *)

let test_confidence_to_float () =
  check (float 0.01) "high" 1.0 (confidence_to_float High);
  check (float 0.01) "medium" 0.5 (confidence_to_float Medium);
  check (float 0.01) "low" 0.2 (confidence_to_float Low)

let test_confidence_of_string () =
  check bool "high" true (confidence_of_string "high" = High);
  check bool "High" true (confidence_of_string "High" = High);
  check bool "medium" true (confidence_of_string "medium" = Medium);
  check bool "MEDIUM" true (confidence_of_string "MEDIUM" = Medium);
  check bool "low" true (confidence_of_string "low" = Low);
  check bool "unknown defaults to low" true (confidence_of_string "garbage" = Low)

(** {1 Context Mode} *)

let test_context_mode_to_string () =
  check string "none" "none" (context_mode_to_string CM_None);
  check string "summary" "summary" (context_mode_to_string CM_Summary);
  check string "full" "full" (context_mode_to_string CM_Full)

let test_context_mode_of_string () =
  check bool "none" true (context_mode_of_string "none" = CM_None);
  check bool "full" true (context_mode_of_string "full" = CM_Full);
  check bool "summary" true (context_mode_of_string "summary" = CM_Summary);
  check bool "default" true (context_mode_of_string "anything" = CM_Summary)

let test_context_mode_roundtrip () =
  let modes = [CM_None; CM_Summary; CM_Full] in
  List.iter (fun m ->
    let s = context_mode_to_string m in
    let m' = context_mode_of_string s in
    check bool (Printf.sprintf "roundtrip %s" s) true (m = m')
  ) modes

(** {1 Additional node_type_name Tests} *)

let dummy = { id = "d"; node_type = Pipeline []; input_mapping = [];
              output_key = None; depends_on = None }

let test_node_type_name_gate () =
  check string "gate" "gate"
    (node_type_name (Gate { condition = "x"; then_node = dummy; else_node = None }))

let test_node_type_name_subgraph () =
  let chain = make_chain ~id:"sub" ~nodes:[] ~output:"n" () in
  check string "subgraph" "subgraph" (node_type_name (Subgraph chain))

let test_node_type_name_chain_ref () =
  check string "chain_ref" "chain_ref" (node_type_name (ChainRef "ref1"))

let test_node_type_name_map () =
  check string "map" "map" (node_type_name (Map { func = "f"; inner = dummy }))

let test_node_type_name_bind () =
  check string "bind" "bind" (node_type_name (Bind { func = "f"; inner = dummy }))

let test_node_type_name_goal_driven () =
  check string "goal_driven" "goal_driven"
    (node_type_name (GoalDriven {
      goal_metric = "score"; goal_operator = Gte; goal_value = 0.8;
      action_node = dummy; measure_func = "test"; max_iterations = 5;
      strategy_hints = []; conversational = false; relay_models = []
    }))

let test_node_type_name_evaluator () =
  check string "evaluator" "evaluator"
    (node_type_name (Evaluator {
      candidates = []; scoring_func = "llm_judge"; scoring_prompt = None;
      select_strategy = Best; min_score = None
    }))

let test_node_type_name_chain_exec () =
  check string "chain_exec" "chain_exec"
    (node_type_name (ChainExec {
      chain_source = "src"; validate = true; max_depth = 3;
      sandbox = false; context_inject = []; pass_outputs = true
    }))

let test_node_type_name_batch () =
  check string "batch" "batch"
    (node_type_name (Batch { batch_size = 10; parallel = true; inner = dummy;
                             collect_strategy = `List }))

let test_node_type_name_spawn () =
  check string "spawn" "spawn"
    (node_type_name (Spawn { clean = true; inner = dummy; pass_vars = [];
                             inherit_cache = true }))

let test_node_type_name_stream_merge () =
  check string "stream_merge" "stream_merge"
    (node_type_name (StreamMerge { nodes = []; reducer = First; initial = "";
                                   min_results = None; timeout = None }))

let test_node_type_name_feedback_loop () =
  check string "feedback_loop" "feedback_loop"
    (node_type_name (FeedbackLoop {
      generator = dummy;
      evaluator_config = { scoring_func = "test"; scoring_prompt = None;
                          select_strategy = Best };
      improver_prompt = "improve"; max_iterations = 3;
      score_threshold = 0.8; score_operator = Gte;
      conversational = false; relay_models = []
    }))

let test_node_type_name_cascade () =
  check string "cascade" "cascade"
    (node_type_name (Cascade {
      tiers = []; confidence_prompt = None; max_escalations = 2;
      context_mode = CM_Summary; task_hint = None; default_threshold = 0.7
    }))

(** {1 Make Helper Tests — Additional} *)

let test_make_goal_driven () =
  let node = make_goal_driven ~id:"gd1" ~goal_metric:"coverage"
    ~goal_operator:Gte ~goal_value:0.8 ~action_node:dummy
    ~measure_func:"exec_test" ~max_iterations:10 () in
  check string "id" "gd1" node.id;
  match node.node_type with
  | GoalDriven { goal_metric; max_iterations; conversational; relay_models; _ } ->
    check string "metric" "coverage" goal_metric;
    check int "max_iter" 10 max_iterations;
    check bool "not conversational" false conversational;
    check int "no relay models" 0 (List.length relay_models)
  | _ -> fail "expected GoalDriven"

let test_make_goal_driven_full () =
  let node = make_goal_driven ~id:"gd2" ~goal_metric:"score"
    ~goal_operator:Gt ~goal_value:0.9 ~action_node:dummy
    ~measure_func:"call_api" ~max_iterations:5
    ~strategy_hints:[("below_50", "fast")]
    ~conversational:true ~relay_models:["gemini"; "claude"] () in
  match node.node_type with
  | GoalDriven { strategy_hints; conversational; relay_models; _ } ->
    check int "hints" 1 (List.length strategy_hints);
    check bool "conversational" true conversational;
    check int "relay_models" 2 (List.length relay_models)
  | _ -> fail "expected GoalDriven"

let test_make_evaluator () =
  let node = make_evaluator ~id:"ev1" ~candidates:[dummy]
    ~scoring_func:"anti_fake" ~select_strategy:Best ~min_score:0.7 () in
  match node.node_type with
  | Evaluator { scoring_func; select_strategy; min_score; _ } ->
    check string "scoring_func" "anti_fake" scoring_func;
    check bool "best strategy" true (select_strategy = Best);
    check bool "min_score" true (min_score = Some 0.7)
  | _ -> fail "expected Evaluator"

let test_make_retry () =
  let node = make_retry ~id:"r1" ~node:dummy ~max_attempts:3
    ~backoff:(Constant 2.0) ~retry_on:["timeout"] () in
  match node.node_type with
  | Retry { max_attempts; backoff; retry_on; _ } ->
    check int "attempts" 3 max_attempts;
    check bool "constant backoff" true (backoff = Constant 2.0);
    check int "retry_on" 1 (List.length retry_on)
  | _ -> fail "expected Retry"

let test_make_fallback () =
  let node = make_fallback ~id:"fb1" ~primary:dummy ~fallbacks:[dummy] in
  match node.node_type with
  | Fallback { fallbacks; _ } ->
    check int "fallbacks" 1 (List.length fallbacks)
  | _ -> fail "expected Fallback"

let test_make_race () =
  let node = make_race ~id:"race1" ~nodes:[dummy; dummy] ~timeout:10.0 () in
  match node.node_type with
  | Race { nodes; timeout } ->
    check int "nodes" 2 (List.length nodes);
    check bool "timeout" true (timeout = Some 10.0)
  | _ -> fail "expected Race"

let test_make_feedback_loop () =
  let eval_cfg = { scoring_func = "llm_judge"; scoring_prompt = Some "Score it";
                   select_strategy = Best } in
  let node = make_feedback_loop ~id:"fl1" ~generator:dummy ~evaluator_config:eval_cfg
    ~improver_prompt:"Fix: {{feedback}}" ~max_iterations:5 ~score_threshold:0.8 () in
  match node.node_type with
  | FeedbackLoop { max_iterations; score_threshold; score_operator; conversational; relay_models; _ } ->
    check int "max_iter" 5 max_iterations;
    check (float 0.01) "threshold" 0.8 score_threshold;
    check bool "operator Gte" true (score_operator = Gte);
    check bool "not conversational" false conversational;
    check int "no relay" 0 (List.length relay_models)
  | _ -> fail "expected FeedbackLoop"

let test_make_cascade () =
  let tier = {
    tier_node = dummy; tier_index = 0;
    confidence_threshold = 0.7; cost_weight = 1.0; pass_context = true
  } in
  let node = make_cascade ~id:"cas1" ~tiers:[tier] () in
  match node.node_type with
  | Cascade { tiers; context_mode; max_escalations; default_threshold; _ } ->
    check int "tiers" 1 (List.length tiers);
    check bool "summary mode" true (context_mode = CM_Summary);
    check int "max_escalations" 2 max_escalations;
    check (float 0.01) "default_threshold" 0.7 default_threshold
  | _ -> fail "expected Cascade"

(** {1 Count Parallel Groups} *)

let test_count_parallel_groups_llm () =
  let node = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  check int "llm has 0 groups" 0 (count_parallel_groups node)

let test_count_parallel_groups_fanout () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let n2 = make_llm_node ~id:"n2" ~model:"claude" ~prompt:"hi" () in
  let node = make_fanout ~id:"f" [n1; n2] in
  check int "fanout = 1" 1 (count_parallel_groups node)

let test_count_parallel_groups_nested () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let n2 = make_llm_node ~id:"n2" ~model:"claude" ~prompt:"hi" () in
  let fanout = make_fanout ~id:"f" [n1; n2] in
  let pipeline = make_pipeline ~id:"p" [fanout] in
  check int "nested fanout in pipeline" 1 (count_parallel_groups pipeline)

let test_count_parallel_groups_quorum () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = make_quorum ~id:"q" [n1] in
  check int "quorum = 1" 1 (count_parallel_groups node)

let test_count_parallel_groups_merge () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = { id = "m"; node_type = Merge { strategy = Concat; nodes = [n1] };
               input_mapping = []; output_key = None; depends_on = None } in
  check int "merge = 1" 1 (count_parallel_groups node)

let test_count_parallel_groups_race () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = make_race ~id:"r" ~nodes:[n1] () in
  check int "race = 1" 1 (count_parallel_groups node)

let test_count_parallel_groups_gate () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let fanout = make_fanout ~id:"f" [n1] in
  let node = { id = "g"; node_type = Gate { condition = "x"; then_node = fanout;
               else_node = Some n1 }; input_mapping = []; output_key = None;
               depends_on = None } in
  check int "gate with fanout then" 1 (count_parallel_groups node)

let test_count_parallel_groups_batch_parallel () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = { id = "b"; node_type = Batch { batch_size = 5; parallel = true;
               inner = n1; collect_strategy = `List }; input_mapping = [];
               output_key = None; depends_on = None } in
  check int "parallel batch = 1" 1 (count_parallel_groups node)

let test_count_parallel_groups_batch_sequential () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let node = { id = "b"; node_type = Batch { batch_size = 5; parallel = false;
               inner = n1; collect_strategy = `List }; input_mapping = [];
               output_key = None; depends_on = None } in
  check int "sequential batch = 0" 0 (count_parallel_groups node)

let test_count_chain_parallel_groups () =
  let n1 = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"hi" () in
  let fanout = make_fanout ~id:"f" [n1; n1] in
  let chain = make_chain ~id:"c" ~nodes:[fanout; n1] ~output:"f" () in
  check int "chain parallel groups" 1 (count_chain_parallel_groups chain)

(** {1 Batch / Retry Config Defaults} *)

let test_default_batch_config () =
  check int "max_concurrent" 5 default_batch_config.batch_max_concurrent;
  check int "rate_limit" 60 default_batch_config.rate_limit_per_min;
  check bool "normal priority" true (default_batch_config.priority = Normal)

(** {1 Test Suite} *)

let () =
  run "chain_types_coverage" [
    ("consensus_mode", [
      test_case "to_string" `Quick test_consensus_mode_to_string;
      test_case "of_string" `Quick test_consensus_mode_of_string;
      test_case "weighted" `Quick test_consensus_mode_of_string_weighted;
      test_case "weighted invalid" `Quick test_consensus_mode_of_string_weighted_invalid;
      test_case "trim_case" `Quick test_consensus_mode_of_string_trim_case;
    ]);
    ("confidence", [
      test_case "to_float" `Quick test_confidence_to_float;
      test_case "of_string" `Quick test_confidence_of_string;
    ]);
    ("context_mode", [
      test_case "to_string" `Quick test_context_mode_to_string;
      test_case "of_string" `Quick test_context_mode_of_string;
      test_case "roundtrip" `Quick test_context_mode_roundtrip;
    ]);
    ("node_type_name_extra", [
      test_case "gate" `Quick test_node_type_name_gate;
      test_case "subgraph" `Quick test_node_type_name_subgraph;
      test_case "chain_ref" `Quick test_node_type_name_chain_ref;
      test_case "map" `Quick test_node_type_name_map;
      test_case "bind" `Quick test_node_type_name_bind;
      test_case "goal_driven" `Quick test_node_type_name_goal_driven;
      test_case "evaluator" `Quick test_node_type_name_evaluator;
      test_case "chain_exec" `Quick test_node_type_name_chain_exec;
      test_case "batch" `Quick test_node_type_name_batch;
      test_case "spawn" `Quick test_node_type_name_spawn;
      test_case "stream_merge" `Quick test_node_type_name_stream_merge;
      test_case "feedback_loop" `Quick test_node_type_name_feedback_loop;
      test_case "cascade" `Quick test_node_type_name_cascade;
    ]);
    ("make_helpers_extra", [
      test_case "goal_driven" `Quick test_make_goal_driven;
      test_case "goal_driven full" `Quick test_make_goal_driven_full;
      test_case "evaluator" `Quick test_make_evaluator;
      test_case "retry" `Quick test_make_retry;
      test_case "fallback" `Quick test_make_fallback;
      test_case "race" `Quick test_make_race;
      test_case "feedback_loop" `Quick test_make_feedback_loop;
      test_case "cascade" `Quick test_make_cascade;
    ]);
    ("count_parallel_groups", [
      test_case "llm" `Quick test_count_parallel_groups_llm;
      test_case "fanout" `Quick test_count_parallel_groups_fanout;
      test_case "nested" `Quick test_count_parallel_groups_nested;
      test_case "quorum" `Quick test_count_parallel_groups_quorum;
      test_case "merge" `Quick test_count_parallel_groups_merge;
      test_case "race" `Quick test_count_parallel_groups_race;
      test_case "gate" `Quick test_count_parallel_groups_gate;
      test_case "batch parallel" `Quick test_count_parallel_groups_batch_parallel;
      test_case "batch sequential" `Quick test_count_parallel_groups_batch_sequential;
      test_case "chain" `Quick test_count_chain_parallel_groups;
    ]);
    ("batch_defaults", [
      test_case "default_batch_config" `Quick test_default_batch_config;
    ]);
  ]

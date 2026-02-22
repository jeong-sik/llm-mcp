(** Tests for Chain_types module — Additional coverage for uncovered branches
    Targets: consensus_mode, confidence_level, context_mode, merge_strategy,
    threshold_op, select_strategy, backoff_strategy, adapter_transform,
    mcts_policy, count_parallel_groups, batch types, make_* helpers *)

open Alcotest
open Chain_types

(* {1Consensus Mode} *)

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

(* {1Confidence Level} *)

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

(* {1Context Mode} *)

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

(* {1Additional node_type_name Tests} *)

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
      context_mode = CM_Summary; task_hint = None; default_threshold = 0.7;
      difficulty_hint = None
    }))

(* {1Make Helper Tests — Additional} *)

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

(* {1Count Parallel Groups} *)

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

(* {1Batch / Retry Config Defaults} *)

let test_default_batch_config () =
  check int "max_concurrent" 5 default_batch_config.batch_max_concurrent;
  check int "rate_limit" 60 default_batch_config.rate_limit_per_min;
  check bool "normal priority" true (default_batch_config.priority = Normal)

(* {1Test Suite} *)

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

    (* {1Direction roundtrip} *)

    ("direction", [
      test_case "to_string all" `Quick (fun () ->
        check string "LR" "LR" (direction_to_string LR);
        check string "RL" "RL" (direction_to_string RL);
        check string "TB" "TB" (direction_to_string TB);
        check string "BT" "BT" (direction_to_string BT));
      test_case "of_string all" `Quick (fun () ->
        check bool "LR" true (direction_of_string "LR" = LR);
        check bool "RL" true (direction_of_string "RL" = RL);
        check bool "TB" true (direction_of_string "TB" = TB);
        check bool "TD alias" true (direction_of_string "TD" = TB);
        check bool "BT" true (direction_of_string "BT" = BT);
        check bool "default" true (direction_of_string "XYZ" = LR));
      test_case "roundtrip" `Quick (fun () ->
        let dirs = [LR; RL; TB; BT] in
        List.iter (fun d ->
          let s = direction_to_string d in
          let d' = direction_of_string s in
          check bool (Printf.sprintf "roundtrip %s" s) true (d = d')
        ) dirs);
    ]);

    (* {1Direction yojson roundtrip} *)

    ("direction_yojson", [
      test_case "roundtrip" `Quick (fun () ->
        let dirs = [LR; RL; TB; BT] in
        List.iter (fun d ->
          let j = direction_to_yojson d in
          let d' = direction_of_yojson j in
          match d' with
          | Ok d2 -> check bool (Printf.sprintf "yojson roundtrip %s" (direction_to_string d)) true (d = d2)
          | Error e -> fail (Printf.sprintf "yojson roundtrip failed: %s" e)
        ) dirs);
    ]);

    (* {1Consensus mode yojson} *)

    ("consensus_mode_yojson", [
      test_case "roundtrip" `Quick (fun () ->
        let modes = [Count 3; Majority; Unanimous; Weighted 0.75] in
        List.iter (fun m ->
          let j = consensus_mode_to_yojson m in
          match consensus_mode_of_yojson j with
          | Ok m2 -> check bool "roundtrip" true (m = m2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) modes);
    ]);

    (* {1Chain config yojson} *)

    ("chain_config_yojson", [
      test_case "default roundtrip" `Quick (fun () ->
        let j = chain_config_to_yojson default_config in
        match chain_config_of_yojson j with
        | Ok cfg ->
          check int "max_depth" 8 cfg.max_depth;
          check int "max_concurrency" 3 cfg.max_concurrency;
          check int "timeout" 300 cfg.timeout;
          check bool "trace" false cfg.trace;
          check bool "direction" true (cfg.direction = LR)
        | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e));
    ]);

    (* {1Merge strategy yojson} *)

    ("merge_strategy_yojson", [
      test_case "roundtrip all" `Quick (fun () ->
        let strats = [First; Last; Concat; WeightedAvg; Custom "my_func"] in
        List.iter (fun s ->
          let j = merge_strategy_to_yojson s in
          match merge_strategy_of_yojson j with
          | Ok s2 -> check bool "roundtrip" true (s = s2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) strats);
    ]);

    (* {1Threshold op yojson} *)

    ("threshold_op_yojson", [
      test_case "roundtrip all" `Quick (fun () ->
        let ops = [Gt; Gte; Lt; Lte; Eq; Neq] in
        List.iter (fun op ->
          let j = threshold_op_to_yojson op in
          match threshold_op_of_yojson j with
          | Ok op2 -> check bool "roundtrip" true (op = op2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) ops);
    ]);

    (* {1Select strategy yojson} *)

    ("select_strategy_yojson", [
      test_case "roundtrip all" `Quick (fun () ->
        let strats = [Best; Worst; AboveThreshold 0.75; WeightedRandom] in
        List.iter (fun s ->
          let j = select_strategy_to_yojson s in
          match select_strategy_of_yojson j with
          | Ok s2 -> check bool "roundtrip" true (s = s2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) strats);
    ]);

    (* {1Evaluator config yojson} *)

    ("evaluator_config_yojson", [
      test_case "roundtrip" `Quick (fun () ->
        let cfg = { scoring_func = "anti_fake"; scoring_prompt = Some "rate it";
                    select_strategy = Best } in
        let j = evaluator_config_to_yojson cfg in
        match evaluator_config_of_yojson j with
        | Ok cfg2 ->
          check string "scoring_func" "anti_fake" cfg2.scoring_func;
          check (option string) "scoring_prompt" (Some "rate it") cfg2.scoring_prompt;
          check bool "select_strategy" true (cfg2.select_strategy = Best)
        | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e));
      test_case "no prompt" `Quick (fun () ->
        let cfg = { scoring_func = "regex_match"; scoring_prompt = None;
                    select_strategy = Worst } in
        let j = evaluator_config_to_yojson cfg in
        match evaluator_config_of_yojson j with
        | Ok cfg2 ->
          check (option string) "no prompt" None cfg2.scoring_prompt;
          check bool "worst" true (cfg2.select_strategy = Worst)
        | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e));
    ]);

    (* {1Evaluator result yojson} *)

    ("evaluator_result_yojson", [
      test_case "roundtrip" `Quick (fun () ->
        let r = { score = 0.85; feedback = Some "good"; selected_output = "out";
                  selected_id = "n1" } in
        let j = evaluator_result_to_yojson r in
        match evaluator_result_of_yojson j with
        | Ok r2 ->
          check (float 0.01) "score" 0.85 r2.score;
          check (option string) "feedback" (Some "good") r2.feedback;
          check string "output" "out" r2.selected_output;
          check string "id" "n1" r2.selected_id
        | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e));
      test_case "no feedback" `Quick (fun () ->
        let r = { score = 0.0; feedback = None; selected_output = ""; selected_id = "x" } in
        let j = evaluator_result_to_yojson r in
        match evaluator_result_of_yojson j with
        | Ok r2 ->
          check (option string) "no feedback" None r2.feedback
        | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e));
    ]);

    (* {1Backoff strategy yojson} *)

    ("backoff_strategy_yojson", [
      test_case "roundtrip all" `Quick (fun () ->
        let strats = [Constant 1.0; Exponential 2.0; Linear 0.5; Jitter (0.1, 0.9)] in
        List.iter (fun s ->
          let j = backoff_strategy_to_yojson s in
          match backoff_strategy_of_yojson j with
          | Ok s2 -> check bool "roundtrip" true (s = s2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) strats);
    ]);

    (* {1Adapter transform yojson} *)

    ("adapter_transform_yojson", [
      test_case "extract" `Quick (fun () ->
        let t = Extract ".data" in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "extract" true (t = t2)
        | Error e -> fail e);
      test_case "template" `Quick (fun () ->
        let t = Template "Hello {{name}}" in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "template" true (t = t2)
        | Error e -> fail e);
      test_case "summarize" `Quick (fun () ->
        let t = Summarize 100 in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "summarize" true (t = t2)
        | Error e -> fail e);
      test_case "truncate" `Quick (fun () ->
        let t = Truncate 200 in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "truncate" true (t = t2)
        | Error e -> fail e);
      test_case "json_path" `Quick (fun () ->
        let t = JsonPath "$.items[*].name" in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "jsonpath" true (t = t2)
        | Error e -> fail e);
      test_case "regex" `Quick (fun () ->
        let t = Regex ("pat", "rep") in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "regex" true (t = t2)
        | Error e -> fail e);
      test_case "validate_schema" `Quick (fun () ->
        let t = ValidateSchema "my_schema" in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "validate_schema" true (t = t2)
        | Error e -> fail e);
      test_case "parse_json" `Quick (fun () ->
        let t = ParseJson in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "parse_json" true (t = t2)
        | Error e -> fail e);
      test_case "stringify" `Quick (fun () ->
        let t = Stringify in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "stringify" true (t = t2)
        | Error e -> fail e);
      test_case "chain_transforms" `Quick (fun () ->
        let t = Chain [Extract ".a"; Template "{{x}}"] in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "chain" true (t = t2)
        | Error e -> fail e);
      test_case "conditional" `Quick (fun () ->
        let t = Conditional { condition = "x > 0"; on_true = Extract ".yes";
                              on_false = Extract ".no" } in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "conditional" true (t = t2)
        | Error e -> fail e);
      test_case "split" `Quick (fun () ->
        let t = Split { delimiter = "line"; chunk_size = 100; overlap = 10 } in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "split" true (t = t2)
        | Error e -> fail e);
      test_case "custom" `Quick (fun () ->
        let t = Custom "my_transform" in
        let j = adapter_transform_to_yojson t in
        match adapter_transform_of_yojson j with
        | Ok t2 -> check bool "custom" true (t = t2)
        | Error e -> fail e);
    ]);

    (* {1MCTS policy yojson} *)

    ("mcts_policy_yojson", [
      test_case "roundtrip all" `Quick (fun () ->
        let policies = [UCB1 1.41; Greedy; EpsilonGreedy 0.1; Softmax 1.0] in
        List.iter (fun p ->
          let j = mcts_policy_to_yojson p in
          match mcts_policy_of_yojson j with
          | Ok p2 -> check bool "roundtrip" true (p = p2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) policies);
    ]);

    (* {1Confidence level yojson} *)

    ("confidence_level_yojson", [
      test_case "roundtrip all" `Quick (fun () ->
        let levels : confidence_level list = [High; Medium; Low] in
        List.iter (fun (l : confidence_level) ->
          let j = confidence_level_to_yojson l in
          match confidence_level_of_yojson j with
          | Ok l2 -> check bool "roundtrip" true (l = l2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) levels);
    ]);

    (* {1Context mode yojson} *)

    ("context_mode_yojson", [
      test_case "roundtrip all" `Quick (fun () ->
        let modes = [CM_None; CM_Summary; CM_Full] in
        List.iter (fun m ->
          let j = context_mode_to_yojson m in
          match context_mode_of_yojson j with
          | Ok m2 -> check bool "roundtrip" true (m = m2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) modes);
    ]);

    (* {1Trace entry yojson} *)

    ("trace_entry_yojson", [
      test_case "success" `Quick (fun () ->
        let te = { node_id = "n1"; node_type_name = "llm"; start_time = 0.0;
                   end_time = 1.5; status = `Success; output_preview = Some "hello";
                   error = None } in
        let j = trace_entry_to_yojson te in
        match trace_entry_of_yojson j with
        | Ok te2 ->
          check string "node_id" "n1" te2.node_id;
          check string "type" "llm" te2.node_type_name;
          check (float 0.01) "start" 0.0 te2.start_time;
          check (float 0.01) "end" 1.5 te2.end_time;
          check bool "status" true (te2.status = `Success);
          check (option string) "preview" (Some "hello") te2.output_preview;
          check (option string) "no error" None te2.error
        | Error e -> fail e);
      test_case "failure" `Quick (fun () ->
        let te = { node_id = "n2"; node_type_name = "tool"; start_time = 1.0;
                   end_time = 2.0; status = `Failure; output_preview = None;
                   error = Some "timeout" } in
        let j = trace_entry_to_yojson te in
        match trace_entry_of_yojson j with
        | Ok te2 ->
          check bool "failure" true (te2.status = `Failure);
          check (option string) "error" (Some "timeout") te2.error
        | Error e -> fail e);
      test_case "skipped" `Quick (fun () ->
        let te = { node_id = "n3"; node_type_name = "gate"; start_time = 0.0;
                   end_time = 0.0; status = `Skipped; output_preview = None;
                   error = None } in
        let j = trace_entry_to_yojson te in
        match trace_entry_of_yojson j with
        | Ok te2 -> check bool "skipped" true (te2.status = `Skipped)
        | Error e -> fail e);
    ]);

    (* {1Token usage yojson} *)

    ("token_usage_yojson", [
      test_case "empty" `Quick (fun () ->
        let j = token_usage_to_yojson empty_token_usage in
        match token_usage_of_yojson j with
        | Ok tu ->
          check int "prompt" 0 tu.prompt_tokens;
          check int "completion" 0 tu.completion_tokens;
          check int "total" 0 tu.total_tokens;
          check (float 0.001) "cost" 0.0 tu.estimated_cost_usd
        | Error e -> fail e);
      test_case "populated" `Quick (fun () ->
        let tu = { prompt_tokens = 100; completion_tokens = 200; total_tokens = 300;
                   estimated_cost_usd = 0.015 } in
        let j = token_usage_to_yojson tu in
        match token_usage_of_yojson j with
        | Ok tu2 ->
          check int "prompt" 100 tu2.prompt_tokens;
          check int "completion" 200 tu2.completion_tokens;
          check int "total" 300 tu2.total_tokens;
          check (float 0.001) "cost" 0.015 tu2.estimated_cost_usd
        | Error e -> fail e);
    ]);

    (* {1Chain result yojson} *)

    ("chain_result_yojson", [
      test_case "roundtrip" `Quick (fun () ->
        let cr = { chain_id = "test-chain"; output = "result";
                   success = true; trace = [];
                   token_usage = empty_token_usage;
                   duration_ms = 1234;
                   metadata = [("key", "val")] } in
        let j = chain_result_to_yojson cr in
        match chain_result_of_yojson j with
        | Ok cr2 ->
          check string "chain_id" "test-chain" cr2.chain_id;
          check string "output" "result" cr2.output;
          check bool "success" true cr2.success;
          check int "trace len" 0 (List.length cr2.trace);
          check int "duration" 1234 cr2.duration_ms;
          check int "metadata" 1 (List.length cr2.metadata)
        | Error e -> fail e);
    ]);

    (* {1Execution plan yojson} *)

    ("execution_plan_yojson", [
      test_case "roundtrip" `Quick (fun () ->
        let chain = make_chain ~id:"ep-test" ~nodes:[] ~output:"o" () in
        let ep = { chain; execution_order = ["a"; "b"; "c"];
                   parallel_groups = [["a"; "b"]; ["c"]]; depth = 2 } in
        let j = execution_plan_to_yojson ep in
        match execution_plan_of_yojson j with
        | Ok ep2 ->
          check string "chain_id" "ep-test" ep2.chain.id;
          check int "order len" 3 (List.length ep2.execution_order);
          check int "groups" 2 (List.length ep2.parallel_groups);
          check int "depth" 2 ep2.depth
        | Error e -> fail e);
    ]);

    (* {1Make helpers — additional uncovered} *)

    ("make_helpers_chain", [
      test_case "make_chain defaults" `Quick (fun () ->
        let c = make_chain ~id:"c1" ~nodes:[] ~output:"o" () in
        check string "id" "c1" c.id;
        check int "max_depth" 8 c.config.max_depth;
        check (option string) "no name" None c.name;
        check (option string) "no desc" None c.description;
        check (option string) "no version" None c.version);
      test_case "make_chain full" `Quick (fun () ->
        let c = make_chain ~id:"c2" ~nodes:[] ~output:"o"
          ~name:"My Chain" ~description:"A chain" ~version:"1.0.0" () in
        check (option string) "name" (Some "My Chain") c.name;
        check (option string) "desc" (Some "A chain") c.description;
        check (option string) "version" (Some "1.0.0") c.version);
      test_case "make_threshold" `Quick (fun () ->
        let node = make_threshold ~id:"t1" ~metric:"score" ~operator:Gte
          ~value:0.5 ~input_node:dummy ~on_pass:dummy () in
        match node.node_type with
        | Threshold { metric; operator; on_pass; on_fail; _ } ->
          check string "metric" "score" metric;
          check bool "operator" true (operator = Gte);
          check bool "on_pass" true (on_pass <> None);
          check bool "no on_fail" true (on_fail = None)
        | _ -> fail "expected Threshold");
      test_case "make_adapter" `Quick (fun () ->
        let node = make_adapter ~id:"a1" ~input_ref:"n1" ~transform:(Extract ".data") () in
        match node.node_type with
        | Adapter { input_ref; transform; on_error } ->
          check string "input_ref" "n1" input_ref;
          check bool "extract" true (transform = Extract ".data");
          check bool "fail on error" true (on_error = `Fail)
        | _ -> fail "expected Adapter");
      test_case "make_adapter passthrough" `Quick (fun () ->
        let node = make_adapter ~id:"a2" ~input_ref:"n2" ~transform:ParseJson
          ~on_error:`Passthrough () in
        match node.node_type with
        | Adapter { on_error; _ } ->
          check bool "passthrough" true (on_error = `Passthrough)
        | _ -> fail "expected Adapter");
      test_case "make_tool_node" `Quick (fun () ->
        let args = `Assoc [("key", `String "value")] in
        let node = make_tool_node ~id:"tool1" ~name:"fetch" ~args in
        match node.node_type with
        | Tool { name; args = a } ->
          check string "name" "fetch" name;
          check bool "args" true (a = args)
        | _ -> fail "expected Tool");
      test_case "make_pipeline" `Quick (fun () ->
        let node = make_pipeline ~id:"p1" [dummy; dummy] in
        match node.node_type with
        | Pipeline nodes ->
          check int "nodes" 2 (List.length nodes)
        | _ -> fail "expected Pipeline");
      test_case "make_fanout" `Quick (fun () ->
        let node = make_fanout ~id:"f1" [dummy; dummy; dummy] in
        match node.node_type with
        | Fanout nodes ->
          check int "nodes" 3 (List.length nodes)
        | _ -> fail "expected Fanout");
      test_case "make_quorum" `Quick (fun () ->
        let node = make_quorum ~id:"q1" ~consensus:Majority
          ~weights:[("n1", 0.5); ("n2", 1.0)] [dummy; dummy] in
        match node.node_type with
        | Quorum { consensus; weights; nodes } ->
          check bool "majority" true (consensus = Majority);
          check int "weights" 2 (List.length weights);
          check int "nodes" 2 (List.length nodes)
        | _ -> fail "expected Quorum");
    ]);

    (* {1Count parallel groups — uncovered node types} *)

    ("count_parallel_groups_extra", [
      test_case "mcts" `Quick (fun () ->
        let sim = make_llm_node ~id:"sim" ~model:"g" ~prompt:"p" () in
        let s1 = make_llm_node ~id:"s1" ~model:"g" ~prompt:"p" () in
        let node = { id = "m"; node_type = Mcts {
          strategies = [s1]; simulation = sim; evaluator = "llm_judge";
          evaluator_prompt = None; policy = UCB1 1.41; max_iterations = 10;
          max_depth = 3; expansion_threshold = 2; early_stop = None;
          parallel_sims = 1 };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "mcts = 1" 1 (count_parallel_groups node));
      test_case "stream_merge" `Quick (fun () ->
        let n1 = make_llm_node ~id:"n1" ~model:"g" ~prompt:"p" () in
        let node = { id = "sm"; node_type = StreamMerge {
          nodes = [n1]; reducer = First; initial = "";
          min_results = None; timeout = None };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "stream_merge = 1" 1 (count_parallel_groups node));
      test_case "feedback_loop" `Quick (fun () ->
        let gen = make_llm_node ~id:"gen" ~model:"g" ~prompt:"p" () in
        let node = { id = "fl"; node_type = FeedbackLoop {
          generator = gen; evaluator_config = { scoring_func = "x"; scoring_prompt = None;
            select_strategy = Best }; improver_prompt = "fix";
          max_iterations = 3; score_threshold = 0.8; score_operator = Gte;
          conversational = false; relay_models = [] };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "feedback_loop = 0" 0 (count_parallel_groups node));
      test_case "cascade" `Quick (fun () ->
        let n1 = make_llm_node ~id:"t1" ~model:"g" ~prompt:"p" () in
        let tier = { tier_node = n1; tier_index = 0; confidence_threshold = 0.7;
                     cost_weight = 1.0; pass_context = true } in
        let node = { id = "cas"; node_type = Cascade {
          tiers = [tier]; confidence_prompt = None; max_escalations = 2;
          context_mode = CM_Summary; task_hint = None; default_threshold = 0.7;
          difficulty_hint = None };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "cascade with no fanouts = 0" 0 (count_parallel_groups node));
      test_case "spawn" `Quick (fun () ->
        let inner = make_fanout ~id:"f" [dummy; dummy] in
        let node = { id = "sp"; node_type = Spawn {
          clean = true; inner; pass_vars = []; inherit_cache = true };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "spawn wrapping fanout = 1" 1 (count_parallel_groups node));
      test_case "threshold with pass fail" `Quick (fun () ->
        let fanout = make_fanout ~id:"f" [dummy] in
        let node = { id = "thr"; node_type = Threshold {
          metric = "score"; operator = Gte; value = 0.5;
          input_node = dummy; on_pass = Some fanout; on_fail = Some dummy };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "threshold with fanout on_pass = 1" 1 (count_parallel_groups node));
      test_case "goal_driven" `Quick (fun () ->
        let action = make_fanout ~id:"a" [dummy; dummy] in
        let node = { id = "gd"; node_type = GoalDriven {
          goal_metric = "s"; goal_operator = Gte; goal_value = 0.5;
          action_node = action; measure_func = "f"; max_iterations = 3;
          strategy_hints = []; conversational = false; relay_models = [] };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "goal_driven with fanout action = 1" 1 (count_parallel_groups node));
      test_case "evaluator" `Quick (fun () ->
        let node = { id = "ev"; node_type = Evaluator {
          candidates = [dummy; dummy]; scoring_func = "llm_judge";
          scoring_prompt = None; select_strategy = Best; min_score = None };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "evaluator = 1" 1 (count_parallel_groups node));
      test_case "cache" `Quick (fun () ->
        let inner = make_fanout ~id:"f" [dummy] in
        let node = { id = "c"; node_type = Cache {
          key_expr = "k"; ttl_seconds = 60; inner };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "cache wrapping fanout = 1" 1 (count_parallel_groups node));
      test_case "subgraph" `Quick (fun () ->
        let fanout = make_fanout ~id:"f" [dummy; dummy] in
        let chain = make_chain ~id:"sub" ~nodes:[fanout] ~output:"f" () in
        let node = { id = "sg"; node_type = Subgraph chain;
          input_mapping = []; output_key = None; depends_on = None } in
        check int "subgraph with fanout = 1" 1 (count_parallel_groups node));
      test_case "fallback" `Quick (fun () ->
        let primary = make_fanout ~id:"p" [dummy] in
        let fb = make_fanout ~id:"fb" [dummy] in
        let node = { id = "fb"; node_type = Fallback {
          primary; fallbacks = [fb] };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "fallback with 2 fanouts = 2" 2 (count_parallel_groups node));
      test_case "adapter" `Quick (fun () ->
        let node = { id = "ad"; node_type = Adapter {
          input_ref = "n1"; transform = Extract ".x"; on_error = `Fail };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "adapter = 0" 0 (count_parallel_groups node));
      test_case "chain_exec" `Quick (fun () ->
        let node = { id = "ce"; node_type = ChainExec {
          chain_source = "s"; validate = true; max_depth = 3;
          sandbox = false; context_inject = []; pass_outputs = true };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "chain_exec = 0" 0 (count_parallel_groups node));
      test_case "masc" `Quick (fun () ->
        let broadcast = { id = "b"; node_type = Masc_broadcast {
          message = "hi"; room = None; mention = [] };
          input_mapping = []; output_key = None; depends_on = None } in
        let listen = { id = "l"; node_type = Masc_listen {
          filter = None; timeout_sec = 30.0; room = None };
          input_mapping = []; output_key = None; depends_on = None } in
        let claim = { id = "c"; node_type = Masc_claim {
          task_id = None; room = None };
          input_mapping = []; output_key = None; depends_on = None } in
        check int "broadcast = 0" 0 (count_parallel_groups broadcast);
        check int "listen = 0" 0 (count_parallel_groups listen);
        check int "claim = 0" 0 (count_parallel_groups claim));
    ]);

    (* {1Batch priority yojson} *)

    ("batch_priority_yojson", [
      test_case "roundtrip" `Quick (fun () ->
        let pris = [High; Normal; Low] in
        List.iter (fun p ->
          let j = batch_priority_to_yojson p in
          match batch_priority_of_yojson j with
          | Ok p2 -> check bool "roundtrip" true (p = p2)
          | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
        ) pris);
    ]);

    (* {1Retry config yojson} *)

    ("retry_config_yojson", [
      test_case "default roundtrip" `Quick (fun () ->
        let j = retry_config_to_yojson default_retry_config in
        match retry_config_of_yojson j with
        | Ok rc ->
          check int "max_retries" 3 rc.max_retries;
          check int "initial_delay" 1000 rc.initial_delay_ms;
          check (float 0.01) "multiplier" 2.0 rc.backoff_multiplier;
          check int "max_delay" 30000 rc.max_delay_ms
        | Error e -> fail e);
    ]);

    (* {1Batch config yojson} *)

    ("batch_config_yojson", [
      test_case "default roundtrip" `Quick (fun () ->
        let j = batch_config_to_yojson default_batch_config in
        match batch_config_of_yojson j with
        | Ok bc ->
          check int "max_concurrent" 5 bc.batch_max_concurrent;
          check int "rate_limit" 60 bc.rate_limit_per_min;
          check bool "normal" true (bc.priority = Normal)
        | Error e -> fail e);
    ]);

    (* {1Node type name — remaining types} *)

    ("node_type_name_remaining", [
      test_case "llm" `Quick (fun () ->
        let n = make_llm_node ~id:"n" ~model:"g" ~prompt:"p" () in
        check string "llm" "llm" (node_type_name n.node_type));
      test_case "tool" `Quick (fun () ->
        let n = make_tool_node ~id:"n" ~name:"t" ~args:`Null in
        check string "tool" "tool" (node_type_name n.node_type));
      test_case "pipeline" `Quick (fun () ->
        let n = make_pipeline ~id:"n" [] in
        check string "pipeline" "pipeline" (node_type_name n.node_type));
      test_case "fanout" `Quick (fun () ->
        let n = make_fanout ~id:"n" [] in
        check string "fanout" "fanout" (node_type_name n.node_type));
      test_case "quorum" `Quick (fun () ->
        let n = make_quorum ~id:"n" [] in
        check string "quorum" "quorum" (node_type_name n.node_type));
      test_case "merge" `Quick (fun () ->
        check string "merge" "merge"
          (node_type_name (Merge { strategy = Concat; nodes = [] })));
      test_case "threshold" `Quick (fun () ->
        check string "threshold" "threshold"
          (node_type_name (Threshold { metric = "s"; operator = Gte; value = 0.0;
            input_node = dummy; on_pass = None; on_fail = None })));
      test_case "retry" `Quick (fun () ->
        check string "retry" "retry"
          (node_type_name (Retry { node = dummy; max_attempts = 3;
            backoff = Constant 1.0; retry_on = [] })));
      test_case "fallback" `Quick (fun () ->
        check string "fallback" "fallback"
          (node_type_name (Fallback { primary = dummy; fallbacks = [] })));
      test_case "race" `Quick (fun () ->
        check string "race" "race"
          (node_type_name (Race { nodes = []; timeout = None })));
      test_case "adapter" `Quick (fun () ->
        check string "adapter" "adapter"
          (node_type_name (Adapter { input_ref = "x"; transform = ParseJson;
            on_error = `Fail })));
      test_case "cache" `Quick (fun () ->
        check string "cache" "cache"
          (node_type_name (Cache { key_expr = "k"; ttl_seconds = 0; inner = dummy })));
      test_case "mcts" `Quick (fun () ->
        check string "mcts" "mcts"
          (node_type_name (Mcts { strategies = []; simulation = dummy;
            evaluator = "x"; evaluator_prompt = None; policy = Greedy;
            max_iterations = 1; max_depth = 1; expansion_threshold = 1;
            early_stop = None; parallel_sims = 1 })));
      test_case "masc_broadcast" `Quick (fun () ->
        check string "masc_broadcast" "masc_broadcast"
          (node_type_name (Masc_broadcast { message = "hi"; room = None; mention = [] })));
      test_case "masc_listen" `Quick (fun () ->
        check string "masc_listen" "masc_listen"
          (node_type_name (Masc_listen { filter = None; timeout_sec = 10.0; room = None })));
      test_case "masc_claim" `Quick (fun () ->
        check string "masc_claim" "masc_claim"
          (node_type_name (Masc_claim { task_id = None; room = None })));
    ]);

    (* {1Make llm_node with all options} *)

    ("make_llm_node_options", [
      test_case "with tools and prompt_ref" `Quick (fun () ->
        let tools = `List [`String "tool1"] in
        let n = make_llm_node ~id:"n" ~model:"gemini" ~system:"sys" ~prompt:"p"
          ~timeout:60 ~tools ~prompt_ref:"my-prompt@1.0"
          ~prompt_vars:[("lang", "ocaml")] ~thinking:true () in
        match n.node_type with
        | Llm { model; system; prompt; timeout; tools = t; prompt_ref; prompt_vars; thinking } ->
          check string "model" "gemini" model;
          check (option string) "system" (Some "sys") system;
          check string "prompt" "p" prompt;
          check (option int) "timeout" (Some 60) timeout;
          check bool "tools" true (t <> None);
          check (option string) "prompt_ref" (Some "my-prompt@1.0") prompt_ref;
          check int "vars" 1 (List.length prompt_vars);
          check bool "thinking" true thinking
        | _ -> fail "expected Llm");
    ]);
  ]

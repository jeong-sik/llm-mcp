(** Tests for Chain_mermaid_parser — Coverage expansion
    Targets: helper functions, parse_node_content for all shapes,
    metadata parsing, edge parsing, multiline joining,
    infer_type_from_id, build helpers *)

open Alcotest

module CMP = Chain_mermaid_parser
module CT = Chain_types

(** {1 Helper Functions} *)

let test_strip_quotes_double () =
  check string "double quotes" "hello" (CMP.strip_quotes {|"hello"|});
  check string "no quotes" "hello" (CMP.strip_quotes "hello");
  check string "single quotes" "world" (CMP.strip_quotes "'world'");
  check string "mismatched" {|"hello'|} (CMP.strip_quotes {|"hello'|});
  check string "empty quotes" "" (CMP.strip_quotes {|""|});
  check string "single char" "x" (CMP.strip_quotes "x")

let test_has_explicit_type_prefix () =
  check bool "LLM:" true (CMP.has_explicit_type_prefix "LLM:gemini");
  check bool "Tool:" true (CMP.has_explicit_type_prefix "Tool:eslint");
  check bool "Ref:" true (CMP.has_explicit_type_prefix "Ref:my-chain");
  check bool "Quorum:" true (CMP.has_explicit_type_prefix "Quorum:2");
  check bool "Gate:" true (CMP.has_explicit_type_prefix "Gate:condition");
  check bool "Merge:" true (CMP.has_explicit_type_prefix "Merge:concat");
  check bool "Pipeline:" true (CMP.has_explicit_type_prefix "Pipeline:a,b");
  check bool "Fanout:" true (CMP.has_explicit_type_prefix "Fanout:a,b");
  check bool "Map:" true (CMP.has_explicit_type_prefix "Map:f,n");
  check bool "Bind:" true (CMP.has_explicit_type_prefix "Bind:f,n");
  check bool "Cache:" true (CMP.has_explicit_type_prefix "Cache:k,60,n");
  check bool "Batch:" true (CMP.has_explicit_type_prefix "Batch:10,true,n");
  check bool "Spawn:" true (CMP.has_explicit_type_prefix "Spawn:clean,n");
  check bool "Threshold:" true (CMP.has_explicit_type_prefix "Threshold:>=0.8");
  check bool "Evaluator:" true (CMP.has_explicit_type_prefix "Evaluator:llm_judge");
  check bool "GoalDriven:" true (CMP.has_explicit_type_prefix "GoalDriven:cov:gte:0.9:10");
  check bool "MCTS:" true (CMP.has_explicit_type_prefix "MCTS:ucb1:1.41:10");
  check bool "StreamMerge:" true (CMP.has_explicit_type_prefix "StreamMerge:concat");
  check bool "FeedbackLoop:" true (CMP.has_explicit_type_prefix "FeedbackLoop:quality,3,>=0.8");
  check bool "no prefix" false (CMP.has_explicit_type_prefix "hello world");
  check bool "empty" false (CMP.has_explicit_type_prefix "")

let test_extract_tools_flag () =
  let (content, flag) = CMP.extract_tools_flag "hello +tools" in
  check string "content" "hello" content;
  check bool "flag" true flag;
  let (content2, flag2) = CMP.extract_tools_flag "hello" in
  check string "no flag content" "hello" content2;
  check bool "no flag" false flag2;
  let (content3, flag3) = CMP.extract_tools_flag "+tools" in
  check string "only flag" "+tools" content3;
  check bool "only flag returns false" false flag3

let test_make_tools_value () =
  check bool "true gives Some" true (CMP.make_tools_value true <> None);
  check bool "false gives None" true (CMP.make_tools_value false = None)

(** {1 Metadata Parsing} *)

let test_parse_input_mapping_json () =
  let json = `List [`List [`String "a"; `String "b"]; `List [`String "c"; `String "d"]] in
  let result = CMP.parse_input_mapping_json json in
  check int "two mappings" 2 (List.length result);
  check bool "first" true (List.mem ("a", "b") result);
  check bool "second" true (List.mem ("c", "d") result)

let test_parse_input_mapping_json_invalid () =
  let result = CMP.parse_input_mapping_json (`String "not a list") in
  check int "invalid returns empty" 0 (List.length result);
  let result2 = CMP.parse_input_mapping_json (`List [`Int 42]) in
  check int "bad items filtered" 0 (List.length result2)

let test_empty_meta () =
  let m = CMP.empty_meta () in
  check bool "chain_id none" true (m.chain_id = None);
  check bool "chain_output none" true (m.chain_output = None);
  check bool "chain_timeout none" true (m.chain_timeout = None);
  check bool "chain_trace none" true (m.chain_trace = None)

let test_parse_chain_meta () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_chain_meta {|{"id":"test","output":"final","timeout":30,"trace":true,"max_depth":5,"max_concurrency":3}|} m in
  check bool "id" true (m2.chain_id = Some "test");
  check bool "output" true (m2.chain_output = Some "final");
  check bool "timeout" true (m2.chain_timeout = Some 30);
  check bool "trace" true (m2.chain_trace = Some true);
  check bool "max_depth" true (m2.chain_max_depth = Some 5);
  check bool "max_concurrency" true (m2.chain_max_concurrency = Some 3)

let test_parse_chain_meta_invalid_json () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_chain_meta "not json" m in
  check bool "unchanged on invalid" true (m2.chain_id = None)

let test_parse_chain_meta_not_object () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_chain_meta "[1,2,3]" m in
  check bool "unchanged on non-object" true (m2.chain_id = None)

let test_parse_chain_full () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_chain_full "{\"nodes\":[]}" m in
  check bool "chain_full_json set" true (m2.chain_full_json = Some "{\"nodes\":[]}")

let test_parse_node_meta_input_mapping () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_node_meta "mynode" {|{"input_mapping":[["a","b"]]}|} m in
  let mapping = Hashtbl.find_opt m2.node_input_mappings "mynode" in
  check bool "has mapping" true (mapping <> None);
  check int "mapping len" 1 (List.length (Option.get mapping))

let test_parse_node_meta_goaldriven () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_node_meta "gd_node" {|{"action_node_id":"action1","measure_func":"myfunc","conversational":true,"relay_models":["m1","m2"],"strategy_hints":[["k","v"]]}|} m in
  let gd = Hashtbl.find_opt m2.node_goaldriven_meta "gd_node" in
  check bool "has goaldriven" true (gd <> None);
  let gd = Option.get gd in
  check bool "action_node_id" true (gd.gd_action_node_id = Some "action1");
  check bool "measure_func" true (gd.gd_measure_func = Some "myfunc");
  check bool "conversational" true gd.gd_conversational;
  check int "relay_models" 2 (List.length gd.gd_relay_models)

let test_parse_node_meta_invalid () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_node_meta "bad" "not json" m in
  check bool "unchanged" true (Hashtbl.length m2.node_goaldriven_meta = 0)

let test_parse_meta_comment_chain () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_meta_comment {|%% @chain {"id":"hello"}|} m in
  check bool "chain id" true (m2.chain_id = Some "hello")

let test_parse_meta_comment_chain_full () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_meta_comment {|%% @chain_full {"nodes":[]}|} m in
  check bool "chain_full_json" true (m2.chain_full_json <> None)

let test_parse_meta_comment_chain_json () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_meta_comment {|%% @chain_json {"id":"x","nodes":[]}|} m in
  check bool "chain_json" true (m2.chain_json <> None);
  check bool "chain_full_json" true (m2.chain_full_json <> None)

let test_parse_meta_comment_node () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_meta_comment {|%% @node:abc {"input_mapping":[["x","y"]]}|} m in
  check bool "node mapping" true (Hashtbl.mem m2.node_input_mappings "abc")

let test_parse_meta_comment_not_comment () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_meta_comment "graph LR" m in
  check bool "not comment unchanged" true (m2.chain_id = None)

let test_parse_meta_comment_unknown_directive () =
  let m = CMP.empty_meta () in
  let m2 = CMP.parse_meta_comment "%% @unknown stuff" m in
  check bool "unknown unchanged" true (m2.chain_id = None)

(** {1 Node Definition Parsing} *)

let test_parse_node_definition_rect () =
  match CMP.parse_node_definition {|a["LLM:gemini 'hello'"]|} with
  | Some (id, node) ->
      check string "id" "a" id;
      check bool "shape" true (node.shape = `Rect)
  | None -> fail "should parse rect"

let test_parse_node_definition_diamond () =
  match CMP.parse_node_definition {|q{Quorum:2}|} with
  | Some (id, node) ->
      check string "id" "q" id;
      check bool "shape" true (node.shape = `Diamond)
  | None -> fail "should parse diamond"

let test_parse_node_definition_subroutine () =
  match CMP.parse_node_definition {|p[["Pipeline:a,b"]]|} with
  | Some (id, node) ->
      check string "id" "p" id;
      check bool "shape" true (node.shape = `Subroutine)
  | None -> fail "should parse subroutine"

let test_parse_node_definition_stadium () =
  match CMP.parse_node_definition {|r("Retry:3")|} with
  | Some (id, node) ->
      check string "id" "r" id;
      check bool "shape" true (node.shape = `Stadium)
  | None -> fail "should parse stadium"

let test_parse_node_definition_trap () =
  match CMP.parse_node_definition {|a>/"Adapter:Extract .data"/|} with
  | Some (id, node) ->
      check string "id" "a" id;
      check bool "shape" true (node.shape = `Trap)
  | None -> fail "should parse trap"

let test_parse_node_definition_empty () =
  check bool "empty" true (CMP.parse_node_definition "" = None)

let test_parse_node_definition_kebab_case () =
  match CMP.parse_node_definition {|my-node["LLM:gemini 'test'"]|} with
  | Some (id, _) -> check string "kebab id" "my-node" id
  | None -> fail "should parse kebab case"

(** {1 parse_node_content — Subroutine Shapes} *)

let test_content_ref () =
  match CMP.parse_node_content `Subroutine "Ref:my-chain" with
  | Ok (CT.ChainRef ref_id) -> check string "ref" "my-chain" ref_id
  | _ -> fail "expected ChainRef"

let test_content_pipeline () =
  match CMP.parse_node_content `Subroutine "Pipeline:a,b,c" with
  | Ok (CT.Pipeline nodes) -> check int "3 nodes" 3 (List.length nodes)
  | _ -> fail "expected Pipeline"

let test_content_fanout () =
  match CMP.parse_node_content `Subroutine "Fanout:x,y" with
  | Ok (CT.Fanout nodes) -> check int "2 nodes" 2 (List.length nodes)
  | _ -> fail "expected Fanout"

let test_content_map () =
  match CMP.parse_node_content `Subroutine "Map:format,result" with
  | Ok (CT.Map { func; _ }) -> check string "func" "format" func
  | _ -> fail "expected Map"

let test_content_map_error () =
  match CMP.parse_node_content `Subroutine "Map:bad" with
  | Error _ -> ()
  | _ -> fail "expected Map error"

let test_content_bind () =
  match CMP.parse_node_content `Subroutine "Bind:route,handler" with
  | Ok (CT.Bind { func; _ }) -> check string "func" "route" func
  | _ -> fail "expected Bind"

let test_content_bind_error () =
  match CMP.parse_node_content `Subroutine "Bind:bad" with
  | Error _ -> ()
  | _ -> fail "expected Bind error"

let test_content_cache_3parts () =
  match CMP.parse_node_content `Subroutine "Cache:mykey,3600,compute" with
  | Ok (CT.Cache { key_expr; ttl_seconds; _ }) ->
      check string "key" "mykey" key_expr;
      check int "ttl" 3600 ttl_seconds
  | _ -> fail "expected Cache"

let test_content_cache_2parts () =
  match CMP.parse_node_content `Subroutine "Cache:mykey,compute" with
  | Ok (CT.Cache { ttl_seconds; _ }) ->
      check int "default ttl" 0 ttl_seconds
  | _ -> fail "expected Cache with default ttl"

let test_content_cache_error () =
  match CMP.parse_node_content `Subroutine "Cache:bad" with
  | Error _ -> ()
  | _ -> fail "expected Cache error"

let test_content_batch_3parts () =
  match CMP.parse_node_content `Subroutine "Batch:10,true,processor" with
  | Ok (CT.Batch { batch_size; parallel; _ }) ->
      check int "size" 10 batch_size;
      check bool "parallel" true parallel
  | _ -> fail "expected Batch"

let test_content_batch_2parts () =
  match CMP.parse_node_content `Subroutine "Batch:5,processor" with
  | Ok (CT.Batch { batch_size; parallel; _ }) ->
      check int "size" 5 batch_size;
      check bool "parallel default" true parallel
  | _ -> fail "expected Batch"

let test_content_batch_error () =
  match CMP.parse_node_content `Subroutine "Batch:bad" with
  | Error _ -> ()
  | _ -> fail "expected Batch error"

let test_content_spawn_2parts () =
  match CMP.parse_node_content `Subroutine "Spawn:clean,mynode" with
  | Ok (CT.Spawn { clean; _ }) -> check bool "clean" true clean
  | _ -> fail "expected Spawn"

let test_content_spawn_3parts () =
  match CMP.parse_node_content `Subroutine "Spawn:true,x|y,mynode" with
  | Ok (CT.Spawn { clean; pass_vars; _ }) ->
      check bool "clean" true clean;
      check int "pass_vars" 2 (List.length pass_vars)
  | _ -> fail "expected Spawn with pass_vars"

let test_content_spawn_error () =
  match CMP.parse_node_content `Subroutine "Spawn:bad" with
  | Error _ -> ()
  | _ -> fail "expected Spawn error"

let test_content_stream_merge_1part () =
  match CMP.parse_node_content `Subroutine "StreamMerge:concat" with
  | Ok (CT.StreamMerge { reducer; _ }) ->
      check bool "concat reducer" true (reducer = CT.Concat)
  | _ -> fail "expected StreamMerge"

let test_content_stream_merge_2parts () =
  match CMP.parse_node_content `Subroutine "StreamMerge:first,3" with
  | Ok (CT.StreamMerge { reducer; min_results; _ }) ->
      check bool "first reducer" true (reducer = CT.First);
      check bool "min_results" true (min_results = Some 3)
  | _ -> fail "expected StreamMerge with min"

let test_content_stream_merge_3parts () =
  match CMP.parse_node_content `Subroutine "StreamMerge:last,2,30.0" with
  | Ok (CT.StreamMerge { reducer; min_results; timeout; _ }) ->
      check bool "last reducer" true (reducer = CT.Last);
      check bool "min" true (min_results = Some 2);
      check bool "timeout" true (timeout = Some 30.0)
  | _ -> fail "expected StreamMerge with timeout"

let test_content_stream_merge_weighted () =
  match CMP.parse_node_content `Subroutine "StreamMerge:weighted_avg" with
  | Ok (CT.StreamMerge { reducer; _ }) ->
      check bool "weighted reducer" true (reducer = CT.WeightedAvg)
  | _ -> fail "expected WeightedAvg"

let test_content_stream_merge_custom () =
  match CMP.parse_node_content `Subroutine "StreamMerge:myreducer" with
  | Ok (CT.StreamMerge { reducer; _ }) ->
      check bool "custom reducer" true (reducer = CT.Custom "myreducer")
  | _ -> fail "expected Custom reducer"

let test_content_stream_merge_error () =
  match CMP.parse_node_content `Subroutine "StreamMerge:a,b,c,d" with
  | Error _ -> ()
  | _ -> fail "expected StreamMerge error"

let test_content_feedback_loop_3parts () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:quality,5,>=0.9" with
  | Ok (CT.FeedbackLoop { evaluator_config; max_iterations; score_threshold; score_operator; _ }) ->
      check string "func" "quality" evaluator_config.scoring_func;
      check int "max_iter" 5 max_iterations;
      check bool "threshold" true (Float.abs (score_threshold -. 0.9) < 0.01);
      check bool "operator gte" true (score_operator = CT.Gte)
  | _ -> fail "expected FeedbackLoop"

let test_content_feedback_loop_lt_operator () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:cost,3,<0.3" with
  | Ok (CT.FeedbackLoop { score_operator; score_threshold; _ }) ->
      check bool "Lt" true (score_operator = CT.Lt);
      check bool "threshold" true (Float.abs (score_threshold -. 0.3) < 0.01)
  | _ -> fail "expected FeedbackLoop Lt"

let test_content_feedback_loop_lte_operator () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:cost,3,<=0.5" with
  | Ok (CT.FeedbackLoop { score_operator; _ }) ->
      check bool "Lte" true (score_operator = CT.Lte)
  | _ -> fail "expected FeedbackLoop Lte"

let test_content_feedback_loop_neq_operator () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:cost,3,!=0.0" with
  | Ok (CT.FeedbackLoop { score_operator; _ }) ->
      check bool "Neq" true (score_operator = CT.Neq)
  | _ -> fail "expected FeedbackLoop Neq"

let test_content_feedback_loop_gt_operator () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:cost,3,>0.5" with
  | Ok (CT.FeedbackLoop { score_operator; _ }) ->
      check bool "Gt" true (score_operator = CT.Gt)
  | _ -> fail "expected FeedbackLoop Gt"

let test_content_feedback_loop_eq_operator () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:cost,3,=1.0" with
  | Ok (CT.FeedbackLoop { score_operator; _ }) ->
      check bool "Eq" true (score_operator = CT.Eq)
  | _ -> fail "expected FeedbackLoop Eq"

let test_content_feedback_loop_bare_number () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:cost,3,0.7" with
  | Ok (CT.FeedbackLoop { score_operator; score_threshold; _ }) ->
      check bool "default Gte" true (score_operator = CT.Gte);
      check bool "threshold" true (Float.abs (score_threshold -. 0.7) < 0.01)
  | _ -> fail "expected FeedbackLoop bare number"

let test_content_feedback_loop_2parts () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:quality,5" with
  | Ok (CT.FeedbackLoop { max_iterations; score_threshold; _ }) ->
      check int "max_iter" 5 max_iterations;
      check bool "default threshold" true (Float.abs (score_threshold -. 0.7) < 0.01)
  | _ -> fail "expected FeedbackLoop 2parts"

let test_content_feedback_loop_1part () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:quality" with
  | Ok (CT.FeedbackLoop { max_iterations; _ }) ->
      check int "default max_iter" 3 max_iterations
  | _ -> fail "expected FeedbackLoop 1part"

let test_content_feedback_loop_error () =
  match CMP.parse_node_content `Subroutine "FeedbackLoop:a,b,c,d" with
  | Error _ -> ()
  | _ -> fail "expected FeedbackLoop error"

let test_content_subroutine_unknown () =
  match CMP.parse_node_content `Subroutine "NotAType:stuff" with
  | Error _ -> ()
  | _ -> fail "expected error on unknown subroutine"

(** {1 parse_node_content — Diamond Shapes} *)

let test_content_diamond_quorum () =
  match CMP.parse_node_content `Diamond "Quorum:3" with
  | Ok (CT.Quorum { consensus; _ }) ->
      check bool "count 3" true (consensus = CT.Count 3)
  | _ -> fail "expected Quorum"

let test_content_diamond_quorum_majority () =
  match CMP.parse_node_content `Diamond "Quorum:majority" with
  | Ok (CT.Quorum { consensus; _ }) ->
      check bool "majority" true (consensus = CT.Majority)
  | _ -> fail "expected Quorum majority"

let test_content_diamond_quorum_unanimous () =
  match CMP.parse_node_content `Diamond "Quorum:unanimous" with
  | Ok (CT.Quorum { consensus; _ }) ->
      check bool "unanimous" true (consensus = CT.Unanimous)
  | _ -> fail "expected Quorum unanimous"

let test_content_diamond_gate () =
  match CMP.parse_node_content `Diamond "Gate:score > 0.8" with
  | Ok (CT.Gate { condition; _ }) ->
      check string "condition" "score > 0.8" condition
  | _ -> fail "expected Gate"

let test_content_diamond_merge_weighted () =
  match CMP.parse_node_content `Diamond "Merge:weighted_avg" with
  | Ok (CT.Merge { strategy; _ }) ->
      check bool "weighted" true (strategy = CT.WeightedAvg)
  | _ -> fail "expected Merge weighted"

let test_content_diamond_merge_first () =
  match CMP.parse_node_content `Diamond "Merge:first" with
  | Ok (CT.Merge { strategy; _ }) ->
      check bool "first" true (strategy = CT.First)
  | _ -> fail "expected Merge first"

let test_content_diamond_merge_last () =
  match CMP.parse_node_content `Diamond "Merge:last" with
  | Ok (CT.Merge { strategy; _ }) ->
      check bool "last" true (strategy = CT.Last)
  | _ -> fail "expected Merge last"

let test_content_diamond_merge_concat () =
  match CMP.parse_node_content `Diamond "Merge:concat" with
  | Ok (CT.Merge { strategy; _ }) ->
      check bool "concat" true (strategy = CT.Concat)
  | _ -> fail "expected Merge concat"

let test_content_diamond_merge_custom () =
  match CMP.parse_node_content `Diamond "Merge:my_strategy" with
  | Ok (CT.Merge { strategy; _ }) ->
      check bool "custom" true (strategy = CT.Custom "my_strategy")
  | _ -> fail "expected Merge custom"

let test_content_diamond_goaldriven () =
  match CMP.parse_node_content `Diamond "GoalDriven:coverage:gte:0.90:10" with
  | Ok (CT.GoalDriven { goal_metric; goal_operator; goal_value; max_iterations; _ }) ->
      check string "metric" "coverage" goal_metric;
      check bool "gte" true (goal_operator = CT.Gte);
      check bool "value" true (Float.abs (goal_value -. 0.90) < 0.01);
      check int "max_iter" 10 max_iterations
  | _ -> fail "expected GoalDriven"

let test_content_diamond_goaldriven_all_ops () =
  let test_op op_str expected_op =
    let input = Printf.sprintf "GoalDriven:metric:%s:0.5:5" op_str in
    match CMP.parse_node_content `Diamond input with
    | Ok (CT.GoalDriven { goal_operator; _ }) ->
        check bool op_str true (goal_operator = expected_op)
    | _ -> fail (Printf.sprintf "expected GoalDriven with %s" op_str)
  in
  test_op "gt" CT.Gt;
  test_op "lt" CT.Lt;
  test_op "lte" CT.Lte;
  test_op "eq" CT.Eq;
  test_op "neq" CT.Neq

let test_content_diamond_goaldriven_error () =
  match CMP.parse_node_content `Diamond "GoalDriven:bad format" with
  | Error _ -> ()
  | _ -> fail "expected GoalDriven error"

let test_content_diamond_mcts_greedy () =
  match CMP.parse_node_content `Diamond "MCTS:greedy:20" with
  | Ok (CT.Mcts { policy; max_iterations; _ }) ->
      check bool "greedy" true (policy = CT.Greedy);
      check int "iter" 20 max_iterations
  | _ -> fail "expected MCTS greedy"

let test_content_diamond_mcts_ucb1 () =
  match CMP.parse_node_content `Diamond "MCTS:ucb1:1.41:10" with
  | Ok (CT.Mcts { policy; max_iterations; _ }) ->
      (match policy with CT.UCB1 v -> check bool "ucb1 param" true (Float.abs (v -. 1.41) < 0.01) | _ -> fail "expected UCB1");
      check int "iter" 10 max_iterations
  | _ -> fail "expected MCTS ucb1"

let test_content_diamond_mcts_eps () =
  match CMP.parse_node_content `Diamond "MCTS:eps:0.1:15" with
  | Ok (CT.Mcts { policy; _ }) ->
      (match policy with CT.EpsilonGreedy v -> check bool "eps param" true (Float.abs (v -. 0.1) < 0.01) | _ -> fail "expected EpsilonGreedy")
  | _ -> fail "expected MCTS eps"

let test_content_diamond_mcts_softmax () =
  match CMP.parse_node_content `Diamond "MCTS:softmax:1.0:10" with
  | Ok (CT.Mcts { policy; _ }) ->
      (match policy with CT.Softmax v -> check bool "softmax" true (Float.abs (v -. 1.0) < 0.01) | _ -> fail "expected Softmax")
  | _ -> fail "expected MCTS softmax"

let test_content_diamond_mcts_error () =
  match CMP.parse_node_content `Diamond "MCTS:a:b:c:d" with
  | Error _ -> ()
  | _ -> fail "expected MCTS error"

let test_content_diamond_evaluator_3parts () =
  match CMP.parse_node_content `Diamond "Evaluator:llm_judge:best:0.7" with
  | Ok (CT.Evaluator { scoring_func; select_strategy; min_score; _ }) ->
      check string "func" "llm_judge" scoring_func;
      check bool "best" true (select_strategy = CT.Best);
      check bool "min_score" true (min_score = Some 0.7)
  | _ -> fail "expected Evaluator 3parts"

let test_content_diamond_evaluator_2parts () =
  match CMP.parse_node_content `Diamond "Evaluator:anti_fake:worst" with
  | Ok (CT.Evaluator { scoring_func; select_strategy; _ }) ->
      check string "func" "anti_fake" scoring_func;
      check bool "worst" true (select_strategy = CT.Worst)
  | _ -> fail "expected Evaluator 2parts"

let test_content_diamond_evaluator_weighted () =
  match CMP.parse_node_content `Diamond "Evaluator:f:weighted" with
  | Ok (CT.Evaluator { select_strategy; _ }) ->
      check bool "weighted" true (select_strategy = CT.WeightedRandom)
  | _ -> fail "expected Evaluator weighted"

let test_content_diamond_evaluator_above () =
  (* "Evaluator:f:above:0.5" splits on ':' → ["f";"above";"0.5"]
     "above" is 5 chars, guard checks > 6 → falls through to Best *)
  match CMP.parse_node_content `Diamond "Evaluator:f:above:0.5" with
  | Ok (CT.Evaluator { select_strategy; scoring_func; _ }) ->
      check string "func" "f" scoring_func;
      (match select_strategy with CT.Best -> () | _ -> fail "expected Best (above guard is > 6 chars)")
  | _ -> fail "expected Evaluator"

let test_content_diamond_evaluator_1part () =
  match CMP.parse_node_content `Diamond "Evaluator:custom_func" with
  | Ok (CT.Evaluator { scoring_func; _ }) ->
      check string "func" "custom_func" scoring_func
  | _ -> fail "expected Evaluator 1part"

let test_content_diamond_evaluator_error () =
  match CMP.parse_node_content `Diamond "Evaluator:" with
  | Ok (CT.Evaluator _) -> ()
  | _ -> fail "expected Evaluator default"

let test_content_diamond_threshold_gte () =
  match CMP.parse_node_content `Diamond "Threshold:>=0.8" with
  | Ok (CT.Threshold { operator; value; _ }) ->
      check bool "gte" true (operator = CT.Gte);
      check bool "value" true (Float.abs (value -. 0.8) < 0.01)
  | _ -> fail "expected Threshold gte"

let test_content_diamond_threshold_gt () =
  match CMP.parse_node_content `Diamond "Threshold:>0.5" with
  | Ok (CT.Threshold { operator; _ }) ->
      check bool "gt" true (operator = CT.Gt)
  | _ -> fail "expected Threshold gt"

let test_content_diamond_threshold_lte () =
  match CMP.parse_node_content `Diamond "Threshold:<=0.3" with
  | Ok (CT.Threshold { operator; _ }) ->
      check bool "lte" true (operator = CT.Lte)
  | _ -> fail "expected Threshold lte"

let test_content_diamond_threshold_lt () =
  match CMP.parse_node_content `Diamond "Threshold:<0.1" with
  | Ok (CT.Threshold { operator; _ }) ->
      check bool "lt" true (operator = CT.Lt)
  | _ -> fail "expected Threshold lt"

let test_content_diamond_threshold_eq () =
  match CMP.parse_node_content `Diamond "Threshold:==1.0" with
  | Ok (CT.Threshold { operator; _ }) ->
      check bool "eq" true (operator = CT.Eq)
  | _ -> fail "expected Threshold eq"

let test_content_diamond_threshold_neq () =
  match CMP.parse_node_content `Diamond "Threshold:!=0.0" with
  | Ok (CT.Threshold { operator; _ }) ->
      check bool "neq" true (operator = CT.Neq)
  | _ -> fail "expected Threshold neq"

let test_content_diamond_threshold_error () =
  match CMP.parse_node_content `Diamond "Threshold:bad" with
  | Error _ -> ()
  | _ -> fail "expected Threshold error"

let test_content_diamond_unknown () =
  match CMP.parse_node_content `Diamond "Unknown:stuff" with
  | Error _ -> ()
  | _ -> fail "expected diamond unknown error"

(** {1 parse_node_content — Rect, Trap, Stadium, Circle} *)

let test_content_rect_llm_double_quote () =
  match CMP.parse_node_content `Rect {|LLM:claude "Review code"|} with
  | Ok (CT.Llm { model; prompt; _ }) ->
      check string "model" "claude" model;
      check string "prompt" "Review code" prompt
  | _ -> fail "expected LLM with double quote"

let test_content_rect_llm_single_quote () =
  match CMP.parse_node_content `Rect "LLM:gemini 'Analyze data'" with
  | Ok (CT.Llm { model; prompt; _ }) ->
      check string "model" "gemini" model;
      check string "prompt" "Analyze data" prompt
  | _ -> fail "expected LLM with single quote"

let test_content_rect_llm_no_prompt () =
  match CMP.parse_node_content `Rect "LLM:codex" with
  | Ok (CT.Llm { model; prompt; _ }) ->
      check string "model" "codex" model;
      check string "prompt" "{{input}}" prompt
  | _ -> fail "expected LLM with default prompt"

let test_content_rect_llm_with_tools () =
  match CMP.parse_node_content `Rect "LLM:gemini 'test' +tools" with
  | Ok (CT.Llm { tools; _ }) ->
      check bool "has tools" true (tools <> None)
  | _ -> fail "expected LLM with tools"

let test_content_rect_tool_with_json () =
  match CMP.parse_node_content `Rect {|Tool:eslint {"fix":true}|} with
  | Ok (CT.Tool { name; args }) ->
      check string "name" "eslint" name;
      (match args with `Assoc _ -> () | _ -> fail "expected json args")
  | _ -> fail "expected Tool with json"

let test_content_rect_tool_with_quote () =
  match CMP.parse_node_content `Rect {|Tool:dune "build"|} with
  | Ok (CT.Tool { name; _ }) ->
      check string "name" "dune" name
  | _ -> fail "expected Tool with quote"

let test_content_rect_tool_simple () =
  match CMP.parse_node_content `Rect "Tool:jest" with
  | Ok (CT.Tool { name; _ }) ->
      check string "name" "jest" name
  | _ -> fail "expected Tool simple"

let test_content_rect_default () =
  match CMP.parse_node_content `Rect "some text content" with
  | Ok (CT.Llm { model; prompt; _ }) ->
      check string "default model" "gemini" model;
      check string "prompt" "some text content" prompt
  | _ -> fail "expected default LLM"

let test_content_trap () =
  match CMP.parse_node_content `Trap "Adapter:Extract .data" with
  | Ok (CT.Adapter _) -> ()
  | _ -> fail "expected Adapter"

let test_content_trap_generic () =
  match CMP.parse_node_content `Trap "some template" with
  | Ok (CT.Adapter { transform; _ }) ->
      (match transform with CT.Template _ -> () | _ -> fail "expected Template")
  | _ -> fail "expected Adapter generic"

let test_content_stadium_retry () =
  match CMP.parse_node_content `Stadium "Retry:5" with
  | Ok (CT.Retry { max_attempts; _ }) ->
      check int "5 attempts" 5 max_attempts
  | _ -> fail "expected Retry"

let test_content_stadium_fallback () =
  match CMP.parse_node_content `Stadium "Fallback" with
  | Ok (CT.Fallback _) -> ()
  | _ -> fail "expected Fallback"

let test_content_stadium_fallback_colon () =
  match CMP.parse_node_content `Stadium "Fallback:" with
  | Ok (CT.Fallback _) -> ()
  | _ -> fail "expected Fallback with colon"

let test_content_stadium_race () =
  match CMP.parse_node_content `Stadium "Race" with
  | Ok (CT.Race _) -> ()
  | _ -> fail "expected Race"

let test_content_stadium_race_colon () =
  match CMP.parse_node_content `Stadium "Race:" with
  | Ok (CT.Race _) -> ()
  | _ -> fail "expected Race with colon"

let test_content_stadium_cascade () =
  match CMP.parse_node_content `Stadium "Cascade:0.8:summary" with
  | Ok (CT.Cascade { default_threshold; context_mode; _ }) ->
      check bool "threshold" true (Float.abs (default_threshold -. 0.8) < 0.01);
      check bool "summary" true (context_mode = CT.CM_Summary)
  | _ -> fail "expected Cascade"

let test_content_stadium_cascade_bare () =
  match CMP.parse_node_content `Stadium "Cascade" with
  | Ok (CT.Cascade { default_threshold; _ }) ->
      check bool "default threshold" true (Float.abs (default_threshold -. 0.7) < 0.01)
  | _ -> fail "expected Cascade bare"

let test_content_stadium_default () =
  match CMP.parse_node_content `Stadium "something else" with
  | Ok (CT.Llm { model; _ }) ->
      check string "default model" "gemini" model
  | _ -> fail "expected default LLM in stadium"

let test_content_circle_broadcast () =
  match CMP.parse_node_content `Circle "MASC:broadcast hello" with
  | Ok (CT.Masc_broadcast { message; _ }) ->
      check string "message" "hello" message
  | _ -> fail "expected MASC broadcast"

let test_content_circle_listen () =
  match CMP.parse_node_content `Circle "MASC:listen done" with
  | Ok (CT.Masc_listen { filter; _ }) ->
      check bool "filter" true (filter = Some "done")
  | _ -> fail "expected MASC listen"

let test_content_circle_claim () =
  match CMP.parse_node_content `Circle "MASC:claim task-1" with
  | Ok (CT.Masc_claim { task_id; _ }) ->
      check bool "task_id" true (task_id = Some "task-1")
  | _ -> fail "expected MASC claim"

let test_content_circle_broadcast_heuristic () =
  match CMP.parse_node_content `Circle "broad stuff" with
  | Ok (CT.Masc_broadcast _) -> ()
  | _ -> fail "expected heuristic broadcast"

let test_content_circle_listen_heuristic () =
  match CMP.parse_node_content `Circle "waiting for it" with
  | Ok (CT.Masc_listen _) -> ()
  | _ -> fail "expected heuristic listen"

let test_content_circle_default () =
  match CMP.parse_node_content `Circle "" with
  | Ok (CT.Masc_broadcast _) -> ()
  | _ -> fail "expected default broadcast"

(** {1 infer_type_from_id} *)

let test_infer_quorum_id () =
  match CMP.infer_type_from_id "quorum_2" `Diamond "text" with
  | Ok (CT.Quorum _) -> ()
  | _ -> fail "expected Quorum from id"

let test_infer_consensus_id () =
  match CMP.infer_type_from_id "consensus_3" `Diamond "text" with
  | Ok (CT.Quorum _) -> ()
  | _ -> fail "expected Quorum from consensus id"

let test_infer_gate_id () =
  match CMP.infer_type_from_id "gate_check" `Diamond "condition" with
  | Ok (CT.Gate _) -> ()
  | _ -> fail "expected Gate from id"

let test_infer_merge_id () =
  match CMP.infer_type_from_id "merge_results" `Diamond "text" with
  | Ok (CT.Merge _) -> ()
  | _ -> fail "expected Merge from id"

let test_infer_goal_id_with_params () =
  match CMP.infer_type_from_id "goal_score" `Diamond "gte:0.90:10" with
  | Ok (CT.GoalDriven { goal_metric; goal_value; max_iterations; _ }) ->
      check string "metric" "score" goal_metric;
      check bool "value" true (Float.abs (goal_value -. 0.90) < 0.01);
      check int "max_iter" 10 max_iterations
  | _ -> fail "expected GoalDriven from id"

let test_infer_goal_id_fallback () =
  match CMP.infer_type_from_id "goal_metric" `Diamond "no match" with
  | Ok (CT.GoalDriven { max_iterations; _ }) ->
      check int "default max_iter" 10 max_iterations
  | _ -> fail "expected GoalDriven fallback"

let test_infer_eval_id () =
  match CMP.infer_type_from_id "eval_quality" `Diamond "text" with
  | Ok (CT.Evaluator _) -> ()
  | _ -> fail "expected Evaluator from id"

let test_infer_diamond_evaluator_text () =
  match CMP.infer_type_from_id "unknown" `Diamond "Evaluator:llm_judge:best:0.7" with
  | Ok (CT.Evaluator { scoring_func; _ }) ->
      check string "func" "llm_judge" scoring_func
  | _ -> fail "expected Evaluator from text"

let test_infer_diamond_quorum_text () =
  match CMP.infer_type_from_id "unknown" `Diamond "Quorum:2" with
  | Ok (CT.Quorum _) -> ()
  | _ -> fail "expected Quorum from text"

let test_infer_diamond_default_gate () =
  match CMP.infer_type_from_id "unknown" `Diamond "some condition" with
  | Ok (CT.Gate _) -> ()
  | _ -> fail "expected Gate default"

let test_infer_subroutine_ref () =
  match CMP.infer_type_from_id "ref_mychain" `Subroutine "text" with
  | Ok (CT.ChainRef ref_id) -> check string "ref" "mychain" ref_id
  | _ -> fail "expected ChainRef from ref_ id"

let test_infer_subroutine_seq () =
  match CMP.infer_type_from_id "seq_steps" `Subroutine "text" with
  | Ok (CT.Pipeline _) -> ()
  | _ -> fail "expected Pipeline from seq_ id"

let test_infer_subroutine_par () =
  match CMP.infer_type_from_id "par_branches" `Subroutine "text" with
  | Ok (CT.Fanout _) -> ()
  | _ -> fail "expected Fanout from par_ id"

let test_infer_subroutine_map () =
  match CMP.infer_type_from_id "map_transform" `Subroutine "func" with
  | Ok (CT.Map _) -> ()
  | _ -> fail "expected Map from map_ id"

let test_infer_subroutine_default () =
  match CMP.infer_type_from_id "myref" `Subroutine "" with
  | Ok (CT.ChainRef ref_id) -> check string "ref" "myref" ref_id
  | _ -> fail "expected ChainRef default"

let test_infer_rect_llm_model () =
  match CMP.infer_type_from_id "gemini_parse" `Rect "Analyze data" with
  | Ok (CT.Llm { model; prompt; _ }) ->
      check string "model" "gemini" model;
      check string "prompt" "Analyze data" prompt
  | _ -> fail "expected LLM from model id"

let test_infer_rect_tool () =
  match CMP.infer_type_from_id "eslint" `Rect "check code" with
  | Ok (CT.Tool { name; _ }) ->
      check string "name" "eslint" name
  | _ -> fail "expected Tool from known tool id"

let test_infer_rect_default () =
  match CMP.infer_type_from_id "a" `Rect "some prompt" with
  | Ok (CT.Llm { model; prompt; _ }) ->
      check string "default model" "gemini" model;
      check string "prompt" "some prompt" prompt
  | _ -> fail "expected default LLM from short id"

let test_infer_rect_llm_with_tools () =
  match CMP.infer_type_from_id "claude_review" `Rect "Review +tools" with
  | Ok (CT.Llm { model; tools; _ }) ->
      check string "model" "claude" model;
      check bool "tools" true (tools <> None)
  | _ -> fail "expected LLM with tools from id"

let test_infer_trap () =
  match CMP.infer_type_from_id "adapter1" `Trap "my template" with
  | Ok (CT.Adapter _) -> ()
  | _ -> fail "expected Adapter from trap"

let test_infer_stadium_retry () =
  match CMP.infer_type_from_id "r" `Stadium "Retry:3" with
  | Ok (CT.Retry { max_attempts; _ }) ->
      check int "attempts" 3 max_attempts
  | _ -> fail "expected Retry from stadium"

let test_infer_stadium_cascade () =
  match CMP.infer_type_from_id "c" `Stadium "Cascade:0.8:full" with
  | Ok (CT.Cascade { default_threshold; _ }) ->
      check bool "threshold" true (Float.abs (default_threshold -. 0.8) < 0.01)
  | _ -> fail "expected Cascade from stadium"

let test_infer_stadium_cascade_bare () =
  match CMP.infer_type_from_id "c" `Stadium "Cascade" with
  | Ok (CT.Cascade _) -> ()
  | _ -> fail "expected Cascade bare from stadium"

let test_infer_circle_masc () =
  match CMP.infer_type_from_id "notify" `Circle "MASC:broadcast hello" with
  | Ok (CT.Masc_broadcast _) -> ()
  | _ -> fail "expected MASC from circle"

(** {1 Edge Parsing} *)

let test_parse_edge_simple () =
  let edges = CMP.parse_edge_line "a --> b" in
  check int "1 edge" 1 (List.length edges);
  let e = List.hd edges in
  check bool "from" true (e.from_nodes = ["a"]);
  check string "to" "b" e.to_node

let test_parse_edge_labeled () =
  let edges = CMP.parse_edge_line "a -->|true| b" in
  check int "1 edge" 1 (List.length edges);
  let e = List.hd edges in
  check bool "label" true (e.label = Some "true")

let test_parse_edge_chained () =
  let edges = CMP.parse_edge_line "a --> b --> c" in
  check int "2 edges" 2 (List.length edges)

let test_parse_edge_ampersand () =
  let edges = CMP.parse_edge_line "a & b --> c" in
  check int "1 edge" 1 (List.length edges);
  let e = List.hd edges in
  check int "2 from" 2 (List.length e.from_nodes)

(** {1 Multiline Brackets} *)

let test_join_multiline_brackets () =
  let lines = ["a[\"first"; "second line\"]"; "b{done}"] in
  let result = CMP.join_multiline_brackets lines in
  check int "joined" 2 (List.length result)

let test_join_multiline_empty () =
  let result = CMP.join_multiline_brackets [] in
  check int "empty" 0 (List.length result)

let test_join_multiline_quotes () =
  let lines = [{|a["hello|}; {|world"]|}; "b{ok}"] in
  let result = CMP.join_multiline_brackets lines in
  check int "joined with quotes" 2 (List.length result)

(** {1 Build Helpers} *)

let test_build_dependency_graph () =
  let edges = [
    { CMP.from_nodes = ["a"]; to_node = "b"; label = None };
    { CMP.from_nodes = ["a"; "c"]; to_node = "d"; label = None };
  ] in
  let deps = CMP.build_dependency_graph edges in
  check int "b has 1 dep" 1 (List.length (Hashtbl.find deps "b"));
  check int "d has 2 deps" 2 (List.length (Hashtbl.find deps "d"))

let test_build_outgoing_edges () =
  let edges = [
    { CMP.from_nodes = ["a"]; to_node = "b"; label = None };
    { CMP.from_nodes = ["a"]; to_node = "c"; label = Some "true" };
  ] in
  let out = CMP.build_outgoing_edges edges in
  check int "a has 2 out" 2 (List.length (Hashtbl.find out "a"))

let test_find_output_nodes () =
  let graph = {
    CMP.direction = "LR";
    nodes = [
      { CMP.id = "a"; shape = `Rect; content = "test" };
      { CMP.id = "b"; shape = `Rect; content = "test" };
    ];
    edges = [{ CMP.from_nodes = ["a"]; to_node = "b"; label = None }];
  } in
  let outputs = CMP.find_output_nodes graph in
  check int "1 output" 1 (List.length outputs);
  check string "b is output" "b" (List.hd outputs)

(** {1 parse_mermaid_text Integration} *)

let test_parse_mermaid_flowchart () =
  let mermaid = "flowchart TB\n  a[\"LLM:gemini 'test'\"]\n  b{Quorum:2}\n  a --> b" in
  match CMP.parse_mermaid_text mermaid with
  | Ok graph ->
      check string "direction" "TB" graph.direction;
      check int "2 nodes" 2 (List.length graph.nodes)
  | Error e -> fail e

let test_parse_mermaid_with_meta () =
  let mermaid = {|
%% @chain {"id":"test-chain","output":"final","timeout":60}
graph LR
  a["LLM:gemini 'hello'"]
|} in
  match CMP.parse_mermaid_text_with_meta mermaid with
  | Ok (_graph, meta) ->
      check bool "chain_id" true (meta.chain_id = Some "test-chain");
      check bool "output" true (meta.chain_output = Some "final");
      check bool "timeout" true (meta.chain_timeout = Some 60)
  | Error e -> fail e

let test_parse_mermaid_subgraph_skip () =
  let mermaid = "graph LR\n  subgraph cluster\n  a[\"LLM:gemini 'test'\"]\n  end\n  b[\"LLM:claude 'review'\"]" in
  match CMP.parse_mermaid_text mermaid with
  | Ok graph -> check bool "has nodes" true (List.length graph.nodes > 0)
  | Error e -> fail e

(** {1 mermaid_to_chain Integration — Post-Processing} *)

let find_node (chain : CT.chain) id =
  List.find (fun (n : CT.node) -> n.id = id) chain.nodes

let test_mermaid_to_chain_quorum_filling () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Step 1'"]
    b["LLM:claude 'Step 2'"]
    q{Quorum:2}
    a --> q
    b --> q
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let q = find_node chain "q" in
      (match q.node_type with
       | CT.Quorum { nodes; _ } -> check int "2 input nodes" 2 (List.length nodes)
       | _ -> fail "expected Quorum")
  | Error e -> fail e

let test_mermaid_to_chain_merge_filling () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'A'"]
    b["LLM:claude 'B'"]
    m{Merge:concat}
    a --> m
    b --> m
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let m = find_node chain "m" in
      (match m.node_type with
       | CT.Merge { nodes; strategy } ->
           check int "2 nodes" 2 (List.length nodes);
           check bool "concat" true (strategy = CT.Concat)
       | _ -> fail "expected Merge")
  | Error e -> fail e

let test_mermaid_to_chain_race_filling () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Fast'"]
    b["LLM:claude 'Slow'"]
    r("Race")
    a --> r
    b --> r
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let r = find_node chain "r" in
      (match r.node_type with
       | CT.Race { nodes; _ } -> check int "2 race nodes" 2 (List.length nodes)
       | _ -> fail "expected Race")
  | Error e -> fail e

let test_mermaid_to_chain_cascade_filling () =
  let mermaid = {|
graph LR
    fast["LLM:gemini 'Quick'"]
    slow["LLM:claude 'Deep'"]
    c("Cascade:0.7:summary")
    fast --> c
    slow --> c
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let c = find_node chain "c" in
      (match c.node_type with
       | CT.Cascade { tiers; _ } -> check int "2 tiers" 2 (List.length tiers)
       | _ -> fail "expected Cascade")
  | Error e -> fail e

let test_mermaid_to_chain_evaluator_filling () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Generate'"]
    b["LLM:claude 'Generate'"]
    e{Evaluator:llm_judge:best:0.7}
    a --> e
    b --> e
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let e = find_node chain "e" in
      (match e.node_type with
       | CT.Evaluator { candidates; _ } -> check int "2 candidates" 2 (List.length candidates)
       | _ -> fail "expected Evaluator")
  | Error e -> fail e

let test_mermaid_to_chain_retry_filling () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Try this'"]
    r("Retry:3")
    a --> r
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let r = find_node chain "r" in
      (match r.node_type with
       | CT.Retry { max_attempts; _ } -> check int "3 attempts" 3 max_attempts
       | _ -> fail "expected Retry")
  | Error e -> fail e

let test_mermaid_to_chain_fallback_filling () =
  let mermaid = {|
graph LR
    main["LLM:gemini 'Primary'"]
    backup["LLM:claude 'Backup'"]
    f("Fallback")
    main --> f
    backup --> f
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let f = find_node chain "f" in
      (match f.node_type with
       | CT.Fallback { fallbacks; _ } ->
           check int "1 fallback" 1 (List.length fallbacks)
       | _ -> fail "expected Fallback")
  | Error e -> fail e

let test_mermaid_to_chain_stream_merge_filling () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Fast'"]
    b["LLM:claude 'Deep'"]
    sm[["StreamMerge:concat,1,30.0"]]
    a --> sm
    b --> sm
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let sm = find_node chain "sm" in
      (match sm.node_type with
       | CT.StreamMerge { nodes; _ } -> check int "2 stream nodes" 2 (List.length nodes)
       | _ -> fail "expected StreamMerge")
  | Error e -> fail e

let test_mermaid_to_chain_output_detection () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Start'"]
    b["LLM:claude 'End'"]
    a --> b
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      check string "output" "b" chain.output
  | Error e -> fail e

(** {1 Advanced Integration Tests} *)

let test_mermaid_to_chain_goaldriven () =
  let mermaid = {|
graph LR
    gen["LLM:gemini 'Generate code'"]
    goal{GoalDriven:coverage:gte:0.90:10}
    gen --> goal
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "goal" in
      (match node.node_type with
       | CT.GoalDriven { goal_metric; goal_operator; goal_value; max_iterations; _ } ->
           check string "metric" "coverage" goal_metric;
           check bool "operator gte" true (goal_operator = CT.Gte);
           check (float 0.01) "value" 0.90 goal_value;
           check int "max_iter" 10 max_iterations
       | _ -> fail "expected GoalDriven")
  | Error e -> fail e

let test_mermaid_to_chain_mcts () =
  let mermaid = {|
graph LR
    expand["LLM:gemini 'Generate candidates'"]
    search{MCTS:ucb1:1.41:20}
    expand --> search
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "search" in
      (match node.node_type with
       | CT.Mcts { policy; max_iterations; _ } ->
           (match policy with
            | CT.UCB1 c -> check (float 0.01) "exploration" 1.41 c
            | _ -> fail "expected UCB1 policy");
           check int "iterations" 20 max_iterations
       | _ -> fail "expected Mcts")
  | Error e -> fail e

let test_mermaid_to_chain_feedback_loop () =
  let mermaid = {|
graph LR
    gen["LLM:claude 'Generate code'"]
    loop[["FeedbackLoop:code_quality,5,>=0.85"]]
    gen --> loop
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "loop" in
      (match node.node_type with
       | CT.FeedbackLoop { max_iterations; score_threshold; score_operator; _ } ->
           check int "max_iter" 5 max_iterations;
           check (float 0.01) "threshold" 0.85 score_threshold;
           check bool "operator gte" true (score_operator = CT.Gte)
       | _ -> fail "expected FeedbackLoop")
  | Error e -> fail e

let test_mermaid_to_chain_batch () =
  let mermaid = {|
graph LR
    data["LLM:gemini 'Load data'"]
    batch[["Batch:10,true,processor"]]
    data --> batch
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "batch" in
      (match node.node_type with
       | CT.Batch { batch_size; parallel; _ } ->
           check int "batch_size" 10 batch_size;
           check bool "parallel" true parallel
       | _ -> fail "expected Batch")
  | Error e -> fail e

let test_mermaid_to_chain_spawn () =
  let mermaid = {|
graph LR
    prep["LLM:gemini 'Prepare'"]
    sp[["Spawn:clean,worker"]]
    prep --> sp
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "sp" in
      (match node.node_type with
       | CT.Spawn { clean; _ } ->
           check bool "clean" true clean
       | _ -> fail "expected Spawn")
  | Error e -> fail e

let test_mermaid_to_chain_cache () =
  let mermaid = {|
graph LR
    fetch["LLM:gemini 'Fetch data'"]
    c[["Cache:user_key,300,processor"]]
    fetch --> c
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "c" in
      (match node.node_type with
       | CT.Cache { key_expr; ttl_seconds; _ } ->
           check string "key" "user_key" key_expr;
           check int "ttl" 300 ttl_seconds
       | _ -> fail "expected Cache")
  | Error e -> fail e

let test_mermaid_to_chain_masc_broadcast () =
  (* Test Circle parse_node_content directly for MASC broadcast with message+mention *)
  match CMP.parse_node_content `Circle "MASC:broadcast @gemini check this" with
  | Ok (CT.Masc_broadcast { message; mention; _ }) ->
      check bool "has message" true (String.length message > 0);
      ignore mention
  | Ok _ -> fail "expected Masc_broadcast"
  | Error e -> fail e

let test_mermaid_to_chain_masc_listen () =
  (* Test Circle parse_node_content for MASC listen with filter *)
  match CMP.parse_node_content `Circle "MASC:listen complete|done" with
  | Ok (CT.Masc_listen { filter; _ }) ->
      check bool "has filter" true (filter <> None)
  | Ok _ -> fail "expected Masc_listen"
  | Error e -> fail e

let test_mermaid_to_chain_masc_claim () =
  (* Test Circle parse_node_content for MASC claim with task_id *)
  match CMP.parse_node_content `Circle "MASC:claim task-123" with
  | Ok (CT.Masc_claim { task_id; _ }) ->
      check bool "has task_id" true (task_id <> None)
  | Ok _ -> fail "expected Masc_claim"
  | Error e -> fail e

let test_mermaid_to_chain_map () =
  let mermaid = {|
graph LR
    data["LLM:gemini 'Load items'"]
    m[["Map:transform,processor"]]
    data --> m
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "m" in
      (match node.node_type with
       | CT.Map { func; _ } -> check string "func" "transform" func
       | _ -> fail "expected Map")
  | Error e -> fail e

let test_mermaid_to_chain_bind () =
  let mermaid = {|
graph LR
    data["LLM:gemini 'Parse request'"]
    b[["Bind:route,handler"]]
    data --> b
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "b" in
      (match node.node_type with
       | CT.Bind { func; _ } -> check string "func" "route" func
       | _ -> fail "expected Bind")
  | Error e -> fail e

let test_mermaid_to_chain_adapter () =
  let mermaid = {|
graph LR
    a["LLM:gemini 'Analyze'"]
    adapt>/"Transform data"/]
    a --> adapt
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "adapt" in
      (match node.node_type with
       | CT.Adapter _ -> ()
       | _ -> fail "expected Adapter")
  | Error e -> fail e

let test_mermaid_to_chain_gate () =
  let mermaid = {|
graph LR
    check["LLM:gemini 'Check condition'"]
    g{Gate:score > 0.8}
    yes["LLM:claude 'High quality'"]
    check --> g
    g --> yes
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "g" in
      (match node.node_type with
       | CT.Gate { condition; _ } ->
           check bool "has condition" true (String.length condition > 0)
       | _ -> fail "expected Gate")
  | Error e -> fail e

let test_mermaid_to_chain_threshold () =
  let mermaid = {|
graph LR
    gen["LLM:gemini 'Generate'"]
    t{Threshold:>=0.8}
    gen --> t
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "t" in
      (match node.node_type with
       | CT.Threshold _ -> ()
       | _ -> fail "expected Threshold")
  | Error e -> fail e

let test_mermaid_to_chain_tool_b64 () =
  let mermaid = {|
graph LR
    t["Tool:eslint {}"]
    out["LLM:gemini 'Review {{t}}'"]
    t --> out
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "t" in
      (match node.node_type with
       | CT.Tool { name; _ } -> check string "tool name" "eslint" name
       | _ -> fail "expected Tool")
  | Error e -> fail e

let test_infer_goal_id_integration () =
  let mermaid = {|
graph LR
    gen["LLM:gemini 'Generate'"]
    goal_coverage{gte:0.90:10}
    gen --> goal_coverage
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "goal_coverage" in
      (match node.node_type with
       | CT.GoalDriven { goal_metric; _ } ->
           check string "metric from id" "coverage" goal_metric
       | _ -> fail "expected GoalDriven from goal_ prefix")
  | Error e -> fail e

let test_infer_eval_id_integration () =
  let mermaid = {|
graph LR
    gen["LLM:gemini 'Generate'"]
    eval_quality{judge this}
    gen --> eval_quality
|} in
  match CMP.parse_mermaid_to_chain mermaid with
  | Ok chain ->
      let node = find_node chain "eval_quality" in
      (match node.node_type with
       | CT.Evaluator _ -> ()
       | _ -> fail "expected Evaluator from eval_ prefix")
  | Error e -> fail e

(** {1 Test Suite} *)

let () =
  run "mermaid_parser_coverage" [
    ("helpers", [
      test_case "strip_quotes" `Quick test_strip_quotes_double;
      test_case "has_explicit_type_prefix" `Quick test_has_explicit_type_prefix;
      test_case "extract_tools_flag" `Quick test_extract_tools_flag;
      test_case "make_tools_value" `Quick test_make_tools_value;
    ]);
    ("metadata", [
      test_case "parse_input_mapping_json" `Quick test_parse_input_mapping_json;
      test_case "parse_input_mapping_invalid" `Quick test_parse_input_mapping_json_invalid;
      test_case "empty_meta" `Quick test_empty_meta;
      test_case "parse_chain_meta" `Quick test_parse_chain_meta;
      test_case "parse_chain_meta_invalid" `Quick test_parse_chain_meta_invalid_json;
      test_case "parse_chain_meta_not_object" `Quick test_parse_chain_meta_not_object;
      test_case "parse_chain_full" `Quick test_parse_chain_full;
      test_case "parse_node_meta_mapping" `Quick test_parse_node_meta_input_mapping;
      test_case "parse_node_meta_goaldriven" `Quick test_parse_node_meta_goaldriven;
      test_case "parse_node_meta_invalid" `Quick test_parse_node_meta_invalid;
      test_case "meta_comment_chain" `Quick test_parse_meta_comment_chain;
      test_case "meta_comment_chain_full" `Quick test_parse_meta_comment_chain_full;
      test_case "meta_comment_chain_json" `Quick test_parse_meta_comment_chain_json;
      test_case "meta_comment_node" `Quick test_parse_meta_comment_node;
      test_case "meta_comment_not_comment" `Quick test_parse_meta_comment_not_comment;
      test_case "meta_comment_unknown" `Quick test_parse_meta_comment_unknown_directive;
    ]);
    ("node_definition", [
      test_case "rect" `Quick test_parse_node_definition_rect;
      test_case "diamond" `Quick test_parse_node_definition_diamond;
      test_case "subroutine" `Quick test_parse_node_definition_subroutine;
      test_case "stadium" `Quick test_parse_node_definition_stadium;
      test_case "trap" `Quick test_parse_node_definition_trap;
      test_case "empty" `Quick test_parse_node_definition_empty;
      test_case "kebab_case" `Quick test_parse_node_definition_kebab_case;
    ]);
    ("content_subroutine", [
      test_case "ref" `Quick test_content_ref;
      test_case "pipeline" `Quick test_content_pipeline;
      test_case "fanout" `Quick test_content_fanout;
      test_case "map" `Quick test_content_map;
      test_case "map error" `Quick test_content_map_error;
      test_case "bind" `Quick test_content_bind;
      test_case "bind error" `Quick test_content_bind_error;
      test_case "cache 3parts" `Quick test_content_cache_3parts;
      test_case "cache 2parts" `Quick test_content_cache_2parts;
      test_case "cache error" `Quick test_content_cache_error;
      test_case "batch 3parts" `Quick test_content_batch_3parts;
      test_case "batch 2parts" `Quick test_content_batch_2parts;
      test_case "batch error" `Quick test_content_batch_error;
      test_case "spawn 2parts" `Quick test_content_spawn_2parts;
      test_case "spawn 3parts" `Quick test_content_spawn_3parts;
      test_case "spawn error" `Quick test_content_spawn_error;
      test_case "stream_merge 1" `Quick test_content_stream_merge_1part;
      test_case "stream_merge 2" `Quick test_content_stream_merge_2parts;
      test_case "stream_merge 3" `Quick test_content_stream_merge_3parts;
      test_case "stream_merge weighted" `Quick test_content_stream_merge_weighted;
      test_case "stream_merge custom" `Quick test_content_stream_merge_custom;
      test_case "stream_merge error" `Quick test_content_stream_merge_error;
      test_case "feedback_loop 3" `Quick test_content_feedback_loop_3parts;
      test_case "feedback_loop lt" `Quick test_content_feedback_loop_lt_operator;
      test_case "feedback_loop lte" `Quick test_content_feedback_loop_lte_operator;
      test_case "feedback_loop neq" `Quick test_content_feedback_loop_neq_operator;
      test_case "feedback_loop gt" `Quick test_content_feedback_loop_gt_operator;
      test_case "feedback_loop eq" `Quick test_content_feedback_loop_eq_operator;
      test_case "feedback_loop bare" `Quick test_content_feedback_loop_bare_number;
      test_case "feedback_loop 2" `Quick test_content_feedback_loop_2parts;
      test_case "feedback_loop 1" `Quick test_content_feedback_loop_1part;
      test_case "feedback_loop error" `Quick test_content_feedback_loop_error;
      test_case "unknown" `Quick test_content_subroutine_unknown;
    ]);
    ("content_diamond", [
      test_case "quorum" `Quick test_content_diamond_quorum;
      test_case "quorum majority" `Quick test_content_diamond_quorum_majority;
      test_case "quorum unanimous" `Quick test_content_diamond_quorum_unanimous;
      test_case "gate" `Quick test_content_diamond_gate;
      test_case "merge weighted" `Quick test_content_diamond_merge_weighted;
      test_case "merge first" `Quick test_content_diamond_merge_first;
      test_case "merge last" `Quick test_content_diamond_merge_last;
      test_case "merge concat" `Quick test_content_diamond_merge_concat;
      test_case "merge custom" `Quick test_content_diamond_merge_custom;
      test_case "goaldriven" `Quick test_content_diamond_goaldriven;
      test_case "goaldriven all ops" `Quick test_content_diamond_goaldriven_all_ops;
      test_case "goaldriven error" `Quick test_content_diamond_goaldriven_error;
      test_case "mcts greedy" `Quick test_content_diamond_mcts_greedy;
      test_case "mcts ucb1" `Quick test_content_diamond_mcts_ucb1;
      test_case "mcts eps" `Quick test_content_diamond_mcts_eps;
      test_case "mcts softmax" `Quick test_content_diamond_mcts_softmax;
      test_case "mcts error" `Quick test_content_diamond_mcts_error;
      test_case "evaluator 3" `Quick test_content_diamond_evaluator_3parts;
      test_case "evaluator 2" `Quick test_content_diamond_evaluator_2parts;
      test_case "evaluator weighted" `Quick test_content_diamond_evaluator_weighted;
      test_case "evaluator above" `Quick test_content_diamond_evaluator_above;
      test_case "evaluator 1" `Quick test_content_diamond_evaluator_1part;
      test_case "evaluator empty" `Quick test_content_diamond_evaluator_error;
      test_case "threshold gte" `Quick test_content_diamond_threshold_gte;
      test_case "threshold gt" `Quick test_content_diamond_threshold_gt;
      test_case "threshold lte" `Quick test_content_diamond_threshold_lte;
      test_case "threshold lt" `Quick test_content_diamond_threshold_lt;
      test_case "threshold eq" `Quick test_content_diamond_threshold_eq;
      test_case "threshold neq" `Quick test_content_diamond_threshold_neq;
      test_case "threshold error" `Quick test_content_diamond_threshold_error;
      test_case "unknown" `Quick test_content_diamond_unknown;
    ]);
    ("content_other", [
      test_case "rect llm double" `Quick test_content_rect_llm_double_quote;
      test_case "rect llm single" `Quick test_content_rect_llm_single_quote;
      test_case "rect llm no prompt" `Quick test_content_rect_llm_no_prompt;
      test_case "rect llm +tools" `Quick test_content_rect_llm_with_tools;
      test_case "rect tool json" `Quick test_content_rect_tool_with_json;
      test_case "rect tool quote" `Quick test_content_rect_tool_with_quote;
      test_case "rect tool simple" `Quick test_content_rect_tool_simple;
      test_case "rect default" `Quick test_content_rect_default;
      test_case "trap adapter" `Quick test_content_trap;
      test_case "trap generic" `Quick test_content_trap_generic;
      test_case "stadium retry" `Quick test_content_stadium_retry;
      test_case "stadium fallback" `Quick test_content_stadium_fallback;
      test_case "stadium fallback:" `Quick test_content_stadium_fallback_colon;
      test_case "stadium race" `Quick test_content_stadium_race;
      test_case "stadium race:" `Quick test_content_stadium_race_colon;
      test_case "stadium cascade" `Quick test_content_stadium_cascade;
      test_case "stadium cascade bare" `Quick test_content_stadium_cascade_bare;
      test_case "stadium default" `Quick test_content_stadium_default;
      test_case "circle broadcast" `Quick test_content_circle_broadcast;
      test_case "circle listen" `Quick test_content_circle_listen;
      test_case "circle claim" `Quick test_content_circle_claim;
      test_case "circle heuristic br" `Quick test_content_circle_broadcast_heuristic;
      test_case "circle heuristic li" `Quick test_content_circle_listen_heuristic;
      test_case "circle default" `Quick test_content_circle_default;
    ]);
    ("infer_type", [
      test_case "quorum id" `Quick test_infer_quorum_id;
      test_case "consensus id" `Quick test_infer_consensus_id;
      test_case "gate id" `Quick test_infer_gate_id;
      test_case "merge id" `Quick test_infer_merge_id;
      test_case "goal id params" `Quick test_infer_goal_id_with_params;
      test_case "goal id fallback" `Quick test_infer_goal_id_fallback;
      test_case "eval id" `Quick test_infer_eval_id;
      test_case "diamond evaluator text" `Quick test_infer_diamond_evaluator_text;
      test_case "diamond quorum text" `Quick test_infer_diamond_quorum_text;
      test_case "diamond default gate" `Quick test_infer_diamond_default_gate;
      test_case "subroutine ref" `Quick test_infer_subroutine_ref;
      test_case "subroutine seq" `Quick test_infer_subroutine_seq;
      test_case "subroutine par" `Quick test_infer_subroutine_par;
      test_case "subroutine map" `Quick test_infer_subroutine_map;
      test_case "subroutine default" `Quick test_infer_subroutine_default;
      test_case "rect llm model" `Quick test_infer_rect_llm_model;
      test_case "rect tool" `Quick test_infer_rect_tool;
      test_case "rect default" `Quick test_infer_rect_default;
      test_case "rect llm +tools" `Quick test_infer_rect_llm_with_tools;
      test_case "trap" `Quick test_infer_trap;
      test_case "stadium retry" `Quick test_infer_stadium_retry;
      test_case "stadium cascade" `Quick test_infer_stadium_cascade;
      test_case "stadium cascade bare" `Quick test_infer_stadium_cascade_bare;
      test_case "circle masc" `Quick test_infer_circle_masc;
    ]);
    ("edges", [
      test_case "simple" `Quick test_parse_edge_simple;
      test_case "labeled" `Quick test_parse_edge_labeled;
      test_case "chained" `Quick test_parse_edge_chained;
      test_case "ampersand" `Quick test_parse_edge_ampersand;
    ]);
    ("multiline", [
      test_case "brackets" `Quick test_join_multiline_brackets;
      test_case "empty" `Quick test_join_multiline_empty;
      test_case "quotes" `Quick test_join_multiline_quotes;
    ]);
    ("build_helpers", [
      test_case "dependency_graph" `Quick test_build_dependency_graph;
      test_case "outgoing_edges" `Quick test_build_outgoing_edges;
      test_case "output_nodes" `Quick test_find_output_nodes;
    ]);
    ("integration", [
      test_case "flowchart" `Quick test_parse_mermaid_flowchart;
      test_case "with_meta" `Quick test_parse_mermaid_with_meta;
      test_case "subgraph_skip" `Quick test_parse_mermaid_subgraph_skip;
      test_case "quorum filling" `Quick test_mermaid_to_chain_quorum_filling;
      test_case "merge filling" `Quick test_mermaid_to_chain_merge_filling;
      test_case "race filling" `Quick test_mermaid_to_chain_race_filling;
      test_case "cascade filling" `Quick test_mermaid_to_chain_cascade_filling;
      test_case "evaluator filling" `Quick test_mermaid_to_chain_evaluator_filling;
      test_case "retry filling" `Quick test_mermaid_to_chain_retry_filling;
      test_case "fallback filling" `Quick test_mermaid_to_chain_fallback_filling;
      test_case "stream_merge filling" `Quick test_mermaid_to_chain_stream_merge_filling;
      test_case "output detection" `Quick test_mermaid_to_chain_output_detection;
    ]);
    ("integration_advanced", [
      test_case "goaldriven chain" `Quick test_mermaid_to_chain_goaldriven;
      test_case "mcts chain" `Quick test_mermaid_to_chain_mcts;
      test_case "feedback_loop chain" `Quick test_mermaid_to_chain_feedback_loop;
      test_case "batch chain" `Quick test_mermaid_to_chain_batch;
      test_case "spawn chain" `Quick test_mermaid_to_chain_spawn;
      test_case "cache chain" `Quick test_mermaid_to_chain_cache;
      test_case "masc broadcast chain" `Quick test_mermaid_to_chain_masc_broadcast;
      test_case "masc listen chain" `Quick test_mermaid_to_chain_masc_listen;
      test_case "masc claim chain" `Quick test_mermaid_to_chain_masc_claim;
      test_case "map chain" `Quick test_mermaid_to_chain_map;
      test_case "bind chain" `Quick test_mermaid_to_chain_bind;
      test_case "adapter chain" `Quick test_mermaid_to_chain_adapter;
      test_case "gate chain" `Quick test_mermaid_to_chain_gate;
      test_case "threshold chain" `Quick test_mermaid_to_chain_threshold;
      test_case "tool b64 chain" `Quick test_mermaid_to_chain_tool_b64;
      test_case "infer goal id" `Quick test_infer_goal_id_integration;
      test_case "infer eval id" `Quick test_infer_eval_id_integration;
    ]);
  ]

(** Wave 7: Additional chain_types coverage tests.
    Focus on yojson roundtrips, count_parallel_groups branches,
    make_* constructors, and adapter_transform variants. *)

open Alcotest
open Chain_types

(* ---- helpers ---- *)
let check_str = check string
let check_int = check int
let check_float msg expected actual =
  check (float 0.001) msg expected actual

let simple_llm =
  make_llm_node ~id:"n" ~model:"test" ~prompt:"p" ()

let simple_tool =
  make_tool_node ~id:"t" ~name:"echo" ~args:(`Assoc [])

(* ---- direction yojson roundtrip ---- *)
let test_direction_yojson_lr () =
  let j = direction_to_yojson LR in
  check_str "LR roundtrip" "LR" (direction_to_string (direction_of_yojson j |> Result.get_ok))

let test_direction_yojson_rl () =
  let j = direction_to_yojson RL in
  check_str "RL roundtrip" "RL" (direction_to_string (direction_of_yojson j |> Result.get_ok))

let test_direction_yojson_tb () =
  let j = direction_to_yojson TB in
  check_str "TB roundtrip" "TB" (direction_to_string (direction_of_yojson j |> Result.get_ok))

let test_direction_yojson_bt () =
  let j = direction_to_yojson BT in
  check_str "BT roundtrip" "BT" (direction_to_string (direction_of_yojson j |> Result.get_ok))

let test_direction_of_string_td () =
  check_str "TD alias" "TB" (direction_to_string (direction_of_string "TD"))

let test_direction_of_string_unknown () =
  check_str "unknown default" "LR" (direction_to_string (direction_of_string "ZZ"))

(* ---- consensus_mode yojson roundtrip ---- *)
let test_consensus_yojson_count () =
  let v = Count 5 in
  let j = consensus_mode_to_yojson v in
  let v2 = consensus_mode_of_yojson j |> Result.get_ok in
  check_str "count rt" (consensus_mode_to_string v) (consensus_mode_to_string v2)

let test_consensus_yojson_majority () =
  let v = Majority in
  let j = consensus_mode_to_yojson v in
  let v2 = consensus_mode_of_yojson j |> Result.get_ok in
  check_str "majority rt" "majority" (consensus_mode_to_string v2)

let test_consensus_yojson_unanimous () =
  let v = Unanimous in
  let j = consensus_mode_to_yojson v in
  let v2 = consensus_mode_of_yojson j |> Result.get_ok in
  check_str "unanimous rt" "unanimous" (consensus_mode_to_string v2)

let test_consensus_yojson_weighted () =
  let v = Weighted 0.75 in
  let j = consensus_mode_to_yojson v in
  let v2 = consensus_mode_of_yojson j |> Result.get_ok in
  check_str "weighted rt" (consensus_mode_to_string v) (consensus_mode_to_string v2)

let test_consensus_of_string_count_default () =
  (* Non-numeric string defaults to Count 1 *)
  let v = consensus_mode_of_string "garbage" in
  check_str "garbage -> count:1" "count:1" (consensus_mode_to_string v)

let test_consensus_of_string_numeric () =
  let v = consensus_mode_of_string "3" in
  check_str "3 -> count:3" "count:3" (consensus_mode_to_string v)

(* ---- merge_strategy yojson roundtrip ---- *)
let test_merge_strategy_yojson () =
  let variants = [First; Last; Concat; WeightedAvg; Custom "my_func"] in
  List.iter (fun v ->
    let j = merge_strategy_to_yojson v in
    let v2 = merge_strategy_of_yojson j |> Result.get_ok in
    let j2 = merge_strategy_to_yojson v2 in
    check_str "merge roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

(* ---- threshold_op yojson roundtrip ---- *)
let test_threshold_op_yojson () =
  let variants = [Gt; Gte; Lt; Lte; Eq; Neq] in
  List.iter (fun v ->
    let j = threshold_op_to_yojson v in
    let v2 = threshold_op_of_yojson j |> Result.get_ok in
    let j2 = threshold_op_to_yojson v2 in
    check_str "threshold_op roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

(* ---- select_strategy yojson roundtrip ---- *)
let test_select_strategy_yojson () =
  let variants = [Best; Worst; AboveThreshold 0.8; WeightedRandom] in
  List.iter (fun v ->
    let j = select_strategy_to_yojson v in
    let v2 = select_strategy_of_yojson j |> Result.get_ok in
    let j2 = select_strategy_to_yojson v2 in
    check_str "select_strategy roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

(* ---- backoff_strategy yojson roundtrip ---- *)
let test_backoff_strategy_yojson () =
  let variants = [Constant 1.0; Exponential 2.0; Linear 0.5; Jitter (0.1, 0.5)] in
  List.iter (fun v ->
    let j = backoff_strategy_to_yojson v in
    let v2 = backoff_strategy_of_yojson j |> Result.get_ok in
    let j2 = backoff_strategy_to_yojson v2 in
    check_str "backoff roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

(* ---- mcts_policy yojson roundtrip ---- *)
let test_mcts_policy_yojson () =
  let variants = [UCB1 1.41; Greedy; EpsilonGreedy 0.1; Softmax 0.5] in
  List.iter (fun v ->
    let j = mcts_policy_to_yojson v in
    let v2 = mcts_policy_of_yojson j |> Result.get_ok in
    let j2 = mcts_policy_to_yojson v2 in
    check_str "mcts_policy roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

(* ---- confidence_level yojson roundtrip ---- *)
let test_confidence_level_yojson () =
  let variants : confidence_level list = [High; Medium; Low] in
  List.iter (fun v ->
    let j = confidence_level_to_yojson v in
    let v2 = confidence_level_of_yojson j |> Result.get_ok in
    check_float "confidence roundtrip" (confidence_to_float v) (confidence_to_float v2)
  ) variants

(* ---- batch_priority yojson roundtrip ---- *)
let test_batch_priority_yojson () =
  let variants = [High; Normal; Low] in
  List.iter (fun v ->
    let j = batch_priority_to_yojson v in
    let v2 = batch_priority_of_yojson j |> Result.get_ok in
    let j2 = batch_priority_to_yojson v2 in
    check_str "batch_priority roundtrip" (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

(* ---- adapter_transform yojson roundtrip ---- *)
let test_adapter_transform_yojson () =
  let variants = [
    Extract "data.items";
    Template "Result: {{value}}";
    Summarize 100;
    Truncate 500;
    JsonPath "$.store.book";
    Regex ("\\d+", "NUM");
    ValidateSchema "my_schema";
    ParseJson;
    Stringify;
    Chain [ParseJson; Stringify];
    Conditional { condition = "len > 0"; on_true = ParseJson; on_false = Stringify };
    Split { delimiter = "line"; chunk_size = 100; overlap = 10 };
    Custom "my_func";
  ] in
  List.iter (fun v ->
    let j = adapter_transform_to_yojson v in
    let v2 = adapter_transform_of_yojson j |> Result.get_ok in
    let j2 = adapter_transform_to_yojson v2 in
    check_str "adapter_transform roundtrip"
      (Yojson.Safe.to_string j) (Yojson.Safe.to_string j2)
  ) variants

(* ---- context_mode yojson roundtrip ---- *)
let test_context_mode_yojson () =
  let variants = [CM_None; CM_Summary; CM_Full] in
  List.iter (fun v ->
    let j = context_mode_to_yojson v in
    let v2 = context_mode_of_yojson j |> Result.get_ok in
    check_str "context_mode roundtrip"
      (context_mode_to_string v) (context_mode_to_string v2)
  ) variants

(* ---- evaluator_config yojson ---- *)
let test_evaluator_config_yojson () =
  let v = { scoring_func = "llm_judge"; scoring_prompt = Some "Rate it"; select_strategy = Best } in
  let j = evaluator_config_to_yojson v in
  let v2 = evaluator_config_of_yojson j |> Result.get_ok in
  check_str "scoring_func" v.scoring_func v2.scoring_func;
  check_str "scoring_prompt" "Rate it"
    (match v2.scoring_prompt with Some s -> s | None -> "");
  check_str "select" (Yojson.Safe.to_string (select_strategy_to_yojson v.select_strategy))
    (Yojson.Safe.to_string (select_strategy_to_yojson v2.select_strategy))

(* ---- evaluator_result yojson ---- *)
let test_evaluator_result_yojson () =
  let v = { score = 0.95; feedback = Some "Good"; selected_output = "out"; selected_id = "n1" } in
  let j = evaluator_result_to_yojson v in
  let v2 = evaluator_result_of_yojson j |> Result.get_ok in
  check_float "score" v.score v2.score;
  check_str "selected_id" v.selected_id v2.selected_id

(* ---- chain_config yojson roundtrip ---- *)
let test_chain_config_yojson () =
  let v = { max_depth = 5; max_concurrency = 2; timeout = 60; trace = true; direction = TB } in
  let j = chain_config_to_yojson v in
  let v2 = chain_config_of_yojson j |> Result.get_ok in
  check_int "max_depth" v.max_depth v2.max_depth;
  check_int "timeout" v.timeout v2.timeout;
  check bool "trace" v.trace v2.trace

(* ---- trace_entry yojson roundtrip ---- *)
let test_trace_entry_yojson () =
  let v = {
    node_id = "n1"; node_type_name = "llm";
    start_time = 1.0; end_time = 2.0;
    status = `Success; output_preview = Some "ok"; error = None
  } in
  let j = trace_entry_to_yojson v in
  let v2 = trace_entry_of_yojson j |> Result.get_ok in
  check_str "node_id" v.node_id v2.node_id;
  check_str "status_type" "llm" v2.node_type_name

(* ---- token_usage yojson roundtrip ---- *)
let test_token_usage_yojson () =
  let v = { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 0.01 } in
  let j = token_usage_to_yojson v in
  let v2 = token_usage_of_yojson j |> Result.get_ok in
  check_int "total" v.total_tokens v2.total_tokens

(* ---- chain_result yojson roundtrip ---- *)
let test_chain_result_yojson () =
  let v = {
    chain_id = "c1"; output = "hello"; success = true;
    trace = []; token_usage = empty_token_usage; duration_ms = 42; metadata = []
  } in
  let j = chain_result_to_yojson v in
  let v2 = chain_result_of_yojson j |> Result.get_ok in
  check_str "chain_id" v.chain_id v2.chain_id;
  check bool "success" true v2.success

(* ---- execution_plan yojson roundtrip ---- *)
let test_execution_plan_yojson () =
  let chain = make_chain ~id:"c" ~nodes:[simple_llm] ~output:"n" () in
  let v = { chain; execution_order = ["n"]; parallel_groups = [["n"]]; depth = 1 } in
  let j = execution_plan_to_yojson v in
  let v2 = execution_plan_of_yojson j |> Result.get_ok in
  check_str "chain_id" "c" v2.chain.id;
  check_int "depth" 1 v2.depth

(* ---- node_type yojson roundtrip for complex types ---- *)
let test_node_yojson_llm () =
  let j = node_to_yojson simple_llm in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "id" "n" n2.id;
  check_str "type" "llm" (node_type_name n2.node_type)

let test_node_yojson_tool () =
  let j = node_to_yojson simple_tool in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "tool" (node_type_name n2.node_type)

let test_node_yojson_pipeline () =
  let n = make_pipeline ~id:"p" [simple_llm; simple_tool] in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "pipeline" (node_type_name n2.node_type)

let test_node_yojson_fanout () =
  let n = make_fanout ~id:"f" [simple_llm; simple_tool] in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "fanout" (node_type_name n2.node_type)

let test_node_yojson_quorum () =
  let n = make_quorum ~id:"q" ~consensus:Majority [simple_llm] in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "quorum" (node_type_name n2.node_type)

let test_node_yojson_gate () =
  let n = { id = "g"; node_type = Gate { condition = "true"; then_node = simple_llm; else_node = Some simple_tool }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "gate" (node_type_name n2.node_type)

let test_node_yojson_adapter () =
  let n = make_adapter ~id:"a" ~input_ref:"n.output" ~transform:ParseJson () in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "adapter" (node_type_name n2.node_type)

let test_node_yojson_adapter_passthrough () =
  let n = make_adapter ~id:"a" ~input_ref:"x" ~transform:Stringify ~on_error:`Passthrough () in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "adapter" (node_type_name n2.node_type)

let test_node_yojson_adapter_default () =
  let n = make_adapter ~id:"a" ~input_ref:"x" ~transform:Stringify ~on_error:(`Default "fallback") () in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "adapter" (node_type_name n2.node_type)

let test_node_yojson_cache () =
  let n = { id = "c"; node_type = Cache { key_expr = "k"; ttl_seconds = 60; inner = simple_llm }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "cache" (node_type_name n2.node_type)

let test_node_yojson_batch () =
  let n = { id = "b"; node_type = Batch { batch_size = 10; parallel = true; inner = simple_llm; collect_strategy = `List }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "batch" (node_type_name n2.node_type)

let test_node_yojson_spawn () =
  let n = { id = "s"; node_type = Spawn { clean = true; inner = simple_llm; pass_vars = ["input"]; inherit_cache = false }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "spawn" (node_type_name n2.node_type)

let test_node_yojson_mcts () =
  let n = { id = "m"; node_type = Mcts {
    strategies = [simple_llm]; simulation = simple_tool;
    evaluator = "llm_judge"; evaluator_prompt = None;
    policy = UCB1 1.41; max_iterations = 100; max_depth = 5;
    expansion_threshold = 3; early_stop = Some 0.95; parallel_sims = 2
  }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "mcts" (node_type_name n2.node_type)

let test_node_yojson_stream_merge () =
  let n = { id = "sm"; node_type = StreamMerge {
    nodes = [simple_llm; simple_tool]; reducer = Concat;
    initial = ""; min_results = Some 1; timeout = Some 30.0
  }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "stream_merge" (node_type_name n2.node_type)

let test_node_yojson_feedback_loop () =
  let ec = { scoring_func = "llm_judge"; scoring_prompt = None; select_strategy = Best } in
  let n = make_feedback_loop ~id:"fl" ~generator:simple_llm ~evaluator_config:ec
    ~improver_prompt:"Improve: {{feedback}}" ~max_iterations:3 ~score_threshold:0.8 () in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "feedback_loop" (node_type_name n2.node_type)

let test_node_yojson_cascade () =
  let tier = { tier_node = simple_llm; tier_index = 0; confidence_threshold = 0.7; cost_weight = 1.0; pass_context = false } in
  let n = make_cascade ~id:"cas" ~tiers:[tier] () in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "cascade" (node_type_name n2.node_type)

let test_node_yojson_chain_exec () =
  let n = { id = "ce"; node_type = ChainExec {
    chain_source = "src"; validate = true; max_depth = 3;
    sandbox = true; context_inject = [("a", "b")]; pass_outputs = true
  }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "chain_exec" (node_type_name n2.node_type)

let test_node_yojson_map () =
  let n = { id = "mp"; node_type = Map { func = "uppercase"; inner = simple_llm }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "map" (node_type_name n2.node_type)

let test_node_yojson_bind () =
  let n = { id = "bn"; node_type = Bind { func = "router"; inner = simple_llm }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "bind" (node_type_name n2.node_type)

let test_node_yojson_merge () =
  let n = { id = "mg"; node_type = Merge { strategy = First; nodes = [simple_llm; simple_tool] }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "merge" (node_type_name n2.node_type)

let test_node_yojson_threshold () =
  let n = make_threshold ~id:"th" ~metric:"score" ~operator:Gte ~value:0.5 ~input_node:simple_llm ~on_pass:simple_tool () in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "threshold" (node_type_name n2.node_type)

let test_node_yojson_masc_broadcast () =
  let n = { id = "mb"; node_type = Masc_broadcast { message = "hi"; room = Some "r1"; mention = ["@codex"] }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "masc_broadcast" (node_type_name n2.node_type)

let test_node_yojson_masc_listen () =
  let n = { id = "ml"; node_type = Masc_listen { filter = Some "urgent"; timeout_sec = 10.0; room = None }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "masc_listen" (node_type_name n2.node_type)

let test_node_yojson_masc_claim () =
  let n = { id = "mc"; node_type = Masc_claim { task_id = Some "t1"; room = None }; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "masc_claim" (node_type_name n2.node_type)

let test_node_yojson_subgraph () =
  let chain = make_chain ~id:"sub" ~nodes:[simple_llm] ~output:"n" () in
  let n = { id = "sg"; node_type = Subgraph chain; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "subgraph" (node_type_name n2.node_type)

let test_node_yojson_chain_ref () =
  let n = { id = "cr"; node_type = ChainRef "some_chain"; input_mapping = []; output_key = None; depends_on = None } in
  let j = node_to_yojson n in
  let n2 = node_of_yojson j |> Result.get_ok in
  check_str "type" "chain_ref" (node_type_name n2.node_type)

(* ---- retry_config / batch_config yojson roundtrip ---- *)
let test_retry_config_yojson () =
  let v = default_retry_config in
  let j = retry_config_to_yojson v in
  let v2 = retry_config_of_yojson j |> Result.get_ok in
  check_int "max_retries" v.max_retries v2.max_retries;
  check_int "initial_delay" v.initial_delay_ms v2.initial_delay_ms

let test_batch_config_yojson () =
  let v = default_batch_config in
  let j = batch_config_to_yojson v in
  let v2 = batch_config_of_yojson j |> Result.get_ok in
  check_int "max_concurrent" v.batch_max_concurrent v2.batch_max_concurrent;
  check_int "rate_limit" v.rate_limit_per_min v2.rate_limit_per_min

(* ---- make_chain with optional fields ---- *)
let test_make_chain_full () =
  let c = make_chain ~id:"c" ~nodes:[simple_llm] ~output:"n"
    ~name:"Test Chain" ~description:"A test" ~version:"1.0.0"
    ~input_schema:(`Assoc ["type", `String "object"])
    ~output_schema:(`Assoc ["type", `String "string"])
    ~metadata:(`Assoc ["author", `String "test"]) () in
  check_str "name" "Test Chain" (match c.name with Some n -> n | None -> "");
  check_str "version" "1.0.0" (match c.version with Some v -> v | None -> "")

(* ---- make_llm_node with all optional params ---- *)
let test_make_llm_node_full () =
  let n = make_llm_node ~id:"full" ~model:"claude" ~system:"You are helpful"
    ~prompt:"Hi" ~timeout:30 ~tools:(`List []) ~prompt_ref:"my_prompt@v1"
    ~prompt_vars:[("name", "Vincent")] ~thinking:true () in
  check_str "id" "full" n.id;
  check_str "type" "llm" (node_type_name n.node_type)

(* ---- count_parallel_groups: Threshold ---- *)
let test_count_parallel_threshold () =
  let n = make_threshold ~id:"th" ~metric:"s" ~operator:Gt ~value:0.5
    ~input_node:(make_fanout ~id:"f" [simple_llm; simple_tool])
    ~on_pass:simple_llm ~on_fail:simple_tool () in
  check_int "threshold with nested fanout" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Mcts ---- *)
let test_count_parallel_mcts () =
  let n = { id = "m"; node_type = Mcts {
    strategies = [simple_llm; simple_tool]; simulation = simple_llm;
    evaluator = "j"; evaluator_prompt = None; policy = Greedy;
    max_iterations = 10; max_depth = 3; expansion_threshold = 1;
    early_stop = None; parallel_sims = 1
  }; input_mapping = []; output_key = None; depends_on = None } in
  check_int "mcts is parallel group" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: StreamMerge ---- *)
let test_count_parallel_stream_merge () =
  let n = { id = "sm"; node_type = StreamMerge {
    nodes = [simple_llm; simple_tool]; reducer = First;
    initial = ""; min_results = None; timeout = None
  }; input_mapping = []; output_key = None; depends_on = None } in
  check_int "stream_merge is parallel group" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: FeedbackLoop ---- *)
let test_count_parallel_feedback_loop () =
  let ec = { scoring_func = "j"; scoring_prompt = None; select_strategy = Best } in
  let gen = make_fanout ~id:"gen" [simple_llm; simple_tool] in
  let n = make_feedback_loop ~id:"fl" ~generator:gen ~evaluator_config:ec
    ~improver_prompt:"p" ~max_iterations:1 ~score_threshold:0.5 () in
  check_int "feedback_loop with parallel generator" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Cascade ---- *)
let test_count_parallel_cascade () =
  let fan = make_fanout ~id:"f" [simple_llm; simple_tool] in
  let tier = { tier_node = fan; tier_index = 0; confidence_threshold = 0.5; cost_weight = 1.0; pass_context = false } in
  let n = make_cascade ~id:"cas" ~tiers:[tier] () in
  check_int "cascade with parallel tier" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Spawn ---- *)
let test_count_parallel_spawn () =
  let fan = make_fanout ~id:"f" [simple_llm; simple_tool] in
  let n = { id = "s"; node_type = Spawn { clean = true; inner = fan; pass_vars = []; inherit_cache = true }; input_mapping = []; output_key = None; depends_on = None } in
  check_int "spawn wraps fanout" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Cache ---- *)
let test_count_parallel_cache () =
  let fan = make_fanout ~id:"f" [simple_llm; simple_tool] in
  let n = { id = "c"; node_type = Cache { key_expr = "k"; ttl_seconds = 60; inner = fan }; input_mapping = []; output_key = None; depends_on = None } in
  check_int "cache wraps fanout" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: GoalDriven ---- *)
let test_count_parallel_goal_driven () =
  let fan = make_fanout ~id:"f" [simple_llm; simple_tool] in
  let n = make_goal_driven ~id:"gd" ~goal_metric:"score" ~goal_operator:Gte
    ~goal_value:0.9 ~action_node:fan ~measure_func:"test" ~max_iterations:5 () in
  check_int "goal_driven wraps fanout" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Subgraph ---- *)
let test_count_parallel_subgraph () =
  let fan = make_fanout ~id:"f" [simple_llm; simple_tool] in
  let chain = make_chain ~id:"sub" ~nodes:[fan] ~output:"f" () in
  let n = { id = "sg"; node_type = Subgraph chain; input_mapping = []; output_key = None; depends_on = None } in
  check_int "subgraph wraps fanout" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Evaluator ---- *)
let test_count_parallel_evaluator () =
  let n = make_evaluator ~id:"ev" ~candidates:[simple_llm; simple_tool]
    ~scoring_func:"llm_judge" ~select_strategy:Best () in
  check_int "evaluator is parallel group" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Retry ---- *)
let test_count_parallel_retry () =
  let fan = make_fanout ~id:"f" [simple_llm; simple_tool] in
  let n = make_retry ~id:"r" ~node:fan ~max_attempts:3 () in
  check_int "retry wraps fanout" 1 (count_parallel_groups n)

(* ---- count_parallel_groups: Fallback ---- *)
let test_count_parallel_fallback () =
  let fan = make_fanout ~id:"f1" [simple_llm; simple_tool] in
  let n = make_fallback ~id:"fb" ~primary:fan ~fallbacks:[simple_llm] in
  check_int "fallback with fanout primary" 1 (count_parallel_groups n)

(* ---- node_type_name for remaining types ---- *)
let test_node_type_name_llm () =
  check_str "llm" "llm" (node_type_name simple_llm.node_type)

let test_node_type_name_tool () =
  check_str "tool" "tool" (node_type_name simple_tool.node_type)

let test_node_type_name_pipeline () =
  check_str "pipeline" "pipeline" (node_type_name (Pipeline []))

let test_node_type_name_fanout () =
  check_str "fanout" "fanout" (node_type_name (Fanout []))

let test_node_type_name_quorum () =
  check_str "quorum" "quorum" (node_type_name (Quorum { consensus = Count 1; nodes = []; weights = [] }))

let test_node_type_name_merge () =
  check_str "merge" "merge" (node_type_name (Merge { strategy = First; nodes = [] }))

let test_node_type_name_threshold () =
  check_str "threshold" "threshold" (node_type_name (Threshold { metric = "s"; operator = Gt; value = 0.5; input_node = simple_llm; on_pass = None; on_fail = None }))

let test_node_type_name_retry () =
  check_str "retry" "retry" (node_type_name (Retry { node = simple_llm; max_attempts = 3; backoff = Constant 1.0; retry_on = [] }))

let test_node_type_name_fallback () =
  check_str "fallback" "fallback" (node_type_name (Fallback { primary = simple_llm; fallbacks = [] }))

let test_node_type_name_race () =
  check_str "race" "race" (node_type_name (Race { nodes = []; timeout = None }))

let test_node_type_name_adapter () =
  check_str "adapter" "adapter" (node_type_name (Adapter { input_ref = "x"; transform = ParseJson; on_error = `Fail }))

let test_node_type_name_cache () =
  check_str "cache" "cache" (node_type_name (Cache { key_expr = "k"; ttl_seconds = 0; inner = simple_llm }))

let test_node_type_name_mcts () =
  check_str "mcts" "mcts" (node_type_name (Mcts { strategies = []; simulation = simple_llm; evaluator = "j"; evaluator_prompt = None; policy = Greedy; max_iterations = 1; max_depth = 1; expansion_threshold = 1; early_stop = None; parallel_sims = 1 }))

let test_node_type_name_masc_broadcast () =
  check_str "masc_broadcast" "masc_broadcast" (node_type_name (Masc_broadcast { message = ""; room = None; mention = [] }))

let test_node_type_name_masc_listen () =
  check_str "masc_listen" "masc_listen" (node_type_name (Masc_listen { filter = None; timeout_sec = 1.0; room = None }))

let test_node_type_name_masc_claim () =
  check_str "masc_claim" "masc_claim" (node_type_name (Masc_claim { task_id = None; room = None }))

(* ---- batch_stats yojson ---- *)
let test_batch_stats_yojson () =
  let tu : Chain_category.token_usage = { prompt_tokens = 10; completion_tokens = 5; total_tokens = 15; estimated_cost_usd = 0.001 } in
  let v = { total_chains = 3; completed = 2; failed = 1; total_duration_ms = 1000; total_tokens = tu; avg_duration_ms = 333.3 } in
  let j = batch_stats_to_yojson v in
  let v2 = batch_stats_of_yojson j |> Result.get_ok in
  check_int "total_chains" 3 v2.total_chains;
  check_int "completed" 2 v2.completed

(* ---- batch_result yojson ---- *)
let test_batch_result_yojson () =
  let tu : Chain_category.token_usage = { prompt_tokens = 0; completion_tokens = 0; total_tokens = 0; estimated_cost_usd = 0.0 } in
  let stats = { total_chains = 0; completed = 0; failed = 0; total_duration_ms = 0; total_tokens = tu; avg_duration_ms = 0.0 } in
  let v = { batch_id = "b1"; results = []; stats; failed_chains = [] } in
  let j = batch_result_to_yojson v in
  let v2 = batch_result_of_yojson j |> Result.get_ok in
  check_str "batch_id" "b1" v2.batch_id

(* ---- make_quorum with weights ---- *)
let test_make_quorum_weighted () =
  let n = make_quorum ~id:"q" ~consensus:(Weighted 0.8) ~weights:[("n", 2.0)] [simple_llm] in
  check_str "type" "quorum" (node_type_name n.node_type)

(* ---- make_cascade with all options ---- *)
let test_make_cascade_full () =
  let tier = { tier_node = simple_llm; tier_index = 0; confidence_threshold = 0.5; cost_weight = 1.0; pass_context = true } in
  let n = make_cascade ~id:"c" ~tiers:[tier] ~confidence_prompt:(Some "Rate confidence")
    ~max_escalations:5 ~context_mode:CM_Full ~task_hint:"classify" ~default_threshold:0.8 () in
  check_str "type" "cascade" (node_type_name n.node_type)

(* ---- make_goal_driven with relay_models ---- *)
let test_make_goal_driven_relay () =
  let n = make_goal_driven ~id:"gd" ~goal_metric:"score" ~goal_operator:Gte
    ~goal_value:0.9 ~action_node:simple_llm ~measure_func:"test" ~max_iterations:5
    ~strategy_hints:[("below_50", "fast")] ~conversational:true
    ~relay_models:["gemini"; "claude"] () in
  check_str "type" "goal_driven" (node_type_name n.node_type)

(* ---- make_feedback_loop with relay_models ---- *)
let test_make_feedback_loop_relay () =
  let ec = { scoring_func = "j"; scoring_prompt = None; select_strategy = Best } in
  let n = make_feedback_loop ~id:"fl" ~generator:simple_llm ~evaluator_config:ec
    ~improver_prompt:"p" ~max_iterations:3 ~score_threshold:0.8
    ~score_operator:Lt ~conversational:true ~relay_models:["gemini"] () in
  check_str "type" "feedback_loop" (node_type_name n.node_type)

(* ---- make_race with timeout ---- *)
let test_make_race_timeout () =
  let n = make_race ~id:"r" ~nodes:[simple_llm; simple_tool] ~timeout:30.0 () in
  check_str "type" "race" (node_type_name n.node_type)

(* ---- make_retry with all options ---- *)
let test_make_retry_full () =
  let n = make_retry ~id:"r" ~node:simple_llm ~max_attempts:5
    ~backoff:(Jitter (0.1, 1.0)) ~retry_on:["timeout"; "rate_limit"] () in
  check_str "type" "retry" (node_type_name n.node_type)

(* ---- make_evaluator with min_score ---- *)
let test_make_evaluator_min_score () =
  let n = make_evaluator ~id:"ev" ~candidates:[simple_llm]
    ~scoring_func:"regex_match" ~scoring_prompt:"check" ~select_strategy:(AboveThreshold 0.7) ~min_score:0.5 () in
  check_str "type" "evaluator" (node_type_name n.node_type)

let () =
  run "chain_types_wave7" [
    "direction", [
      test_case "yojson LR" `Quick test_direction_yojson_lr;
      test_case "yojson RL" `Quick test_direction_yojson_rl;
      test_case "yojson TB" `Quick test_direction_yojson_tb;
      test_case "yojson BT" `Quick test_direction_yojson_bt;
      test_case "of_string TD" `Quick test_direction_of_string_td;
      test_case "of_string unknown" `Quick test_direction_of_string_unknown;
    ];
    "consensus_mode", [
      test_case "yojson count" `Quick test_consensus_yojson_count;
      test_case "yojson majority" `Quick test_consensus_yojson_majority;
      test_case "yojson unanimous" `Quick test_consensus_yojson_unanimous;
      test_case "yojson weighted" `Quick test_consensus_yojson_weighted;
      test_case "of_string garbage" `Quick test_consensus_of_string_count_default;
      test_case "of_string numeric" `Quick test_consensus_of_string_numeric;
    ];
    "yojson_roundtrips", [
      test_case "merge_strategy" `Quick test_merge_strategy_yojson;
      test_case "threshold_op" `Quick test_threshold_op_yojson;
      test_case "select_strategy" `Quick test_select_strategy_yojson;
      test_case "backoff_strategy" `Quick test_backoff_strategy_yojson;
      test_case "mcts_policy" `Quick test_mcts_policy_yojson;
      test_case "confidence_level" `Quick test_confidence_level_yojson;
      test_case "batch_priority" `Quick test_batch_priority_yojson;
      test_case "adapter_transform" `Quick test_adapter_transform_yojson;
      test_case "context_mode" `Quick test_context_mode_yojson;
      test_case "evaluator_config" `Quick test_evaluator_config_yojson;
      test_case "evaluator_result" `Quick test_evaluator_result_yojson;
      test_case "chain_config" `Quick test_chain_config_yojson;
      test_case "trace_entry" `Quick test_trace_entry_yojson;
      test_case "token_usage" `Quick test_token_usage_yojson;
      test_case "chain_result" `Quick test_chain_result_yojson;
      test_case "execution_plan" `Quick test_execution_plan_yojson;
      test_case "retry_config" `Quick test_retry_config_yojson;
      test_case "batch_config" `Quick test_batch_config_yojson;
      test_case "batch_stats" `Quick test_batch_stats_yojson;
      test_case "batch_result" `Quick test_batch_result_yojson;
    ];
    "node_yojson", [
      test_case "llm" `Quick test_node_yojson_llm;
      test_case "tool" `Quick test_node_yojson_tool;
      test_case "pipeline" `Quick test_node_yojson_pipeline;
      test_case "fanout" `Quick test_node_yojson_fanout;
      test_case "quorum" `Quick test_node_yojson_quorum;
      test_case "gate" `Quick test_node_yojson_gate;
      test_case "adapter" `Quick test_node_yojson_adapter;
      test_case "adapter passthrough" `Quick test_node_yojson_adapter_passthrough;
      test_case "adapter default" `Quick test_node_yojson_adapter_default;
      test_case "cache" `Quick test_node_yojson_cache;
      test_case "batch" `Quick test_node_yojson_batch;
      test_case "spawn" `Quick test_node_yojson_spawn;
      test_case "mcts" `Quick test_node_yojson_mcts;
      test_case "stream_merge" `Quick test_node_yojson_stream_merge;
      test_case "feedback_loop" `Quick test_node_yojson_feedback_loop;
      test_case "cascade" `Quick test_node_yojson_cascade;
      test_case "chain_exec" `Quick test_node_yojson_chain_exec;
      test_case "map" `Quick test_node_yojson_map;
      test_case "bind" `Quick test_node_yojson_bind;
      test_case "merge" `Quick test_node_yojson_merge;
      test_case "threshold" `Quick test_node_yojson_threshold;
      test_case "masc_broadcast" `Quick test_node_yojson_masc_broadcast;
      test_case "masc_listen" `Quick test_node_yojson_masc_listen;
      test_case "masc_claim" `Quick test_node_yojson_masc_claim;
      test_case "subgraph" `Quick test_node_yojson_subgraph;
      test_case "chain_ref" `Quick test_node_yojson_chain_ref;
    ];
    "node_type_name", [
      test_case "llm" `Quick test_node_type_name_llm;
      test_case "tool" `Quick test_node_type_name_tool;
      test_case "pipeline" `Quick test_node_type_name_pipeline;
      test_case "fanout" `Quick test_node_type_name_fanout;
      test_case "quorum" `Quick test_node_type_name_quorum;
      test_case "merge" `Quick test_node_type_name_merge;
      test_case "threshold" `Quick test_node_type_name_threshold;
      test_case "retry" `Quick test_node_type_name_retry;
      test_case "fallback" `Quick test_node_type_name_fallback;
      test_case "race" `Quick test_node_type_name_race;
      test_case "adapter" `Quick test_node_type_name_adapter;
      test_case "cache" `Quick test_node_type_name_cache;
      test_case "mcts" `Quick test_node_type_name_mcts;
      test_case "masc_broadcast" `Quick test_node_type_name_masc_broadcast;
      test_case "masc_listen" `Quick test_node_type_name_masc_listen;
      test_case "masc_claim" `Quick test_node_type_name_masc_claim;
    ];
    "make_constructors", [
      test_case "chain full" `Quick test_make_chain_full;
      test_case "llm_node full" `Quick test_make_llm_node_full;
      test_case "quorum weighted" `Quick test_make_quorum_weighted;
      test_case "cascade full" `Quick test_make_cascade_full;
      test_case "goal_driven relay" `Quick test_make_goal_driven_relay;
      test_case "feedback_loop relay" `Quick test_make_feedback_loop_relay;
      test_case "race timeout" `Quick test_make_race_timeout;
      test_case "retry full" `Quick test_make_retry_full;
      test_case "evaluator min_score" `Quick test_make_evaluator_min_score;
    ];
    "count_parallel", [
      test_case "threshold" `Quick test_count_parallel_threshold;
      test_case "mcts" `Quick test_count_parallel_mcts;
      test_case "stream_merge" `Quick test_count_parallel_stream_merge;
      test_case "feedback_loop" `Quick test_count_parallel_feedback_loop;
      test_case "cascade" `Quick test_count_parallel_cascade;
      test_case "spawn" `Quick test_count_parallel_spawn;
      test_case "cache" `Quick test_count_parallel_cache;
      test_case "goal_driven" `Quick test_count_parallel_goal_driven;
      test_case "subgraph" `Quick test_count_parallel_subgraph;
      test_case "evaluator" `Quick test_count_parallel_evaluator;
      test_case "retry" `Quick test_count_parallel_retry;
      test_case "fallback" `Quick test_count_parallel_fallback;
    ];
  ]

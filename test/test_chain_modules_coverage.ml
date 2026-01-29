(** Chain Modules Coverage Tests

    Comprehensive tests for chain engine modules:
    - chain_types.ml: Type constructors and serialization
    - chain_adapter_eio.ml: All adapter transforms
    - chain_orchestrator_eio.ml: Orchestration with mock chains
    - chain_batch.ml: Batch execution
    - chain_category.ml: Category theory abstractions
    - chain_composer.ml: Chain composition

    @author Test Coverage Agent
    @since 2026-01
*)

open Chain_types
open Chain_category

(* ============================================================
   Test Utilities
   ============================================================ *)

let check_ok msg = function
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "%s: %s" msg e)

let check_error msg = function
  | Ok _ -> Alcotest.fail (Printf.sprintf "%s: Expected error but got Ok" msg)
  | Error _ -> ()

let json_roundtrip to_json of_json value =
  let json = to_json value in
  match of_json json with
  | Ok v -> v
  | Error e -> Alcotest.fail (Printf.sprintf "JSON roundtrip failed: %s" e)

(* ============================================================
   Chain Types Tests
   ============================================================ *)

let test_direction_serialization () =
  let directions = [LR; RL; TB; BT] in
  List.iter (fun d ->
    let s = direction_to_string d in
    let d' = direction_of_string s in
    Alcotest.(check string) "direction roundtrip" (direction_to_string d) (direction_to_string d')
  ) directions;
  (* Test TD alias *)
  Alcotest.(check string) "TD alias" "TB" (direction_to_string (direction_of_string "TD"))

let test_direction_yojson () =
  let directions = [LR; RL; TB; BT] in
  List.iter (fun d ->
    let d' = json_roundtrip direction_to_yojson direction_of_yojson d in
    Alcotest.(check string) "direction yojson" (direction_to_string d) (direction_to_string d')
  ) directions

let test_merge_strategy_serialization () =
  let strategies = [First; Last; Concat; WeightedAvg; Custom "my_merge"] in
  List.iter (fun s ->
    let _ = json_roundtrip merge_strategy_to_yojson merge_strategy_of_yojson s in ()
  ) strategies

let test_threshold_op_serialization () =
  let ops = [Gt; Gte; Lt; Lte; Eq; Neq] in
  List.iter (fun op ->
    let _ = json_roundtrip threshold_op_to_yojson threshold_op_of_yojson op in ()
  ) ops

let test_select_strategy_serialization () =
  let strategies = [Best; Worst; AboveThreshold 0.8; WeightedRandom] in
  List.iter (fun s ->
    let _ = json_roundtrip select_strategy_to_yojson select_strategy_of_yojson s in ()
  ) strategies

let test_backoff_strategy_serialization () =
  let strategies = [
    Constant 1.0;
    Exponential 2.0;
    Linear 0.5;
    Jitter (0.1, 0.5);
  ] in
  List.iter (fun s ->
    let _ = json_roundtrip backoff_strategy_to_yojson backoff_strategy_of_yojson s in ()
  ) strategies

let test_adapter_transform_serialization () =
  let transforms = [
    Extract "data.field";
    Template "Result: {{value}}";
    Summarize 100;
    Truncate 500;
    JsonPath "$.data.items[0]";
    Regex ("\\d+", "NUM");
    ValidateSchema {|{"type":"object"}|};
    ParseJson;
    Stringify;
    Chain [Extract "a"; Template "{{value}}"];
    Conditional { condition = "contains:test"; on_true = Extract "a"; on_false = Extract "b" };
    Split { delimiter = "line"; chunk_size = 100; overlap = 10 };
    Custom "identity";
  ] in
  List.iter (fun t ->
    let _ = json_roundtrip adapter_transform_to_yojson adapter_transform_of_yojson t in ()
  ) transforms

let test_mcts_policy_serialization () =
  let policies = [
    UCB1 1.41;
    Greedy;
    EpsilonGreedy 0.1;
    Softmax 0.5;
  ] in
  List.iter (fun p ->
    let _ = json_roundtrip mcts_policy_to_yojson mcts_policy_of_yojson p in ()
  ) policies

let test_evaluator_config_serialization () =
  let config = {
    scoring_func = "llm_judge";
    scoring_prompt = Some "Rate this output 0-1";
    select_strategy = Best;
  } in
  let _ = json_roundtrip evaluator_config_to_yojson evaluator_config_of_yojson config in ()

let test_chain_config_serialization () =
  let config = { default_config with max_depth = 10; trace = true } in
  let config' = json_roundtrip chain_config_to_yojson chain_config_of_yojson config in
  Alcotest.(check int) "max_depth" 10 config'.max_depth;
  Alcotest.(check bool) "trace" true config'.trace

let test_make_chain () =
  let node = make_llm_node ~id:"test" ~model:"gemini" ~prompt:"Hello" () in
  let chain = make_chain ~id:"test-chain" ~nodes:[node] ~output:"test" () in
  Alcotest.(check string) "chain id" "test-chain" chain.id;
  Alcotest.(check int) "nodes count" 1 (List.length chain.nodes);
  Alcotest.(check string) "output" "test" chain.output

let test_make_chain_with_metadata () =
  let node = make_llm_node ~id:"n1" ~model:"claude" ~prompt:"Test" () in
  let chain = make_chain
    ~id:"meta-chain"
    ~nodes:[node]
    ~output:"n1"
    ~name:"Test Chain"
    ~description:"A test chain"
    ~version:"1.0.0"
    () in
  Alcotest.(check (option string)) "name" (Some "Test Chain") chain.name;
  Alcotest.(check (option string)) "description" (Some "A test chain") chain.description;
  Alcotest.(check (option string)) "version" (Some "1.0.0") chain.version

let test_node_type_name () =
  let cases = [
    (Llm { model="g"; system=None; prompt="p"; timeout=None; tools=None; prompt_ref=None; prompt_vars=[]; thinking=false }, "llm");
    (Tool { name="t"; args=`Null }, "tool");
    (Pipeline [], "pipeline");
    (Fanout [], "fanout");
    (Quorum { required=2; nodes=[] }, "quorum");
    (ChainRef "ref", "chain_ref");
    (Cache { key_expr="k"; ttl_seconds=60; inner={id="x";node_type=Pipeline [];input_mapping=[];output_key=None;depends_on=None} }, "cache");
    (Batch { batch_size=10; parallel=true; inner={id="x";node_type=Pipeline [];input_mapping=[];output_key=None;depends_on=None}; collect_strategy=`List }, "batch");
  ] in
  List.iter (fun (nt, expected) ->
    Alcotest.(check string) "node_type_name" expected (node_type_name nt)
  ) cases

let test_token_usage () =
  let usage = { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 0.01 } in
  let usage' = json_roundtrip token_usage_to_yojson token_usage_of_yojson usage in
  Alcotest.(check int) "prompt_tokens" 100 usage'.prompt_tokens;
  Alcotest.(check int) "total_tokens" 150 usage'.total_tokens

let test_empty_token_usage () =
  Alcotest.(check int) "empty prompt" 0 empty_token_usage.prompt_tokens;
  Alcotest.(check int) "empty completion" 0 empty_token_usage.completion_tokens

let test_trace_entry_serialization () =
  let entry = {
    node_id = "test";
    node_type_name = "llm";
    start_time = 1000.0;
    end_time = 1001.5;
    status = `Success;
    output_preview = Some "output";
    error = None;
  } in
  let _ = json_roundtrip trace_entry_to_yojson trace_entry_of_yojson entry in ()

let test_chain_result_serialization () =
  let result = {
    chain_id = "test";
    output = "result";
    success = true;
    trace = [];
    token_usage = empty_token_usage;
    duration_ms = 100;
    metadata = [("key", "value")];
  } in
  let result' = json_roundtrip chain_result_to_yojson chain_result_of_yojson result in
  Alcotest.(check bool) "success" true result'.success;
  Alcotest.(check string) "output" "result" result'.output

let test_execution_plan_serialization () =
  let node = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:"test" () in
  let chain = make_chain ~id:"c1" ~nodes:[node] ~output:"n1" () in
  let plan = {
    chain;
    execution_order = ["n1"];
    parallel_groups = [["n1"]];
    depth = 1;
  } in
  let plan' = json_roundtrip execution_plan_to_yojson execution_plan_of_yojson plan in
  Alcotest.(check int) "depth" 1 plan'.depth

let test_batch_types () =
  (* batch_priority *)
  let priorities = [High; Normal; Low] in
  List.iter (fun p ->
    let _ = json_roundtrip batch_priority_to_yojson batch_priority_of_yojson p in ()
  ) priorities;

  (* retry_config *)
  let _ = json_roundtrip retry_config_to_yojson retry_config_of_yojson default_retry_config in

  (* batch_config *)
  let _ = json_roundtrip batch_config_to_yojson batch_config_of_yojson default_batch_config in
  ()

let test_helper_functions () =
  (* make_tool_node *)
  let tool = make_tool_node ~id:"t1" ~name:"test_tool" ~args:(`Assoc [("arg", `String "val")]) in
  Alcotest.(check string) "tool id" "t1" tool.id;

  (* make_pipeline *)
  let n1 = make_llm_node ~id:"a" ~model:"g" ~prompt:"p" () in
  let pipeline = make_pipeline ~id:"pipe" [n1] in
  (match pipeline.node_type with
   | Pipeline nodes -> Alcotest.(check int) "pipeline nodes" 1 (List.length nodes)
   | _ -> Alcotest.fail "Expected Pipeline");

  (* make_fanout *)
  let fanout = make_fanout ~id:"fan" [n1] in
  (match fanout.node_type with
   | Fanout nodes -> Alcotest.(check int) "fanout nodes" 1 (List.length nodes)
   | _ -> Alcotest.fail "Expected Fanout");

  (* make_quorum *)
  let quorum = make_quorum ~id:"q" ~required:2 [n1] in
  (match quorum.node_type with
   | Quorum { required; _ } -> Alcotest.(check int) "quorum required" 2 required
   | _ -> Alcotest.fail "Expected Quorum");

  (* make_retry *)
  let retry = make_retry ~id:"r" ~node:n1 ~max_attempts:3 () in
  (match retry.node_type with
   | Retry { max_attempts; _ } -> Alcotest.(check int) "retry attempts" 3 max_attempts
   | _ -> Alcotest.fail "Expected Retry");

  (* make_fallback *)
  let fallback = make_fallback ~id:"fb" ~primary:n1 ~fallbacks:[n1] in
  (match fallback.node_type with
   | Fallback { fallbacks; _ } -> Alcotest.(check int) "fallback count" 1 (List.length fallbacks)
   | _ -> Alcotest.fail "Expected Fallback");

  (* make_race *)
  let race = make_race ~id:"race" ~nodes:[n1] ~timeout:5.0 () in
  (match race.node_type with
   | Race { timeout; _ } -> Alcotest.(check (option (float 0.01))) "race timeout" (Some 5.0) timeout
   | _ -> Alcotest.fail "Expected Race")

(* ============================================================
   Chain Adapter Tests
   ============================================================ *)

let test_extract_transform () =
  let input = {|{"data":{"name":"test","value":42}}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (Extract "data.name") input in
  check_ok "extract" result;
  match result with
  | Ok v -> Alcotest.(check string) "extracted value" "\"test\"" v
  | Error _ -> ()

let test_extract_array_index () =
  let input = {|{"items":["a","b","c"]}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (Extract "items.[1]") input in
  check_ok "extract array" result;
  match result with
  | Ok v -> Alcotest.(check string) "array value" "\"b\"" v
  | Error _ -> ()

let test_extract_not_found () =
  let input = {|{"data":{}}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (Extract "data.missing") input in
  check_error "extract missing" result

let test_template_transform () =
  let input = "hello" in
  let result = Chain_adapter_eio.apply_adapter_transform (Template "Result: {{value}}!") input in
  check_ok "template" result;
  match result with
  | Ok v -> Alcotest.(check string) "template result" "Result: hello!" v
  | Error _ -> ()

let test_summarize_transform () =
  let long_text = String.make 500 'x' in
  let result = Chain_adapter_eio.apply_adapter_transform (Summarize 50) long_text in
  check_ok "summarize" result;
  match result with
  | Ok v ->
    Alcotest.(check bool) "summarized shorter" true (String.length v < String.length long_text);
    Alcotest.(check bool) "ends with ellipsis" true (String.sub v (String.length v - 3) 3 = "...")
  | Error _ -> ()

let test_summarize_short_text () =
  let short_text = "short" in
  let result = Chain_adapter_eio.apply_adapter_transform (Summarize 100) short_text in
  check_ok "summarize short" result;
  match result with
  | Ok v -> Alcotest.(check string) "unchanged" short_text v
  | Error _ -> ()

let test_truncate_transform () =
  let long_text = String.make 100 'y' in
  let result = Chain_adapter_eio.apply_adapter_transform (Truncate 20) long_text in
  check_ok "truncate" result;
  match result with
  | Ok v ->
    Alcotest.(check int) "truncated length" 23 (String.length v); (* 20 + "..." *)
    Alcotest.(check bool) "ends with ellipsis" true (String.sub v 20 3 = "...")
  | Error _ -> ()

let test_jsonpath_transform () =
  let input = {|{"data":{"items":[1,2,3]}}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (JsonPath "$.data.items") input in
  check_ok "jsonpath" result;
  match result with
  | Ok v -> Alcotest.(check string) "jsonpath result" "[1,2,3]" v
  | Error _ -> ()

let test_jsonpath_root () =
  let input = {|{"a":1}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (JsonPath "$") input in
  check_ok "jsonpath root" result;
  match result with
  | Ok v -> Alcotest.(check string) "root unchanged" input v
  | Error _ -> ()

let test_regex_transform () =
  let input = "Hello 123 World 456" in
  let result = Chain_adapter_eio.apply_adapter_transform (Regex ("\\([0-9]+\\)", "NUM")) input in
  check_ok "regex" result;
  match result with
  | Ok v -> Alcotest.(check string) "regex result" "Hello NUM World NUM" v
  | Error _ -> ()

let test_validate_schema_object () =
  let input = {|{"name":"test","age":25}|} in
  let schema = {|{"type":"object","required":["name"]}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (ValidateSchema schema) input in
  check_ok "schema object" result

let test_validate_schema_type_mismatch () =
  let input = {|"not an object"|} in
  let schema = {|{"type":"object"}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (ValidateSchema schema) input in
  check_error "schema type mismatch" result

let test_validate_schema_missing_required () =
  let input = {|{"age":25}|} in
  let schema = {|{"type":"object","required":["name"]}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (ValidateSchema schema) input in
  check_error "schema missing required" result

let test_validate_schema_simple_type () =
  let input = {|"hello"|} in
  let result = Chain_adapter_eio.apply_adapter_transform (ValidateSchema "string") input in
  check_ok "simple type" result

let test_validate_schema_enum () =
  let input = {|"a"|} in
  let schema = {|{"enum":["a","b","c"]}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (ValidateSchema schema) input in
  check_ok "enum valid" result

let test_validate_schema_string_length () =
  let input = {|"hello"|} in
  let schema = {|{"type":"string","minLength":3,"maxLength":10}|} in
  let result = Chain_adapter_eio.apply_adapter_transform (ValidateSchema schema) input in
  check_ok "string length valid" result

let test_parse_json_valid () =
  let input = {|{"valid":"json"}|} in
  let result = Chain_adapter_eio.apply_adapter_transform ParseJson input in
  check_ok "parse valid json" result

let test_parse_json_invalid () =
  let input = "not valid json {" in
  let result = Chain_adapter_eio.apply_adapter_transform ParseJson input in
  check_error "parse invalid json" result

let test_stringify_non_json () =
  let input = "plain text" in
  let result = Chain_adapter_eio.apply_adapter_transform Stringify input in
  check_ok "stringify" result;
  match result with
  | Ok v -> Alcotest.(check string) "stringified" "\"plain text\"" v
  | Error _ -> ()

let test_stringify_already_json () =
  let input = {|{"a":1}|} in
  let result = Chain_adapter_eio.apply_adapter_transform Stringify input in
  check_ok "stringify json" result;
  match result with
  | Ok v -> Alcotest.(check string) "unchanged json" input v
  | Error _ -> ()

let test_chain_transforms () =
  let input = {|{"data":"hello"}|} in
  let chain = Chain [
    Extract "data";
    Template "Result: {{value}}";
  ] in
  let result = Chain_adapter_eio.apply_adapter_transform chain input in
  check_ok "chain transforms" result;
  match result with
  | Ok v -> Alcotest.(check string) "chained result" "Result: \"hello\"" v
  | Error _ -> ()

let test_chain_transforms_error_propagation () =
  let input = {|{}|} in
  let chain = Chain [
    Extract "missing";
    Template "{{value}}";
  ] in
  let result = Chain_adapter_eio.apply_adapter_transform chain input in
  check_error "chain error propagation" result

let test_conditional_contains () =
  let input = "this contains test word" in
  let cond = Conditional {
    condition = "contains:test";
    on_true = Template "FOUND: {{value}}";
    on_false = Template "NOT FOUND: {{value}}";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional contains" result;
  match result with
  | Ok v -> Alcotest.(check bool) "contains found" true (String.sub v 0 5 = "FOUND")
  | Error _ -> ()

let test_conditional_eq () =
  let input = "exact" in
  let cond = Conditional {
    condition = "eq:exact";
    on_true = Template "MATCH";
    on_false = Template "NO MATCH";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional eq" result;
  match result with
  | Ok v -> Alcotest.(check string) "eq match" "MATCH" v
  | Error _ -> ()

let test_conditional_neq () =
  let input = "something" in
  let cond = Conditional {
    condition = "neq:exact";
    on_true = Template "DIFFERENT";
    on_false = Template "SAME";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional neq" result;
  match result with
  | Ok v -> Alcotest.(check string) "neq match" "DIFFERENT" v
  | Error _ -> ()

let test_conditional_gt () =
  let input = "10.5" in
  let cond = Conditional {
    condition = "gt:5.0";
    on_true = Template "GREATER";
    on_false = Template "NOT GREATER";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional gt" result;
  match result with
  | Ok v -> Alcotest.(check string) "gt result" "GREATER" v
  | Error _ -> ()

let test_conditional_gte () =
  let input = "5.0" in
  let cond = Conditional {
    condition = "gte:5.0";
    on_true = Template "GTE";
    on_false = Template "LT";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional gte" result;
  match result with
  | Ok v -> Alcotest.(check string) "gte result" "GTE" v
  | Error _ -> ()

let test_conditional_lt () =
  let input = "3" in
  let cond = Conditional {
    condition = "lt:5";
    on_true = Template "LESS";
    on_false = Template "NOT LESS";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional lt" result;
  match result with
  | Ok v -> Alcotest.(check string) "lt result" "LESS" v
  | Error _ -> ()

let test_conditional_empty () =
  let input = "   " in
  let cond = Conditional {
    condition = "empty";
    on_true = Template "EMPTY";
    on_false = Template "NOT EMPTY";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional empty" result;
  match result with
  | Ok v -> Alcotest.(check string) "empty result" "EMPTY" v
  | Error _ -> ()

let test_conditional_nonempty () =
  let input = "content" in
  let cond = Conditional {
    condition = "nonempty";
    on_true = Template "HAS CONTENT";
    on_false = Template "NO CONTENT";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional nonempty" result;
  match result with
  | Ok v -> Alcotest.(check string) "nonempty result" "HAS CONTENT" v
  | Error _ -> ()

let test_conditional_startswith () =
  let input = "hello world" in
  let cond = Conditional {
    condition = "startswith:hello";
    on_true = Template "STARTS";
    on_false = Template "NOT STARTS";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional startswith" result;
  match result with
  | Ok v -> Alcotest.(check string) "startswith result" "STARTS" v
  | Error _ -> ()

let test_conditional_endswith () =
  let input = "hello world" in
  let cond = Conditional {
    condition = "endswith:world";
    on_true = Template "ENDS";
    on_false = Template "NOT ENDS";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional endswith" result;
  match result with
  | Ok v -> Alcotest.(check string) "endswith result" "ENDS" v
  | Error _ -> ()

let test_conditional_matches () =
  let input = "test123abc" in
  let cond = Conditional {
    condition = "matches:[0-9]+";
    on_true = Template "HAS NUMBERS";
    on_false = Template "NO NUMBERS";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "conditional matches" result;
  match result with
  | Ok v -> Alcotest.(check string) "matches result" "HAS NUMBERS" v
  | Error _ -> ()

let test_conditional_legacy_contains () =
  let input = "some text here" in
  let cond = Conditional {
    condition = "text";  (* legacy plain text = contains *)
    on_true = Template "FOUND";
    on_false = Template "NOT FOUND";
  } in
  let result = Chain_adapter_eio.apply_adapter_transform cond input in
  check_ok "legacy contains" result;
  match result with
  | Ok v -> Alcotest.(check string) "legacy result" "FOUND" v
  | Error _ -> ()

let test_split_by_line () =
  (* Use long lines to force multiple chunks *)
  let long_line = String.make 50 'x' in
  let input = Printf.sprintf "%s\n%s\n%s" long_line long_line long_line in
  let split = Split { delimiter = "line"; chunk_size = 20; overlap = 0 } in
  let result = Chain_adapter_eio.apply_adapter_transform split input in
  check_ok "split by line" result;
  match result with
  | Ok v ->
    let json = Yojson.Safe.from_string v in
    (match json with
     | `List chunks -> Alcotest.(check bool) "has chunks" true (List.length chunks >= 1)
     | _ -> Alcotest.fail "Expected JSON array")
  | Error _ -> ()

let test_split_by_paragraph () =
  (* Use long paragraphs to force multiple chunks *)
  let long_para = String.make 100 'y' in
  let input = Printf.sprintf "%s\n\n%s\n\n%s" long_para long_para long_para in
  let split = Split { delimiter = "paragraph"; chunk_size = 50; overlap = 0 } in
  let result = Chain_adapter_eio.apply_adapter_transform split input in
  check_ok "split by paragraph" result;
  match result with
  | Ok v ->
    let json = Yojson.Safe.from_string v in
    (match json with
     | `List chunks -> Alcotest.(check bool) "has chunks" true (List.length chunks >= 1)
     | _ -> Alcotest.fail "Expected JSON array")
  | Error _ -> ()

let test_split_by_sentence () =
  let input = "First sentence. Second sentence! Third sentence?" in
  let split = Split { delimiter = "sentence"; chunk_size = 100; overlap = 0 } in
  let result = Chain_adapter_eio.apply_adapter_transform split input in
  check_ok "split by sentence" result

let test_split_with_overlap () =
  let input = "word1 word2 word3 word4 word5 word6 word7 word8" in
  let split = Split { delimiter = " "; chunk_size = 10; overlap = 2 } in
  let result = Chain_adapter_eio.apply_adapter_transform split input in
  check_ok "split with overlap" result

let test_custom_identity () =
  let input = "test value" in
  let result = Chain_adapter_eio.apply_adapter_transform (Custom "identity") input in
  check_ok "custom identity" result;
  match result with
  | Ok v -> Alcotest.(check string) "identity" input v
  | Error _ -> ()

let test_custom_uppercase () =
  let input = "hello" in
  let result = Chain_adapter_eio.apply_adapter_transform (Custom "uppercase") input in
  check_ok "custom uppercase" result;
  match result with
  | Ok v -> Alcotest.(check string) "uppercase" "HELLO" v
  | Error _ -> ()

let test_custom_lowercase () =
  let input = "HELLO" in
  let result = Chain_adapter_eio.apply_adapter_transform (Custom "lowercase") input in
  check_ok "custom lowercase" result;
  match result with
  | Ok v -> Alcotest.(check string) "lowercase" "hello" v
  | Error _ -> ()

let test_custom_trim () =
  let input = "  spaced  " in
  let result = Chain_adapter_eio.apply_adapter_transform (Custom "trim") input in
  check_ok "custom trim" result;
  match result with
  | Ok v -> Alcotest.(check string) "trim" "spaced" v
  | Error _ -> ()

(* ============================================================
   Chain Category Tests
   ============================================================ *)

let test_result_monad_pure () =
  let open Result_monad in
  let x = pure 42 in
  match x with
  | Ok v -> Alcotest.(check int) "pure" 42 v
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_monad_map () =
  let open Result_monad in
  let x = pure 10 in
  let y = map (fun n -> n * 2) x in
  match y with
  | Ok v -> Alcotest.(check int) "map" 20 v
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_monad_bind () =
  let open Result_monad in
  let x = pure 5 in
  let y = x >>= fun n -> pure (n + 1) in
  match y with
  | Ok v -> Alcotest.(check int) "bind" 6 v
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_monad_error_propagation () =
  let open Result_monad in
  let x : int t = Error "failed" in
  let y = map (fun n -> n * 2) x in
  match y with
  | Error e -> Alcotest.(check string) "error prop" "failed" e
  | Ok _ -> Alcotest.fail "Expected Error"

let test_result_monad_sequence () =
  let open Result_monad in
  let xs = [pure 1; pure 2; pure 3] in
  match sequence xs with
  | Ok vs -> Alcotest.(check (list int)) "sequence" [1;2;3] vs
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_monad_sequence_error () =
  let open Result_monad in
  let xs = [pure 1; Error "mid error"; pure 3] in
  match sequence xs with
  | Error e -> Alcotest.(check string) "sequence error" "mid error" e
  | Ok _ -> Alcotest.fail "Expected Error"

let test_result_monad_catch () =
  let open Result_monad in
  let x = catch (fun () -> 42) in
  match x with
  | Ok v -> Alcotest.(check int) "catch success" 42 v
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_monad_catch_exception () =
  let open Result_monad in
  let x = catch (fun () -> failwith "boom") in
  match x with
  | Error e -> Alcotest.(check bool) "catch exception" true (String.length e > 0)
  | Ok _ -> Alcotest.fail "Expected Error"

let test_result_monad_map_error () =
  let open Result_monad in
  let x : int t = Error "original" in
  let y = map_error (fun e -> "wrapped: " ^ e) x in
  match y with
  | Error e -> Alcotest.(check string) "map_error" "wrapped: original" e
  | Ok _ -> Alcotest.fail "Expected Error"

let test_result_monad_kleisli () =
  let open Result_monad in
  let f x = pure (x + 1) in
  let g x = pure (x * 2) in
  let h = f >=> g in
  match h 5 with
  | Ok v -> Alcotest.(check int) "kleisli" 12 v  (* (5+1)*2 = 12 *)
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_kleisli_arr () =
  let open Result_kleisli in
  let f = arr (fun x -> x * 2) in
  match run f 5 with
  | Ok v -> Alcotest.(check int) "arr" 10 v
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_kleisli_compose () =
  let open Result_kleisli in
  let f = arr (fun x -> x + 1) in
  let g = arr (fun x -> x * 2) in
  let h = f >>> g in
  match run h 5 with
  | Ok v -> Alcotest.(check int) "compose" 12 v  (* (5+1)*2 = 12 *)
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_kleisli_fanout () =
  let open Result_kleisli in
  let f = arr (fun x -> x + 1) in
  let g = arr (fun x -> x * 2) in
  let h = f &&& g in
  match run h 5 with
  | Ok (a, b) ->
    Alcotest.(check int) "fanout a" 6 a;
    Alcotest.(check int) "fanout b" 10 b
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_kleisli_parallel () =
  let open Result_kleisli in
  let f = arr (fun x -> x + 1) in
  let g = arr (fun x -> x * 2) in
  let h = f *** g in
  match run h (5, 3) with
  | Ok (a, b) ->
    Alcotest.(check int) "parallel a" 6 a;
    Alcotest.(check int) "parallel b" 6 b
  | Error _ -> Alcotest.fail "Expected Ok"

let test_result_kleisli_guard () =
  let open Result_kleisli in
  let f = guard ~error:"must be positive" (fun x -> x > 0) in
  Alcotest.(check bool) "guard pass" true (Result.is_ok (run f 5));
  Alcotest.(check bool) "guard fail" true (Result.is_error (run f (-1)))

let test_result_kleisli_from_option () =
  let open Result_kleisli in
  let f = from_option ~error:"not found" (fun x -> if x > 0 then Some x else None) in
  Alcotest.(check bool) "from_option some" true (Result.is_ok (run f 5));
  Alcotest.(check bool) "from_option none" true (Result.is_error (run f (-1)))

let test_verdict_monoid () =
  let open Verdict_monoid in
  (* identity *)
  Alcotest.(check bool) "empty is Pass" true (match empty with Pass _ -> true | _ -> false);

  (* Fail takes precedence *)
  let v1 = concat (Pass "ok") (Fail "error") in
  Alcotest.(check bool) "fail precedence" true (match v1 with Fail _ -> true | _ -> false);

  (* Warn combines *)
  let v2 = concat (Warn "w1") (Warn "w2") in
  (match v2 with
   | Warn s -> Alcotest.(check bool) "warn combine" true (String.length s > 5)
   | _ -> Alcotest.fail "Expected Warn");

  (* concat_all *)
  let v3 = concat_all [Pass "a"; Pass "b"; Pass "c"] in
  Alcotest.(check bool) "concat_all pass" true (match v3 with Pass _ -> true | _ -> false)

let test_confidence_monoid () =
  let open Confidence_monoid in
  Alcotest.(check (float 0.001)) "empty" 1.0 empty;
  Alcotest.(check (float 0.001)) "concat" 0.8 (concat 0.8 0.8);  (* (0.8 + 0.8) / 2 = 0.8 *)

  let scores = [0.9; 0.8; 0.7] in
  let geo = geometric scores in
  Alcotest.(check bool) "geometric < max" true (geo < 0.9);
  Alcotest.(check bool) "geometric > min" true (geo > 0.7);

  let harm = harmonic scores in
  Alcotest.(check bool) "harmonic < max" true (harm < 0.9);

  let wt = weighted [1.0; 2.0; 1.0] scores in
  Alcotest.(check bool) "weighted reasonable" true (wt > 0.7 && wt < 0.9)

let test_token_monoid () =
  let open Token_monoid in
  let t1 = { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 0.01 } in
  let t2 = { prompt_tokens = 200; completion_tokens = 100; total_tokens = 300; estimated_cost_usd = 0.02 } in
  let combined = concat t1 t2 in
  Alcotest.(check int) "prompt sum" 300 combined.prompt_tokens;
  Alcotest.(check int) "completion sum" 150 combined.completion_tokens;
  Alcotest.(check int) "total sum" 450 combined.total_tokens;
  Alcotest.(check (float 0.001)) "cost sum" 0.03 combined.estimated_cost_usd

(* ============================================================
   Chain Composer Tests
   ============================================================ *)

let test_masc_task_serialization () =
  let open Chain_composer in
  let task = {
    task_id = "t1";
    title = "Test Task";
    description = Some "A description";
    priority = 2;
    status = "todo";
    assignee = Some "user1";
    metadata = [("key", "value")];
  } in
  let task' = json_roundtrip masc_task_to_yojson masc_task_of_yojson task in
  Alcotest.(check string) "task id" "t1" task'.task_id;
  Alcotest.(check int) "priority" 2 task'.priority

let test_task_relation_serialization () =
  let open Chain_composer in
  let relations = [
    Sequential ("a", "b");
    Parallel ["a"; "b"; "c"];
    Conditional { condition_task = "check"; then_tasks = ["a"]; else_tasks = ["b"] };
    Quorum { tasks = ["a"; "b"; "c"]; min_success = 2 };
  ] in
  List.iter (fun r ->
    let _ = json_roundtrip task_relation_to_yojson task_relation_of_yojson r in ()
  ) relations

let test_replan_reason_serialization () =
  let open Chain_composer in
  let reasons = [
    TaskFailed "task1";
    GoalNotAchieved;
    NewTaskAdded "task2";
    ContextChanged;
    TimeoutApproaching;
  ] in
  List.iter (fun r ->
    let _ = json_roundtrip replan_reason_to_yojson replan_reason_of_yojson r in ()
  ) reasons

let test_composition_analysis_serialization () =
  let open Chain_composer in
  let analysis = {
    goal = "Complete task";
    tasks = [];
    relations = [];
    estimated_duration_ms = 5000;
    critical_path = ["task1"; "task2"];
    parallelizable_groups = [["a"; "b"]; ["c"]];
  } in
  let analysis' = json_roundtrip composition_analysis_to_yojson composition_analysis_of_yojson analysis in
  Alcotest.(check int) "estimated duration" 5000 analysis'.estimated_duration_ms

let test_build_design_context () =
  let open Chain_composer in
  let tasks = [
    { task_id = "t1"; title = "Task One"; description = Some "First task"; priority = 1; status = "todo"; assignee = None; metadata = [] };
    { task_id = "t2"; title = "Task Two"; description = None; priority = 2; status = "todo"; assignee = None; metadata = [] };
  ] in
  let context = build_design_context ~goal:"Complete project" ~tasks in
  Alcotest.(check bool) "contains goal" true (String.length context > 0);
  Alcotest.(check bool) "contains task1" true (try Str.search_forward (Str.regexp_string "Task One") context 0 >= 0 with Not_found -> false);
  Alcotest.(check bool) "contains task2" true (try Str.search_forward (Str.regexp_string "Task Two") context 0 >= 0 with Not_found -> false)

let test_init_state () =
  let open Chain_composer in
  let state = init_state ~session_id:"test-123" ~goal:"Test goal" ~tasks:[] ~max_replans:5 in
  Alcotest.(check string) "session_id" "test-123" state.session_id;
  Alcotest.(check string) "goal" "Test goal" state.goal;
  Alcotest.(check int) "max_replans" 5 state.max_replans;
  Alcotest.(check int) "replan_count" 0 state.replan_count

(* ============================================================
   Chain Orchestrator Tests
   ============================================================ *)

let test_parse_chain_design_mermaid () =
  let response = {|Here's the chain:
```mermaid
graph LR
  a["LLM:gemini 'Step 1'"]
  b["LLM:claude 'Step 2'"]
  a --> b
```
That should work!|} in
  let result = Chain_orchestrator_eio.parse_chain_design response in
  check_ok "parse mermaid" result

let test_parse_chain_design_json () =
  let response = {|Here's the chain:
```json
{
  "id": "test",
  "nodes": [
    {"id": "a", "type": "llm", "model": "gemini", "prompt": "test"}
  ],
  "output": "a"
}
```
|} in
  let result = Chain_orchestrator_eio.parse_chain_design response in
  check_ok "parse json" result

let test_parse_chain_design_invalid () =
  let response = "Just some random text without any chain format" in
  let result = Chain_orchestrator_eio.parse_chain_design response in
  check_error "parse invalid" result

let test_calc_parallelization_efficiency () =
  let eff1 = Chain_orchestrator_eio.calc_parallelization_efficiency ~parallel_groups:1 ~total_nodes:1 in
  Alcotest.(check (float 0.01)) "single node" 1.0 eff1;

  let eff2 = Chain_orchestrator_eio.calc_parallelization_efficiency ~parallel_groups:3 ~total_nodes:4 in
  Alcotest.(check (float 0.01)) "parallel groups" 1.0 eff2;

  let eff3 = Chain_orchestrator_eio.calc_parallelization_efficiency ~parallel_groups:1 ~total_nodes:4 in
  Alcotest.(check bool) "sequential lower" true (eff3 < 1.0)

let test_orchestration_types () =
  let open Chain_orchestrator_eio in
  (* orchestration_result *)
  let result = {
    success = true;
    final_metrics = None;
    verification = None;
    total_replans = 0;
    summary = "Completed successfully";
  } in
  let result' = json_roundtrip orchestration_result_to_yojson orchestration_result_of_yojson result in
  Alcotest.(check bool) "success" true result'.success;

  (* orchestration_error *)
  let errors = [
    DesignFailed "design error";
    CompileFailed "compile error";
    ExecutionFailed "exec error";
    VerificationFailed "verify error";
    MaxReplansExceeded;
    Timeout;
  ] in
  List.iter (fun e ->
    let _ = json_roundtrip orchestration_error_to_yojson orchestration_error_of_yojson e in ()
  ) errors

let test_default_orchestration_config () =
  let open Chain_orchestrator_eio in
  Alcotest.(check int) "max_replans" 3 default_config.max_replans;
  Alcotest.(check int) "timeout_ms" 300000 default_config.timeout_ms;
  Alcotest.(check bool) "trace_enabled" true default_config.trace_enabled;
  Alcotest.(check bool) "verify_on_complete" true default_config.verify_on_complete

(* ============================================================
   Chain Batch Tests
   ============================================================ *)

let test_priority_queue () =
  let open Chain_batch in
  let q = PriorityQueue.create () in
  Alcotest.(check bool) "initially empty" true (PriorityQueue.is_empty q);

  PriorityQueue.push q ~priority:2 ~value:"low";
  PriorityQueue.push q ~priority:1 ~value:"high";
  PriorityQueue.push q ~priority:3 ~value:"lowest";

  Alcotest.(check int) "length after push" 3 (PriorityQueue.length q);

  (* Pop should return in priority order *)
  let v1 = PriorityQueue.pop q in
  Alcotest.(check (option string)) "first pop" (Some "high") v1;

  let v2 = PriorityQueue.pop q in
  Alcotest.(check (option string)) "second pop" (Some "low") v2;

  let v3 = PriorityQueue.pop q in
  Alcotest.(check (option string)) "third pop" (Some "lowest") v3;

  let v4 = PriorityQueue.pop q in
  Alcotest.(check (option string)) "empty pop" None v4

let test_calculate_delay () =
  let open Chain_batch in
  let config = default_retry_config in

  let delay0 = calculate_delay config 0 in
  Alcotest.(check (float 0.01)) "delay 0" 1.0 delay0;  (* 1000ms / 1000 *)

  let delay1 = calculate_delay config 1 in
  Alcotest.(check (float 0.01)) "delay 1" 2.0 delay1;  (* 1000 * 2^1 / 1000 *)

  let delay2 = calculate_delay config 2 in
  Alcotest.(check (float 0.01)) "delay 2" 4.0 delay2  (* 1000 * 2^2 / 1000 *)

let test_estimate_tokens () =
  let open Chain_batch in
  let node = make_llm_node ~id:"n1" ~model:"gemini" ~prompt:(String.make 100 'x') () in
  let chain = make_chain ~id:"c1" ~nodes:[node] ~output:"n1" () in
  let tokens = estimate_tokens chain in
  Alcotest.(check int) "estimated tokens" 25 tokens  (* 100 / 4 *)

let test_chunk_by_tokens () =
  let open Chain_batch in
  let make_chain_with_prompt size =
    let node = make_llm_node ~id:"n" ~model:"g" ~prompt:(String.make size 'x') () in
    make_chain ~id:(Printf.sprintf "c%d" size) ~nodes:[node] ~output:"n" ()
  in
  let chains = [
    make_chain_with_prompt 100;
    make_chain_with_prompt 200;
    make_chain_with_prompt 150;
    make_chain_with_prompt 50;
  ] in
  let chunks = chunk_chains (TokenBased 100) chains in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks > 1)

let test_chunk_by_items () =
  let open Chain_batch in
  let chains = List.init 10 (fun i ->
    let node = make_llm_node ~id:"n" ~model:"g" ~prompt:"test" () in
    make_chain ~id:(Printf.sprintf "c%d" i) ~nodes:[node] ~output:"n" ()
  ) in
  let chunks = chunk_chains (ItemBased 3) chains in
  Alcotest.(check int) "chunk count" 4 (List.length chunks);  (* 10/3 = 4 chunks *)

  (* First 3 chunks should have 3 items, last should have 1 *)
  let lengths = List.map List.length chunks in
  Alcotest.(check (list int)) "chunk sizes" [3; 3; 3; 1] lengths

(* ============================================================
   Test Suite
   ============================================================ *)

let types_tests = [
  "direction serialization", `Quick, test_direction_serialization;
  "direction yojson", `Quick, test_direction_yojson;
  "merge_strategy serialization", `Quick, test_merge_strategy_serialization;
  "threshold_op serialization", `Quick, test_threshold_op_serialization;
  "select_strategy serialization", `Quick, test_select_strategy_serialization;
  "backoff_strategy serialization", `Quick, test_backoff_strategy_serialization;
  "adapter_transform serialization", `Quick, test_adapter_transform_serialization;
  "mcts_policy serialization", `Quick, test_mcts_policy_serialization;
  "evaluator_config serialization", `Quick, test_evaluator_config_serialization;
  "chain_config serialization", `Quick, test_chain_config_serialization;
  "make_chain", `Quick, test_make_chain;
  "make_chain with metadata", `Quick, test_make_chain_with_metadata;
  "node_type_name", `Quick, test_node_type_name;
  "token_usage", `Quick, test_token_usage;
  "empty_token_usage", `Quick, test_empty_token_usage;
  "trace_entry serialization", `Quick, test_trace_entry_serialization;
  "chain_result serialization", `Quick, test_chain_result_serialization;
  "execution_plan serialization", `Quick, test_execution_plan_serialization;
  "batch types", `Quick, test_batch_types;
  "helper functions", `Quick, test_helper_functions;
]

let adapter_tests = [
  "extract transform", `Quick, test_extract_transform;
  "extract array index", `Quick, test_extract_array_index;
  "extract not found", `Quick, test_extract_not_found;
  "template transform", `Quick, test_template_transform;
  "summarize transform", `Quick, test_summarize_transform;
  "summarize short text", `Quick, test_summarize_short_text;
  "truncate transform", `Quick, test_truncate_transform;
  "jsonpath transform", `Quick, test_jsonpath_transform;
  "jsonpath root", `Quick, test_jsonpath_root;
  "regex transform", `Quick, test_regex_transform;
  "validate schema object", `Quick, test_validate_schema_object;
  "validate schema type mismatch", `Quick, test_validate_schema_type_mismatch;
  "validate schema missing required", `Quick, test_validate_schema_missing_required;
  "validate schema simple type", `Quick, test_validate_schema_simple_type;
  "validate schema enum", `Quick, test_validate_schema_enum;
  "validate schema string length", `Quick, test_validate_schema_string_length;
  "parse json valid", `Quick, test_parse_json_valid;
  "parse json invalid", `Quick, test_parse_json_invalid;
  "stringify non-json", `Quick, test_stringify_non_json;
  "stringify already json", `Quick, test_stringify_already_json;
  "chain transforms", `Quick, test_chain_transforms;
  "chain transforms error propagation", `Quick, test_chain_transforms_error_propagation;
  "conditional contains", `Quick, test_conditional_contains;
  "conditional eq", `Quick, test_conditional_eq;
  "conditional neq", `Quick, test_conditional_neq;
  "conditional gt", `Quick, test_conditional_gt;
  "conditional gte", `Quick, test_conditional_gte;
  "conditional lt", `Quick, test_conditional_lt;
  "conditional empty", `Quick, test_conditional_empty;
  "conditional nonempty", `Quick, test_conditional_nonempty;
  "conditional startswith", `Quick, test_conditional_startswith;
  "conditional endswith", `Quick, test_conditional_endswith;
  "conditional matches", `Quick, test_conditional_matches;
  "conditional legacy contains", `Quick, test_conditional_legacy_contains;
  "split by line", `Quick, test_split_by_line;
  "split by paragraph", `Quick, test_split_by_paragraph;
  "split by sentence", `Quick, test_split_by_sentence;
  "split with overlap", `Quick, test_split_with_overlap;
  "custom identity", `Quick, test_custom_identity;
  "custom uppercase", `Quick, test_custom_uppercase;
  "custom lowercase", `Quick, test_custom_lowercase;
  "custom trim", `Quick, test_custom_trim;
]

let category_tests = [
  "result monad pure", `Quick, test_result_monad_pure;
  "result monad map", `Quick, test_result_monad_map;
  "result monad bind", `Quick, test_result_monad_bind;
  "result monad error propagation", `Quick, test_result_monad_error_propagation;
  "result monad sequence", `Quick, test_result_monad_sequence;
  "result monad sequence error", `Quick, test_result_monad_sequence_error;
  "result monad catch", `Quick, test_result_monad_catch;
  "result monad catch exception", `Quick, test_result_monad_catch_exception;
  "result monad map_error", `Quick, test_result_monad_map_error;
  "result monad kleisli", `Quick, test_result_monad_kleisli;
  "result kleisli arr", `Quick, test_result_kleisli_arr;
  "result kleisli compose", `Quick, test_result_kleisli_compose;
  "result kleisli fanout", `Quick, test_result_kleisli_fanout;
  "result kleisli parallel", `Quick, test_result_kleisli_parallel;
  "result kleisli guard", `Quick, test_result_kleisli_guard;
  "result kleisli from_option", `Quick, test_result_kleisli_from_option;
  "verdict monoid", `Quick, test_verdict_monoid;
  "confidence monoid", `Quick, test_confidence_monoid;
  "token monoid", `Quick, test_token_monoid;
]

let composer_tests = [
  "masc_task serialization", `Quick, test_masc_task_serialization;
  "task_relation serialization", `Quick, test_task_relation_serialization;
  "replan_reason serialization", `Quick, test_replan_reason_serialization;
  "composition_analysis serialization", `Quick, test_composition_analysis_serialization;
  "build_design_context", `Quick, test_build_design_context;
  "init_state", `Quick, test_init_state;
]

let orchestrator_tests = [
  "parse chain design mermaid", `Quick, test_parse_chain_design_mermaid;
  "parse chain design json", `Quick, test_parse_chain_design_json;
  "parse chain design invalid", `Quick, test_parse_chain_design_invalid;
  "calc parallelization efficiency", `Quick, test_calc_parallelization_efficiency;
  "orchestration types", `Quick, test_orchestration_types;
  "default orchestration config", `Quick, test_default_orchestration_config;
]

let batch_tests = [
  "priority queue", `Quick, test_priority_queue;
  "calculate delay", `Quick, test_calculate_delay;
  "estimate tokens", `Quick, test_estimate_tokens;
  "chunk by tokens", `Quick, test_chunk_by_tokens;
  "chunk by items", `Quick, test_chunk_by_items;
]

let () =
  Alcotest.run "Chain Modules Coverage" [
    "Chain Types", types_tests;
    "Chain Adapter", adapter_tests;
    "Chain Category", category_tests;
    "Chain Composer", composer_tests;
    "Chain Orchestrator", orchestrator_tests;
    "Chain Batch", batch_tests;
  ]

(** Tests for pure helper functions in Chain_executor_eio.

    Targets: parse_confidence_from_output, build_confidence_system_prompt,
    summarize_for_context, substitute_prompt (additional), resolve_single_input,
    set_node_status, next_attempt, plan_to_steps, make_context. *)

open Alcotest

let confidence_str (level : Chain_types.confidence_level) =
  match level with
  | High -> "High"
  | Medium -> "Medium"
  | Low -> "Low"

(* --- parse_confidence_from_output --- *)

let test_parse_confidence_high () =
  let (level, cleaned) =
    Chain_executor_eio.parse_confidence_from_output
      "Here is my answer.\nConfidence:High"
  in
  check string "level" "High" (confidence_str level);
  check string "cleaned" "Here is my answer." cleaned

let test_parse_confidence_medium () =
  let (level, cleaned) =
    Chain_executor_eio.parse_confidence_from_output
      "Some output\nConfidence:Medium\nExtra line"
  in
  check string "level" "Medium" (confidence_str level);
  check bool "extra line preserved" true (String.length cleaned > 0)

let test_parse_confidence_low () =
  let (level, cleaned) =
    Chain_executor_eio.parse_confidence_from_output
      "Answer text\nconfidence:low"
  in
  check string "level" "Low" (confidence_str level);
  check string "cleaned" "Answer text" cleaned

let test_parse_confidence_not_found () =
  let (level, cleaned) =
    Chain_executor_eio.parse_confidence_from_output "No confidence here"
  in
  check string "defaults to Low" "Low" (confidence_str level);
  check string "output unchanged" "No confidence here" cleaned

let test_parse_confidence_empty () =
  let (level, cleaned) =
    Chain_executor_eio.parse_confidence_from_output ""
  in
  check string "empty defaults to Low" "Low" (confidence_str level);
  check string "cleaned is empty" "" cleaned

let test_parse_confidence_case_insensitive () =
  let (level, _) =
    Chain_executor_eio.parse_confidence_from_output
      "result\nCONFIDENCE:High"
  in
  check string "case insensitive" "High" (confidence_str level)

(* --- build_confidence_system_prompt --- *)

let test_build_confidence_prompt_custom () =
  let result =
    Chain_executor_eio.build_confidence_system_prompt
      ~confidence_prompt:(Some "Rate yourself 1-10") None
  in
  check string "custom prompt" "Rate yourself 1-10" result

let test_build_confidence_prompt_default_no_hint () =
  let result =
    Chain_executor_eio.build_confidence_system_prompt
      ~confidence_prompt:None None
  in
  check bool "contains Confidence:" true
    (try ignore (Str.search_forward (Str.regexp "Confidence:") result 0); true
     with Not_found -> false)

let test_build_confidence_prompt_with_hint () =
  let result =
    Chain_executor_eio.build_confidence_system_prompt
      ~confidence_prompt:None (Some "Summarize the article")
  in
  check bool "contains hint" true
    (try ignore (Str.search_forward (Str.regexp "Summarize") result 0); true
     with Not_found -> false)

(* --- summarize_for_context --- *)

let test_summarize_short () =
  let result = Chain_executor_eio.summarize_for_context "Hello world" in
  check string "short unchanged" "Hello world" result

let test_summarize_long () =
  let long = String.make 600 'x' in
  let result = Chain_executor_eio.summarize_for_context long in
  check int "truncated to 503" 503 (String.length result);
  check bool "ends with ..." true (String.sub result 500 3 = "...")

let test_summarize_exactly_500 () =
  let text = String.make 500 'a' in
  let result = Chain_executor_eio.summarize_for_context text in
  check int "exactly 500 unchanged" 500 (String.length result)

(* --- make_context / set_node_status / next_attempt --- *)

let test_make_context_defaults () =
  let ctx =
    Chain_executor_eio.make_context
      ~start_time:1000.0 ~trace_enabled:false ~timeout:30 ~chain_id:"test" ()
  in
  check bool "outputs empty" true (Hashtbl.length ctx.outputs = 0);
  check bool "trace_enabled" false ctx.trace_enabled;
  check int "timeout" 30 ctx.timeout;
  check string "chain_id" "test" ctx.chain_id

let test_set_node_status () =
  let ctx =
    Chain_executor_eio.make_context
      ~start_time:0.0 ~trace_enabled:false ~timeout:10 ~chain_id:"t" ()
  in
  Chain_executor_eio.set_node_status ctx "n1" Chain_executor_eio.Running;
  check bool "Running" true
    (Hashtbl.find_opt ctx.node_status "n1" = Some Chain_executor_eio.Running)

let test_set_node_status_overwrite () =
  let ctx =
    Chain_executor_eio.make_context
      ~start_time:0.0 ~trace_enabled:false ~timeout:10 ~chain_id:"t" ()
  in
  Chain_executor_eio.set_node_status ctx "n" Chain_executor_eio.Planned;
  Chain_executor_eio.set_node_status ctx "n" Chain_executor_eio.Completed;
  check bool "overwritten" true
    (Hashtbl.find_opt ctx.node_status "n" = Some Chain_executor_eio.Completed)

let test_next_attempt_first () =
  let ctx =
    Chain_executor_eio.make_context
      ~start_time:0.0 ~trace_enabled:false ~timeout:10 ~chain_id:"t" ()
  in
  check int "first is 1" 1 (Chain_executor_eio.next_attempt ctx "n1")

let test_next_attempt_increments () =
  let ctx =
    Chain_executor_eio.make_context
      ~start_time:0.0 ~trace_enabled:false ~timeout:10 ~chain_id:"t" ()
  in
  ignore (Chain_executor_eio.next_attempt ctx "n1");
  ignore (Chain_executor_eio.next_attempt ctx "n1");
  check int "third is 3" 3 (Chain_executor_eio.next_attempt ctx "n1")

let test_next_attempt_independent () =
  let ctx =
    Chain_executor_eio.make_context
      ~start_time:0.0 ~trace_enabled:false ~timeout:10 ~chain_id:"t" ()
  in
  ignore (Chain_executor_eio.next_attempt ctx "a");
  ignore (Chain_executor_eio.next_attempt ctx "a");
  let na = Chain_executor_eio.next_attempt ctx "a" in
  let nb = Chain_executor_eio.next_attempt ctx "b" in
  check int "a=3" 3 na;
  check int "b=1" 1 nb

(* --- resolve_single_input --- *)

let make_test_ctx () =
  let ctx =
    Chain_executor_eio.make_context
      ~start_time:0.0 ~trace_enabled:false ~timeout:10 ~chain_id:"test" ()
  in
  Hashtbl.replace ctx.outputs "node1" "hello world";
  Hashtbl.replace ctx.outputs "json_node" {|{"name":"Alice","age":30}|};
  Hashtbl.replace ctx.outputs "bullet_node" "- file_key: abc123\n- node_id: 42";
  ctx

let test_resolve_direct_ref () =
  let ctx = make_test_ctx () in
  check string "direct" "hello world"
    (Chain_executor_eio.resolve_single_input ctx "node1")

let test_resolve_placeholder () =
  let ctx = make_test_ctx () in
  check string "placeholder" "hello world"
    (Chain_executor_eio.resolve_single_input ctx "{{node1}}")

let test_resolve_json_path () =
  let ctx = make_test_ctx () in
  check string "json path" "Alice"
    (Chain_executor_eio.resolve_single_input ctx "{{json_node.name}}")

let test_resolve_json_path_int () =
  let ctx = make_test_ctx () in
  check string "json path int" "30"
    (Chain_executor_eio.resolve_single_input ctx "{{json_node.age}}")

let test_resolve_bullet () =
  let ctx = make_test_ctx () in
  check string "bullet" "abc123"
    (Chain_executor_eio.resolve_single_input ctx "{{bullet_node.file_key}}")

let test_resolve_missing () =
  let ctx = make_test_ctx () in
  check string "missing returns original" "{{nonexistent}}"
    (Chain_executor_eio.resolve_single_input ctx "{{nonexistent}}")

let test_resolve_dot_path_direct () =
  let ctx = make_test_ctx () in
  check string "dot path" "Alice"
    (Chain_executor_eio.resolve_single_input ctx "json_node.name")

(* --- substitute_prompt extra cases --- *)

let test_substitute_prompt_empty_inputs () =
  check string "no substitutions" "Hello {{name}}"
    (Chain_executor_eio.substitute_prompt "Hello {{name}}" [])

let test_substitute_prompt_empty_template () =
  check string "empty template" ""
    (Chain_executor_eio.substitute_prompt "" [("name", "world")])

let test_substitute_prompt_double_var () =
  check string "both replaced" "hello and hello"
    (Chain_executor_eio.substitute_prompt "{{x}} and {{x}}" [("x", "hello")])

(* --- plan_to_steps --- *)

let make_node id =
  Chain_types.({
    id;
    node_type = Llm {
      model = "stub"; system = None; prompt = "test";
      timeout = None; tools = None; prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  })

let test_plan_to_steps_empty () =
  let plan = Chain_types.({
    chain = make_chain ~id:"c" ~nodes:[] ~output:"" ();
    execution_order = [];
    parallel_groups = [];
    depth = 0;
  }) in
  check int "no steps" 0 (List.length (Chain_executor_eio.plan_to_steps plan))

let test_plan_to_steps_sequential () =
  let nodes = [make_node "a"; make_node "b"] in
  let plan = Chain_types.({
    chain = make_chain ~id:"c" ~nodes ~output:"b" ();
    execution_order = ["a"; "b"];
    parallel_groups = [["a"]; ["b"]];
    depth = 1;
  }) in
  let steps = Chain_executor_eio.plan_to_steps plan in
  check int "two steps" 2 (List.length steps);
  (match steps with
   | [Chain_executor_eio.Sequential n1; Chain_executor_eio.Sequential n2] ->
       check string "first" "a" n1.id;
       check string "second" "b" n2.id
   | _ -> fail "expected two Sequential")

let test_plan_to_steps_parallel () =
  let nodes = [make_node "a"; make_node "b"; make_node "c"] in
  let plan = Chain_types.({
    chain = make_chain ~id:"c" ~nodes ~output:"c" ();
    execution_order = ["a"; "b"; "c"];
    parallel_groups = [["a"; "b"]; ["c"]];
    depth = 1;
  }) in
  let steps = Chain_executor_eio.plan_to_steps plan in
  check int "two steps" 2 (List.length steps);
  (match steps with
   | [Chain_executor_eio.Parallel ns; Chain_executor_eio.Sequential n] ->
       check int "parallel size" 2 (List.length ns);
       check string "final" "c" n.id
   | _ -> fail "expected Parallel then Sequential")

let test_plan_to_steps_missing_node () =
  let nodes = [make_node "a"] in
  let plan = Chain_types.({
    chain = make_chain ~id:"c" ~nodes ~output:"a" ();
    execution_order = ["a"; "missing"];
    parallel_groups = [["a"]; ["missing"]];
    depth = 1;
  }) in
  check int "only valid node" 1
    (List.length (Chain_executor_eio.plan_to_steps plan))

(* --- Test Suite --- *)

let () =
  run "chain_executor_helpers" [
    ("parse_confidence", [
      test_case "high" `Quick test_parse_confidence_high;
      test_case "medium" `Quick test_parse_confidence_medium;
      test_case "low" `Quick test_parse_confidence_low;
      test_case "not found" `Quick test_parse_confidence_not_found;
      test_case "empty" `Quick test_parse_confidence_empty;
      test_case "case insensitive" `Quick test_parse_confidence_case_insensitive;
    ]);
    ("build_confidence_prompt", [
      test_case "custom" `Quick test_build_confidence_prompt_custom;
      test_case "default no hint" `Quick test_build_confidence_prompt_default_no_hint;
      test_case "with hint" `Quick test_build_confidence_prompt_with_hint;
    ]);
    ("summarize_for_context", [
      test_case "short" `Quick test_summarize_short;
      test_case "long" `Quick test_summarize_long;
      test_case "exactly 500" `Quick test_summarize_exactly_500;
    ]);
    ("make_context", [
      test_case "defaults" `Quick test_make_context_defaults;
    ]);
    ("set_node_status", [
      test_case "set" `Quick test_set_node_status;
      test_case "overwrite" `Quick test_set_node_status_overwrite;
    ]);
    ("next_attempt", [
      test_case "first" `Quick test_next_attempt_first;
      test_case "increments" `Quick test_next_attempt_increments;
      test_case "independent" `Quick test_next_attempt_independent;
    ]);
    ("resolve_single_input", [
      test_case "direct ref" `Quick test_resolve_direct_ref;
      test_case "placeholder" `Quick test_resolve_placeholder;
      test_case "json path" `Quick test_resolve_json_path;
      test_case "json path int" `Quick test_resolve_json_path_int;
      test_case "bullet" `Quick test_resolve_bullet;
      test_case "missing" `Quick test_resolve_missing;
      test_case "dot path" `Quick test_resolve_dot_path_direct;
    ]);
    ("substitute_prompt", [
      test_case "empty inputs" `Quick test_substitute_prompt_empty_inputs;
      test_case "empty template" `Quick test_substitute_prompt_empty_template;
      test_case "double var" `Quick test_substitute_prompt_double_var;
    ]);
    ("plan_to_steps", [
      test_case "empty" `Quick test_plan_to_steps_empty;
      test_case "sequential" `Quick test_plan_to_steps_sequential;
      test_case "parallel" `Quick test_plan_to_steps_parallel;
      test_case "missing node" `Quick test_plan_to_steps_missing_node;
    ]);
  ]

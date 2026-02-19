(** Tests for Chain_telemetry module

    Type and event tests:
    - chain_event: yojson roundtrip
    - event_to_json_string / event_of_json_string: string roundtrip
    - string_of_event: pretty printing
    - Event constructors: chain_start, node_start, node_complete, etc.
    - Subscription API: subscribe, emit, unsubscribe
    - Running chains tracking
    - Event filtering
*)

open Alcotest
open Chain_telemetry
open Chain_category

(** {1 Test Helpers} *)

let token_usage_example : token_usage = {
  prompt_tokens = 100;
  completion_tokens = 50;
  total_tokens = 150;
  estimated_cost_usd = 0.01;
}

let verdict_pass = Pass "ok"
let verdict_fail = Fail "error"

(** {1 Event Constructor Tests} *)

let test_chain_start_constructor () =
  let event = chain_start ~chain_id:"test-123" ~nodes:5 () in
  match event with
  | ChainStart payload ->
      check string "chain_id" "test-123" payload.start_chain_id;
      check int "nodes" 5 payload.start_nodes;
      check (option string) "no mermaid" None payload.start_mermaid_dsl
  | _ -> fail "expected ChainStart"

let test_chain_start_with_mermaid () =
  let event = chain_start ~chain_id:"viz-chain" ~nodes:3 ~mermaid_dsl:"graph LR\n  a --> b" () in
  match event with
  | ChainStart payload ->
      check (option string) "mermaid" (Some "graph LR\n  a --> b") payload.start_mermaid_dsl
  | _ -> fail "expected ChainStart"

let test_node_start_constructor () =
  let event = node_start ~node_id:"n1" ~node_type:"llm" () in
  match event with
  | NodeStart payload ->
      check string "id" "n1" payload.node_start_id;
      check string "type" "llm" payload.node_start_type;
      check (option string) "no parent" None payload.node_parent
  | _ -> fail "expected NodeStart"

let test_node_start_with_parent () =
  let event = node_start ~node_id:"child" ~node_type:"tool" ~parent:"parent" () in
  match event with
  | NodeStart payload ->
      check (option string) "parent" (Some "parent") payload.node_parent
  | _ -> fail "expected NodeStart"

let test_node_complete_constructor () =
  let event = node_complete
    ~node_id:"done"
    ~duration_ms:1500
    ~tokens:token_usage_example
    ~verdict:verdict_pass
    ~confidence:0.95
    ()
  in
  match event with
  | NodeComplete payload ->
      check string "id" "done" payload.node_complete_id;
      check int "duration" 1500 payload.node_duration_ms;
      check int "tokens" 150 payload.node_tokens.total_tokens;
      check (float 0.01) "confidence" 0.95 payload.node_confidence;
      check (option string) "no preview" None payload.node_output_preview
  | _ -> fail "expected NodeComplete"

let test_node_complete_with_preview () =
  let event = node_complete
    ~node_id:"preview"
    ~duration_ms:100
    ~tokens:token_usage_example
    ~verdict:verdict_fail
    ~confidence:0.5
    ~output_preview:"Result: ..."
    ()
  in
  match event with
  | NodeComplete payload ->
      check (option string) "preview" (Some "Result: ...") payload.node_output_preview
  | _ -> fail "expected NodeComplete"

let test_chain_complete_constructor () =
  let event = chain_complete
    ~chain_id:"finished"
    ~duration_ms:5000
    ~tokens:token_usage_example
    ~executed:10
    ~skipped:2
  in
  match event with
  | ChainComplete payload ->
      check string "chain_id" "finished" payload.complete_chain_id;
      check int "duration" 5000 payload.complete_duration_ms;
      check int "executed" 10 payload.nodes_executed;
      check int "skipped" 2 payload.nodes_skipped
  | _ -> fail "expected ChainComplete"

let test_error_constructor () =
  let event = error ~node_id:"bad" ~message:"Connection timeout" ~retries:3 in
  match event with
  | Error payload ->
      check string "node_id" "bad" payload.error_node_id;
      check string "message" "Connection timeout" payload.error_message;
      check int "retries" 3 payload.error_retries
  | _ -> fail "expected Error"

(** {1 JSON Roundtrip Tests} *)

let test_chain_start_yojson_roundtrip () =
  let event = chain_start ~chain_id:"rt-test" ~nodes:5 ~mermaid_dsl:"graph" () in
  let json = chain_event_to_yojson event in
  match chain_event_of_yojson json with
  | Ok (ChainStart p) ->
      check string "chain_id" "rt-test" p.start_chain_id;
      check int "nodes" 5 p.start_nodes
  | Ok _ -> fail "wrong variant"
  | Error e -> fail e

let test_node_start_yojson_roundtrip () =
  let event = node_start ~node_id:"n1" ~node_type:"llm" ~parent:"root" () in
  let json = chain_event_to_yojson event in
  match chain_event_of_yojson json with
  | Ok (NodeStart p) ->
      check string "id" "n1" p.node_start_id;
      check (option string) "parent" (Some "root") p.node_parent
  | Ok _ -> fail "wrong variant"
  | Error e -> fail e

let test_node_complete_yojson_roundtrip () =
  let event = node_complete
    ~node_id:"n2"
    ~duration_ms:500
    ~tokens:token_usage_example
    ~verdict:verdict_pass
    ~confidence:0.9
    ~output_preview:"output"
    ()
  in
  let json = chain_event_to_yojson event in
  match chain_event_of_yojson json with
  | Ok (NodeComplete p) ->
      check int "duration" 500 p.node_duration_ms;
      check int "tokens" 150 p.node_tokens.total_tokens
  | Ok _ -> fail "wrong variant"
  | Error e -> fail e

let test_chain_complete_yojson_roundtrip () =
  let event = chain_complete
    ~chain_id:"done-chain"
    ~duration_ms:2000
    ~tokens:token_usage_example
    ~executed:5
    ~skipped:0
  in
  let json = chain_event_to_yojson event in
  match chain_event_of_yojson json with
  | Ok (ChainComplete p) ->
      check int "executed" 5 p.nodes_executed;
      check int "duration" 2000 p.complete_duration_ms
  | Ok _ -> fail "wrong variant"
  | Error e -> fail e

let test_error_yojson_roundtrip () =
  let event = error ~node_id:"err-node" ~message:"crash" ~retries:2 in
  let json = chain_event_to_yojson event in
  match chain_event_of_yojson json with
  | Ok (Error p) ->
      check string "message" "crash" p.error_message;
      check int "retries" 2 p.error_retries
  | Ok _ -> fail "wrong variant"
  | Error e -> fail e

(** {1 String Serialization Tests} *)

let test_event_string_roundtrip () =
  let event = chain_start ~chain_id:"str-test" ~nodes:3 () in
  let str = event_to_json_string event in
  match event_of_json_string str with
  | Ok (ChainStart p) ->
      check string "chain_id" "str-test" p.start_chain_id
  | Ok _ -> fail "wrong variant"
  | Error e -> fail e

let test_event_string_invalid () =
  match event_of_json_string "not valid json" with
  | Ok _ -> fail "should fail"
  | Error _ -> ()  (* expected *)

(** {1 Pretty Printing Tests} *)

let test_string_of_event_chain_start () =
  let event = chain_start ~chain_id:"pp-test" ~nodes:3 () in
  let str = string_of_event event in
  check bool "contains chain_id" true (Common.contains ~substring:"pp-test" str)

let test_string_of_event_error () =
  let event = error ~node_id:"err" ~message:"failure" ~retries:1 in
  let str = string_of_event event in
  check bool "contains message" true (Common.contains ~substring:"failure" str)

(** {1 Subscription Tests} *)

let test_subscribe_and_emit () =
  let received = ref [] in
  let handler ev = received := ev :: !received in
  let sub = subscribe handler in
  check bool "is_active" true (is_active sub);
  let event = chain_start ~chain_id:"sub-test" ~nodes:1 () in
  emit event;
  check int "received 1" 1 (List.length !received);
  unsubscribe sub;
  check bool "inactive" false (is_active sub)

let test_multiple_subscribers () =
  let count1 = ref 0 in
  let count2 = ref 0 in
  let sub1 = subscribe (fun _ -> incr count1) in
  let sub2 = subscribe (fun _ -> incr count2) in
  check int "2 subscribers" 2 (subscriber_count ());
  emit (chain_start ~chain_id:"multi" ~nodes:1 ());
  check int "count1" 1 !count1;
  check int "count2" 1 !count2;
  unsubscribe sub1;
  unsubscribe sub2

(** {1 Event Filtering Tests} *)

let test_chain_events_filter () =
  let event1 = chain_start ~chain_id:"c1" ~nodes:1 () in
  let event2 = node_start ~node_id:"n1" ~node_type:"llm" () in
  check bool "accepts chain_start" true (chain_events_only event1);
  check bool "rejects node_start" false (chain_events_only event2)

let test_node_events_filter () =
  let event1 = chain_start ~chain_id:"c1" ~nodes:1 () in
  let event2 = node_start ~node_id:"n1" ~node_type:"llm" () in
  check bool "rejects chain_start" false (node_events_only event1);
  check bool "accepts node_start" true (node_events_only event2)

let test_errors_only_filter () =
  let event1 = chain_start ~chain_id:"c1" ~nodes:1 () in
  let event2 = error ~node_id:"bad" ~message:"fail" ~retries:0 in
  check bool "rejects chain_start" false (errors_only event1);
  check bool "accepts error" true (errors_only event2)

let test_for_chain_filter () =
  let filter = for_chain "target-chain" in
  let event1 = chain_start ~chain_id:"target-chain" ~nodes:1 () in
  let event2 = chain_start ~chain_id:"other-chain" ~nodes:1 () in
  check bool "accepts target" true (filter event1);
  check bool "rejects other" false (filter event2)

(** {1 Running Chains Tests} *)

let test_running_chains_tracking () =
  register_running_chain ~chain_id:"track-test" ~total_nodes:10;
  let chains = get_running_chains () in
  check bool "has chain" true (List.exists (fun (id, _, _) -> id = "track-test") chains);
  update_chain_progress ~chain_id:"track-test" ~nodes_completed:5;
  unregister_running_chain ~chain_id:"track-test";
  let chains2 = get_running_chains () in
  check bool "removed" false (List.exists (fun (id, _, _) -> id = "track-test") chains2)

(** {1 Test Suite} *)

let constructor_tests = [
  test_case "chain_start" `Quick test_chain_start_constructor;
  test_case "chain_start with mermaid" `Quick test_chain_start_with_mermaid;
  test_case "node_start" `Quick test_node_start_constructor;
  test_case "node_start with parent" `Quick test_node_start_with_parent;
  test_case "node_complete" `Quick test_node_complete_constructor;
  test_case "node_complete with preview" `Quick test_node_complete_with_preview;
  test_case "chain_complete" `Quick test_chain_complete_constructor;
  test_case "error" `Quick test_error_constructor;
]

let yojson_tests = [
  test_case "ChainStart" `Quick test_chain_start_yojson_roundtrip;
  test_case "NodeStart" `Quick test_node_start_yojson_roundtrip;
  test_case "NodeComplete" `Quick test_node_complete_yojson_roundtrip;
  test_case "ChainComplete" `Quick test_chain_complete_yojson_roundtrip;
  test_case "Error" `Quick test_error_yojson_roundtrip;
]

let string_tests = [
  test_case "roundtrip" `Quick test_event_string_roundtrip;
  test_case "invalid" `Quick test_event_string_invalid;
]

let test_string_of_event_node_start () =
  let event = node_start ~node_id:"n1" ~node_type:"llm" () in
  let str = string_of_event event in
  check bool "contains NODE_START" true (Common.contains ~substring:"NODE_START" str);
  check bool "contains node_id" true (Common.contains ~substring:"n1" str);
  (* Test with parent *)
  let event2 = node_start ~node_id:"n2" ~node_type:"tool" ~parent:"n1" () in
  let str2 = string_of_event event2 in
  check bool "contains parent" true (Common.contains ~substring:"parent:" str2)

let test_string_of_event_node_complete () =
  let event = node_complete ~node_id:"n1" ~duration_ms:150 ~tokens:{ prompt_tokens = 10; completion_tokens = 20; total_tokens = 30; estimated_cost_usd = 0.0 }
    ~verdict:(Pass "good") ~confidence:0.95 () in
  let str = string_of_event event in
  check bool "contains NODE_COMPLETE" true (Common.contains ~substring:"NODE_COMPLETE" str);
  check bool "contains PASS" true (Common.contains ~substring:"PASS" str);
  check bool "contains duration" true (Common.contains ~substring:"150ms" str)

let test_string_of_event_chain_complete () =
  let event = chain_complete ~chain_id:"c1" ~duration_ms:500
    ~tokens:{ prompt_tokens = 100; completion_tokens = 200; total_tokens = 300; estimated_cost_usd = 0.0 }
    ~executed:3 ~skipped:1 in
  let str = string_of_event event in
  check bool "contains CHAIN_COMPLETE" true (Common.contains ~substring:"CHAIN_COMPLETE" str);
  check bool "contains nodes" true (Common.contains ~substring:"3/4" str)

(** {1 Event Logging Tests} *)

let test_logging_lifecycle () =
  (* Start fresh *)
  disable_logging ();
  clear_log ();
  (* Enable logging *)
  enable_logging ~max_size:100 ();
  (* Emit some events *)
  emit (chain_start ~chain_id:"log-test" ~nodes:2 ());
  emit (node_start ~node_id:"ln1" ~node_type:"llm" ());
  (* Get recent events *)
  let events = get_recent_events ~limit:10 () in
  check bool "events logged" true (List.length events >= 2);
  (* Enable again (should be idempotent) *)
  enable_logging ();
  (* Clear log *)
  clear_log ();
  let events2 = get_recent_events () in
  check int "cleared" 0 (List.length events2);
  (* Disable logging *)
  disable_logging ();
  (* Disable again (idempotent) *)
  disable_logging ()

(** {1 subscribe_filtered Tests} *)

let test_subscribe_filtered () =
  let received = ref [] in
  let sub = subscribe_filtered ~filter:errors_only (fun ev -> received := ev :: !received) in
  emit (chain_start ~chain_id:"filtered-test" ~nodes:1 ());
  emit (error ~node_id:"err1" ~message:"bad" ~retries:0);
  emit (node_start ~node_id:"n1" ~node_type:"llm" ());
  check int "only error" 1 (List.length !received);
  unsubscribe sub

(** {1 for_node Filter Tests} *)

let test_for_node_filter () =
  let filter = for_node "target-node" in
  let event_ns = node_start ~node_id:"target-node" ~node_type:"llm" () in
  let event_nc = node_complete ~node_id:"target-node" ~duration_ms:100
    ~tokens:{ prompt_tokens = 5; completion_tokens = 10; total_tokens = 15; estimated_cost_usd = 0.0 }
    ~verdict:(Pass "ok") ~confidence:0.9 () in
  let event_err = error ~node_id:"target-node" ~message:"err" ~retries:0 in
  let event_other = node_start ~node_id:"other-node" ~node_type:"llm" () in
  let event_chain = chain_start ~chain_id:"c1" ~nodes:1 () in
  check bool "accepts matching node_start" true (filter event_ns);
  check bool "accepts matching node_complete" true (filter event_nc);
  check bool "accepts matching error" true (filter event_err);
  check bool "rejects other node" false (filter event_other);
  check bool "rejects chain event" false (filter event_chain)

(** {1 Console Handler Tests} *)

let test_console_handler () =
  (* console_handler just prints, verify it does not raise *)
  let event = chain_start ~chain_id:"console-test" ~nodes:1 () in
  console_handler event;
  console_handler ~prefix:"[TEST]" event;
  ()

let pretty_print_tests = [
  test_case "ChainStart" `Quick test_string_of_event_chain_start;
  test_case "NodeStart" `Quick test_string_of_event_node_start;
  test_case "NodeComplete" `Quick test_string_of_event_node_complete;
  test_case "ChainComplete" `Quick test_string_of_event_chain_complete;
  test_case "Error" `Quick test_string_of_event_error;
]

let subscription_tests = [
  test_case "subscribe and emit" `Quick test_subscribe_and_emit;
  test_case "multiple subscribers" `Quick test_multiple_subscribers;
  test_case "subscribe_filtered" `Quick test_subscribe_filtered;
]

let filter_tests = [
  test_case "chain_events_only" `Quick test_chain_events_filter;
  test_case "node_events_only" `Quick test_node_events_filter;
  test_case "errors_only" `Quick test_errors_only_filter;
  test_case "for_chain" `Quick test_for_chain_filter;
  test_case "for_node" `Quick test_for_node_filter;
]

let logging_tests = [
  test_case "logging lifecycle" `Quick test_logging_lifecycle;
]

let console_tests = [
  test_case "console_handler" `Quick test_console_handler;
]

let tracking_tests = [
  test_case "running chains" `Quick test_running_chains_tracking;
]

let () =
  run "chain_telemetry" [
    ("constructors", constructor_tests);
    ("yojson_roundtrip", yojson_tests);
    ("string_serialization", string_tests);
    ("pretty_print", pretty_print_tests);
    ("subscription", subscription_tests);
    ("filters", filter_tests);
    ("logging", logging_tests);
    ("console", console_tests);
    ("tracking", tracking_tests);
  ]

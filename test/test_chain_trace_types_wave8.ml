(** Wave 8: chain_trace_types — trace_to_entry + traces_to_entries coverage *)

open Chain_trace_types

let check_string msg expected actual =
  Alcotest.(check string) msg expected actual

let check_float msg expected actual =
  Alcotest.(check (float 0.001)) msg expected actual

(* ── trace_to_entry ── *)
let test_trace_to_entry_node_start () =
  let t = { timestamp = 1.0; node_id = "n1";
            event = NodeStart { node_type = "llm"; attempt = 1 } } in
  let entry = trace_to_entry t "fallback" in
  check_string "node_id" "n1" entry.node_id;
  check_string "node_type from event" "llm" entry.node_type_name;
  check_float "timestamp" 1.0 entry.start_time;
  Alcotest.(check bool) "success" true (entry.status = `Success)

let test_trace_to_entry_node_complete_success () =
  let t = { timestamp = 2.0; node_id = "n2";
            event = NodeComplete { duration_ms = 100; success = true;
                                   node_type = "tool"; attempt = 1 } } in
  let entry = trace_to_entry t "default" in
  check_string "node_type" "tool" entry.node_type_name;
  Alcotest.(check bool) "success" true (entry.status = `Success);
  Alcotest.(check bool) "no error" true (entry.error = None)

let test_trace_to_entry_node_complete_failure () =
  let t = { timestamp = 3.0; node_id = "n3";
            event = NodeComplete { duration_ms = 50; success = false;
                                   node_type = "llm"; attempt = 2 } } in
  let entry = trace_to_entry t "default" in
  Alcotest.(check bool) "failure" true (entry.status = `Failure)

let test_trace_to_entry_node_error () =
  let t = { timestamp = 4.0; node_id = "n4";
            event = NodeError { message = "timeout"; error_class = Some "Timeout";
                                node_type = "gate"; attempt = 1 } } in
  let entry = trace_to_entry t "default" in
  check_string "node_type" "gate" entry.node_type_name;
  Alcotest.(check bool) "failure" true (entry.status = `Failure);
  check_string "error msg" "timeout" (Option.get entry.error)

let test_trace_to_entry_chain_start () =
  let t = { timestamp = 0.0; node_id = "c1";
            event = ChainStart { chain_id = "test-chain"; mermaid_dsl = Some "graph LR" } } in
  let entry = trace_to_entry t "pipeline" in
  (* ChainStart has no node_type in event, so uses fallback *)
  check_string "uses fallback name" "pipeline" entry.node_type_name;
  Alcotest.(check bool) "success" true (entry.status = `Success)

let test_trace_to_entry_chain_complete () =
  let t = { timestamp = 5.0; node_id = "c1";
            event = ChainComplete { chain_id = "test-chain"; success = true } } in
  let entry = trace_to_entry t "pipeline" in
  check_string "uses fallback name" "pipeline" entry.node_type_name;
  Alcotest.(check bool) "success" true (entry.status = `Success)

(* ── traces_to_entries ── *)
let test_traces_to_entries_empty () =
  let result = traces_to_entries [] in
  Alcotest.(check int) "empty" 0 (List.length result)

let test_traces_to_entries_paired () =
  let traces = [
    { timestamp = 1.0; node_id = "n1";
      event = NodeStart { node_type = "llm"; attempt = 1 } };
    { timestamp = 2.0; node_id = "n1";
      event = NodeComplete { duration_ms = 1000; success = true;
                             node_type = "llm"; attempt = 1 } };
  ] in
  let entries = traces_to_entries traces in
  Alcotest.(check int) "1 entry" 1 (List.length entries);
  let e = List.hd entries in
  check_string "node_id" "n1" e.node_id;
  check_string "type" "llm" e.node_type_name;
  Alcotest.(check bool) "success" true (e.status = `Success)

let test_traces_to_entries_with_error () =
  let traces = [
    { timestamp = 1.0; node_id = "n1";
      event = NodeStart { node_type = "tool"; attempt = 1 } };
    { timestamp = 2.0; node_id = "n1";
      event = NodeError { message = "fail"; error_class = None;
                          node_type = "tool"; attempt = 1 } };
  ] in
  let entries = traces_to_entries traces in
  Alcotest.(check int) "1 entry" 1 (List.length entries);
  let e = List.hd entries in
  Alcotest.(check bool) "failure" true (e.status = `Failure);
  check_string "error" "fail" (Option.get e.error)

let test_traces_to_entries_multiple_nodes () =
  let traces = [
    { timestamp = 1.0; node_id = "a";
      event = NodeStart { node_type = "llm"; attempt = 1 } };
    { timestamp = 1.5; node_id = "b";
      event = NodeStart { node_type = "tool"; attempt = 1 } };
    { timestamp = 2.0; node_id = "a";
      event = NodeComplete { duration_ms = 1000; success = true;
                             node_type = "llm"; attempt = 1 } };
    { timestamp = 3.0; node_id = "b";
      event = NodeComplete { duration_ms = 1500; success = false;
                             node_type = "tool"; attempt = 1 } };
  ] in
  let entries = traces_to_entries traces in
  Alcotest.(check int) "2 entries" 2 (List.length entries);
  let sorted = List.sort (fun (a : Chain_types.trace_entry) b ->
    String.compare a.node_id b.node_id) entries in
  let ea = List.nth sorted 0 in
  let eb = List.nth sorted 1 in
  check_string "first is a" "a" ea.node_id;
  Alcotest.(check bool) "a success" true (ea.status = `Success);
  check_string "second is b" "b" eb.node_id;
  Alcotest.(check bool) "b failure" true (eb.status = `Failure)

let test_traces_to_entries_chain_events () =
  let traces = [
    { timestamp = 0.0; node_id = "chain";
      event = ChainStart { chain_id = "c1"; mermaid_dsl = None } };
    { timestamp = 5.0; node_id = "chain";
      event = ChainComplete { chain_id = "c1"; success = true } };
  ] in
  let entries = traces_to_entries traces in
  Alcotest.(check int) "1 entry" 1 (List.length entries);
  let e = List.hd entries in
  check_string "unknown type" "unknown" e.node_type_name

let () =
  let open Alcotest in
  run "Chain_trace_types_wave8" [
    "trace_to_entry", [
      test_case "node_start" `Quick test_trace_to_entry_node_start;
      test_case "complete success" `Quick test_trace_to_entry_node_complete_success;
      test_case "complete failure" `Quick test_trace_to_entry_node_complete_failure;
      test_case "node error" `Quick test_trace_to_entry_node_error;
      test_case "chain start" `Quick test_trace_to_entry_chain_start;
      test_case "chain complete" `Quick test_trace_to_entry_chain_complete;
    ];
    "traces_to_entries", [
      test_case "empty" `Quick test_traces_to_entries_empty;
      test_case "paired start+complete" `Quick test_traces_to_entries_paired;
      test_case "with error" `Quick test_traces_to_entries_with_error;
      test_case "multiple nodes" `Quick test_traces_to_entries_multiple_nodes;
      test_case "chain events" `Quick test_traces_to_entries_chain_events;
    ];
  ]

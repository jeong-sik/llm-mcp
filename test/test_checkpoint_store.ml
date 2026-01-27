(** Tests for Checkpoint_store module

    Pure function tests:
    - checkpoint_to_json / checkpoint_of_json: JSON roundtrip
    - checkpoint_path: path generation
    - generate_run_id: format validation
    - make_checkpoint: constructor
*)

open Alcotest
open Checkpoint_store

(** {1 Test Helpers} *)

(** Create a minimal test checkpoint *)
let test_checkpoint ?(run_id="test_run_123") ?(chain_id="test_chain") () =
  {
    run_id;
    chain_id;
    node_id = "node_1";
    outputs = [("step1", "result1"); ("step2", "result2")];
    traces = [];  (* Empty for simplicity *)
    timestamp = 1704067200.0;  (* 2024-01-01 00:00:00 UTC *)
    total_tokens = None;
  }

(** {1 JSON Roundtrip Tests} *)

let test_json_roundtrip_basic () =
  let cp = test_checkpoint () in
  let json = checkpoint_to_json cp in
  match checkpoint_of_json json with
  | Ok cp2 ->
      check string "run_id preserved" cp.run_id cp2.run_id;
      check string "chain_id preserved" cp.chain_id cp2.chain_id;
      check string "node_id preserved" cp.node_id cp2.node_id;
      check int "outputs count" (List.length cp.outputs) (List.length cp2.outputs);
      check (float 0.001) "timestamp preserved" cp.timestamp cp2.timestamp
  | Error e -> fail e

let test_json_roundtrip_with_tokens () =
  let tokens : Chain_category.token_usage = {
    prompt_tokens = 100;
    completion_tokens = 50;
    total_tokens = 150;
    estimated_cost_usd = 0.0;
  } in
  let cp = { (test_checkpoint ()) with total_tokens = Some tokens } in
  let json = checkpoint_to_json cp in
  match checkpoint_of_json json with
  | Ok cp2 ->
      (match cp2.total_tokens with
       | Some t ->
           check int "prompt_tokens" 100 t.prompt_tokens;
           check int "completion_tokens" 50 t.completion_tokens;
           check int "total_tokens" 150 t.total_tokens
       | None -> fail "total_tokens should be Some")
  | Error e -> fail e

let test_json_roundtrip_empty_outputs () =
  let cp = { (test_checkpoint ()) with outputs = [] } in
  let json = checkpoint_to_json cp in
  match checkpoint_of_json json with
  | Ok cp2 -> check int "empty outputs" 0 (List.length cp2.outputs)
  | Error e -> fail e

(** {1 generate_run_id Tests} *)

let test_generate_run_id_format () =
  let run_id = generate_run_id () in
  (* Format: timestamp_randomhex *)
  check bool "contains underscore" true (String.contains run_id '_');
  let parts = String.split_on_char '_' run_id in
  check int "two parts" 2 (List.length parts);
  let hex_part = List.nth parts 1 in
  check int "hex part length" 4 (String.length hex_part)

let test_generate_run_id_unique () =
  let id1 = generate_run_id () in
  let id2 = generate_run_id () in
  (* Should be different (with high probability) *)
  check bool "ids different" true (id1 <> id2)

(** {1 make_checkpoint Tests} *)

let test_make_checkpoint () =
  let cp = make_checkpoint
    ~run_id:"test_run"
    ~chain_id:"test_chain"
    ~node_id:"node_a"
    ~outputs:[("a", "result_a")]
    ~traces:[]
    ()
  in
  check string "run_id" "test_run" cp.run_id;
  check string "chain_id" "test_chain" cp.chain_id;
  check string "node_id" "node_a" cp.node_id;
  check (option pass) "no tokens" None cp.total_tokens;
  check bool "timestamp set" true (cp.timestamp > 0.0)

let test_make_checkpoint_with_tokens () =
  let tokens : Chain_category.token_usage = {
    prompt_tokens = 200;
    completion_tokens = 100;
    total_tokens = 300;
    estimated_cost_usd = 0.0;
  } in
  let cp = make_checkpoint
    ~run_id:"run_with_tokens"
    ~chain_id:"chain_1"
    ~node_id:"final"
    ~outputs:[]
    ~traces:[]
    ~total_tokens:tokens
    ()
  in
  match cp.total_tokens with
  | Some t -> check int "has tokens" 300 t.total_tokens
  | None -> fail "expected tokens"

(** {1 create Tests} *)

let test_create_default () =
  let store = create () in
  check bool "base_dir not empty" true (String.length store.base_dir > 0);
  check bool "contains checkpoints" true (Common.contains ~substring:"checkpoints" store.base_dir)

let test_create_custom () =
  let store = create ~base_dir:"/custom/path" () in
  check string "custom base_dir" "/custom/path" store.base_dir

(** {1 Test Suite} *)

let json_tests = [
  test_case "basic roundtrip" `Quick test_json_roundtrip_basic;
  test_case "with tokens" `Quick test_json_roundtrip_with_tokens;
  test_case "empty outputs" `Quick test_json_roundtrip_empty_outputs;
]

let run_id_tests = [
  test_case "format" `Quick test_generate_run_id_format;
  test_case "uniqueness" `Quick test_generate_run_id_unique;
]

let make_tests = [
  test_case "basic" `Quick test_make_checkpoint;
  test_case "with tokens" `Quick test_make_checkpoint_with_tokens;
]

let create_tests = [
  test_case "default" `Quick test_create_default;
  test_case "custom" `Quick test_create_custom;
]

let () =
  run "checkpoint_store" [
    ("json_roundtrip", json_tests);
    ("generate_run_id", run_id_tests);
    ("make_checkpoint", make_tests);
    ("create", create_tests);
  ]

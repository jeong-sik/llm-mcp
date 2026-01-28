(** Tests for Chain_batch module - Batch scheduling pure functions

    Pure function tests:
    - calculate_delay: exponential backoff calculation
    - estimate_tokens: chain token estimation
    - chunk_chains: batch chunking strategies
*)

open Alcotest
open Chain_types
open Chain_batch

(** {1 Test Helpers} *)

(** Create a minimal chain for testing *)
let make_test_chain id prompt_len =
  let node = {
    id = "n1";
    node_type = Llm {
      model = "test";
      system = None;
      prompt = String.make prompt_len 'x';  (* prompt of given length *)
      timeout = None;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  make_chain ~id ~nodes:[node] ~output:"n1" ()

(** {1 calculate_delay Tests} *)

let default_retry_config : retry_config = {
  max_retries = 3;
  initial_delay_ms = 100;
  max_delay_ms = 10000;
  backoff_multiplier = 2.0;
}

let test_calculate_delay_first_attempt () =
  let delay = calculate_delay default_retry_config 0 in
  (* 100ms = 0.1s *)
  check (float 0.01) "first attempt" 0.1 delay

let test_calculate_delay_second_attempt () =
  let delay = calculate_delay default_retry_config 1 in
  (* 100 * 2^1 = 200ms = 0.2s *)
  check (float 0.01) "second attempt" 0.2 delay

let test_calculate_delay_third_attempt () =
  let delay = calculate_delay default_retry_config 2 in
  (* 100 * 2^2 = 400ms = 0.4s *)
  check (float 0.01) "third attempt" 0.4 delay

let test_calculate_delay_clamped_to_max () =
  let config = { default_retry_config with max_delay_ms = 300 } in
  let delay = calculate_delay config 5 in
  (* 100 * 2^5 = 3200, but clamped to 300ms = 0.3s *)
  check (float 0.01) "clamped" 0.3 delay

let test_calculate_delay_with_different_multiplier () =
  let config = { default_retry_config with backoff_multiplier = 3.0 } in
  let delay = calculate_delay config 2 in
  (* 100 * 3^2 = 900ms = 0.9s *)
  check (float 0.01) "3x multiplier" 0.9 delay

(** {1 estimate_tokens Tests} *)

let test_estimate_tokens_simple () =
  let chain = make_test_chain "test1" 100 in
  let tokens = estimate_tokens chain in
  (* 100 chars / 4 = ~25 tokens *)
  check bool "reasonable estimate" true (tokens >= 20 && tokens <= 30)

let test_estimate_tokens_long_prompt () =
  let chain = make_test_chain "test2" 1000 in
  let tokens = estimate_tokens chain in
  (* 1000 chars / 4 = ~250 tokens *)
  check bool "long prompt" true (tokens >= 200 && tokens <= 300)

let test_estimate_tokens_empty () =
  let chain = make_test_chain "test3" 0 in
  let tokens = estimate_tokens chain in
  check bool "empty prompt" true (tokens >= 0)

(** {1 chunk_chains Tests} *)

let test_chunk_item_based_exact () =
  let chains = List.init 6 (fun i -> make_test_chain (Printf.sprintf "c%d" i) 100) in
  let chunks = chunk_chains (ItemBased 2) chains in
  check int "chunk count" 3 (List.length chunks);
  List.iter (fun chunk -> check int "chunk size" 2 (List.length chunk)) chunks

let test_chunk_item_based_remainder () =
  let chains = List.init 5 (fun i -> make_test_chain (Printf.sprintf "c%d" i) 100) in
  let chunks = chunk_chains (ItemBased 2) chains in
  check int "chunk count" 3 (List.length chunks);
  check int "last chunk" 1 (List.length (List.nth chunks 2))

let test_chunk_item_based_empty () =
  let chunks = chunk_chains (ItemBased 2) [] in
  check int "empty" 0 (List.length chunks)

let test_chunk_item_based_single () =
  let chains = [make_test_chain "single" 100] in
  let chunks = chunk_chains (ItemBased 10) chains in
  check int "one chunk" 1 (List.length chunks);
  check int "one item" 1 (List.length (List.hd chunks))

let test_chunk_token_based_basic () =
  (* Create chains with ~25 tokens each (100 chars / 4) *)
  let chains = List.init 4 (fun i -> make_test_chain (Printf.sprintf "c%d" i) 100) in
  let chunks = chunk_chains (TokenBased 60) chains in
  (* Each chain ~25 tokens, max 60 per chunk -> ~2 per chunk *)
  check bool "has chunks" true (List.length chunks >= 1)

let test_chunk_adaptive_basic () =
  let chains = List.init 5 (fun i -> make_test_chain (Printf.sprintf "c%d" i) 100) in
  (* Adaptive takes target chunk size as int *)
  let chunks = chunk_chains (Adaptive 3) chains in
  check bool "has chunks" true (List.length chunks >= 1)

(** {1 Test Suite} *)

let delay_tests = [
  test_case "first attempt" `Quick test_calculate_delay_first_attempt;
  test_case "second attempt" `Quick test_calculate_delay_second_attempt;
  test_case "third attempt" `Quick test_calculate_delay_third_attempt;
  test_case "clamped to max" `Quick test_calculate_delay_clamped_to_max;
  test_case "different multiplier" `Quick test_calculate_delay_with_different_multiplier;
]

let estimate_tests = [
  test_case "simple" `Quick test_estimate_tokens_simple;
  test_case "long prompt" `Quick test_estimate_tokens_long_prompt;
  test_case "empty" `Quick test_estimate_tokens_empty;
]

let chunk_tests = [
  test_case "item exact" `Quick test_chunk_item_based_exact;
  test_case "item remainder" `Quick test_chunk_item_based_remainder;
  test_case "item empty" `Quick test_chunk_item_based_empty;
  test_case "item single" `Quick test_chunk_item_based_single;
  test_case "token basic" `Quick test_chunk_token_based_basic;
  test_case "adaptive basic" `Quick test_chunk_adaptive_basic;
]

let () =
  run "chain_batch" [
    ("calculate_delay", delay_tests);
    ("estimate_tokens", estimate_tests);
    ("chunk_chains", chunk_tests);
  ]

(** Tests for Chain_conversation module - Multi-turn conversation helpers

    Coverage targets:
    - estimate_tokens: empty, short, long strings
    - estimate_conversation_tokens: empty history, with messages, with summaries
    - make: defaults, custom models, empty models
    - add_message: adds to history, updates total_tokens
    - rotate_model: cycles through models, single model no-op
    - needs_summarization: below threshold, above threshold, within window
    - build_context_prompt: empty, with history, with summaries
    - summarize_history: success path, error path
    - maybe_summarize_and_rotate: triggered vs not triggered
*)

open Alcotest
open Chain_conversation

(* ── Token estimation ───────────────────────────────────── *)

let test_estimate_tokens_empty () =
  check int "empty" 0 (estimate_tokens "")

let test_estimate_tokens_short () =
  (* "hi" = 2 chars → (2 + 3) / 4 = 1 *)
  check int "hi" 1 (estimate_tokens "hi")

let test_estimate_tokens_medium () =
  (* "hello world" = 11 chars → (11 + 3) / 4 = 3 *)
  check int "hello world" 3 (estimate_tokens "hello world")

let test_estimate_tokens_long () =
  let s = String.make 400 'a' in
  (* 400 chars → (400 + 3) / 4 = 100 *)
  check int "400 chars" 100 (estimate_tokens s)

let test_estimate_conversation_tokens_empty () =
  let conv = make () in
  check int "empty conv" 0 (estimate_conversation_tokens conv)

let test_estimate_conversation_tokens_with_messages () =
  let conv = make () in
  add_message conv ~role:"user" ~content:"hello" ~iteration:1 ~model:"gemini";
  add_message conv ~role:"assistant" ~content:"world" ~iteration:1 ~model:"gemini";
  let tokens = estimate_conversation_tokens conv in
  check bool "positive" true (tokens > 0)

let test_estimate_conversation_tokens_with_summaries () =
  let conv = make () in
  conv.summaries <- ["previous context summary"];
  let tokens = estimate_conversation_tokens conv in
  check bool "includes summaries" true (tokens > 0)

(* ── Context creation ───────────────────────────────────── *)

let test_make_defaults () =
  let conv = make () in
  check string "default model" "gemini" conv.current_model;
  check int "model index" 0 conv.model_index;
  check int "token threshold" 6000 conv.token_threshold;
  check int "window size" 10 conv.window_size;
  check int "empty history" 0 (List.length conv.history);
  check int "zero tokens" 0 conv.total_tokens;
  check int "no summaries" 0 (List.length conv.summaries)

let test_make_custom_models () =
  let conv = make ~models:["claude"; "codex"] () in
  check string "first model" "claude" conv.current_model;
  check int "2 models" 2 (List.length conv.models)

let test_make_empty_models () =
  let conv = make ~models:[] () in
  check string "fallback model" "gemini" conv.current_model

let test_make_custom_threshold () =
  let conv = make ~token_threshold:1000 ~window_size:5 () in
  check int "threshold" 1000 conv.token_threshold;
  check int "window" 5 conv.window_size

(* ── Message management ─────────────────────────────────── *)

let test_add_message_single () =
  let conv = make () in
  add_message conv ~role:"user" ~content:"test" ~iteration:1 ~model:"gemini";
  check int "history length" 1 (List.length conv.history);
  check bool "tokens increased" true (conv.total_tokens > 0)

let test_add_message_multiple () =
  let conv = make () in
  add_message conv ~role:"user" ~content:"first" ~iteration:1 ~model:"gemini";
  add_message conv ~role:"assistant" ~content:"second" ~iteration:1 ~model:"gemini";
  add_message conv ~role:"user" ~content:"third" ~iteration:2 ~model:"claude";
  check int "history length" 3 (List.length conv.history);
  (* Newest first: third is at head *)
  let hd = List.hd conv.history in
  check string "newest role" "user" hd.role;
  check string "newest content" "third" hd.content;
  check int "newest iteration" 2 hd.iteration;
  check string "newest model" "claude" hd.model

(* ── Model rotation ─────────────────────────────────────── *)

let test_rotate_model_cycles () =
  let conv = make ~models:["a"; "b"; "c"] () in
  check string "initial" "a" conv.current_model;
  rotate_model conv;
  check string "after 1st rotate" "b" conv.current_model;
  rotate_model conv;
  check string "after 2nd rotate" "c" conv.current_model;
  rotate_model conv;
  check string "wraps to a" "a" conv.current_model

let test_rotate_model_single () =
  let conv = make ~models:["only"] () in
  rotate_model conv;
  check string "unchanged" "only" conv.current_model

let test_rotate_model_empty () =
  let conv = make ~models:[] () in
  rotate_model conv;
  check string "fallback unchanged" "gemini" conv.current_model

(* ── Summarization check ────────────────────────────────── *)

let test_needs_summarization_below_threshold () =
  let conv = make ~token_threshold:10000 ~window_size:5 () in
  add_message conv ~role:"user" ~content:"hi" ~iteration:1 ~model:"gemini";
  check bool "no summarization needed" false (needs_summarization conv)

let test_needs_summarization_above_threshold () =
  let conv = make ~token_threshold:10 ~window_size:2 () in
  (* Add enough messages to exceed threshold *)
  for i = 1 to 20 do
    add_message conv ~role:"user" ~content:(String.make 100 'x') ~iteration:i ~model:"gemini"
  done;
  check bool "summarization needed" true (needs_summarization conv)

let test_needs_summarization_within_window () =
  let conv = make ~token_threshold:10 ~window_size:100 () in
  (* Tokens exceeded but history within window *)
  for i = 1 to 5 do
    add_message conv ~role:"user" ~content:(String.make 100 'x') ~iteration:i ~model:"gemini"
  done;
  (* total_tokens > 10 but List.length history (5) <= window_size (100) *)
  check bool "within window" false (needs_summarization conv)

(* ── Build context prompt ───────────────────────────────── *)

let test_build_context_empty () =
  let conv = make () in
  let prompt = build_context_prompt conv in
  check string "empty" "" prompt

let test_build_context_with_history () =
  let conv = make () in
  add_message conv ~role:"user" ~content:"hello" ~iteration:1 ~model:"gemini";
  add_message conv ~role:"assistant" ~content:"world" ~iteration:1 ~model:"gemini";
  let prompt = build_context_prompt conv in
  check bool "has history section" true (Common.contains ~substring:"Recent History" prompt);
  check bool "has user message" true (Common.contains ~substring:"hello" prompt);
  check bool "has assistant message" true (Common.contains ~substring:"world" prompt)

let test_build_context_with_summaries () =
  let conv = make () in
  conv.summaries <- ["summary of earlier chat"];
  add_message conv ~role:"user" ~content:"next question" ~iteration:2 ~model:"claude";
  let prompt = build_context_prompt conv in
  check bool "has summary section" true (Common.contains ~substring:"Previous Context Summary" prompt);
  check bool "has summary text" true (Common.contains ~substring:"summary of earlier chat" prompt);
  check bool "has history" true (Common.contains ~substring:"next question" prompt)

let test_build_context_format () =
  let conv = make () in
  add_message conv ~role:"user" ~content:"test msg" ~iteration:3 ~model:"flash";
  let prompt = build_context_prompt conv in
  check bool "has role" true (Common.contains ~substring:"user" prompt);
  check bool "has model" true (Common.contains ~substring:"flash" prompt);
  check bool "has iteration" true (Common.contains ~substring:"iter 3" prompt)

(* ── Summarize history ──────────────────────────────────── *)

let mock_exec_fn_ok ~model:_ ?system:_ ~prompt:_ ?tools:_ ?thinking:_ () =
  Ok "Summarized: key decisions were made."

let mock_exec_fn_err ~model:_ ?system:_ ~prompt:_ ?tools:_ ?thinking:_ () =
  Error "network failure"

let test_summarize_history_success () =
  let conv = make ~token_threshold:10 ~window_size:2 () in
  for i = 1 to 6 do
    add_message conv ~role:"user" ~content:(Printf.sprintf "message %d" i) ~iteration:i ~model:"gemini"
  done;
  let summary = summarize_history ~exec_fn:mock_exec_fn_ok conv in
  check bool "has summary text" true (Common.contains ~substring:"Summarized" summary);
  (* After summarization: history trimmed to window_size, summary added *)
  check bool "history trimmed" true (List.length conv.history <= 2);
  check int "one summary" 1 (List.length conv.summaries)

let test_summarize_history_error () =
  let conv = make ~token_threshold:10 ~window_size:2 () in
  for i = 1 to 6 do
    add_message conv ~role:"user" ~content:(Printf.sprintf "msg %d" i) ~iteration:i ~model:"gemini"
  done;
  let summary = summarize_history ~exec_fn:mock_exec_fn_err conv in
  check bool "fallback text" true (Common.contains ~substring:"summarization failed" summary)

(* ── Maybe summarize and rotate ─────────────────────────── *)

let test_maybe_summarize_triggered () =
  let conv = make ~models:["a";"b"] ~token_threshold:10 ~window_size:2 () in
  for i = 1 to 10 do
    add_message conv ~role:"user" ~content:(String.make 50 'z') ~iteration:i ~model:"a"
  done;
  check string "before rotate" "a" conv.current_model;
  maybe_summarize_and_rotate ~exec_fn:mock_exec_fn_ok conv;
  (* Should have rotated *)
  check string "after rotate" "b" conv.current_model;
  check bool "summaries added" true (List.length conv.summaries > 0)

let test_maybe_summarize_not_triggered () =
  let conv = make ~models:["a";"b"] ~token_threshold:100000 ~window_size:100 () in
  add_message conv ~role:"user" ~content:"short" ~iteration:1 ~model:"a";
  maybe_summarize_and_rotate ~exec_fn:mock_exec_fn_ok conv;
  check string "not rotated" "a" conv.current_model;
  check int "no summaries" 0 (List.length conv.summaries)

(* ── Test suite ─────────────────────────────────────────── *)

let () =
  run "chain_conversation" [
    ("estimate_tokens", [
      test_case "empty" `Quick test_estimate_tokens_empty;
      test_case "short" `Quick test_estimate_tokens_short;
      test_case "medium" `Quick test_estimate_tokens_medium;
      test_case "long" `Quick test_estimate_tokens_long;
    ]);
    ("estimate_conversation_tokens", [
      test_case "empty conv" `Quick test_estimate_conversation_tokens_empty;
      test_case "with messages" `Quick test_estimate_conversation_tokens_with_messages;
      test_case "with summaries" `Quick test_estimate_conversation_tokens_with_summaries;
    ]);
    ("make", [
      test_case "defaults" `Quick test_make_defaults;
      test_case "custom models" `Quick test_make_custom_models;
      test_case "empty models" `Quick test_make_empty_models;
      test_case "custom threshold" `Quick test_make_custom_threshold;
    ]);
    ("add_message", [
      test_case "single" `Quick test_add_message_single;
      test_case "multiple" `Quick test_add_message_multiple;
    ]);
    ("rotate_model", [
      test_case "cycles" `Quick test_rotate_model_cycles;
      test_case "single model" `Quick test_rotate_model_single;
      test_case "empty models" `Quick test_rotate_model_empty;
    ]);
    ("needs_summarization", [
      test_case "below threshold" `Quick test_needs_summarization_below_threshold;
      test_case "above threshold" `Quick test_needs_summarization_above_threshold;
      test_case "within window" `Quick test_needs_summarization_within_window;
    ]);
    ("build_context_prompt", [
      test_case "empty" `Quick test_build_context_empty;
      test_case "with history" `Quick test_build_context_with_history;
      test_case "with summaries" `Quick test_build_context_with_summaries;
      test_case "format" `Quick test_build_context_format;
    ]);
    ("summarize_history", [
      test_case "success" `Quick test_summarize_history_success;
      test_case "error fallback" `Quick test_summarize_history_error;
    ]);
    ("maybe_summarize_and_rotate", [
      test_case "triggered" `Quick test_maybe_summarize_triggered;
      test_case "not triggered" `Quick test_maybe_summarize_not_triggered;
    ]);
  ]

(** Tests for Model_registry module

    Category resolution, passthrough, case insensitivity,
    registry queries, and model_info field invariants. *)

open Alcotest

let option_string = option string

(* ──────────────────────────────────────────
   resolve: category → Some "backend:model"
   ────────────────────────────────────────── *)

let test_resolve_reasoning () =
  let r = Model_registry.resolve "reasoning" in
  check bool "is Some" true (Option.is_some r);
  let v = Option.get r in
  check bool "starts with ollama:" true
    (String.length v > 7 && String.sub v 0 7 = "ollama:")

let test_resolve_coding () =
  let r = Model_registry.resolve "coding" in
  check bool "is Some" true (Option.is_some r);
  let v = Option.get r in
  check bool "starts with ollama:" true
    (String.length v > 7 && String.sub v 0 7 = "ollama:")

let test_resolve_general () =
  let r = Model_registry.resolve "general" in
  check bool "is Some" true (Option.is_some r)

let test_resolve_multimodal () =
  let r = Model_registry.resolve "multimodal" in
  check bool "is Some" true (Option.is_some r)

(* ──────────────────────────────────────────
   resolve: non-category → None (passthrough)
   ────────────────────────────────────────── *)

let test_resolve_passthrough () =
  check option_string "gemini" None (Model_registry.resolve "gemini");
  check option_string "claude" None (Model_registry.resolve "claude");
  check option_string "codex" None (Model_registry.resolve "codex");
  check option_string "ollama:qwen3:1.7b" None (Model_registry.resolve "ollama:qwen3:1.7b");
  check option_string "unknown-model" None (Model_registry.resolve "unknown-model")

(* ──────────────────────────────────────────
   resolve: case insensitive
   ────────────────────────────────────────── *)

let test_resolve_case_insensitive () =
  check bool "Reasoning" true (Option.is_some (Model_registry.resolve "Reasoning"));
  check bool "REASONING" true (Option.is_some (Model_registry.resolve "REASONING"));
  check bool "Coding" true (Option.is_some (Model_registry.resolve "Coding"));
  check bool "CODING" true (Option.is_some (Model_registry.resolve "CODING"));
  check bool "General" true (Option.is_some (Model_registry.resolve "General"))

(* ──────────────────────────────────────────
   category_of_string
   ────────────────────────────────────────── *)

let category_testable : Model_registry.category Alcotest.testable =
  Alcotest.testable
    (fun fmt c -> Format.fprintf fmt "%s" (Model_registry.category_to_string c))
    (=)

let option_category = option category_testable

let test_category_of_string () =
  check option_category "reasoning" (Some Reasoning) (Model_registry.category_of_string "reasoning");
  check option_category "coding" (Some Coding) (Model_registry.category_of_string "coding");
  check option_category "code alias" (Some Coding) (Model_registry.category_of_string "code");
  check option_category "general" (Some General) (Model_registry.category_of_string "general");
  check option_category "multimodal" (Some Multimodal) (Model_registry.category_of_string "multimodal");
  check option_category "vision alias" (Some Multimodal) (Model_registry.category_of_string "vision");
  check option_category "embedding" (Some Embedding) (Model_registry.category_of_string "embedding");
  check option_category "embed alias" (Some Embedding) (Model_registry.category_of_string "embed");
  check option_category "nonexistent" None (Model_registry.category_of_string "nonexistent")

(* ──────────────────────────────────────────
   by_category / all
   ────────────────────────────────────────── *)

let test_by_category_non_empty () =
  check bool "reasoning non-empty" true
    (List.length (Model_registry.by_category Reasoning) > 0);
  check bool "coding non-empty" true
    (List.length (Model_registry.by_category Coding) > 0);
  check bool "general non-empty" true
    (List.length (Model_registry.by_category General) > 0)

let test_all_non_empty () =
  check bool "all non-empty" true
    (List.length (Model_registry.all ()) > 0)

(* ──────────────────────────────────────────
   model_info field invariants
   ────────────────────────────────────────── *)

let test_model_info_invariants () =
  List.iter (fun (m : Model_registry.model_info) ->
    check bool (Printf.sprintf "%s name non-empty" m.name) true (String.length m.name > 0);
    check bool (Printf.sprintf "%s backend non-empty" m.name) true (String.length m.backend > 0);
    check bool (Printf.sprintf "%s context_length > 0" m.name) true (m.context_length > 0)
  ) (Model_registry.all ())

(* ──────────────────────────────────────────
   Dynamic availability (v2)
   ────────────────────────────────────────── *)

let test_availability_filtering () =
  (* Clear any previous state *)
  Model_registry.clear_available_models ();

  (* Before setting: all models available (backwards compat) *)
  let r1 = Model_registry.resolve "reasoning" in
  check bool "before set: reasoning resolves" true (Option.is_some r1);

  (* Set only one model as available *)
  Model_registry.set_available_models ["deepseek-r1:1.5b"];

  (* Now reasoning should resolve to deepseek (only available one) *)
  let r2 = Model_registry.resolve "reasoning" in
  check bool "after set: reasoning resolves" true (Option.is_some r2);
  let v2 = Option.get r2 in
  check bool "resolves to available model" true
    (String.length v2 > 0 && (String.sub v2 (String.length v2 - 14) 14 = "deepseek-r1:1.5b"
     || v2 = "ollama:deepseek-r1:1.5b"));

  (* Cleanup *)
  Model_registry.clear_available_models ()

let test_fallback_when_none_available () =
  Model_registry.clear_available_models ();

  (* Set models that don't match any in registry *)
  Model_registry.set_available_models ["nonexistent-model"];

  (* Should fallback to first in registry *)
  let r = Model_registry.resolve "reasoning" in
  check bool "fallback: reasoning still resolves" true (Option.is_some r);

  Model_registry.clear_available_models ()

let test_non_ollama_always_available () =
  Model_registry.clear_available_models ();
  Model_registry.set_available_models [];  (* Empty = no ollama models *)

  (* Multimodal (gemini backend) should still resolve *)
  let r = Model_registry.resolve "multimodal" in
  check bool "multimodal resolves even with empty ollama" true (Option.is_some r);

  Model_registry.clear_available_models ()

(* ──────────────────────────────────────────
   Test runner
   ────────────────────────────────────────── *)

let () =
  run "Model_registry" [
    "resolve", [
      test_case "reasoning" `Quick test_resolve_reasoning;
      test_case "coding" `Quick test_resolve_coding;
      test_case "general" `Quick test_resolve_general;
      test_case "multimodal" `Quick test_resolve_multimodal;
      test_case "passthrough" `Quick test_resolve_passthrough;
      test_case "case insensitive" `Quick test_resolve_case_insensitive;
    ];
    "category_of_string", [
      test_case "all categories" `Quick test_category_of_string;
    ];
    "registry queries", [
      test_case "by_category non-empty" `Quick test_by_category_non_empty;
      test_case "all non-empty" `Quick test_all_non_empty;
    ];
    "model_info", [
      test_case "field invariants" `Quick test_model_info_invariants;
    ];
    "availability (v2)", [
      test_case "filtering" `Quick test_availability_filtering;
      test_case "fallback" `Quick test_fallback_when_none_available;
      test_case "non-ollama always available" `Quick test_non_ollama_always_available;
    ];
  ]

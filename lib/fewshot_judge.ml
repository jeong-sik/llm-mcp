(** Few-shot Judge - LLM-based judgment for few-shot injection

    2-stage architecture:
    1. Fast LLM (qwen:0.6b ~200ms) judges if few-shot needed
    2. If needed, inject appropriate few-shot before main LLM call

    Replaces heuristic keyword matching with intelligent classification.
*)

(** Available few-shot categories *)
type fewshot_category =
  | OCaml_5x_Effects      (* Effect handlers, algebraic effects *)
  | Rust_Async            (* tokio, async/await patterns *)
  | React_Server          (* RSC, Server Actions *)
  | Haskell_Modern        (* lens, mtl, modern Haskell *)
  | None_Needed           (* No few-shot required *)

(** Convert category to filename *)
let category_to_file = function
  | OCaml_5x_Effects -> Some "ocaml-5x-effects.md"
  | Rust_Async -> Some "rust-async.md"
  | React_Server -> Some "react-server.md"
  | Haskell_Modern -> Some "haskell-modern.md"
  | None_Needed -> None

(** Parse LLM response to category *)
let parse_category response =
  let r = String.lowercase_ascii response in
  if String.length r > 200 then None_Needed  (* Too long = invalid *)
  else if Fewshot_injector.contains_ci r "ocaml" ||
          Fewshot_injector.contains_ci r "effect" then OCaml_5x_Effects
  else if Fewshot_injector.contains_ci r "rust" ||
          Fewshot_injector.contains_ci r "tokio" then Rust_Async
  else if Fewshot_injector.contains_ci r "react" ||
          Fewshot_injector.contains_ci r "server" then React_Server
  else if Fewshot_injector.contains_ci r "haskell" ||
          Fewshot_injector.contains_ci r "lens" then Haskell_Modern
  else None_Needed

(** Judge prompt for few-shot injection *)
let judge_prompt = {|You are a classifier. Given a user prompt, determine if it needs few-shot examples.

Categories:
- ocaml: OCaml 5.x Effect handlers, algebraic effects
- rust: Rust async, tokio, async/await
- react: React Server Components, Server Actions, RSC
- haskell: Modern Haskell, lens, mtl, monad transformers
- none: No specialized few-shot needed

Respond with ONLY the category name (ocaml/rust/react/haskell/none).

User prompt: |}

(** Build the judgment request
    Returns: (model, prompt) for fast LLM call *)
let build_judge_request user_prompt =
  let full_prompt = judge_prompt ^ user_prompt in
  ("qwen3:0.6b", full_prompt)  (* Fast small model *)

(** Process judgment response and get few-shot content *)
let process_judgment response =
  let category = parse_category response in
  match category_to_file category with
  | None -> None
  | Some filename -> Fewshot_injector.load_fewshot filename

(** Hybrid mode: Try heuristic first, fall back to LLM if uncertain
    This saves the LLM call for obvious cases *)
let should_use_llm_judge prompt =
  (* If heuristic has high confidence, skip LLM judge *)
  let (_, detected) = Fewshot_injector.inject_safe prompt in
  match detected with
  | Some _ -> false  (* Heuristic found something, trust it *)
  | None ->
      (* Heuristic didn't find anything, but prompt looks technical *)
      let technical_keywords = [
        "code"; "implement"; "write"; "function"; "type";
        "async"; "effect"; "monad"; "component"; "handler"
      ] in
      List.exists (fun kw -> Fewshot_injector.contains_ci prompt kw) technical_keywords

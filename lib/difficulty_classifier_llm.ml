(** LLM-based Difficulty Classifier for Cascade Routing

    Uses glm-4.7-flash via exec_fn for query classification.
    Zero external dependency beyond exec_fn — the chain engine's
    existing model routing handles Ollama HTTP calls.

    Falls back to heuristic {!Difficulty_classifier.classify_full}
    on LLM failure or parse error.

    MANIFEST: "휴리스틱한 구현을 하기 전에 LLM 구현을 먼저 고민한다.
    비용보다 품질이다." *)

let classification_system_prompt =
  "Classify the user query. Reply ONLY with exactly two lines, nothing else:\n\
   DIFFICULTY: easy|medium|hard\n\
   CATEGORY: coding|reasoning|general\n\
   \n\
   Rules:\n\
   - easy: greetings, simple facts, short questions with no technical content\n\
   - medium: explanations, moderate technical questions, multi-sentence queries\n\
   - hard: code generation, debugging, complex analysis, architecture, multi-step reasoning\n\
   - coding: anything about code, programming, debugging, implementation\n\
   - reasoning: analysis, comparison, proof, math, tradeoffs, evaluation\n\
   - general: everything else"

let default_model = "ollama:glm-4.7-flash"

(* ──────────────────────────────────────────────────────────────
   Response parsing
   ────────────────────────────────────────────────────────────── *)

let starts_with_ci ~prefix str =
  let plen = String.length prefix in
  String.length str >= plen &&
  String.lowercase_ascii (String.sub str 0 plen) = String.lowercase_ascii prefix

let parse_response response =
  let lines = String.split_on_char '\n' response in
  let find_value prefix =
    List.find_map (fun line ->
      let trimmed = String.trim line in
      if starts_with_ci ~prefix trimmed then
        let plen = String.length prefix in
        let value = String.trim (String.sub trimmed plen (String.length trimmed - plen)) in
        Some (String.lowercase_ascii value)
      else
        None
    ) lines
  in
  match find_value "DIFFICULTY:", find_value "CATEGORY:" with
  | Some d, Some c ->
    let difficulty = Difficulty_classifier.difficulty_of_string d in
    let category = match c with
      | "coding" -> Difficulty_classifier.Coding
      | "reasoning" -> Difficulty_classifier.Reasoning
      | "general" -> Difficulty_classifier.General
      | _ -> Difficulty_classifier.General
    in
    Some (difficulty, category)
  | _ -> None

(* ──────────────────────────────────────────────────────────────
   Classification entry point
   ────────────────────────────────────────────────────────────── *)

let classify ~exec_fn ?(model = default_model) query =
  let features = Difficulty_classifier.extract_features query in
  match exec_fn ~model ~system:classification_system_prompt ~prompt:query with
  | Ok response ->
    begin match parse_response response with
    | Some (difficulty, category) ->
      { Difficulty_classifier.difficulty; category; features }
    | None ->
      (* LLM responded but output unparseable — heuristic fallback *)
      Difficulty_classifier.classify_full query
    end
  | Error _msg ->
    (* LLM call failed — heuristic fallback *)
    Difficulty_classifier.classify_full query

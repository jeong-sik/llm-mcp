(** Difficulty Classifier for Cascade Routing

    Heuristic-based query difficulty classification.
    No LLM calls — pure string analysis for zero-overhead routing decisions.

    Classification signals:
    - Input length (character count)
    - Code pattern presence (backticks, braces, arrows, keywords)
    - Multiline content
    - Technical keyword density
    - Question complexity (compound/nested questions) *)

type difficulty =
  | Easy
  | Medium
  | Hard
[@@deriving yojson]

type category_hint =
  | Coding
  | Reasoning
  | General
[@@deriving yojson]

type features = {
  input_length : int;
  has_code : bool;
  has_multiline : bool;
  question_depth : int;
  domain_keywords : int;
}
[@@deriving yojson]

type classification = {
  difficulty : difficulty;
  category : category_hint;
  features : features;
}
[@@deriving yojson]

(* ──────────────────────────────────────────────────────────────
   String helpers (self-contained, no external dependency)
   ────────────────────────────────────────────────────────────── *)

let contains_substring ~sub str =
  let sub_len = String.length sub in
  let str_len = String.length str in
  if sub_len > str_len then false
  else
    let found = ref false in
    let i = ref 0 in
    while !i <= str_len - sub_len && not !found do
      if String.sub str !i sub_len = sub then found := true
      else incr i
    done;
    !found

let lowercase s = String.lowercase_ascii s

(* ──────────────────────────────────────────────────────────────
   Code pattern detection
   ────────────────────────────────────────────────────────────── *)

(** Code-indicative patterns: backtick blocks, braces, arrows, keywords *)
let code_patterns = [
  "```";      (* fenced code block *)
  "` ";       (* inline code start *)
  " `";       (* inline code boundary *)
  "->";       (* arrow operator *)
  "=>";       (* fat arrow *)
  "let ";     (* OCaml/JS binding *)
  "def ";     (* Python function *)
  "fn ";      (* Rust function *)
  "func ";    (* Go function *)
  "class ";   (* class definition *)
  "import ";  (* module import *)
  "match ";   (* pattern matching *)
  "impl ";    (* implementation block *)
  "struct ";  (* struct definition *)
  "module ";  (* module definition *)
]

(** Characters that strongly indicate code content *)
let code_chars = ['{'; '}'; '|'; '\\']

let has_code_content query =
  let q = lowercase query in
  (* Check for code-indicative substrings *)
  let has_pattern = List.exists (fun pat -> contains_substring ~sub:pat q) code_patterns in
  (* Check for code-indicative characters *)
  let has_char = List.exists (fun c -> String.contains query c) code_chars in
  (* Backtick count >= 2 suggests code *)
  let backtick_count = String.fold_left (fun acc c -> if c = '`' then acc + 1 else acc) 0 query in
  has_pattern || has_char || backtick_count >= 2

(* ──────────────────────────────────────────────────────────────
   Technical keyword detection
   ────────────────────────────────────────────────────────────── *)

(** Technical domain keywords indicating complexity *)
let tech_keywords = [
  (* Architecture/Design *)
  "api"; "database"; "algorithm"; "architecture"; "security";
  "performance"; "migration"; "microservice"; "deployment";
  (* Programming concepts *)
  "refactor"; "debug"; "optimize"; "implement"; "compile";
  "runtime"; "memory"; "thread"; "concurrent"; "async";
  (* Specific technologies *)
  "docker"; "kubernetes"; "graphql"; "rest"; "grpc";
  "postgres"; "redis"; "nginx"; "webpack"; "typescript";
]

(** Reasoning keywords indicating analytical depth *)
let reasoning_keywords = [
  "compare"; "tradeoff"; "trade-off"; "pros and cons";
  "difference between"; "when should i"; "why is";
  "analyze"; "evaluate"; "prove"; "derive";
  "complexity"; "big-o"; "optimal"; "mathematical";
]

(** Coding-specific keywords *)
let coding_keywords = [
  "function"; "variable"; "error"; "bug"; "fix";
  "code"; "write"; "generate"; "test"; "unit test";
  "review"; "refactor"; "type"; "interface"; "abstract";
]

let count_keyword_matches keywords query =
  let q = lowercase query in
  List.fold_left (fun acc kw ->
    if contains_substring ~sub:kw q then acc + 1 else acc
  ) 0 keywords

(* ──────────────────────────────────────────────────────────────
   Question depth detection
   ────────────────────────────────────────────────────────────── *)

(** Count question marks and compound question indicators *)
let detect_question_depth query =
  let q = lowercase query in
  let question_marks = String.fold_left (fun acc c -> if c = '?' then acc + 1 else acc) 0 query in
  let compound_indicators = [
    "and also"; "additionally"; "furthermore"; "moreover";
    "in addition"; "as well as"; "on top of";
  ] in
  let compounds = count_keyword_matches compound_indicators q in
  question_marks + compounds

(* ──────────────────────────────────────────────────────────────
   Feature extraction
   ────────────────────────────────────────────────────────────── *)

let extract_features query =
  let input_length = String.length query in
  let has_code = has_code_content query in
  let has_multiline = String.contains query '\n' in
  let question_depth = detect_question_depth query in
  let domain_keywords = count_keyword_matches tech_keywords query in
  { input_length; has_code; has_multiline; question_depth; domain_keywords }

(* ──────────────────────────────────────────────────────────────
   Category classification
   ────────────────────────────────────────────────────────────── *)

(** Determine query category based on keyword density *)
let classify_category query =
  let coding_score = count_keyword_matches coding_keywords query in
  let reasoning_score = count_keyword_matches reasoning_keywords query in
  let has_code = has_code_content query in
  (* Code content is a strong signal *)
  let coding_score = if has_code then coding_score + 3 else coding_score in
  if coding_score > reasoning_score && coding_score > 0 then Coding
  else if reasoning_score > 0 then Reasoning
  else General

(* ──────────────────────────────────────────────────────────────
   Difficulty classification
   ────────────────────────────────────────────────────────────── *)

(** Classify query difficulty from extracted features.

    Decision boundaries:
    - Easy: short (<80 chars), no code, no multiline, 0-1 tech keywords, 0 nested questions
    - Hard: long (>500 chars) OR code content OR 5+ tech keywords OR 2+ questions
    - Medium: everything else *)
let classify_from_features (f : features) : difficulty =
  (* Hard signals (any one triggers Hard) *)
  if f.has_code then Hard
  else if f.input_length > 500 then Hard
  else if f.domain_keywords >= 5 then Hard
  else if f.question_depth >= 2 then Hard
  else if f.has_multiline && f.domain_keywords >= 3 then Hard
  (* Easy signals (all must be true) *)
  else if f.input_length < 80 && f.domain_keywords <= 1 && f.question_depth <= 1
          && not f.has_multiline then Easy
  (* Everything else is Medium *)
  else Medium

let classify query =
  let features = extract_features query in
  classify_from_features features

let classify_full query =
  let features = extract_features query in
  let difficulty = classify_from_features features in
  let category = classify_category query in
  { difficulty; category; features }

(* ──────────────────────────────────────────────────────────────
   Conversion helpers
   ────────────────────────────────────────────────────────────── *)

let category_to_model_registry = function
  | Coding -> Model_registry.Coding
  | Reasoning -> Model_registry.Reasoning
  | General -> Model_registry.General

let difficulty_to_string = function
  | Easy -> "easy"
  | Medium -> "medium"
  | Hard -> "hard"

let difficulty_of_string s =
  match lowercase s with
  | "easy" -> Easy
  | "medium" -> Medium
  | "hard" | _ -> Hard

let category_hint_to_string = function
  | Coding -> "coding"
  | Reasoning -> "reasoning"
  | General -> "general"

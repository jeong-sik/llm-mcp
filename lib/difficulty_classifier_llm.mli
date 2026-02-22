(** LLM-based Difficulty Classifier for Cascade Routing

    Uses an LLM (default: glm-4.7-flash via Ollama) for query classification.
    Falls back to heuristic classifier on LLM call failure or parse error.

    MANIFEST: "휴리스틱한 구현을 하기 전에 LLM 구현을 먼저 고민한다.
    비용보다 품질이다." *)

val classify :
  exec_fn:(model:string -> system:string -> prompt:string -> (string, string) result) ->
  ?model:string ->
  string ->
  Difficulty_classifier.classification
(** [classify ~exec_fn ?model query] classifies query difficulty using LLM.
    [exec_fn] is the chain engine's model execution function, simplified
    to (model, system, prompt) -> result.
    Falls back to {!Difficulty_classifier.classify_full} on any error. *)

val parse_response : string -> (Difficulty_classifier.difficulty * Difficulty_classifier.category_hint) option
(** [parse_response response] extracts difficulty and category from LLM output.
    Expected format: "DIFFICULTY: easy\nCATEGORY: coding".
    Returns [None] if parsing fails. Exposed for testing. *)

val classification_system_prompt : string
(** System prompt used for LLM classification. *)

val default_model : string
(** Default model for classification: "ollama:glm-4.7-flash". *)

(** Difficulty Classifier for Cascade Routing

    Classifies query difficulty using heuristic features to enable
    difficulty-aware cascade routing. Easy queries stay local (no API calls),
    medium queries allow limited escalation, hard queries use full cascade.

    Reference: "A Unified Approach to Routing and Cascading for LLMs"
    (ETH Zurich, ICLR 2025) — routing + cascading integration outperforms
    either approach alone.

    Cost model: Local models (GLM, qwen) have fixed VRAM cost.
    External APIs (Gemini, Claude) have per-token cost.
    This classifier reduces external API calls for queries that
    local models can handle. *)

(** Query difficulty level *)
type difficulty =
  | Easy    (** Short, simple queries — local model only, no escalation *)
  | Medium  (** Moderate complexity — local model + 1 escalation max *)
  | Hard    (** Complex queries — full cascade (existing behavior) *)
[@@deriving yojson]

(** Category hint for model selection *)
type category_hint =
  | Coding     (** Code generation, review, debugging *)
  | Reasoning  (** Math, logic, analysis, comparison *)
  | General    (** General chat, simple questions *)
[@@deriving yojson]

(** Extracted features from query text *)
type features = {
  input_length : int;       (** Character count *)
  has_code : bool;          (** Contains code patterns (backticks, braces, arrows) *)
  has_multiline : bool;     (** Contains newline characters *)
  question_depth : int;     (** Number of nested/compound questions *)
  domain_keywords : int;    (** Technical keyword density *)
}
[@@deriving yojson]

(** Full classification result *)
type classification = {
  difficulty : difficulty;
  category : category_hint;
  features : features;
}
[@@deriving yojson]

val classify : string -> difficulty
(** [classify query] returns difficulty based on heuristic features.
    Lightweight — suitable for every incoming query. *)

val classify_full : string -> classification
(** [classify_full query] returns difficulty + category + extracted features.
    Use when model selection needs category information. *)

val extract_features : string -> features
(** [extract_features query] extracts heuristic features from query text. *)

val category_to_model_registry : category_hint -> Model_registry.category
(** Maps classifier category to Model_registry category for model resolution.
    [Coding] -> [Model_registry.Coding], etc. *)

val difficulty_to_string : difficulty -> string
(** Convert difficulty to lowercase string: "easy", "medium", "hard" *)

val difficulty_of_string : string -> difficulty
(** Parse difficulty from string. Defaults to [Hard] for unknown input. *)

val category_hint_to_string : category_hint -> string
(** Convert category hint to string: "coding", "reasoning", "general" *)

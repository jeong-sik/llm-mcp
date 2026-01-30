(** Model Category Registry

    Maps semantic categories (reasoning, coding, general, etc.)
    to concrete model names for the Chain DSL.
    Usage: [LLM:reasoning 'prompt'] resolves to a concrete ollama model. *)

type category =
  | Reasoning
  | Coding
  | General
  | Multimodal
  | Embedding

type model_info = {
  name : string;
  category : category;
  backend : string;
  context_length : int;
  thinking_style : bool;
}

val category_to_string : category -> string
(** [category_to_string cat] returns the lowercase string for a category. *)

val category_of_string : string -> category option
(** [category_of_string s] parses a category from a case-insensitive string.
    Aliases: ["code"] → [Coding], ["vision"] → [Multimodal], ["embed"] → [Embedding]. *)

val all : unit -> model_info list
(** [all ()] returns the full static registry. *)

val by_category : category -> model_info list
(** [by_category cat] filters models by category. First element is the default. *)

(** {2 Dynamic Availability (v2)} *)

val set_available_models : string list -> unit
(** [set_available_models names] sets the list of available ollama models.
    Call with output of [ollama list] before resolving categories.
    Empty list = no filtering (all considered available). *)

val clear_available_models : unit -> unit
(** [clear_available_models ()] resets availability to "all available". *)

val is_available : model_info -> bool
(** [is_available m] returns true if model is available.
    Non-ollama backends always return true. *)

val resolve : string -> string option
(** [resolve s] resolves a category string to a concrete ["backend:model"] name.
    Filters by availability and falls back through the preference list.
    Returns [None] if [s] is not a recognized category (pass-through). *)

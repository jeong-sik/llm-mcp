(** Prompt Registry - Versioned template storage and management

    This module provides a registry for storing and managing prompt templates
    with versioning, variable extraction, and usage metrics tracking.

    @author Chain Engine
    @since 0.3.0
*)

(** {1 Types} *)

(** Usage metrics for tracking prompt effectiveness *)
type prompt_metrics = {
  usage_count: int;      (** Number of times this prompt has been used *)
  avg_score: float;      (** Average quality score (0.0 - 1.0) *)
  last_used: float;      (** Unix timestamp of last usage *)
}

(** A single prompt entry in the registry *)
type prompt_entry = {
  id: string;                     (** Unique identifier *)
  template: string;               (** Prompt template with {{var}} placeholders *)
  version: string;                (** Semantic version string *)
  variables: string list;         (** Extracted variable names from template *)
  metrics: prompt_metrics option; (** Optional usage metrics *)
  created_at: float;              (** Unix timestamp of creation *)
  deprecated: bool;               (** Whether this prompt is deprecated *)
}

(** Registry statistics *)
type registry_stats = {
  total_prompts: int;
  active_prompts: int;
  deprecated_prompts: int;
  most_used: string option;
  avg_usage: float;
}

(** {1 JSON Serialization} *)

val prompt_metrics_to_yojson : prompt_metrics -> Yojson.Safe.t
val prompt_metrics_of_yojson : Yojson.Safe.t -> (prompt_metrics, string) result
val prompt_entry_to_yojson : prompt_entry -> Yojson.Safe.t
val prompt_entry_of_yojson : Yojson.Safe.t -> (prompt_entry, string) result

(** {1 Variable Extraction} *)

(** Extract variable names from a template string.
    Matches {{variable_name}} patterns.

    @param template The template string to extract variables from
    @return List of unique variable names found in the template

    Example:
    {[
      extract_variables "Hello {{name}}, your score is {{score}}"
      (* Returns ["name"; "score"] *)
    ]}
*)
val extract_variables : string -> string list

(** {1 Initialization} *)

(** Initialize the registry with optional file persistence.

    @param persist_dir Optional directory for JSON file persistence
*)
val init : ?persist_dir:string -> unit -> unit

(** {1 Registration and Lookup} *)

(** Register a prompt entry in the registry.
    Automatically extracts variables if not provided.

    @param entry The prompt entry to register
*)
val register : prompt_entry -> unit

(** Get a prompt entry by ID and optional version.
    If version is not specified, returns the latest non-deprecated version.

    @param id The prompt ID
    @param version Optional specific version to retrieve
    @return The prompt entry if found
*)
val get : id:string -> ?version:string -> unit -> prompt_entry option

(** Get all versions of a prompt by ID.

    @param id The prompt ID
    @return List of all versions of the prompt
*)
val get_versions : id:string -> unit -> prompt_entry list

(** List all registered prompt entries.

    @return All prompt entries in the registry
*)
val list_all : unit -> prompt_entry list

(** List all prompt IDs (unique, without versions).

    @return All unique prompt IDs
*)
val list_ids : unit -> string list

(** Check if a prompt exists.

    @param id The prompt ID
    @param version Optional specific version to check
    @return true if the prompt exists
*)
val exists : id:string -> ?version:string -> unit -> bool

(** Unregister a prompt entry.
    If no version is specified, removes all versions.

    @param id The prompt ID
    @param version Optional specific version to remove
    @return true if removed successfully
*)
val unregister : id:string -> ?version:string -> unit -> bool

(** Mark a prompt as deprecated.

    @param id The prompt ID
    @param version The version to deprecate
    @return true if deprecated successfully
*)
val deprecate : id:string -> version:string -> unit -> bool

(** {1 Metrics} *)

(** Update usage metrics after a prompt is used.

    @param id The prompt ID
    @param version The version used
    @param score Quality score (0.0 - 1.0)
*)
val update_metrics : id:string -> version:string -> score:float -> unit -> unit

(** {1 Template Rendering} *)

(** Render a prompt template with the given variables.

    @param template The template string
    @param vars List of (variable_name, value) pairs
    @return Rendered string or error message
*)
val render_template : template:string -> vars:(string * string) list -> unit -> (string, string) result

(** Render a registered prompt by ID with the given variables.

    @param id The prompt ID
    @param version Optional specific version
    @param vars List of (variable_name, value) pairs
    @return Rendered string or error message
*)
val render : id:string -> ?version:string -> vars:(string * string) list -> unit -> (string, string) result

(** {1 Statistics} *)

(** Get registry statistics.

    @return Statistics about the registry
*)
val stats : unit -> registry_stats

(** {1 Utility Functions} *)

(** Clear all registered prompts. *)
val clear : unit -> unit

(** Count of registered prompts (all versions). *)
val count : unit -> int

(** Count of unique prompt IDs. *)
val count_unique : unit -> int

(** Export registry to JSON.

    @return JSON representation of all prompt entries
*)
val to_json : unit -> Yojson.Safe.t

(** Import registry from JSON.

    @param json JSON array of prompt entries
    @return Number of entries imported or error message
*)
val of_json : Yojson.Safe.t -> (int, string) result

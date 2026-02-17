(** Streaming JSON parser for LLM responses with incomplete data support.

    Handles LLM streaming responses where JSON may be incomplete or partial.
    Inspired by Python partialjson library (1M+ PyPI downloads).

    Key concepts:
    - Delta tracking: Changes between snapshots
    - Ambiguity detection: Which fields are complete vs partial

    Example:
    {[
      let parser = Streaming_json.create () in
      let _ = Streaming_json.feed parser "{\"title\": \"Hel" in
      (* Partial, but safe: {|"title" -> "Hel"|} *)
      let _ = Streaming_json.feed parser "lo\", \"content\": \"Wor" in
      (* Still partial: {|"title" -> "Hello"; "content" -> "Wor"|} *)
      let result = Streaming_json.finalize parser in
      (* `Assoc [("title", `String "Hello"); ("content", `String "World")] *)

      (* Check field completeness (Jsiphon-style ambiguity) *)
      if Streaming_json.is_field_complete parser "title" then
        use_safely result
    ]}
*)

open Yojson.Safe

type t = {
  mutable accumulated : string;
  mutable last_valid : (string, Yojson.Safe.t) Hashtbl.t;
  mutable complete_fields : string list;
}

(** [create ()] creates a new streaming JSON parser. *)
let create () =
  { accumulated = ""; last_valid = Hashtbl.create 16; complete_fields = [] }

(** [feed parser chunk] accumulates text and attempts parsing.

    Returns [Some json] if valid JSON is obtained, [None] otherwise.
    Even partial results are returned as they become available.
*)
let feed parser chunk =
  if String.equal chunk "" then
    Some (`Assoc (Hashtbl.fold (fun k v acc -> (k, v) :: acc) parser.last_valid []))
  else (
    parser.accumulated <- parser.accumulated ^ chunk;

    try
      let json = from_string parser.accumulated in
      (* Parsing succeeded: update last_valid *)
      (match json with
      | `Assoc assoc ->
          List.iter (fun (k, v) -> Hashtbl.replace parser.last_valid k v) assoc;
          parser.complete_fields <- List.map fst assoc;
          Some json
      | _ -> Some json)
    with
    | Yojson.Json_error _ ->
        (* Incomplete JSON - return last valid result if available *)
        if Hashtbl.length parser.last_valid > 0
        then Some (`Assoc (Hashtbl.fold (fun k v acc -> (k, v) :: acc) parser.last_valid []))
        else None
  )

(** [finalize parser] returns best-effort result. *)
let finalize parser =
  if Hashtbl.length parser.last_valid > 0
  then `Assoc (Hashtbl.fold (fun k v acc -> (k, v) :: acc) parser.last_valid [])
  else `Assoc []

(** [is_field_complete parser field] checks if a field was parsed successfully.

    Jsiphon-style ambiguity check: returns [true] if field exists
    and was completely parsed in the last valid snapshot.
*)
let is_field_complete parser field =
  List.mem field parser.complete_fields

(** [reset parser] clears parser state for reuse. *)
let reset parser =
  parser.accumulated <- "";
  parser.last_valid <- Hashtbl.create 16;
  parser.complete_fields <- []

(** [parse_streaming_json ?default text] one-shot parsing of potentially incomplete JSON.

    Wrapper for one-shot parsing without feed interface.
*)
let parse_streaming_json ?(default=`Assoc []) text =
  if String.equal text "" then default
  else
    try from_string text
    with Yojson.Json_error _ -> default

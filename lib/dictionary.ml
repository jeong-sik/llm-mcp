(** Dictionary.ml - Trained Dictionary Compression for LLM Responses

    Trained zstd dictionaries for maximum compression
    of LLM-specific content (JSON, code, markdown, tool calls).

    Benefits:
    - 70-85% compression for small payloads (<1KB) vs 40-50% without
    - Domain-specific patterns: tool_call, content_type, json keys
    - ~10x faster than retrain per session

    Usage:
    {[
      (* Load pre-trained dictionary *)
      let dict = Dictionary.load_default () in

      (* Compress with dictionary *)
      let compressed = Dictionary.compress_with_dict dict data in

      (* Decompress with dictionary *)
      let original = Dictionary.decompress_with_dict dict compressed in
    ]}

    @author Second Brain
    @since Phase 5 (standardized compression)
*)

(** {1 Types} *)

(** Content type for dictionary selection *)
type content_type =
  | Code        (** Python, TypeScript, OCaml, etc. *)
  | JSON        (** API responses, tool calls *)
  | Markdown    (** Documentation, prose *)
  | Mixed       (** General LLM output *)

(** Model type for cross-model optimization
    Based on cross_model_validation.json findings:
    - Same-model: 88-95% compression
    - Cross-model: 36.9% avg degradation
    - Universal: trained on all models, ~65-75% compression (balanced)
*)
type model_type =
  | Claude      (** Anthropic Claude responses *)
  | GPT         (** OpenAI GPT responses *)
  | Gemini      (** Google Gemini responses *)
  | Llama       (** Meta Llama/local LLM responses *)
  | Universal   (** Mixed training from all models - recommended for multi-agent *)

(** Trained dictionary *)
type t = {
  dict_id: string;
  content_type: content_type;
  model_type: model_type;  (** NEW: Model-specific optimization *)
  dict_data: string;  (** Raw dictionary bytes *)
  sample_count: int;
  created_at: float;
}

(** {1 Constants} *)

let magic = "ZDCT"  (** Dictionary-compressed magic header *)
let version = 1
let default_dict_size = 110 * 1024  (** 110KB - zstd recommended *)
let min_samples = 100  (** Minimum samples for training *)
let min_payload_size = 64  (** Minimum size for dictionary compression *)

(** {1 Helper functions for safe file I/O} *)

(** Read binary file with proper cleanup on exception *)
let read_binary_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () ->
      try close_in ic with
      | ex ->
          Log.warn "dictionary" "close_in failed in finalizer: %s"
            (Printexc.to_string ex))
    (fun () ->
    let len = in_channel_length ic in
    really_input_string ic len
    )

(** Write binary file with proper cleanup on exception *)
let write_binary_file path content =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () ->
      try close_out oc with
      | ex ->
          Log.warn "dictionary" "close_out failed in finalizer: %s"
            (Printexc.to_string ex))
    (fun () ->
    output_string oc content
    )

(** {1 Content Type Detection} *)

let contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let detect_content_type (data : string) : content_type =
  let len = String.length data in
  if len < 10 then Mixed
  else
    let start = String.sub data 0 (min 100 len) in
    if String.contains start '{' || String.contains start '[' then JSON
    else if String.contains start '#' && String.contains start '\n' then Markdown
    else if contains_substring start "def " || contains_substring start "function "
         || contains_substring start "let " || contains_substring start "class " then Code
    else Mixed

let string_of_content_type = function
  | Code -> "code"
  | JSON -> "json"
  | Markdown -> "markdown"
  | Mixed -> "mixed"

let content_type_of_string = function
  | "code" -> Code
  | "json" -> JSON
  | "markdown" -> Markdown
  | _ -> Mixed

(** {2 Model Type Detection} *)

let string_of_model_type = function
  | Claude -> "claude"
  | GPT -> "gpt"
  | Gemini -> "gemini"
  | Llama -> "llama"
  | Universal -> "universal"

let model_type_of_string = function
  | "claude" -> Claude
  | "gpt" | "openai" -> GPT
  | "gemini" | "google" -> Gemini
  | "llama" | "meta" | "local" -> Llama
  | "universal" | "mixed" | _ -> Universal

(** Detect model type from response patterns.
    Based on observed response characteristics:
    - Claude: "I'll help", "Let me", structured thinking
    - GPT: "Certainly!", "I'd be happy", numbered lists
    - Gemini: "Here's", concise, technical focus
    - Llama: Variable, often mirrors Claude/GPT patterns
*)
let detect_model_type (data : string) : model_type =
  let len = String.length data in
  if len < 20 then Universal
  else
    let sample = String.sub data 0 (min 200 len) |> String.lowercase_ascii in
    (* Claude patterns *)
    if contains_substring sample "i'll help" ||
       contains_substring sample "let me " ||
       contains_substring sample "<thinking>" then Claude
    (* GPT patterns *)
    else if contains_substring sample "certainly!" ||
            contains_substring sample "i'd be happy" ||
            contains_substring sample "here's what" then GPT
    (* Gemini patterns *)
    else if contains_substring sample "here's " ||
            contains_substring sample "the answer is" then Gemini
    (* Default to Universal for multi-agent compatibility *)
    else Universal

(** {1 Dictionary I/O} *)

(** Load dictionary from file.
    Supports both v1 (legacy) and v2 (with model_type) formats.
    - v1: "ZDCT|v1|json|100|1736697600.0"
    - v2: "ZDCT|v2|json|claude|100|1736697600.0"
    Uses with_open_* for proper resource cleanup.
*)
let load (path : string) : (t, string) result =
  try
    let data = read_binary_file path in
    let len = String.length data in
    (* Parse metadata from first line *)
    let idx = String.index data '\n' in
    let meta = String.sub data 0 idx in
    let dict_data = String.sub data (idx + 1) (len - idx - 1) in
    let parts = String.split_on_char '|' meta in
    match parts with
    (* v2 format with model_type *)
    | [m; v; ct; mt; sc; ca] when m = magic && v = "v2" ->
        Ok {
          dict_id = Filename.basename path;
          content_type = content_type_of_string ct;
          model_type = model_type_of_string mt;
          dict_data;
          sample_count = int_of_string sc;
          created_at = float_of_string ca;
        }
    (* v1 legacy format - default to Universal model *)
    | [m; v; ct; sc; ca] when m = magic && v = "v1" ->
        Ok {
          dict_id = Filename.basename path;
          content_type = content_type_of_string ct;
          model_type = Universal;  (* Legacy dicts work for all models *)
          dict_data;
          sample_count = int_of_string sc;
          created_at = float_of_string ca;
        }
    | _ -> Error ("Invalid dictionary format: " ^ path)
  with
  | Sys_error e -> Error e
  | _ -> Error ("Failed to load dictionary: " ^ path)

(** Save dictionary to file (v2 format with model_type).
    Uses Fun.protect for proper resource cleanup.
*)
let save (dict : t) (path : string) : (unit, string) result =
  try
    let oc = open_out_bin path in
    Fun.protect
      ~finally:(fun () ->
        try close_out oc with
        | ex ->
            Log.warn "dictionary" "close_out failed in finalizer: %s"
              (Printexc.to_string ex))
      (fun () ->
      (* v2 format: ZDCT|v2|content_type|model_type|sample_count|created_at *)
      Printf.fprintf oc "%s|v2|%s|%s|%d|%.1f\n"
        magic
        (string_of_content_type dict.content_type)
        (string_of_model_type dict.model_type)
        dict.sample_count
        dict.created_at;
      output_string oc dict.dict_data
      );
    Ok ()
  with Sys_error e -> Error e

(** {1 Dictionary Training} *)

(** Train dictionary from samples.
    Requires zstd CLI for training (OCaml bindings don't expose dict training).
    @param samples List of sample strings
    @param content_type Type of content for labeling
    @param model_type Model type (default: Universal for multi-agent compatibility)
    @return Trained dictionary or error
*)
let train ~(samples : string list) ~(content_type : content_type) ?(model_type = Universal) () : (t, string) result =
  let sample_count = List.length samples in
  if sample_count < min_samples then
    Error (Printf.sprintf "Need at least %d samples, got %d" min_samples sample_count)
  else
    (* Write samples to temp files *)
    let temp_dir = Filename.concat (Filename.get_temp_dir_name ()) "zdict_train" in
    (try Unix.mkdir temp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let sample_files = List.mapi (fun i s ->
      let path = Filename.concat temp_dir (Printf.sprintf "sample_%d.txt" i) in
      write_binary_file path s;
      path
    ) samples in
    (* Train with zstd CLI *)
    let dict_path = Filename.concat temp_dir "trained.dict" in
    let cmd = Printf.sprintf "zstd --train -o %s --maxdict=%d %s/*.txt 2>/dev/null"
      dict_path default_dict_size temp_dir in
    let exit_code = Sys.command cmd in
    (* Clean up sample files *)
    List.iter (fun f -> try Sys.remove f with _ -> ()) sample_files;
    if exit_code <> 0 then
      Error "zstd training failed (ensure zstd CLI is installed)"
    else
      try
        let dict_data = read_binary_file dict_path in
        Sys.remove dict_path;
        (try Unix.rmdir temp_dir with _ -> ());
        Ok {
          dict_id = Printf.sprintf "dict_%s_%s_%d"
            (string_of_content_type content_type)
            (string_of_model_type model_type)
            sample_count;
          content_type;
          model_type;
          dict_data;
          sample_count;
          created_at = Unix.gettimeofday ();
        }
      with e ->
        Error (Printf.sprintf "Failed to read trained dict: %s" (Printexc.to_string e))

(** Train Universal dictionary from multiple model samples.
    Recommended for multi-agent scenarios (MASC).
    Expected compression: 65-75% (balanced across models)
*)
let train_universal ~(claude_samples : string list) ~(gpt_samples : string list)
    ~(gemini_samples : string list) ~(llama_samples : string list)
    ~(content_type : content_type) : (t, string) result =
  (* Mix samples from all models *)
  let all_samples = claude_samples @ gpt_samples @ gemini_samples @ llama_samples in
  train ~samples:all_samples ~content_type ~model_type:Universal ()

(** {1 Dictionary Compression/Decompression} *)

(** Compress data using trained dictionary.
    Format: ZDCT (4) + orig_size (4 BE) + dict_id_len (1) + dict_id + compressed
*)
let compress_with_dict (dict : t) ?(level = 3) (data : string) : string =
  let orig_size = String.length data in
  if orig_size < min_payload_size then data
  else
    try
      (* Use zstd with dictionary via temp file (OCaml bindings limited) *)
      let temp_in = Filename.temp_file "zdict_in" ".bin" in
      let temp_out = Filename.temp_file "zdict_out" ".zst" in
      let temp_dict = Filename.temp_file "zdict" ".dict" in
      (* Write data and dict *)
      write_binary_file temp_in data;
      write_binary_file temp_dict dict.dict_data;
      (* Compress with dict *)
      let cmd = Printf.sprintf "zstd -%d -D %s -o %s %s 2>/dev/null"
        level temp_dict temp_out temp_in in
      let _ = Sys.command cmd in
      (* Read result *)
      let compressed = read_binary_file temp_out in
      (* Clean up *)
      Sys.remove temp_in;
      Sys.remove temp_out;
      Sys.remove temp_dict;
      (* Build header: ZDCT + orig_size (4 BE) + dict_id_len (1) + dict_id *)
      if String.length compressed >= orig_size then data
      else
        let dict_id_len = min 255 (String.length dict.dict_id) in
        let header = Bytes.create (4 + 4 + 1 + dict_id_len) in
        Bytes.blit_string magic 0 header 0 4;
        Bytes.set header 4 (Char.chr ((orig_size lsr 24) land 0xFF));
        Bytes.set header 5 (Char.chr ((orig_size lsr 16) land 0xFF));
        Bytes.set header 6 (Char.chr ((orig_size lsr 8) land 0xFF));
        Bytes.set header 7 (Char.chr (orig_size land 0xFF));
        Bytes.set header 8 (Char.chr dict_id_len);
        Bytes.blit_string dict.dict_id 0 header 9 dict_id_len;
        Bytes.to_string header ^ compressed
    with _ -> data

(** Decompress dictionary-compressed data *)
let decompress_with_dict (dict : t) (data : string) : (string, string) result =
  let len = String.length data in
  if len < 9 || String.sub data 0 4 <> magic then
    Error "Not dictionary-compressed data"
  else
    try
      (* Parse header *)
      let orig_size =
        (Char.code data.[4] lsl 24) lor
        (Char.code data.[5] lsl 16) lor
        (Char.code data.[6] lsl 8) lor
        Char.code data.[7]
      in
      let dict_id_len = Char.code data.[8] in
      let header_size = 9 + dict_id_len in
      if len < header_size then Error "Truncated header"
      else
        let compressed = String.sub data header_size (len - header_size) in
        (* Decompress via temp file *)
        let temp_in = Filename.temp_file "zdict_in" ".zst" in
        let temp_out = Filename.temp_file "zdict_out" ".bin" in
        let temp_dict = Filename.temp_file "zdict" ".dict" in
        write_binary_file temp_in compressed;
        write_binary_file temp_dict dict.dict_data;
        let cmd = Printf.sprintf "zstd -d -D %s -o %s %s 2>/dev/null"
          temp_dict temp_out temp_in in
        let exit_code = Sys.command cmd in
        if exit_code <> 0 then begin
          Sys.remove temp_in;
          Sys.remove temp_dict;
          (try Sys.remove temp_out with _ -> ());
          Error "Decompression failed"
        end else begin
          let result = read_binary_file temp_out in
          let result_len = String.length result in
          Sys.remove temp_in;
          Sys.remove temp_out;
          Sys.remove temp_dict;
          if result_len <> orig_size then
            Error (Printf.sprintf "Size mismatch: expected %d, got %d" orig_size result_len)
          else
            Ok result
        end
    with e -> Error (Printexc.to_string e)

(** {1 Built-in Dictionaries} *)

(** Directory for pre-trained dictionaries *)
let dict_dir () : string =
  let default_me_root () =
    let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
    Filename.concat home "me"
  in
  let find_repo_root me_root =
    match Sys.getenv_opt "LLM_MCP_REPO_ROOT" with
    | Some path -> Some path
    | None ->
        let candidates =
          [Filename.concat me_root "llm-mcp"; Filename.concat me_root "workspace/llm-mcp"]
        in
        let rec find_in_workspace workspace entries =
          match entries with
          | [] -> None
          | dir :: rest ->
              let candidate = Filename.concat (Filename.concat workspace dir) "llm-mcp" in
              if Sys.file_exists candidate then Some candidate
              else find_in_workspace workspace rest
        in
        (match List.find_opt Sys.file_exists candidates with
        | Some path -> Some path
        | None ->
            let workspace = Filename.concat me_root "workspace" in
            (match Sys.readdir workspace with
            | entries -> find_in_workspace workspace (Array.to_list entries)
            | exception _ -> None))
  in
  match Sys.getenv_opt "LLM_MCP_DICTS_DIR" with
  | Some dir -> dir
  | None ->
      let me_root = Sys.getenv_opt "ME_ROOT" |> Option.value ~default:(default_me_root ()) in
      (match find_repo_root me_root with
      | Some root -> Filename.concat root "data/dicts"
      | None -> Filename.concat (Sys.getcwd ()) "data/dicts")

(** Load pre-trained dictionary for content type *)
let load_for_type (content_type : content_type) : t option =
  let name = string_of_content_type content_type in
  let path = Filename.concat (dict_dir ()) (name ^ ".zdict") in
  match load path with
  | Ok d -> Some d
  | Error _ -> None

(** Load default (mixed) dictionary *)
let load_default () : t option =
  load_for_type Mixed

(** Check if data is dictionary-compressed *)
let is_dict_compressed (data : string) : bool =
  String.length data >= 4 && String.sub data 0 4 = magic

(** {1 Auto Compression} *)

(** Compress with auto-detected dictionary *)
let compress_auto ?(level = 3) (data : string) : string =
  let content_type = detect_content_type data in
  match load_for_type content_type with
  | Some dict -> compress_with_dict dict ~level data
  | None ->
      (* Fall back to generic zstd *)
      let len = String.length data in
      if len < min_payload_size then data
      else
        try
          let compressed = Zstd.compress ~level data in
          if String.length compressed < len then compressed else data
        with _ -> data

(** Decompress with auto-detected dictionary *)
let decompress_auto (data : string) : string =
  if is_dict_compressed data then
    (* Extract dict_id from header and load *)
    let dict_id_len = Char.code data.[8] in
    let dict_id = String.sub data 9 dict_id_len in
    (* Try to find matching dictionary *)
    let content_type =
      if contains_substring dict_id "json" then JSON
      else if contains_substring dict_id "code" then Code
      else if contains_substring dict_id "markdown" then Markdown
      else Mixed
    in
    match load_for_type content_type with
    | Some dict ->
        (match decompress_with_dict dict data with
         | Ok d -> d
         | Error _ -> data)
    | None -> data
  else
    (* Not dictionary-compressed, return as-is.
       Note: For regular zstd-compressed data, caller should use Zstd.decompress
       with known orig_size. This function is specifically for dictionary
       compression with embedded headers. *)
    data

(** {1 Debug} *)

let pp fmt (dict : t) =
  Format.fprintf fmt "Dictionary{id=%s, type=%s, samples=%d, size=%d}"
    dict.dict_id
    (string_of_content_type dict.content_type)
    dict.sample_count
    (String.length dict.dict_data)

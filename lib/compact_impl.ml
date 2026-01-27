(** Compact Protocol Implementation - Encoding/Decoding Functions

    Extracted from types.ml for modularity.
    Contains: Caching, MessagePack encoding, Base85 codec, format_tool_result.

    Type definitions remain in Types module.
*)

open Types

(** {1 Cache Configuration - Memory Leak Prevention} *)

(** Maximum entries before LRU eviction *)
let max_tool_cache_size = 256
let max_system_prompt_cache_size = 64

(** In-memory tool cache *)
let tool_cache : (string, tool_cache_entry) Hashtbl.t = Hashtbl.create 64

(** In-memory system prompt cache with timestamp for LRU *)
type prompt_cache_entry = { prompt: string; added_at: float }
let system_prompt_cache : (string, prompt_cache_entry) Hashtbl.t = Hashtbl.create 16

(** Evict oldest entries when cache exceeds max size (LRU) *)
let evict_oldest_tool_cache () =
  if Hashtbl.length tool_cache > max_tool_cache_size then begin
    let entries = Hashtbl.fold (fun k v acc -> (k, v.cached_at) :: acc) tool_cache [] in
    let sorted = List.sort (fun (_, t1) (_, t2) -> compare t1 t2) entries in
    (* Remove oldest 25% *)
    let to_remove = max 1 (List.length sorted / 4) in
    List.iteri (fun i (k, _) -> if i < to_remove then Hashtbl.remove tool_cache k) sorted
  end

let evict_oldest_prompt_cache () =
  if Hashtbl.length system_prompt_cache > max_system_prompt_cache_size then begin
    let entries = Hashtbl.fold (fun k v acc -> (k, v.added_at) :: acc) system_prompt_cache [] in
    let sorted = List.sort (fun (_, t1) (_, t2) -> compare t1 t2) entries in
    let to_remove = max 1 (List.length sorted / 4) in
    List.iteri (fun i (k, _) -> if i < to_remove then Hashtbl.remove system_prompt_cache k) sorted
  end

(** Generate short hash for caching (MD5, first 8 chars) *)
let short_hash s =
  let digest = Digest.string s in
  let hex = Digest.to_hex digest in
  String.sub hex 0 8  (* First 8 chars *)

(** Register tool definition, return cache reference *)
let cache_tool_def ~name ~schema : string =
  evict_oldest_tool_cache ();  (* Memory leak prevention *)
  let hash = short_hash (name ^ schema) in
  let id = "t_" ^ hash in
  if not (Hashtbl.mem tool_cache id) then begin
    Hashtbl.add tool_cache id {
      tool_id = id;
      tool_name = name;
      schema_hash = short_hash schema;
      cached_at = Unix.gettimeofday ();
    }
  end;
  id

(** Lookup cached tool *)
let lookup_tool id : tool_cache_entry option =
  Hashtbl.find_opt tool_cache id

(** Register system prompt, return hash reference *)
let cache_system_prompt prompt : string =
  evict_oldest_prompt_cache ();  (* Memory leak prevention *)
  let hash = "s_" ^ short_hash prompt in
  if not (Hashtbl.mem system_prompt_cache hash) then
    Hashtbl.add system_prompt_cache hash { prompt; added_at = Unix.gettimeofday () };
  hash

(** Lookup cached system prompt *)
let lookup_system_prompt hash : string option =
  match Hashtbl.find_opt system_prompt_cache hash with
  | Some entry -> Some entry.prompt
  | None -> None

(** Clear all caches (for testing or memory pressure) *)
let clear_caches () =
  Hashtbl.clear tool_cache;
  Hashtbl.clear system_prompt_cache

(** Get cache statistics *)
let cache_stats () =
  Printf.sprintf "tool_cache: %d/%d, prompt_cache: %d/%d"
    (Hashtbl.length tool_cache) max_tool_cache_size
    (Hashtbl.length system_prompt_cache) max_system_prompt_cache_size

(** Encode delta operation to string *)
let rec encode_delta = function
  | Full s -> "D|F|" ^ s
  | Append s -> "D|+|" ^ s
  | Replace (pos, s) -> Printf.sprintf "D|R|%d|%s" pos s
  | Compressed op ->
      let inner = encode_delta op in
      (match Grpc_core.Codec.Gzip.encoder ~level:4 inner with
       | Ok compressed -> "D|Z|" ^ Base64.encode_string compressed
       | Error _ -> encode_delta op)  (* Fallback to uncompressed *)

(** Decode delta operation from string *)
and decode_delta s =
  if String.length s < 4 then None
  else if String.sub s 0 4 = "D|F|" then
    Some (Full (String.sub s 4 (String.length s - 4)))
  else if String.sub s 0 4 = "D|+|" then
    Some (Append (String.sub s 4 (String.length s - 4)))
  else if String.sub s 0 4 = "D|R|" then begin
    let rest = String.sub s 4 (String.length s - 4) in
    match String.index_opt rest '|' with
    | Some idx ->
        let pos = int_of_string (String.sub rest 0 idx) in
        let content = String.sub rest (idx + 1) (String.length rest - idx - 1) in
        Some (Replace (pos, content))
    | None -> None
  end
  else if String.sub s 0 4 = "D|Z|" then begin
    let rest = String.sub s 4 (String.length s - 4) in
    let decoded = Base64.decode_exn rest in
    match Grpc_core.Codec.Gzip.decoder decoded with
    | Ok decompressed -> decode_delta decompressed
    | Error _ -> None
  end
  else None

(** Apply delta to content *)
let rec apply_delta content = function
  | Full s -> s
  | Append s -> content ^ s
  | Replace (pos, s) ->
      if pos >= String.length content then content ^ s
      else String.sub content 0 pos ^ s
  | Compressed op -> apply_delta content op

(** Find longest common prefix length between two strings *)
let common_prefix_length s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let min_len = min len1 len2 in
  let rec find_diff i =
    if i >= min_len then i
    else if s1.[i] = s2.[i] then find_diff (i + 1)
    else i
  in
  find_diff 0

(** Compute diff between old and new content, return optimal delta.

    Strategy: Find longest common prefix, then choose the most compact encoding:
    - If new = old: no change needed (return Full "" or caller handles)
    - If new = old + suffix: use Append
    - If old is prefix of new with modifications: use Replace from diff point
    - Otherwise: use Full replacement

    Always compares encoding sizes and returns the smallest option. *)
let compute_delta ~old_content ~new_content : delta_op =
  if old_content = new_content then
    (* No change - Full empty is still more compact than encoding position *)
    Full new_content
  else if old_content = "" then
    Full new_content
  else
    let old_len = String.length old_content in
    let new_len = String.length new_content in
    let prefix_len = common_prefix_length old_content new_content in

    (* Check if it's a pure append case *)
    if prefix_len = old_len && new_len > old_len then
      (* New content is old + appended *)
      let suffix = String.sub new_content old_len (new_len - old_len) in
      Append suffix
    else if prefix_len > 0 then
      (* Some common prefix exists - use Replace from the diff point *)
      let replacement = String.sub new_content prefix_len (new_len - prefix_len) in
      let replace_op = Replace (prefix_len, replacement) in
      (* Compare encoding sizes: Full vs Replace *)
      let full_size = 2 + new_len in  (* "F|" + content *)
      let replace_size = 2 + String.length (string_of_int prefix_len) + 1 + String.length replacement in  (* "R|pos|" + content *)
      if replace_size < full_size then replace_op else Full new_content
    else
      (* No common prefix - full replacement *)
      Full new_content

(** Encode compact_request to MessagePack binary *)
let encode_compact_request (r : compact_request) : string =
  let open Msgpck in
  let deltas_encoded = List.map (fun d -> of_string (encode_delta d)) r.context_deltas in
  let msg = of_list [
    of_int r.req_version;
    (match r.tool_ref with Some s -> of_string s | None -> Nil);
    (match r.tool_def with Some s -> of_string s | None -> Nil);
    of_string r.args;
    of_list deltas_encoded;
    (match r.system_ref with Some s -> of_string s | None -> Nil);
    (match r.system_prompt with Some s -> of_string s | None -> Nil);
  ] in
  let buf = StringBuf.to_string msg in
  Buffer.contents buf

(** Decode MessagePack binary to compact_request *)
let decode_compact_request (data : string) : compact_request option =
  try
    let open Msgpck in
    let (_bytes_read, msg) = StringBuf.read ~pos:0 data in
    match to_list msg with
    | [ver; tool_ref; tool_def; args; deltas; sys_ref; sys_prompt] ->
        let to_string_opt = function Nil -> None | v -> Some (to_string v) in
        let decode_deltas lst =
          List.filter_map (fun v -> decode_delta (to_string v)) (to_list lst)
        in
        Some {
          req_version = to_int ver;
          tool_ref = to_string_opt tool_ref;
          tool_def = to_string_opt tool_def;
          args = to_string args;
          context_deltas = decode_deltas deltas;
          system_ref = to_string_opt sys_ref;
          system_prompt = to_string_opt sys_prompt;
        }
    | _ -> None
  with _ -> None

(** Encode response to compact DSL: "RES|OK|G3|150|result" *)
let encode_compact_response r =
  Printf.sprintf "RES|%s|%s|%d|%s"
    (string_of_status_code r.status)
    (string_of_model_code r.model)
    r.tokens
    r.result

(** Decode compact DSL to response *)
let decode_compact_response s =
  match String.split_on_char '|' s with
  | "RES" :: status :: model :: tokens :: rest ->
      Some {
        version = 1;
        status = status_code_of_string status;
        model = model_code_of_string model;
        tokens = Safe_parse.int ~context:"compact:tokens" ~default:0 tokens;
        result = String.concat "|" rest;  (* Result may contain | *)
      }
  | _ -> None

(** Default MessagePack encoding version.
    v1 = string status/model (legacy, most compatible)
    v2 = int status/model (smaller)
    v3 = int + optional tokens omission (smallest) *)
let default_msgpack_version = ref 3

(** Convert tool_result to compact_response *)
let tool_result_to_compact ?(version = !default_msgpack_version) (r : tool_result) : compact_response =
  let tokens =
    match List.assoc_opt "tokens" r.extra with
    | Some t -> Safe_parse.int ~context:"tool_result:tokens" ~default:0 t
    | None -> 0
  in
  {
    version;
    status = if r.returncode = 0 then OK else ERR;
    model = model_code_of_string r.model;
    tokens;
    result = r.response;
  }

(** Encode compact_response to MessagePack binary.
    v1: [1, "OK", "G3", tokens, result] - string encoding
    v2: [2, 0, 0, tokens, result] - int encoding
    v3: [3, 0, 0, result] or [3, 0, 0, tokens, result] - int, optional tokens *)
let encode_msgpack_response (r : compact_response) : string =
  let open Msgpck in
  let msg = match r.version with
    | 1 ->
        (* v1: String encoding for status/model *)
        of_list [
          of_int 1;
          of_string (string_of_status_code r.status);
          of_string (string_of_model_code r.model);
          of_int r.tokens;
          of_string r.result;
        ]
    | 2 ->
        (* v2: Int encoding for status/model *)
        of_list [
          of_int 2;
          of_int (int_of_status_code r.status);
          of_int (int_of_model_code r.model);
          of_int r.tokens;
          of_string r.result;
        ]
    | 3 ->
        (* v3: Int encoding, omit tokens if zero *)
        if r.tokens = 0 then
          of_list [
            of_int 3;
            of_int (int_of_status_code r.status);
            of_int (int_of_model_code r.model);
            of_string r.result;
          ]
        else
          of_list [
            of_int 3;
            of_int (int_of_status_code r.status);
            of_int (int_of_model_code r.model);
            of_int r.tokens;
            of_string r.result;
          ]
    | _ ->
        (* Default to v1 for unknown versions *)
        of_list [
          of_int 1;
          of_string (string_of_status_code r.status);
          of_string (string_of_model_code r.model);
          of_int r.tokens;
          of_string r.result;
        ]
  in
  let buf = StringBuf.to_string msg in
  Buffer.contents buf

(** Decode MessagePack binary to compact_response.
    Auto-detects version from first element. *)
let decode_msgpack_response (data : string) : compact_response option =
  try
    let open Msgpck in
    let (_bytes_read, msg) = StringBuf.read ~pos:0 data in
    match to_list msg with
    (* v1: [1, "OK", "G3", tokens, result] *)
    | [ver; status; model; tokens; result] when to_int ver = 1 ->
        Some {
          version = 1;
          status = status_code_of_string (to_string status);
          model = model_code_of_string (to_string model);
          tokens = to_int tokens;
          result = to_string result;
        }
    (* v2: [2, status_int, model_int, tokens, result] *)
    | [ver; status; model; tokens; result] when to_int ver = 2 ->
        Some {
          version = 2;
          status = status_code_of_int (to_int status);
          model = model_code_of_int (to_int model);
          tokens = to_int tokens;
          result = to_string result;
        }
    (* v3: [3, status_int, model_int, result] - tokens omitted (=0) *)
    | [ver; status; model; result] when to_int ver = 3 ->
        Some {
          version = 3;
          status = status_code_of_int (to_int status);
          model = model_code_of_int (to_int model);
          tokens = 0;
          result = to_string result;
        }
    (* v3: [3, status_int, model_int, tokens, result] - tokens present *)
    | [ver; status; model; tokens; result] when to_int ver = 3 ->
        Some {
          version = 3;
          status = status_code_of_int (to_int status);
          model = model_code_of_int (to_int model);
          tokens = to_int tokens;
          result = to_string result;
        }
    | _ -> None
  with _ -> None

(** Base85 (ASCII85) encoding - 25% overhead vs Base64's 33%
    Z85 alphabet: 0-9A-Za-z.-:+=^!/*?&<>()[]{}@%$#
    Each 4 bytes → 5 characters *)
let base85_alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-:+=^!/*?&<>()[]{}@%$#"

let encode_base85 (data : string) : string =
  let len = String.length data in
  let buf = Buffer.create ((len * 5 / 4) + 5) in
  let i = ref 0 in
  while !i + 4 <= len do
    let b0 = Char.code data.[!i] in
    let b1 = Char.code data.[!i + 1] in
    let b2 = Char.code data.[!i + 2] in
    let b3 = Char.code data.[!i + 3] in
    let value = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
    (* Convert to base 85 *)
    let c4 = value mod 85 in
    let v1 = value / 85 in
    let c3 = v1 mod 85 in
    let v2 = v1 / 85 in
    let c2 = v2 mod 85 in
    let v3 = v2 / 85 in
    let c1 = v3 mod 85 in
    let c0 = v3 / 85 in
    Buffer.add_char buf base85_alphabet.[c0];
    Buffer.add_char buf base85_alphabet.[c1];
    Buffer.add_char buf base85_alphabet.[c2];
    Buffer.add_char buf base85_alphabet.[c3];
    Buffer.add_char buf base85_alphabet.[c4];
    i := !i + 4
  done;
  (* Handle remaining bytes (1-3) *)
  let remaining = len - !i in
  if remaining > 0 then begin
    let padded = Bytes.make 4 '\000' in
    for j = 0 to remaining - 1 do
      Bytes.set padded j data.[!i + j]
    done;
    let b0 = Char.code (Bytes.get padded 0) in
    let b1 = Char.code (Bytes.get padded 1) in
    let b2 = Char.code (Bytes.get padded 2) in
    let b3 = Char.code (Bytes.get padded 3) in
    let value = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
    let c4 = value mod 85 in
    let v1 = value / 85 in
    let c3 = v1 mod 85 in
    let v2 = v1 / 85 in
    let c2 = v2 mod 85 in
    let v3 = v2 / 85 in
    let c1 = v3 mod 85 in
    let c0 = v3 / 85 in
    (* Only output chars needed for remaining bytes + 1 *)
    let out_chars = remaining + 1 in
    if out_chars >= 1 then Buffer.add_char buf base85_alphabet.[c0];
    if out_chars >= 2 then Buffer.add_char buf base85_alphabet.[c1];
    if out_chars >= 3 then Buffer.add_char buf base85_alphabet.[c2];
    if out_chars >= 4 then Buffer.add_char buf base85_alphabet.[c3];
    if out_chars >= 5 then Buffer.add_char buf base85_alphabet.[c4]
  end;
  Buffer.contents buf

(** Decode Base85 encoded string back to binary data.
    v1.3: Returns structured decode_error for better error handling. *)
let decode_base85 (data : string) : (string, decode_error) result =
  if String.length data = 0 then Ok ""
  else begin
    (* Build reverse lookup table *)
    let reverse_table = Array.make 256 (-1) in
    String.iteri (fun i c -> reverse_table.(Char.code c) <- i) base85_alphabet;

    let len = String.length data in

    (* Check for invalid length early *)
    if len = 1 then Error (InvalidBase85Length len)
    else begin
      let buf = Buffer.create ((len * 4 / 5) + 4) in
      let error = ref None in
      let i = ref 0 in

      (* Helper to find first invalid char in a range *)
      let find_invalid_char start count =
        let rec check j =
          if j >= count then None
          else
            let pos = start + j in
            let c = data.[pos] in
            if reverse_table.(Char.code c) < 0 then Some (c, pos)
            else check (j + 1)
        in check 0
      in

      (* Decode full 5-char blocks *)
      while !error = None && !i + 5 <= len do
        match find_invalid_char !i 5 with
        | Some (c, pos) -> error := Some (InvalidBase85Char (c, pos))
        | None ->
            let c0 = reverse_table.(Char.code data.[!i]) in
            let c1 = reverse_table.(Char.code data.[!i + 1]) in
            let c2 = reverse_table.(Char.code data.[!i + 2]) in
            let c3 = reverse_table.(Char.code data.[!i + 3]) in
            let c4 = reverse_table.(Char.code data.[!i + 4]) in
            let value = c0 * 52200625 + c1 * 614125 + c2 * 7225 + c3 * 85 + c4 in
            Buffer.add_char buf (Char.chr ((value lsr 24) land 0xFF));
            Buffer.add_char buf (Char.chr ((value lsr 16) land 0xFF));
            Buffer.add_char buf (Char.chr ((value lsr 8) land 0xFF));
            Buffer.add_char buf (Char.chr (value land 0xFF));
            i := !i + 5
      done;

      match !error with
      | Some e -> Error e
      | None ->
          (* Handle remaining chars (2-4) → (1-3) bytes *)
          let remaining = len - !i in
          if remaining > 0 then begin
            match find_invalid_char !i remaining with
            | Some (c, pos) -> Error (InvalidBase85Char (c, pos))
            | None ->
                let padded = Array.make 5 84 in
                for j = 0 to remaining - 1 do
                  padded.(j) <- reverse_table.(Char.code data.[!i + j])
                done;
                let value = padded.(0) * 52200625 + padded.(1) * 614125 +
                            padded.(2) * 7225 + padded.(3) * 85 + padded.(4) in
                let out_bytes = remaining - 1 in
                if out_bytes >= 1 then Buffer.add_char buf (Char.chr ((value lsr 24) land 0xFF));
                if out_bytes >= 2 then Buffer.add_char buf (Char.chr ((value lsr 16) land 0xFF));
                if out_bytes >= 3 then Buffer.add_char buf (Char.chr ((value lsr 8) land 0xFF));
                Ok (Buffer.contents buf)
          end
          else Ok (Buffer.contents buf)
    end
  end

(** Convenience wrapper that converts decode_error to string.
    For backward compatibility with existing code. *)
let decode_base85_string (data : string) : (string, string) result =
  match decode_base85 data with
  | Ok s -> Ok s
  | Error e -> Error (string_of_decode_error e)

(** Format tool_result based on response_format.
    v1.3: Adaptive format selection based on response size. *)
let format_tool_result ~(format : response_format) (r : tool_result) : string =
  let verbose_fallback () = Yojson.Safe.to_string (tool_result_to_yojson r) in
  let compact_response = lazy (tool_result_to_compact r) in
  let msgpack_bytes = lazy (encode_msgpack_response (Lazy.force compact_response)) in
  match format with
  | Verbose -> verbose_fallback ()
  | Compact ->
      Safe_parse.try_or ~context:"encode:compact" ~fallback:verbose_fallback
        (fun () -> encode_compact_response (Lazy.force compact_response))
  | Binary ->
      Safe_parse.try_or ~context:"encode:binary" ~fallback:verbose_fallback
        (fun () -> Printf.sprintf "M%s" (Base64.encode_string (Lazy.force msgpack_bytes)))
  | Base85 ->
      Safe_parse.try_or ~context:"encode:base85" ~fallback:verbose_fallback
        (fun () -> Printf.sprintf "A%s" (encode_base85 (Lazy.force msgpack_bytes)))
  | Compressed ->
      Safe_parse.try_or ~context:"encode:gzip" ~fallback:verbose_fallback (fun () ->
        let msgpack = Lazy.force msgpack_bytes in
        match Grpc_core.Codec.Gzip.encoder ~level:4 msgpack with
        | Ok compressed -> Printf.sprintf "Z%s" (encode_base85 compressed)
        | Error _ -> verbose_fallback ())
  | ZstdDict ->
      Safe_parse.try_or ~context:"encode:zstd_dict" ~fallback:verbose_fallback (fun () ->
        let msgpack = Lazy.force msgpack_bytes in
        match Dictionary.load_default () with
        | Some dict ->
            let compressed = Dictionary.compress_with_dict dict msgpack in
            if String.length compressed < String.length msgpack then
              Printf.sprintf "D%s" (encode_base85 compressed)
            else
              Printf.sprintf "S%s" (encode_base85 (Zstd.compress ~level:3 msgpack))
        | None ->
            Printf.sprintf "S%s" (encode_base85 (Zstd.compress ~level:3 msgpack)))
  | Auto ->
      (* v1.4 Adaptive Format Selection with Zstd (Compact Protocol v4) *)
      let result_len = String.length r.response in
      if result_len < auto_compact_threshold then
        Safe_parse.try_or ~context:"encode:auto_compact" ~fallback:verbose_fallback
          (fun () -> encode_compact_response (Lazy.force compact_response))
      else if result_len < auto_base85_threshold then
        Safe_parse.try_or ~context:"encode:auto_base85" ~fallback:verbose_fallback
          (fun () -> Printf.sprintf "A%s" (encode_base85 (Lazy.force msgpack_bytes)))
      else
        Safe_parse.try_or ~context:"encode:auto_zstd" ~fallback:verbose_fallback (fun () ->
          let msgpack = Lazy.force msgpack_bytes in
          let compressed = Zstd.compress ~level:3 msgpack in
          Printf.sprintf "S%s" (encode_base85 compressed))

(** Decode formatted response back to compact_response.
    Compact Protocol v4: Supports all format prefixes.

    Format prefixes:
    - M: MessagePack + Base64
    - A: MessagePack + Base85
    - Z: Gzip + Base85
    - S: Zstd + Base85 (v4)
    - D: Dictionary Zstd + Base85 (v4)
    - RES|...: DSL format
    - {...}: JSON verbose

    @since v1.4 *)
let decode_formatted_response (data : string) : (compact_response, string) result =
  if String.length data = 0 then
    Error "Empty response"
  else
    let prefix = data.[0] in
    let rest = String.sub data 1 (String.length data - 1) in
    match prefix with
    | 'M' ->
        (* MessagePack + Base64 *)
        (try
          let decoded = Base64.decode_exn rest in
          match decode_msgpack_response decoded with
          | Some r -> Ok r
          | None -> Error "Invalid MessagePack in M format"
        with e -> Error (Printf.sprintf "M decode failed: %s" (Printexc.to_string e)))
    | 'A' ->
        (* MessagePack + Base85 *)
        (match decode_base85 rest with
        | Ok decoded ->
            (match decode_msgpack_response decoded with
            | Some r -> Ok r
            | None -> Error "Invalid MessagePack in A format")
        | Error e -> Error (Printf.sprintf "A decode failed: %s" (string_of_decode_error e)))
    | 'Z' ->
        (* Gzip + Base85 *)
        (match decode_base85 rest with
        | Ok decoded ->
            (match Grpc_core.Codec.Gzip.decoder decoded with
            | Ok decompressed ->
                (match decode_msgpack_response decompressed with
                | Some r -> Ok r
                | None -> Error "Invalid MessagePack in Z format")
            | Error e -> Error (Printf.sprintf "Z decompress failed: %s" e))
        | Error e -> Error (Printf.sprintf "Z decode failed: %s" (string_of_decode_error e)))
    | 'S' ->
        (* Zstd + Base85 (Compact Protocol v4) *)
        (match decode_base85 rest with
        | Ok decoded ->
            (try
              let estimated_size = max 4096 (String.length decoded * 8) in
              let decompressed = Zstd.decompress estimated_size decoded in
              match decode_msgpack_response decompressed with
              | Some r -> Ok r
              | None -> Error "Invalid MessagePack in S format"
            with Zstd.Error e -> Error (Printf.sprintf "S decompress failed: %s" e))
        | Error e -> Error (Printf.sprintf "S decode failed: %s" (string_of_decode_error e)))
    | 'D' ->
        (* Dictionary Zstd + Base85 (Compact Protocol v4) *)
        (match decode_base85 rest with
        | Ok decoded ->
            (match Dictionary.load_default () with
            | Some dict ->
                (match Dictionary.decompress_with_dict dict decoded with
                | Ok decompressed ->
                    (match decode_msgpack_response decompressed with
                    | Some r -> Ok r
                    | None -> Error "Invalid MessagePack in D format")
                | Error e -> Error (Printf.sprintf "D decompress failed: %s" e))
            | None -> Error "D format requires dictionary but none loaded")
        | Error e -> Error (Printf.sprintf "D decode failed: %s" (string_of_decode_error e)))
    | 'R' ->
        (* DSL format: RES|OK|G3|150|result *)
        (match decode_compact_response data with
        | Some r -> Ok r
        | None -> Error "Invalid DSL format")
    | '{' ->
        (* JSON verbose - parse and convert *)
        (try
          let json = Yojson.Safe.from_string data in
          let status_str = Safe_parse.json_string ~context:"decode_compact" ~default:"OK" json "status" in
          let model_str = Safe_parse.json_string ~context:"decode_compact" ~default:"unknown" json "model" in
          Ok {
            version = 1;
            status = (try status_code_of_string status_str with Failure _ -> OK);
            model = (try model_code_of_string model_str with Failure _ -> Unknown "");
            tokens = Safe_parse.json_int ~context:"decode_compact" ~default:0 json "tokens";
            result = Safe_parse.json_string ~context:"decode_compact" ~default:"" json "response";
          }
        with e -> Error (Printf.sprintf "JSON parse failed: %s" (Printexc.to_string e)))
    | _ ->
        Error (Printf.sprintf "Unknown format prefix: %c" prefix)

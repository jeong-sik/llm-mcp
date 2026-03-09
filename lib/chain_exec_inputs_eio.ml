include Chain_utils

type exec_context = Chain_exec_context_eio.exec_context

let resolve_single_input (ctx : exec_context) (ref_str : string) : string =
  let try_extract_json_path raw path_parts =
    let parse_index part =
      if String.length part >= 3 && part.[0] = '[' && part.[String.length part - 1] = ']' then
        try Some (int_of_string (String.sub part 1 (String.length part - 2))) with _ -> None
      else
        try Some (int_of_string part) with _ -> None
    in
    try
      let json = Yojson.Safe.from_string raw in
      let rec walk j = function
        | [] ->
            (match j with
             | `String s -> Ok s
             | `Int i -> Ok (string_of_int i)
             | `Float f -> Ok (string_of_float f)
             | `Bool b -> Ok (string_of_bool b)
             | `Null -> Ok "null"
             | _ -> Ok (Yojson.Safe.to_string j))
        | key :: rest ->
            (match j with
             | `Assoc fields ->
                 (match List.assoc_opt key fields with
                  | Some v -> walk v rest
                  | None -> Error (Printf.sprintf "Key '%s' not found" key))
             | `List items ->
                 (match parse_index key with
                  | Some idx when idx >= 0 && idx < List.length items ->
                      walk (List.nth items idx) rest
                  | _ -> Error "Invalid array index")
             | _ -> Error (Printf.sprintf "Cannot extract '%s' from non-object" key))
      in
      walk json path_parts
    with _ -> Error "JSON parse error"
  in
  let try_extract_bullet_value raw key =
    let prefix = "- " ^ key ^ ":" in
    raw
    |> String.split_on_char '\n'
    |> List.find_map (fun line ->
           let line = String.trim line in
           if starts_with ~prefix line then
             let start = String.length prefix in
             let len = String.length line - start in
             Some (String.trim (String.sub line start len))
           else None)
  in
  let trimmed = String.trim ref_str in
  let is_placeholder =
    starts_with ~prefix:"{{" trimmed && ends_with ~suffix:"}}" trimmed
  in
  if is_placeholder then
    let var = String.sub trimmed 2 (String.length trimmed - 4) in
    let parts = String.split_on_char '.' var in
    let node_id, path = match parts with
      | id :: rest -> (id, rest)
      | [] -> (var, [])
    in
    (match Hashtbl.find_opt ctx.outputs node_id with
     | Some value ->
         if path = [] then value
         else (match try_extract_json_path value path with
               | Ok v -> v
               | Error _ ->
                   (match path with
                    | [key] ->
                        (match try_extract_bullet_value value key with
                         | Some v -> v
                         | None -> value)
                    | _ -> value))
     | None -> ref_str)
  else
    let parts = String.split_on_char '.' trimmed in
    let node_id, path = match parts with
      | id :: rest -> (id, rest)
      | [] -> (trimmed, [])
    in
    match Hashtbl.find_opt ctx.outputs node_id with
    | Some value ->
        if path = [] then value
        else (match try_extract_json_path value path with
              | Ok v -> v
              | Error _ ->
                  (match path with
                   | [key] ->
                       (match try_extract_bullet_value value key with
                        | Some v -> v
                        | None -> value)
                   | _ -> value))
    | None -> ref_str

let resolve_inputs ctx (mappings : (string * string) list) : (string * string) list =
  let use_key_as_source ~key ~ref_str =
    if ref_str = key then true
    else
      let klen = String.length key in
      let rlen = String.length ref_str in
      klen > rlen &&
      String.sub key 0 rlen = ref_str &&
      (key.[rlen] = '.' || key.[rlen] = '[')
  in
  List.filter_map (fun (key, ref_str) ->
    let source = if use_key_as_source ~key ~ref_str then key else ref_str in
    let value = resolve_single_input ctx source in
    if value = source then None else Some (key, value)
  ) mappings

let substitute_prompt prompt (inputs : (string * string) list) : string =
  List.fold_left (fun acc (key, value) ->
    let pattern = "{{" ^ key ^ "}}" in
    let buf = Buffer.create (String.length acc) in
    let rec replace start =
      match String.index_from_opt acc start '{' with
      | None -> Buffer.add_substring buf acc start (String.length acc - start)
      | Some i ->
          if i + String.length pattern <= String.length acc &&
             String.sub acc i (String.length pattern) = pattern then begin
            Buffer.add_substring buf acc start (i - start);
            Buffer.add_string buf value;
            replace (i + String.length pattern)
          end else begin
            Buffer.add_substring buf acc start (i - start);
            Buffer.add_char buf acc.[i];
            replace (i + 1)
          end
    in
    replace 0;
    Buffer.contents buf
  ) prompt inputs

let substitute_json ctx (json : Yojson.Safe.t) : Yojson.Safe.t =
  let strip_unresolved_placeholders (s : string) : string =
    let re = Str.regexp "{{[^}]+}}" in
    try
      ignore (Str.search_forward re s 0);
      Printf.eprintf
        "[chain] unresolved placeholder stripped in tool args: %s\n%!"
        (truncate_with_ellipsis s);
      Str.global_replace re "" s
    with
    | Not_found -> s
    | _ -> s
  in
  let rec map = function
    | `String s ->
        let mappings = Chain_parser.extract_input_mappings s in
        if mappings = [] then `String s
        else
          let inputs = resolve_inputs ctx mappings in
          let substituted = substitute_prompt s inputs in
          `String (strip_unresolved_placeholders substituted)
    | `Assoc fields ->
        `Assoc (List.map (fun (k, v) -> (k, map v)) fields)
    | `List items ->
        `List (List.map map items)
    | other -> other
  in
  map json

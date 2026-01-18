(** train_dictionary.ml - CLI for training zstd dictionaries

    Usage:
      dune exec train_dictionary -- --type json --samples-dir ./samples/json --output ./data/json.zdict

    Training requires:
    1. Directory with sample files (min 100 samples)
    2. zstd CLI installed (for training)

    Sample collection:
    - Use scripts/collect_samples.py to gather LLM outputs
    - Organize by content type: code/, json/, markdown/, mixed/
*)

let usage = {|
train_dictionary - Train zstd dictionary for LLM response compression

USAGE:
  train_dictionary --type <TYPE> --samples-dir <DIR> --output <PATH>
  train_dictionary --list-types

OPTIONS:
  --type TYPE         Content type: code, json, markdown, mixed
  --samples-dir DIR   Directory containing sample files
  --output PATH       Output dictionary path (e.g., ./data/json.zdict)
  --list-types        List available content types
  --help              Show this help

EXAMPLES:
  # Train JSON dictionary from samples
  train_dictionary --type json --samples-dir ./samples/json --output ./data/json.zdict

  # Train code dictionary
  train_dictionary --type code --samples-dir ./samples/code --output ./data/code.zdict
|}

let content_type_of_string = function
  | "code" -> Some Llm_mcp.Dictionary.Code
  | "json" -> Some Llm_mcp.Dictionary.JSON
  | "markdown" -> Some Llm_mcp.Dictionary.Markdown
  | "mixed" -> Some Llm_mcp.Dictionary.Mixed
  | _ -> None

let read_samples_from_dir (dir : string) : string list =
  let files = Sys.readdir dir in
  Array.to_list files
  |> List.filter (fun f -> not (String.get f 0 = '.'))
  |> List.map (fun f ->
      let path = Filename.concat dir f in
      let ic = open_in_bin path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;
      content
    )

let () =
  let args = Array.to_list Sys.argv |> List.tl in

  (* Parse arguments *)
  let rec parse acc = function
    | [] -> acc
    | "--help" :: _ ->
        print_endline usage;
        exit 0
    | "--list-types" :: _ ->
        print_endline "Available content types:";
        print_endline "  code      - Python, TypeScript, OCaml, etc.";
        print_endline "  json      - API responses, tool calls";
        print_endline "  markdown  - Documentation, prose";
        print_endline "  mixed     - General LLM output (default)";
        exit 0
    | "--type" :: t :: rest -> parse (("type", t) :: acc) rest
    | "--samples-dir" :: d :: rest -> parse (("samples-dir", d) :: acc) rest
    | "--output" :: o :: rest -> parse (("output", o) :: acc) rest
    | arg :: _ ->
        Printf.eprintf "Unknown argument: %s\n%s" arg usage;
        exit 1
  in
  let opts = parse [] args in

  let get key =
    try List.assoc key opts
    with Not_found ->
      Printf.eprintf "Missing required argument: --%s\n%s" key usage;
      exit 1
  in

  let type_str = get "type" in
  let samples_dir = get "samples-dir" in
  let output = get "output" in

  (* Validate content type *)
  let content_type = match content_type_of_string type_str with
    | Some t -> t
    | None ->
        Printf.eprintf "Invalid content type: %s\n" type_str;
        Printf.eprintf "Use --list-types to see available types\n";
        exit 1
  in

  (* Validate samples directory *)
  if not (Sys.file_exists samples_dir && Sys.is_directory samples_dir) then begin
    Printf.eprintf "Samples directory not found: %s\n" samples_dir;
    exit 1
  end;

  (* Read samples *)
  Printf.printf "Reading samples from %s...\n%!" samples_dir;
  let samples = read_samples_from_dir samples_dir in
  Printf.printf "Found %d samples\n%!" (List.length samples);

  if List.length samples < 100 then begin
    Printf.eprintf "Error: Need at least 100 samples, got %d\n" (List.length samples);
    Printf.eprintf "Collect more samples with scripts/collect_samples.py\n";
    exit 1
  end;

  (* Train dictionary *)
  Printf.printf "Training dictionary (type=%s)...\n%!" type_str;
  match Llm_mcp.Dictionary.train ~samples ~content_type with
  | Error e ->
      Printf.eprintf "Training failed: %s\n" e;
      exit 1
  | Ok dict ->
      Printf.printf "Dictionary trained: %d samples, %d bytes\n%!"
        dict.sample_count (String.length dict.dict_data);
      (* Ensure output directory exists *)
      let dir = Filename.dirname output in
      if not (Sys.file_exists dir) then
        Unix.mkdir dir 0o755;
      (* Save dictionary *)
      match Llm_mcp.Dictionary.save dict output with
      | Error e ->
          Printf.eprintf "Failed to save: %s\n" e;
          exit 1
      | Ok () ->
          Printf.printf "Saved to %s\n" output;
          (* Show compression stats *)
          let test_sample = List.hd samples in
          let compressed = Llm_mcp.Dictionary.compress_with_dict dict test_sample in
          let ratio = 1.0 -. (float (String.length compressed) /. float (String.length test_sample)) in
          Printf.printf "Test compression: %.1f%% savings\n" (ratio *. 100.0)

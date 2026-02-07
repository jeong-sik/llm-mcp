(** Ouroboros v20.0: Neural Architecture Search (NAS) - OCaml
    Analyzes codebase health, connectivity, and usage to optimize structure. *)

open Common

let script_dir = Filename.concat me_root "scripts"

let check_connectivity filename =
  let base = Filename.chop_extension filename in
  let target = Filename.concat script_dir filename in
  let found = ref false in
  let rec walk dir =
    if not !found then begin
      try
        Sys.readdir dir
        |> Array.iter (fun name ->
          if not !found then begin
            let path = Filename.concat dir name in
            try
              if Sys.is_directory path then
                walk path
              else if path <> target then
                match read_file_opt path with
                | None -> ()
                | Some s -> if contains ~substring:base s then found := true
            with _ -> ()
          end)
      with _ -> ()
    end
  in
  if Sys.file_exists script_dir && Sys.is_directory script_dir then walk script_dir;
  !found

let scan_vitality () =
  print_endline "🏥 [SURGEON] Scanning system vitality (Usage & Imports)...";

  if not (Sys.file_exists script_dir) then begin
    print_endline "❌ Scripts directory not found.";
    exit 1
  end;

  let files = Sys.readdir script_dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".py") in

  let candidates = List.filter_map (fun filename ->
    let filepath = Filename.concat script_dir filename in
    let stats = Unix.stat filepath in
    let days_since_access = (Unix.time () -. stats.Unix.st_atime) /. (24.0 *. 3600.0) in
    let is_connected = check_connectivity filename in

    let status =
      if days_since_access > 30.0 && not is_connected then "ATROPHIED (Dead Neuron)"
      else if not is_connected then "ISOLATED (Orphan Neuron)"
      else "HEALTHY"
    in

    Printf.printf "  - %s: %s (Idle: %dd)\n" filename status (int_of_float days_since_access);

    if status = "ATROPHIED (Dead Neuron)" then Some filepath else None
  ) files in

  candidates

let () =
  let dead_tissue = scan_vitality () in
  if dead_tissue <> [] then
    Printf.printf "\n⚠️ Recommended Action: %d candidates for pruning found.\n" (List.length dead_tissue)
  else
    print_endline "\n✨ System is lean and optimized. No pruning needed."

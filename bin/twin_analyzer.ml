(** twin_analyzer - Digital Twin persona analyzer

    Ouroboros v21.0: Digital Twin Analyzer
    Synthesizes user persona from interaction logs and system history.

    Usage:
      twin-analyzer [proposal_title]
*)

let me_root = Common.me_root

let twin_file =
  Filename.concat (Filename.concat (Filename.concat me_root "lib") "state") "user_twin.json"

(** Load twin state from JSON file *)
let load_state () =
  if Sys.file_exists twin_file then
    try
      let ic = open_in twin_file in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      Yojson.Safe.from_string s
    with Sys_error _ | Yojson.Json_error _ -> `Assoc [("persona", `Assoc []); ("interaction_stats", `Assoc [])]
  else
    `Assoc [("persona", `Assoc []); ("interaction_stats", `Assoc [])]

(** Get nested JSON value *)
let get_nested json keys =
  let rec aux j = function
    | [] -> Some j
    | k :: ks ->
        match j with
        | `Assoc fields ->
            (match List.assoc_opt k fields with
             | Some v -> aux v ks
             | None -> None)
        | _ -> None
  in
  aux json keys

(** Check if "Evolution" is in persona values *)
let has_evolution_value state =
  match get_nested state ["persona"; "personality"; "values"] with
  | Some (`List values) ->
      List.exists (function
        | `String s -> String.lowercase_ascii s = "evolution"
        | _ -> false
      ) values
  | _ -> false

(** Predict user action based on Digital Twin persona *)
let predict_action state proposal =
  Printf.printf "👁️  [TWIN] Mirroring Master's consciousness for: '%s'\n" proposal;

  if has_evolution_value state then begin
    print_endline "   > Master loves evolution. Confidence: HIGH.";
    "APPROVE"
  end else
    "ASK_USER"

(** Sync with logs (mock implementation) *)
let sync_with_logs state =
  print_endline "📡 [TWIN] Syncing with Master's recent activities...";

  (* Update interaction stats *)
  let new_state = match state with
    | `Assoc fields ->
        let interaction_stats = match List.assoc_opt "interaction_stats" fields with
          | Some (`Assoc stats) ->
              let total = match List.assoc_opt "total_sessions" stats with
                | Some (`Int n) -> n + 1
                | Some (`Float n) -> int_of_float n + 1
                | _ -> 1
              in
              `Assoc (("total_sessions", `Int total) :: List.remove_assoc "total_sessions" stats)
          | _ -> `Assoc [("total_sessions", `Int 1)]
        in
        `Assoc (("interaction_stats", interaction_stats) :: List.remove_assoc "interaction_stats" fields)
    | _ -> state
  in

  (* Write back to file *)
  let dir = Filename.dirname twin_file in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755;

  let oc = open_out twin_file in
  output_string oc (Yojson.Safe.pretty_to_string new_state);
  close_out oc;

  print_endline "✅ Twin Persona updated.";
  new_state

let () =
  let state = load_state () in
  let updated_state = sync_with_logs state in

  if Array.length Sys.argv > 1 then begin
    let _ = predict_action updated_state Sys.argv.(1) in
    ()
  end

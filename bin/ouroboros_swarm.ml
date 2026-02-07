(** Ouroboros v14.0: Hive Link (Swarm Communication) - OCaml
    Broadcasts heartbeat and checks other nodes via Git. *)

open Common

let swarm_dir = Filename.concat (Filename.concat me_root "logs") "swarm"
let synapse_dir = Filename.concat swarm_dir "synapses"
let _task_dir = Filename.concat swarm_dir "tasks"  (* Reserved for future use *)

let hostname =
  Unix.gethostname ()

let heartbeat_file = Filename.concat swarm_dir (Printf.sprintf "heartbeat_%s.json" hostname)
let liquid_state_path = Filename.concat (Filename.concat (Filename.concat me_root "lib") "state") "liquid_state.json"

let get_mood () =
  let emotion_path = Filename.concat (Filename.concat (Filename.concat me_root "lib") "state") "emotion.json" in
  if Sys.file_exists emotion_path then
    match read_json_opt emotion_path with
    | Some json ->
        let open Yojson.Safe.Util in
        (try json |> member "current_mood" |> to_string with Type_error _ -> "NEUTRAL")
    | None -> "NEUTRAL"
  else "NEUTRAL"

let pulse ?(status="IDLE") ?(task="None") () =
  ensure_dir swarm_dir;
  ensure_dir synapse_dir;

  let data = `Assoc [
    ("hostname", `String hostname);
    ("timestamp", `String (time_str ()));
    ("status", `String status);
    ("current_task", `String task);
    ("mood", `String (get_mood ()));
  ] in

  ignore (write_json heartbeat_file data);

  (* Export synapses if liquid state exists *)
  if Sys.file_exists liquid_state_path then begin
    match read_json_opt liquid_state_path with
    | Some json ->
        let open Yojson.Safe.Util in
        let weights = try json |> member "weights" with Type_error _ -> `Null in
        if weights <> `Null then begin
          let synapse_file = Filename.concat synapse_dir (Printf.sprintf "state_%s.json" hostname) in
          ignore (write_json synapse_file weights)
        end
    | None -> ()
  end;

  Printf.printf "💓 Pulse sent from %s: %s (Synapses Exported)\n" hostname status

let scan_swarm () =
  print_endline "\n🐝 Scanning the Hive...";
  ensure_dir swarm_dir;

  let files = Sys.readdir swarm_dir in
  let found = ref false in

  Array.iter (fun f ->
    if String.length f > 10 && String.sub f 0 10 = "heartbeat_" &&
       f <> (Printf.sprintf "heartbeat_%s.json" hostname) then begin
      let path = Filename.concat swarm_dir f in
      match read_json_opt path with
      | Some json ->
          let open Yojson.Safe.Util in
          let h = try json |> member "hostname" |> to_string with Type_error _ -> "?" in
          let s = try json |> member "status" |> to_string with Type_error _ -> "?" in
          let t = try json |> member "current_task" |> to_string with Type_error _ -> "?" in
          Printf.printf "  - Node: %s | Status: %s | Task: %s\n" h s t;
          found := true
      | None -> ()
    end
  ) files;

  if not !found then
    print_endline "  (No other active nodes detected. I am alone.)"

let sync_synapses () =
  print_endline "🧠 [SYNAPSE] Synchronizing with Hive Mind...";

  if not (Sys.file_exists liquid_state_path) then begin
    print_endline "   (No liquid state found)";
    ()
  end else
    match read_json_opt liquid_state_path with
    | None -> ()
    | Some my_state ->
        let open Yojson.Safe.Util in
        let my_weights = try my_state |> member "weights" |> to_assoc with Type_error _ -> [] in

        ensure_dir synapse_dir;
        let peer_files = Sys.readdir synapse_dir in

        if Array.length peer_files = 0 then
          print_endline "   (No peers found)"
        else begin
          let merged = ref my_weights in

          Array.iter (fun f ->
            if f <> Printf.sprintf "state_%s.json" hostname then begin
              let path = Filename.concat synapse_dir f in
              match read_json_opt path with
              | Some peer_weights ->
                  Printf.printf "   Merging knowledge from %s...\n" f;
                  let pw = try to_assoc peer_weights with Type_error _ -> [] in
                  List.iter (fun (k, v) ->
                    let pv = try to_float v with Type_error _ -> 1.0 in
                    let current = try List.assoc k !merged |> to_float with Type_error _ | Not_found -> 1.0 in
                    let new_val = current +. (pv -. current) *. 0.2 in
                    merged := (k, `Float new_val) :: (List.remove_assoc k !merged)
                  ) pw
              | None -> ()
            end
          ) peer_files;

          let updated = `Assoc [
            ("weights", `Assoc !merged);
            ("last_sync", `String (time_str ()));
          ] in
          ignore (write_json liquid_state_path updated);
          print_endline "✨ Synapse Sync Complete. Brain Updated."
        end

let print_usage () =
  print_endline "Usage: ouroboros-swarm [pulse|scan|sync] [status]"

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "pulse" :: rest ->
      let status = match rest with s :: _ -> s | [] -> "IDLE" in
      pulse ~status ()
  | ["scan"] -> scan_swarm ()
  | ["sync"] -> sync_synapses ()
  | [] ->
      pulse ();
      scan_swarm ()
  | _ -> print_usage ()

(** credits_fetcher.ml - Credits Unified Monitoring
    OCaml replacement for scripts/credits_fetcher.py *)

open Cmdliner

let me_root =
  try Unix.getenv "ME_ROOT" with Not_found -> Filename.concat (Unix.getenv "HOME") "me"

let credits_json = Filename.concat me_root "data/state/credits.json"

(* ANSI colors *)
let green s = Printf.sprintf "\027[92m%s\027[0m" s
let yellow s = Printf.sprintf "\027[93m%s\027[0m" s
let red s = Printf.sprintf "\027[91m%s\027[0m" s
let bold s = Printf.sprintf "\027[1m%s\027[0m" s

let color_percent pct =
  let s = Printf.sprintf "%.0f%%" pct in
  if pct >= 70.0 then green s else if pct >= 30.0 then yellow s else red s

let format_money amount =
  let s = Printf.sprintf "$%.2f" amount in
  if amount >= 50.0 then green s else if amount >= 20.0 then yellow s else red s

let load_credits () =
  if Sys.file_exists credits_json then (
    let ic = open_in credits_json in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Yojson.Safe.from_string s)
  else `Assoc [ ("updated_at", `Null); ("services", `Assoc []) ]

let save_credits data =
  let dir = Filename.dirname credits_json in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime now in
  let timestamp =
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
  in
  let updated =
    match data with
    | `Assoc items -> `Assoc (("updated_at", `String timestamp) :: List.remove_assoc "updated_at" items)
    | _ -> data
  in
  let oc = open_out credits_json in
  output_string oc (Yojson.Safe.pretty_to_string updated);
  close_out oc;
  Printf.printf "✅ Saved to %s\n" credits_json

let curl_get url headers =
  let header_args =
    List.map (fun (k, v) -> Printf.sprintf "-H '%s: %s'" k v) headers |> String.concat " "
  in
  let cmd = Printf.sprintf "curl -s %s '%s'" header_args url in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  Buffer.contents buf

let curl_post url headers body =
  let header_args =
    List.map (fun (k, v) -> Printf.sprintf "-H '%s: %s'" k v) headers |> String.concat " "
  in
  let cmd = Printf.sprintf "curl -s -X POST %s -d '%s' '%s'" header_args body url in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  Buffer.contents buf

let fetch_elevenlabs () =
  match Sys.getenv_opt "ELEVENLABS_API_KEY" with
  | None ->
      Printf.printf "⚠️  ELEVENLABS_API_KEY not set\n";
      None
  | Some api_key ->
      let url = "https://api.elevenlabs.io/v1/user/subscription" in
      let headers = [ ("xi-api-key", api_key) ] in
      let resp = curl_get url headers in
      (try
         let json = Yojson.Safe.from_string resp in
         let open Yojson.Safe.Util in
         let remaining = json |> member "character_count" |> to_int in
         let limit = json |> member "character_limit" |> to_int in
         let pct = float_of_int remaining /. float_of_int (max limit 1) *. 100.0 in
         Some
           (`Assoc
             [
               ("remaining", `Int remaining);
               ("limit", `Int limit);
               ("percent_remaining", `Float (Float.round (pct *. 10.0) /. 10.0));
               ("source", `String "api");
             ])
       with _ ->
         Printf.printf "❌ ElevenLabs fetch failed\n";
         None)

let fetch_runpod () =
  match Sys.getenv_opt "RUNPOD_API_TOKEN" with
  | None ->
      Printf.printf "⚠️  RUNPOD_API_TOKEN not set\n";
      None
  | Some api_key ->
      let url = Printf.sprintf "https://api.runpod.io/graphql?api_key=%s" api_key in
      let headers = [ ("Content-Type", "application/json") ] in
      let query = {|{"query":"query { myself { currentSpendPerHr clientBalance } }"}|} in
      let resp = curl_post url headers query in
      (try
         let json = Yojson.Safe.from_string resp in
         let open Yojson.Safe.Util in
         let myself = json |> member "data" |> member "myself" in
         let balance = myself |> member "clientBalance" |> to_float in
         let spend_hr = myself |> member "currentSpendPerHr" |> to_float in
         let hours = if spend_hr > 0.0 then int_of_float (balance /. spend_hr) else 9999 in
         Some
           (`Assoc
             [
               ("balance", `Float (Float.round (balance *. 100.0) /. 100.0));
               ("spend_per_hr", `Float (Float.round (spend_hr *. 1000.0) /. 1000.0));
               ("hours_remaining", `Int hours);
               ("source", `String "api");
             ])
       with _ ->
         Printf.printf "❌ RunPod fetch failed\n";
         None)

let do_fetch () =
  let data = load_credits () in
  let services =
    match data with
    | `Assoc items -> (
        match List.assoc_opt "services" items with
        | Some (`Assoc s) -> s
        | _ -> [])
    | _ -> []
  in
  Printf.printf "📡 Fetching ElevenLabs...\n";
  let services =
    match fetch_elevenlabs () with
    | Some el ->
        let open Yojson.Safe.Util in
        Printf.printf "   ✅ %d / %d chars (%.1f%%)\n"
          (el |> member "remaining" |> to_int)
          (el |> member "limit" |> to_int)
          (el |> member "percent_remaining" |> to_float);
        ("elevenlabs", el) :: List.remove_assoc "elevenlabs" services
    | None -> services
  in
  Printf.printf "📡 Fetching RunPod...\n";
  let services =
    match fetch_runpod () with
    | Some rp ->
        let open Yojson.Safe.Util in
        Printf.printf "   ✅ $%.2f (%dh remaining)\n"
          (rp |> member "balance" |> to_float)
          (rp |> member "hours_remaining" |> to_int);
        ("runpod", rp) :: List.remove_assoc "runpod" services
    | None -> services
  in
  let new_data =
    match data with
    | `Assoc items ->
        `Assoc (("services", `Assoc services) :: List.remove_assoc "services" items)
    | _ -> `Assoc [ ("services", `Assoc services) ]
  in
  save_credits new_data

let get_string_opt json key =
  try Some Yojson.Safe.Util.(json |> member key |> to_string) with _ -> None

let get_float_opt json key =
  try Some Yojson.Safe.Util.(json |> member key |> to_float) with _ -> None

let get_int_opt json key =
  try Some Yojson.Safe.Util.(json |> member key |> to_int) with _ -> None

let show_summary () =
  let data = load_credits () in
  let open Yojson.Safe.Util in
  let services = try data |> member "services" with _ -> `Assoc [] in
  let updated = get_string_opt data "updated_at" |> Option.value ~default:"Unknown" in
  Printf.printf "\n%s\n" (bold "📊 Credits Summary");
  Printf.printf "   Updated: %s\n" (String.sub updated 0 (min 19 (String.length updated)));
  Printf.printf "%s\n" (String.make 60 '\xe2');
  Printf.printf "%-20s %-20s %-20s\n" "Service" "Status" "Details";
  Printf.printf "%s\n" (String.make 60 '-');

  (* ElevenLabs *)
  (match services |> member "elevenlabs" with
  | `Assoc _ as el ->
      let pct = get_float_opt el "percent_remaining" |> Option.value ~default:0.0 in
      let remaining = get_int_opt el "remaining" |> Option.value ~default:0 in
      Printf.printf "%-20s %-30s %d chars\n" "ElevenLabs" (color_percent pct) remaining
  | _ -> ());

  (* RunPod *)
  (match services |> member "runpod" with
  | `Assoc _ as rp ->
      let bal = get_float_opt rp "balance" |> Option.value ~default:0.0 in
      let hrs = get_int_opt rp "hours_remaining" |> Option.value ~default:0 in
      Printf.printf "%-20s %-30s %dh remaining\n" "RunPod" (format_money bal) hrs
  | _ -> ());

  (* Railway *)
  (match services |> member "railway" with
  | `Assoc _ as rail ->
      let est = get_float_opt rail "estimated_bill" |> Option.value ~default:0.0 in
      let period = get_string_opt rail "period" |> Option.value ~default:"" in
      Printf.printf "%-20s Est: $%.2f%-15s %s\n" "Railway" est "" period
  | _ -> ());

  (* Anthropic API *)
  (match services |> member "anthropic" with
  | `Assoc _ as anth ->
      let bal = get_float_opt anth "balance" |> Option.value ~default:0.0 in
      Printf.printf "%-20s %-30s\n" "Anthropic API" (format_money bal)
  | _ -> ());

  Printf.printf "%s\n" (String.make 60 '\xe2');
  Printf.printf "💡 Use 'sb credits all' for detailed view\n";
  Printf.printf "   Use 'sb credits fetch' to refresh API services\n\n"

let show_all () =
  let data = load_credits () in
  let open Yojson.Safe.Util in
  let services = try data |> member "services" with _ -> `Assoc [] in
  let updated = get_string_opt data "updated_at" |> Option.value ~default:"Unknown" in
  Printf.printf "\n%s\n" (bold "📊 Credits Detailed View");
  Printf.printf "   Updated: %s\n" updated;
  Printf.printf "%s\n" (String.make 70 '=');

  (* ElevenLabs *)
  Printf.printf "\n%s\n%s\n" (bold "🔊 ElevenLabs") (String.make 70 '-');
  (match services |> member "elevenlabs" with
  | `Assoc _ as el ->
      Printf.printf "  Characters: %d / %d\n"
        (get_int_opt el "remaining" |> Option.value ~default:0)
        (get_int_opt el "limit" |> Option.value ~default:0);
      Printf.printf "  Remaining: %.1f%%\n"
        (get_float_opt el "percent_remaining" |> Option.value ~default:0.0)
  | _ -> Printf.printf "  No data available\n");

  (* RunPod *)
  Printf.printf "\n%s\n%s\n" (bold "🚀 RunPod") (String.make 70 '-');
  (match services |> member "runpod" with
  | `Assoc _ as rp ->
      Printf.printf "  Balance: $%.2f\n" (get_float_opt rp "balance" |> Option.value ~default:0.0);
      Printf.printf "  Spend/Hour: $%.4f\n"
        (get_float_opt rp "spend_per_hr" |> Option.value ~default:0.0);
      Printf.printf "  Hours Remaining: %d\n"
        (get_int_opt rp "hours_remaining" |> Option.value ~default:0)
  | _ -> Printf.printf "  No data available\n");

  (* Railway *)
  Printf.printf "\n%s\n%s\n" (bold "🚂 Railway") (String.make 70 '-');
  (match services |> member "railway" with
  | `Assoc _ as rail ->
      Printf.printf "  Current Usage: $%.2f\n"
        (get_float_opt rail "current_usage" |> Option.value ~default:0.0);
      Printf.printf "  Estimated Bill: $%.2f\n"
        (get_float_opt rail "estimated_bill" |> Option.value ~default:0.0);
      Printf.printf "  Period: %s\n" (get_string_opt rail "period" |> Option.value ~default:"N/A")
  | _ -> Printf.printf "  No data available\n");

  (* Anthropic *)
  Printf.printf "\n%s\n%s\n" (bold "🤖 Anthropic API") (String.make 70 '-');
  (match services |> member "anthropic" with
  | `Assoc _ as anth ->
      Printf.printf "  Balance: $%.2f\n" (get_float_opt anth "balance" |> Option.value ~default:0.0)
  | _ -> Printf.printf "  No data available\n");

  Printf.printf "\n%s\n" (String.make 70 '=');
  Printf.printf "💡 Run 'sb credits fetch' to refresh API-based services\n\n"

let run cmd =
  match cmd with
  | "fetch" -> do_fetch ()
  | "all" -> show_all ()
  | "summary" | "" -> show_summary ()
  | _ ->
      Printf.eprintf "Unknown command: %s\n" cmd;
      exit 1

let cmd_arg =
  let doc = "Command: fetch, summary, all" in
  Arg.(value & pos 0 string "summary" & info [] ~docv:"COMMAND" ~doc)

let cmd =
  let doc = "Credits Unified Monitoring" in
  let info = Cmd.info "credits-fetcher" ~doc in
  Cmd.v info Term.(const run $ cmd_arg)

let () = exit (Cmd.eval cmd)

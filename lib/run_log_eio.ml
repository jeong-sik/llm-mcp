(** LLM-MCP run log (JSONL) - Pure Eio Version

    This is designed for lightweight observability and data collection.
    It intentionally avoids logging prompts/responses; only lengths + metadata.

    Pure Eio: Uses Eio.Mutex and Eio.Path for async-safe file operations.
*)

open Types

let default_log_path () =
  Common.me_path ["logs"; "llm_mcp_runs.jsonl"]

let log_path () =
  match Sys.getenv_opt "LLM_MCP_RUN_LOG_PATH" with
  | Some p when String.length p > 0 -> p
  | _ -> default_log_path ()

(** Write mutex - created lazily per-domain *)
let write_mutex = Eio.Mutex.create ()

let ensure_log_dir () =
  let path = log_path () in
  Common.ensure_dir (Filename.dirname path);
  path

let assoc_of_kv (kvs : (string * string) list) =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) kvs)

(** Append a JSON line to the log file (Eio version) *)
let append_jsonl ~fs json =
  let path = ensure_log_dir () in
  let line = Yojson.Safe.to_string json ^ "\n" in
  Eio.Mutex.use_rw ~protect:true write_mutex (fun () ->
    let file_path = Eio.Path.(fs / path) in
    (* Open with O_APPEND for atomic appends *)
    Eio.Path.with_open_out
      ~append:true
      ~create:(`If_missing 0o644)
      file_path
      (fun flow -> Eio.Flow.copy_string line flow))

(** Record a tool execution to the run log *)
let record ~fs ~(tool : string) ~(streamed : bool) ~(prompt_chars : int) ~(duration_ms : int)
    (result : tool_result) =
  let json =
    `Assoc [
      ("ts", `Int (int_of_float (Unix.gettimeofday ())));
      ("iso", `String (Common.iso_timestamp ()));
      ("tool", `String tool);
      ("model", `String result.model);
      ("returncode", `Int result.returncode);
      ("duration_ms", `Int duration_ms);
      ("prompt_chars", `Int prompt_chars);
      ("response_chars", `Int (String.length result.response));
      ("streamed", `Bool streamed);
      ("extra", assoc_of_kv result.extra);
    ]
  in
  try
    append_jsonl ~fs json
  with exn ->
    Printf.eprintf "[run_log_eio] Write failed: %s\n%!" (Printexc.to_string exn)

(** Read all events from the log file (Pure OCaml - no Eio needed) *)
let read_events () =
  let path = log_path () in
  if not (Sys.file_exists path) then []
  else
    Common.read_lines path
    |> List.filter_map (fun line ->
      let line = String.trim line in
      if line = "" then None
      else
        try Some (Yojson.Safe.from_string line)
        with _ -> None)

let int_field json key ~default =
  let open Yojson.Safe.Util in
  try json |> member key |> to_int
  with _ -> default

let string_field json key ~default =
  let open Yojson.Safe.Util in
  try json |> member key |> to_string
  with _ -> default

let take_last n lst =
  if n <= 0 then []
  else
    let len = List.length lst in
    let drop = max 0 (len - n) in
    let rec drop_n i xs =
      if i <= 0 then xs
      else match xs with [] -> [] | _ :: tl -> drop_n (i - 1) tl
    in
    drop_n drop lst

(** Read recent events since a timestamp *)
let read_recent ~since_ts ~limit =
  let events =
    read_events ()
    |> List.filter (fun ev -> int_field ev "ts" ~default:0 >= since_ts)
  in
  take_last limit events

(** Compute statistics over events in a time range *)
let stats ~since_ts ~until_ts =
  let events =
    read_events ()
    |> List.filter (fun ev ->
      let ts = int_field ev "ts" ~default:0 in
      ts >= since_ts && (until_ts = 0 || ts <= until_ts))
  in
  let total = List.length events in
  let success =
    List.fold_left
      (fun acc ev -> if int_field ev "returncode" ~default:(-1) = 0 then acc + 1 else acc)
      0
      events
  in
  let durations =
    events |> List.map (fun ev -> int_field ev "duration_ms" ~default:0)
  in
  let duration_sum = List.fold_left ( + ) 0 durations in
  let duration_avg = if total = 0 then 0 else duration_sum / total in

  let by_tool = Hashtbl.create 16 in
  List.iter (fun ev ->
    let tool = string_field ev "tool" ~default:"unknown" in
    let cur = match Hashtbl.find_opt by_tool tool with Some n -> n | None -> 0 in
    Hashtbl.replace by_tool tool (cur + 1)
  ) events;
  let by_tool_json =
    Hashtbl.fold (fun tool count acc -> (tool, `Int count) :: acc) by_tool []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> fun fields -> `Assoc fields
  in

  `Assoc [
    ("since_ts", `Int since_ts);
    ("until_ts", `Int until_ts);
    ("total", `Int total);
    ("success", `Int success);
    ("failure", `Int (total - success));
    ("avg_duration_ms", `Int duration_avg);
    ("by_tool", by_tool_json);
  ]

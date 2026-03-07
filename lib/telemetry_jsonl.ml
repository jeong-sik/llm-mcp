let expand_tilde path =
  if path <> "" && path.[0] = '~' then
    match Sys.getenv_opt "HOME" with
    | Some home -> home ^ String.sub path 1 (String.length path - 1)
    | None -> path
  else
    path

let telemetry_file () =
  let normalize_env = function
    | Some "" -> None
    | other -> other
  in
  let from_env =
    match Sys.getenv_opt "LLM_MCP_TELEMETRY_FILE" with
    | Some p -> normalize_env (Some p)
    | None -> normalize_env (Sys.getenv_opt "MCP_TELEMETRY_FILE")
  in
  from_env
  |> Option.value ~default:"data/telemetry.jsonl"
  |> expand_tilde

type tool_usage_stats = {
  count : int;
  success_count : int;
  failure_count : int;
  last_used_at : float option;
}

type tool_usage_summary = {
  telemetry_path : string;
  telemetry_available : bool;
  total_calls : int;
  stats_by_tool : (string, tool_usage_stats) Hashtbl.t;
}

let empty_tool_usage_stats =
  {
    count = 0;
    success_count = 0;
    failure_count = 0;
    last_used_at = None;
  }

let update_tool_usage stats_by_tool ~tool_name ~success ~timestamp =
  let current =
    match Hashtbl.find_opt stats_by_tool tool_name with
    | Some stats -> stats
    | None -> empty_tool_usage_stats
  in
  let updated =
    {
      count = current.count + 1;
      success_count = current.success_count + (if success then 1 else 0);
      failure_count = current.failure_count + (if success then 0 else 1);
      last_used_at =
        Some
          (match current.last_used_at with
          | Some previous -> max previous timestamp
          | None -> timestamp);
    }
  in
  Hashtbl.replace stats_by_tool tool_name updated

let summarize_tool_usage ?path () =
  let telemetry_path = Option.value ~default:(telemetry_file ()) path in
  let stats_by_tool = Hashtbl.create 32 in
  let total_calls = ref 0 in
  let telemetry_available = Sys.file_exists telemetry_path in
  if telemetry_available then
    let ic = open_in telemetry_path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        try
          while true do
            let line = input_line ic in
            if String.trim line <> "" then
              (try
                 let open Yojson.Safe.Util in
                 let json = Yojson.Safe.from_string line in
                 match json |> member "event" with
                 | `List [ `String "Tool_called"; (`Assoc _ as payload) ] -> (
                     match payload |> member "tool_name" with
                     | `String tool_name ->
                         let success =
                           match payload |> member "success" with
                           | `Bool value -> value
                           | _ -> false
                         in
                         let timestamp =
                           match json |> member "timestamp" with
                           | `Float value -> value
                           | `Int value -> float_of_int value
                           | _ -> 0.0
                         in
                         incr total_calls;
                         update_tool_usage stats_by_tool ~tool_name ~success
                           ~timestamp
                     | _ -> ())
                 | _ -> ()
               with _ -> ())
          done
        with End_of_file -> ())
  else
    ();
  {
    telemetry_path;
    telemetry_available;
    total_calls = !total_calls;
    stats_by_tool;
  }

let tool_usage_fields summary tool_name =
  let stats =
    match Hashtbl.find_opt summary.stats_by_tool tool_name with
    | Some stats -> stats
    | None -> empty_tool_usage_stats
  in
  [
    ("usageCount", `Int stats.count);
    ("usageSuccessCount", `Int stats.success_count);
    ("usageFailureCount", `Int stats.failure_count);
    ( "usageLastUsedAt",
      match stats.last_used_at with
      | Some timestamp -> `Float timestamp
      | None -> `Null );
  ]

let rec ensure_dir path =
  if path = "" || path = "." || path = "/" then
    ()
  else if Sys.file_exists path then
    ()
  else (
    ensure_dir (Filename.dirname path);
    Unix.mkdir path 0o755
  )

let ensure_parent_dir path =
  let dir = Filename.dirname path in
  try ensure_dir dir with _ -> ()

let write_mutex = Mutex.create ()

let append_json (json : Yojson.Safe.t) =
  let path = telemetry_file () in
  ensure_parent_dir path;
  Mutex.lock write_mutex;
  Common.protect
    ~module_name:"telemetry_jsonl"
    ~finally_label:"Mutex.unlock"
    ~finally:(fun () -> Mutex.unlock write_mutex)
    (fun () ->
      try
        let oc =
          open_out_gen [ Open_append; Open_creat; Open_text ] 0o644 path
        in
        Common.protect
          ~module_name:"telemetry_jsonl"
          ~finally_label:"close_out"
          ~finally:(fun () -> close_out oc)
          (fun () ->
            output_string oc (Yojson.Safe.to_string json);
            output_char oc '\n';
            flush oc)
      with _ -> ())

let log_tool_called ~tool_name ~url ~duration_ms ~success ~error =
  let error_json = match error with Some e -> `String e | None -> `Null in
  let payload =
    `Assoc
      [
        ("tool_name", `String tool_name);
        ("success", `Bool success);
        ("duration_ms", `Int duration_ms);
        ("agent_id", `Null);
        ("url", `String url);
        ("error", error_json);
      ]
  in
  let record =
    `Assoc
      [
        ("timestamp", `Float (Unix.gettimeofday ()));
        ("event", `List [ `String "Tool_called"; payload ]);
      ]
  in
  append_json record

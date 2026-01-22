(** agent_stats - Agent Statistics Tracker

    OCaml port of features/hooks/lib/agent_stats.py
    Tracks SubAgent calls from session transcript.

    Usage:
      echo '{"sessionId": "xxx", "transcript_path": "/path/to/transcript.jsonl"}' | agent_stats
*)

(* Use Common module utilities *)
let read_file = Llm_mcp.Common.read_file_opt
let read_lines = Llm_mcp.Common.read_lines
let iso_timestamp = Llm_mcp.Common.iso_timestamp

(** Extract agent type from a tool_use item *)
let extract_agent_type item =
  let open Yojson.Safe.Util in
  try
    let name = item |> member "name" |> to_string in
    if name = "Task" then
      let input = item |> member "input" in
      let agent_type = input |> member "subagent_type" |> to_string_option in
      agent_type
    else None
  with Sys_error _ | Yojson.Json_error _ -> None

(** Parse transcript line for agent calls *)
let parse_line_for_agents line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let event_type = json |> member "type" |> to_string_option in
    match event_type with
    | Some "assistant" ->
        let message = json |> member "message" in
        let content = message |> member "content" in
        begin match content with
        | `List items ->
            List.filter_map (fun item ->
              match item |> member "type" |> to_string_option with
              | Some "tool_use" -> extract_agent_type item
              | _ -> None
            ) items
        | _ -> []
        end
    | _ -> []
  with Yojson.Safe.Util.Type_error _ | Yojson.Json_error _ -> []

(** Parse session transcript for agent calls *)
let parse_session_transcript transcript_path =
  let lines = read_lines transcript_path in
  List.concat_map parse_line_for_agents lines

(** Count occurrences in a list *)
let count_items items =
  let tbl = Hashtbl.create 16 in
  List.iter (fun item ->
    let count = try Hashtbl.find tbl item with Not_found -> 0 in
    Hashtbl.replace tbl item (count + 1)
  ) items;
  tbl

(** Get stats file path *)
let get_stats_file () =
  let me_root = Llm_mcp.Common.me_root in
  Filename.concat (Filename.concat (Filename.concat me_root ".claude") "state") "agent-stats.json"

(** Load existing agent stats *)
let load_agent_stats () =
  let stats_file = get_stats_file () in
  match read_file stats_file with
  | Some content ->
      begin try
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        let total_calls = json |> member "total_calls" |> to_assoc in
        let tbl = Hashtbl.create 16 in
        List.iter (fun (k, v) ->
          Hashtbl.add tbl k (v |> to_int)
        ) total_calls;
        tbl
      with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> Hashtbl.create 16
      end
  | None -> Hashtbl.create 16

(** Save agent stats *)
let save_agent_stats stats =
  let stats_file = get_stats_file () in
  (* Create parent directory *)
  let parent = Filename.dirname stats_file in
  if not (Sys.file_exists parent) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote parent)));

  (* Build JSON *)
  let items = Hashtbl.fold (fun k v acc -> (k, `Int v) :: acc) stats [] in
  let json = `Assoc [
    ("total_calls", `Assoc items);
    ("last_updated", `String (iso_timestamp ()));
  ] in

  try
    let oc = open_out stats_file in
    output_string oc (Yojson.Safe.pretty_to_string json);
    output_char oc '\n';
    close_out oc
  with Sys_error _ -> ()

(** Update stats and return context/warnings *)
let update_stats _session_id transcript_path =
  (* Parse transcript *)
  let agent_calls = parse_session_transcript transcript_path in

  if List.length agent_calls = 0 then
    `Assoc [
      ("context", `List []);
      ("warnings", `List []);
    ]
  else begin
    (* Load existing stats *)
    let stats = load_agent_stats () in

    (* Count session calls *)
    let session_counts = count_items agent_calls in

    (* Update totals *)
    Hashtbl.iter (fun agent count ->
      let existing = try Hashtbl.find stats agent with Not_found -> 0 in
      Hashtbl.replace stats agent (existing + count)
    ) session_counts;

    (* Save *)
    save_agent_stats stats;

    (* Generate context *)
    let context = ref [] in
    let total = List.length agent_calls in
    context := !context @ [Printf.sprintf "🤖 %d agent calls this session" total];

    (* Top 3 agents *)
    let sorted = Hashtbl.fold (fun k v acc -> (k, v) :: acc) session_counts []
      |> List.sort (fun (_, a) (_, b) -> compare b a)
      |> (fun l -> if List.length l > 3 then List.filteri (fun i _ -> i < 3) l else l)
    in
    List.iter (fun (agent, count) ->
      context := !context @ [Printf.sprintf "   • %s: %d" agent count]
    ) sorted;

    `Assoc [
      ("context", `List (List.map (fun s -> `String s) !context));
      ("warnings", `List []);
    ]
  end

(** Format output as human-readable text *)
let format_output = Llm_mcp.Common.format_result_output

(** Main function *)
let main () =
  (* Read JSON input from stdin *)
  let input =
    if Unix.isatty Unix.stdin then ""
    else begin
      let buf = Buffer.create 256 in
      try
        while true do
          Buffer.add_string buf (input_line stdin);
          Buffer.add_char buf '\n'
        done;
        Buffer.contents buf
      with End_of_file -> String.trim (Buffer.contents buf)
    end
  in

  let session_id, transcript_path =
    if String.length input > 0 then
      try
        let json = Yojson.Safe.from_string input in
        let open Yojson.Safe.Util in
        let sid = json |> member "sessionId" |> to_string_option in
        let tp = json |> member "transcript_path" |> to_string_option in
        (sid, tp)
      with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> (None, None)
    else (None, None)
  in

  let result = match session_id, transcript_path with
    | Some sid, Some tp when Sys.file_exists tp ->
        update_stats sid tp
    | Some _, Some tp ->
        `Assoc [
          ("context", `List []);
          ("warnings", `List [`String (Printf.sprintf "⚠️  Transcript not found: %s" tp)]);
        ]
    | _ ->
        `Assoc [
          ("context", `List []);
          ("warnings", `List [`String "⚠️  No session ID or transcript path provided"]);
        ]
  in

  (* Print formatted output *)
  print_string (format_output result);

  (* Print JSON for programmatic use *)
  print_endline "--- JSON ---";
  print_endline (Yojson.Safe.pretty_to_string result);

  0

let () = exit (main ())

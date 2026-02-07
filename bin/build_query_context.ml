(** build_query_context - Build rich context for query improvement

    OCaml port of .claude/hooks/build-query-context.py
    Analyzes transcript, git state, and recent errors.

    Usage:
      build_query_context [--transcript <path>] [--session-id <id>] [--turns <n>]
*)

open Cmdliner

(** Get ME_ROOT from Common module *)
let me_root () = Common.me_root

(** Run an external command (argv-only) and capture stdout, with a real timeout.
    Stdout/stderr are isolated; stderr is discarded for stability. *)
let run_command_with_timeout prog args timeout_sec =
  try
    let res =
      Common.Subprocess.run_capture ~timeout_s:timeout_sec ~stderr:`Dev_null prog args
    in
    res.stdout
  with _ -> ""

(** Check if git index.lock exists *)
let check_git_lock () =
  let lock_file = Filename.concat (me_root ()) ".git/index.lock" in
  Sys.file_exists lock_file

(** Get git state (changed files, untracked, commits) *)
let get_git_state () =
  if check_git_lock () then
    `Assoc [
      ("changed_files", `List []);
      ("untracked_files", `List []);
      ("recent_commits", `List []);
      ("has_changes", `Bool false);
      ("locked", `Bool true);
      ("lock_warning", `String "⚠️ Git lock 감지 - `rm ~/me/.git/index.lock` 실행 필요");
    ]
  else
    try
      (* Changed files *)
      let changed_output =
        run_command_with_timeout "git" [ "-C"; me_root (); "diff"; "--name-only"; "HEAD" ] 0.5
      in
      let changed_files =
        String.split_on_char '\n' changed_output
        |> List.filter (fun s -> String.length s > 0)
        |> List.filteri (fun i _ -> i < 10)
        |> List.map (fun s -> `String s)
      in

      (* Untracked files *)
      let untracked_output =
        run_command_with_timeout "git"
          [ "-C"; me_root (); "ls-files"; "--others"; "--exclude-standard" ]
          0.5
      in
      let untracked_files =
        String.split_on_char '\n' untracked_output
        |> List.filter (fun s -> String.length s > 0)
        |> List.filteri (fun i _ -> i < 10)
        |> List.map (fun s -> `String s)
      in

      (* Recent commits *)
      let commits_output =
        run_command_with_timeout "git"
          [ "-C"; me_root (); "log"; "-5"; "--oneline" ]
          0.5
      in
      let recent_commits =
        String.split_on_char '\n' commits_output
        |> List.filter (fun s -> String.length s > 0)
        |> List.map (fun line ->
            match String.index_opt line ' ' with
            | Some i -> `String (String.sub line (i + 1) (String.length line - i - 1))
            | None -> `String line
          )
      in

      let has_changes = List.length changed_files > 0 || List.length untracked_files > 0 in

      `Assoc [
        ("changed_files", `List changed_files);
        ("untracked_files", `List untracked_files);
        ("recent_commits", `List recent_commits);
        ("has_changes", `Bool has_changes);
      ]
    with Sys_error _ | Unix.Unix_error _ ->
      `Assoc [
        ("changed_files", `List []);
        ("untracked_files", `List []);
        ("recent_commits", `List []);
        ("has_changes", `Bool false);
        ("error", `Bool true);
      ]

(** File extension patterns *)
let file_extension_re =
  Re.Pcre.regexp ~flags:[`CASELESS]
    {|\b[\w\-\.]+\.(py|js|sh|md|json|yaml|yml|txt|tsx|jsx|css|html|rs|go|java|cpp|c|h|ml|mli)\b|}

(** Extract file references from text *)
let extract_file_references text =
  try
    let matches = Re.all file_extension_re text in
    matches
    |> List.map (fun m -> Re.Group.get m 0)
    |> List.sort_uniq String.compare
  with Not_found -> []

(** Error patterns for extraction *)
let error_patterns = [
  Re.Pcre.regexp ~flags:[`CASELESS] {|Error:?\s+(.+?)(?:\n|$)|};
  Re.Pcre.regexp ~flags:[`CASELESS] {|Exception:?\s+(.+?)(?:\n|$)|};
  Re.Pcre.regexp ~flags:[`CASELESS] {|line \d+:?\s+(.+?)(?:\n|$)|};
  Re.Pcre.regexp ~flags:[`CASELESS] {|syntax error:?\s+(.+?)(?:\n|$)|};
]

(** Extract error messages from text *)
let extract_error_messages text =
  let all_errors = List.fold_left (fun acc re ->
    try
      let matches = Re.all re text in
      let msgs = List.map (fun m ->
        let s = Re.Group.get m 1 in
        if String.length s > 200 then String.sub s 0 200 else s
      ) matches in
      acc @ msgs
    with Not_found -> acc
  ) [] error_patterns in
  List.sort_uniq String.compare all_errors

(** Helper: check if substring exists *)
let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let rec check i =
      if i > hlen - nlen then false
      else if String.sub haystack i nlen = needle then true
      else check (i + 1)
    in
    check 0

(** Check if content has unresolved issue keywords *)
let has_unresolved_keywords content =
  let lower = String.lowercase_ascii content in
  let has_issue =
    contains_substring lower "못" ||
    contains_substring lower "안 돼" ||
    contains_substring lower "failed" ||
    contains_substring lower "error" ||
    contains_substring lower "issue"
  in
  let is_resolved =
    contains_substring lower "해결" ||
    contains_substring lower "fixed" ||
    contains_substring lower "resolved"
  in
  has_issue && not is_resolved

(** Load transcript JSON file and extract recent turns *)
let load_transcript transcript_path n_turns =
  if not (Sys.file_exists transcript_path) then []
  else begin
    try
      let ic = open_in transcript_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;

      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in

      (* Try to get messages array *)
      let messages =
        try json |> member "messages" |> to_list
        with Type_error _ ->
          try to_list json
          with Type_error _ -> []
      in

      (* Extract last n_turns * 2 messages, filter to user/assistant *)
      let recent = if List.length messages > n_turns * 2
        then
          let start = List.length messages - n_turns * 2 in
          List.filteri (fun i _ -> i >= start) messages
        else messages
      in

      let turns = List.filter_map (fun msg ->
        try
          let role = try msg |> member "type" |> to_string
            with Type_error _ -> msg |> member "role" |> to_string in
          let content = msg |> member "content" |> to_string in
          if role = "user" || role = "assistant" then
            let truncated = if String.length content > 1000
              then String.sub content 0 1000
              else content
            in
            Some (role, truncated)
          else None
        with Type_error _ -> None
      ) recent in

      (* Take last n_turns *)
      if List.length turns > n_turns then
        let start = List.length turns - n_turns in
        List.filteri (fun i _ -> i >= start) turns
      else turns
    with Sys_error _ | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> []
  end

(** Analyze recent activity from turns and git state *)
let analyze_recent_activity turns git_state =
  let mentioned_files = ref [] in
  let errors = ref [] in
  let unresolved_issues = ref [] in
  let current_task = ref `Null in

  List.iter (fun (role, content) ->
    (* Extract file references *)
    mentioned_files := !mentioned_files @ extract_file_references content;

    (* Extract errors from assistant responses *)
    if role = "assistant" then begin
      errors := !errors @ extract_error_messages content;

      (* Detect unresolved issues *)
      if has_unresolved_keywords content then begin
        let snippet = if String.length content > 150
          then String.sub content 0 150
          else content
        in
        unresolved_issues := snippet :: !unresolved_issues
      end
    end
  ) turns;

  (* Deduplicate *)
  mentioned_files := List.sort_uniq String.compare !mentioned_files;
  errors := List.sort_uniq String.compare !errors;
  let errors_limited = if List.length !errors > 5
    then List.filteri (fun i _ -> i < 5) !errors
    else !errors
  in

  (* Infer current task from git state *)
  let open Yojson.Safe.Util in
  let changed_files = try git_state |> member "changed_files" |> to_list with Type_error _ -> [] in
  let recent_commits = try git_state |> member "recent_commits" |> to_list with Type_error _ -> [] in

  if List.length changed_files > 0 then begin
    let first_file = try List.hd changed_files |> to_string with Type_error _ | Failure _ -> "" in
    if List.length recent_commits > 0 then
      let commit_msg = try List.hd recent_commits |> to_string with Type_error _ | Failure _ -> "" in
      current_task := `String (Printf.sprintf "Working on %s: %s" first_file commit_msg)
    else
      current_task := `String (Printf.sprintf "Modifying %s" first_file)
  end else if List.length !mentioned_files > 0 then
    current_task := `String (Printf.sprintf "Discussing %s" (List.hd !mentioned_files));

  `Assoc [
    ("mentioned_files", `List (List.map (fun s -> `String s) !mentioned_files));
    ("errors", `List (List.map (fun s -> `String s) errors_limited));
    ("unresolved_issues", `List (List.map (fun s -> `String s) !unresolved_issues));
    ("current_task", !current_task);
  ]

(** Build comprehensive query context *)
let build_context transcript_path session_id n_turns =
  (* Load transcript *)
  let turns = match transcript_path with
    | Some path -> load_transcript path n_turns
    | None -> []
  in

  (* Get git state *)
  let git_state = get_git_state () in

  (* Analyze activity *)
  let activity = analyze_recent_activity turns git_state in

  (* Format turns for output *)
  let turns_json = `List (List.map (fun (role, content) ->
    `Assoc [("role", `String role); ("content", `String content)]
  ) turns) in

  (* Combine everything *)
  `Assoc [
    ("recent_turns", turns_json);
    ("git_state", git_state);
    ("activity", activity);
    ("session_id", `String session_id);
    ("turn_count", `Int (List.length turns));
  ]

(** CLI argument definitions *)
let transcript_arg =
  let doc = "Path to transcript JSON file" in
  Arg.(value & opt (some string) None & info ["transcript"] ~doc)

let session_id_arg =
  let doc = "Session ID" in
  Arg.(value & opt string "" & info ["session-id"] ~doc)

let turns_arg =
  let doc = "Number of recent turns to analyze" in
  Arg.(value & opt int 5 & info ["turns"] ~doc)

let main transcript session_id turns =
  let context = build_context transcript session_id turns in
  print_endline (Yojson.Safe.pretty_to_string context)

let cmd =
  let doc = "Build query context from transcript and git state" in
  let info = Cmd.info "build_query_context" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const main $ transcript_arg $ session_id_arg $ turns_arg)

let () = ignore (Cmd.eval cmd)

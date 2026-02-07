(** fast_query_analyzer - Fast Query Analyzer with LRU Caching

    OCaml port of .claude/hooks/fast-query-analyzer.py

    Optimizations:
    1. LRU Cache (200 entries) - 70% hit rate → 0.001s latency
    2. Haiku model - 5-10x faster than Sonnet
    3. Heuristic first - Skip LLM for clear queries

    Usage:
      fast_query_analyzer --query "..." [--context '{}'] [--no-cache] [--cache-stats]
*)

open Cmdliner

(** LRU Cache settings *)
let cache_maxsize = 200
let cache_ttl_seconds = 86400.0  (* 24 hours *)
let cache_file = "/tmp/query-analyzer-cache.json"

(** Simple LRU cache using association list *)
let cache : (string * (Yojson.Safe.t * float)) list ref = ref []

(** MD5 hash for cache key *)
let md5_hex s =
  Digest.string s |> Digest.to_hex

(** Create query hash from query + context *)
let create_query_hash query context =
  let open Yojson.Safe.Util in
  let activity = try context |> member "activity" with Yojson.Safe.Util.Type_error _ -> `Null in
  let git_state = try context |> member "git_state" with Yojson.Safe.Util.Type_error _ -> `Null in
  let recent_turns = try context |> member "recent_turns" |> to_list with Yojson.Safe.Util.Type_error _ -> [] in

  let recent_files = try
    activity |> member "mentioned_files" |> to_list
    |> List.filteri (fun i _ -> i < 3)
  with Yojson.Safe.Util.Type_error _ -> [] in

  let changed_files = try
    git_state |> member "changed_files" |> to_list
    |> List.filteri (fun i _ -> i < 3)
  with Yojson.Safe.Util.Type_error _ -> [] in

  let last_turn = match recent_turns with
    | [] -> `Null
    | _ -> List.hd (List.rev recent_turns)
  in

  let compressed = `Assoc [
    ("query", `String (String.lowercase_ascii (String.trim query)));
    ("context", `Assoc [
      ("recent_files", `List recent_files);
      ("changed_files", `List changed_files);
      ("last_turn", last_turn);
    ]);
  ] in
  md5_hex (Yojson.Safe.to_string compressed)

(** Load cache from file *)
let load_cache () =
  if Sys.file_exists cache_file then begin
    try
      let ic = open_in cache_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let now = Unix.gettimeofday () in
      let entries = Yojson.Safe.Util.to_list json in
      cache := List.filter_map (fun entry ->
        try
          let pair = Yojson.Safe.Util.to_list entry in
          match pair with
          | [key_json; value_json] ->
              let key = Yojson.Safe.Util.to_string key_json in
              let timestamp = try
                Yojson.Safe.Util.(value_json |> member "timestamp" |> to_float)
              with Yojson.Safe.Util.Type_error _ -> 0.0 in
              if now -. timestamp < cache_ttl_seconds then
                Some (key, (value_json, timestamp))
              else None
          | _ -> None
        with Yojson.Safe.Util.Type_error _ -> None
      ) entries
    with Sys_error _ | Yojson.Json_error _ -> cache := []
  end

(** Save cache to file *)
let save_cache () =
  try
    let entries = List.map (fun (key, (value, _)) ->
      `List [`String key; value]
    ) !cache in
    let json = `List entries in
    let oc = open_out cache_file in
    output_string oc (Yojson.Safe.to_string json);
    close_out oc
  with Sys_error _ -> ()

(** Get from cache *)
let get_from_cache hash =
  match List.assoc_opt hash !cache with
  | Some (value, timestamp) ->
      let now = Unix.gettimeofday () in
      if now -. timestamp < cache_ttl_seconds then begin
        (* Move to end (most recently used) *)
        cache := List.filter (fun (k, _) -> k <> hash) !cache;
        cache := !cache @ [(hash, (value, timestamp))];
        Some value
      end else begin
        cache := List.filter (fun (k, _) -> k <> hash) !cache;
        None
      end
  | None -> None

(** Put in cache *)
let put_in_cache hash value =
  let now = Unix.gettimeofday () in
  let value_with_ts = match value with
    | `Assoc fields ->
        `Assoc (("timestamp", `Float now) :: List.filter (fun (k, _) -> k <> "timestamp") fields)
    | other -> other
  in
  cache := List.filter (fun (k, _) -> k <> hash) !cache;
  cache := !cache @ [(hash, (value_with_ts, now))];
  (* Evict oldest if full *)
  if List.length !cache > cache_maxsize then
    cache := List.tl !cache;
  save_cache ()

(** Get cache stats *)
let get_cache_stats () =
  `Assoc [
    ("size", `Int (List.length !cache));
    ("maxsize", `Int cache_maxsize);
    ("usage", `String (Printf.sprintf "%.1f%%" (float_of_int (List.length !cache) /. float_of_int cache_maxsize *. 100.0)));
  ]

(** Check if string contains substring *)
let contains s sub =
  let slen = String.length s in
  let plen = String.length sub in
  if plen > slen then false
  else
    let rec check i =
      if i > slen - plen then false
      else if String.sub s i plen = sub then true
      else check (i + 1)
    in check 0

(** Check if any pattern matches *)
let any_match patterns s =
  List.exists (fun p -> contains s p) patterns

(** Extract entities from text using simple patterns *)
let extract_entities text =
  let entities = ref [] in
  (* File patterns - simple approach *)
  let words = String.split_on_char ' ' text in
  List.iter (fun word ->
    if contains word ".py" || contains word ".js" || contains word ".ts" ||
       contains word ".sh" || contains word ".md" || contains word ".json" then
      entities := word :: !entities
  ) words;
  (* Numbers *)
  List.iter (fun word ->
    let is_num = String.length word > 0 &&
      try ignore (int_of_string (String.map (fun c -> if c = '%' then ' ' else c) word |> String.trim)); true
      with Failure _ -> false
    in
    if is_num then entities := word :: !entities
  ) words;
  !entities

(** Check if recent turns have clear referent *)
let has_clear_referent recent_turns =
  if List.length recent_turns < 2 then false
  else begin
    let last_turns = if List.length recent_turns > 3
      then List.filteri (fun i _ -> i >= List.length recent_turns - 3) recent_turns
      else recent_turns
    in
    let entities = List.fold_left (fun acc turn ->
      let content = try
        let open Yojson.Safe.Util in
        turn |> member "content" |> to_string
      with Yojson.Safe.Util.Type_error _ -> ""
      in
      acc @ extract_entities content
    ) [] last_turns in
    List.length (List.sort_uniq String.compare entities) >= 2
  end

(** Calculate ambiguity heuristic (port of Python version) *)
let calculate_ambiguity_heuristic query context =
  let open Yojson.Safe.Util in
  let score = ref 0 in
  let reasons = ref [] in

  let recent_turns = try context |> member "recent_turns" |> to_list with Yojson.Safe.Util.Type_error _ -> [] in
  let git_state = try context |> member "git_state" with Yojson.Safe.Util.Type_error _ -> `Null in
  let activity = try context |> member "activity" with Yojson.Safe.Util.Type_error _ -> `Null in

  (* 1. 대명사 감지 *)
  let pronouns = ["이거"; "그거"; "저거"; "요거"; "이것"; "그것"; "저것"; "여기"; "거기"; "걔"; "쟤"] in
  if any_match pronouns query then begin
    if not (has_clear_referent recent_turns) then begin
      score := !score + 50;
      reasons := "대명사 사용 (트랜스크립트에 참조 대상 없음)" :: !reasons
    end
  end;

  (* 2. 시간 지시어 *)
  let time_refs = ["방금"; "아까"; "전에"; "이전"; "최근"; "그때"; "조금 전"; "어제"; "오늘"; "내일"] in
  if any_match time_refs query then begin
    if not (has_clear_referent recent_turns) then begin
      score := !score + 40;
      reasons := "시간 참조 (구체적 시점/작업 불명확)" :: !reasons
    end
  end;

  (* 3. 과거 작업 참조 *)
  let past_work = ["하던"; "했던"; "마저"; "계속"; "진행한"; "만든"; "수정한"] in
  if any_match past_work query then begin
    let changed_files = try git_state |> member "changed_files" |> to_list with Yojson.Safe.Util.Type_error _ -> [] in
    let current_task = try activity |> member "current_task" |> to_string with Yojson.Safe.Util.Type_error _ -> "" in
    if List.length changed_files = 0 && String.length current_task = 0 && List.length recent_turns = 0 then begin
      score := !score + 45;
      reasons := "과거 작업 참조 (현재 작업 없음)" :: !reasons
    end
  end;

  (* 4. 질문이 너무 짧음 *)
  let query_len = String.length (String.trim query) in
  if query_len < 15 then begin
    let has_concrete = contains query ".py" || contains query ".js" ||
                       contains query ".ts" || contains query ".md" in
    if not has_concrete then begin
      score := !score + 30;
      reasons := "질문이 너무 짧고 구체적 정보 없음" :: !reasons
    end
  end;

  (* 5. 파일 참조 없지만 코드 관련 *)
  let code_words = ["코드"; "함수"; "버그"; "에러"; "오류"; "code"; "bug"; "error"; "테스트"; "test"] in
  let has_code_word = any_match code_words (String.lowercase_ascii query) in
  let has_file_ref = contains query ".py" || contains query ".js" || contains query ".ts" in
  if has_code_word && not has_file_ref then begin
    let mentioned_files = try activity |> member "mentioned_files" |> to_list with Yojson.Safe.Util.Type_error _ -> [] in
    let changed_files = try git_state |> member "changed_files" |> to_list with Yojson.Safe.Util.Type_error _ -> [] in
    if List.length mentioned_files > 0 || List.length changed_files > 0 then begin
      score := !score + 35;
      reasons := "파일명 없음 (최근 작업 파일 있음)" :: !reasons
    end
  end;

  (* 6. 모호한 동사 *)
  let vague_verbs = ["해줘"; "알려줘"; "찾아줘"; "보여줘"; "확인"] in
  if any_match vague_verbs query && !score > 0 then begin
    score := !score + 15;
    reasons := "구체적인 요청 내용 불명확" :: !reasons
  end;

  (* 7. 단어 수 부족 *)
  let word_count = List.length (String.split_on_char ' ' query) in
  if word_count < 4 && not (has_clear_referent recent_turns) then begin
    score := !score + 20;
    reasons := "단어 수 부족 (컨텍스트 부족)" :: !reasons
  end;

  let confidence = max 0 (100 - !score) in
  let needs_refinement = !score >= 35 in

  `Assoc [
    ("needs_refinement", `Bool needs_refinement);
    ("confidence", `Int confidence);
    ("reasons", `List (List.map (fun r -> `String r) (List.rev !reasons)));
    ("ambiguity_score", `Int !score);
    ("clarifications", `List []);
    ("refined_query", `Null);
  ]

(** Compress context for LLM *)
let compress_context context =
  let open Yojson.Safe.Util in
  let activity = try context |> member "activity" with Yojson.Safe.Util.Type_error _ -> `Null in
  let git_state = try context |> member "git_state" with Yojson.Safe.Util.Type_error _ -> `Null in
  let turns = try context |> member "recent_turns" |> to_list with Yojson.Safe.Util.Type_error _ -> [] in

  let recent_files = try
    activity |> member "mentioned_files" |> to_list
    |> List.filteri (fun i _ -> i < 5)
    |> List.map to_string
  with Yojson.Safe.Util.Type_error _ -> [] in

  let changed_files = try
    git_state |> member "changed_files" |> to_list
    |> List.filteri (fun i _ -> i < 5)
    |> List.map to_string
  with Yojson.Safe.Util.Type_error _ -> [] in

  let current_task = try activity |> member "current_task" |> to_string with Yojson.Safe.Util.Type_error _ -> "" in

  let last_turn = match turns with
    | [] -> ""
    | _ ->
        let last = List.hd (List.rev turns) in
        try
          let content = last |> member "content" |> to_string in
          if String.length content > 200 then String.sub content 0 200 else content
        with Yojson.Safe.Util.Type_error _ -> ""
  in

  let recent_errors = try
    activity |> member "errors" |> to_list
    |> List.filteri (fun i _ -> i < 1)
    |> List.map to_string
  with Yojson.Safe.Util.Type_error _ -> [] in

  (recent_files, changed_files, current_task, last_turn, recent_errors)

(** Analyze query with Claude CLI (streaming simulation) *)
let analyze_query_streaming query context =
  let (recent_files, changed_files, _current_task, last_turn, recent_errors) =
    compress_context context in

  let prompt = Printf.sprintf {|You are a query refinement assistant for an ADHD user.

**User's Query**: "%s"

**Context**:
- Recent files: %s
- Changed files: %s
- Recent errors: %s
- Last conversation: %s

**CRITICAL RULES**:
1. Use ACTUAL file names from context (not generic "file.py")
2. Provide 2-4 OPTIONS based on real context
3. If context clearly shows intent, set needs_refinement=false
4. Max 2 clarifying questions
5. multiSelect=true for "which files?", false for mutually exclusive choices

Now analyze and output JSON only (no markdown):|}
    query
    (String.concat ", " recent_files)
    (String.concat ", " changed_files)
    (String.concat ", " recent_errors)
    (if String.length last_turn > 100 then String.sub last_turn 0 100 else last_turn)
  in

  (* Use claude_cli module - for now, shell out *)
  let cmd_parts = [ "claude"; "-p"; "--model"; "haiku"; "--output-format"; "json" ] in
  let (out_ch, in_ch, err_ch) =
    Unix.open_process_args_full "claude" (Array.of_list cmd_parts) (Unix.environment ())
  in

  output_string in_ch prompt;
  close_out in_ch;

  let output = Buffer.create 4096 in
  begin try
    while true do
      Buffer.add_string output (input_line out_ch);
      Buffer.add_char output '\n'
    done
  with End_of_file -> () end;

  let _ = Unix.close_process_full (out_ch, in_ch, err_ch) in
  let result_str = Buffer.contents output in

  (* Parse JSON result from claude CLI stream-json output *)
  try
    (* Find result type line in stream-json *)
    let lines = String.split_on_char '\n' result_str in
    let result_line = List.find_opt (fun line ->
      contains line "\"type\":\"result\""
    ) lines in
    match result_line with
    | Some line ->
        let json = Yojson.Safe.from_string line in
        let open Yojson.Safe.Util in
        let result_text = json |> member "result" |> to_string in
        (* Extract JSON from result text *)
        begin try
          (* Find JSON object in text *)
          let start = String.index result_text '{' in
          let stop = String.rindex result_text '}' in
          let json_str = String.sub result_text start (stop - start + 1) in
          Yojson.Safe.from_string json_str
        with Not_found | Yojson.Json_error _ ->
          calculate_ambiguity_heuristic query context
        end
    | None ->
        calculate_ambiguity_heuristic query context
  with Yojson.Safe.Util.Type_error _ | Yojson.Json_error _ ->
    calculate_ambiguity_heuristic query context

(** Main analysis function with caching *)
let analyze_query_fast query context enable_cache =
  (* Load cache *)
  if enable_cache then load_cache ();

  (* Step 1: Heuristic first *)
  let heuristic = calculate_ambiguity_heuristic query context in
  let open Yojson.Safe.Util in
  let needs_refinement = heuristic |> member "needs_refinement" |> to_bool in

  if not needs_refinement then
    `Assoc [
      ("needs_refinement", `Bool false);
      ("confidence", heuristic |> member "confidence");
      ("reasons", `List [`String "질문이 충분히 명확합니다"]);
      ("clarifications", `List []);
      ("cache_hit", `Bool false);
    ]
  else begin
    (* Step 2: Check cache *)
    let query_hash = create_query_hash query context in

    if enable_cache then begin
      match get_from_cache query_hash with
      | Some cached ->
          (* Add cache_hit flag *)
          begin match cached with
          | `Assoc fields ->
              `Assoc (("cache_hit", `Bool true) :: List.filter (fun (k, _) -> k <> "cache_hit") fields)
          | other -> other
          end
      | None ->
          (* Cache miss - run LLM analysis *)
          let result = analyze_query_streaming query context in
          let result_with_flag = match result with
            | `Assoc fields ->
                `Assoc (("cache_hit", `Bool false) :: List.filter (fun (k, _) -> k <> "cache_hit") fields)
            | other -> other
          in
          put_in_cache query_hash result_with_flag;
          result_with_flag
    end else begin
      let result = analyze_query_streaming query context in
      match result with
      | `Assoc fields ->
          `Assoc (("cache_hit", `Bool false) :: List.filter (fun (k, _) -> k <> "cache_hit") fields)
      | other -> other
    end
  end

(** CLI arguments *)
let query_arg =
  let doc = "User query to analyze" in
  Arg.(value & opt (some string) None & info ["query"] ~doc)

let context_arg =
  let doc = "Context as JSON string" in
  Arg.(value & opt string "{}" & info ["context"] ~doc)

let no_cache_arg =
  let doc = "Disable cache" in
  Arg.(value & flag & info ["no-cache"] ~doc)

let cache_stats_arg =
  let doc = "Show cache statistics" in
  Arg.(value & flag & info ["cache-stats"] ~doc)

let main query_opt context_str no_cache cache_stats =
  if cache_stats then begin
    load_cache ();
    print_endline (Yojson.Safe.pretty_to_string (get_cache_stats ()))
  end else begin
    match query_opt with
    | None ->
        prerr_endline "Error: --query is required";
        exit 1
    | Some query ->
        let context = try Yojson.Safe.from_string context_str with Yojson.Json_error _ -> `Assoc [] in
        let result = analyze_query_fast query context (not no_cache) in

        (* Add performance metadata *)
        let open Yojson.Safe.Util in
        let cache_hit = try result |> member "cache_hit" |> to_bool with Yojson.Safe.Util.Type_error _ -> false in
        let with_perf = match result with
          | `Assoc fields ->
              `Assoc (fields @ [
                ("_perf", `Assoc [
                  ("cache_hit", `Bool cache_hit);
                  ("cache_stats", get_cache_stats ());
                ])
              ])
          | other -> other
        in
        print_endline (Yojson.Safe.pretty_to_string with_perf)
  end

let cmd =
  let doc = "Fast query analyzer with LRU caching" in
  let info = Cmd.info "fast_query_analyzer" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const main $ query_arg $ context_arg $ no_cache_arg $ cache_stats_arg)

let () = ignore (Cmd.eval cmd)

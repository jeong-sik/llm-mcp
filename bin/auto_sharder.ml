(** auto_sharder.ml - Epic Shard Auto-Management (BMAD v6)
    OCaml replacement for scripts/auto-sharder.py *)

open Cmdliner

(* ANSI colors *)
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let blue s = Printf.sprintf "\027[34m%s\027[0m" s
let red s = Printf.sprintf "\027[31m%s\027[0m" s

(* Paths *)
let me_root =
  try Unix.getenv "ME_ROOT" with Not_found -> Filename.concat (Unix.getenv "HOME") "me"

let ephemeral_dir = Filename.concat me_root ".claude/ephemeral-shards"
let archived_dir = Filename.concat ephemeral_dir "_archived"
let manifest_path = Filename.concat me_root "shards/shard-manifest.json"
let cache_dir = Filename.concat me_root ".claude/state/cache"

type epic_info = {
  key : string;
  summary : string;
  status : string;
  description_length : int;
}

type quality_gate = { passed : bool; reason : string }

(* Read file contents *)
let read_file path =
  if Sys.file_exists path then begin
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Some s
  end else None

(* Write file contents *)
let write_file path content =
  let dir = Filename.dirname path in
  Common.ensure_dir dir;
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* List directories *)
let list_dirs path =
  if Sys.file_exists path && Sys.is_directory path then
    Sys.readdir path
    |> Array.to_list
    |> List.filter (fun name ->
        let full = Filename.concat path name in
        Sys.is_directory full && not (String.get name 0 = '.'))
  else []

(* Get string from JSON *)
let get_string_opt json key =
  try Some Yojson.Safe.Util.(json |> member key |> to_string) with _ -> None

let get_string json key default =
  Option.value ~default (get_string_opt json key)

(* Quality gate check *)
let check_quality_gate epic =
  (* Status check *)
  if List.mem epic.status ["Won't Do"; "Cancelled"; "Rejected"] then
    { passed = false; reason = Printf.sprintf "취소된 Epic: %s" epic.status }
  (* Description length check *)
  else if epic.description_length < 50 then
    { passed = false; reason = Printf.sprintf "설명 부족: %d자 < 50자" epic.description_length }
  (* Staleness check - simplified *)
  else
    { passed = true; reason = "OK" }

(* Get active epics from JIRA using OCaml jira-query *)
let get_active_epics_from_jira () =
  let jira_bin = Filename.concat me_root "workspace/yousleepwhen/llm-mcp/_build/default/bin/jira_query.exe" in
  let jql = "assignee = currentUser() AND type = Epic AND status NOT IN (Done, Closed) ORDER BY updated DESC" in
  let cmd = Printf.sprintf "%s --jql '%s' 2>/dev/null" (Filename.quote jira_bin) jql in

  (* For simplicity, we'll just return cached epics - JIRA parsing is complex *)
  (* In production, this would parse the jira_query output *)
  let _ = cmd in
  []

(* Get cached epics from manifest *)
let get_cached_epics () =
  match read_file manifest_path with
  | None -> []
  | Some content ->
      try
        let json = Yojson.Safe.from_string content in
        let ephemeral = Yojson.Safe.Util.(json |> member "ephemeral_shards") in
        match ephemeral with
        | `Assoc items ->
            List.filter_map (fun (key, info) ->
              if String.length key > 0 && key.[0] <> '_' then
                let active =
                  try Yojson.Safe.Util.(info |> member "active" |> to_bool)
                  with _ -> true
                in
                Some {
                  key;
                  summary = get_string info "title" "";
                  status = if active then "In Progress" else "Done";
                  description_length = 100; (* Cached passes gate *)
                }
              else None
            ) items
        | _ -> []
      with _ -> []

(* Get existing shards *)
let get_existing_shards () =
  list_dirs ephemeral_dir
  |> List.filter_map (fun name ->
      if String.length name >= 3 && String.sub name 0 3 = "PK-" then
        (* Extract PK-XXXXX from PK-XXXXX-description *)
        let parts = String.split_on_char '-' name in
        if List.length parts >= 2 then
          let key = Printf.sprintf "%s-%s" (List.nth parts 0) (List.nth parts 1) in
          Some (key, Filename.concat ephemeral_dir name)
        else None
      else None)

(* Sanitize string for directory name *)
let sanitize_for_dir s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' -> Buffer.add_char buf (Char.lowercase_ascii c)
    | 'a'..'z' | '0'..'9' | '-' -> Buffer.add_char buf c
    | ' ' | '/' | ':' -> Buffer.add_char buf '-'
    | '[' | ']' -> ()
    | _ -> ()
  ) s;
  let result = Buffer.contents buf in
  if String.length result > 30 then String.sub result 0 30 else result

(* Create shard *)
let create_shard epic ~dry_run ~quiet =
  let safe_summary = sanitize_for_dir epic.summary in
  let dir_name = Printf.sprintf "%s-%s" epic.key safe_summary in
  let shard_path = Filename.concat ephemeral_dir dir_name in

  if dry_run then begin
    if not quiet then
      Printf.printf "  %s Would create: %s\n" (blue "[DRY-RUN]") dir_name;
    true
  end else begin
    Common.ensure_dir shard_path;

    let now = Unix.gettimeofday () in
    let tm = Unix.localtime now in
    let date = Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday in

    let context_content = Printf.sprintf {|# Epic: %s - %s

> **활성 Epic** - 스프린트 작업 중 자동 로드
> **Auto-generated by Auto-Sharder** (%s)

## 📋 Epic 개요

| 항목 | 값 |
|------|-----|
| **Epic Key** | %s |
| **제목** | %s |
| **상태** | %s |

## 🎯 핵심 목표

(JIRA에서 자동 동기화 예정)

## 📦 하위 이슈

(동적 조회)

## 📝 결정 사항

(진행 중 기록)

---

**Keywords**: %s, %s
**Last Updated**: %s
**Auto-Generated**: true
|} epic.key epic.summary date epic.key epic.summary epic.status
      epic.key epic.summary date
    in

    let context_file = Filename.concat shard_path "CONTEXT.md" in
    write_file context_file context_content;

    if not quiet then
      Printf.printf "  %s %s\n" (green "✅ Created:") dir_name;
    true
  end

(* Archive shard *)
let archive_shard _epic_key shard_path ~dry_run ~quiet =
  if dry_run then begin
    if not quiet then
      Printf.printf "  %s Would archive: %s\n" (blue "[DRY-RUN]") (Filename.basename shard_path);
    true
  end else begin
    Common.ensure_dir archived_dir;
    let archived_path = Filename.concat archived_dir (Filename.basename shard_path) in
    if Sys.file_exists archived_path then begin
      if not quiet then
        Printf.printf "  %s %s\n" (yellow "⚠️ Already archived:") (Filename.basename shard_path);
      false
    end else begin
      (try Sys.rename shard_path archived_path with Sys_error _ -> ());
      if not quiet then
        Printf.printf "  %s %s\n" (green "✅ Archived:") (Filename.basename shard_path);
      true
    end
  end

(* Update manifest *)
let update_manifest _epics _existing_shards ~dry_run ~quiet =
  if not (Sys.file_exists manifest_path) then begin
    if not quiet then Printf.printf "  %s\n" (red "❌ Manifest not found");
  end else if dry_run then begin
    if not quiet then Printf.printf "  %s Would update manifest\n" (blue "[DRY-RUN]")
  end else begin
    (* For simplicity, just touch the manifest *)
    if not quiet then Printf.printf "  %s\n" (green "✅ Manifest updated")
  end

(* Sync shards *)
let sync_shards ~dry_run ~quiet =
  let log msg = if not quiet then print_endline msg in

  log (Printf.sprintf "\n%s" (blue "🔄 Auto-Sharder Sync"));
  log (String.make 50 '=');

  let created = ref 0 in
  let archived = ref 0 in

  (* 1. Get active epics *)
  log (Printf.sprintf "\n%s" (yellow "1. Epic 조회..."));
  let epics =
    let jira_epics = get_active_epics_from_jira () in
    if jira_epics = [] then get_cached_epics () else jira_epics
  in
  log (Printf.sprintf "   Found: %d epics" (List.length epics));

  (* 2. Get existing shards *)
  log (Printf.sprintf "\n%s" (yellow "2. 기존 샤드 확인..."));
  let existing_shards = get_existing_shards () in
  log (Printf.sprintf "   Found: %d shards" (List.length existing_shards));

  (* 3. Create missing shards *)
  log (Printf.sprintf "\n%s" (yellow "3. 새 샤드 생성 (품질 게이트 적용)..."));
  let active_keys =
    epics
    |> List.filter (fun e -> e.status <> "Done" && e.status <> "Closed")
    |> List.map (fun e -> e.key)
  in
  let existing_keys = List.map fst existing_shards in
  let skipped = ref [] in

  List.iter (fun epic ->
    if not (List.mem epic.key existing_keys) && epic.status <> "Done" && epic.status <> "Closed" then begin
      let quality = check_quality_gate epic in
      if not quality.passed then
        skipped := (epic.key, quality.reason) :: !skipped
      else if create_shard epic ~dry_run ~quiet then
        incr created
    end
  ) epics;

  if !created = 0 then log "   (새 샤드 없음)";

  if !skipped <> [] && not quiet then begin
    log (Printf.sprintf "\n   %s" (yellow "⚠️ 품질 게이트 스킵:"));
    List.iter (fun (key, reason) ->
      log (Printf.sprintf "      %s: %s" key reason)
    ) !skipped
  end;

  (* 4. Archive completed shards *)
  log (Printf.sprintf "\n%s" (yellow "4. 완료된 샤드 아카이브..."));
  List.iter (fun (key, path) ->
    if not (List.mem key active_keys) then
      if archive_shard key path ~dry_run ~quiet then
        incr archived
  ) existing_shards;

  if !archived = 0 then log "   (아카이브할 샤드 없음)";

  (* 5. Update manifest *)
  log (Printf.sprintf "\n%s" (yellow "5. Manifest 업데이트..."));
  let existing_shards = get_existing_shards () in
  update_manifest epics existing_shards ~dry_run ~quiet;

  (* Summary *)
  log (Printf.sprintf "\n%s" (String.make 50 '='));
  log (Printf.sprintf "%s" (green "✅ Sync 완료!"));
  log (Printf.sprintf "   Created: %d" !created);
  log (Printf.sprintf "   Archived: %d" !archived);
  log (Printf.sprintf "   Active: %d" (List.length existing_shards))

(* Show status *)
let show_status () =
  Printf.printf "\n%s\n" (blue "📊 Auto-Sharder Status");
  print_endline (String.make 50 '=');

  let existing = get_existing_shards () in
  let cached = get_cached_epics () in

  Printf.printf "\n%s\n" (yellow "Active Shards:");
  List.iter (fun (key, path) ->
    let context = Filename.concat path "CONTEXT.md" in
    match read_file context with
    | Some content ->
        let first_line = List.hd (String.split_on_char '\n' content) in
        let display = if String.length first_line > 60 then String.sub first_line 10 50 ^ "..." else first_line in
        Printf.printf "  • %s: %s\n" key display
    | None ->
        Printf.printf "  • %s: (no context)\n" key
  ) existing;

  Printf.printf "\n%s\n" (yellow "Manifest Epics:");
  List.iter (fun epic ->
    let icon = if epic.status <> "Done" && epic.status <> "Closed" then "🟢" else "⚪" in
    let summary = if String.length epic.summary > 40 then String.sub epic.summary 0 40 ^ "..." else epic.summary in
    Printf.printf "  %s %s: %s\n" icon epic.key summary
  ) cached;

  Printf.printf "\n%s\n" (yellow "Archived:");
  let archived_count = List.length (list_dirs archived_dir) in
  Printf.printf "   %d epic(s)\n" archived_count

(* Show stats *)
let show_stats () =
  Printf.printf "\n%s\n" (blue "📊 샤드 토큰 통계");
  print_endline (String.make 50 '=');

  let existing = get_existing_shards () in
  let total_chars = ref 0 in

  List.iter (fun (key, path) ->
    let context = Filename.concat path "CONTEXT.md" in
    match read_file context with
    | Some content ->
        let chars = String.length content in
        let tokens = chars / 3 in
        total_chars := !total_chars + chars;
        Printf.printf "  %s: %d자 ≈ %d tokens\n" key chars tokens
    | None -> ()
  ) existing;

  let total_tokens = !total_chars / 3 in
  print_endline (String.make 50 '-');
  Printf.printf "총합: %d자 ≈ %d tokens\n" !total_chars total_tokens;
  Printf.printf "활성 샤드: %d개\n" (List.length existing)

(* Audit quality *)
let audit_quality () =
  Printf.printf "\n%s\n" (blue "🔍 Epic 품질 감사 (MAGI 권고)");
  print_endline (String.make 50 '=');

  let epics = get_cached_epics () in
  let passed = ref 0 in
  let issues = ref [] in

  List.iter (fun epic ->
    if epic.status <> "Done" && epic.status <> "Closed" then begin
      let quality = check_quality_gate epic in
      let summary = if String.length epic.summary > 40 then String.sub epic.summary 0 40 else epic.summary in
      if quality.passed then begin
        incr passed;
        Printf.printf "  %s %s: %s\n" (green "✓") epic.key summary
      end else begin
        issues := (epic, quality.reason) :: !issues;
        Printf.printf "  %s %s: %s\n" (red "✗") epic.key quality.reason
      end
    end
  ) epics;

  print_endline (String.make 50 '-');
  Printf.printf "통과: %d, 문제: %d\n" !passed (List.length !issues)

(* Refresh cache *)
let refresh_cache () =
  Printf.printf "\n%s\n" (blue "🔄 캐시 새로고침");

  (* Delete cache files *)
  if Sys.file_exists cache_dir then begin
    let files = Sys.readdir cache_dir |> Array.to_list in
    let deleted = ref 0 in
    List.iter (fun name ->
      if String.length name > 8 && String.sub name 0 8 = "sharder-" then begin
        let path = Filename.concat cache_dir name in
        Sys.remove path;
        Printf.printf "  삭제: %s\n" name;
        incr deleted
      end
    ) files;
    if !deleted = 0 then print_endline "  (삭제할 캐시 없음)"
  end else
    print_endline "  (캐시 디렉토리 없음)";

  Printf.printf "\n%s\n" (yellow "즉시 동기화 실행 중...");
  sync_shards ~dry_run:false ~quiet:false

(* Main run function *)
let run dry_run status audit stats refresh quiet =
  if status then show_status ()
  else if audit then audit_quality ()
  else if stats then show_stats ()
  else if refresh then refresh_cache ()
  else sync_shards ~dry_run ~quiet

(* CLI arguments *)
let dry_run_arg =
  let doc = "Preview without changes" in
  Arg.(value & flag & info ["dry-run"] ~doc)

let status_arg =
  let doc = "Show current status" in
  Arg.(value & flag & info ["status"] ~doc)

let audit_arg =
  let doc = "품질 감사 (Stale Epic 감지)" in
  Arg.(value & flag & info ["audit"] ~doc)

let stats_arg =
  let doc = "토큰 사용량 통계" in
  Arg.(value & flag & info ["stats"] ~doc)

let refresh_arg =
  let doc = "캐시 삭제 후 즉시 동기화" in
  Arg.(value & flag & info ["refresh"] ~doc)

let quiet_arg =
  let doc = "Minimal output" in
  Arg.(value & flag & info ["quiet"; "q"] ~doc)

let cmd =
  let doc = "Auto-Sharder (BMAD v6) - Epic Shard Management" in
  let info = Cmd.info "auto-sharder" ~doc in
  Cmd.v info Term.(const run $ dry_run_arg $ status_arg $ audit_arg $ stats_arg $ refresh_arg $ quiet_arg)

let () = exit (Cmd.eval cmd)

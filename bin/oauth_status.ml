(** oauth_status - Google OAuth Status Checker

    OCaml port of scripts/check-oauth-status.py
    Checks Google Unified Token validity and expiry.

    Usage:
      oauth-status              # Human-readable output
      oauth-status --json       # JSON output for hooks
      oauth-status --quiet      # Only show issues
*)

(* Use Common module utilities *)
let read_file = Llm_mcp.Common.read_file_opt
let me_root () = Llm_mcp.Common.me_root

(** Account configuration *)
type account = {
  name: string;
  token_path: string;
  email: string;
  label: string;
}

let get_accounts () =
  let root = me_root () in
  let cred_dir = Filename.concat root ".credentials" in
  [
    {
      name = "yousleepwhen";
      token_path = Filename.concat cred_dir "google-unified-token.json";
      email = "yousleepwhen@gmail.com";
      label = "Google (개인)";
    };
    {
      name = "vincent.dev";
      token_path = Filename.concat cred_dir "google-unified-token-vincent.json";
      email = "vincent.dev@kidsnote.com";
      label = "Google (회사)";
    };
  ]

(** Token status *)
type token_status =
  | Valid of float option * bool  (* hours_left option, has_refresh *)
  | AutoRefresh
  | Expired
  | Missing
  | Error of string
  | Unknown

(** Parse ISO datetime string to Unix timestamp *)
let parse_iso_datetime s =
  (* Format: 2025-01-03T12:34:56Z or 2025-01-03T12:34:56+00:00 *)
  try
    let s = if String.length s > 0 && s.[String.length s - 1] = 'Z'
            then String.sub s 0 (String.length s - 1) ^ "+00:00"
            else s in
    (* Extract date and time parts *)
    let date_time = String.sub s 0 19 in
    Scanf.sscanf date_time "%d-%d-%dT%d:%d:%d"
      (fun year month day hour min sec ->
        let tm = {
          Unix.tm_sec = sec;
          tm_min = min;
          tm_hour = hour;
          tm_mday = day;
          tm_mon = month - 1;
          tm_year = year - 1900;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = false;
        } in
        let (time, _) = Unix.mktime tm in
        time)
  with _ -> 0.0

(** Check token file *)
let check_token path =
  if not (Sys.file_exists path) then
    Missing
  else
    match read_file path with
    | None -> Error "Cannot read file"
    | Some content ->
        try
          let json = Yojson.Safe.from_string content in
          let open Yojson.Safe.Util in

          let has_refresh =
            match json |> member "refresh_token" with
            | `Null -> false
            | `String "" -> false
            | `String _ -> true
            | _ -> false
          in

          let expiry = json |> member "expiry" |> to_string_option in
          match expiry with
          | Some exp_str ->
              let expiry_time = parse_iso_datetime exp_str in
              let now = Unix.time () in
              let hours_left = (expiry_time -. now) /. 3600.0 in

              if hours_left < 0.0 then
                if has_refresh then AutoRefresh else Expired
              else
                Valid (Some hours_left, has_refresh)

          | None ->
              if has_refresh then Valid (None, true)
              else Unknown
        with e -> Error (Printexc.to_string e)

(** Format result for human reading *)
let format_result label status =
  match status with
  | Valid (Some hours, has_refresh) ->
      let refresh_icon = if has_refresh then " 🔄" else "" in
      if hours < 1.0 then
        Printf.sprintf "⚠️  %s: %d분 남음%s" label (int_of_float (hours *. 60.0)) refresh_icon
      else if hours < 24.0 then
        Printf.sprintf "✅ %s: %.1f시간 남음%s" label hours refresh_icon
      else
        let days = int_of_float (hours /. 24.0) in
        Printf.sprintf "✅ %s: %d일 남음%s" label days refresh_icon

  | Valid (None, has_refresh) ->
      let refresh_icon = if has_refresh then " 🔄" else "" in
      Printf.sprintf "✅ %s: Valid%s" label refresh_icon

  | AutoRefresh ->
      Printf.sprintf "🔄 %s: 만료됨 (자동 갱신 가능)" label

  | Expired ->
      Printf.sprintf "🔴 %s: 만료됨 - 재인증 필요!" label

  | Missing ->
      Printf.sprintf "⚠️  %s: 미인증" label

  | Error msg ->
      Printf.sprintf "❌ %s: %s" label msg

  | Unknown ->
      Printf.sprintf "❓ %s: Unknown" label

(** Check if status is an issue *)
let is_issue status =
  match status with
  | Expired | Missing | Error _ -> true
  | _ -> false

(** Status to JSON *)
let status_to_json account status =
  let status_str = match status with
    | Valid _ -> "valid"
    | AutoRefresh -> "auto_refresh"
    | Expired -> "expired"
    | Missing -> "missing"
    | Error _ -> "error"
    | Unknown -> "unknown"
  in
  let base = [
    ("status", `String status_str);
    ("email", `String account.email);
    ("label", `String account.label);
  ] in
  let extra = match status with
    | Valid (Some hours, has_refresh) ->
        [("hours_left", `Float hours); ("has_refresh", `Bool has_refresh)]
    | Valid (None, has_refresh) ->
        [("has_refresh", `Bool has_refresh)]
    | Error msg ->
        [("error", `String msg)]
    | _ -> []
  in
  `Assoc (base @ extra)

(** Main function *)
let main () =
  let json_mode = ref false in
  let quiet_mode = ref false in

  (* Parse args *)
  let args = Array.to_list Sys.argv in
  List.iter (fun arg ->
    if arg = "--json" then json_mode := true
    else if arg = "--quiet" then quiet_mode := true
  ) args;

  let accounts = get_accounts () in
  let results = List.map (fun acc ->
    let status = check_token acc.token_path in
    (acc, status)
  ) accounts in

  let issues = List.filter (fun (_, status) -> is_issue status) results in

  if !json_mode then begin
    (* JSON output *)
    let accounts_json = List.map (fun (acc, status) ->
      (acc.name, status_to_json acc status)
    ) results in
    let issues_json = List.map (fun (acc, _) -> `String acc.name) issues in
    let output = `Assoc [
      ("accounts", `Assoc accounts_json);
      ("issues", `List issues_json);
      ("all_ok", `Bool (List.length issues = 0));
    ] in
    print_endline (Yojson.Safe.pretty_to_string output)
  end
  else if !quiet_mode then begin
    (* Quiet mode - only show issues *)
    if List.length issues > 0 then begin
      print_endline "⚠️  Google OAuth 문제 발견:";
      List.iter (fun (acc, status) ->
        Printf.printf "   %s\n" (format_result acc.label status)
      ) issues;
      print_endline "   → /google-auth 로 재인증하세요"
    end
  end
  else begin
    (* Normal output *)
    print_endline "🔐 Google OAuth Status";
    print_endline (String.make 40 '=');
    List.iter (fun (acc, status) ->
      print_endline (format_result acc.label status)
    ) results;

    print_newline ();
    if List.length issues = 0 then
      print_endline "✅ 모든 토큰 정상 (자동 갱신 활성화됨)"
    else begin
      Printf.printf "⚠️  %d개 계정에 문제 있음\n" (List.length issues);
      print_endline "   → /google-auth 로 재인증하세요"
    end
  end;

  if List.length issues > 0 then 1 else 0

let () = exit (main ())

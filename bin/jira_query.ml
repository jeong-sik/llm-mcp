(** jira_query.ml - JIRA Query CLI using Basic Authentication
    OCaml replacement for scripts/jira_query_basic_auth.py *)

open Cmdliner

let jira_base_url = "https://kidsnote.atlassian.net"

(* ANSI colors *)
let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let cyan s = Printf.sprintf "\027[96m%s\027[0m" s
let green s = Printf.sprintf "\027[92m%s\027[0m" s

(* Get JIRA credentials from environment *)
let get_credentials () =
  let email =
    match Sys.getenv_opt "JIRA_EMAIL" with
    | Some e -> Some e
    | None -> Sys.getenv_opt "JIRA_USERNAME"
  in
  let token = Sys.getenv_opt "JIRA_API_TOKEN" in
  match email, token with
  | Some e, Some t -> Some (e, t)
  | _ -> None

(* URL encode *)
let url_encode s =
  let buf = Buffer.create (String.length s * 3) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char buf c
    | ' ' -> Buffer.add_string buf "%20"
    | _ ->
        Printf.bprintf buf "%%%02X" (Char.code c)
  ) s;
  Buffer.contents buf

(* HTTPS/TLS Support *)
let make_https_ctx () =
  match Ca_certs.authenticator () with
  | Error (`Msg m) ->
      Printf.eprintf "Warning: Failed to load system CAs: %s. HTTPS will fail.\n%!" m;
      None
  | Ok authenticator ->
      match Tls.Config.client ~authenticator () with
      | Error (`Msg m) ->
          Printf.eprintf "Warning: TLS config error: %s. HTTPS will fail.\n%!" m;
          None
      | Ok tls_config ->
          Some (fun uri raw ->
              let host =
                match Uri.host uri with
                | None -> None
                | Some h ->
                    match Domain_name.of_string h with
                    | Error _ -> None
                    | Ok dn ->
                        match Domain_name.host dn with
                        | Error _ -> None
                        | Ok host -> Some host
              in
              Tls_eio.client_of_flow tls_config ?host raw)

let read_all ?(max_size = 8 * 1024 * 1024) flow =
  Eio.Buf_read.(of_flow ~max_size flow |> take_all)

let basic_auth_header_value ~email ~token =
  let auth = Base64.encode_string (email ^ ":" ^ token) in
  "Basic " ^ auth

let jira_headers ~email ~token =
  Cohttp.Header.of_list [
    ("Authorization", basic_auth_header_value ~email ~token);
    ("Accept", "application/json");
    ("Content-Type", "application/json");
  ]

let http_get_string ~sw ~client ~headers url =
  let uri = Uri.of_string url in
  let resp, body_flow = Cohttp_eio.Client.get client ~sw ~headers uri in
  let status = Cohttp.Response.status resp in
  let code = Cohttp.Code.code_of_status status in
  let body_str = read_all body_flow in
  if Cohttp.Code.is_success code then Ok body_str else Error (code, body_str)

let http_post_string ~sw ~client ~headers url body =
  let uri = Uri.of_string url in
  let body_eio = Cohttp_eio.Body.of_string body in
  let resp, body_flow = Cohttp_eio.Client.post client ~sw ~headers ~body:body_eio uri in
  let status = Cohttp.Response.status resp in
  let code = Cohttp.Code.code_of_status status in
  let body_str = read_all body_flow in
  if Cohttp.Code.is_success code then Ok body_str else Error (code, body_str)

let ensure_dir path =
  let rec loop p =
    if p = "" || p = Filename.current_dir_name then ()
    else if Sys.file_exists p then ()
    else begin
      let parent = Filename.dirname p in
      if parent <> p then loop parent;
      Unix.mkdir p 0o755
    end
  in
  try loop path with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let download_to_file ~sw ~client ~headers ~fs ~url ~filepath =
  let uri = Uri.of_string url in
  let resp, body_flow = Cohttp_eio.Client.get client ~sw ~headers uri in
  let status = Cohttp.Response.status resp in
  let code = Cohttp.Code.code_of_status status in
  if not (Cohttp.Code.is_success code) then
    let body_str = read_all ~max_size:(1024 * 1024) body_flow in
    Error (code, body_str)
  else begin
    ensure_dir (Filename.dirname filepath);
    let file_path = Eio.Path.(fs / filepath) in
    Eio.Path.with_open_out
      ~create:(`Or_truncate 0o644)
      file_path
      (fun out_flow -> Eio.Flow.copy body_flow out_flow);
    Ok ()
  end

(* Get string from JSON *)
let get_string_opt json key =
  try Some Yojson.Safe.Util.(json |> member key |> to_string) with _ -> None

let get_string json key default =
  Option.value ~default (get_string_opt json key)

(* Format datetime *)
let format_datetime s =
  if String.length s >= 16 then
    (* Take MM-DD HH:MM from ISO format *)
    let month = String.sub s 5 2 in
    let day = String.sub s 8 2 in
    let hour = String.sub s 11 2 in
    let min = String.sub s 14 2 in
    Printf.sprintf "%s-%s %s:%s" month day hour min
  else s

(* Format single issue *)
let format_issue issue =
  let open Yojson.Safe.Util in
  let key = get_string issue "key" "?" in
  let fields = try member "fields" issue with _ -> `Null in
  let summary = get_string fields "summary" "(요약 없음)" in
  let status =
    try fields |> member "status" |> member "name" |> to_string
    with _ -> "미정"
  in
  let assignee =
    try
      let a = fields |> member "assignee" in
      if a = `Null then "미할당"
      else a |> member "displayName" |> to_string
    with _ -> "미할당"
  in
  let priority =
    try fields |> member "priority" |> member "name" |> to_string
    with _ -> "미정"
  in
  let updated =
    try fields |> member "updated" |> to_string |> format_datetime
    with _ -> "미정"
  in

  Printf.printf "\n%s | %s\n" (bold (cyan key)) summary;
  Printf.printf "  상태: %s | 우선순위: %s\n" status priority;
  Printf.printf "  담당자: %s | 업데이트: %s\n" assignee updated;

  (* Epic link *)
  (try
    let parent = fields |> member "parent" in
    if parent <> `Null then begin
      let epic_key = get_string parent "key" "" in
      let epic_summary =
        try parent |> member "fields" |> member "summary" |> to_string
        with _ -> ""
      in
      if epic_key <> "" then
        Printf.printf "  Epic: %s (%s)\n" epic_key epic_summary
    end
  with _ -> ())

(* Query JIRA issues *)
let query_jira ~sw ~client email token jql =
  let url = Printf.sprintf
    "%s/rest/api/3/search/jql?jql=%s&maxResults=50&fields=key,summary,status,assignee,reporter,priority,updated,created,parent"
    jira_base_url (url_encode jql)
  in
  Printf.printf "  요청 중: %s/rest/api/3/search/jql\n" jira_base_url;
  let headers = jira_headers ~email ~token in
  match http_get_string ~sw ~client ~headers url with
  | Error (code, body) ->
      Printf.printf "❌ HTTP 오류: %d\n" code;
      Printf.printf "  응답: %s\n" (String.sub body 0 (min 200 (String.length body)));
      None
  | Ok resp ->
      try
        let json = Yojson.Safe.from_string resp in
        let open Yojson.Safe.Util in

        (* Check for error *)
        (try
          let errors = json |> member "errorMessages" |> to_list in
          if errors <> [] then begin
            Printf.printf "❌ JIRA API 오류:\n";
            List.iter (fun e -> Printf.printf "  %s\n" (to_string e)) errors;
            None
          end else
            Some json
        with _ -> Some json)
      with e ->
        Printf.printf "❌ JSON 파싱 오류: %s\n" (Printexc.to_string e);
        Printf.printf "  응답: %s\n" (String.sub resp 0 (min 200 (String.length resp)));
        None

(* Create JIRA issue *)
let create_issue ~sw ~client email token project summary description issue_type priority =
  let url = Printf.sprintf "%s/rest/api/3/issue" jira_base_url in
  let payload =
    `Assoc [
      ("fields",
       `Assoc [
         ("project", `Assoc [ ("key", `String project) ]);
         ("summary", `String summary);
         ("description",
          `Assoc [
            ("type", `String "doc");
            ("version", `Int 1);
            ("content",
             `List [
               `Assoc [
                 ("type", `String "paragraph");
                 ("content",
                  `List [
                    `Assoc [
                      ("type", `String "text");
                      ("text", `String description);
                    ]
                  ]);
               ]
             ]);
          ]);
         ("issuetype", `Assoc [ ("name", `String issue_type) ]);
         ("priority", `Assoc [ ("name", `String priority) ]);
       ]);
    ]
    |> Yojson.Safe.to_string
  in
  Printf.printf "이슈 생성 중: %s\n" summary;
  let headers = jira_headers ~email ~token in
  match http_post_string ~sw ~client ~headers url payload with
  | Error (code, body) ->
      Printf.printf "❌ HTTP 오류: %d\n" code;
      Printf.printf "  응답: %s\n" (String.sub body 0 (min 500 (String.length body)));
      false
  | Ok resp ->
      try
        let json = Yojson.Safe.from_string resp in
        let open Yojson.Safe.Util in
        (try
          let key = json |> member "key" |> to_string in
          Printf.printf "\n%s 이슈가 성공적으로 생성되었습니다: %s\n" (green "✅") key;
          Printf.printf "🔗 %s/browse/%s\n\n" jira_base_url key;
          true
        with _ ->
          Printf.printf "❌ 이슈 생성 실패\n";
          Printf.printf "  응답: %s\n" resp;
          false)
      with _ ->
        Printf.printf "❌ JSON 파싱 오류\n";
        false

(* Get attachments *)
let get_attachments ~sw ~client ~fs email token issue_key download_dir =
  let url = Printf.sprintf "%s/rest/api/3/issue/%s?fields=attachment,summary"
    jira_base_url issue_key
  in
  let headers = jira_headers ~email ~token in
  match http_get_string ~sw ~client ~headers url with
  | Error (code, body) ->
      Printf.printf "❌ HTTP 오류: %d\n" code;
      Printf.printf "  응답: %s\n" (String.sub body 0 (min 200 (String.length body)));
      false
  | Ok resp ->
      try
        let json = Yojson.Safe.from_string resp in
        let open Yojson.Safe.Util in

        let summary = try json |> member "fields" |> member "summary" |> to_string with _ -> "" in
        let attachments =
          try json |> member "fields" |> member "attachment" |> to_list
          with _ -> []
        in

        Printf.printf "\n📋 %s: %s\n" issue_key summary;
        Printf.printf "%s\n" (String.make 80 '=');

        if attachments = [] then begin
          Printf.printf "  첨부파일 없음\n";
          true
        end else begin
          Printf.printf "📎 첨부파일 (%d개):\n\n" (List.length attachments);

          List.iteri (fun i att ->
            let filename = get_string att "filename" "unknown" in
            let size = try att |> member "size" |> to_int with _ -> 0 in
            let created =
              let c = get_string att "created" "" in
              if String.length c >= 10 then String.sub c 0 10 else c
            in
            let author =
              try att |> member "author" |> member "displayName" |> to_string
              with _ -> "unknown"
            in
            let content_url = get_string att "content" "" in

            let size_str =
              if size > 1024 * 1024 then
                Printf.sprintf "%.1fMB" (float_of_int size /. (1024. *. 1024.))
              else if size > 1024 then
                Printf.sprintf "%.1fKB" (float_of_int size /. 1024.)
              else
                Printf.sprintf "%dB" size
            in

            Printf.printf "  %d. [%s] %s (%s)\n" (i + 1) created filename size_str;
            Printf.printf "     작성자: %s\n" author;
            Printf.printf "     URL: %s\n\n" content_url
          ) attachments;

          (* Download if requested *)
          (match download_dir with
          | Some dir ->
              ensure_dir dir;
              Printf.printf "\n📥 다운로드 중... → %s/\n" dir;

              List.iter (fun att ->
                let filename = get_string att "filename" "unknown" in
                let content_url = get_string att "content" "" in
                if content_url <> "" then begin
                  let filepath = Filename.concat dir filename in
                  match download_to_file ~sw ~client ~headers ~fs ~url:content_url ~filepath with
                  | Ok () ->
                      Printf.printf "  ✅ %s\n" filename
                  | Error (code, body) ->
                      Printf.printf "  ❌ %s (HTTP %d)\n" filename code;
                      Printf.printf "     응답: %s\n"
                        (String.sub body 0 (min 200 (String.length body)))
                end
              ) attachments;

              Printf.printf "\n✅ 다운로드 완료: %s/\n" dir
          | None -> ());

          true
        end
      with e ->
        Printf.printf "❌ 첨부파일 조회 오류: %s\n" (Printexc.to_string e);
        false

(* Main run function *)
let run jql key attachments download create project summary description issue_type priority =
  match get_credentials () with
  | None ->
      Printf.printf "❌ JIRA 자격증명을 가져올 수 없습니다.\n";
      Printf.printf "   JIRA_EMAIL 및 JIRA_API_TOKEN 환경변수를 설정하세요.\n"
  | Some (email, token) ->
      Eio_main.run @@ fun env ->
      Mirage_crypto_rng_unix.use_default ();
      Eio.Switch.run @@ fun sw ->
      let net = Eio.Stdenv.net env in
      let fs = Eio.Stdenv.fs env in
      let https = make_https_ctx () in
      let client = Cohttp_eio.Client.make ~https net in

      (* Attachments mode *)
      if attachments <> "" then begin
        let _ = get_attachments ~sw ~client ~fs email token attachments download in ()
      end

      (* Create mode *)
      else if create then begin
        if summary = "" then
          Printf.printf "❌ 이슈 생성을 위해서는 --summary가 필요합니다.\n"
        else begin
          let desc = if description = "" then summary else description in
          let _ = create_issue ~sw ~client email token project summary desc issue_type priority in ()
        end
      end

      (* Query mode *)
      else begin
        Printf.printf "🔍 JIRA 이슈 조회 중...\n\n";
        Printf.printf "  인증: %s\n\n" email;

        let query =
          if jql <> "" then jql
          else if key <> "" then Printf.sprintf "key = \"%s\"" key
          else begin
            (* Default: recent issues for user *)
            let now = Unix.gettimeofday () in
            let tm = Unix.localtime (now -. (7.0 *. 24. *. 3600.)) in
            let start_date = Printf.sprintf "%04d-%02d-%02d"
              (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday in
            Printf.sprintf
              "(assignee = \"%s\" OR reporter = \"%s\") AND status != Closed AND updated >= %s ORDER BY updated DESC"
              email email start_date
          end
        in

        Printf.printf "  쿼리: %s\n\n" query;

        match query_jira ~sw ~client email token query with
        | None -> ()
        | Some json ->
            let open Yojson.Safe.Util in
            let issues = try json |> member "issues" |> to_list with _ -> [] in

            if issues = [] then
              Printf.printf "\n❌ 조건에 맞는 이슈가 없습니다.\n\n"
            else begin
              Printf.printf "\n%s\n" (String.make 100 '=');
              Printf.printf "📋 JIRA 이슈 조회 결과\n";
              Printf.printf "총 %d개 이슈\n" (List.length issues);
              Printf.printf "%s\n" (String.make 100 '=');

              (* Group by status *)
              let by_status = Hashtbl.create 16 in
              List.iter (fun issue ->
                let status =
                  try issue |> member "fields" |> member "status" |> member "name" |> to_string
                  with _ -> "미정"
                in
                let existing = try Hashtbl.find by_status status with Not_found -> [] in
                Hashtbl.replace by_status status (issue :: existing)
              ) issues;

              (* Display by status *)
              let statuses = Hashtbl.fold (fun k _ acc -> k :: acc) by_status []
                |> List.sort String.compare
              in
              List.iter (fun status ->
                let status_issues = Hashtbl.find by_status status in
                Printf.printf "\n### %s (%d개)\n" status (List.length status_issues);
                Printf.printf "%s\n" (String.make 100 '-');
                List.iter format_issue (List.rev status_issues)
              ) statuses;

              (* Summary *)
              if key = "" then begin
                Printf.printf "\n%s\n" (String.make 100 '=');
                Printf.printf "📊 상태별 요약\n";
                List.iter (fun status ->
                  let count = List.length (Hashtbl.find by_status status) in
                  Printf.printf "  - %s: %d개\n" status count
                ) statuses;
                Printf.printf "%s\n\n" (String.make 100 '=')
              end
            end
      end

(* CLI arguments *)
let jql_arg =
  let doc = "JQL query string" in
  Arg.(value & opt string "" & info ["jql"] ~docv:"JQL" ~doc)

let key_arg =
  let doc = "Issue key (e.g., PK-12345)" in
  Arg.(value & opt string "" & info ["key"; "k"] ~docv:"KEY" ~doc)

let attachments_arg =
  let doc = "List attachments for issue" in
  Arg.(value & opt string "" & info ["attachments"] ~docv:"ISSUE_KEY" ~doc)

let download_arg =
  let doc = "Download attachments to directory" in
  Arg.(value & opt (some string) None & info ["download"] ~docv:"DIR" ~doc)

let create_arg =
  let doc = "Create a new issue" in
  Arg.(value & flag & info ["create"] ~doc)

let project_arg =
  let doc = "JIRA Project key" in
  Arg.(value & opt string "PK" & info ["project"] ~docv:"PROJECT" ~doc)

let summary_arg =
  let doc = "Issue summary" in
  Arg.(value & opt string "" & info ["summary"] ~docv:"SUMMARY" ~doc)

let description_arg =
  let doc = "Issue description" in
  Arg.(value & opt string "" & info ["description"] ~docv:"DESC" ~doc)

let type_arg =
  let doc = "Issue type (Bug, Task, Story)" in
  Arg.(value & opt string "Bug" & info ["type"] ~docv:"TYPE" ~doc)

let priority_arg =
  let doc = "Issue priority (High, Medium, Low)" in
  Arg.(value & opt string "Medium" & info ["priority"] ~docv:"PRIORITY" ~doc)

let cmd =
  let doc = "Query and create JIRA issues using Basic Auth" in
  let info = Cmd.info "jira-query" ~doc in
  Cmd.v info Term.(const run $ jql_arg $ key_arg $ attachments_arg $ download_arg
                   $ create_arg $ project_arg $ summary_arg $ description_arg
                   $ type_arg $ priority_arg)

let () = exit (Cmd.eval cmd)

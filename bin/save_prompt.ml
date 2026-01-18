(** save_prompt - Save user prompts to PostgreSQL

    OCaml port of .claude/hooks/save-prompt.py
    Called asynchronously from UserPromptSubmit hook.

    Usage:
      save_prompt --session-id <id> --prompt <text> [--category <cat>] [--confidence <f>] [--segment <n>]
*)

open Cmdliner

(** Korean character detection using UTF-8 byte check *)
let has_korean text =
  (* Korean Hangul syllables are U+AC00 to U+D7A3
     In UTF-8: EA B0 80 to ED 9E A3
     Check for bytes in Korean UTF-8 range *)
  let len = String.length text in
  let rec check i =
    if i >= len - 2 then false
    else
      let b0 = Char.code text.[i] in
      let b1 = Char.code text.[i + 1] in
      let b2 = Char.code text.[i + 2] in
      (* Check for Korean UTF-8 sequences:
         First byte: 0xEA-0xED (234-237)
         Second byte: 0x80-0xBF (128-191)
         Third byte: 0x80-0xBF (128-191) *)
      if b0 >= 0xEA && b0 <= 0xED &&
         b1 >= 0x80 && b1 <= 0xBF &&
         b2 >= 0x80 && b2 <= 0xBF then
        true
      else
        check (i + 1)
  in
  check 0

(** Check if text looks like a question/request *)
let is_question text =
  (* Check for question mark *)
  if String.contains text '?' then true
  else
    (* Check for common Korean question/request patterns *)
    let korean_patterns = [
      "해줘"; "해주세요"; "할까"; "해봐"; "해보자";
      "보여"; "보자"; "볼까";
      "싶어"; "싶은데";
      "좀"; "ㅋㅋ";
      "확인"; "검토"; "분석"; "추천"; "설명";
      "어때"; "어떻"; "어떤";
      "왜"; "뭐"; "뭘"; "어디"; "언제"; "누가";
    ] in
    List.exists (fun p ->
      (* Simple substring search *)
      let plen = String.length p in
      let tlen = String.length text in
      let rec search i =
        if i > tlen - plen then false
        else if String.sub text i plen = p then true
        else search (i + 1)
      in
      search 0
    ) korean_patterns

(** Get hostname *)
let get_hostname () =
  Unix.gethostname ()

(** Save prompt to PostgreSQL *)
let save_prompt ~session_id ~prompt_text ~routing_category ~routing_confidence ~segment_number =
  let open Lwt.Syntax in

  (* Get PostgreSQL URL from environment *)
  let pg_url = Sys.getenv_opt "RAILWAY_PG_URL" in
  match pg_url with
  | None ->
      prerr_endline "Warning: RAILWAY_PG_URL not set";
      Lwt.return false
  | Some url ->
      (* Validate prompt *)
      if String.length prompt_text < 10 then
        Lwt.return false
      else if String.length prompt_text > 0 && prompt_text.[0] = '/' then
        Lwt.return false
      else if String.sub prompt_text 0 (min 9 (String.length prompt_text)) = "<command-" then
        Lwt.return false
      else begin
        (* Connect and insert *)
        let uri = Uri.of_string url in
        let* conn_result = Caqti_lwt_unix.connect uri in
        match conn_result with
        | Error err ->
            prerr_endline (Printf.sprintf "DB connection error: %s" (Caqti_error.show err));
            Lwt.return false
        | Ok (module Db : Caqti_lwt.CONNECTION) ->
            let hostname = get_hostname () in
            let prompt_len = String.length prompt_text in
            let truncated = if prompt_len > 5000 then String.sub prompt_text 0 5000 else prompt_text in
            let has_kr = has_korean prompt_text in
            let is_q = is_question prompt_text in

            (* Prepared statement - caqti 1.x Infix syntax *)
            let open Caqti_request.Infix in
            let open Caqti_type.Std in
            let param_type = t2
              (t2 (t2 (t2 string string) (t2 int bool)) (t2 bool (option string)))
              (t2 (t2 (option float) int) string)
            in
            let insert_query = (param_type ->. unit) ~oneshot:true
              {|INSERT INTO user_prompts
                (session_id, prompt_text, prompt_length, has_korean, is_question,
                 routing_category, routing_confidence, segment_number, hostname)
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)|}
            in
            let params = (
              (((session_id, truncated), (prompt_len, has_kr)), (is_q, routing_category)),
              ((routing_confidence, segment_number), hostname)
            ) in
            let* insert_result = Db.exec insert_query params in
            let* () = Db.disconnect () in
            match insert_result with
            | Ok () -> Lwt.return true
            | Error err ->
                prerr_endline (Printf.sprintf "Error saving prompt: %s" (Caqti_error.show err));
                Lwt.return false
      end

(** CLI argument definitions *)
let session_id =
  let doc = "Session ID" in
  Arg.(required & opt (some string) None & info ["session-id"] ~doc)

let prompt =
  let doc = "Prompt text to save" in
  Arg.(required & opt (some string) None & info ["prompt"] ~doc)

let category =
  let doc = "Routing category (optional)" in
  Arg.(value & opt (some string) None & info ["category"] ~doc)

let confidence =
  let doc = "Routing confidence (optional)" in
  Arg.(value & opt (some float) None & info ["confidence"] ~doc)

let segment =
  let doc = "Segment number (default: 1)" in
  Arg.(value & opt int 1 & info ["segment"] ~doc)

let main session_id prompt category confidence segment =
  let success = Lwt_main.run (
    save_prompt
      ~session_id
      ~prompt_text:prompt
      ~routing_category:category
      ~routing_confidence:confidence
      ~segment_number:segment
  ) in
  exit (if success then 0 else 1)

let cmd =
  let doc = "Save user prompts to PostgreSQL" in
  let info = Cmd.info "save_prompt" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const main $ session_id $ prompt $ category $ confidence $ segment)

let () = ignore (Cmd.eval cmd)

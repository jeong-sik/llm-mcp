(** memory_retriever - Semantic search for related memories on each prompt (Eio)

    OCaml port of .claude/hooks/memory_retriever.py
    Searches Qdrant vector DB for memories similar to user's query.
    Uses RunPod for BGE-M3 embeddings.

    Usage:
      memory_retriever --query "your search query" [--limit 3] [--format xml|json|text]

    Error Handling:
    - Network errors: logged and returned as empty results (graceful degradation)
    - JSON parse errors: logged with context for debugging
    - TLS errors: logged, falls back to no HTTPS support
*)

(** Verbose logging flag - set to true for debugging *)
let verbose = try Sys.getenv "MEMORY_RETRIEVER_VERBOSE" <> "" with Not_found -> false

(** Log message if verbose mode is enabled *)
let log_verbose fmt =
  Printf.ksprintf (fun s ->
    if verbose then Printf.eprintf "[memory-retriever:debug] %s\n%!" s
  ) fmt

(** Log error message (always) *)
let log_error fmt =
  Printf.ksprintf (fun s ->
    Printf.eprintf "[memory-retriever] %s\n%!" s
  ) fmt

(** Environment variable helpers *)
let get_env_or key default =
  match Sys.getenv_opt key with
  | Some v when String.length v > 0 -> v
  | _ -> default

(** RunPod Configuration *)
let runpod_api_token () = get_env_or "RUNPOD_API_TOKEN" ""
let runpod_endpoint_id () = get_env_or "RUNPOD_ENDPOINT_ID" "yxeb20x9h85ys9"
let runpod_api_url () =
  Printf.sprintf "https://api.runpod.ai/v2/%s/runsync" (runpod_endpoint_id ())

(** Qdrant Configuration *)
let qdrant_url () =
  get_env_or "QDRANT_URL" "https://qdrant-production-498f.up.railway.app"

(** Collections to search (priority order) *)
let search_collections = [
  "procedural_memory";   (* 절차적 패턴 (how-to) *)
  "retrospectives";      (* 회고 (learnings) *)
  "neo4j_communities";   (* 그래프 커뮤니티 요약 *)
  "official_docs";       (* 공식 문서 *)
  "jira_issues";         (* JIRA 이슈 컨텍스트 *)
]

(** Performance settings *)
let max_results_per_collection = 2
let score_threshold = 0.55
let timeout_seconds = 8.0

(** Memory search result *)
type memory = {
  score: float;
  collection: string;
  title: string;
  content: string;
  source: string;
  date: string;
}

(** {1 HTTPS/TLS Support} *)

(** Create HTTPS context for secure connections using system CA certificates *)
let make_https_ctx () =
  match Ca_certs.authenticator () with
  | Error (`Msg m) ->
    Printf.eprintf "[memory-retriever] Warning: Failed to load system CAs: %s\n%!" m;
    None
  | Ok authenticator ->
    match Tls.Config.client ~authenticator () with
    | Error (`Msg m) ->
      Printf.eprintf "[memory-retriever] Warning: TLS config error: %s\n%!" m;
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

(** Create Cohttp_eio client *)
let make_client env =
  let net = Eio.Stdenv.net env in
  let https = make_https_ctx () in
  Cohttp_eio.Client.make ~https net

(** {1 HTTP Helpers} *)

(** HTTP POST with Eio *)
let http_post ~sw ~client uri ~headers ~body =
  let headers = Cohttp.Header.of_list headers in
  let body_eio = Cohttp_eio.Body.of_string body in
  let response, body_stream = Cohttp_eio.Client.post client ~sw ~headers ~body:body_eio uri in
  let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body_stream |> take_all) in
  (response, body_str)

(** HTTP GET with Eio *)
let http_get ~sw ~client uri =
  let response, body_stream = Cohttp_eio.Client.get client ~sw uri in
  let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body_stream |> take_all) in
  (response, body_str)

(** {1 Embedding and Search Functions} *)

(** Parse embeddings from RunPod response - handles multiple formats *)
let parse_embeddings_from_output output =
  let open Yojson.Safe.Util in
  (* Format 1: Infinity Embedding API format {data: [{embedding: [...]}]} *)
  match output |> member "data" with
  | `List data_items ->
      log_verbose "Parsing Infinity Embedding format";
      let embeddings = List.filter_map (fun item ->
        match item |> member "embedding" with
        | `List floats ->
            (try Some (List.map to_float floats)
             with Type_error (msg, _) ->
               log_verbose "Float conversion error in embedding: %s" msg;
               None)
        | _ -> None
      ) data_items in
      if embeddings <> [] then Some embeddings else None
  | _ ->
      (* Format 2: Custom handler format {embeddings: [[...]]} *)
      match output |> member "embeddings" with
      | `List emb_list ->
          log_verbose "Parsing custom handler format";
          let embeddings = List.filter_map (fun e ->
            match e with
            | `List floats ->
                (try Some (List.map to_float floats)
                 with Type_error (msg, _) ->
                   log_verbose "Float conversion error: %s" msg;
                   None)
            | _ -> None
          ) emb_list in
          if embeddings <> [] then Some embeddings else None
      | _ ->
          log_verbose "Unknown embedding format in output: %s"
            (Yojson.Safe.to_string output |> fun s ->
             if String.length s > 100 then String.sub s 0 100 ^ "..." else s);
          None

(** Get embedding from RunPod BGE-M3 *)
let get_embedding ~sw ~client text =
  let token = runpod_api_token () in
  if String.length token = 0 then begin
    log_error "RUNPOD_API_TOKEN not set";
    None
  end else begin
    let uri = Uri.of_string (runpod_api_url ()) in
    let headers = [
      ("Content-Type", "application/json");
      ("Authorization", "Bearer " ^ token);
    ] in
    let payload = `Assoc [
      ("input", `Assoc [
        ("texts", `List [`String text])
      ])
    ] in
    let body = Yojson.Safe.to_string payload in

    try
      let response, body_str = http_post ~sw ~client uri ~headers ~body in
      let status = Http.Response.status response in
      let status_code = Http.Status.to_int status in

      if Cohttp.Code.is_success status_code then begin
        match Yojson.Safe.from_string body_str with
        | json ->
            let open Yojson.Safe.Util in
            (match json |> member "status" |> to_string_option with
             | Some "COMPLETED" ->
                 let output = json |> member "output" in
                 (match parse_embeddings_from_output output with
                  | Some (embedding :: _) -> Some embedding
                  | Some [] | None ->
                      log_verbose "No embeddings found in completed response";
                      None)
             | Some status ->
                 log_verbose "RunPod job status: %s" status;
                 None
             | None ->
                 log_error "Missing 'status' field in RunPod response";
                 None)
        | exception Yojson.Json_error msg ->
            log_error "JSON parse error: %s" msg;
            None
      end
      else begin
        log_error "RunPod HTTP %d: %s" status_code
          (if String.length body_str > 200
           then String.sub body_str 0 200 ^ "..."
           else body_str);
        None
      end
    with
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure _), _) as exn ->
        log_error "Connection failed to RunPod: %s" (Printexc.to_string exn);
        None
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) as exn ->
        log_error "Connection reset by RunPod: %s" (Printexc.to_string exn);
        None
    | Eio.Time.Timeout ->
        log_error "Timeout connecting to RunPod";
        None
    | Yojson.Json_error msg ->
        log_error "JSON parse error: %s" msg;
        None
    | exn ->
        log_error "RunPod error: %s" (Printexc.to_string exn);
        None
  end

(** Check if Qdrant collection exists *)
let qdrant_has_collection ~sw ~client collection_name =
  let uri = Uri.of_string (qdrant_url () ^ "/collections") in
  try
    let response, body_str = http_get ~sw ~client uri in
    let status_code = Http.Status.to_int (Http.Response.status response) in
    if Cohttp.Code.is_success status_code then begin
      match Yojson.Safe.from_string body_str with
      | json ->
          let open Yojson.Safe.Util in
          (match json |> member "result" |> member "collections" with
           | `List collections ->
               let names = List.filter_map (fun c ->
                 match c |> member "name" with
                 | `String name -> Some name
                 | _ -> None
               ) collections in
               List.mem collection_name names
           | _ ->
               log_verbose "Unexpected collections format";
               false)
      | exception Yojson.Json_error msg ->
          log_verbose "Qdrant collections JSON error: %s" msg;
          false
    end
    else begin
      log_verbose "Qdrant collections HTTP %d" status_code;
      false
    end
  with
  | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure _), _) ->
      log_verbose "Cannot connect to Qdrant";
      false
  | exn ->
      log_verbose "Qdrant check error: %s" (Printexc.to_string exn);
      false

(** Extract string from payload trying multiple field names *)
let get_payload_string payload field_names =
  let open Yojson.Safe.Util in
  List.fold_left (fun acc key ->
    match acc with
    | Some _ -> acc
    | None ->
        match payload |> member key with
        | `String s when String.length s > 0 -> Some s
        | _ -> None
  ) None field_names
  |> Option.value ~default:""

(** Parse a single Qdrant search result into a memory record *)
let parse_qdrant_result collection_name result =
  let open Yojson.Safe.Util in
  match result |> member "score" with
  | `Float score ->
      let payload = result |> member "payload" in
      let title = get_payload_string payload ["title"; "topic"; "pattern_name"; "filename"; "name"] in
      let content = get_payload_string payload ["content"; "tldr"; "text"; "summary"] in
      let source = get_payload_string payload ["source"; "session_id"; "file_path"; "filename"] in
      let date = get_payload_string payload ["date"; "created_at"] in
      Some {
        score;
        collection = collection_name;
        title = if title = "" then "Untitled" else title;
        content;
        source;
        date;
      }
  | `Int score_int ->
      (* Handle integer scores (rare but possible) *)
      let score = float_of_int score_int in
      let payload = result |> member "payload" in
      let title = get_payload_string payload ["title"; "topic"; "pattern_name"; "filename"; "name"] in
      Some {
        score;
        collection = collection_name;
        title = if title = "" then "Untitled" else title;
        content = get_payload_string payload ["content"; "tldr"; "text"; "summary"];
        source = get_payload_string payload ["source"; "session_id"; "file_path"; "filename"];
        date = get_payload_string payload ["date"; "created_at"];
      }
  | _ ->
      log_verbose "Missing or invalid score in Qdrant result";
      None

(** Search Qdrant collection *)
let qdrant_search ~sw ~client collection_name query_vector =
  let uri = Uri.of_string (qdrant_url () ^ "/collections/" ^ collection_name ^ "/points/search") in
  let headers = [("Content-Type", "application/json")] in
  let payload = `Assoc [
    ("vector", `List (List.map (fun f -> `Float f) query_vector));
    ("limit", `Int max_results_per_collection);
    ("score_threshold", `Float score_threshold);
    ("with_payload", `Bool true);
  ] in
  let body = Yojson.Safe.to_string payload in

  try
    let response, body_str = http_post ~sw ~client uri ~headers ~body in
    let status_code = Http.Status.to_int (Http.Response.status response) in

    if Cohttp.Code.is_success status_code then begin
      match Yojson.Safe.from_string body_str with
      | json ->
          let open Yojson.Safe.Util in
          (match json |> member "result" with
           | `List results ->
               List.filter_map (parse_qdrant_result collection_name) results
           | _ ->
               log_verbose "Unexpected result format for %s" collection_name;
               [])
      | exception Yojson.Json_error msg ->
          log_verbose "Qdrant search JSON error: %s" msg;
          []
    end
    else begin
      log_verbose "Qdrant search HTTP %d for %s" status_code collection_name;
      []
    end
  with
  | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure _), _) ->
      log_verbose "Cannot connect to Qdrant for search";
      []
  | exn ->
      log_verbose "Qdrant search error: %s" (Printexc.to_string exn);
      []

(** Truncate text with ellipsis *)
let truncate text max_len =
  if String.length text = 0 then ""
  else begin
    let text = String.map (fun c -> if c = '\n' then ' ' else c) text in
    let text = String.trim text in
    if String.length text <= max_len then text
    else (String.sub text 0 (max_len - 3)) ^ "..."
  end

(** Search memories across all collections *)
let search_memories ~sw ~client query limit =
  let start_time = Unix.gettimeofday () in

  (* Get query embedding *)
  let embedding_opt = get_embedding ~sw ~client query in
  match embedding_opt with
  | None ->
      prerr_endline "[memory-retriever] Failed to get embedding";
      []
  | Some query_vector ->
      (* Search each collection *)
      let rec search_collections_loop acc remaining =
        match remaining with
        | [] -> acc
        | collection :: rest ->
            (* Check timeout *)
            let elapsed = Unix.gettimeofday () -. start_time in
            if elapsed > timeout_seconds then
              acc
            else begin
              (* Check if collection exists *)
              let exists = qdrant_has_collection ~sw ~client collection in
              if not exists then
                search_collections_loop acc rest
              else begin
                let memories = qdrant_search ~sw ~client collection query_vector in
                let memories = List.map (fun m ->
                  { m with content = truncate m.content 200 }
                ) memories in
                search_collections_loop (acc @ memories) rest
              end
            end
      in
      let all_memories = search_collections_loop [] search_collections in

      (* Sort by score and limit *)
      let sorted = List.sort (fun a b -> compare b.score a.score) all_memories in
      let limited = if List.length sorted > limit
        then List.filteri (fun i _ -> i < limit) sorted
        else sorted
      in
      limited

(** {1 Output Formatters} *)

(** Format memories as XML for context injection *)
let format_as_xml memories =
  if List.length memories = 0 then ""
  else begin
    let lines = ref [Printf.sprintf "<related_memories count=\"%d\">" (List.length memories)] in
    List.iteri (fun i mem ->
      let rank = i + 1 in
      let score_pct = int_of_float (mem.score *. 100.0) in
      lines := !lines @ [
        Printf.sprintf "  <memory rank=\"%d\" relevance=\"%d%%\" source=\"%s\">" rank score_pct mem.collection;
        Printf.sprintf "    <title>%s</title>" mem.title;
      ];
      if String.length mem.content > 0 then
        lines := !lines @ [Printf.sprintf "    <summary>%s</summary>" mem.content];
      if String.length mem.source > 0 then
        lines := !lines @ [Printf.sprintf "    <reference>%s</reference>" mem.source];
      lines := !lines @ ["  </memory>"]
    ) memories;
    lines := !lines @ ["</related_memories>"];
    String.concat "\n" !lines
  end

(** Format memories as JSON *)
let format_as_json memories =
  let items = List.map (fun mem ->
    `Assoc [
      ("score", `Float mem.score);
      ("collection", `String mem.collection);
      ("title", `String mem.title);
      ("content", `String mem.content);
      ("source", `String mem.source);
      ("date", `String mem.date);
    ]
  ) memories in
  Yojson.Safe.pretty_to_string (`Assoc [
    ("count", `Int (List.length memories));
    ("memories", `List items);
  ])

(** Format memories as text *)
let format_as_text memories =
  if List.length memories = 0 then "No related memories found."
  else begin
    let lines = List.mapi (fun i mem ->
      let rank = i + 1 in
      let score_pct = int_of_float (mem.score *. 100.0) in
      let line1 = Printf.sprintf "%d. [%d%%] %s" rank score_pct mem.title in
      let line2 =
        if String.length mem.content > 0 then
          Printf.sprintf "   %s" (truncate mem.content 100)
        else ""
      in
      if line2 = "" then line1 else line1 ^ "\n" ^ line2
    ) memories in
    String.concat "\n\n" lines
  end

(** {1 CLI} *)

open Cmdliner

let query_arg =
  let doc = "User query to search for" in
  Arg.(required & opt (some string) None & info ["query"; "q"] ~doc)

let limit_arg =
  let doc = "Maximum results to return" in
  Arg.(value & opt int 3 & info ["limit"; "l"] ~doc)

let format_arg =
  let doc = "Output format: xml, json, or text" in
  Arg.(value & opt string "xml" & info ["format"] ~doc)

let json_flag =
  let doc = "Output as JSON (shortcut for --format json)" in
  Arg.(value & flag & info ["json"] ~doc)

let main query limit format_str json_flag =
  let format = if json_flag then "json" else format_str in
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let client = make_client env in
  let memories = search_memories ~sw ~client query limit in

  let output = match format with
    | "json" -> format_as_json memories
    | "text" -> format_as_text memories
    | _ -> format_as_xml memories
  in

  if String.length output > 0 then
    print_endline output

let cmd =
  let doc = "Search related memories from Qdrant vector DB" in
  let info = Cmd.info "memory_retriever" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const main $ query_arg $ limit_arg $ format_arg $ json_flag)

let () = ignore (Cmd.eval cmd)

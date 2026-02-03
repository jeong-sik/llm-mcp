(** HTTP Server Eio - httpun-eio 기반 HTTP 서버

    Eio-native HTTP server for LLM-MCP.
    Based on figma-mcp/masc-mcp proven patterns.

    Features:
    - MCP streamable-http protocol (SSE + JSON-RPC)
    - Graceful shutdown with SSE notification broadcast
    - Session management with protocol version negotiation
*)

open Printf

(** ============== Server Configuration ============== *)

type config = {
  port: int;
  host: string;
  max_connections: int;
}

let default_config = {
  port = 8932;
  host = "127.0.0.1";
  max_connections = 128;
}

(** ============== Request/Response Helpers ============== *)

module Cors = struct
  let mode =
    Sys.getenv_opt "LLM_MCP_CORS_MODE"
    |> Option.value ~default:"restrict"
    |> String.lowercase_ascii

  let split_csv value =
    value
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")

  let default_allowed_origins = [
    "http://localhost";
    "http://localhost:*";
    "https://localhost";
    "https://localhost:*";
    "http://127.0.0.1";
    "http://127.0.0.1:*";
    "https://127.0.0.1";
    "https://127.0.0.1:*";
    "http://[::1]";
    "http://[::1]:*";
    "https://[::1]";
    "https://[::1]:*";
  ]

  let allowed_origins =
    match Sys.getenv_opt "LLM_MCP_CORS_ALLOWED_ORIGINS" with
    | Some value -> split_csv value
    | None -> default_allowed_origins

  let allow_private_network =
    match Sys.getenv_opt "LLM_MCP_CORS_ALLOW_PRIVATE_NETWORK" with
    | Some v ->
        let v = String.lowercase_ascii (String.trim v) in
        v = "1" || v = "true" || v = "yes"
    | None -> false

  let allow_headers =
    Sys.getenv_opt "LLM_MCP_CORS_ALLOW_HEADERS"
    |> Option.value ~default:"Content-Type, Accept, Authorization, X-API-Key, X-MCP-API-Key, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id, X-Requested-With"

  let expose_headers = "Mcp-Session-Id, Mcp-Protocol-Version"
  let allow_methods = "GET, POST, DELETE, OPTIONS"

  let origin_of reqd =
    let request = Httpun.Reqd.request reqd in
    Httpun.Headers.get request.headers "origin"

  let has_prefix ~prefix s =
    let lp = String.length prefix in
    String.length s >= lp && String.sub s 0 lp = prefix

  let has_suffix ~suffix s =
    let ls = String.length suffix in
    let len = String.length s in
    len >= ls && String.sub s (len - ls) ls = suffix

  let matches_pattern origin pattern =
    let pattern = String.trim pattern in
    if pattern = "*" then true
    else if String.length pattern > 0 && pattern.[String.length pattern - 1] = '*'
    then
      let prefix = String.sub pattern 0 (String.length pattern - 1) in
      has_prefix ~prefix origin
    else if String.length pattern > 0 && pattern.[0] = '*'
    then
      let suffix = String.sub pattern 1 (String.length pattern - 1) in
      has_suffix ~suffix origin
    else origin = pattern

  let origin_allowed origin =
    List.exists (matches_pattern origin) allowed_origins

  let is_allowed reqd =
    match mode with
    | "restrict" -> (
        match origin_of reqd with
        | None -> true
        | Some origin -> origin_allowed origin)
    | _ -> true

  let allow_origin_value reqd =
    match mode with
    | "permissive" -> Some "*"
    | "restrict" -> (
        match origin_of reqd with
        | Some origin when origin_allowed origin -> Some origin
        | _ -> None)
    | _ -> Some "*"

  let headers reqd ~include_methods ~include_headers ~include_expose =
    let base =
      match allow_origin_value reqd with
      | Some origin ->
          let vary = if origin = "*" then [] else [("vary", "Origin")] in
          ("access-control-allow-origin", origin) :: vary
      | None -> []
    in
    let headers =
      if include_methods then
        ("access-control-allow-methods", allow_methods) :: base
      else base
    in
    let headers =
      if include_headers then
        ("access-control-allow-headers", allow_headers) :: headers
      else headers
    in
    let headers =
      if include_expose then
        ("access-control-expose-headers", expose_headers) :: headers
      else headers
    in
    if allow_private_network then
      ("access-control-allow-private-network", "true") :: headers
    else headers
end

module Response = struct
  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ Cors.headers reqd ~include_methods:false ~include_headers:false ~include_expose:false) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true ~include_expose:true) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let html ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/html; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ Cors.headers reqd ~include_methods:false ~include_headers:false ~include_expose:false) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  (** 202 Accepted response for notifications (MCP Streamable HTTP) *)
  let accepted reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-length", "0");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true ~include_expose:true) in
    let response = Httpun.Response.create ~headers `Accepted in
    Httpun.Reqd.respond_with_string reqd response "";
    Server_metrics.finish_reqd reqd `Accepted

  let json_with_session ?(status = `OK) ~session_id ~protocol_version body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("mcp-session-id", session_id);
      ("mcp-protocol-version", protocol_version);
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true ~include_expose:true) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list ([
      ("access-control-max-age", "86400");
      ("content-length", "0");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true ~include_expose:true) in
    let response = Httpun.Response.create ~headers `No_content in
    Httpun.Reqd.respond_with_string reqd response "";
    Server_metrics.finish_reqd reqd `No_content

  (** SSE streaming response for MCP streamable-http protocol *)
  let sse_stream ~session_id ~protocol_version reqd ~on_write =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/event-stream");
      ("cache-control", "no-cache");
      ("connection", "keep-alive");
      ("x-accel-buffering", "no");
      ("mcp-session-id", session_id);
      ("mcp-protocol-version", protocol_version);
    ] @ Cors.headers reqd ~include_methods:false ~include_headers:false ~include_expose:true) in
    let response = Httpun.Response.create ~headers `OK in
    let body = Httpun.Reqd.respond_with_streaming reqd response in
    on_write body;
    Server_metrics.finish_reqd reqd `OK
end

module Request = struct
  let default_max_body_bytes = 20 * 1024 * 1024

  let parse_positive_int value =
    try
      let v = int_of_string value in
      if v > 0 then Some v else None
    with _ -> None

  let max_body_bytes =
    let from_env name =
      match Sys.getenv_opt name with
      | Some v -> parse_positive_int v
      | None -> None
    in
    match from_env "LLM_MCP_MAX_BODY_BYTES" with
    | Some v -> v
    | None ->
        (match from_env "MCP_MAX_BODY_BYTES" with
         | Some v -> v
         | None -> default_max_body_bytes)

  let respond_error reqd status body =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("connection", "close");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true ~include_expose:true) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let respond_too_large reqd max_bytes =
    let body = Printf.sprintf
      "413 Request Entity Too Large (max %d bytes)" max_bytes
    in
    respond_error reqd `Payload_too_large body

  let respond_internal_error reqd exn =
    let body = Printf.sprintf
      "500 Internal Server Error: %s" (Printexc.to_string exn)
    in
    respond_error reqd `Internal_server_error body

  (** Read request body - accumulates chunks until EOF.
      Uses callback pattern - the response MUST be sent from within the callback. *)
  let read_body_async reqd callback =
    let request = Httpun.Reqd.request reqd in
    let content_length =
      match Httpun.Headers.get request.headers "content-length" with
      | Some v -> parse_positive_int v
      | None -> None
    in
    let body = Httpun.Reqd.request_body reqd in
    let stopped = ref false in
    let stop () =
      if not !stopped then begin
        stopped := true;
        (try Httpun.Body.Reader.close body with _ -> ())
      end
    in
    match content_length with
    | Some len when len > max_body_bytes ->
        stop ();
        respond_too_large reqd max_body_bytes
    | _ ->
    let initial_capacity =
      match content_length with
      | Some len when len > 0 && len < max_body_bytes -> len
      | _ -> 1024
    in
    let buf = Buffer.create initial_capacity in
    let seen_bytes = ref 0 in
    let rec read_loop () =
      Httpun.Body.Reader.schedule_read body
        ~on_eof:(fun () ->
          let body_str = Buffer.contents buf in
          try callback body_str with exn ->
            respond_internal_error reqd exn)
        ~on_read:(fun buffer ~off ~len ->
          if !stopped then ()
          else
            let next_bytes = !seen_bytes + len in
            if next_bytes > max_body_bytes then begin
              stop ();
              respond_too_large reqd max_body_bytes
            end else begin
              seen_bytes := next_bytes;
              let chunk = Bigstringaf.substring buffer ~off ~len in
              Buffer.add_string buf chunk;
              read_loop ()
            end)
    in
    read_loop ()

  (** Get path from request target *)
  let path (request : Httpun.Request.t) =
    request.target |> String.split_on_char '?' |> List.hd

  (** Get HTTP method *)
  let meth (request : Httpun.Request.t) =
    request.meth

  (** Get header value *)
  let header (request : Httpun.Request.t) name =
    Httpun.Headers.get request.headers name
end

(** ============== SSE Helpers ============== *)

(** SSE client registry for shutdown notification *)
type sse_client = {
  body: Httpun.Body.Writer.t;
  mutable connected: bool;
}

let sse_clients : (int, sse_client) Hashtbl.t = Hashtbl.create 64
let sse_client_counter = ref 0

let register_sse_client body =
  incr sse_client_counter;
  let id = !sse_client_counter in
  let client = { body; connected = true } in
  Hashtbl.add sse_clients id client;
  Server_metrics.sse_open ();
  id

let unregister_sse_client id =
  (match Hashtbl.find_opt sse_clients id with
   | Some c -> c.connected <- false
   | None -> ());
  Hashtbl.remove sse_clients id
  ; Server_metrics.sse_close ()

let sse_client_count () = Hashtbl.length sse_clients

let broadcast_sse_shutdown reason =
  let data = sprintf
    {|{"jsonrpc":"2.0","method":"notifications/shutdown","params":{"reason":"%s","message":"Server is shutting down, please reconnect"}}|}
    reason
  in
  let msg = sprintf "event: notification\ndata: %s\n\n" data in
  Hashtbl.iter (fun client_id client ->
    if client.connected then
      try
        Httpun.Body.Writer.write_string client.body msg;
        Httpun.Body.Writer.flush client.body ignore
      with exn ->
        Printf.eprintf "[HTTP] SSE write failed to client %d: %s\n%!"
          client_id (Printexc.to_string exn)
  ) sse_clients

(** Send SSE event and flush immediately *)
let send_sse_event body ~event ~data =
  let msg = sprintf "event: %s\ndata: %s\n\n" event data in
  Httpun.Body.Writer.write_string body msg;
  Httpun.Body.Writer.flush body ignore

(** Send SSE event with id *)
let send_sse_event_with_id body ~id ~event ~data =
  let msg = sprintf "id: %d\nevent: %s\ndata: %s\n\n" id event data in
  Httpun.Body.Writer.write_string body msg;
  Httpun.Body.Writer.flush body ignore

(** ============== Graceful Shutdown ============== *)

exception Shutdown

(** ============== Error Handler ============== *)

let error_handler _client_addr ?request:_ error start_response =
  let response_body = start_response Httpun.Headers.empty in
  let msg = match error with
    | `Exn exn -> Printexc.to_string exn
    | `Bad_request -> "Bad Request"
    | `Bad_gateway -> "Bad Gateway"
    | `Internal_server_error -> "Internal Server Error"
  in
  Httpun.Body.Writer.write_string response_body msg;
  Httpun.Body.Writer.close response_body

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

module Response = struct
  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id");
      ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let html ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/html; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  (** 202 Accepted response for notifications (MCP Streamable HTTP) *)
  let accepted reqd =
    let headers = Httpun.Headers.of_list [
      ("content-length", "0");
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id");
    ] in
    let response = Httpun.Response.create ~headers `Accepted in
    Httpun.Reqd.respond_with_string reqd response ""

  let json_with_session ?(status = `OK) ~session_id ~protocol_version body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id");
      ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
      ("mcp-session-id", session_id);
      ("mcp-protocol-version", protocol_version);
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list [
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, DELETE, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id");
      ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
      ("access-control-max-age", "86400");
      ("content-length", "0");
    ] in
    let response = Httpun.Response.create ~headers `No_content in
    Httpun.Reqd.respond_with_string reqd response ""

  (** SSE streaming response for MCP streamable-http protocol *)
  let sse_stream ~session_id ~protocol_version reqd ~on_write =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/event-stream");
      ("cache-control", "no-cache");
      ("connection", "keep-alive");
      ("x-accel-buffering", "no");
      ("access-control-allow-origin", "*");
      ("mcp-session-id", session_id);
      ("mcp-protocol-version", protocol_version);
    ] in
    let response = Httpun.Response.create ~headers `OK in
    let body = Httpun.Reqd.respond_with_streaming reqd response in
    on_write body
end

module Request = struct
  (** Read request body - accumulates chunks until EOF.
      Uses callback pattern - the response MUST be sent from within the callback. *)
  let read_body_async reqd callback =
    let body = Httpun.Reqd.request_body reqd in
    let chunks = ref [] in
    let rec read_loop () =
      Httpun.Body.Reader.schedule_read body
        ~on_eof:(fun () ->
          callback (String.concat "" (List.rev !chunks)))
        ~on_read:(fun buffer ~off ~len ->
          let chunk = Bigstringaf.substring buffer ~off ~len in
          chunks := chunk :: !chunks;
          read_loop ())
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
  id

let unregister_sse_client id =
  (match Hashtbl.find_opt sse_clients id with
   | Some c -> c.connected <- false
   | None -> ());
  Hashtbl.remove sse_clients id

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

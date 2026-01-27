(** MCP Client for Eio - Direct-Style Implementation

    Enables llm-mcp to call external MCP servers using Eio.
    Direct-style (no monadic bind), structured concurrency.

    Protocol: JSON-RPC 2.0 over HTTP
    Default endpoints:
    - llm-mcp internal tools: self-execution
    - masc-mcp: http://127.0.0.1:8935
    - Other MCP: configurable via tool arguments
*)

open Printf

module Client = Cohttp_eio.Client
module Body = Cohttp_eio.Body

(** {1 Configuration} *)

(** Request ID counter - atomic for thread safety *)
let request_id = Atomic.make 0

let next_id () =
  Atomic.fetch_and_add request_id 1

(** Default MASC-MCP endpoint *)
let masc_mcp_url = "http://127.0.0.1:8935"

(** {1 JSON-RPC Helpers} *)

(** Build JSON-RPC 2.0 request *)
let build_jsonrpc_request method_name params =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int (next_id ()));
    ("method", `String method_name);
    ("params", params);
  ]

(** {1 Core Client Functions} *)

(** Client type - wraps Eio network and cohttp client *)
type 'a t = {
  client : Client.t;
  sw : Eio.Switch.t;
  clock : 'a Eio.Time.clock;
}

(** Create MCP client from Eio environment *)
let make ~sw ~net ~clock =
  let client = Client.make ~https:None net in
  { client; sw; clock }

let retry_policy = Mcp_resilience.default_policy
let mcp_breaker = Mcp_resilience.create_circuit_breaker ~name:"mcp_client" ~failure_threshold:5 ()

(** Call MCP server via HTTP POST - Direct-style with Resilience *)
let call_mcp_server t ~url ~method_name ~params =
  let uri = Uri.of_string (url ^ "/mcp") in
  let body_str = build_jsonrpc_request method_name params |> Yojson.Safe.to_string in
  let headers = Http.Header.init_with "Content-Type" "application/json" in
  let body = Body.of_string body_str in

  let op () =
    let result =
      try
        let (resp, resp_body) =
          Client.post t.client ~sw:t.sw ~headers ~body uri
        in

        let status = Http.Response.status resp in
        let code = Http.Status.to_int status in
        let resp_str =
          Eio.Buf_read.(of_flow ~max_size:max_int resp_body |> take_all)
        in

        if Cohttp.Code.is_success code then begin
          let open Yojson.Safe.Util in
          try
            let json = Yojson.Safe.from_string resp_str in
            let result = json |> member "result" in
            let error = json |> member "error" in
            if error <> `Null then
              let msg = Safe_parse.json_string ~context:"mcp:error" ~default:"Unknown error" error "message" in
              Result.Error msg
            else
              Result.Ok result
          with e ->
            Result.Error (sprintf "Parse error: %s" (Printexc.to_string e))
        end else if code = 429 || (code >= 500 && code < 600) then
          Result.Error (sprintf "Retryable HTTP error %d: %s" code resp_str)
        else
          Result.Error (sprintf "HTTP error %d: %s" code resp_str)
      with
      | e -> Result.Error (sprintf "Connection error: %s" (Printexc.to_string e))
    in
    match result with
    | Ok value -> Mcp_resilience.Ok value
    | Error err -> Mcp_resilience.Error err
  in

  let classify msg =
    if String.starts_with ~prefix:"Retryable HTTP error" msg then
      Mcp_resilience.Retry
    else if String.starts_with ~prefix:"Connection error" msg then
      Mcp_resilience.Retry
    else
      Mcp_resilience.Fail msg
  in
  match Mcp_resilience.with_retry_eio
          ~clock:t.clock
          ~policy:retry_policy
          ~circuit_breaker:(Some mcp_breaker)
          ~op_name:"mcp_call"
          ~classify
          op
  with
  | Ok res -> Ok res
  | Error err -> Error err
  | CircuitOpen -> Error "Circuit breaker open"
  | TimedOut -> Error "Request timed out"

(** {1 Tool Operations} *)

(** Call tools/call on an MCP server *)
let call_tool t ~url ~tool_name ~arguments =
  let params = `Assoc [
    ("name", `String tool_name);
    ("arguments", arguments);
  ] in
  let start = Unix.gettimeofday () in
  let result = call_mcp_server t ~url ~method_name:"tools/call" ~params in
  let duration_ms =
    int_of_float ((Unix.gettimeofday () -. start) *. 1000.0)
  in
  let error = match result with Ok _ -> None | Error msg -> Some msg in
  Telemetry_jsonl.log_tool_called
    ~tool_name
    ~url
    ~duration_ms
    ~success:(Result.is_ok result)
    ~error;
  result

(** List available tools from MCP server *)
let list_tools t ~url =
  let start = Unix.gettimeofday () in
  let result = call_mcp_server t ~url ~method_name:"tools/list" ~params:`Null in
  let duration_ms =
    int_of_float ((Unix.gettimeofday () -. start) *. 1000.0)
  in
  let error = match result with Ok _ -> None | Error msg -> Some msg in
  Telemetry_jsonl.log_tool_called
    ~tool_name:"tools/list"
    ~url
    ~duration_ms
    ~success:(Result.is_ok result)
    ~error;
  result

(** {1 MASC-MCP Convenience Functions} *)

(** Call MASC tool *)
let call_masc t ~tool_name ~arguments =
  call_tool t ~url:masc_mcp_url ~tool_name ~arguments

(** {1 Result Formatting} *)

(** Format tool result for Ollama chat *)
let format_tool_result tool_name result =
  match result with
  | Ok json ->
      `Assoc [
        ("role", `String "tool");
        ("content", `String (Yojson.Safe.to_string json));
        ("name", `String tool_name);
      ]
  | Error msg ->
      `Assoc [
        ("role", `String "tool");
        ("content", `String (sprintf "Error: %s" msg));
        ("name", `String tool_name);
      ]

(** {1 One-Shot Convenience Functions}

    These functions create a temporary client for single requests.
    Use these when you don't need to reuse connections.
*)

(** One-shot call to MCP server *)
let call_once ~sw ~net ~clock ~url ~method_name ~params =
  let t = make ~sw ~net ~clock in
  call_mcp_server t ~url ~method_name ~params

(** One-shot tool call *)
let call_tool_once ~sw ~net ~clock ~url ~tool_name ~arguments =
  let t = make ~sw ~net ~clock in
  call_tool t ~url ~tool_name ~arguments

(** One-shot MASC call *)
let call_masc_once ~sw ~net ~clock ~tool_name ~arguments =
  let t = make ~sw ~net ~clock in
  call_masc t ~tool_name ~arguments

(** Tools MCP Parse - Pure MCP Response Parsing *)

(** Parse a JSON-RPC result, extracting text content or raw result *)
val parse_json_result : string -> string option

(** Extract SSE data lines from response body *)
val extract_sse_data_lines : string -> string list

(** Build JSON-RPC tool call request *)
val build_tool_call_request : tool_name:string -> arguments:Yojson.Safe.t -> string

(** Parse MCP HTTP response (streamable-http SSE or plain JSON-RPC) *)
val parse_http_response : string -> string option

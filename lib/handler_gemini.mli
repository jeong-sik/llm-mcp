(** Gemini API handler - Direct API + Retry with circuit breaker *)

val parse_models_response :
  include_all:bool ->
  filter:string option ->
  Yojson.Safe.t ->
  Yojson.Safe.t list

val list_models :
  sw:Eio.Switch.t ->
  proc_mgr:[> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  filter:string option ->
  include_all:bool ->
  unit ->
  Types_core.tool_result

val execute_direct_api :
  sw:Eio.Switch.t ->
  proc_mgr:[> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  model:string ->
  prompt:string ->
  thinking_level:Types_core.thinking_level ->
  timeout:int ->
  stream:'a ->
  Types_core.tool_result

val execute_with_retry :
  sw:Eio.Switch.t ->
  proc_mgr:([> [> `Generic ] Eio.Process.mgr_ty ] as 'pm) Eio.Resource.t ->
  clock:([> float Eio.Time.clock_ty ] as 'cl) Eio.Resource.t ->
  ?max_retries:int ->
  model:string ->
  thinking_level:Types_core.thinking_level ->
  timeout:int ->
  stream:bool ->
  args:Types_llm.tool_args ->
  execute_cli_streaming:(sw:Eio.Switch.t ->
    proc_mgr:'pm Eio.Resource.t ->
    clock:'cl Eio.Resource.t ->
    timeout:int ->
    model_name:string ->
    extra_base:(string * string) list ->
    string -> string list -> Types_core.tool_result) ->
  unit ->
  Types_core.tool_result

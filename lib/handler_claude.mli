(** Claude/Anthropic API handler *)

val execute_direct_api :
  sw:Eio.Switch.t ->
  proc_mgr:[> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  model:string ->
  prompt:string ->
  system_prompt:string option ->
  timeout:int ->
  stream:'a ->
  ?api_key_override:string ->
  unit ->
  Types_core.tool_result

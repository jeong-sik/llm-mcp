(** Utility tool handlers - GitHub PR diff, Slack post *)

val external_tool_timeout : int

val execute_gh_pr_diff :
  sw:Eio.Switch.t ->
  proc_mgr:[> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  repo:string ->
  pr_number:int ->
  Types_core.tool_result

val execute_slack_post :
  sw:Eio.Switch.t ->
  proc_mgr:[> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  channel:string ->
  text:string ->
  thread_ts:string option ->
  Types_core.tool_result

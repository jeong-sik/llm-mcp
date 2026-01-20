(** Agent Core Eio - Effect-based Agent Loop Abstraction

    Pure Eio implementation of the Agent Loop pattern.
    No Lwt dependency - direct style programming with effects.

    Usage:
    {[
      module My_Backend : Agent_core_eio.Sigs.LLM_BACKEND = struct ... end
      module My_Tools : Agent_core_eio.Sigs.TOOL_EXECUTOR = struct ... end
      module My_Loop = Agent_core_eio.Make_Loop(My_Backend)(My_Tools)(Agent_core_eio.Default_state)

      let () = Eio_main.run @@ fun env ->
        let clock = Eio.Stdenv.clock env in
        let result = Eio.Switch.run @@ fun sw ->
          My_Loop.run ~sw ~clock ~config ~backend_config ~tools "Hello!"
        in
        ...
    ]}
*)

(** Re-export types *)
module Types = Agent_types

(** Re-export Eio signatures *)
module Sigs = Agent_sigs_eio

(** Retry utilities *)
module Retry = Mcp_resilience

(** Timeout utilities *)
module Timeout = Timeout_eio

(** Composable Meta-Validator *)
module Validator = Validator_eio

(** Validator Presets - Common Patterns *)
module Validator_presets = Validator_presets

(** Spec DSL - SSOT-Driven Validation *)
module Spec_dsl = Spec_dsl

(** Validation Stack - Multi-Level Validation Hierarchy *)
module Validation_stack = Validation_stack

(** Ollama Backend - Direct Eio implementation for local LLM *)
module Ollama_backend_eio = Ollama_backend_eio

(** Main functor for creating agent loops *)
module Make_Loop = Agent_loop_eio.Make

(** Default state manager *)
module Default_state = Agent_loop_eio.Default_state

(** Convenience re-exports from Types *)
type role = Types.role = User | Assistant | Tool | System
type tool_call = Types.tool_call
type message = Types.message
type tool = Types.tool
type tool_result = Types.tool_result = ToolSuccess of string | ToolError of string
type retry_policy = Types.retry_policy
type loop_config = Types.loop_config
type loop_result = Types.loop_result

let default_retry_policy = Types.default_retry_policy
let default_loop_config = Types.default_loop_config

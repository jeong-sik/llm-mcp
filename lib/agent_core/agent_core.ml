(** Agent Core - Reusable Agent Loop abstraction

    This module provides a functor-based approach to building agentic loops
    that can work with different LLM backends, tool executors, and state managers.

    {1 Quick Start}

    {[
      (* Define your backend *)
      module My_Backend : Agent_sigs.LLM_BACKEND = struct ... end

      (* Define your tools *)
      module My_Tools : Agent_sigs.TOOL_EXECUTOR = struct ... end

      (* Create the loop *)
      module My_Loop = Agent_loop_functor.Make(My_Backend)(My_Tools)(Default_state)

      (* Run it *)
      let result = My_Loop.run
        ~config:Agent_types.default_loop_config
        ~backend_config:my_config
        ~initial_prompt:"Hello"
        ~tools:[]
        ()
    ]}

    {1 Key Features}

    - {b Retry with backoff}: Automatic retry on transient failures
    - {b Timeout per turn}: Prevents infinite hangs
    - {b Memory management}: Sliding window to prevent OOM
    - {b Composable}: Mix and match backends, tools, state managers

    {1 Available Modules}

    - {!Agent_types} - Shared type definitions
    - {!Agent_sigs} - Module type signatures
    - {!Agent_loop_functor} - The main functor
    - {!Default_state} - Simple state manager implementation
*)

module Types = Agent_types
module Sigs = Agent_sigs
module Retry = Retry
module Timeout = Timeout
module Agent_loop_functor = Agent_loop_functor
module Make_Loop = Agent_loop_functor.Make
module Default_state = Default_state

(** Agent Loop Functor - Eio Version (Effect-based Direct Style)

    Implements the agentic loop pattern with Eio's effect system:
    1. Send prompt to LLM with available tools
    2. If LLM returns tool_calls, execute them (potentially in parallel)
    3. Send tool results back to LLM
    4. Repeat until no more tool_calls or max_turns reached

    Key differences from Lwt version:
    - Direct style: no let%lwt, no Lwt.return
    - Parallel execution via Eio.Fiber.all
    - Timeout via Eio.Time.with_timeout
    - Cleaner, more readable code
*)

open Agent_types
open Agent_sigs_eio

(** Re-export utilities *)
module Retry = Mcp_resilience
module Timeout = Timeout_eio

(** {1 Agent Loop Functor} *)
module Make
    (Backend : LLM_BACKEND)
    (Tools : TOOL_EXECUTOR)
    (State : STATE_MANAGER) = struct

  (** Internal turn result type *)
  type turn_result =
    | TurnOk of State.t * string * bool  (** state, response, is_final *)
    | TurnError of string

  (** Execute a single turn: call LLM, execute tools if needed.

      @param clock Eio clock for timeout/retry
      @param config Loop configuration
      @param backend_config Backend-specific config
      @param tools Available tools
      @param state Current conversation state
  *)
  let execute_turn ~clock ~config:loop_config ~backend_config ~tools state =
    let messages = State.get_messages state in

    (* Call LLM with retry *)
    let call_with_retry () =
      let classify _ = Retry.Retry in
      Retry.with_retry_eio
        ~clock
        ~policy:loop_config.retry_policy
        ~op_name:"llm_agent_turn"
        ~classify
        (fun () ->
           match Backend.call ~config:backend_config ~messages ~tools with
           | Ok res -> Mcp_resilience.Ok res
           | Error err -> Mcp_resilience.Error err
           | exception exn -> Mcp_resilience.Error (Printexc.to_string exn))
    in

    (* Wrap in timeout *)
    let call_result =
      Timeout.with_timeout_ms ~clock ~timeout_ms:loop_config.timeout_ms call_with_retry
    in

    match call_result with
    | None ->
      TurnError "Turn timed out"
    | Some (Error err) ->
      TurnError ("Failed: " ^ err)
    | Some CircuitOpen ->
      TurnError "Circuit breaker open"
    | Some TimedOut ->
      TurnError "Retry timed out"
    | Some (Ok response) ->
      let content = Backend.extract_content response in
      let tool_calls = Backend.parse_tool_calls response in
      let is_final = Backend.is_final response in

      (* Create assistant message *)
      let assistant_msg = {
        role = Assistant;
        content;
        tool_calls;
        name = None;
      } in

      let state = State.add_message state assistant_msg in

      match tool_calls with
      | None | Some [] ->
        (* No tool calls = conversation complete *)
        TurnOk (state, content, is_final)
      | Some calls ->
        (* Execute each tool call - could be parallelized with Eio.Fiber.all *)
        let tool_results = List.map (fun tc ->
          let result = Tools.execute tc in
          match result with
          | Ok tr -> (tc, tr)
          | Error err -> (tc, ToolError err)
        ) calls in

        (* Add tool result messages *)
        let tool_messages = List.map (fun (tc, tr) ->
          Tools.to_message tc tr
        ) tool_results in

        let state = State.add_messages state tool_messages in
        TurnOk (state, content, false)

  (** Execute tool calls in parallel using Eio fibers.

      @param calls List of tool calls to execute
      @return List of (tool_call, tool_result) pairs
  *)
  let execute_tools_parallel ~sw calls =
    (* Launch all tool executions as fibers *)
    let fibers = List.map (fun tc ->
      Eio.Fiber.fork_promise ~sw (fun () ->
        match Tools.execute tc with
        | Ok tr -> (tc, tr)
        | Error err -> (tc, ToolError err)
      )
    ) calls in
    (* Await all results *)
    List.map Eio.Promise.await_exn fibers

  (** Execute a turn with parallel tool execution.

      Uses Eio.Switch for structured concurrency.
  *)
  let execute_turn_parallel ~sw ~clock ~config:loop_config ~backend_config ~tools state =
    let messages = State.get_messages state in

    let call_with_retry () =
      let classify _ = Retry.Retry in
      Retry.with_retry_eio
        ~clock
        ~policy:loop_config.retry_policy
        ~op_name:"llm_agent_turn_parallel"
        ~classify
        (fun () ->
           match Backend.call ~config:backend_config ~messages ~tools with
           | Ok res -> Mcp_resilience.Ok res
           | Error err -> Mcp_resilience.Error err
           | exception exn -> Mcp_resilience.Error (Printexc.to_string exn))
    in

    let call_result =
      Timeout.with_timeout_ms ~clock ~timeout_ms:loop_config.timeout_ms call_with_retry
    in

    match call_result with
    | None -> TurnError "Turn timed out"
    | Some (Error err) ->
      TurnError ("Failed: " ^ err)
    | Some CircuitOpen -> TurnError "Circuit breaker open"
    | Some TimedOut ->
      TurnError "Retry timed out"
    | Some (Ok response) ->
      let content = Backend.extract_content response in
      let tool_calls = Backend.parse_tool_calls response in
      let is_final = Backend.is_final response in

      let assistant_msg = {
        role = Assistant; content; tool_calls; name = None;
      } in
      let state = State.add_message state assistant_msg in

      match tool_calls with
      | None | Some [] -> TurnOk (state, content, is_final)
      | Some calls ->
        (* Parallel execution! *)
        let tool_results = execute_tools_parallel ~sw calls in
        let tool_messages = List.map (fun (tc, tr) ->
          Tools.to_message tc tr
        ) tool_results in
        let state = State.add_messages state tool_messages in
        TurnOk (state, content, false)

  (** Main loop runner.

      @param sw Eio switch for structured concurrency
      @param clock Eio clock
      @param callbacks Optional event callbacks
      @param config Loop configuration
      @param backend_config Backend configuration
      @param tools Available tools
      @param initial_prompt Initial user prompt
      @return Loop result
  *)
  let run
      ~sw
      ~clock
      ?(callbacks = no_callbacks)
      ~config:loop_config
      ~backend_config
      ~tools
      initial_prompt =

    let initial_msg = {
      role = User;
      content = initial_prompt;
      tool_calls = None;
      name = None;
    } in

    let initial_state = State.create () |> fun s -> State.add_message s initial_msg in

    let rec loop state last_response =
      let turn = State.get_turn_count state in

      if turn >= loop_config.max_turns then
        MaxTurnsReached { last_response; turns_used = turn }
      else begin
        callbacks.on_turn_start turn;

        (* Apply sliding window before each turn *)
        let state = State.apply_sliding_window state ~max_messages:loop_config.max_messages in
        let state = State.increment_turn state in

        match execute_turn_parallel ~sw ~clock ~config:loop_config ~backend_config ~tools state with
        | TurnError err ->
          callbacks.on_error err;
          Error err
        | TurnOk (new_state, response, is_final) ->
          callbacks.on_turn_end turn response;
          if is_final then
            Completed { response; turns_used = turn + 1 }
          else
            loop new_state response
      end
    in
    loop initial_state ""

  (** Simple run without switch - creates one internally *)
  let run_simple
      ~clock
      ?(callbacks = no_callbacks)
      ~config
      ~backend_config
      ~tools
      initial_prompt =
    Eio.Switch.run @@ fun sw ->
      run ~sw ~clock ~callbacks ~config ~backend_config ~tools initial_prompt
end

(** {1 Default State Manager Implementation} *)

module Default_state : STATE_MANAGER = struct
  type t = {
    messages : message list;
    turn_count : int;
  }

  let create () = { messages = []; turn_count = 0 }

  let add_message state msg =
    { state with messages = state.messages @ [msg] }

  let add_messages state msgs =
    { state with messages = state.messages @ msgs }

  let get_messages state = state.messages

  let get_turn_count state = state.turn_count

  let increment_turn state =
    { state with turn_count = state.turn_count + 1 }

  let apply_sliding_window state ~max_messages =
    let len = List.length state.messages in
    if len <= max_messages then state
    else
      let to_keep = len - max_messages in
      { state with messages = List.filteri (fun i _ -> i >= to_keep) state.messages }

  let estimate_tokens state =
    List.fold_left (fun acc msg ->
      acc + estimate_tokens_of_message msg
    ) 0 state.messages
end

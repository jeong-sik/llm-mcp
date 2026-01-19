(** Agent Loop Functor - Core agentic loop with retry and timeout

    Implements the agentic loop pattern:
    1. Send prompt to LLM with available tools
    2. If LLM returns tool_calls, execute them
    3. Send tool results back to LLM
    4. Repeat until no more tool_calls or max_turns reached

    Key features:
    - Configurable retry with exponential backoff
    - Timeout per turn and overall
    - Memory management via sliding window
    - Composable via module interfaces
*)

open Agent_types
open Agent_sigs

(** Re-export Retry and Timeout for convenience *)
module Retry = Retry
module Timeout = Timeout

(** {1 Agent Loop Functor} *)
module Make
    (Backend : LLM_BACKEND)
    (Tools : TOOL_EXECUTOR)
    (State : STATE_MANAGER) = struct

  (** Internal turn result type *)
  type turn_result =
    | TurnOk of State.t * string * bool  (** state, response, is_final *)
    | TurnError of string

  (** Execute a single turn: call LLM, execute tools if needed *)
  let execute_turn ~config:loop_config ~backend_config ~tools state =
    let messages = State.get_messages state in

    (* Call LLM with retry *)
    let call_with_retry () =
      Retry.with_retry loop_config.retry_policy (fun () ->
        Backend.call ~config:backend_config ~messages ~tools
      )
    in

    (* Wrap in timeout *)
    let%lwt call_result =
      Timeout.with_timeout_ms loop_config.timeout_ms call_with_retry
    in

    match call_result with
    | None ->
      Lwt.return (TurnError "Turn timed out")
    | Some (Exhausted { attempts; last_error }) ->
      Lwt.return (TurnError (Printf.sprintf "Failed after %d attempts: %s" attempts last_error))
    | Some RetryCircuitOpen ->
      Lwt.return (TurnError "Circuit breaker open")
    | Some (RetryTimedOut { timeout_ms }) ->
      Lwt.return (TurnError (Printf.sprintf "Retry timed out after %dms" timeout_ms))
    | Some (Success response) ->
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
        Lwt.return (TurnOk (state, content, is_final))
      | Some calls ->
        (* Execute each tool call *)
        let%lwt tool_results = Lwt_list.map_s (fun tc ->
          let%lwt result = Tools.execute tc in
          match result with
          | Ok tr -> Lwt.return (tc, tr)
          | Error err -> Lwt.return (tc, ToolError err)
        ) calls in

        (* Add tool result messages *)
        let tool_messages = List.map (fun (tc, tr) ->
          Tools.to_message tc tr
        ) tool_results in

        let state = State.add_messages state tool_messages in

        (* Not final if we had tool calls *)
        Lwt.return (TurnOk (state, content, false))

  (** Main loop runner *)
  let run
      ?(callbacks = no_callbacks)
      ~config:loop_config
      ~backend_config
      ~initial_prompt
      ~tools
      () =

    (* Initialize state with user prompt *)
    let initial_state = State.create () in
    let user_msg = {
      role = User;
      content = initial_prompt;
      tool_calls = None;
      name = None;
    } in
    let state = State.add_message initial_state user_msg in

    let rec loop state =
      let turn = State.get_turn state in

      (* Check max turns *)
      if turn > loop_config.max_turns then
        let messages = State.get_messages state in
        let last_response = match List.rev messages with
          | msg :: _ when msg.role = Assistant -> msg.content
          | _ -> ""
        in
        Lwt.return (MaxTurnsReached { last_response; turns_used = turn - 1 })
      else begin
        (* Memory management: trim if needed *)
        let state =
          if State.message_count state > loop_config.max_messages then
            State.trim_to_window state ~max_messages:loop_config.max_messages
          else
            state
        in

        (* Callback: turn start *)
        let%lwt () = match callbacks.on_turn_start with
          | Some f -> f turn
          | None -> Lwt.return_unit
        in

        (* Execute turn *)
        let%lwt turn_result = execute_turn ~config:loop_config ~backend_config ~tools state in

        match turn_result with
        | TurnError err ->
          let%lwt () = match callbacks.on_error with
            | Some f -> f err
            | None -> Lwt.return_unit
          in
          Lwt.return (Error err)

        | TurnOk (new_state, response, is_final) ->
          let state = State.increment_turn new_state in

          (* Callback: turn end *)
          let%lwt () = match callbacks.on_turn_end with
            | Some f ->
              let msg = { role = Assistant; content = response; tool_calls = None; name = None } in
              f turn msg
            | None -> Lwt.return_unit
          in

          if is_final then
            Lwt.return (Completed { response; turns_used = turn })
          else
            loop state
      end
    in

    (* Run with overall timeout *)
    let overall_timeout = loop_config.timeout_ms * loop_config.max_turns in
    let%lwt result =
      Timeout.with_timeout_ms overall_timeout (fun () -> loop state)
    in

    match result with
    | None ->
      let turn = State.get_turn state in
      Lwt.return (TimedOut { turns_completed = turn - 1 })
    | Some r -> Lwt.return r
end

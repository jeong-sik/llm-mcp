(** 1-Minute Soak Test - Agent State Persistence *)

open Alcotest
module Types = Agent_core.Types
module Agent_loop_functor = Agent_core.Agent_loop_functor
module State = Agent_core.Default_state

open Types

(* Stateful problem-solving backend *)
module Problem_Solver_Backend = struct
  type config = {
    problem_count: int ref;
    solved_count: int ref;
    start_time: float ref;
    target_duration_sec: float;
  }

  type response = {
    content: string;
    tool_calls: tool_call list option;
  }

  let name = "problem_solver"

  let call ~config ~messages ~tools:_ =
    let elapsed = Unix.gettimeofday () -. !(config.start_time) in
    let msg_count = List.length messages in
    
    (* Simulate thinking time *)
    let%lwt () = Lwt_unix.sleep 0.01 in
    
    if elapsed >= config.target_duration_sec then
      (* Time's up - finish *)
      Lwt.return (Result.Ok {
        content = Printf.sprintf "Completed! Solved %d problems in %.1fs with %d messages"
          !(config.solved_count) elapsed msg_count;
        tool_calls = None;
      })
    else begin
      incr config.problem_count;
      let pid = !(config.problem_count) in

      (* Alternate between needing tools and solving directly *)
      if pid mod 3 = 0 then begin
        (* Use a tool *)
        let tc = { id = Printf.sprintf "calc_%d" pid;
                   name = "calculate";
                   arguments = `Assoc [("n", `Int pid)] } in
        Lwt.return (Result.Ok {
          content = Printf.sprintf "Problem %d: Need calculation..." pid;
          tool_calls = Some [tc];
        })
      end else begin
        (* Solve directly *)
        incr config.solved_count;
        let tc = { id = Printf.sprintf "next_%d" pid;
                   name = "next_problem";
                   arguments = `Null } in
        Lwt.return (Result.Ok {
          content = Printf.sprintf "Problem %d solved! (%.1fs elapsed, %d msgs)"
            pid elapsed msg_count;
          tool_calls = Some [tc];
        })
      end
    end

  let parse_tool_calls r = r.tool_calls
  let extract_content r = r.content
  let is_final r = r.tool_calls = None
  
  let make_config ~target_duration_sec = {
    problem_count = ref 0;
    solved_count = ref 0;
    start_time = ref (Unix.gettimeofday ());
    target_duration_sec;
  }
end

module Problem_Tools = struct
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    let%lwt () = Lwt_unix.sleep 0.005 in (* Simulate tool work *)
    match tc.name with
    | "calculate" -> 
        Lwt.return (Result.Ok (ToolSuccess "Calculated: 42"))
    | "next_problem" ->
        Lwt.return (Result.Ok (ToolSuccess "Ready for next"))
    | _ -> 
        Lwt.return (Result.Ok (ToolSuccess "Done"))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.name }

  let available_tools () = ["calculate"; "next_problem"]
end

module Soak_Loop = Agent_loop_functor.Make(Problem_Solver_Backend)(Problem_Tools)(State)

let test_1_minute_soak () =
  let duration = 60.0 in (* 1 minute *)
  Printf.printf "\n🚀 Starting 1-minute soak test...\n%!";
  
  let backend_config = Problem_Solver_Backend.make_config ~target_duration_sec:duration in
  let loop_config = {
    default_loop_config with
    max_turns = 100000;  (* Allow many turns *)
    max_messages = 100;  (* Sliding window *)
    timeout_ms = 5000;   (* 5s per turn *)
  } in
  
  let print_progress () =
    let elapsed = Unix.gettimeofday () -. !(backend_config.start_time) in
    let problems = !(backend_config.problem_count) in
    let solved = !(backend_config.solved_count) in
    Printf.printf "  ⏱️  %.0fs | Problems: %d | Solved: %d | Rate: %.1f/s\n%!" 
      elapsed problems solved (float_of_int problems /. max 1.0 elapsed)
  in
  
  (* Progress reporter - runs in background, doesn't affect result *)
  let stop_progress = ref false in
  let progress_thread =
    let rec report () =
      let%lwt () = Lwt_unix.sleep 10.0 in
      if not !stop_progress then begin
        print_progress ();
        report ()
      end else
        Lwt.return_unit
    in
    Lwt.async (fun () -> report ());
    Lwt.return_unit
  in

  let main_task =
    let%lwt () = progress_thread in
    Soak_Loop.run
      ~config:loop_config
      ~backend_config
      ~initial_prompt:"Start solving problems"
      ~tools:[]
      ()
  in

  let result = Lwt_main.run main_task in
  stop_progress := true;
  
  print_progress ();
  
  match result with
  | Completed { response; turns_used } ->
      Printf.printf "✅ %s\n%!" response;
      Printf.printf "   Turns used: %d\n%!" turns_used;
      check bool "should complete many turns" true (turns_used > 100);
      check bool "should solve problems" true (!(backend_config.solved_count) > 50)
  | MaxTurnsReached { turns_used; _ } ->
      Printf.printf "⚠️  Max turns reached: %d\n%!" turns_used;
      check bool "should use many turns" true (turns_used > 100)
  | _ -> 
      fail "unexpected result"

let () =
  run "Agent Soak Test (1 min)" [
    "Soak", [
      "1 minute continuous operation", `Slow, test_1_minute_soak;
    ]
  ]

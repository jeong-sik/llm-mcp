(** 1-Hour Soak Test - Agent State Persistence with Martial Arts Quotes

    무협지 대사와 함께하는 1시간 에이전트 내구 테스트!

    Features:
    - 1 hour continuous operation
    - Progress logging every 1 minute
    - Martial arts quotes every 10 minutes
    - Full statistics at the end
*)

module Types = Agent_core.Types
module Agent_loop_functor = Agent_core.Agent_loop_functor
module State = Agent_core.Default_state

open Types

(** Martial arts quotes - 무협지 명대사 *)
let martial_arts_quotes = [|
  "하늘 아래 나와 겨룰 자, 이 강호에 없노라! (天下無敵)";
  "검은 마음에서 나오는 것, 네 검에는 살기가 없구나.";
  "십 년 면벽, 오늘 비로소 검을 뽑는다!";
  "강호에 피바람이 분다... 네놈의 목은 내가 가져가마!";
  "이 한 수로 천하를 평정하리라. 무림맹주의 자리는 내 것이다!";
  "소림의 나한권, 한번 맛보거라!";
  "네 무공이 아무리 높다 한들, 이 화산파의 매화검을 이길 수 있겠느냐?";
  "주화입마(走火入魔)... 결국 이렇게 되는구나.";
  "사부님의 은혜, 이 검으로 갚겠습니다!";
  "천외천(天外天), 인외인(人外人)... 강호는 넓도다.";
  "비급을 얻었다고? 하하, 그것이 네 죽음을 앞당기는 것이니라!";
  "묵혼장(墨魂掌)! 이 일격에 네 목숨을 거두마!";
|]

let get_quote minute =
  let idx = (minute / 10) mod (Array.length martial_arts_quotes) in
  martial_arts_quotes.(idx)

(** Logging *)
let log_file = ref None
let log_channel = ref None

let init_log () =
  let timestamp = Unix.gettimeofday () |> int_of_float in
  let filename = Printf.sprintf "soak_1hour_%d.log" timestamp in
  let path = Filename.concat (Sys.getcwd ()) filename in
  let oc = open_out path in
  log_file := Some path;
  log_channel := Some oc;
  Printf.fprintf oc "=== 1-Hour Soak Test Started ===\n";
  Printf.fprintf oc "Timestamp: %s\n" (Unix.gettimeofday () |> string_of_float);
  Printf.fprintf oc "=====================================\n\n";
  flush oc;
  path

let log msg =
  let timestamp = Unix.gettimeofday () in
  let time_str =
    let tm = Unix.localtime timestamp in
    Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  let line = Printf.sprintf "[%s] %s" time_str msg in
  Printf.printf "%s\n%!" line;
  match !log_channel with
  | Some oc -> Printf.fprintf oc "%s\n" line; flush oc
  | None -> ()

let close_log () =
  match !log_channel with
  | Some oc -> close_out oc
  | None -> ()

(** Stateful problem-solving backend *)
module Problem_Solver_Backend = struct
  type config = {
    problem_count: int ref;
    solved_count: int ref;
    tool_calls_count: int ref;
    start_time: float ref;
    target_duration_sec: float;
    last_log_minute: int ref;
  }

  type response = {
    content: string;
    tool_calls: tool_call list option;
  }

  let name = "martial_arts_problem_solver"

  let call ~config ~messages ~tools:_ =
    let elapsed = Unix.gettimeofday () -. !(config.start_time) in
    let current_minute = int_of_float (elapsed /. 60.0) in
    let msg_count = List.length messages in

    (* Log progress every minute *)
    if current_minute > !(config.last_log_minute) then begin
      config.last_log_minute := current_minute;
      let problems = !(config.problem_count) in
      let solved = !(config.solved_count) in
      let tools = !(config.tool_calls_count) in
      let rate = float_of_int problems /. max 1.0 elapsed in

      log (Printf.sprintf "⏱️  %d분 경과 | 문제: %d | 해결: %d | 도구호출: %d | 메시지: %d | %.1f/s"
             current_minute problems solved tools msg_count rate);

      (* Martial arts quote every 10 minutes *)
      if current_minute > 0 && current_minute mod 10 = 0 then begin
        log "";
        log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
        log (Printf.sprintf "🥋 무협지 명언 #%d" (current_minute / 10));
        log (Printf.sprintf "   \"%s\"" (get_quote current_minute));
        log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
        log ""
      end
    end;

    (* Simulate thinking time *)
    let%lwt () = Lwt_unix.sleep 0.01 in

    if elapsed >= config.target_duration_sec then
      (* Time's up - finish *)
      Lwt.return (Result.Ok {
        content = Printf.sprintf "무림정복 완료! %d개의 문제를 해결하고 %d회 검을 휘둘렀노라!"
          !(config.solved_count) !(config.tool_calls_count);
        tool_calls = None;
      })
    else begin
      incr config.problem_count;
      let pid = !(config.problem_count) in

      (* Alternate between needing tools and solving directly *)
      if pid mod 3 = 0 then begin
        incr config.tool_calls_count;
        let tc = { id = Printf.sprintf "sword_%d" pid;
                   name = "swing_sword";
                   arguments = `Assoc [("power", `Int (pid mod 100))] } in
        Lwt.return (Result.Ok {
          content = Printf.sprintf "적 %d호 발견! 검을 휘두른다..." pid;
          tool_calls = Some [tc];
        })
      end else begin
        incr config.solved_count;
        let tc = { id = Printf.sprintf "next_%d" pid;
                   name = "next_enemy";
                   arguments = `Null } in
        Lwt.return (Result.Ok {
          content = Printf.sprintf "적 %d호 처치! (%.0f초 경과, %d 메시지)"
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
    tool_calls_count = ref 0;
    start_time = ref (Unix.gettimeofday ());
    target_duration_sec;
    last_log_minute = ref (-1);
  }
end

module Martial_Tools = struct
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    let%lwt () = Lwt_unix.sleep 0.005 in
    match tc.name with
    | "swing_sword" ->
        Lwt.return (Result.Ok (ToolSuccess "슈아악! 검기가 허공을 가른다!"))
    | "next_enemy" ->
        Lwt.return (Result.Ok (ToolSuccess "다음 적을 향해 전진!"))
    | _ ->
        Lwt.return (Result.Ok (ToolSuccess "무공 발동!"))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "실패: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.name }

  let available_tools () = ["swing_sword"; "next_enemy"; "meditate"]
end

module Soak_Loop = Agent_loop_functor.Make(Problem_Solver_Backend)(Martial_Tools)(State)

let run_soak_test ~duration_minutes () =
  let duration_sec = float_of_int duration_minutes *. 60.0 in

  let log_path = init_log () in
  log (Printf.sprintf "🚀 %d분 무협 내구 테스트 시작!" duration_minutes);
  log (Printf.sprintf "📝 로그 파일: %s" log_path);
  log "";
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
  log "🥋 무협지 명언 #0 (시작)";
  log "   \"천리길도 한 걸음부터, 만 권의 책도 한 글자부터!\"";
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
  log "";

  let backend_config = Problem_Solver_Backend.make_config ~target_duration_sec:duration_sec in
  let loop_config = {
    default_loop_config with
    max_turns = 10_000_000;  (* Allow many turns *)
    max_messages = 100;      (* Sliding window *)
    timeout_ms = 10_000;     (* 10s per turn *)
  } in

  let start_time = Unix.gettimeofday () in

  let main_task =
    Soak_Loop.run
      ~config:loop_config
      ~backend_config
      ~initial_prompt:"무림의 적들을 모두 처치하라!"
      ~tools:[]
      ()
  in

  let result = Lwt_main.run main_task in

  let elapsed = Unix.gettimeofday () -. start_time in
  let problems = !(backend_config.problem_count) in
  let solved = !(backend_config.solved_count) in
  let tools = !(backend_config.tool_calls_count) in

  log "";
  log "═══════════════════════════════════════════════════════════";
  log "                    🏆 최종 결과 🏆                         ";
  log "═══════════════════════════════════════════════════════════";
  log (Printf.sprintf "⏱️  총 소요 시간: %.1f초 (%.1f분)" elapsed (elapsed /. 60.0));
  log (Printf.sprintf "⚔️  총 문제 수: %d" problems);
  log (Printf.sprintf "✅  해결한 문제: %d" solved);
  log (Printf.sprintf "🗡️  도구 호출 횟수: %d" tools);
  log (Printf.sprintf "📊  처리율: %.1f problems/sec" (float_of_int problems /. elapsed));
  log (Printf.sprintf "💾  메모리: bounded (100 messages sliding window)");
  log "";

  (match result with
   | Completed { response; turns_used } ->
       log (Printf.sprintf "✅ 완료! %s" response);
       log (Printf.sprintf "   턴 수: %d" turns_used)
   | MaxTurnsReached { turns_used; _ } ->
       log (Printf.sprintf "⚠️  최대 턴 도달: %d" turns_used)
   | TimedOut { turns_completed } ->
       log (Printf.sprintf "⏰ 타임아웃: %d 턴" turns_completed)
   | Error msg ->
       log (Printf.sprintf "❌ 에러: %s" msg)
   | CircuitOpen ->
       log "🔴 서킷 브레이커 작동!");

  log "";
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
  log "🥋 무협지 명언 (종료)";
  log "   \"오늘의 싸움은 끝났다. 내일 또 강호에서 보자!\"";
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";

  close_log ();

  Printf.printf "\n📝 전체 로그: %s\n" log_path;

  (* Return stats for verification *)
  (problems, solved, tools, elapsed)

(** Main entry point - run for specified duration *)
let () =
  (* Parse command line for duration, default 60 minutes *)
  let duration =
    if Array.length Sys.argv > 1 then
      try int_of_string Sys.argv.(1)
      with _ -> 60
    else 60
  in

  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════╗\n";
  Printf.printf "║     🥋 Agent Core 무협 내구 테스트 🥋                  ║\n";
  Printf.printf "║        %d분 연속 운영 테스트                          ║\n" duration;
  Printf.printf "╚═══════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  let (problems, solved, _tools, elapsed) = run_soak_test ~duration_minutes:duration () in

  (* Basic assertions *)
  let expected_min_problems = duration * 50 in  (* At least 50 problems per minute *)
  if problems < expected_min_problems then
    Printf.printf "⚠️  Warning: Expected at least %d problems, got %d\n" expected_min_problems problems
  else
    Printf.printf "✅ Performance check passed: %d problems in %.1f minutes\n" problems (elapsed /. 60.0);

  if solved < problems / 3 then
    Printf.printf "⚠️  Warning: Low solve rate: %d/%d\n" solved problems
  else
    Printf.printf "✅ Solve rate check passed: %d/%d (%.1f%%)\n" solved problems
      (100.0 *. float_of_int solved /. float_of_int problems)

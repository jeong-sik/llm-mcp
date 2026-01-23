(** Quote Agent - 3-hour long-running goal-based loop demo (Eio version)

    Prints a famous quote every 10 minutes for 3 hours.
    At each hour mark (1h, 2h, 3h), composes a Korean poem using accumulated quotes.
    Demonstrates Goal-based Loop pattern + LLM integration.

    Usage: dune exec bin/quote_agent.exe
*)

(* Categories for quote generation *)
let categories = [|
  "프로그래밍과 소프트웨어 개발";
  "철학과 존재";
  "과학과 탐구";
  "삶과 성장";
  "창의성과 예술";
  "리더십과 협업";
  "시간과 변화";
  "사랑과 관계";
  "지혜와 배움";
  "용기와 도전";
|]

(* Ollama backend config *)
let ollama_config = Agent_core_eio.Ollama_backend_eio.{
  base_url = "http://127.0.0.1:11434";
  model = "qwen3:1.7b";
  temperature = 0.9;
  stream = false;
  timeout_ms = Some 30_000;
}

(* Generate quote using Ollama with context from previous quotes *)
let generate_quote ~sw ~net ~previous_quotes category =
  let open Agent_core_eio.Types in

  (* Build context from previous quotes *)
  let context_section =
    if List.length previous_quotes = 0 then
      "이것이 첫 번째 명언입니다."
    else
      let prev_list = List.mapi (fun i q ->
        Printf.sprintf "%d. %s" (i+1) q
      ) (List.rev previous_quotes) |> String.concat "\n" in
      Printf.sprintf {|지금까지 생성된 명언들:
%s

위 명언들의 흐름을 이어받아, 연결되거나 발전하는 새로운 명언을 만들어주세요.|} prev_list
  in

  let prompt = Printf.sprintf {|당신은 명언 전문가입니다.

%s

이번 주제: "%s"

규칙:
1. 이전 명언들과 자연스럽게 연결되는 통찰
2. 20-50자 사이로 간결하게
3. 형식: "명언 내용" - 가상의 현자 이름

명언 하나만 출력하세요 (설명 없이):|} context_section category in

  let messages = [{ role = User; content = prompt; tool_calls = None; name = None }] in
  let result = Agent_core_eio.Ollama_backend_eio.call ~sw ~net ~config:ollama_config ~messages ~tools:[] in
  match result with
  | Ok response ->
    (* Clean up the response - remove thinking tags if present *)
    let content = response.content in
    let cleaned =
      if String.length content > 0 then
        (* Remove <think>...</think> tags *)
        let re = Str.regexp "<think>[^<]*</think>" in
        Str.global_replace re "" content |> String.trim
      else content
    in
    cleaned
  | Error e -> Printf.sprintf "(생성 실패: %s)" e

(* Get random category *)
let get_random_category () =
  let idx = Random.int (Array.length categories) in
  categories.(idx)

(* Format duration *)
let format_duration_mins mins =
  let hours = mins / 60 in
  let remaining_mins = mins mod 60 in
  if hours > 0 then
    Printf.sprintf "%dh %dm" hours remaining_mins
  else
    Printf.sprintf "%dm" remaining_mins

(* Main agent state *)
type state = {
  iteration : int;
  start_time : float;
  quotes_shown : string list;
  poems_written : int;
}

(* Goal status - Failed kept for future error handling *)
type goal_status =
  | Reached of string
  | NotReached of string
  | Failed of string
[@@warning "-37"]

(* Goal: 18 iterations (3 hours at 10-min intervals) *)
let max_iterations = 18
let interval_seconds = 600.0  (* 10 minutes *)

(* Hour marks for poem writing: iteration 6, 12, 18 *)
let is_hour_mark iteration =
  iteration > 0 && iteration mod 6 = 0

(* Check if goal reached *)
let check_goal state =
  if state.iteration >= max_iterations then
    Reached (Printf.sprintf "Completed %d quotes and %d poems over 3 hours!" max_iterations state.poems_written)
  else
    let progress = (float_of_int state.iteration) /. (float_of_int max_iterations) *. 100.0 in
    NotReached (Printf.sprintf "Progress: %.1f%% (%d/%d)" progress state.iteration max_iterations)

(* Print status bar *)
let print_status state =
  let elapsed = Unix.gettimeofday () -. state.start_time in
  let elapsed_mins = int_of_float (elapsed /. 60.0) in
  let remaining = max_iterations - state.iteration in
  let remaining_mins = remaining * 10 in

  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  🎯 Quote Agent - Goal-based Loop Demo (Eio)                 ║\n";
  Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
  Printf.printf "║  Iteration: %2d / %2d                                          ║\n" state.iteration max_iterations;
  Printf.printf "║  Elapsed:   %-6s                                         ║\n" (format_duration_mins elapsed_mins);
  Printf.printf "║  Remaining: %-6s (%d quotes)                              ║\n" (format_duration_mins remaining_mins) remaining;
  Printf.printf "║  Poems written: %d / 3                                        ║\n" state.poems_written;
  Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
  Printf.printf "║  📜 Quote #%d:                                                ║\n" (state.iteration + 1);
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";
  flush stdout

(* Generate poem using Ollama *)
let generate_poem_with_ollama ~sw ~net quotes_list hour_num =
  let open Agent_core_eio.Types in
  let quotes_text = String.concat "\n" (List.mapi (fun i q -> Printf.sprintf "%d. %s" (i+1) q) (List.rev quotes_list)) in
  let prompt = Printf.sprintf {|다음 명언들을 영감으로 삼아 아름다운 한국어 시를 한 편 지어주세요.
이것은 %d시간째 시입니다. 명언들의 핵심 메시지를 담아 4-8줄의 시를 작성해주세요.

명언들:
%s

시의 제목도 지어주세요. 형식:
제목: [제목]

[시 내용]|} hour_num quotes_text in

  (* Use Ollama backend *)
  let backend_config = Agent_core_eio.Ollama_backend_eio.{
    base_url = "http://127.0.0.1:11434";
    model = "qwen3:1.7b";
    temperature = 0.8;
    stream = false;
    timeout_ms = Some 60_000;
  } in

  let messages = [{ role = User; content = prompt; tool_calls = None; name = None }] in
  let result = Agent_core_eio.Ollama_backend_eio.call ~sw ~net ~config:backend_config ~messages ~tools:[] in
  match result with
  | Ok response -> response.content
  | Error e -> Printf.sprintf "(시 생성 실패: %s)\n\n대신 간단한 시를 드립니다:\n\n제목: 명언의 빛\n\n지혜의 말들이 모여\n하나의 길을 비추네\n삶의 여정 속에서\n우리는 배우고 성장하리" e

(* Print poem section *)
let print_poem hour_num poem =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  ✨ %d시간째 - 명언으로 짓는 시 ✨                            ║\n" hour_num;
  Printf.printf "╠══════════════════════════════════════════════════════════════╣\n";
  Printf.printf "%s\n" poem;
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";
  flush stdout

(* Main loop *)
let run_agent ~sw ~net ~clock =
  let start_time = Unix.gettimeofday () in
  let initial_state = { iteration = 0; start_time; quotes_shown = []; poems_written = 0 } in

  Printf.printf "\n";
  Printf.printf "🚀 Starting Quote Agent - 3 Hour Run (with Poetry!) [Eio]\n";
  Printf.printf "   Interval: 10 minutes\n";
  Printf.printf "   Total quotes: %d\n" max_iterations;
  Printf.printf "   Poems at: 1h, 2h, 3h (using accumulated quotes)\n";
  let tm = Unix.localtime start_time in
  Printf.printf "   Started at: %04d-%02d-%02d %02d:%02d:%02d\n"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
  Printf.printf "\n";
  flush stdout;

  let rec loop state =
    match check_goal state with
    | Reached summary ->
      Printf.printf "\n🎉 %s\n" summary;
      Printf.printf "   Total runtime: %s\n"
        (format_duration_mins (int_of_float ((Unix.gettimeofday () -. start_time) /. 60.0)));
      flush stdout;
      state

    | Failed reason ->
      Printf.printf "\n❌ Failed: %s\n" reason;
      flush stdout;
      state

    | NotReached _ ->
      (* Print status and generate quote with LLM *)
      print_status state;
      let category = get_random_category () in
      Printf.printf "   📚 주제: %s\n" category;
      Printf.printf "   🤖 명언 생성 중... (컨텍스트: %d개 이전 명언)\n" (List.length state.quotes_shown);
      flush stdout;
      let quote = generate_quote ~sw ~net ~previous_quotes:state.quotes_shown category in
      Printf.printf "   %s\n\n" quote;
      flush stdout;

      (* Update state with new quote *)
      let new_quotes = quote :: state.quotes_shown in
      let new_iteration = state.iteration + 1 in

      (* Check if it's an hour mark - write poem! *)
      let (poems_written, new_quotes_for_state) =
        if is_hour_mark new_iteration then begin
          let hour_num = new_iteration / 6 in
          Printf.printf "🎭 %d시간 경과! 지금까지의 명언들로 시를 짓습니다...\n" hour_num;
          flush stdout;
          let poem = generate_poem_with_ollama ~sw ~net new_quotes hour_num in
          print_poem hour_num poem;
          (state.poems_written + 1, new_quotes)
        end else
          (state.poems_written, new_quotes)
      in

      (* Wait for next interval *)
      Printf.printf "⏳ Next quote in 10 minutes...\n";
      if new_iteration < max_iterations then
        Printf.printf "   (Press Ctrl+C to stop early)\n";
      flush stdout;

      Eio.Time.sleep clock interval_seconds;

      (* Continue loop *)
      let new_state = {
        state with
        iteration = new_iteration;
        quotes_shown = new_quotes_for_state;
        poems_written;
      } in

      loop new_state
  in

  loop initial_state

(* Entry point *)
let () =
  Random.self_init ();

  (* Handle Ctrl+C gracefully *)
  Sys.set_signal Sys.sigint (Signal_handle (fun _ ->
    Printf.printf "\n\n⚠️  Interrupted by user. Exiting gracefully...\n";
    exit 0
  ));

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let final_state = run_agent ~sw ~net ~clock in
    Printf.printf "\n📊 Session Summary:\n";
    Printf.printf "   Quotes shown: %d\n" (List.length final_state.quotes_shown);
    Printf.printf "   Poems written: %d\n" final_state.poems_written

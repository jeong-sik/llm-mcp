(** GoalDriven Tests with Claude API

    Tests the call_api measure_func and Claude integration.
    Requires ANTHROPIC_API_KEY environment variable.
*)

open Chain_types

(* ============================================================================
   Helpers
   ============================================================================ *)

(** Truncate string to max n characters *)
let truncate n s = if String.length s > n then String.sub s 0 n else s

(** Compile chain and return plan *)
let compile_exn chain =
  match Chain_compiler.compile chain with
  | Ok plan -> plan
  | Error msg -> failwith ("compile failed: " ^ msg)

(** Execute chain and return result *)
let execute_chain ~sw ~clock ~exec_fn ~tool_exec chain _input =
  let plan = compile_exn chain in
  let timeout = chain.config.timeout in
  let trace = chain.config.trace in
  let result = Chain_executor_eio.execute ~sw ~clock ~timeout ~trace ~exec_fn ~tool_exec plan in
  if result.success then Ok result.output
  else Error (Printf.sprintf "chain failed: %s" result.output)

(* ============================================================================
   Claude CLI Client
   ============================================================================ *)

(** Check if Claude API is available *)
let claude_available () : bool =
  match Sys.getenv_opt "ANTHROPIC_API_KEY" with
  | Some key when String.length key > 10 -> true
  | _ -> false

(** Call Claude CLI *)
let call_claude ~prompt () : (string, string) result =
  (* Escape special characters for shell *)
  let escaped_prompt = String.concat "\\n" (String.split_on_char '\n' prompt) in
  let escaped_prompt = String.concat "\\\"" (String.split_on_char '"' escaped_prompt) in

  let cmd = Printf.sprintf
    "echo \"%s\" | claude -p --model haiku 2>/dev/null"
    escaped_prompt in

  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in

  match status with
  | Unix.WEXITED 0 -> Ok (String.trim output)
  | _ -> Error (Printf.sprintf "Claude CLI error: %s" output)

(** Mock tool executor - no tools for Claude tests *)
let mock_tool_exec ~name:_ ~args:_ = Error "No tools"

(* ============================================================================
   Test Execution Helpers
   ============================================================================ *)

(** Exec_fn that routes claude-cli to actual Claude, others to mock *)
let make_claude_exec_fn () : (model:string -> ?system:string -> prompt:string -> ?tools:Yojson.Safe.t -> ?thinking:bool -> unit -> (string, string) result) =
  fun ~model ?system:_ ~prompt ?tools:_ ?thinking:_ () ->
    Printf.printf "  [exec] model=%s prompt=%s...\n%!"
      model (truncate 50 prompt);

    if String.length model >= 10 && String.sub model 0 10 = "claude-cli" then
      call_claude ~prompt ()
    else
      (* Mock for non-Claude models *)
      Ok "{\"score\": 0.85}"

(* ============================================================================
   Tests
   ============================================================================ *)

(** Test 1: GoalDriven with call_api measure_func
    Claude returns JSON with score, goal checks if score >= 0.8 *)
let test_goaldriven_call_api () =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Test: GoalDriven with call_api measure_func (Claude)\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "evaluate_action";
    node_type = Llm {
      model = "claude-cli";
      system = None;
      prompt = {|Return ONLY a JSON object with a score between 0.0 and 1.0.
Example: {"score": 0.85}
No explanation, just JSON.|};
      timeout = Some 30;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let goal = {
    id = "quality_goal";
    node_type = GoalDriven {
      goal_metric = "score";
      goal_operator = Gte;
      goal_value = 0.8;
      action_node = action;
      measure_func = "call_api";  (* Parse JSON and extract metric *)
      max_iterations = 2;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let chain = {
    id = "claude_call_api_test";
    nodes = [goal];
    output = "quality_goal";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_claude_exec_fn () in
      match execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" with
      | Ok result ->
          Printf.printf "  Result: %s\n%!" result;
          Printf.printf "✅ call_api measure_func test passed\n%!"
      | Error msg ->
          Printf.printf "  Error: %s\n%!" msg;
          failwith ("call_api test failed: " ^ msg)


(** Test 2: Claude with parse_json measure_func
    Different from call_api - recursively searches for metric *)
let test_parse_json_claude () =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Test: parse_json measure_func with Claude\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "json_action";
    node_type = Llm {
      model = "claude-cli";
      system = None;
      prompt = {|Return ONLY a JSON object with nested structure.
Example: {"metrics": {"accuracy": 0.9}, "status": "ok"}
No explanation, just JSON.|};
      timeout = Some 30;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let goal = {
    id = "json_goal";
    node_type = GoalDriven {
      goal_metric = "accuracy";  (* Will search nested JSON *)
      goal_operator = Gte;
      goal_value = 0.8;
      action_node = action;
      measure_func = "parse_json";
      max_iterations = 2;
      strategy_hints = [];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let chain = {
    id = "parse_json_test";
    nodes = [goal];
    output = "json_goal";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_claude_exec_fn () in
      match execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" with
      | Ok result ->
          Printf.printf "  Result: %s\n%!" result;
          Printf.printf "✅ parse_json measure_func test passed\n%!"
      | Error msg ->
          Printf.printf "  Error: %s\n%!" msg;
          failwith ("parse_json test failed: " ^ msg)


(** Test 3: Strategy hints test - below_50 / above_50 strategies *)
let test_strategy_hints () =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Test: Strategy hints with Claude\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "strategy_action";
    node_type = Llm {
      model = "claude-cli";
      system = None;
      prompt = {|You are optimizing a value. Return ONLY JSON with value field.
Target: achieve value >= 0.7

Return: {"value": 0.75}
No explanation, just JSON.|};
      timeout = Some 30;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let goal = {
    id = "optimize_goal";
    node_type = GoalDriven {
      goal_metric = "value";
      goal_operator = Gte;
      goal_value = 0.7;
      action_node = action;
      measure_func = "call_api";
      max_iterations = 2;
      strategy_hints = [
        ("below_50", "Be aggressive");
        ("above_50", "Fine-tune");
      ];
      conversational = false;
      relay_models = [];
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let chain = {
    id = "strategy_test";
    nodes = [goal];
    output = "optimize_goal";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_claude_exec_fn () in
      match execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" with
      | Ok result ->
          Printf.printf "  Result: %s\n%!" result;
          Printf.printf "✅ Strategy hints test passed\n%!"
      | Error msg ->
          Printf.printf "  Error: %s\n%!" msg;
          failwith ("strategy hints test failed: " ^ msg)


(** Test 4: Conversational mode - context accumulates *)
let test_conversational_mode () =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Test: Conversational mode with Claude\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "conversation_action";
    node_type = Llm {
      model = "claude-cli";
      system = None;
      prompt = {|Count from 1 upward. Return JSON with current count and progress (count/3).
Previous context: {{input}}

If first turn, start at 1. Otherwise increment.
Return: {"count": N, "progress": N/3}
No explanation, just JSON.|};
      timeout = Some 30;
      tools = None;
      prompt_ref = None;
      prompt_vars = []; thinking = false;
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let goal = {
    id = "count_goal";
    node_type = GoalDriven {
      goal_metric = "progress";
      goal_operator = Gte;
      goal_value = 0.66;  (* Need count >= 2 to pass *)
      action_node = action;
      measure_func = "call_api";
      max_iterations = 4;
      strategy_hints = [];
      conversational = true;  (* Enable context accumulation *)
      relay_models = [];
    };
    input_mapping = []; output_key = None; depends_on = None;
  } in

  let chain = {
    id = "conversational_test";
    nodes = [goal];
    output = "count_goal";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_claude_exec_fn () in
      match execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" with
      | Ok result ->
          Printf.printf "  Result: %s\n%!" result;
          Printf.printf "✅ Conversational mode test passed\n%!"
      | Error msg ->
          Printf.printf "  Error: %s\n%!" msg;
          failwith ("conversational test failed: " ^ msg)


(* ============================================================================
   Main
   ============================================================================ *)

let () =
  if not (claude_available ()) then begin
    Printf.printf "⚠️  ANTHROPIC_API_KEY not set, skipping Claude tests\n";
    exit 0
  end;

  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║     GoalDriven Claude API Tests                             ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n%!";

  Alcotest.run "GoalDriven Claude" [
    "call_api", [
      ("basic", `Quick, test_goaldriven_call_api);
    ];
    "parse_json", [
      ("nested", `Quick, test_parse_json_claude);
    ];
    "strategy", [
      ("hints", `Quick, test_strategy_hints);
    ];
    "conversational", [
      ("context", `Quick, test_conversational_mode);
    ];
  ]

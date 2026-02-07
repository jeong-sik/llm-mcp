(** GoalDriven Tests with Gemini API

    Tests the call_api measure_func and real LLM integration.
    Uses Gemini CLI via gemini command.
*)

open Chain_types

let env_truthy name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | _ -> false

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
   Gemini CLI Client
   ============================================================================ *)

(** Check if Gemini CLI is available *)
let gemini_available () : bool =
  Common.command_exists "gemini" && env_truthy "LLM_MCP_RUN_GEMINI_CLI_TESTS"

let default_gemini_cli_model () : string =
  match Sys.getenv_opt "LLM_MCP_GEMINI_CLI_MODEL" with
  | Some m when String.trim m <> "" -> String.trim m
  | _ -> "gemini-2.5-flash"

let resolve_gemini_model (model : string) : string =
  match String.lowercase_ascii (String.trim model) with
  | "" | "gemini" -> default_gemini_cli_model ()
  | "pro" -> "gemini-2.5-pro"
  | "flash" -> "gemini-2.5-flash"
  | "flash-lite" -> "gemini-2.5-flash-lite"
  | "3-pro" -> "gemini-3-pro-preview"
  | "3-flash" -> "gemini-3-flash-preview"
  | m -> m

(** Call Gemini CLI *)
let call_gemini ~model ~prompt () : (string, string) result =
  let model = resolve_gemini_model model in
  let argv = [| "gemini"; "-m"; model; "-p"; prompt |] in
  let ic = Unix.open_process_args_in "gemini" argv in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in

  match status with
  | Unix.WEXITED 0 -> Ok (String.trim output)
  | _ -> Error (Printf.sprintf "Gemini CLI error (model=%s): %s" model output)

(** Mock tool executor - no tools for Gemini tests *)
let mock_tool_exec ~name:_ ~args:_ = Error "No tools"

(* ============================================================================
   Test Execution Helpers
   ============================================================================ *)

(** Exec_fn that routes to Gemini *)
let make_gemini_exec_fn () : (model:string -> ?system:string -> prompt:string -> ?tools:Yojson.Safe.t -> ?thinking:bool -> unit -> (string, string) result) =
  fun ~model ?system:_ ~prompt ?tools:_ ?thinking:_ () ->
    Printf.printf "  [exec] model=%s prompt=%s...\n%!"
      model (truncate 50 prompt);
    call_gemini ~model ~prompt ()

(* ============================================================================
   Tests
   ============================================================================ *)

(** Test 1: GoalDriven with call_api measure_func
    Gemini returns JSON with score, goal checks if score >= 0.8 *)
let test_goaldriven_call_api () =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Test: GoalDriven with call_api measure_func (Gemini)\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "evaluate_action";
    node_type = Llm {
      model = "gemini";
      system = None;
      prompt = {|Return ONLY a JSON object with a score between 0.0 and 1.0.
Example: {"score": 0.85}
No explanation, no markdown, just the raw JSON object.|};
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
    id = "gemini_call_api_test";
    nodes = [goal];
    output = "quality_goal";
    config = default_config;
    name = None; description = None; version = None;
    input_schema = None; output_schema = None; metadata = None
  } in

  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      let exec_fn = make_gemini_exec_fn () in
      match execute_chain ~sw ~clock ~exec_fn ~tool_exec:mock_tool_exec chain "" with
      | Ok result ->
          Printf.printf "  Result: %s\n%!" result;
          Printf.printf "✅ call_api measure_func test passed\n%!"
      | Error msg ->
          Printf.printf "  Error: %s\n%!" msg;
          failwith ("call_api test failed: " ^ msg)


(** Test 2: Gemini with parse_json measure_func
    Different from call_api - recursively searches for metric *)
let test_parse_json_gemini () =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Test: parse_json measure_func with Gemini\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "json_action";
    node_type = Llm {
      model = "gemini";
      system = None;
      prompt = {|Return ONLY a JSON object with nested structure containing accuracy metric.
Example: {"metrics": {"accuracy": 0.9}, "status": "ok"}
No explanation, no markdown, just the raw JSON object.|};
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
      let exec_fn = make_gemini_exec_fn () in
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
  Printf.printf "Test: Strategy hints with Gemini\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "strategy_action";
    node_type = Llm {
      model = "gemini";
      system = None;
      prompt = {|You are optimizing a value. Return ONLY JSON with value field.
Target: achieve value >= 0.7

Return exactly: {"value": 0.75}
No explanation, no markdown, just the raw JSON object.|};
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
      let exec_fn = make_gemini_exec_fn () in
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
  Printf.printf "Test: Conversational mode with Gemini\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n%!";

  let action = {
    id = "conversation_action";
    node_type = Llm {
      model = "gemini";
      system = None;
      prompt = {|Count from 1 upward. Return JSON with current count and progress (count/3).
Previous context: {{input}}

If this is the first turn (empty context), start at 1. Otherwise look at the last count and increment by 1.
Return ONLY: {"count": N, "progress": X} where X = N/3.0
No explanation, no markdown, just the raw JSON object.|};
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
      let exec_fn = make_gemini_exec_fn () in
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
  if not (gemini_available ()) then begin
    Printf.printf "⚠️  Skipping Gemini CLI tests.\n";
    Printf.printf "    - Set LLM_MCP_RUN_GEMINI_CLI_TESTS=1 to enable\n";
    Printf.printf "    - Optionally set LLM_MCP_GEMINI_CLI_MODEL=gemini-2.5-flash\n";
    exit 0
  end;

  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║     GoalDriven Gemini API Tests                             ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n%!";

  Alcotest.run "GoalDriven Gemini" [
    "call_api", [
      ("basic", `Quick, test_goaldriven_call_api);
    ];
    "parse_json", [
      ("nested", `Quick, test_parse_json_gemini);
    ];
    "strategy", [
      ("hints", `Quick, test_strategy_hints);
    ];
    "conversational", [
      ("context", `Quick, test_conversational_mode);
    ];
  ]

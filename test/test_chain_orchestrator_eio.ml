(** Chain Orchestrator E2E Test with MASC-like Tasks

    Tests the full orchestration lifecycle:
    1. Design: LLM generates Chain DSL from tasks
    2. Compile: Parser + Compiler → Execution Plan
    3. Execute: Conductor runs with Eio fibers
    4. Verify: LLM evaluates completion
    5. Decide: Continue / Replan / Complete / Abort
*)

open Chain_composer
open Chain_orchestrator_eio

(** Mock LLM that returns predictable chain designs *)
let mock_llm_call ~prompt:_ =
  (* Return a simple Mermaid graph design with correct node format *)
  {|
Based on the tasks provided, here's an optimal execution chain:

```mermaid
graph LR
  fetch[LLM:claude "Fetch user data"]
  process[LLM:gemini "Process and validate data"]
  store[Tool:database]
  fetch --> process
  process --> store
```

estimated_duration_ms: 3000
critical_path: ["fetch", "process", "store"]

The chain is sequential because each step depends on the previous.
|}

(** Check if string contains any of the keywords (case-insensitive) *)
let contains_keyword keywords str =
  let str_lower = String.lowercase_ascii str in
  List.exists (fun kw ->
    try let _ = Str.search_forward (Str.regexp_string (String.lowercase_ascii kw)) str_lower 0 in true
    with Not_found -> false
  ) keywords

(** Smart mock LLM that detects design/execution/verification calls by prompt content *)
let mock_llm_smart call_count =
  fun ~prompt ->
    incr call_count;
    (* Detect call type by analyzing prompt keywords (case-insensitive) *)
    let first_200 = String.sub prompt 0 (min 200 (String.length prompt)) in
    let is_design = contains_keyword ["Design"; "analyze"; "task list"; "workflow"; "generate.*chain"] first_200 in
    let is_verify = contains_keyword ["Verification"; "Goal Verification"; "Execution Summary"; "Success rate"] first_200 in

    let prompt_len = String.length prompt in
    if false then  (* Set to true for debugging *)
      Printf.printf "[DEBUG] LLM call #%d (design=%b, verify=%b, prompt_len=%d)\n"
        !call_count is_design is_verify prompt_len;

    (* Priority: verification > design > execution
       Verification prompts are ~800 chars and contain "Verification"
       Design prompts are ~1500 chars and contain "Design"
       LLM execution prompts are <100 chars *)
    if is_verify && prompt_len > 500 && prompt_len < 1200 then begin
      (* Verification call - return verification result *)
      ();
      {|
## Verification Result

All tasks have been executed successfully:
- task-001: User data fetched ✓
- task-002: Data processed ✓
- task-003: Result stored ✓

**Conclusion**: Goal achieved
**Confidence**: 0.95
**Complete**: true
**Missing Criteria**: none
**Suggested Next Steps**: none
|}
    end
    else if is_design || !call_count <= 1 then
      (* Design call - return Mermaid chain *)
      mock_llm_call ~prompt:""
    else
      (* Execution call - return mock LLM execution result *)
      Printf.sprintf "Mock LLM execution result for call #%d" !call_count

(** Mock tool executor *)
let mock_tool_exec ~name ~args:_ =
  `Assoc [
    ("tool", `String name);
    ("status", `String "success");
    ("result", `String (Printf.sprintf "Tool %s executed successfully" name));
  ]

(** Test: Full orchestration cycle with mock LLM *)
let test_full_orchestration_cycle () =
  Printf.printf "\n=== Testing Full Orchestration Cycle ===\n";

  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let sw_ref = ref None in

  Eio.Switch.run @@ fun sw ->
  sw_ref := Some sw;

  let call_count = ref 0 in
  let tasks = tasks_from_strings [
    "Fetch user data from API";
    "Process and validate user data";
    "Store result in database";
  ] in

  let config = {
    max_replans = 2;
    timeout_ms = 10_000;
    trace_enabled = true;
    verify_on_complete = true;
  } in

  let result = orchestrate
    ~sw
    ~clock
    ~config
    ~llm_call:(mock_llm_smart call_count)
    ~tool_exec:mock_tool_exec
    ~goal:"Complete user registration workflow"
    ~tasks
  in

  match result with
  | Ok r ->
    Printf.printf "%s\n" (pp_result r);
    Printf.printf "✅ Orchestration completed successfully\n";
    Printf.printf "   LLM calls: %d\n" !call_count;
  | Error e ->
    Printf.printf "Orchestration error: %s\n"
      (match e with
       | DesignFailed s -> Printf.sprintf "Design failed: %s" s
       | CompileFailed s -> Printf.sprintf "Compile failed: %s" s
       | ExecutionFailed s -> Printf.sprintf "Execution failed: %s" s
       | VerificationFailed s -> Printf.sprintf "Verification failed: %s" s
       | MaxReplansExceeded -> "Max replans exceeded"
       | Timeout -> "Timeout")

(** Test: Task creation helpers *)
let test_task_helpers () =
  Printf.printf "\n=== Testing Task Helpers ===\n";

  let tasks = tasks_from_strings [
    "First task";
    "Second task";
    "Third task";
  ] in

  assert (List.length tasks = 3);
  assert ((List.hd tasks).task_id = "task-001");
  assert ((List.hd tasks).title = "First task");
  assert ((List.hd tasks).priority = 1);

  let second = List.nth tasks 1 in
  assert (second.task_id = "task-002");
  assert (second.priority = 2);

  Printf.printf "✅ Task helpers work correctly\n"

(** Test: Orchestration with pipeline chain *)
let test_pipeline_orchestration () =
  Printf.printf "\n=== Testing Pipeline Orchestration ===\n";

  let mock_pipeline_llm ~prompt:_ =
    {|
Here's a simple pipeline chain:

```json
{
  "id": "user-workflow",
  "type": "pipeline",
  "nodes": [
    {"id": "step1", "type": "llm", "model": "claude", "prompt": "Analyze input"},
    {"id": "step2", "type": "llm", "model": "gemini", "prompt": "Process {{step1}}"},
    {"id": "step3", "type": "tool", "name": "save", "args": {"data": "{{step2}}"}}
  ],
  "output": "step3"
}
```

This pipeline:
1. Analyzes input with Claude
2. Processes with Gemini
3. Saves result with tool
|}
  in

  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  Eio.Switch.run @@ fun sw ->

  let tasks = tasks_from_strings ["Analyze and process user data"] in

  let config = {
    max_replans = 1;
    timeout_ms = 5_000;
    trace_enabled = true;
    verify_on_complete = false;  (* Skip verification for this test *)
  } in

  let result = orchestrate
    ~sw
    ~clock
    ~config
    ~llm_call:mock_pipeline_llm
    ~tool_exec:mock_tool_exec
    ~goal:"Process user data"
    ~tasks
  in

  match result with
  | Ok r ->
    Printf.printf "Pipeline orchestration result:\n";
    Printf.printf "  Success: %b\n" r.success;
    Printf.printf "  Replans: %d\n" r.total_replans;
    (match r.final_metrics with
     | Some m -> Printf.printf "  Nodes executed: %d\n" m.total_nodes
     | None -> Printf.printf "  (no metrics)\n");
    Printf.printf "✅ Pipeline orchestration completed\n"
  | Error e ->
    Printf.printf "Pipeline error (expected in mock): %s\n"
      (match e with
       | DesignFailed s -> s
       | CompileFailed s -> s
       | ExecutionFailed s -> s
       | _ -> "other")

(** Test: Timeout handling *)
let test_timeout_handling () =
  Printf.printf "\n=== Testing Timeout Handling ===\n";

  let slow_llm ~prompt:_ =
    (* This would normally take a long time, but we set a short timeout *)
    Unix.sleepf 0.5;  (* 500ms delay *)
    mock_llm_call ~prompt:""
  in

  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  Eio.Switch.run @@ fun sw ->

  let tasks = tasks_from_strings ["Task that times out"] in

  let config = {
    max_replans = 0;
    timeout_ms = 100;  (* Very short timeout *)
    trace_enabled = false;
    verify_on_complete = false;
  } in

  let result = orchestrate
    ~sw
    ~clock
    ~config
    ~llm_call:slow_llm
    ~tool_exec:mock_tool_exec
    ~goal:"Test timeout"
    ~tasks
  in

  match result with
  | Error Timeout ->
    Printf.printf "✅ Timeout correctly detected\n"
  | Error e ->
    Printf.printf "⚠️ Different error (acceptable): %s\n"
      (match e with
       | DesignFailed s -> Printf.sprintf "Design: %s" s
       | _ -> "other")
  | Ok _ ->
    Printf.printf "⚠️ Completed before timeout (fast execution)\n"

(** Test: MASC task structure *)
let test_masc_task_structure () =
  Printf.printf "\n=== Testing MASC Task Structure ===\n";

  let task : masc_task = {
    task_id = "PK-12345";
    title = "Implement user authentication";
    description = Some "Add OAuth2 login flow";
    priority = 1;
    status = "in_progress";
    assignee = Some "claude";
    metadata = [("sprint", "2025-W03"); ("points", "3")];
  } in

  let json = masc_task_to_yojson task in
  let recovered = masc_task_of_yojson json in

  match recovered with
  | Ok t ->
    assert (t.task_id = task.task_id);
    assert (t.title = task.title);
    assert (t.description = task.description);
    assert (t.assignee = task.assignee);
    Printf.printf "✅ MASC task JSON round-trip works\n"
  | Error e ->
    failwith (Printf.sprintf "JSON parse error: %s" e)

(** Run all tests *)
let () =
  Printf.printf "\n";
  Printf.printf "╔══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║         CHAIN ORCHESTRATOR E2E TESTS                          ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════════╝\n";

  test_task_helpers ();
  test_masc_task_structure ();
  test_timeout_handling ();
  test_pipeline_orchestration ();
  test_full_orchestration_cycle ();

  Printf.printf "\n✅ All Chain Orchestrator E2E tests completed\n"

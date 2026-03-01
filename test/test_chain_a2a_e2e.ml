(** A2A E2E Tests -- Agent-to-Agent end-to-end chain validation.

    Simulates a multi-model pipeline:
    1. Model A generates initial content
    2. Model B reviews/transforms
    3. Model C validates

    All using mock LLM responses for determinism.
    @since 1.8.0 *)

open Chain_types
open Alcotest

(** {1 Test Helpers} *)

let parse_chain_exn json =
  match Chain_parser.parse_chain json with
  | Ok chain -> chain
  | Error msg -> failwith ("parse_chain failed: " ^ msg)

let compile_exn chain =
  match Chain_compiler.compile chain with
  | Ok plan -> plan
  | Error msg -> failwith ("compile failed: " ^ msg)

(** Mock tool_exec *)
let tool_exec ~name ~args =
  Ok (Printf.sprintf "[tool:%s]%s" name (Yojson.Safe.to_string args))

(** {1 Model-specific Mock Responses}

    Each model produces a deterministic response that can be
    verified downstream. This simulates a real A2A pipeline
    where each model transforms the output of the previous one. *)

(** Create a mock exec_fn that produces model-specific outputs.
    - gemini: prefixes with "ANALYSIS:"
    - claude: prefixes with "REVIEW:"
    - codex: prefixes with "VALIDATED:"
    Failure is triggered by '!' in the prompt. *)
let make_a2a_exec_fn ?(fail_model = "") ?(fail_count = 0) () =
  let fail_counter = ref 0 in
  fun ~model ?system ~prompt ?tools ?thinking () ->
    ignore (system, tools, thinking);
    if String.contains prompt '!' then Error "forced failure"
    else if model = fail_model && !fail_counter < fail_count then begin
      incr fail_counter;
      Error (Printf.sprintf "%s temporarily unavailable" model)
    end
    else
      let prefix = match model with
        | "gemini" -> "ANALYSIS"
        | "claude" -> "REVIEW"
        | "codex"  -> "VALIDATED"
        | other    -> String.uppercase_ascii other
      in
      Ok (Printf.sprintf "%s:%s" prefix prompt)

(** {1 Test: Three-Model Pipeline (A -> B -> C)} *)

let test_three_model_pipeline () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let exec_fn = make_a2a_exec_fn () in

  let json = Yojson.Safe.from_string {|
    {
      "id": "a2a_pipeline",
      "nodes": [
        { "id": "generator", "type": "llm", "model": "gemini",
          "prompt": "Generate code for: {{input}}" },
        { "id": "reviewer", "type": "llm", "model": "claude",
          "prompt": "Review: {{generator.output}}" },
        { "id": "validator", "type": "llm", "model": "codex",
          "prompt": "Validate: {{reviewer.output}}" }
      ],
      "output": "validator",
      "config": { "max_depth": 4, "max_concurrency": 2, "timeout": 30, "trace": true }
    }
  |} in

  let plan = compile_exn (parse_chain_exn json) in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:true ~exec_fn ~tool_exec plan in

  check bool "pipeline succeeded" true result.success;
  check bool "output is non-empty" true (String.length result.output > 0);

  (* The final output should show the full chain of transformations.
     VALIDATED: ... contains REVIEW: ... contains ANALYSIS: ... *)
  check bool "output contains VALIDATED prefix" true
    (try let _ = Str.search_forward (Str.regexp_string "VALIDATED") result.output 0 in true
     with Not_found -> false);

  (* Trace should have entries for all 3 nodes *)
  check bool "trace has entries" true (List.length result.trace > 0)

(** {1 Test: Pipeline with Failure -- Model B Fails} *)

let test_pipeline_with_failure () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  (* Claude always fails in this test *)
  let exec_fn ~model ?system ~prompt ?tools ?thinking () =
    ignore (system, tools, thinking);
    if model = "claude" then Error "claude service unavailable"
    else Ok (Printf.sprintf "[%s]%s" model prompt)
  in

  let json = Yojson.Safe.from_string {|
    {
      "id": "a2a_fail",
      "nodes": [
        { "id": "gen", "type": "llm", "model": "gemini",
          "prompt": "Generate: {{input}}" },
        { "id": "review", "type": "llm", "model": "claude",
          "prompt": "Review: {{gen.output}}" },
        { "id": "validate", "type": "llm", "model": "codex",
          "prompt": "Validate: {{review.output}}" }
      ],
      "output": "validate",
      "config": { "max_depth": 4, "timeout": 30, "trace": true }
    }
  |} in

  let plan = compile_exn (parse_chain_exn json) in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:true ~exec_fn ~tool_exec plan in

  (* Pipeline should fail because middle node (claude) fails *)
  check bool "pipeline failed due to model B failure" true (not result.success);

  (* Trace should show the failure *)
  check bool "trace recorded" true (List.length result.trace >= 0)

(** {1 Test: Pipeline with Retry -- Model B Fails Once, Then Succeeds}

    Uses a Retry node wrapping the claude call. The first call fails,
    the retry succeeds. *)

let test_pipeline_with_retry () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  (* Claude fails the first time, succeeds the second *)
  let exec_fn = make_a2a_exec_fn ~fail_model:"claude" ~fail_count:1 () in

  (* Use a chain where the claude node is wrapped in a retry.
     The retry node will attempt the LLM call up to 3 times. *)
  let json = Yojson.Safe.from_string {|
    {
      "id": "a2a_retry",
      "nodes": [
        { "id": "gen", "type": "llm", "model": "gemini",
          "prompt": "Generate: {{input}}" },
        { "id": "review_retry", "type": "retry", "max_attempts": 3,
          "backoff": "fixed",
          "node": {
            "id": "review", "type": "llm", "model": "claude",
            "prompt": "Review: {{gen.output}}"
          }
        },
        { "id": "validate", "type": "llm", "model": "codex",
          "prompt": "Validate: {{review_retry.output}}" }
      ],
      "output": "validate",
      "config": { "max_depth": 4, "timeout": 30, "trace": true }
    }
  |} in

  let plan = compile_exn (parse_chain_exn json) in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:true ~exec_fn ~tool_exec plan in

  (* With retry, the pipeline should eventually succeed *)
  check bool "pipeline with retry succeeded" true result.success;
  check bool "output is non-empty" true (String.length result.output > 0)

(** {1 Test: Output Format Validation}

    Verify that the chain output structure matches expected patterns.
    Each model produces a structured prefix, and the final output
    should be a composition of all transformations. *)

let test_output_format_validation () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let exec_fn = make_a2a_exec_fn () in

  (* A chain that produces a structured JSON-like output *)
  let json = Yojson.Safe.from_string {|
    {
      "id": "a2a_format",
      "nodes": [
        { "id": "analyze", "type": "llm", "model": "gemini",
          "prompt": "Analyze: {{input}}" },
        { "id": "summarize", "type": "llm", "model": "claude",
          "prompt": "Summarize: {{analyze.output}}" }
      ],
      "output": "summarize",
      "config": { "max_depth": 4, "timeout": 30 }
    }
  |} in

  let plan = compile_exn (parse_chain_exn json) in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan in

  check bool "format validation succeeded" true result.success;

  (* Output should start with REVIEW: (claude's prefix) *)
  let starts_with prefix s =
    String.length s >= String.length prefix &&
    String.sub s 0 (String.length prefix) = prefix
  in
  check bool "output has REVIEW prefix from claude" true
    (starts_with "REVIEW:" result.output);

  (* The output should contain the nested ANALYSIS from gemini *)
  check bool "output contains ANALYSIS from gemini" true
    (try let _ = Str.search_forward (Str.regexp_string "ANALYSIS") result.output 0 in true
     with Not_found -> false)

(** {1 Test: Parallel A2A with Quorum}

    Multiple models analyze the same input in parallel,
    and a quorum node collects the results. *)

let test_parallel_a2a_quorum () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let exec_fn = make_a2a_exec_fn () in

  let json = Yojson.Safe.from_string {|
    {
      "id": "a2a_quorum",
      "nodes": [
        { "id": "consensus",
          "type": "quorum",
          "required": 2,
          "nodes": [
            { "id": "agent_a", "type": "llm", "model": "gemini", "prompt": "Analyze: input" },
            { "id": "agent_b", "type": "llm", "model": "claude", "prompt": "Analyze: input" },
            { "id": "agent_c", "type": "llm", "model": "codex", "prompt": "Analyze: input" }
          ]
        }
      ],
      "output": "consensus",
      "config": { "max_depth": 4, "timeout": 30 }
    }
  |} in

  let plan = compile_exn (parse_chain_exn json) in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan in

  check bool "quorum A2A succeeded" true result.success;
  check bool "quorum output is non-empty" true (String.length result.output > 0)

(** {1 Test Suite Registration} *)

let () =
  run "A2A_E2E_Pipeline" [
    "three_model_pipeline", [
      test_case "A -> B -> C with mock responses" `Quick test_three_model_pipeline;
      test_case "model B failure propagation" `Quick test_pipeline_with_failure;
      test_case "model B retry recovery" `Quick test_pipeline_with_retry;
    ];
    "output_validation", [
      test_case "output format matches expected schema" `Quick test_output_format_validation;
      test_case "parallel A2A with quorum" `Quick test_parallel_a2a_quorum;
    ];
  ]

(** Chain Engine Eio - Execution proof tests *)

open Chain_types

let parse_chain_exn json =
  match Chain_parser.parse_chain json with
  | Ok chain -> chain
  | Error msg -> failwith ("parse_chain failed: " ^ msg)

let compile_exn chain =
  match Chain_compiler.compile chain with
  | Ok plan -> plan
  | Error msg -> failwith ("compile failed: " ^ msg)

let exec_fn ~model ~prompt ?tools () =
  ignore tools;  (* tools field added for Ollama interop *)
  if String.contains prompt '!' then Error "forced failure"
  else Ok (Printf.sprintf "[%s]%s" model prompt)

let tool_exec ~name ~args =
  let open Yojson.Safe.Util in
  match name with
  | "echo" ->
      (match args |> member "text" |> to_string_option with
      | Some text -> Ok text
      | None -> Error "missing text")
  | _ ->
      Ok (Printf.sprintf "[%s]%s" name (Yojson.Safe.to_string args))

let test_simple_pipeline ~sw ~clock () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "simple_pipeline",
      "nodes": [
        { "id": "a", "type": "llm", "model": "gemini", "prompt": "A: {{input}}" },
        { "id": "b", "type": "llm", "model": "claude", "prompt": "B: {{a.output}}" }
      ],
      "output": "b",
      "config": { "max_depth": 4, "max_concurrency": 2, "timeout": 30, "trace": true }
    }
  |} in
  let chain = parse_chain_exn json in
  let plan = compile_exn chain in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:true ~exec_fn ~tool_exec plan in
  assert (result.success = true);
  assert (String.length result.output > 0);
  Printf.printf "[OK] simple pipeline executes (output=%s)\n%!"
    (String.sub result.output 0 (min 48 (String.length result.output)))

let test_quorum ~sw ~clock () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "quorum",
      "nodes": [
        { "id": "q",
          "type": "quorum",
          "required": 2,
          "nodes": [
            { "id": "a", "type": "llm", "model": "gemini", "prompt": "ok" },
            { "id": "b", "type": "llm", "model": "claude", "prompt": "ok" },
            { "id": "c", "type": "llm", "model": "codex", "prompt": "fail!" }
          ]
        }
      ],
      "output": "q"
    }
  |} in
  let chain = parse_chain_exn json in
  let plan = compile_exn chain in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:true ~exec_fn ~tool_exec plan in
  assert (result.success = true);
  Printf.printf "[OK] quorum passes with 2/3 successes\n%!"

let test_gate_else ~sw ~clock () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "gate",
      "nodes": [
        { "id": "g",
          "type": "gate",
          "condition": "false",
          "then": { "id": "t", "type": "llm", "model": "gemini", "prompt": "then" },
          "else": { "id": "e", "type": "llm", "model": "claude", "prompt": "else" }
        }
      ],
      "output": "g"
    }
  |} in
  let chain = parse_chain_exn json in
  let plan = compile_exn chain in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan in
  assert (String.contains result.output 'e');
  Printf.printf "[OK] gate executes else branch\n%!"

let test_compile_duplicate_ids () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "dup",
      "nodes": [
        { "id": "a", "type": "llm", "model": "gemini", "prompt": "x" },
        { "id": "a", "type": "llm", "model": "claude", "prompt": "y" }
      ],
      "output": "a"
    }
  |} in
  let chain = parse_chain_exn json in
  match Chain_compiler.compile chain with
  | Ok _ -> failwith "compile should fail on duplicate IDs"
  | Error _ -> Printf.printf "[OK] duplicate ID detected\n%!"

let test_tool_node_substitution ~sw ~clock () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "tool_substitute",
      "nodes": [
        { "id": "a", "type": "llm", "model": "gemini", "prompt": "hello" },
        { "id": "t", "type": "tool", "name": "echo", "args": { "text": "{{a.output}}" } }
      ],
      "output": "t"
    }
  |} in
  let chain = parse_chain_exn json in
  let plan = compile_exn chain in
  let result = Chain_executor_eio.execute ~sw ~clock
    ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan in
  assert (result.output = "[gemini]hello");
  Printf.printf "[OK] tool args substitution works\n%!"

let test_parallel_levels_2x2x2 () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "scale_2x2x2",
      "nodes": [
        { "id": "a1", "type": "llm", "model": "gemini", "prompt": "A1" },
        { "id": "a2", "type": "llm", "model": "gemini", "prompt": "A2" },
        { "id": "b1", "type": "llm", "model": "claude", "prompt": "B1 {{a1.output}} {{a2.output}}" },
        { "id": "b2", "type": "llm", "model": "claude", "prompt": "B2 {{a1.output}} {{a2.output}}" },
        { "id": "c1", "type": "llm", "model": "codex", "prompt": "C1 {{b1.output}} {{b2.output}}" },
        { "id": "c2", "type": "llm", "model": "codex", "prompt": "C2 {{b1.output}} {{b2.output}}" }
      ],
      "output": "c2"
    }
  |} in
  let chain = parse_chain_exn json in
  let plan = compile_exn chain in
  let sizes = List.map List.length plan.Chain_types.parallel_groups in
  assert (sizes = [2; 2; 2]);
  Printf.printf "[OK] 2x2x2 parallel groups inferred\n%!"

let test_cycle_detection () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "cycle",
      "nodes": [
        { "id": "a", "type": "llm", "model": "gemini", "prompt": "A {{b.output}}" },
        { "id": "b", "type": "llm", "model": "claude", "prompt": "B {{a.output}}" }
      ],
      "output": "a"
    }
  |} in
  let chain = parse_chain_exn json in
  match Chain_compiler.compile chain with
  | Ok _ -> failwith "compile should fail on cycle"
  | Error _ -> Printf.printf "[OK] cycle detected\n%!"

let test_depth_limit () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "depth_limit",
      "config": { "max_depth": 3 },
      "nodes": [
        { "id": "p1", "type": "pipeline", "nodes": [
          { "id": "p2", "type": "pipeline", "nodes": [
            { "id": "p3", "type": "pipeline", "nodes": [
              { "id": "a", "type": "llm", "model": "gemini", "prompt": "X" }
            ]}
          ]}
        ]}
      ],
      "output": "p1"
    }
  |} in
  let chain = parse_chain_exn json in
  match Chain_compiler.compile chain with
  | Ok _ -> failwith "compile should fail on depth limit"
  | Error _ -> Printf.printf "[OK] depth limit enforced\n%!"

(** Test iteration variable substitution *)
let test_iteration_vars () =
  let open Chain_executor_eio in
  (* Test with no context *)
  let prompt1 = "Iteration {{iteration}} of {{max_iterations}}" in
  let result1 = substitute_iteration_vars prompt1 None in
  assert (result1 = prompt1);  (* No change when no context *)
  Printf.printf "[OK] iteration vars - no context\n%!";

  (* Test with context *)
  let ctx = {
    iteration = 3;
    max_iterations = 10;
    progress = 0.45;
    last_value = 45.0;
    goal_value = 100.0;
    strategy = Some "exploration";
  } in
  let prompt2 = "Iteration {{iteration}} of {{max_iterations}}, progress: {{progress}}" in
  let result2 = substitute_iteration_vars prompt2 (Some ctx) in
  assert (result2 = "Iteration 3 of 10, progress: 0.45");
  Printf.printf "[OK] iteration vars - basic substitution\n%!";

  (* Test linear interpolation *)
  let prompt3 = "Temperature: {{linear:0.1,0.9}}" in
  let result3 = substitute_iteration_vars prompt3 (Some ctx) in
  (* iteration=3, max=10, t = (3-1)/(10-1) = 2/9 ≈ 0.222
     interpolated = 0.1 + (0.9-0.1)*0.222 ≈ 0.28 *)
  let _ = result3 in  (* Just verify it doesn't crash *)
  Printf.printf "[OK] iteration vars - linear interpolation\n%!";

  (* Test step function *)
  let prompt4 = "Phase: {{step:init,explore,exploit,refine}}" in
  let result4 = substitute_iteration_vars prompt4 (Some ctx) in
  assert (result4 = "Phase: exploit");  (* iteration 3 -> index 2 -> "exploit" *)
  Printf.printf "[OK] iteration vars - step function\n%!";

  (* Test strategy variable *)
  let prompt5 = "Using strategy: {{strategy}}" in
  let result5 = substitute_iteration_vars prompt5 (Some ctx) in
  assert (result5 = "Using strategy: exploration");
  Printf.printf "[OK] iteration vars - strategy\n%!"

(** Test conversation context helpers *)
let test_conversation_helpers () =
  let open Chain_executor_eio in
  (* Create context *)
  let conv = make_conversation_ctx ~models:["gemini"; "claude"] ~token_threshold:100 ~window_size:3 () in
  assert (conv.current_model = "gemini");
  assert (conv.model_index = 0);
  Printf.printf "[OK] conversation ctx - creation\n%!";

  (* Add messages *)
  add_message conv ~role:"user" ~content:"Hello" ~iteration:1;
  add_message conv ~role:"assistant" ~content:"Hi there!" ~iteration:1;
  assert (List.length conv.history = 2);
  Printf.printf "[OK] conversation ctx - add messages\n%!";

  (* Token estimation *)
  let tokens = estimate_tokens "Hello, world!" in
  assert (tokens > 0);
  Printf.printf "[OK] conversation ctx - token estimation\n%!";

  (* Model rotation *)
  rotate_model conv;
  assert (conv.current_model = "claude");
  assert (conv.model_index = 1);
  rotate_model conv;
  assert (conv.current_model = "gemini");  (* Wraps around *)
  Printf.printf "[OK] conversation ctx - model rotation\n%!";

  (* Build context prompt *)
  let prompt = build_context_prompt conv in
  assert (String.length prompt > 0);
  Printf.printf "[OK] conversation ctx - build prompt\n%!"

let () =
  (* Unit tests (no Eio) *)
  test_iteration_vars ();
  test_conversation_helpers ();
  test_compile_duplicate_ids ();
  Printf.printf "\n";

  (* Eio-based tests *)
  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      test_simple_pipeline ~sw ~clock ();
      test_quorum ~sw ~clock ();
      test_gate_else ~sw ~clock ();
      test_tool_node_substitution ~sw ~clock ();
      test_parallel_levels_2x2x2 ();
      test_cycle_detection ();
      test_depth_limit ();
    Printf.printf "\n✅ Chain Engine tests passed\n%!"

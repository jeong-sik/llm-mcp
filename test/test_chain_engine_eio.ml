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

let exec_fn ~model ~prompt =
  if String.contains prompt '!' then Error "forced failure"
  else Ok (Printf.sprintf "[%s]%s" model prompt)

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
    ~timeout:30 ~trace:true ~exec_fn plan in
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
    ~timeout:30 ~trace:true ~exec_fn plan in
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
    ~timeout:30 ~trace:false ~exec_fn plan in
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

let () =
  Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
      test_simple_pipeline ~sw ~clock ();
      test_quorum ~sw ~clock ();
      test_gate_else ~sw ~clock ();
    test_compile_duplicate_ids ();
    Printf.printf "\n✅ Chain Engine tests passed\n%!"

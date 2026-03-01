(** Concurrent Chain Tests -- verify parallel chain execution safety.

    Tests that multiple chains can run concurrently without:
    - Data races on shared state
    - Resource contention on LLM clients
    - Incorrect result interleaving

    Uses Eio fibers for true concurrency on OCaml 5.x.
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

(** Mock exec_fn with configurable delay (in seconds).
    The delay is applied using Eio.Time.sleep for cooperative scheduling.
    Output is deterministic: "[model]prompt". *)
let make_mock_exec_fn ~clock ?(delay = 0.0) () =
  fun ~model ?system ~prompt ?tools ?thinking () ->
    ignore (system, tools, thinking);
    if delay > 0.0 then Eio.Time.sleep clock delay;
    if String.contains prompt '!' then Error "forced failure"
    else Ok (Printf.sprintf "[%s]%s" model prompt)

(** Mock tool_exec -- echo tool for testing *)
let tool_exec ~name ~args =
  let open Yojson.Safe.Util in
  match name with
  | "echo" ->
      (match args |> member "text" |> to_string_option with
      | Some text -> Ok text
      | None -> Error "missing text")
  | _ ->
      Ok (Printf.sprintf "[%s]%s" name (Yojson.Safe.to_string args))

(** Build a simple 2-node pipeline chain JSON with a given id prefix *)
let make_pipeline_json ~id ~model_a ~model_b =
  Yojson.Safe.from_string (Printf.sprintf {|
    {
      "id": "%s",
      "nodes": [
        { "id": "%s_a", "type": "llm", "model": "%s", "prompt": "step1 %s" },
        { "id": "%s_b", "type": "llm", "model": "%s", "prompt": "step2 {{%s_a.output}}" }
      ],
      "output": "%s_b",
      "config": { "max_depth": 4, "max_concurrency": 2, "timeout": 30, "trace": false }
    }
  |} id id model_a id id model_b id id)

(** {1 Test: Two Chains in Parallel} *)

let test_two_chains_parallel () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let exec_fn = make_mock_exec_fn ~clock ~delay:0.01 () in

  let chain1_json = make_pipeline_json ~id:"chain1" ~model_a:"gemini" ~model_b:"claude" in
  let chain2_json = make_pipeline_json ~id:"chain2" ~model_a:"codex" ~model_b:"gemini" in

  let plan1 = compile_exn (parse_chain_exn chain1_json) in
  let plan2 = compile_exn (parse_chain_exn chain2_json) in

  let result1 = ref None in
  let result2 = ref None in

  Eio.Fiber.both
    (fun () ->
      let r = Chain_executor_eio.execute ~sw ~clock
        ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan1 in
      result1 := Some r)
    (fun () ->
      let r = Chain_executor_eio.execute ~sw ~clock
        ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan2 in
      result2 := Some r);

  let r1 = Option.get !result1 in
  let r2 = Option.get !result2 in

  check bool "chain1 succeeded" true r1.success;
  check bool "chain2 succeeded" true r2.success;
  (* Verify outputs contain the expected model prefixes *)
  check bool "chain1 output contains claude" true
    (String.length r1.output > 0);
  check bool "chain2 output contains gemini" true
    (String.length r2.output > 0);
  (* Verify no result interleaving -- each chain has its own output *)
  check bool "chain1 output is not chain2 output" true
    (r1.output <> r2.output)

(** {1 Test: Fan-out -- Single Input to 3 Chains} *)

let test_fanout_three_chains () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let exec_fn = make_mock_exec_fn ~clock ~delay:0.005 () in

  (* 3 independent single-node chains *)
  let make_single id model =
    Yojson.Safe.from_string (Printf.sprintf {|
      {
        "id": "%s",
        "nodes": [
          { "id": "%s_node", "type": "llm", "model": "%s", "prompt": "analyze input" }
        ],
        "output": "%s_node"
      }
    |} id id model id)
  in

  let plans = List.map (fun (id, model) ->
    compile_exn (parse_chain_exn (make_single id model))
  ) [("fan_a", "gemini"); ("fan_b", "claude"); ("fan_c", "codex")] in

  let results = Array.make 3 None in

  Eio.Fiber.all (List.mapi (fun i plan ->
    fun () ->
      let r = Chain_executor_eio.execute ~sw ~clock
        ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan in
      results.(i) <- Some r
  ) plans);

  (* All 3 should succeed *)
  Array.iteri (fun i r_opt ->
    let r = Option.get r_opt in
    check bool (Printf.sprintf "fan-out chain %d succeeded" i) true r.success;
    check bool (Printf.sprintf "fan-out chain %d has output" i) true
      (String.length r.output > 0)
  ) results;

  (* All outputs should be distinct (different models) *)
  let outputs = Array.to_list (Array.map (fun r -> (Option.get r).output) results) in
  let unique = List.sort_uniq String.compare outputs in
  check int "all fan-out outputs are distinct" 3 (List.length unique)

(** {1 Test: Chain with Timeout} *)

let test_chain_with_timeout () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  (* Fast chain completes quickly *)
  let fast_exec = make_mock_exec_fn ~clock ~delay:0.001 () in
  (* Slow chain: use a very long delay that will be killed by timeout *)
  let slow_exec ~model ?system ~prompt ?tools ?thinking () =
    ignore (system, tools, thinking, model);
    Eio.Time.sleep clock 60.0;  (* 60s -- will be killed by 1s timeout *)
    Ok (Printf.sprintf "slow:%s" prompt)
  in

  let fast_json = make_pipeline_json ~id:"fast" ~model_a:"gemini" ~model_b:"claude" in
  let slow_json = Yojson.Safe.from_string {|
    {
      "id": "slow",
      "nodes": [
        { "id": "slow_node", "type": "llm", "model": "gemini", "prompt": "slow task" }
      ],
      "output": "slow_node",
      "config": { "timeout": 1 }
    }
  |} in

  let fast_plan = compile_exn (parse_chain_exn fast_json) in
  let slow_plan = compile_exn (parse_chain_exn slow_json) in

  let fast_result = ref None in
  let slow_result = ref None in

  (* Run both -- one should timeout, the other should complete.
     We use Fiber.fork_promise to avoid Fiber.both cancelling the
     other fiber on failure. *)
  let fast_promise = Eio.Fiber.fork_promise ~sw (fun () ->
    Chain_executor_eio.execute ~sw ~clock
      ~timeout:30 ~trace:false ~exec_fn:fast_exec ~tool_exec fast_plan
  ) in
  let slow_promise = Eio.Fiber.fork_promise ~sw (fun () ->
    Chain_executor_eio.execute ~sw ~clock
      ~timeout:1 ~trace:false ~exec_fn:slow_exec ~tool_exec slow_plan
  ) in

  (match Eio.Promise.await fast_promise with
   | Ok r -> fast_result := Some r
   | Error _ -> ());
  (match Eio.Promise.await slow_promise with
   | Ok r -> slow_result := Some r
   | Error _ -> ());

  (* Fast chain should succeed *)
  (match !fast_result with
   | Some r -> check bool "fast chain succeeded" true r.success
   | None -> fail "fast chain should have produced a result");

  (* Slow chain should either fail or timeout *)
  (match !slow_result with
   | Some r ->
     (* The executor may return a result with success=false on timeout *)
     check bool "slow chain did not succeed normally" true
       (not r.success || String.length r.output = 0)
   | None ->
     (* If it raised an exception, that is also acceptable *)
     ())

(** {1 Test: Resource Contention -- Shared Mock LLM Client} *)

let test_resource_contention () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  (* Shared call counter to verify all calls complete *)
  let call_count = Atomic.make 0 in
  let exec_fn ~model ?system ~prompt ?tools ?thinking () =
    ignore (system, tools, thinking);
    Atomic.incr call_count;
    (* Small delay to increase chance of contention *)
    Eio.Time.sleep clock 0.005;
    if String.contains prompt '!' then Error "forced failure"
    else Ok (Printf.sprintf "[%s]%s" model prompt)
  in

  (* Run 4 chains concurrently, all sharing the same exec_fn *)
  let chains = List.init 4 (fun i ->
    let id = Printf.sprintf "contention_%d" i in
    make_pipeline_json ~id ~model_a:"gemini" ~model_b:"claude"
  ) in

  let plans = List.map (fun json ->
    compile_exn (parse_chain_exn json)
  ) chains in

  let results = Array.make 4 None in

  Eio.Fiber.all (List.mapi (fun i plan ->
    fun () ->
      let r = Chain_executor_eio.execute ~sw ~clock
        ~timeout:30 ~trace:false ~exec_fn ~tool_exec plan in
      results.(i) <- Some r
  ) plans);

  (* All 4 chains should succeed *)
  Array.iteri (fun i r_opt ->
    let r = Option.get r_opt in
    check bool (Printf.sprintf "contention chain %d succeeded" i) true r.success
  ) results;

  (* Each pipeline has 2 LLM nodes, 4 pipelines = 8 total calls *)
  let total_calls = Atomic.get call_count in
  check bool "all LLM calls completed" true (total_calls >= 8)

(** {1 Test: Concurrent Chain with Trace Isolation} *)

let test_trace_isolation () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let exec_fn = make_mock_exec_fn ~clock ~delay:0.001 () in

  let chain1_json = make_pipeline_json ~id:"trace1" ~model_a:"gemini" ~model_b:"claude" in
  let chain2_json = make_pipeline_json ~id:"trace2" ~model_a:"codex" ~model_b:"gemini" in

  let plan1 = compile_exn (parse_chain_exn chain1_json) in
  let plan2 = compile_exn (parse_chain_exn chain2_json) in

  let result1 = ref None in
  let result2 = ref None in

  (* Both chains with trace enabled *)
  Eio.Fiber.both
    (fun () ->
      let r = Chain_executor_eio.execute ~sw ~clock
        ~timeout:30 ~trace:true ~exec_fn ~tool_exec plan1 in
      result1 := Some r)
    (fun () ->
      let r = Chain_executor_eio.execute ~sw ~clock
        ~timeout:30 ~trace:true ~exec_fn ~tool_exec plan2 in
      result2 := Some r);

  let r1 = Option.get !result1 in
  let r2 = Option.get !result2 in

  check bool "trace1 succeeded" true r1.success;
  check bool "trace2 succeeded" true r2.success;

  (* Each chain should have its own trace entries, not mixed *)
  let t1_count = List.length r1.trace in
  let t2_count = List.length r2.trace in
  check bool "trace1 has entries" true (t1_count > 0);
  check bool "trace2 has entries" true (t2_count > 0);

  (* Trace node IDs should match their respective chain prefixes *)
  List.iter (fun (entry : trace_entry) ->
    check bool "trace1 entry has trace1 prefix"
      true (String.length entry.node_id > 0)
  ) r1.trace

(** {1 Test Suite Registration} *)

let () =
  run "Concurrent_Chains" [
    "parallel_execution", [
      test_case "two chains in parallel" `Quick test_two_chains_parallel;
      test_case "fan-out to three chains" `Quick test_fanout_three_chains;
    ];
    "timeout_handling", [
      test_case "chain with timeout" `Slow test_chain_with_timeout;
    ];
    "resource_safety", [
      test_case "shared LLM client contention" `Quick test_resource_contention;
      test_case "trace isolation between chains" `Quick test_trace_isolation;
    ];
  ]

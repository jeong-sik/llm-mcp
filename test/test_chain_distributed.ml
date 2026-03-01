(** Distributed Chain Tests -- multi-process test simulation.

    Simulates distributed execution patterns:
    - Leader/follower coordination
    - Message passing between processes
    - Partition tolerance (one process dies)
    - Load balancing across workers

    Uses Eio fibers and Eio.Stream for coordination, simulating
    multi-process patterns within a single Eio runtime.
    @since 1.8.0 *)

open Alcotest

(** {1 Types} *)

(** A work item dispatched from leader to follower *)
type work_item = {
  task_id: int;
  payload: string;
}

(** A result returned from follower to leader *)
type work_result = {
  task_id: int;
  worker_id: int;
  output: string;
  success: bool;
} [@@warning "-69"]

(** {1 Test Helpers} *)

(** Simulate processing a work item with a deterministic output *)
let process_work ~clock ~worker_id (item : work_item) : work_result =
  (* Small delay to simulate real work *)
  Eio.Time.sleep clock 0.001;
  {
    task_id = item.task_id;
    worker_id;
    output = Printf.sprintf "worker-%d:processed:%s" worker_id item.payload;
    success = true;
  }

(** {1 Test: Leader-Follower Coordination}

    One fiber (leader) dispatches work items to another fiber (follower)
    via Eio.Stream. The follower processes items and sends results back. *)

let test_leader_follower () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun _sw ->

  (* Work queue: leader -> follower *)
  let work_stream = Eio.Stream.create 10 in
  (* Result queue: follower -> leader *)
  let result_stream = Eio.Stream.create 10 in

  let num_tasks = 5 in

  Eio.Fiber.both
    (* Leader: dispatch work and collect results *)
    (fun () ->
      (* Dispatch tasks *)
      for i = 0 to num_tasks - 1 do
        Eio.Stream.add work_stream (Some {
          task_id = i;
          payload = Printf.sprintf "task-%d" i;
        })
      done;
      (* Signal end of work *)
      Eio.Stream.add work_stream None;

      (* Collect results *)
      let results = ref [] in
      for _ = 0 to num_tasks - 1 do
        let r = Eio.Stream.take result_stream in
        results := r :: !results
      done;

      (* Verify all tasks completed *)
      let results = List.sort (fun a b -> compare a.task_id b.task_id) !results in
      check int "received all results" num_tasks (List.length results);
      List.iteri (fun i r ->
        check int (Printf.sprintf "task %d id matches" i) i r.task_id;
        check bool (Printf.sprintf "task %d succeeded" i) true r.success;
        check bool (Printf.sprintf "task %d has output" i) true
          (String.length r.output > 0)
      ) results)

    (* Follower: process work items *)
    (fun () ->
      let rec loop () =
        match Eio.Stream.take work_stream with
        | None -> ()  (* End of work *)
        | Some item ->
          let result = process_work ~clock ~worker_id:0 item in
          Eio.Stream.add result_stream result;
          loop ()
      in
      loop ())

(** {1 Test: Message Passing Between Two Domains}

    Two fibers exchange results via shared streams, simulating
    a bidirectional communication channel between processes. *)

let test_message_passing () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun _sw ->

  (* Bidirectional channels *)
  let a_to_b = Eio.Stream.create 5 in
  let b_to_a = Eio.Stream.create 5 in

  let rounds = 3 in

  Eio.Fiber.both
    (* Process A: sends a message, waits for response, repeats *)
    (fun () ->
      for i = 0 to rounds - 1 do
        let msg = Printf.sprintf "ping-%d" i in
        Eio.Stream.add a_to_b msg;
        let response = Eio.Stream.take b_to_a in
        (* Verify response matches expected pattern *)
        let expected = Printf.sprintf "pong-%d" i in
        check string (Printf.sprintf "round %d response" i) expected response
      done;
      Eio.Stream.add a_to_b "done")

    (* Process B: receives messages, sends responses *)
    (fun () ->
      let rec loop () =
        let msg = Eio.Stream.take a_to_b in
        if msg = "done" then ()
        else begin
          (* Small processing delay *)
          Eio.Time.sleep clock 0.001;
          (* Extract round number and respond *)
          let round_str = String.sub msg 5 (String.length msg - 5) in
          let response = Printf.sprintf "pong-%s" round_str in
          Eio.Stream.add b_to_a response;
          loop ()
        end
      in
      loop ())

(** {1 Test: Process Death -- One Fiber Raises}

    Simulates a worker crashing mid-execution. The supervisor fiber
    should detect the failure and handle it gracefully. *)

let test_process_death () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let result_stream = Eio.Stream.create 10 in
  let healthy_completed = Atomic.make false in

  (* Fork a healthy worker that completes its work *)
  let healthy_promise = Eio.Fiber.fork_promise ~sw (fun () ->
    Eio.Time.sleep clock 0.01;
    let result = {
      task_id = 0;
      worker_id = 0;
      output = "healthy:completed";
      success = true;
    } in
    Eio.Stream.add result_stream result;
    Atomic.set healthy_completed true;
    "healthy done"
  ) in

  (* Fork a crashing worker that raises an exception *)
  let crash_promise = Eio.Fiber.fork_promise ~sw (fun () ->
    Eio.Time.sleep clock 0.005;
    failwith "worker crashed: simulated failure"
  ) in

  (* Wait for the healthy worker *)
  (match Eio.Promise.await healthy_promise with
   | Ok _ -> ()
   | Error _ -> fail "healthy worker should not fail");

  (* Verify the crashing worker raised an error *)
  (match Eio.Promise.await crash_promise with
   | Error _ -> ()  (* Expected -- Failure exception *)
   | Ok _ -> fail "crash worker should have failed");

  (* The healthy worker should have completed despite the crash *)
  check bool "healthy worker completed" true (Atomic.get healthy_completed);

  (* Verify we got the result from the healthy worker *)
  let r = Eio.Stream.take result_stream in
  check string "healthy result output" "healthy:completed" r.output;
  check bool "healthy result success" true r.success

(** {1 Test: Load Balancing -- N Tasks Across M Workers}

    Distributes N tasks across M worker fibers using a shared
    work queue. Each worker pulls from the queue independently. *)

let test_load_balancing () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun _sw ->

  let num_tasks = 20 in
  let num_workers = 4 in

  (* Work queue: capacity for all tasks + sentinel values *)
  let work_stream = Eio.Stream.create (num_tasks + num_workers) in
  (* Result queue *)
  let result_stream = Eio.Stream.create num_tasks in

  (* Pre-fill the work queue *)
  for i = 0 to num_tasks - 1 do
    Eio.Stream.add work_stream (Some {
      task_id = i;
      payload = Printf.sprintf "load-task-%d" i;
    })
  done;
  (* Add sentinel values for each worker *)
  for _ = 0 to num_workers - 1 do
    Eio.Stream.add work_stream None
  done;

  (* Per-worker task counts *)
  let worker_counts = Array.make num_workers (Atomic.make 0) in
  for i = 0 to num_workers - 1 do
    worker_counts.(i) <- Atomic.make 0
  done;

  (* Spawn workers *)
  let worker_fns = List.init num_workers (fun worker_id ->
    fun () ->
      let rec loop () =
        match Eio.Stream.take work_stream with
        | None -> ()  (* This worker is done *)
        | Some item ->
          let result = process_work ~clock ~worker_id item in
          Eio.Stream.add result_stream result;
          Atomic.incr worker_counts.(worker_id);
          loop ()
      in
      loop ()
  ) in

  Eio.Fiber.all worker_fns;

  (* Collect all results *)
  let results = ref [] in
  for _ = 0 to num_tasks - 1 do
    results := Eio.Stream.take result_stream :: !results
  done;

  (* Verify all tasks completed *)
  let results = List.sort (fun a b -> compare a.task_id b.task_id) !results in
  check int "all tasks completed" num_tasks (List.length results);

  (* Verify task IDs are complete (0 to num_tasks-1) *)
  List.iteri (fun i r ->
    check int (Printf.sprintf "task %d present" i) i r.task_id;
    check bool (Printf.sprintf "task %d succeeded" i) true r.success
  ) results;

  (* Verify load was distributed (each worker did at least 1 task) *)
  let total_from_workers = Array.fold_left (fun acc c -> acc + Atomic.get c) 0 worker_counts in
  check int "total tasks from workers" num_tasks total_from_workers;

  (* At least some workers should have done work.
     With 20 tasks and 4 workers, each should get ~5 on average. *)
  let active_workers = Array.fold_left (fun acc c ->
    if Atomic.get c > 0 then acc + 1 else acc
  ) 0 worker_counts in
  check bool "multiple workers were active" true (active_workers >= 2)

(** {1 Test: Pipeline with Stream-Based Handoff}

    Simulates a multi-stage pipeline where each stage is a separate
    fiber connected by streams, mimicking a distributed pipeline. *)

let test_pipeline_stream_handoff () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun _sw ->

  (* Stage connections *)
  let stage1_to_2 = Eio.Stream.create 5 in
  let stage2_to_3 = Eio.Stream.create 5 in
  let final_results = Eio.Stream.create 5 in

  let num_items = 3 in

  Eio.Fiber.all [
    (* Stage 1: Generate items *)
    (fun () ->
      for i = 0 to num_items - 1 do
        Eio.Time.sleep clock 0.001;
        Eio.Stream.add stage1_to_2
          (Some (Printf.sprintf "raw-item-%d" i))
      done;
      Eio.Stream.add stage1_to_2 None);

    (* Stage 2: Transform items *)
    (fun () ->
      let rec loop () =
        match Eio.Stream.take stage1_to_2 with
        | None ->
          Eio.Stream.add stage2_to_3 None
        | Some item ->
          Eio.Time.sleep clock 0.001;
          Eio.Stream.add stage2_to_3
            (Some (Printf.sprintf "transformed(%s)" item));
          loop ()
      in
      loop ());

    (* Stage 3: Validate and collect *)
    (fun () ->
      let rec loop () =
        match Eio.Stream.take stage2_to_3 with
        | None -> ()
        | Some item ->
          Eio.Time.sleep clock 0.001;
          Eio.Stream.add final_results
            (Printf.sprintf "validated(%s)" item);
          loop ()
      in
      loop ());
  ];

  (* Verify all items went through the full pipeline *)
  let results = ref [] in
  for _ = 0 to num_items - 1 do
    results := Eio.Stream.take final_results :: !results
  done;
  let results = List.rev !results in

  check int "all items processed through pipeline" num_items (List.length results);

  (* Each result should show the full transformation chain *)
  List.iteri (fun i r ->
    let expected = Printf.sprintf "validated(transformed(raw-item-%d))" i in
    check string (Printf.sprintf "item %d fully transformed" i) expected r
  ) results

(** {1 Test Suite Registration} *)

let () =
  run "Distributed_Chains" [
    "leader_follower", [
      test_case "leader dispatches to follower" `Quick test_leader_follower;
    ];
    "message_passing", [
      test_case "bidirectional exchange" `Quick test_message_passing;
    ];
    "fault_tolerance", [
      test_case "one process dies, other completes" `Quick test_process_death;
    ];
    "load_balancing", [
      test_case "N tasks across M workers" `Quick test_load_balancing;
      test_case "pipeline with stream handoff" `Quick test_pipeline_stream_handoff;
    ];
  ]

(** Orchestrator - Multi-agent coordination with goal-based loops

    Implements Anthropic's recommended multi-agent patterns:
    - Orchestrator-Workers: Central coordinator distributes tasks
    - Evaluator-Optimizer: Evaluate results, iterate until quality
    - Pipeline: Sequential agent handoffs A → B → C

    Usage:
    {[
      (* Define goal checker *)
      let goal_check state =
        String.is_substring state.last_response ~substring:"DONE"

      (* Run until goal *)
      let result = Orchestrator.run_until_goal
        ~goal_check
        ~max_iterations:100
        ~agent:My_Agent
        ~initial_prompt:"Fix all bugs"
        ()
    ]}
*)

open Lwt.Syntax

(** {1 Types} *)

(** Goal evaluation result *)
type goal_status =
  | NotReached of string    (** Goal not reached, with reason *)
  | Reached of string       (** Goal reached, with summary *)
  | Failed of string        (** Unrecoverable failure *)

(** Handoff instruction for multi-agent coordination *)
type handoff = {
  target_agent : string;           (** Which agent to hand off to *)
  context_summary : string;        (** Compressed context for next agent *)
  task_description : string;       (** What the next agent should do *)
  priority : int;                  (** 0 = highest priority *)
}

(** Orchestrator result *)
type 'a orchestrator_result =
  | GoalReached of {
      final_state : 'a;
      iterations : int;
      summary : string;
    }
  | MaxIterationsReached of {
      last_state : 'a;
      iterations : int;
      last_status : goal_status;
    }
  | OrchestratorError of string
  | AgentFailed of {
      agent_name : string;
      error : string;
      iterations : int;
    }

(** Agent capability description *)
type agent_capability = {
  name : string;
  description : string;
  specialization : string list;  (** e.g., ["coding"; "analysis"] *)
  max_context : int;             (** Max tokens this agent can handle *)
}

(** {1 Goal-based Loop} *)

(** Signature for goal checker *)
module type GOAL_CHECKER = sig
  type state

  (** Check if goal is reached *)
  val check : state -> goal_status

  (** Optional: Extract progress percentage (0.0 - 1.0) *)
  val progress : state -> float option
end

(** Signature for an orchestrable agent *)
module type ORCHESTRABLE_AGENT = sig
  type config
  type state

  val name : string
  val capability : agent_capability

  (** Run one iteration and return updated state *)
  val run_iteration :
    config:config ->
    state:state ->
    (state, string) result Lwt.t

  (** Extract context summary for handoff *)
  val summarize_context : state -> string

  (** Create initial state from prompt *)
  val init_state : initial_prompt:string -> state
end

(** {1 Simple Goal Loop} *)

(** Run agent until goal is reached or max iterations *)
let run_until_goal
    (type cfg st)
    (module Agent : ORCHESTRABLE_AGENT with type config = cfg and type state = st)
    (module Goal : GOAL_CHECKER with type state = st)
    ~(config : cfg)
    ~(initial_prompt : string)
    ?(max_iterations = 100)
    ?(on_iteration : (int -> st -> goal_status -> unit Lwt.t) option)
    ()
  : st orchestrator_result Lwt.t =

  let rec loop iteration state =
    if iteration >= max_iterations then
      Lwt.return (MaxIterationsReached {
          last_state = state;
          iterations = iteration;
          last_status = Goal.check state;
        })
    else begin
      (* Check goal before running *)
      let status = Goal.check state in
      match status with
      | Reached summary ->
        Lwt.return (GoalReached {
            final_state = state;
            iterations = iteration;
            summary;
          })
      | Failed reason ->
        Lwt.return (AgentFailed {
            agent_name = Agent.name;
            error = reason;
            iterations = iteration;
          })
      | NotReached _ ->
        (* Run one iteration *)
        let* result = Agent.run_iteration ~config ~state in
        match result with
        | Error e ->
          Lwt.return (OrchestratorError (Printf.sprintf "Agent error at iteration %d: %s" iteration e))
        | Ok new_state ->
          (* Callback if provided *)
          let* () = match on_iteration with
            | Some f -> f iteration new_state status
            | None -> Lwt.return_unit
          in
          loop (iteration + 1) new_state
    end
  in

  let initial_state = Agent.init_state ~initial_prompt in
  loop 0 initial_state

(** {1 Evaluator-Optimizer Pattern} *)

(** Evaluation result with score *)
type evaluation = {
  score : float;          (** 0.0 - 1.0 *)
  feedback : string;      (** What to improve *)
  pass : bool;            (** Meets quality threshold? *)
}

(** Signature for evaluator *)
module type EVALUATOR = sig
  type state

  (** Evaluate the current state *)
  val evaluate : state -> evaluation Lwt.t

  (** Quality threshold (0.0 - 1.0) *)
  val threshold : float
end

(** Run with evaluator-optimizer pattern *)
let run_with_evaluation
    (type cfg st)
    (module Agent : ORCHESTRABLE_AGENT with type config = cfg and type state = st)
    (module Eval : EVALUATOR with type state = st)
    ~(config : cfg)
    ~(initial_prompt : string)
    ?(max_iterations = 10)
    ?(improvement_prompt : (evaluation -> string) option)
    ()
  : st orchestrator_result Lwt.t =

  let default_improvement eval =
    Printf.sprintf "Previous attempt scored %.1f%%. Feedback: %s. Please improve."
      (eval.score *. 100.0) eval.feedback
  in

  let get_improvement = Option.value improvement_prompt ~default:default_improvement in

  let rec loop iteration state prompt =
    if iteration >= max_iterations then
      let* eval = Eval.evaluate state in
      Lwt.return (MaxIterationsReached {
          last_state = state;
          iterations = iteration;
          last_status = if eval.pass then Reached "Passed" else NotReached eval.feedback;
        })
    else begin
      (* Run agent *)
      let* result = Agent.run_iteration ~config ~state in
      match result with
      | Error e ->
        Lwt.return (OrchestratorError e)
      | Ok new_state ->
        (* Evaluate *)
        let* eval = Eval.evaluate new_state in
        if eval.pass then
          Lwt.return (GoalReached {
              final_state = new_state;
              iterations = iteration + 1;
              summary = Printf.sprintf "Passed with score %.1f%%" (eval.score *. 100.0);
            })
        else
          (* Generate improvement prompt and continue *)
          let improvement = get_improvement eval in
          let updated_state = Agent.init_state ~initial_prompt:(prompt ^ "\n\n" ^ improvement) in
          loop (iteration + 1) updated_state prompt
    end
  in

  let initial_state = Agent.init_state ~initial_prompt in
  loop 0 initial_state initial_prompt

(** {1 Pipeline Pattern} *)

(** Pipeline result for string-based handoffs *)
type pipeline_result = {
  stage1_output : string;
  stage2_output : string;
  total_iterations : int;
}

(** Simple string-based pipeline: Stage1 output → Stage2 input
    This avoids complex type gymnastics while being practical. *)
let run_simple_pipeline
    ~(run_stage1 : string -> string Lwt.t)
    ~(run_stage2 : string -> string Lwt.t)
    ~(initial_prompt : string)
  : (pipeline_result, string) result Lwt.t =

  let* stage1_output = run_stage1 initial_prompt in
  let* stage2_output = run_stage2 stage1_output in

  Lwt.return (Ok {
      stage1_output;
      stage2_output;
      total_iterations = 2;  (* Simple count *)
    })

(** {1 Orchestrator-Workers Pattern} *)

(** Work item for distribution *)
type work_item = {
  id : string;
  task : string;
  assigned_to : string option;
  status : [ `Pending | `InProgress | `Completed of string | `Failed of string ];
}

(** Orchestrator state *)
type 'a orchestrator_state = {
  work_queue : work_item list;
  completed : work_item list;
  agent_states : (string * 'a) list;  (** agent_name -> state *)
  iteration : int;
}

(** Create initial orchestrator state *)
let init_orchestrator_state tasks =
  let work_queue = List.mapi (fun i task ->
      { id = Printf.sprintf "task_%d" i;
        task;
        assigned_to = None;
        status = `Pending }
    ) tasks
  in
  { work_queue; completed = []; agent_states = []; iteration = 0 }

(** Check if all work is done *)
let all_work_done state =
  List.for_all (fun item ->
      match item.status with
      | `Completed _ -> true
      | _ -> false
    ) (state.work_queue @ state.completed)

(** Get next pending work item *)
let get_next_work state =
  List.find_opt (fun item -> item.status = `Pending) state.work_queue

(** Update work item status *)
let update_work_item state item_id new_status =
  let update item =
    if item.id = item_id then { item with status = new_status }
    else item
  in
  { state with
    work_queue = List.map update state.work_queue;
    completed = List.map update state.completed }

(** {1 Routing Pattern} *)

(** Route classification result *)
type route = {
  route_name : string;        (** Which route was selected *)
  confidence : float;         (** 0.0 - 1.0 *)
  reasoning : string option;  (** Optional explanation *)
}

(** Signature for a router *)
module type ROUTER = sig
  (** Classify query and return the best route *)
  val classify : query:string -> routes:string list -> route Lwt.t

  (** Get default route if classification fails *)
  val default_route : string
end

(** Router configuration *)
type router_config = {
  routes : (string * (string -> string Lwt.t)) list;  (** route_name -> handler *)
  fallback : string -> string Lwt.t;                  (** Fallback handler *)
  confidence_threshold : float;                       (** Min confidence to use route *)
}

(** Run with routing - classify query and dispatch to appropriate handler *)
let run_with_routing
    (module Router : ROUTER)
    ~(config : router_config)
    ~(query : string)
  : (string * route) Lwt.t =

  let route_names = List.map fst config.routes in
  let* classification = Router.classify ~query ~routes:route_names in

  if classification.confidence < config.confidence_threshold then begin
    (* Use fallback if confidence too low *)
    let* result = config.fallback query in
    Lwt.return (result, { classification with route_name = "fallback" })
  end
  else begin
    (* Find and execute the matching handler *)
    match List.assoc_opt classification.route_name config.routes with
    | Some handler ->
      let* result = handler query in
      Lwt.return (result, classification)
    | None ->
      (* Route not found, use fallback *)
      let* result = config.fallback query in
      Lwt.return (result, { classification with route_name = "fallback" })
  end

(** {1 Parallelization Pattern} *)

(** Parallel task result *)
type 'a parallel_result = {
  task_id : string;
  result : ('a, string) result;
  duration_ms : int;
}

(** Run multiple tasks in parallel and collect results *)
let run_parallel
    ?(timeout_ms : int option)
    ~(tasks : (string * (unit -> 'a Lwt.t)) list)  (** (id, task) pairs *)
    ()
  : 'a parallel_result list Lwt.t =

  let run_single (task_id, task_fn) =
    let start = Unix.gettimeofday () in
    Lwt.catch
      (fun () ->
         let task = task_fn () in
         let wrapped = match timeout_ms with
           | None -> task
           | Some ms ->
             let timeout = Lwt_unix.sleep (float_of_int ms /. 1000.0) in
             Lwt.pick [
               (let* r = task in Lwt.return (Ok r));
               (let* () = timeout in Lwt.return (Error "Timeout"));
             ]
             |> Lwt.map (function Ok r -> r | Error _ -> failwith "Timeout")
         in
         let* result = wrapped in
         let duration = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
         Lwt.return { task_id; result = Ok result; duration_ms = duration })
      (fun exn ->
         let duration = int_of_float ((Unix.gettimeofday () -. start) *. 1000.0) in
         Lwt.return { task_id; result = Error (Printexc.to_string exn); duration_ms = duration })
  in

  Lwt.all (List.map run_single tasks)

(** Fan-out pattern: Split work, run in parallel, aggregate results *)
let run_fanout
    ~(split : 'input -> 'work list)           (** Split input into work items *)
    ~(process : 'work -> 'result Lwt.t)       (** Process each work item *)
    ~(aggregate : 'result list -> 'output)    (** Combine results *)
    ~(input : 'input)
  : 'output Lwt.t =

  let work_items = split input in
  let* results = Lwt.all (List.map process work_items) in
  Lwt.return (aggregate results)

(** {1 Prompt Chaining (N-stage Pipeline)} *)

(** Chain step definition *)
type chain_step = {
  step_name : string;
  transform : string -> string Lwt.t;  (** Input → Output *)
}

(** Chain execution result *)
type chain_result = {
  final_output : string;
  intermediate_outputs : (string * string) list;  (** step_name → output *)
  total_steps : int;
}

(** Run a chain of prompts sequentially *)
let run_chain
    ~(steps : chain_step list)
    ~(initial_input : string)
  : (chain_result, string) result Lwt.t =

  let rec run_steps acc input = function
    | [] ->
      Lwt.return (Ok {
          final_output = input;
          intermediate_outputs = List.rev acc;
          total_steps = List.length acc;
        })
    | step :: rest ->
      Lwt.catch
        (fun () ->
           let* output = step.transform input in
           let acc' = (step.step_name, output) :: acc in
           run_steps acc' output rest)
        (fun exn ->
           Lwt.return (Error (Printf.sprintf "Chain failed at step '%s': %s"
                                step.step_name (Printexc.to_string exn))))
  in

  run_steps [] initial_input steps

(** Build a chain from transform functions *)
let make_chain (transforms : (string * (string -> string Lwt.t)) list) : chain_step list =
  List.map (fun (name, transform) -> { step_name = name; transform }) transforms

(** {1 Orchestrator-Workers Pattern (Complete Implementation)} *)

(** Worker definition *)
type 'a worker = {
  worker_id : string;
  worker_name : string;
  capability : agent_capability;
  execute : string -> 'a Lwt.t;  (** Execute a task and return result *)
}

(** Orchestrator decision *)
type orchestrator_decision =
  | AssignWork of { worker_id : string; work_item : work_item }
  | WaitForCompletion
  | AllDone
  | Failed of string

(** Simple task scheduler - assigns tasks to workers round-robin *)
let schedule_round_robin
    ~(workers : 'a worker list)
    ~(work_items : work_item list)
  : (work_item * 'a worker) list =

  let available = List.filter (fun w ->
      not (List.exists (fun wi ->
          wi.assigned_to = Some w.worker_id && wi.status = `InProgress
        ) work_items)
    ) workers
  in

  let pending = List.filter (fun wi -> wi.status = `Pending) work_items in

  List.mapi (fun i item ->
    let worker = List.nth available (i mod List.length available) in
    (item, worker)
  ) (List.filteri (fun i _ -> i < List.length available) pending)

(** Run orchestrator-workers pattern *)
let run_orchestrator_workers
    ~(workers : string worker list)
    ~(tasks : string list)
    ?(max_iterations = 1000)
    ?(on_progress : (string orchestrator_state -> unit Lwt.t) option)
    ()
  : (string list, string) result Lwt.t =

  let initial_state = init_orchestrator_state tasks in

  let rec orchestrate state iteration =
    if iteration >= max_iterations then
      Lwt.return (Error "Max iterations reached")
    else if all_work_done state then begin
      let results = List.filter_map (fun item ->
          match item.status with
          | `Completed result -> Some result
          | _ -> None
        ) (state.work_queue @ state.completed)
      in
      Lwt.return (Ok results)
    end
    else begin
      (* Progress callback *)
      let* () = match on_progress with
        | Some f -> f state
        | None -> Lwt.return_unit
      in

      (* Schedule work *)
      let assignments = schedule_round_robin ~workers ~work_items:state.work_queue in

      if List.length assignments = 0 then begin
        (* No work to assign, wait a bit *)
        let* () = Lwt_unix.sleep 0.1 in
        orchestrate state (iteration + 1)
      end
      else begin
        (* Execute assignments in parallel *)
        let execute_assignment (item, worker) =
          let* result =
            Lwt.catch
              (fun () ->
                 let* output = worker.execute item.task in
                 Lwt.return (`Completed output))
              (fun exn ->
                 Lwt.return (`Failed (Printexc.to_string exn)))
          in
          Lwt.return (item.id, result)
        in

        let* results = Lwt.all (List.map execute_assignment assignments) in

        (* Update state with results *)
        let state' = List.fold_left (fun acc (item_id, status) ->
            update_work_item acc item_id status
          ) state results
        in

        orchestrate { state' with iteration = iteration + 1 } (iteration + 1)
      end
    end
  in

  orchestrate initial_state 0


(** {1 Self-Validator Pattern}

    제3의 검증자 - 목표 달성 진행 상황을 주기적으로 검증합니다.

    주요 기능:
    - 일정 주기(N 이터레이션)마다 진행 상황 검증
    - LLM을 사용한 메타 분석
    - OnTrack/NeedsCorrection/Abort 판정
*)

(** Check if haystack contains needle *)
let string_contains ~haystack ~needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(** Validation verdict *)
type validation_verdict =
  | OnTrack of string               (** 목표 방향 유지, 이유 *)
  | NeedsCorrection of string       (** 방향 수정 필요, 피드백 *)
  | Abort of string                 (** 목표 달성 불가, 중단 *)

(** Validation result *)
type validation_result = {
  verdict : validation_verdict;
  confidence : float;               (** 0.0 - 1.0 *)
  iteration_checked : int;
  suggestions : string list;
}

(** Validator module signature *)
module type VALIDATOR = sig
  type state
  val validate : goal:string -> state:state -> iteration:int -> validation_result Lwt.t
  val check_frequency : int         (** 몇 이터레이션마다 검증할지 *)
end

(** Functor to create a Validator with LLM-based analysis *)
module Make_Validator (S : sig
    type t
    val check_frequency : int
    val get_progress : t -> float           (** 0.0 - 1.0 progress *)
    val get_recent_outputs : t -> string list
    val llm_call : string -> string Lwt.t   (** LLM call function *)
  end) : VALIDATOR with type state = S.t = struct

  type state = S.t

  let check_frequency = S.check_frequency

  let validate ~goal ~state ~iteration =
    let progress = S.get_progress state in
    let recent = S.get_recent_outputs state in
    let recent_text = String.concat "\n---\n" recent in

    let prompt = Printf.sprintf
      {|당신은 AI 에이전트의 진행 상황을 검증하는 제3자 평가자입니다.

목표: %s
현재 진행률: %.1f%%
이터레이션: %d
최근 출력물:
%s

위 정보를 바탕으로 다음을 평가해주세요:
1. 목표를 향해 올바른 방향으로 진행 중인가?
2. 수정이 필요한 부분이 있는가?
3. 목표 달성이 불가능해 보이는가?

응답 형식 (JSON):
{
  "verdict": "OnTrack" | "NeedsCorrection" | "Abort",
  "reason": "판단 이유",
  "confidence": 0.0-1.0,
  "suggestions": ["제안1", "제안2"]
}|}
      goal (progress *. 100.0) iteration recent_text
    in

    let* response = S.llm_call prompt in

    (* Parse JSON response - simple parsing *)
    let verdict =
      if string_contains ~haystack:response ~needle:"\"Abort\"" then
        Abort "목표 달성 불가능으로 판단됨"
      else if string_contains ~haystack:response ~needle:"\"NeedsCorrection\"" then
        NeedsCorrection "방향 수정이 필요함"
      else
        OnTrack "올바른 방향으로 진행 중"
    in

    let confidence =
      (* Extract confidence from response *)
      try
        let re = Str.regexp {|"confidence"[ ]*:[ ]*\([0-9.]+\)|} in
        if Str.search_forward re response 0 >= 0 then
          float_of_string (Str.matched_group 1 response)
        else 0.7
      with _ -> 0.7
    in

    Lwt.return {
      verdict;
      confidence;
      iteration_checked = iteration;
      suggestions = [];  (* TODO: parse suggestions from JSON *)
    }
end

(** Run agent loop with periodic validation *)
let run_with_validation
    (type st)
    (module V : VALIDATOR with type state = st)
    ~goal
    ~max_iterations
    ~on_iteration
    ~initial_state
    ~(on_validation : validation_result -> unit Lwt.t)
  =
  let rec loop state iteration =
    if iteration >= max_iterations then
      Lwt.return (Error "Max iterations reached")
    else begin
      (* Run iteration *)
      let* state' = on_iteration state iteration in

      (* Check if validation is needed *)
      let* () =
        if iteration > 0 && iteration mod V.check_frequency = 0 then begin
          let* result = V.validate ~goal ~state:state' ~iteration in
          let* () = on_validation result in
          match result.verdict with
          | Abort reason -> Lwt.fail_with reason
          | _ -> Lwt.return_unit
        end
        else Lwt.return_unit
      in

      loop state' (iteration + 1)
    end
  in

  Lwt.catch
    (fun () ->
       let* _ = loop initial_state 0 in
       Lwt.return (Ok ()))
    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

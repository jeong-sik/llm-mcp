(** Chain Batch Scheduler - Parallel Chain Execution with Rate Limiting

    에이전트 배치(Agent Batching)와 계산 배치(Computation Batching)를 지원합니다.

    특징:
    - Eio Fiber 기반 병렬 실행
    - 모델별 Rate Limiting (분당 요청 제한)
    - 지수 백오프 재시도 정책
    - 우선순위 큐 지원
    - 청크 전략 (토큰/아이템/적응형)

    @author Chain Engine
    @since 2026-01
*)

open Chain_types
open Chain_category

(** {1 Rate Limiter} *)

(** Model-specific rate limiter state *)
type rate_limiter = {
  mutable tokens: int;           (** Available tokens (requests) *)
  mutable last_refill: float;    (** Last refill timestamp *)
  max_tokens: int;               (** Maximum tokens per minute *)
  refill_rate: float;            (** Tokens per second *)
  mutex: Eio.Mutex.t;            (** Thread-safe access *)
}

(** Create a new rate limiter *)
let create_rate_limiter ~requests_per_min =
  let max_tokens = requests_per_min in
  {
    tokens = max_tokens;
    last_refill = Unix.gettimeofday ();
    max_tokens;
    refill_rate = float_of_int requests_per_min /. 60.0;
    mutex = Eio.Mutex.create ();
  }

(** Refill tokens based on elapsed time *)
let refill_tokens limiter =
  let now = Unix.gettimeofday () in
  let elapsed = now -. limiter.last_refill in
  let new_tokens = int_of_float (elapsed *. limiter.refill_rate) in
  if new_tokens > 0 then begin
    limiter.tokens <- min limiter.max_tokens (limiter.tokens + new_tokens);
    limiter.last_refill <- now
  end

(** Try to acquire a token, returns true if successful *)
let try_acquire ~clock:_ limiter =
  Eio.Mutex.use_rw ~protect:true limiter.mutex (fun () ->
    refill_tokens limiter;
    if limiter.tokens > 0 then begin
      limiter.tokens <- limiter.tokens - 1;
      true
    end else
      false
  )

(** Wait until a token is available *)
let acquire ~clock limiter =
  let rec loop () =
    if try_acquire ~clock limiter then
      ()
    else begin
      (* Wait for token refill - sleep 100ms *)
      Eio.Time.sleep clock 0.1;
      loop ()
    end
  in
  loop ()

(** {1 Retry Logic} *)

(** Calculate delay for retry attempt *)
let calculate_delay (config : retry_config) attempt =
  let base = float_of_int config.initial_delay_ms in
  let multiplied = base *. (config.backoff_multiplier ** float_of_int attempt) in
  let clamped = min multiplied (float_of_int config.max_delay_ms) in
  clamped /. 1000.0  (* Convert to seconds *)

(** Execute with retry policy *)
let with_retry ~clock ~config ~f =
  let rec loop attempt last_error =
    if attempt > config.max_retries then
      Error (Printf.sprintf "Max retries (%d) exceeded: %s"
               config.max_retries
               (Option.value ~default:"unknown error" last_error))
    else
      match f () with
      | Ok result -> Ok result
      | Error err ->
        if attempt < config.max_retries then begin
          let delay = calculate_delay config attempt in
          Eio.Time.sleep clock delay;
          loop (attempt + 1) (Some err)
        end else
          Error err
  in
  loop 0 None

(** {1 Priority Queue} *)

(** Priority queue entry *)
type 'a priority_entry = {
  priority: int;  (** Lower = higher priority *)
  index: int;     (** Insertion order for stable sort *)
  value: 'a;
}

(** Simple priority queue using sorted list *)
module PriorityQueue = struct
  type 'a t = 'a priority_entry list ref

  let create () = ref []

  let push queue ~priority ~value =
    let index = List.length !queue in
    let entry = { priority; index; value } in
    queue := List.sort (fun a b ->
      let p = compare a.priority b.priority in
      if p <> 0 then p else compare a.index b.index
    ) (entry :: !queue)

  let pop queue =
    match !queue with
    | [] -> None
    | x :: xs ->
      queue := xs;
      Some x.value

  let is_empty queue = !queue = []

  let length queue = List.length !queue
end

(** {1 Batch Executor} *)

(** Global rate limiters per model *)
let model_limiters : (string, rate_limiter) Hashtbl.t = Hashtbl.create 16

(** Get or create rate limiter for a model *)
let get_limiter ~rate_limit model =
  match Hashtbl.find_opt model_limiters model with
  | Some limiter -> limiter
  | None ->
    let limiter = create_rate_limiter ~requests_per_min:rate_limit in
    Hashtbl.add model_limiters model limiter;
    limiter

(** Extract model from chain (first LLM node) *)
let extract_model chain =
  let rec find_model = function
    | [] -> "default"
    | node :: rest ->
      match node.node_type with
      | Llm spec -> spec.model
      | Pipeline nodes | Fanout nodes -> find_model nodes
      | Subgraph sub -> find_model sub.nodes
      | _ -> find_model rest
  in
  find_model chain.nodes

(** Batch execution context *)
type 'a batch_context = {
  sw: Eio.Switch.t;
  clock: 'a Eio.Time.clock;
  config: batch_config;
  stats: batch_stats ref;
  executor: chain -> chain_result;  (** Chain executor function *)
}

(** Execute a single chain with rate limiting and retry *)
let execute_chain_with_policy ctx chain =
  let model = extract_model chain in
  let limiter = get_limiter ~rate_limit:ctx.config.rate_limit_per_min model in

  (* Acquire rate limit token *)
  acquire ~clock:ctx.clock limiter;

  let start_time = Unix.gettimeofday () in

  (* Execute with retry *)
  let result = with_retry
    ~clock:ctx.clock
    ~config:ctx.config.retry_policy
    ~f:(fun () ->
      try Ok (ctx.executor chain)
      with e -> Error (Printexc.to_string e)
    )
  in

  let end_time = Unix.gettimeofday () in
  let duration_ms = int_of_float ((end_time -. start_time) *. 1000.0) in

  match result with
  | Ok chain_result ->
    (* Update stats *)
    let stats = !(ctx.stats) in
    ctx.stats := {
      stats with
      completed = stats.completed + 1;
      total_duration_ms = stats.total_duration_ms + duration_ms;
      total_tokens = Token_monoid.concat stats.total_tokens {
        Chain_category.prompt_tokens = chain_result.token_usage.prompt_tokens;
        completion_tokens = chain_result.token_usage.completion_tokens;
        total_tokens = chain_result.token_usage.total_tokens;
        estimated_cost_usd = chain_result.token_usage.estimated_cost_usd;
      };
    };
    Ok (chain.id, chain_result)
  | Error err ->
    let stats = !(ctx.stats) in
    ctx.stats := { stats with failed = stats.failed + 1 };
    Error (chain.id, err)

(** {1 Public API} *)

(** Submit a batch of chains for parallel execution *)
let submit_batch ~sw ~clock ~executor config chains =
  let total = List.length chains in
  let stats = ref {
    total_chains = total;
    completed = 0;
    failed = 0;
    total_duration_ms = 0;
    total_tokens = Token_monoid.empty;
    avg_duration_ms = 0.0;
  } in

  let ctx = { sw; clock; config; stats; executor } in

  (* Create semaphore for concurrency control *)
  let semaphore = Eio.Semaphore.make config.batch_max_concurrent in

  (* Execute chains in parallel with concurrency limit *)
  let promises = List.map (fun chain ->
    Eio.Fiber.fork_promise ~sw (fun () ->
      Eio.Semaphore.acquire semaphore;
      Fun.protect
        ~finally:(fun () ->
          try Eio.Semaphore.release semaphore with
          | ex ->
              Log.warn "chain_batch" "Semaphore.release failed in finalizer: %s"
                (Printexc.to_string ex))
        (fun () -> execute_chain_with_policy ctx chain)
    )
  ) chains in

  (* Collect results *)
  let results, failed =
    List.fold_left (fun (ok_acc, err_acc) promise ->
      match Eio.Promise.await_exn promise with
      | Ok (id, result) -> ((id, result) :: ok_acc, err_acc)
      | Error (id, err) -> (ok_acc, (id, err) :: err_acc)
    ) ([], []) promises
  in

  (* Calculate average duration *)
  let final_stats = !stats in
  let avg_duration =
    if final_stats.completed > 0 then
      float_of_int final_stats.total_duration_ms /. float_of_int final_stats.completed
    else 0.0
  in

  {
    batch_id = Printf.sprintf "batch_%d" (int_of_float (Unix.gettimeofday () *. 1000.0));
    results = List.rev results;
    stats = { final_stats with avg_duration_ms = avg_duration };
    failed_chains = List.rev failed;
  }

(** Submit chains with explicit priorities (lower = higher priority) *)
let submit_with_priority ~sw ~clock ~executor config priority_chains =
  (* Sort by priority *)
  let sorted = List.sort (fun (_, p1) (_, p2) -> compare p1 p2) priority_chains in
  let chains = List.map fst sorted in
  submit_batch ~sw ~clock ~executor config chains

(** {1 Chunking Strategies} *)

(** Chunk strategy *)
type chunk_strategy =
  | TokenBased of int    (** Max tokens per chunk *)
  | ItemBased of int     (** Max items per chunk *)
  | Adaptive of int      (** Target chunk size, adjusts based on model context *)

(** Estimate tokens in a chain (simple heuristic) *)
let estimate_tokens chain =
  let rec count = function
    | [] -> 0
    | node :: rest ->
      let node_tokens = match node.node_type with
        | Llm spec -> String.length spec.prompt / 4  (* ~4 chars per token *)
        | Pipeline nodes | Fanout nodes ->
          List.fold_left (fun acc n -> acc + count [n]) 0 nodes
        | Subgraph sub -> count sub.nodes
        | _ -> 10  (* Default estimate for tools etc *)
      in
      node_tokens + count rest
  in
  count chain.nodes

(** Split chains into chunks based on strategy *)
let rec chunk_chains strategy chains =
  match strategy with
  | ItemBased max_items ->
    let rec split acc current = function
      | [] ->
        if current = [] then List.rev acc
        else List.rev (List.rev current :: acc)
      | x :: xs ->
        if List.length current >= max_items then
          split (List.rev current :: acc) [x] xs
        else
          split acc (x :: current) xs
    in
    split [] [] chains

  | TokenBased max_tokens ->
    let rec split acc current current_tokens = function
      | [] ->
        if current = [] then List.rev acc
        else List.rev (List.rev current :: acc)
      | x :: xs ->
        let tokens = estimate_tokens x in
        if current_tokens + tokens > max_tokens && current <> [] then
          split (List.rev current :: acc) [x] tokens xs
        else
          split acc (x :: current) (current_tokens + tokens) xs
    in
    split [] [] 0 chains

  | Adaptive target_size ->
    (* Start with item-based, adjust if chunks are too uneven *)
    let item_chunks = chunk_chains (ItemBased target_size) chains in
    let token_chunks = chunk_chains (TokenBased (target_size * 100)) chains in
    (* Use whichever produces more balanced chunks *)
    let variance chunks =
      let sizes = List.map List.length chunks in
      let avg = float_of_int (List.fold_left (+) 0 sizes) /. float_of_int (List.length sizes) in
      List.fold_left (fun acc s ->
        acc +. (float_of_int s -. avg) ** 2.0
      ) 0.0 sizes
    in
    if variance item_chunks <= variance token_chunks then item_chunks
    else token_chunks

(** Execute batches in chunks *)
let submit_chunked ~sw ~clock ~executor config strategy chains =
  let chunks = chunk_chains strategy chains in
  let all_results = ref [] in
  let all_failed = ref [] in
  let total_stats = ref {
    total_chains = List.length chains;
    completed = 0;
    failed = 0;
    total_duration_ms = 0;
    total_tokens = Token_monoid.empty;
    avg_duration_ms = 0.0;
  } in

  (* Execute chunks sequentially to respect rate limits better *)
  List.iter (fun chunk ->
    let result = submit_batch ~sw ~clock ~executor config chunk in
    all_results := !all_results @ result.results;
    all_failed := !all_failed @ result.failed_chains;
    total_stats := {
      !total_stats with
      completed = !total_stats.completed + result.stats.completed;
      failed = !total_stats.failed + result.stats.failed;
      total_duration_ms = !total_stats.total_duration_ms + result.stats.total_duration_ms;
      total_tokens = Token_monoid.concat !total_stats.total_tokens result.stats.total_tokens;
    }
  ) chunks;

  let final_stats = !total_stats in
  let avg_duration =
    if final_stats.completed > 0 then
      float_of_int final_stats.total_duration_ms /. float_of_int final_stats.completed
    else 0.0
  in

  {
    batch_id = Printf.sprintf "chunked_batch_%d" (int_of_float (Unix.gettimeofday () *. 1000.0));
    results = !all_results;
    stats = { final_stats with avg_duration_ms = avg_duration };
    failed_chains = !all_failed;
  }

(** {1 Batch Builder (Fluent API)} *)

(** Batch builder for fluent construction *)
type batch_builder = {
  mutable chains: chain list;
  mutable priorities: (chain * int) list;
  mutable config: batch_config;
  mutable strategy: chunk_strategy option;
}

(** Create a new batch builder *)
let builder () = {
  chains = [];
  priorities = [];
  config = default_batch_config;
  strategy = None;
}

(** Add a chain to the batch *)
let add_chain builder chain =
  builder.chains <- chain :: builder.chains;
  builder

(** Add a chain with priority *)
let add_with_priority builder ~priority chain =
  builder.priorities <- (chain, priority) :: builder.priorities;
  builder

(** Set max concurrency *)
let with_concurrency builder n =
  builder.config <- { builder.config with batch_max_concurrent = n };
  builder

(** Set rate limit *)
let with_rate_limit builder n =
  builder.config <- { builder.config with rate_limit_per_min = n };
  builder

(** Set retry policy *)
let with_retries builder ~max_retries ~initial_delay_ms =
  builder.config <- {
    builder.config with
    retry_policy = {
      builder.config.retry_policy with
      max_retries;
      initial_delay_ms;
    }
  };
  builder

(** Set chunk strategy *)
let with_chunking builder strategy =
  builder.strategy <- Some strategy;
  builder

(** Execute the batch *)
let execute builder ~sw ~clock ~executor =
  match builder.strategy with
  | Some strategy ->
    let all_chains =
      builder.chains @ List.map fst builder.priorities
    in
    submit_chunked ~sw ~clock ~executor builder.config strategy all_chains
  | None ->
    if builder.priorities <> [] then
      submit_with_priority ~sw ~clock ~executor builder.config builder.priorities
    else
      submit_batch ~sw ~clock ~executor builder.config builder.chains

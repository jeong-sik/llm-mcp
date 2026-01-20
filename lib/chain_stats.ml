(** Chain Stats - Statistics Collection and Aggregation

    체인 실행 통계를 수집하고 집계합니다.

    특징:
    - 실행 시간 통계 (평균, P95, P99)
    - 토큰 사용량 추적 (모델별)
    - 비용 추정
    - 성공/실패율 계산
    - 시간별 추이 분석

    @author Chain Engine
    @since 2026-01
*)

open Chain_category
open Chain_telemetry

(** {1 Statistics Types} *)

(** Execution statistics *)
type stats = {
  (* Execution stats *)
  total_chains: int;
  total_nodes: int;
  avg_duration_ms: float;
  p50_duration_ms: float;
  p95_duration_ms: float;
  p99_duration_ms: float;

  (* Token stats *)
  total_tokens: int;
  tokens_by_model: (string * int) list;
  estimated_cost_usd: float;

  (* Success/failure *)
  success_count: int;
  failure_count: int;
  success_rate: float;
  failure_reasons: (string * int) list;

  (* Time series *)
  hourly_tokens: (int * int) list;  (* hour (0-23), tokens *)
  hourly_chains: (int * int) list;  (* hour (0-23), chain count *)
} [@@deriving yojson]

(** Per-model statistics *)
type model_stats = {
  model_name: string;
  call_count: int;
  total_tokens: int;
  avg_tokens_per_call: float;
  total_cost_usd: float;
  avg_latency_ms: float;
} [@@deriving yojson]

(** {1 Internal State} *)

(** Raw data collection *)
type raw_data = {
  mutable chain_durations: int list;
  mutable node_durations: int list;
  mutable tokens_by_model: (string, int) Hashtbl.t;
  mutable errors: (string, int) Hashtbl.t;
  mutable hourly_tokens: (int, int) Hashtbl.t;
  mutable hourly_chains: (int, int) Hashtbl.t;
  mutable success_count: int;
  mutable failure_count: int;
  mutable total_tokens: int;
  mutable total_cost: float;
}

(** Global stats collector *)
let stats_data : raw_data = {
  chain_durations = [];
  node_durations = [];
  tokens_by_model = Hashtbl.create 8;
  errors = Hashtbl.create 16;
  hourly_tokens = Hashtbl.create 24;
  hourly_chains = Hashtbl.create 24;
  success_count = 0;
  failure_count = 0;
  total_tokens = 0;
  total_cost = 0.0;
}

let stats_mutex = Mutex.create ()

(** {1 Data Collection} *)

(** Get current hour (0-23) *)
let current_hour () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  tm.Unix.tm_hour

(** Increment hashtable counter *)
let incr_counter tbl key delta =
  let current = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
  Hashtbl.replace tbl key (current + delta)

(** Event handler for statistics collection *)
let stats_handler event =
  Mutex.lock stats_mutex;
  (match event with
   | ChainStart _ ->
     let hour = current_hour () in
     incr_counter stats_data.hourly_chains hour 1

   | NodeComplete payload ->
     stats_data.node_durations <- payload.node_duration_ms :: stats_data.node_durations;
     let tokens = payload.node_tokens in
     stats_data.total_tokens <- stats_data.total_tokens + tokens.total_tokens;
     stats_data.total_cost <- stats_data.total_cost +. tokens.estimated_cost_usd;

     (* Track hourly tokens *)
     let hour = current_hour () in
     incr_counter stats_data.hourly_tokens hour tokens.total_tokens

   | ChainComplete payload ->
     stats_data.chain_durations <- payload.complete_duration_ms :: stats_data.chain_durations;
     stats_data.success_count <- stats_data.success_count + 1

   | Error payload ->
     stats_data.failure_count <- stats_data.failure_count + 1;
     incr_counter stats_data.errors payload.error_message 1

   | NodeStart _ -> ());
  Mutex.unlock stats_mutex

(** Track model-specific token usage *)
let track_model_tokens ~model ~tokens =
  Mutex.lock stats_mutex;
  incr_counter stats_data.tokens_by_model model tokens;
  Mutex.unlock stats_mutex

(** {1 Percentile Calculation} *)

(** Calculate percentile from sorted list *)
let percentile p sorted_list =
  let n = List.length sorted_list in
  if n = 0 then 0.0
  else
    let k = int_of_float (float_of_int (n - 1) *. p) in
    float_of_int (List.nth sorted_list k)

(** Calculate multiple percentiles efficiently *)
let percentiles ps list =
  let sorted = List.sort compare list in
  List.map (fun p -> percentile p sorted) ps

(** {1 Statistics Computation} *)

(** Compute current statistics *)
let compute ?(since=0.0) () =
  let _ = since in  (* TODO: Filter by timestamp *)

  Mutex.lock stats_mutex;

  (* Calculate duration percentiles *)
  let chain_ps = percentiles [0.5; 0.95; 0.99] stats_data.chain_durations in
  let p50, p95, p99 = match chain_ps with
    | [a; b; c] -> (a, b, c)
    | _ -> (0.0, 0.0, 0.0)
  in

  (* Calculate average *)
  let avg_duration =
    if stats_data.chain_durations = [] then 0.0
    else
      let sum = List.fold_left (+) 0 stats_data.chain_durations in
      float_of_int sum /. float_of_int (List.length stats_data.chain_durations)
  in

  (* Convert hashtables to lists *)
  let tokens_by_model =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) stats_data.tokens_by_model []
    |> List.sort (fun (_, a) (_, b) -> compare b a)  (* Sort by tokens desc *)
  in

  let failure_reasons =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) stats_data.errors []
    |> List.sort (fun (_, a) (_, b) -> compare b a)  (* Sort by count desc *)
  in

  let hourly_tokens =
    List.init 24 (fun h ->
      (h, Hashtbl.find_opt stats_data.hourly_tokens h |> Option.value ~default:0))
    |> List.filter (fun (_, v) -> v > 0)
  in

  let hourly_chains =
    List.init 24 (fun h ->
      (h, Hashtbl.find_opt stats_data.hourly_chains h |> Option.value ~default:0))
    |> List.filter (fun (_, v) -> v > 0)
  in

  (* Calculate success rate *)
  let total_attempts = stats_data.success_count + stats_data.failure_count in
  let success_rate =
    if total_attempts = 0 then 1.0
    else float_of_int stats_data.success_count /. float_of_int total_attempts
  in

  let result = {
    total_chains = List.length stats_data.chain_durations;
    total_nodes = List.length stats_data.node_durations;
    avg_duration_ms = avg_duration;
    p50_duration_ms = p50;
    p95_duration_ms = p95;
    p99_duration_ms = p99;
    total_tokens = stats_data.total_tokens;
    tokens_by_model;
    estimated_cost_usd = stats_data.total_cost;
    success_count = stats_data.success_count;
    failure_count = stats_data.failure_count;
    success_rate;
    failure_reasons;
    hourly_tokens;
    hourly_chains;
  } in

  Mutex.unlock stats_mutex;
  result

(** {1 Reset and Management} *)

(** Reset all statistics *)
let reset () =
  Mutex.lock stats_mutex;
  stats_data.chain_durations <- [];
  stats_data.node_durations <- [];
  Hashtbl.clear stats_data.tokens_by_model;
  Hashtbl.clear stats_data.errors;
  Hashtbl.clear stats_data.hourly_tokens;
  Hashtbl.clear stats_data.hourly_chains;
  stats_data.success_count <- 0;
  stats_data.failure_count <- 0;
  stats_data.total_tokens <- 0;
  stats_data.total_cost <- 0.0;
  Mutex.unlock stats_mutex

(** {1 Subscription Management} *)

(** Stats collection subscription *)
let stats_subscription = ref None

(** Enable automatic stats collection *)
let enable () =
  match !stats_subscription with
  | Some _ -> ()  (* Already enabled *)
  | None ->
    stats_subscription := Some (subscribe stats_handler)

(** Disable automatic stats collection *)
let disable () =
  match !stats_subscription with
  | None -> ()
  | Some sub ->
    unsubscribe sub;
    stats_subscription := None

(** Check if stats collection is enabled *)
let is_enabled () =
  Option.is_some !stats_subscription

(** {1 Model Statistics} *)

(** Cost per 1K tokens by model (approximate) *)
let cost_per_1k_tokens = function
  | "gpt-4" | "gpt-4-turbo" -> 0.03
  | "gpt-3.5-turbo" -> 0.002
  | "claude-3-opus" | "claude-opus-4" -> 0.015
  | "claude-3-sonnet" | "claude-sonnet-4" -> 0.003
  | "claude-3-haiku" | "claude-haiku-4" -> 0.00025
  | "gemini-pro" | "gemini-2.0-flash" -> 0.00025
  | "codex" | "gpt-5.2-codex" -> 0.01
  | _ -> 0.001  (* Default estimate *)

(** Calculate model-specific statistics *)
let model_statistics () : model_stats list =
  Mutex.lock stats_mutex;
  let result : model_stats list =
    Hashtbl.fold (fun model tokens (acc : model_stats list) ->
      let cost = float_of_int tokens *. cost_per_1k_tokens model /. 1000.0 in
      let call_count = 1 in  (* TODO: Track per-model call count *)
      let avg_tokens = float_of_int tokens /. float_of_int (max 1 call_count) in
      ({
        model_name = model;
        call_count;
        total_tokens = tokens;
        avg_tokens_per_call = avg_tokens;
        total_cost_usd = cost;
        avg_latency_ms = 0.0;  (* TODO: Track per-model latency *)
      } : model_stats) :: acc
    ) stats_data.tokens_by_model ([] : model_stats list)
    |> List.sort (fun (a : model_stats) (b : model_stats) -> compare b.total_tokens a.total_tokens)
  in
  Mutex.unlock stats_mutex;
  result

(** {1 Serialization} *)

(** Convert stats to JSON *)
let to_json (stats : stats) =
  stats_to_yojson stats

(** Convert stats to compact string summary *)
let to_summary (stats : stats) =
  Printf.sprintf
    "Chains: %d (%.1f%% success) | Nodes: %d | Tokens: %d ($%.2f) | Avg: %.0fms P95: %.0fms"
    stats.total_chains
    (stats.success_rate *. 100.0)
    stats.total_nodes
    stats.total_tokens
    stats.estimated_cost_usd
    stats.avg_duration_ms
    stats.p95_duration_ms

(** {1 Pretty Printing} *)

(** Format stats for human-readable output *)
let string_of_stats (stats : stats) =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "═══════════════════════════════════════════════════════════════\n";
  Buffer.add_string buf "                     Chain Engine Statistics                    \n";
  Buffer.add_string buf "═══════════════════════════════════════════════════════════════\n";

  Buffer.add_string buf "\n📊 Execution Summary\n";
  Buffer.add_string buf "───────────────────────────────────────────────────────────────\n";
  Buffer.add_string buf (Printf.sprintf "  Total Chains:    %d\n" stats.total_chains);
  Buffer.add_string buf (Printf.sprintf "  Total Nodes:     %d\n" stats.total_nodes);
  Buffer.add_string buf (Printf.sprintf "  Success Rate:    %.1f%% (%d/%d)\n"
    (stats.success_rate *. 100.0)
    stats.success_count
    (stats.success_count + stats.failure_count));

  Buffer.add_string buf "\n⏱️ Latency (ms)\n";
  Buffer.add_string buf "───────────────────────────────────────────────────────────────\n";
  Buffer.add_string buf (Printf.sprintf "  Average:         %.1f\n" stats.avg_duration_ms);
  Buffer.add_string buf (Printf.sprintf "  P50 (Median):    %.1f\n" stats.p50_duration_ms);
  Buffer.add_string buf (Printf.sprintf "  P95:             %.1f\n" stats.p95_duration_ms);
  Buffer.add_string buf (Printf.sprintf "  P99:             %.1f\n" stats.p99_duration_ms);

  Buffer.add_string buf "\n🎟️ Token Usage\n";
  Buffer.add_string buf "───────────────────────────────────────────────────────────────\n";
  Buffer.add_string buf (Printf.sprintf "  Total Tokens:    %d\n" stats.total_tokens);
  Buffer.add_string buf (Printf.sprintf "  Est. Cost:       $%.4f\n" stats.estimated_cost_usd);

  if stats.tokens_by_model <> [] then begin
    Buffer.add_string buf "  By Model:\n";
    List.iter (fun (model, tokens) ->
      Buffer.add_string buf (Printf.sprintf "    %-20s %d tokens\n" model tokens)
    ) stats.tokens_by_model
  end;

  if stats.failure_reasons <> [] then begin
    Buffer.add_string buf "\n❌ Failure Reasons\n";
    Buffer.add_string buf "───────────────────────────────────────────────────────────────\n";
    List.iter (fun (reason, count) ->
      let short_reason =
        if String.length reason > 50 then String.sub reason 0 47 ^ "..."
        else reason
      in
      Buffer.add_string buf (Printf.sprintf "  [%3d] %s\n" count short_reason)
    ) stats.failure_reasons
  end;

  if stats.hourly_tokens <> [] then begin
    Buffer.add_string buf "\n📈 Hourly Token Distribution\n";
    Buffer.add_string buf "───────────────────────────────────────────────────────────────\n";
    List.iter (fun (hour, tokens) ->
      let bar_len = min 40 (tokens / 100) in
      let bar = String.make bar_len '#' in
      Buffer.add_string buf (Printf.sprintf "  %02d:00  %s %d\n" hour bar tokens)
    ) stats.hourly_tokens
  end;

  Buffer.add_string buf "═══════════════════════════════════════════════════════════════\n";
  Buffer.contents buf

(** {1 Prometheus Format} *)

(** Export stats in Prometheus format *)
let to_prometheus (stats : stats) =
  let buf = Buffer.create 1024 in

  (* Chain executions *)
  Buffer.add_string buf "# HELP chain_executions_total Total chain executions\n";
  Buffer.add_string buf "# TYPE chain_executions_total counter\n";
  Buffer.add_string buf (Printf.sprintf "chain_executions_total{status=\"success\"} %d\n" stats.success_count);
  Buffer.add_string buf (Printf.sprintf "chain_executions_total{status=\"failure\"} %d\n" stats.failure_count);

  (* Token usage *)
  Buffer.add_string buf "\n# HELP chain_tokens_total Total tokens used\n";
  Buffer.add_string buf "# TYPE chain_tokens_total counter\n";
  List.iter (fun (model, tokens) ->
    Buffer.add_string buf (Printf.sprintf "chain_tokens_total{model=\"%s\"} %d\n" model tokens)
  ) stats.tokens_by_model;

  (* Duration histogram buckets *)
  Buffer.add_string buf "\n# HELP chain_duration_seconds Chain execution duration\n";
  Buffer.add_string buf "# TYPE chain_duration_seconds histogram\n";
  Buffer.add_string buf (Printf.sprintf "chain_duration_seconds{quantile=\"0.5\"} %.3f\n" (stats.p50_duration_ms /. 1000.0));
  Buffer.add_string buf (Printf.sprintf "chain_duration_seconds{quantile=\"0.95\"} %.3f\n" (stats.p95_duration_ms /. 1000.0));
  Buffer.add_string buf (Printf.sprintf "chain_duration_seconds{quantile=\"0.99\"} %.3f\n" (stats.p99_duration_ms /. 1000.0));

  (* Cost *)
  Buffer.add_string buf "\n# HELP chain_cost_usd_total Estimated cost in USD\n";
  Buffer.add_string buf "# TYPE chain_cost_usd_total counter\n";
  Buffer.add_string buf (Printf.sprintf "chain_cost_usd_total %.4f\n" stats.estimated_cost_usd);

  Buffer.contents buf

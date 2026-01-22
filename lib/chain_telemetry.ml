(** Chain Telemetry - Event Logging and Observability

    체인 실행 이벤트를 로깅하고 구독 기반 관찰 기능을 제공합니다.

    특징:
    - 비동기 이벤트 발행 (emit)
    - 다중 구독자 지원 (subscribe/unsubscribe)
    - 구조화된 이벤트 타입 (ChainStart, NodeComplete, Error 등)
    - 스레드 안전한 구독자 관리

    @author Chain Engine
    @since 2026-01
*)

open Chain_category

(** {1 Event Types} *)

(** Chain start event payload *)
type chain_start_payload = {
  start_chain_id: string;
  start_nodes: int;
  start_timestamp: float;
  start_mermaid_dsl: string option;  (** Mermaid diagram for visualization *)
} [@@deriving yojson]

(** Node start event payload *)
type node_start_payload = {
  node_start_id: string;
  node_start_type: string;
  node_parent: string option;
} [@@deriving yojson]

(** Node complete event payload *)
type node_complete_payload = {
  node_complete_id: string;
  node_duration_ms: int;
  node_tokens: token_usage;
  node_verdict: verdict;
  node_confidence: float;
  node_output_preview: string option;
} [@@deriving yojson]

(** Chain complete event payload *)
type chain_complete_payload = {
  complete_chain_id: string;
  complete_duration_ms: int;
  complete_tokens: token_usage;
  nodes_executed: int;
  nodes_skipped: int;
} [@@deriving yojson]

(** Error event payload *)
type error_payload = {
  error_node_id: string;
  error_message: string;
  error_retries: int;
  error_timestamp: float;
} [@@deriving yojson]

(** All chain events *)
type chain_event =
  | ChainStart of chain_start_payload
  | NodeStart of node_start_payload
  | NodeComplete of node_complete_payload
  | ChainComplete of chain_complete_payload
  | Error of error_payload
[@@deriving yojson]

(** {1 Subscription Management} *)

(** Unique subscription identifier *)
type subscription_id = int

(** Subscription handle *)
type subscription = {
  sub_id: subscription_id;
  mutable active: bool;
}

(** Event handler function type *)
type event_handler = chain_event -> unit

(** Global subscription registry *)
let next_sub_id = ref 0
let subscribers : (subscription_id, event_handler) Hashtbl.t = Hashtbl.create 16
let subscribers_mutex = Mutex.create ()

(** {1 Running Chains Tracking} *)

(** Running chain info: (chain_id, started_at, progress) *)
type running_chain_info = {
  chain_id: string;
  started_at: float;
  mutable progress: float;  (** 0.0 to 1.0 *)
  mutable nodes_completed: int;
  total_nodes: int;
}

let running_chains : (string, running_chain_info) Hashtbl.t = Hashtbl.create 16
let running_chains_mutex = Mutex.create ()

(** Register a chain as running *)
let register_running_chain ~chain_id ~total_nodes =
  Mutex.lock running_chains_mutex;
  let info = {
    chain_id;
    started_at = Unix.gettimeofday ();
    progress = 0.0;
    nodes_completed = 0;
    total_nodes;
  } in
  Hashtbl.replace running_chains chain_id info;
  Mutex.unlock running_chains_mutex

(** Update chain progress *)
let update_chain_progress ~chain_id ~nodes_completed =
  Mutex.lock running_chains_mutex;
  (match Hashtbl.find_opt running_chains chain_id with
   | Some info ->
       info.nodes_completed <- nodes_completed;
       info.progress <- if info.total_nodes > 0
         then float_of_int nodes_completed /. float_of_int info.total_nodes
         else 0.0
   | None -> ());
  Mutex.unlock running_chains_mutex

(** Unregister a completed chain *)
let unregister_running_chain ~chain_id =
  Mutex.lock running_chains_mutex;
  Hashtbl.remove running_chains chain_id;
  Mutex.unlock running_chains_mutex

(** Get all running chains *)
let get_running_chains () : (string * float * float) list =
  Mutex.lock running_chains_mutex;
  let chains = Hashtbl.fold (fun _id info acc ->
    (info.chain_id, info.started_at, info.progress) :: acc
  ) running_chains [] in
  Mutex.unlock running_chains_mutex;
  chains

(** Generate next subscription ID *)
let gen_sub_id () =
  let id = !next_sub_id in
  incr next_sub_id;
  id

(** {1 Event Emission} *)

(** Emit an event to all subscribers *)
let emit event =
  Mutex.lock subscribers_mutex;
  let handlers = Hashtbl.fold (fun _ handler acc -> handler :: acc) subscribers [] in
  Mutex.unlock subscribers_mutex;
  (* Call handlers outside of lock to avoid deadlocks *)
  List.iter (fun handler ->
    try handler event
    with _ -> () (* Ignore handler errors *)
  ) handlers

(** {1 Subscription API} *)

(** Subscribe to chain events *)
let subscribe handler =
  Mutex.lock subscribers_mutex;
  let id = gen_sub_id () in
  Hashtbl.add subscribers id handler;
  Mutex.unlock subscribers_mutex;
  { sub_id = id; active = true }

(** Unsubscribe from chain events *)
let unsubscribe sub =
  if sub.active then begin
    Mutex.lock subscribers_mutex;
    Hashtbl.remove subscribers sub.sub_id;
    sub.active <- false;
    Mutex.unlock subscribers_mutex
  end

(** Check if subscription is active *)
let is_active sub = sub.active

(** Get number of active subscribers *)
let subscriber_count () =
  Mutex.lock subscribers_mutex;
  let count = Hashtbl.length subscribers in
  Mutex.unlock subscribers_mutex;
  count

(** {1 Event Constructors} *)

(** Create a ChainStart event *)
let chain_start ~chain_id ~nodes ?mermaid_dsl () =
  ChainStart {
    start_chain_id = chain_id;
    start_nodes = nodes;
    start_timestamp = Unix.gettimeofday ();
    start_mermaid_dsl = mermaid_dsl;
  }

(** Create a NodeStart event *)
let node_start ~node_id ~node_type ?parent () =
  NodeStart {
    node_start_id = node_id;
    node_start_type = node_type;
    node_parent = parent;
  }

(** Create a NodeComplete event *)
let node_complete ~node_id ~duration_ms ~tokens ~verdict ~confidence ?output_preview () =
  NodeComplete {
    node_complete_id = node_id;
    node_duration_ms = duration_ms;
    node_tokens = tokens;
    node_verdict = verdict;
    node_confidence = confidence;
    node_output_preview = output_preview;
  }

(** Create a ChainComplete event *)
let chain_complete ~chain_id ~duration_ms ~tokens ~executed ~skipped =
  ChainComplete {
    complete_chain_id = chain_id;
    complete_duration_ms = duration_ms;
    complete_tokens = tokens;
    nodes_executed = executed;
    nodes_skipped = skipped;
  }

(** Create an Error event *)
let error ~node_id ~message ~retries =
  Error {
    error_node_id = node_id;
    error_message = message;
    error_retries = retries;
    error_timestamp = Unix.gettimeofday ();
  }

(** {1 Event Logging} *)

(** Event log buffer for persistence *)
let event_log : chain_event list ref = ref []
let event_log_mutex = Mutex.create ()
let max_log_size = ref 10000

(** Logging subscriber that buffers events *)
let logging_handler event =
  Mutex.lock event_log_mutex;
  event_log := event :: !event_log;
  (* Trim if exceeds max size *)
  if List.length !event_log > !max_log_size then
    event_log := List.filteri (fun i _ -> i < !max_log_size) !event_log;
  Mutex.unlock event_log_mutex

(** Initialize logging subscriber *)
let logging_subscription = ref None

let enable_logging ?(max_size=10000) () =
  max_log_size := max_size;
  match !logging_subscription with
  | Some _ -> () (* Already enabled *)
  | None ->
    logging_subscription := Some (subscribe logging_handler)

let disable_logging () =
  match !logging_subscription with
  | None -> ()
  | Some sub ->
    unsubscribe sub;
    logging_subscription := None

(** Get recent events from log *)
let get_recent_events ?(limit=100) () =
  Mutex.lock event_log_mutex;
  let events = List.filteri (fun i _ -> i < limit) !event_log in
  Mutex.unlock event_log_mutex;
  List.rev events  (* Return in chronological order *)

(** Clear event log *)
let clear_log () =
  Mutex.lock event_log_mutex;
  event_log := [];
  Mutex.unlock event_log_mutex

(** {1 Filtering} *)

(** Filter predicate type *)
type event_filter = chain_event -> bool

(** Filter: only chain events *)
let chain_events_only = function
  | ChainStart _ | ChainComplete _ -> true
  | _ -> false

(** Filter: only node events *)
let node_events_only = function
  | NodeStart _ | NodeComplete _ -> true
  | _ -> false

(** Filter: only errors *)
let errors_only = function
  | Error _ -> true
  | _ -> false

(** Filter: events for specific chain *)
let for_chain chain_id = function
  | ChainStart p -> p.start_chain_id = chain_id
  | ChainComplete p -> p.complete_chain_id = chain_id
  | _ -> true  (* Node events don't have chain_id *)

(** Filter: events for specific node *)
let for_node node_id = function
  | NodeStart p -> p.node_start_id = node_id
  | NodeComplete p -> p.node_complete_id = node_id
  | Error p -> p.error_node_id = node_id
  | _ -> false

(** Subscribe with filter *)
let subscribe_filtered ~filter handler =
  subscribe (fun event ->
    if filter event then handler event
  )

(** {1 Serialization} *)

(** Convert event to JSON string *)
let event_to_json_string event =
  Yojson.Safe.to_string (chain_event_to_yojson event)

(** Parse event from JSON string *)
let event_of_json_string str =
  match Yojson.Safe.from_string str with
  | json -> chain_event_of_yojson json
  | exception _ -> Error "Invalid JSON"

(** {1 Pretty Printing} *)

(** Format event for human-readable output *)
let string_of_event = function
  | ChainStart p ->
    Printf.sprintf "[CHAIN_START] %s (%d nodes) at %.3f"
      p.start_chain_id p.start_nodes p.start_timestamp
  | NodeStart p ->
    Printf.sprintf "[NODE_START] %s (%s)%s"
      p.node_start_id p.node_start_type
      (match p.node_parent with Some p -> " parent:" ^ p | None -> "")
  | NodeComplete p ->
    Printf.sprintf "[NODE_COMPLETE] %s in %dms, %d tokens, %s (%.2f confidence)"
      p.node_complete_id p.node_duration_ms p.node_tokens.total_tokens
      (match p.node_verdict with
       | Pass s -> "PASS:" ^ s
       | Warn s -> "WARN:" ^ s
       | Fail s -> "FAIL:" ^ s
       | Defer s -> "DEFER:" ^ s)
      p.node_confidence
  | ChainComplete p ->
    Printf.sprintf "[CHAIN_COMPLETE] %s in %dms, %d tokens, %d/%d nodes"
      p.complete_chain_id p.complete_duration_ms p.complete_tokens.total_tokens
      p.nodes_executed (p.nodes_executed + p.nodes_skipped)
  | Error p ->
    Printf.sprintf "[ERROR] %s: %s (retries: %d) at %.3f"
      p.error_node_id p.error_message p.error_retries p.error_timestamp

(** {1 Console Logger} *)

(** Create a console logging handler *)
let console_handler ?(prefix="[CHAIN]") event =
  Printf.printf "%s %s\n%!" prefix (string_of_event event)

(** Subscribe console logger *)
let enable_console_logging ?(prefix="[CHAIN]") () =
  subscribe (console_handler ~prefix)

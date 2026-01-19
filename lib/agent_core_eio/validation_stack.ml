(** Validation Stack - Multi-Level Validation Hierarchy

    Beyond simple SPEC (correctness), this module provides a unified stack
    for validating at multiple levels:

    - Level 0: SPEC - Correctness (binary: pass/fail)
    - Level 1: METRIC - Quality (continuous: 0.0 ~ 1.0)
    - Level 2: GOAL - Threshold achievement
    - Level 3: TEMPORAL - Time-based properties
    - Level 4: EMERGENT - System-level observation
    - Level 5: JUDGMENT - Human feedback integration

    Each level builds on the previous, creating a composable validation hierarchy.
*)

(* ========================================================================== *)
(* Level 0: SPEC - Correctness (from Spec_dsl)                                *)
(* ========================================================================== *)

(** Re-export SPEC from Spec_dsl *)
module type SPEC = Spec_dsl.SPEC

(* ========================================================================== *)
(* Level 1: METRIC - Quality Measurement                                      *)
(* ========================================================================== *)

(** Continuous quality measurement *)
module type METRIC = sig
  type input
  type output
  type measurement

  (** Measure quality of output given input *)
  val measure : input -> output -> measurement

  (** Convert measurement to normalized float [0.0, 1.0] *)
  val to_float : measurement -> float

  (** Compare two measurements *)
  val compare : measurement -> measurement -> [`Better | `Worse | `Same]

  (** Human-readable description *)
  val describe : measurement -> string
end

(** Common metric types *)
module Metrics = struct
  (** Percentage metric (0-100) *)
  type percentage = { value : float; label : string }

  (** Similarity metric (0.0-1.0) *)
  type similarity = {
    score : float;
    method_name : string;  (* e.g., "cosine", "jaccard", "SSIM" *)
  }

  (** Coverage metric *)
  type coverage = {
    covered : int;
    total : int;
    percentage : float;
  }

  (** Multi-dimensional metric *)
  type composite = {
    dimensions : (string * float) list;
    aggregate : float;
  }

  let percentage_to_float p = p.value /. 100.0
  let similarity_to_float s = s.score
  let coverage_to_float c = c.percentage /. 100.0
  let composite_to_float c = c.aggregate
end

(** Functor to create a simple float-based metric *)
module Make_float_metric (Config : sig
  val name : string
  val min_value : float
  val max_value : float
end) : METRIC with type input = unit
                and type output = float
                and type measurement = float = struct
  type input = unit
  type output = float
  type measurement = float

  let measure () output =
    (* Clamp to valid range *)
    max Config.min_value (min Config.max_value output)

  let to_float m =
    (* Normalize to [0.0, 1.0] *)
    (m -. Config.min_value) /. (Config.max_value -. Config.min_value)

  let compare m1 m2 =
    if m1 > m2 then `Better
    else if m1 < m2 then `Worse
    else `Same

  let describe m =
    Printf.sprintf "%s: %.2f (%.1f%%)" Config.name m (to_float m *. 100.0)
end

(** Test coverage metric *)
module Coverage_metric : METRIC with type input = int (* total *)
                                 and type output = int (* covered *)
                                 and type measurement = Metrics.coverage = struct
  type input = int
  type output = int
  type measurement = Metrics.coverage

  let measure total covered =
    let percentage = if total > 0 then (float_of_int covered /. float_of_int total) *. 100.0 else 0.0 in
    { Metrics.covered; total; percentage }

  let to_float m = Metrics.coverage_to_float m

  let compare m1 m2 =
    if m1.Metrics.percentage > m2.Metrics.percentage then `Better
    else if m1.percentage < m2.percentage then `Worse
    else `Same

  let describe m =
    Printf.sprintf "Coverage: %d/%d (%.1f%%)" m.Metrics.covered m.total m.percentage
end

(** SSIM (Structural Similarity) metric for image comparison *)
module SSIM_metric : METRIC with type input = string (* reference path *)
                             and type output = string (* output path *)
                             and type measurement = Metrics.similarity = struct
  type input = string
  type output = string
  type measurement = Metrics.similarity

  (* Placeholder - actual SSIM would use image processing library *)
  let measure _reference _output =
    { Metrics.score = 0.95; method_name = "SSIM" }

  let to_float m = Metrics.similarity_to_float m

  let compare m1 m2 =
    if m1.Metrics.score > m2.Metrics.score then `Better
    else if m1.score < m2.score then `Worse
    else `Same

  let describe m =
    Printf.sprintf "%s similarity: %.4f (%.1f%%)" m.Metrics.method_name m.score (m.score *. 100.0)
end

(* ========================================================================== *)
(* Level 2: GOAL - Threshold Achievement                                      *)
(* ========================================================================== *)

(** Goal with target threshold *)
module type GOAL = sig
  type measurement
  type target

  (** The target to achieve *)
  val target : target

  (** Distance from measurement to target (0.0 = achieved) *)
  val distance : measurement -> target -> float

  (** Is the goal achieved? *)
  val achieved : measurement -> target -> bool

  (** Progress toward goal [0.0, 1.0] *)
  val progress : measurement -> target -> float

  (** Describe the goal *)
  val describe_goal : target -> string
end

(** Goal result *)
type goal_result = {
  achieved : bool;
  distance : float;
  progress : float;
  description : string;
}

(** Functor to create a simple threshold goal *)
module Make_threshold_goal (M : METRIC) (Config : sig
  val threshold : float  (* normalized threshold [0.0, 1.0] *)
  val direction : [`Higher_is_better | `Lower_is_better]
end) : GOAL with type measurement = M.measurement
             and type target = float = struct
  type measurement = M.measurement
  type target = float

  let target = Config.threshold

  let distance m t =
    let value = M.to_float m in
    match Config.direction with
    | `Higher_is_better -> max 0.0 (t -. value)
    | `Lower_is_better -> max 0.0 (value -. t)

  let achieved m t =
    distance m t <= 0.0

  let progress m t =
    let value = M.to_float m in
    match Config.direction with
    | `Higher_is_better ->
      if t <= 0.0 then 1.0 else min 1.0 (value /. t)
    | `Lower_is_better ->
      if value <= t then 1.0
      else max 0.0 (1.0 -. (value -. t))

  let describe_goal t =
    let dir_str = match Config.direction with
      | `Higher_is_better -> ">="
      | `Lower_is_better -> "<="
    in
    Printf.sprintf "Target: %s %.1f%%" dir_str (t *. 100.0)
end

(** Composite goal combining multiple sub-goals *)
module Composite_goal = struct
  type 'a sub_goal = {
    name : string;
    weight : float;
    check : 'a -> goal_result;
  }

  type 'a t = {
    sub_goals : 'a sub_goal list;
    policy : [`All | `Any | `Weighted of float];
  }

  let create ~policy sub_goals =
    { sub_goals; policy }

  let evaluate t input =
    let results = List.map (fun sg ->
      let r = sg.check input in
      (sg, r)
    ) t.sub_goals in

    let achieved = match t.policy with
      | `All -> List.for_all (fun (_, r) -> r.achieved) results
      | `Any -> List.exists (fun (_, r) -> r.achieved) results
      | `Weighted threshold ->
        let total_weight = List.fold_left (fun acc sg -> acc +. sg.weight) 0.0 t.sub_goals in
        let achieved_weight = List.fold_left (fun acc (sg, r) ->
          if r.achieved then acc +. sg.weight else acc
        ) 0.0 results in
        (achieved_weight /. total_weight) >= threshold
    in

    let avg_progress =
      let sum = List.fold_left (fun acc (sg, r) -> acc +. (r.progress *. sg.weight)) 0.0 results in
      let total_weight = List.fold_left (fun acc sg -> acc +. sg.weight) 0.0 t.sub_goals in
      sum /. total_weight
    in

    let description = String.concat "\n" (List.map (fun (sg, r) ->
      Printf.sprintf "  - %s: %s (%.1f%%)" sg.name
        (if r.achieved then "✓" else "✗")
        (r.progress *. 100.0)
    ) results) in

    { achieved; distance = 1.0 -. avg_progress; progress = avg_progress; description }
end

(* ========================================================================== *)
(* Level 3: TEMPORAL - Time-Based Properties                                  *)
(* ========================================================================== *)

(** Temporal logic for traces *)
module type TEMPORAL = sig
  type state
  type trace = state list

  (** Eventually: ◇φ - property holds at some point *)
  val eventually : (state -> bool) -> trace -> bool

  (** Always: □φ - property holds at all points *)
  val always : (state -> bool) -> trace -> bool

  (** Until: φ U ψ - φ holds until ψ becomes true *)
  val until : (state -> bool) -> (state -> bool) -> trace -> bool

  (** Next: ○φ - property holds at next state *)
  val next : (state -> bool) -> trace -> bool

  (** Bounded eventually: ◇≤n φ - property holds within n steps *)
  val eventually_within : int -> (state -> bool) -> trace -> bool

  (** Bounded always: □≤n φ - property holds for n steps *)
  val always_for : int -> (state -> bool) -> trace -> bool
end

(** Functor to create temporal logic for any state type *)
module Make_temporal (S : sig type t end) : TEMPORAL with type state = S.t = struct
  type state = S.t
  type trace = state list

  let eventually prop trace =
    List.exists prop trace

  let always prop trace =
    List.for_all prop trace

  let rec until phi psi = function
    | [] -> false
    | s :: rest ->
      if psi s then true
      else if phi s then until phi psi rest
      else false

  let next prop = function
    | [] -> false
    | [_] -> false
    | _ :: s :: _ -> prop s

  let eventually_within n prop trace =
    let rec check i = function
      | [] -> false
      | s :: rest ->
        if i >= n then false
        else if prop s then true
        else check (i + 1) rest
    in
    check 0 trace

  let always_for n prop trace =
    let rec check i = function
      | [] -> true
      | s :: rest ->
        if i >= n then true
        else if not (prop s) then false
        else check (i + 1) rest
    in
    check 0 trace
end

(** Temporal property specification *)
type 'a temporal_spec =
  | Eventually of ('a -> bool)
  | Always of ('a -> bool)
  | Until of ('a -> bool) * ('a -> bool)
  | EventuallyWithin of int * ('a -> bool)
  | AlwaysFor of int * ('a -> bool)

(** Check a temporal spec against a trace *)
module Temporal_checker = struct
  let check (type a) (module T : TEMPORAL with type state = a) spec trace =
    match spec with
    | Eventually prop -> T.eventually prop trace
    | Always prop -> T.always prop trace
    | Until (phi, psi) -> T.until phi psi trace
    | EventuallyWithin (n, prop) -> T.eventually_within n prop trace
    | AlwaysFor (n, prop) -> T.always_for n prop trace
end

(* ========================================================================== *)
(* Level 4: EMERGENT - System-Level Properties                                *)
(* ========================================================================== *)

(** Emergent properties observed at system level *)
module type EMERGENT = sig
  type component
  type system = component list
  type observation

  (** Observe system behavior *)
  val observe : system -> observation

  (** Check emergent property *)
  val check_property : observation -> bool

  (** Quantify emergent measure *)
  val measure : observation -> float

  (** Describe observation *)
  val describe : observation -> string
end

(** Common emergent property types *)
module Emergent_properties = struct
  (** System health *)
  type health = {
    healthy_count : int;
    total_count : int;
    health_ratio : float;
  }

  (** System throughput *)
  type throughput = {
    operations_per_second : float;
    latency_p50 : float;
    latency_p99 : float;
  }

  (** System stability *)
  type stability = {
    error_rate : float;
    recovery_time_ms : float;
    mtbf : float;  (* mean time between failures *)
  }

  (** Resource utilization *)
  type utilization = {
    cpu : float;
    memory : float;
    io : float;
  }
end

(** Functor for health-based emergent property *)
module Make_health_emergent (Config : sig
  type component
  val is_healthy : component -> bool
  val threshold : float
end) : EMERGENT with type component = Config.component
                 and type observation = Emergent_properties.health = struct
  type component = Config.component
  type system = component list
  type observation = Emergent_properties.health

  let observe system =
    let healthy_count = List.length (List.filter Config.is_healthy system) in
    let total_count = List.length system in
    let health_ratio = if total_count > 0
      then float_of_int healthy_count /. float_of_int total_count
      else 0.0 in
    { Emergent_properties.healthy_count; total_count; health_ratio }

  let check_property obs =
    obs.Emergent_properties.health_ratio >= Config.threshold

  let measure obs = obs.Emergent_properties.health_ratio

  let describe obs =
    Printf.sprintf "System health: %d/%d (%.1f%%) - %s"
      obs.Emergent_properties.healthy_count
      obs.total_count
      (obs.health_ratio *. 100.0)
      (if check_property obs then "HEALTHY" else "DEGRADED")
end

(* ========================================================================== *)
(* Level 5: JUDGMENT - Human Feedback                                         *)
(* ========================================================================== *)

(** Human judgment integration *)
module type JUDGMENT = sig
  type artifact  (* what is being judged *)
  type feedback
  type judgment

  (** Request human feedback (async) *)
  val request_feedback : artifact -> string -> feedback Eio.Promise.t

  (** Interpret feedback as judgment *)
  val interpret : feedback -> judgment

  (** Confidence in judgment [0.0, 1.0] *)
  val confidence : judgment -> float

  (** Is judgment positive? *)
  val is_positive : judgment -> bool
end

(** Feedback types *)
module Feedback = struct
  (** Simple approval *)
  type approval = Approved | Rejected | NeedsRevision of string

  (** Rating scale *)
  type rating = {
    score : int;  (* e.g., 1-5 *)
    max_score : int;
    comment : string option;
  }

  (** Comparative judgment *)
  type comparison = {
    preferred : [`A | `B | `Neither | `Both];
    reason : string option;
  }

  (** Detailed rubric *)
  type rubric_item = {
    criterion : string;
    score : float;
    feedback : string;
  }

  type rubric = rubric_item list
end

(** Simple approval-based judgment *)
module Approval_judgment : JUDGMENT with type artifact = string
                                     and type feedback = Feedback.approval
                                     and type judgment = bool * float = struct
  type artifact = string
  type feedback = Feedback.approval
  type judgment = bool * float  (* is_positive, confidence *)

  let request_feedback _artifact _prompt =
    (* In real implementation, this would be async human interaction *)
    Eio.Promise.create_resolved Feedback.Approved

  let interpret = function
    | Feedback.Approved -> (true, 1.0)
    | Feedback.Rejected -> (false, 1.0)
    | Feedback.NeedsRevision _ -> (false, 0.5)

  let confidence (_, c) = c
  let is_positive (p, _) = p
end

(** Rating-based judgment *)
module Rating_judgment (Config : sig
  val threshold : float  (* normalized threshold for positive *)
end) : JUDGMENT with type artifact = string
                 and type feedback = Feedback.rating
                 and type judgment = float * float = struct
  type artifact = string
  type feedback = Feedback.rating
  type judgment = float * float  (* normalized_score, confidence *)

  let request_feedback _artifact _prompt =
    Eio.Promise.create_resolved ({ score = 4; max_score = 5; comment = None } : Feedback.rating)

  let interpret (rating : Feedback.rating) =
    let normalized = float_of_int rating.score /. float_of_int rating.max_score in
    let confidence = 1.0 in  (* Could factor in rater reliability *)
    (normalized, confidence)

  let confidence (_, c) = c

  let is_positive (score, _) = score >= Config.threshold
end

(* ========================================================================== *)
(* Level 6: INTERVENOR - Intervention as Agent                                *)
(* ========================================================================== *)

(** Intervention trigger conditions *)
type intervention_trigger =
  | Stalled of { duration_sec : float; progress : float }
  | LowPriority of { importance : float; time_spent_sec : float }
  | RepeatedFailure of { attempts : int; last_error : string }
  | ConfidenceDrop of { current : float; threshold : float }
  | ManualRequest of string

(** Intervention decision *)
type intervention_decision =
  | Continue
  | Pause of string
  | Redirect of { new_goal : string; reason : string }
  | Escalate of { to_level : [`Human | `Senior_agent | `Architect]; reason : string }
  | Abort of string
  | Redefine of { original_problem : string; suggested_reframe : string }

(** Intervenor - can be human OR automated agent *)
module type INTERVENOR = sig
  type context
  type observation

  (** Observe current state *)
  val observe : context -> observation

  (** Detect if intervention is needed *)
  val should_intervene : observation -> intervention_trigger option

  (** Decide what intervention to take *)
  val decide : observation -> intervention_trigger -> intervention_decision

  (** Execute intervention *)
  val intervene : intervention_decision -> unit

  (** Is this intervenor human or automated? *)
  val is_human : bool
end

(** Stall detector - triggers intervention when progress stalls *)
module Stall_detector = struct
  type 'a progress_state = {
    mutable last_progress : float;
    mutable last_update : float;
    mutable stall_count : int;
    importance : float;
    extract_progress : 'a -> float;
  }

  let create ~importance ~extract_progress ~clock =
    { last_progress = 0.0;
      last_update = Eio.Time.now clock;
      stall_count = 0;
      importance;
      extract_progress }

  let update state clock observation =
    let current_progress = state.extract_progress observation in
    let now = Eio.Time.now clock in

    if current_progress > state.last_progress then begin
      state.last_progress <- current_progress;
      state.last_update <- now;
      state.stall_count <- 0;
      None
    end else begin
      let duration = now -. state.last_update in
      state.stall_count <- state.stall_count + 1;
      (* Stall threshold inversely proportional to importance *)
      let threshold = 60.0 /. (state.importance +. 0.1) in
      if duration > threshold then
        Some (Stalled { duration_sec = duration; progress = current_progress })
      else
        None
    end

  let check_low_priority state _clock time_spent =
    (* If importance < 0.3 and time_spent > 30min, flag as low priority *)
    if state.importance < 0.3 && time_spent > 1800.0 then
      Some (LowPriority { importance = state.importance; time_spent_sec = time_spent })
    else
      None
end

(** Human intervenor - prompts for human decision *)
module Human_intervenor : INTERVENOR with type context = string
                                      and type observation = string = struct
  type context = string
  type observation = string

  let is_human = true

  let observe ctx = ctx

  let should_intervene _obs =
    (* Human intervenor doesn't auto-detect, waits for explicit request *)
    None

  let decide _obs trigger =
    match trigger with
    | Stalled { duration_sec; progress } ->
      Printf.printf "⚠️  Task stalled for %.1f seconds at %.1f%% progress.\n" duration_sec (progress *. 100.0);
      Printf.printf "Options: [c]ontinue, [p]ause, [r]edirect, [a]bort? ";
      flush stdout;
      (* In real implementation, would read input *)
      Continue
    | LowPriority { importance; time_spent_sec } ->
      Printf.printf "⚠️  Low priority task (%.1f) taking %.1f minutes.\n" importance (time_spent_sec /. 60.0);
      Printf.printf "Consider: Is this worth the time investment? ";
      flush stdout;
      Continue
    | RepeatedFailure { attempts; last_error } ->
      Printf.printf "❌ Failed %d times. Last error: %s\n" attempts last_error;
      Pause "Awaiting human review"
    | ConfidenceDrop { current; threshold } ->
      Printf.printf "📉 Confidence dropped to %.1f%% (threshold: %.1f%%)\n"
        (current *. 100.0) (threshold *. 100.0);
      Continue
    | ManualRequest reason ->
      Printf.printf "🙋 Manual intervention requested: %s\n" reason;
      Pause reason

  let intervene decision =
    match decision with
    | Continue -> Printf.printf "▶️  Continuing...\n"
    | Pause reason -> Printf.printf "⏸️  Paused: %s\n" reason
    | Redirect { new_goal; reason } ->
      Printf.printf "↪️  Redirecting to: %s (reason: %s)\n" new_goal reason
    | Escalate { to_level; reason } ->
      let level_str = match to_level with
        | `Human -> "human"
        | `Senior_agent -> "senior agent"
        | `Architect -> "architect"
      in
      Printf.printf "⬆️  Escalating to %s: %s\n" level_str reason
    | Abort reason -> Printf.printf "🛑 Aborting: %s\n" reason
    | Redefine { original_problem; suggested_reframe } ->
      Printf.printf "🔄 Redefining problem:\n  Original: %s\n  Suggested: %s\n"
        original_problem suggested_reframe
end

(** AI Agent intervenor - automated decision making *)
module Make_ai_intervenor (Config : sig
  val stall_threshold_sec : float
  val max_failures : int
end) : INTERVENOR with type context = float * float * int  (* progress, time, failures *)
                   and type observation = float * float * int = struct
  type context = float * float * int  (* progress, time_spent, failure_count *)
  type observation = float * float * int

  let is_human = false

  let observe ctx = ctx

  let should_intervene (progress, time_spent, failures) =
    if failures >= Config.max_failures then
      Some (RepeatedFailure { attempts = failures; last_error = "Max failures reached" })
    else if time_spent > Config.stall_threshold_sec && progress < 0.5 then
      Some (Stalled { duration_sec = time_spent; progress })
    else
      None

  let decide _obs trigger =
    match trigger with
    | Stalled { duration_sec; progress } ->
      if progress < 0.1 then
        Redefine {
          original_problem = "Current task";
          suggested_reframe = Printf.sprintf
            "Task stalled at %.0f%% after %.0fs - consider breaking into smaller subtasks"
            (progress *. 100.0) duration_sec
        }
      else if progress < 0.3 then
        Redirect {
          new_goal = "Simplified approach";
          reason = "Low progress after significant time"
        }
      else
        Continue
    | LowPriority _ ->
      Redirect {
        new_goal = "Higher priority task";
        reason = "Opportunity cost too high"
      }
    | RepeatedFailure { attempts; _ } ->
      if attempts >= Config.max_failures then
        Escalate { to_level = `Senior_agent; reason = "Repeated failures" }
      else
        Continue
    | ConfidenceDrop _ -> Continue
    | ManualRequest _ -> Pause "Manual intervention requested"

  let intervene _decision = ()  (* AI intervenor executes silently *)
end

(** Hybrid intervenor - AI decides, human confirms important decisions *)
module Make_hybrid_intervenor
    (AI : INTERVENOR)
    (Human : INTERVENOR) = struct

  type context = AI.context * Human.context
  type observation = AI.observation option * Human.observation option

  let is_human = false  (* Primarily AI-driven *)

  let observe (ai_ctx, human_ctx) =
    (Some (AI.observe ai_ctx), Some (Human.observe human_ctx))

  let should_intervene (ai_obs, _human_obs) =
    match ai_obs with
    | Some obs -> AI.should_intervene obs
    | None -> None

  let decide (ai_obs, human_obs) trigger =
    match ai_obs with
    | Some obs ->
      let ai_decision = AI.decide obs trigger in
      (* For critical decisions, escalate to human *)
      (match ai_decision with
       | Abort _ | Redefine _ | Escalate { to_level = `Human; _ } ->
         (match human_obs with
          | Some hobs -> Human.decide hobs trigger
          | None -> ai_decision)
       | _ -> ai_decision)
    | None ->
      Continue

  let intervene decision =
    AI.intervene decision;
    (* Log for human review if needed *)
    match decision with
    | Abort _ | Redefine _ -> Human.intervene decision
    | _ -> ()
end

(* ========================================================================== *)
(* Unified Validation Stack                                                   *)
(* ========================================================================== *)

(** Result from each validation level *)
type level_result = {
  level : int;
  name : string;
  passed : bool;
  score : float;
  details : string;
}

(** Complete validation stack result *)
type stack_result = {
  all_passed : bool;
  levels : level_result list;
  summary : string;
}

(** Unified validator combining all levels *)
module type UNIFIED_VALIDATOR = sig
  type input
  type output
  type state

  (** Run all validation levels *)
  val validate_all : state -> input -> output -> stack_result

  (** Run up to specified level *)
  val validate_to_level : int -> state -> input -> output -> stack_result

  (** Get required levels for given context *)
  val required_levels : [`Dev | `Staging | `Production] -> int list
end

(** Builder for unified validator - uses closures for type flexibility *)
module Unified_builder = struct
  (** Level validator as closure returning level_result *)
  type ('s, 'i, 'o) level_validator = 's -> 'i -> 'o -> level_result option

  type ('s, 'i, 'o) t = {
    validators : ('s, 'i, 'o) level_validator list;
    trace : 's list ref;
  }

  let create () = { validators = []; trace = ref [] }

  (** Add SPEC-based validator (Level 0) *)
  let with_spec (type s i o)
      (module S : Spec_dsl.SPEC with type state = s and type input = i and type output = o)
      (t : (s, i, o) t) : (s, i, o) t =
    let validator state input output =
      let passed = S.precondition state input &&
                   S.postcondition state input output &&
                   S.invariant state in
      Some { level = 0; name = "SPEC"; passed; score = if passed then 1.0 else 0.0;
             details = if passed then "All contracts satisfied" else "Contract violation" }
    in
    { t with validators = validator :: t.validators }

  (** Add METRIC validator (Level 1) *)
  let with_metric f t =
    let validator _state input output =
      let score = f input output in
      Some { level = 1; name = "METRIC"; passed = true; score;
             details = Printf.sprintf "Quality score: %.2f" score }
    in
    { t with validators = validator :: t.validators }

  (** Add GOAL validator (Level 2) *)
  let with_goal f t =
    let validator _state input output =
      let result = f input output in
      Some { level = 2; name = "GOAL"; passed = result.achieved; score = result.progress;
             details = result.description }
    in
    { t with validators = validator :: t.validators }

  (** Add TEMPORAL validator (Level 3) *)
  let with_temporal f t =
    let validator _state _input _output =
      let passed = f (List.rev !(t.trace)) in
      Some { level = 3; name = "TEMPORAL"; passed; score = if passed then 1.0 else 0.0;
             details = if passed then "Temporal properties satisfied" else "Temporal violation" }
    in
    { t with validators = validator :: t.validators }

  (** Add EMERGENT validator (Level 4) *)
  let with_emergent f t =
    let validator _state _input _output =
      let score = f () in
      let passed = score >= 0.8 in  (* Default threshold *)
      Some { level = 4; name = "EMERGENT"; passed; score;
             details = Printf.sprintf "System health: %.2f" score }
    in
    { t with validators = validator :: t.validators }

  (** Add JUDGMENT validator (Level 5) *)
  let with_judgment f t =
    let validator _state _input _output =
      let (is_positive, confidence) = f () in
      Some { level = 5; name = "JUDGMENT"; passed = is_positive; score = confidence;
             details = if is_positive then "Human approved" else "Human rejected" }
    in
    { t with validators = validator :: t.validators }

  (** Record state for temporal validation *)
  let record_state s t =
    t.trace := s :: !(t.trace)

  (** Run all validators *)
  let validate t state input output =
    let results = List.filter_map (fun v -> v state input output) t.validators in
    let all_passed = List.for_all (fun r -> r.passed) results in
    let summary = Printf.sprintf "%s - %d/%d levels passed"
      (if all_passed then "✅ VALID" else "❌ INVALID")
      (List.length (List.filter (fun r -> r.passed) results))
      (List.length results) in
    { all_passed; levels = results; summary }
end

(* ========================================================================== *)
(* Pretty Printing                                                            *)
(* ========================================================================== *)

module Pretty = struct
  let level_to_string = function
    | 0 -> "SPEC (Correctness)"
    | 1 -> "METRIC (Quality)"
    | 2 -> "GOAL (Achievement)"
    | 3 -> "TEMPORAL (Time)"
    | 4 -> "EMERGENT (System)"
    | 5 -> "JUDGMENT (Human)"
    | n -> Printf.sprintf "LEVEL_%d" n

  let result_to_string r =
    Printf.sprintf "[%s] Level %d - %s: %s (%.1f%%)\n  %s"
      (if r.passed then "✓" else "✗")
      r.level
      (level_to_string r.level)
      r.name
      (r.score *. 100.0)
      r.details

  let stack_result_to_string sr =
    let header = Printf.sprintf "═══════════════════════════════════════════════════════\n%s\n═══════════════════════════════════════════════════════"
      sr.summary in
    let levels = String.concat "\n\n" (List.map result_to_string sr.levels) in
    Printf.sprintf "%s\n\n%s" header levels
end

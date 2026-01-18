(** Retry with Exponential Backoff - Eio Version

    Effect-based retry logic using Eio.Time for delays.
    Direct style - no monadic wrapping.
*)

open Agent_types

(** Calculate delay with optional jitter *)
let calculate_delay ~policy ~attempt =
  let base_delay =
    float_of_int policy.initial_delay_ms *.
    (policy.backoff_multiplier ** float_of_int attempt)
  in
  let capped_delay = min base_delay (float_of_int policy.max_delay_ms) in
  if policy.jitter then
    (* Add ±25% jitter *)
    let jitter_range = capped_delay *. 0.25 in
    let jitter = (Random.float (jitter_range *. 2.0)) -. jitter_range in
    max 0.0 (capped_delay +. jitter)
  else
    capped_delay

(** Retry a function with exponential backoff.

    @param clock Eio clock for sleeping
    @param policy Retry configuration
    @param f Function to retry (returns result)
    @return retry_result with success value or failure info
*)
let with_retry ~clock policy f =
  let rec loop attempt last_error =
    if attempt >= policy.max_attempts then
      Exhausted { attempts = attempt; last_error }
    else begin
      (* Add delay between retries (not on first attempt) *)
      if attempt > 0 then begin
        let delay_ms = calculate_delay ~policy ~attempt in
        let delay_sec = delay_ms /. 1000.0 in
        Eio.Time.sleep clock delay_sec
      end;

      match f () with
      | Ok result -> Success result
      | Error err -> loop (attempt + 1) err
    end
  in
  loop 0 "no attempts made"

(** Retry with circuit breaker pattern.

    If failures exceed threshold in time window, open circuit.
    @param clock Eio clock
    @param policy Retry policy
    @param circuit_threshold Number of failures to trip circuit
    @param circuit_window_sec Time window for counting failures
    @param f Function to retry
*)
let with_retry_circuit ~clock ~policy ~circuit_threshold ~circuit_window_sec f =
  (* Simple circuit breaker state *)
  let failure_times = ref [] in
  let now () = Eio.Time.now clock in

  let count_recent_failures () =
    let cutoff = now () -. circuit_window_sec in
    failure_times := List.filter (fun t -> t > cutoff) !failure_times;
    List.length !failure_times
  in

  let rec loop attempt last_error =
    (* Check circuit breaker *)
    if count_recent_failures () >= circuit_threshold then
      RetryCircuitOpen
    else if attempt >= policy.max_attempts then
      Exhausted { attempts = attempt; last_error }
    else begin
      if attempt > 0 then begin
        let delay_ms = calculate_delay ~policy ~attempt in
        Eio.Time.sleep clock (delay_ms /. 1000.0)
      end;

      match f () with
      | Ok result -> Success result
      | Error err ->
        failure_times := now () :: !failure_times;
        loop (attempt + 1) err
    end
  in
  loop 0 "no attempts made"

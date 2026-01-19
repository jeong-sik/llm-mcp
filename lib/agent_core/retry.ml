(** Retry Module - Exponential backoff with jitter

    Provides configurable retry logic for transient failures.
    Matches the design from masc-mcp/lib/retry.ml.
*)

let sleep_ms ms =
  Lwt_unix.sleep (float_of_int ms /. 1000.0)

let add_jitter ~jitter delay =
  if jitter then
    let variance = delay / 4 in  (* ±25% *)
    let offset = Random.int (max 1 (variance * 2)) - variance in
    max 1 (delay + offset)
  else
    delay

let calculate_delay (policy : Agent_types.retry_policy) attempt =
  let base = float_of_int policy.initial_delay_ms in
  let multiplier = policy.backoff_multiplier ** float_of_int (attempt - 1) in
  let delay = int_of_float (base *. multiplier) in
  let capped = min delay policy.max_delay_ms in
  add_jitter ~jitter:policy.jitter capped

let with_retry (policy : Agent_types.retry_policy) f =
  let open Agent_types in
  let rec loop attempt =
    let%lwt result = f () in
    match result with
    | Ok v -> Lwt.return (Success v)
    | Error err when attempt >= policy.max_attempts ->
      Lwt.return (Exhausted { attempts = attempt; last_error = err })
    | Error _err ->
      let delay = calculate_delay policy attempt in
      let%lwt () = sleep_ms delay in
      loop (attempt + 1)
  in
  loop 1

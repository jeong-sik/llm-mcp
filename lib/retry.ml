(** Retry Logic with Exponential Backoff

    Provides resilient API call handling with configurable retry policies.
    Inspired by AWS SDK and Google Cloud Client libraries.
*)

open Printf

(** {1 Configuration Types} *)

type retry_config = {
  max_attempts : int;           (** Maximum number of retry attempts (including first try) *)
  initial_delay_ms : int;       (** Initial delay in milliseconds before first retry *)
  max_delay_ms : int;           (** Maximum delay cap in milliseconds *)
  backoff_multiplier : float;   (** Multiplier for exponential backoff (typically 2.0) *)
  jitter : bool;                (** Add random jitter to prevent thundering herd *)
}

(** Default configuration: 3 attempts, 1s initial, 30s max, 2x multiplier *)
let default_config = {
  max_attempts = 3;
  initial_delay_ms = 1000;
  max_delay_ms = 30000;
  backoff_multiplier = 2.0;
  jitter = true;
}

(** Aggressive retry for critical operations *)
let aggressive_config = {
  max_attempts = 5;
  initial_delay_ms = 500;
  max_delay_ms = 60000;
  backoff_multiplier = 2.0;
  jitter = true;
}

(** Quick retry for idempotent operations *)
let quick_config = {
  max_attempts = 2;
  initial_delay_ms = 200;
  max_delay_ms = 2000;
  backoff_multiplier = 1.5;
  jitter = false;
}

(** {1 Retry Result Types} *)

type 'a retry_result =
  | Success of 'a
  | AllAttemptsFailed of {
      attempts : int;
      last_error : string;
      errors : string list;
    }

(** {1 Retryable Error Detection} *)

(** Common retryable error patterns across LLM APIs *)
let is_rate_limit_error (response : string) : bool =
  let patterns = [
    "rate_limit";
    "Rate limit";
    "429";
    "Too Many Requests";
    "quota exceeded";
    "RESOURCE_EXHAUSTED";
    "overloaded";
  ] in
  List.exists (fun p ->
    try Str.search_forward (Str.regexp_case_fold p) response 0 >= 0
    with Not_found -> false
  ) patterns

let is_temporary_error (response : string) : bool =
  let patterns = [
    "503";
    "502";
    "504";
    "Service Unavailable";
    "Bad Gateway";
    "Gateway Timeout";
    "temporarily unavailable";
    "UNAVAILABLE";
    "internal error";
    "Internal Server Error";
  ] in
  List.exists (fun p ->
    try Str.search_forward (Str.regexp_case_fold p) response 0 >= 0
    with Not_found -> false
  ) patterns

let is_connection_error (response : string) : bool =
  let patterns = [
    "connection refused";
    "Connection refused";
    "ECONNREFUSED";
    "ETIMEDOUT";
    "network error";
    "DNS";
    "Could not resolve";
  ] in
  List.exists (fun p ->
    try Str.search_forward (Str.regexp_case_fold p) response 0 >= 0
    with Not_found -> false
  ) patterns

(** Determine if an error is retryable *)
let is_retryable_error (response : string) : bool =
  is_rate_limit_error response ||
  is_temporary_error response ||
  is_connection_error response

(** {1 Delay Calculation} *)

(** Calculate delay for a given attempt number (1-indexed) *)
let calculate_delay ~config ~attempt : int =
  let base_delay =
    float_of_int config.initial_delay_ms *.
    (config.backoff_multiplier ** float_of_int (attempt - 1))
  in
  let capped_delay = min base_delay (float_of_int config.max_delay_ms) in
  let final_delay =
    if config.jitter then
      (* Add up to 25% jitter *)
      let jitter_factor = 0.75 +. (Random.float 0.5) in
      capped_delay *. jitter_factor
    else
      capped_delay
  in
  int_of_float final_delay

(** {1 Retry Execution with Eio} *)

(** Execute a function with retry logic using Eio for async sleep.

    @param sw Eio switch for fiber management
    @param clock Eio clock for sleeping
    @param config Retry configuration
    @param is_retryable Optional custom function to determine if error is retryable
    @param f The function to execute, returns (Ok result) or (Error message)
    @return retry_result with success value or failure details
*)
let with_retry
    ~(clock : _ Eio.Time.clock)
    ?(config = default_config)
    ?(is_retryable = is_retryable_error)
    (f : unit -> ('a, string) result)
  : 'a retry_result =
  let rec loop attempt errors =
    if attempt > config.max_attempts then
      AllAttemptsFailed {
        attempts = attempt - 1;
        last_error = (match errors with [] -> "Unknown error" | e :: _ -> e);
        errors = List.rev errors;
      }
    else
      match f () with
      | Ok result -> Success result
      | Error err_msg ->
          if is_retryable err_msg && attempt < config.max_attempts then begin
            let delay_ms = calculate_delay ~config ~attempt in
            (* Log retry attempt *)
            eprintf "[retry] Attempt %d/%d failed: %s. Retrying in %dms...\n%!"
              attempt config.max_attempts
              (String.sub err_msg 0 (min 100 (String.length err_msg)))
              delay_ms;
            (* Sleep before retry *)
            Eio.Time.sleep clock (float_of_int delay_ms /. 1000.0);
            loop (attempt + 1) (err_msg :: errors)
          end else begin
            (* Non-retryable error or max attempts reached *)
            if attempt >= config.max_attempts then
              eprintf "[retry] All %d attempts failed. Last error: %s\n%!"
                config.max_attempts err_msg
            else
              eprintf "[retry] Non-retryable error: %s\n%!"
                (String.sub err_msg 0 (min 100 (String.length err_msg)));
            AllAttemptsFailed {
              attempts = attempt;
              last_error = err_msg;
              errors = List.rev (err_msg :: errors);
            }
          end
  in
  loop 1 []

(** {1 Rate Limit Specific Handling} *)

(** Parse retry-after header value from error response *)
let parse_retry_after (response : string) : int option =
  (* Try to find retry_after in JSON response *)
  try
    let json = Yojson.Safe.from_string response in
    let open Yojson.Safe.Util in
    (* GLM style: {"error": {"retry_after": 5}} *)
    let retry_after =
      try json |> member "error" |> member "retry_after" |> to_int_option
      with _ -> None
    in
    match retry_after with
    | Some v -> Some (v * 1000)  (* Convert seconds to ms *)
    | None ->
        (* OpenAI style: check headers in message *)
        try
          let msg = json |> member "error" |> member "message" |> to_string in
          (* Parse "Please retry after X seconds" pattern *)
          let re = Str.regexp "retry after \\([0-9]+\\)" in
          if Str.search_forward re (String.lowercase_ascii msg) 0 >= 0 then
            Some (int_of_string (Str.matched_group 1 msg) * 1000)
          else None
        with _ -> None
  with _ -> None

(** Execute with rate limit awareness - respects retry-after headers *)
let with_rate_limit_retry
    ~(clock : _ Eio.Time.clock)
    ?(config = default_config)
    (f : unit -> ('a, string) result)
  : 'a retry_result =
  let is_retryable_with_wait err_msg =
    if is_rate_limit_error err_msg then begin
      (* Check for explicit retry-after *)
      match parse_retry_after err_msg with
      | Some wait_ms ->
          eprintf "[rate-limit] Server requested wait of %dms\n%!" wait_ms;
          Eio.Time.sleep clock (float_of_int wait_ms /. 1000.0);
          true
      | None -> true  (* Use default backoff *)
    end else
      is_retryable_error err_msg
  in
  with_retry ~clock ~config ~is_retryable:is_retryable_with_wait f

(** {1 Utility Functions} *)

(** Convert retry_result to standard Result type *)
let to_result : 'a retry_result -> ('a, string) result = function
  | Success x -> Ok x
  | AllAttemptsFailed { last_error; _ } -> Error last_error

(** Map over successful retry result *)
let map (f : 'a -> 'b) : 'a retry_result -> 'b retry_result = function
  | Success x -> Success (f x)
  | AllAttemptsFailed e -> AllAttemptsFailed e

(** Bind for retry results *)
let bind (f : 'a -> 'b retry_result) : 'a retry_result -> 'b retry_result = function
  | Success x -> f x
  | AllAttemptsFailed e -> AllAttemptsFailed e

(** Pretty print retry result for logging *)
let string_of_retry_result (to_string : 'a -> string) : 'a retry_result -> string = function
  | Success x -> sprintf "Success: %s" (to_string x)
  | AllAttemptsFailed { attempts; last_error; errors } ->
      sprintf "Failed after %d attempts. Last error: %s. All errors: [%s]"
        attempts last_error (String.concat "; " errors)

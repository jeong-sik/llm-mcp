(** Retry Logic with Exponential Backoff

    Provides resilient API call handling with configurable retry policies.
*)

(** {1 Configuration} *)

type retry_config = {
  max_attempts : int;           (** Maximum number of retry attempts (including first try) *)
  initial_delay_ms : int;       (** Initial delay in milliseconds before first retry *)
  max_delay_ms : int;           (** Maximum delay cap in milliseconds *)
  backoff_multiplier : float;   (** Multiplier for exponential backoff (typically 2.0) *)
  jitter : bool;                (** Add random jitter to prevent thundering herd *)
}

val default_config : retry_config
(** Default: 3 attempts, 1s initial, 30s max, 2x multiplier, jitter enabled *)

val aggressive_config : retry_config
(** Aggressive: 5 attempts, 500ms initial, 60s max *)

val quick_config : retry_config
(** Quick: 2 attempts, 200ms initial, 2s max, no jitter *)

(** {1 Result Types} *)

type 'a retry_result =
  | Success of 'a
  | AllAttemptsFailed of {
      attempts : int;
      last_error : string;
      errors : string list;
    }

(** {1 Error Detection} *)

val is_rate_limit_error : string -> bool
(** Detect rate limit errors (429, quota exceeded, etc.) *)

val is_temporary_error : string -> bool
(** Detect temporary server errors (503, 502, etc.) *)

val is_connection_error : string -> bool
(** Detect connection errors (ECONNREFUSED, DNS, etc.) *)

val is_retryable_error : string -> bool
(** Combined check for any retryable error *)

(** {1 Retry Execution} *)

val with_retry :
  clock:_ Eio.Time.clock ->
  ?config:retry_config ->
  ?is_retryable:(string -> bool) ->
  (unit -> ('a, string) result) ->
  'a retry_result
(** Execute a function with retry logic.
    @param clock Eio clock for sleeping between retries
    @param config Retry configuration (default: [default_config])
    @param is_retryable Custom retryable error detector (default: [is_retryable_error])
    @param f Function to execute, returns [Ok result] or [Error message] *)

val with_rate_limit_retry :
  clock:_ Eio.Time.clock ->
  ?config:retry_config ->
  (unit -> ('a, string) result) ->
  'a retry_result
(** Execute with rate limit awareness - respects retry-after headers *)

(** {1 Utilities} *)

val to_result : 'a retry_result -> ('a, string) result
(** Convert retry_result to standard Result type *)

val map : ('a -> 'b) -> 'a retry_result -> 'b retry_result
(** Map over successful result *)

val bind : ('a -> 'b retry_result) -> 'a retry_result -> 'b retry_result
(** Monadic bind for retry results *)

val parse_retry_after : string -> int option
(** Parse retry-after value from error response (in milliseconds) *)

val calculate_delay : config:retry_config -> attempt:int -> int
(** Calculate delay for a given attempt number *)

val string_of_retry_result : ('a -> string) -> 'a retry_result -> string
(** Pretty print retry result *)

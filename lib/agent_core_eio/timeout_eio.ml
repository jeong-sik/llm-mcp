(** Timeout Utilities - Eio Version

    Effect-based timeout using Eio.Fiber.first for racing.
    Clean cancellation via Eio's fiber system.
*)

(** Run a function with timeout using fiber racing.

    @param clock Eio clock
    @param timeout_ms Timeout in milliseconds
    @param f Function to run
    @return Some result if completed, None if timed out
*)
let with_timeout_ms ~clock ~timeout_ms f =
  let timeout_sec = float_of_int timeout_ms /. 1000.0 in
  Eio.Fiber.first
    (fun () -> Some (f ()))
    (fun () -> Eio.Time.sleep clock timeout_sec; None)

(** Run a function with timeout, returning result type.

    @param clock Eio clock
    @param timeout_sec Timeout in seconds
    @param f Function to run
    @return Ok result or Error "timeout"
*)
let with_timeout_result ~clock ~timeout_sec f =
  Eio.Fiber.first
    (fun () -> Ok (f ()))
    (fun () -> Eio.Time.sleep clock timeout_sec; Error "timeout")

(** Run with deadline (absolute time).

    @param clock Eio clock
    @param deadline Absolute deadline time
    @param f Function to run
    @return Some result if completed, None if deadline passed
*)
let with_deadline ~clock ~deadline f =
  let now = Eio.Time.now clock in
  let remaining = deadline -. now in
  if remaining <= 0.0 then
    None
  else
    with_timeout_ms ~clock ~timeout_ms:(int_of_float (remaining *. 1000.0)) f

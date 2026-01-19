(** Lwt Bridge - Eio/Lwt Interoperability

    Provides helpers for running Lwt code within Eio context.
    Uses lwt_eio library for seamless integration.

    Usage:
    {[
      Eio_main.run @@ fun env ->
      Lwt_bridge.with_lwt env @@ fun () ->
        (* Now you can call Lwt code with run_lwt *)
        let result = Lwt_bridge.run_lwt (some_lwt_promise) in
        ...
    ]}
*)

(** Initialize Lwt event loop within Eio context.

    This must wrap any code that needs to call Lwt functions.
    The Lwt event loop will use Eio as its backend.

    @param env Eio environment (from Eio_main.run)
    @param f Function to run with Lwt support
    @return Result of f
*)
let with_lwt env f =
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~clock @@ fun _token ->
  f ()

(** Run a Lwt promise within Eio context.

    Converts a Lwt.t to an Eio direct-style value.
    Must be called within a [with_lwt] block.

    @param promise Lwt promise to run
    @return The result of the promise
*)
let run_lwt promise =
  Lwt_eio.run_lwt (fun () -> promise)

(** Run a Lwt function within Eio context.

    Convenience wrapper for [run_lwt].

    @param f Function returning Lwt.t
    @return The result
*)
let run_lwt_fn f =
  Lwt_eio.run_lwt f

(** Run Lwt with result handling.

    @param promise Lwt promise returning Result
    @return The result, propagating errors
*)
let run_lwt_result promise =
  match run_lwt promise with
  | Ok v -> Ok v
  | Error e -> Error e

(** Run Lwt with exception handling.

    Catches Lwt exceptions and converts to Result.

    @param promise Lwt promise
    @return Ok result or Error with exception message
*)
let run_lwt_safe promise =
  try Ok (run_lwt promise)
  with exn -> Error (Printexc.to_string exn)

(** Parallel execution of multiple Lwt promises.

    Runs multiple Lwt promises concurrently using Lwt_list.map_p,
    then awaits all results.

    @param promises List of Lwt promises
    @return List of results
*)
let run_lwt_all promises =
  run_lwt (Lwt_list.map_p Fun.id promises)

(** Run Eio code from Lwt context.

    Use this when you need to call Eio functions from within Lwt code.
    IMPORTANT: Never call Eio code directly from Lwt - always use this wrapper.

    @param f Eio function to run
    @return Lwt.t with the result
*)
let run_eio f =
  Lwt_eio.run_eio f

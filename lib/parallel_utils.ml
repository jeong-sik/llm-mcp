(** OCaml 5.x Parallel Utilities using Domainslib

    Provides true multicore parallelism for CPU-bound tasks.
    Uses a shared task pool to avoid Domain spawn overhead.

    Key difference from Lwt:
    - Lwt.both: Cooperative multitasking on single core (I/O concurrency)
    - parallel_*: True parallel execution on multiple cores
*)

(** Number of worker domains (cores) to use.
    M3 Max has 12 performance + 4 efficiency cores.
    Use 3 for MAGI Trinity (one per LLM). *)
let num_domains =
  let available = Domain.recommended_domain_count () in
  min 4 (max 2 available)  (* 2-4 domains *)

(** Shared task pool - created lazily on first use *)
let pool = lazy (Domainslib.Task.setup_pool ~num_domains ())

(** Get the shared pool *)
let get_pool () = Lazy.force pool

(** Run 2 tasks in parallel and return both results.
    Each task runs in its own Domain. *)
let parallel2 f1 f2 =
  let pool = get_pool () in
  Domainslib.Task.run pool (fun () ->
    let t1 = Domainslib.Task.async pool f1 in
    let t2 = Domainslib.Task.async pool f2 in
    (Domainslib.Task.await pool t1,
     Domainslib.Task.await pool t2))

(** Run 3 tasks in parallel - ideal for MAGI Trinity.
    Returns (result1, result2, result3). *)
let parallel3 f1 f2 f3 =
  let pool = get_pool () in
  Domainslib.Task.run pool (fun () ->
    let t1 = Domainslib.Task.async pool f1 in
    let t2 = Domainslib.Task.async pool f2 in
    let t3 = Domainslib.Task.async pool f3 in
    (Domainslib.Task.await pool t1,
     Domainslib.Task.await pool t2,
     Domainslib.Task.await pool t3))

(** Run N tasks in parallel, return list of results in order. *)
let parallel_map (tasks : (unit -> 'a) list) : 'a list =
  let pool = get_pool () in
  Domainslib.Task.run pool (fun () ->
    let handles = List.map (fun f -> Domainslib.Task.async pool f) tasks in
    List.map (Domainslib.Task.await pool) handles)

(** Run Lwt computation in a separate Domain.
    Useful for parallel Lwt I/O when each Domain has its own event loop.

    WARNING: Each Domain runs its own Lwt_main.run, so Lwt state is NOT shared.
    HTTP connection pools, etc. should be per-Domain. *)
let parallel_lwt3 lwt1 lwt2 lwt3 =
  parallel3
    (fun () -> Lwt_main.run lwt1)
    (fun () -> Lwt_main.run lwt2)
    (fun () -> Lwt_main.run lwt3)

(** Cleanup the pool when done (optional, for clean shutdown) *)
let teardown () =
  if Lazy.is_val pool then
    Domainslib.Task.teardown_pool (Lazy.force pool)

(** Get pool statistics *)
let stats () =
  Printf.sprintf "Parallel pool: %d domains (recommended: %d)"
    num_domains (Domain.recommended_domain_count ())

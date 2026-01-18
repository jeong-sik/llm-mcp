(** Timeout Module - Lwt-based timeout wrapper *)

let with_timeout_ms timeout_ms f =
  let timeout_s = float_of_int timeout_ms /. 1000.0 in
  Lwt.catch
    (fun () ->
      Lwt.pick [
        (let%lwt result = f () in Lwt.return (Some result));
        (let%lwt () = Lwt_unix.sleep timeout_s in Lwt.return None);
      ])
    (fun _exn -> Lwt.return None)

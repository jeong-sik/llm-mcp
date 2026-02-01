(** Version auto-synced from dune-project via dune-build-info *)

let version =
  match Build_info.V1.version () with
  | None -> "0.0.0"
  | Some v -> Build_info.V1.Version.to_string v

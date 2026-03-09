let home_dir () =
  try Unix.getenv "HOME" with Not_found -> "/tmp"

let claude_projects_root ?home () =
  let home = Option.value home ~default:(home_dir ()) in
  Filename.concat home ".claude/projects"

let claude_project_slug path =
  String.map (fun c -> if c = '/' then '-' else c) path

let normalize_override = function
  | Some path ->
      let trimmed = String.trim path in
      if trimmed = "" then None else Some trimmed
  | None -> None

let select_search_root ?cwd ?home ?env_override ?(exists = Sys.file_exists)
    ?(env_lookup = Sys.getenv_opt) () =
  match normalize_override env_override with
  | Some path -> path
  | None -> (
      match normalize_override (env_lookup "CLAUDE_TRANSCRIPTS_DIR") with
      | Some path -> path
      | None ->
          let root = claude_projects_root ?home () in
          let cwd = Option.value cwd ~default:(Sys.getcwd ()) in
          let project_root = Filename.concat root (claude_project_slug cwd) in
          if exists project_root then project_root else root)

open Alcotest

let test_claude_project_slug () =
  let slug = Transcript_paths.claude_project_slug "/Users/dancer/me/workspace/yousleepwhen/llm-mcp" in
  check string "slashes become dashes"
    "-Users-dancer-me-workspace-yousleepwhen-llm-mcp"
    slug

let test_select_search_root_env_override () =
  let root =
    Transcript_paths.select_search_root
      ~home:"/tmp/home"
      ~cwd:"/Users/dancer/me/workspace/yousleepwhen/llm-mcp"
      ~env_override:"/tmp/custom-transcripts"
      ~env_lookup:(fun _ -> None)
      ()
  in
  check string "env override wins" "/tmp/custom-transcripts" root

let test_select_search_root_project_match () =
  let expected =
    Filename.concat
      (Transcript_paths.claude_projects_root ~home:"/tmp/home" ())
      "-Users-dancer-me-workspace-yousleepwhen-llm-mcp"
  in
  let root =
    Transcript_paths.select_search_root
      ~home:"/tmp/home"
      ~cwd:"/Users/dancer/me/workspace/yousleepwhen/llm-mcp"
      ~env_override:""
      ~exists:(fun path -> path = expected)
      ~env_lookup:(fun _ -> None)
      ()
  in
  check string "project root preferred" expected root

let test_select_search_root_fallback_to_projects_root () =
  let root =
    Transcript_paths.select_search_root
      ~home:"/tmp/home"
      ~cwd:"/Users/dancer/me/workspace/yousleepwhen/llm-mcp"
      ~env_override:""
      ~exists:(fun _ -> false)
      ~env_lookup:(fun _ -> None)
      ()
  in
  check bool "falls back to projects root"
    true
    (String.ends_with ~suffix:"/.claude/projects" root)

let () =
  run "transcript_paths" [
    ("slug", [
      test_case "claude project slug" `Quick test_claude_project_slug;
    ]);
    ("root", [
      test_case "env override" `Quick test_select_search_root_env_override;
      test_case "project root preferred" `Quick test_select_search_root_project_match;
      test_case "fallback to generic root" `Quick test_select_search_root_fallback_to_projects_root;
    ]);
  ]

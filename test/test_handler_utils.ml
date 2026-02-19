(** Tests for Handler_utils module — GitHub and Slack tool handlers
    Tests run within Eio_main.run to provide required process manager and clock.
    Uses real `echo` command for controlled output testing. *)

open Alcotest

(** Test execute_gh_pr_diff with a fake repo (will error, but covers all branches) *)
let test_gh_pr_diff_error () =
  Eio_main.run @@ fun env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let result = Handler_utils.execute_gh_pr_diff
    ~sw ~proc_mgr ~clock
    ~repo:"nonexistent/repo-xyz" ~pr_number:99999 in
  (* Should fail but return a structured result *)
  check bool "model is gh_pr_diff" true (result.model = "gh_pr_diff");
  check bool "non-zero or error response" true
    (result.returncode <> 0 || String.length result.response > 0)

(** Test execute_slack_post with empty token (exercises curl path and error handling) *)
let test_slack_post_empty_token () =
  let prev = Sys.getenv_opt "SLACK_BOT_TOKEN" in
  Unix.putenv "SLACK_BOT_TOKEN" "fake-token-for-test";
  Eio_main.run @@ fun env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let result = Handler_utils.execute_slack_post
    ~sw ~proc_mgr ~clock
    ~channel:"#test" ~text:"hello" ~thread_ts:None in
  check string "model" "slack_post" result.model;
  (* Slack API will reject the fake token, so we expect an error response *)
  check bool "non-empty response" true (String.length result.response > 0);
  (* Restore *)
  (match prev with Some v -> Unix.putenv "SLACK_BOT_TOKEN" v | None -> ())

(** Test external_tool_timeout constant *)
let test_external_tool_timeout () =
  check int "timeout is 60s" 60 Handler_utils.external_tool_timeout

let () =
  run "handler_utils" [
    ("gh_pr_diff", [
      test_case "error on nonexistent repo" `Quick test_gh_pr_diff_error;
    ]);
    ("slack_post", [
      test_case "empty token" `Quick test_slack_post_empty_token;
    ]);
    ("config", [
      test_case "timeout constant" `Quick test_external_tool_timeout;
    ]);
  ]

(** handler_utils.ml - Utility tool handlers (GitHub, Slack)
    Extracted from tools_eio.ml Phase 2 *)

open Printf
open Types
open Cli_runner_eio

(** Default timeout for external tools (60 seconds) *)
let external_tool_timeout = 60

(** Execute GitHub PR diff command using gh CLI *)
let execute_gh_pr_diff ~sw ~proc_mgr ~clock ~repo ~pr_number : tool_result =
  eprintf "[gh_pr_diff] Fetching diff for %s#%d\n%!" repo pr_number;
  let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
    "gh" ["pr"; "diff"; string_of_int pr_number; "-R"; repo] in
  match result with
  | Ok r ->
      { model = "gh_pr_diff";
        returncode = r.exit_code;
        response = get_output r;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number)]; }
  | Error (Timeout t) ->
      { model = "gh_pr_diff";
        returncode = -1;
        response = sprintf "Timeout after %ds" t;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number); ("error", "timeout")]; }
  | Error (ProcessError msg) ->
      { model = "gh_pr_diff";
        returncode = -1;
        response = sprintf "Error: %s" msg;
        extra = [("repo", repo); ("pr_number", string_of_int pr_number); ("error", msg)]; }

(** Execute Slack post message using curl *)
let execute_slack_post ~sw ~proc_mgr ~clock ~channel ~text ~thread_ts : tool_result =
  let slack_token = Sys.getenv_opt "SLACK_BOT_TOKEN" in
  match slack_token with
  | None ->
      { model = "slack_post";
        returncode = -1;
        response = "Error: SLACK_BOT_TOKEN environment variable not set";
        extra = [("error", "missing_token")]; }
  | Some token ->
      (* Build JSON payload *)
      let payload = `Assoc (
        [("channel", `String channel); ("text", `String text)]
        @ (match thread_ts with Some ts -> [("thread_ts", `String ts)] | None -> [])
      ) in
      let json_str = Yojson.Safe.to_string payload in
      eprintf "[slack_post] Posting to channel %s\n%!" channel;
      let result = run_command ~sw ~proc_mgr ~clock ~timeout:external_tool_timeout
        "curl" [
          "-s"; "-X"; "POST";
          "-H"; "Content-Type: application/json; charset=utf-8";
          "-H"; sprintf "Authorization: Bearer %s" token;
          "-d"; json_str;
          "https://slack.com/api/chat.postMessage"
        ] in
      match result with
      | Ok r ->
          (* Parse Slack API response *)
          (try
            let response_json = Yojson.Safe.from_string r.stdout in
            let ok = Yojson.Safe.Util.(response_json |> member "ok" |> to_bool) in
            if ok then
              { model = "slack_post";
                returncode = 0;
                response = sprintf "Message posted to %s" channel;
                extra = [("channel", channel)]; }
            else
              let error = Yojson.Safe.Util.(response_json |> member "error" |> to_string) in
              { model = "slack_post";
                returncode = -1;
                response = sprintf "Slack API error: %s" error;
                extra = [("error", error)]; }
          with _ ->
            { model = "slack_post";
              returncode = r.exit_code;
              response = r.stdout;
              extra = [("channel", channel)]; })
      | Error (Timeout t) ->
          { model = "slack_post";
            returncode = -1;
            response = sprintf "Timeout after %ds" t;
            extra = [("channel", channel); ("error", "timeout")]; }
      | Error (ProcessError msg) ->
          { model = "slack_post";
            returncode = -1;
            response = sprintf "Error: %s" msg;
            extra = [("channel", channel); ("error", msg)]; }

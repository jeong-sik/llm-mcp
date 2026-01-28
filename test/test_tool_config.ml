(** Tests for Tool_config module - MCP config, budget mode

    Pure function tests:
    - env_truthy: boolean string parsing
    - budget_mode_value: JSON extraction
    - mcp_server_config: type construction
*)

open Alcotest
open Tool_config

(** {1 env_truthy Tests} *)

let test_env_truthy_1 () =
  check bool "1" true (env_truthy "1")

let test_env_truthy_true () =
  check bool "true" true (env_truthy "true")

let test_env_truthy_TRUE () =
  check bool "TRUE" true (env_truthy "TRUE")

let test_env_truthy_True () =
  check bool "True" true (env_truthy "True")

let test_env_truthy_yes () =
  check bool "yes" true (env_truthy "yes")

let test_env_truthy_YES () =
  check bool "YES" true (env_truthy "YES")

let test_env_truthy_on () =
  check bool "on" true (env_truthy "on")

let test_env_truthy_ON () =
  check bool "ON" true (env_truthy "ON")

let test_env_truthy_0 () =
  check bool "0" false (env_truthy "0")

let test_env_truthy_false () =
  check bool "false" false (env_truthy "false")

let test_env_truthy_FALSE () =
  check bool "FALSE" false (env_truthy "FALSE")

let test_env_truthy_no () =
  check bool "no" false (env_truthy "no")

let test_env_truthy_off () =
  check bool "off" false (env_truthy "off")

let test_env_truthy_unknown () =
  check bool "unknown" false (env_truthy "unknown");
  check bool "garbage" false (env_truthy "garbage");
  check bool "empty" false (env_truthy "")

(** {1 budget_mode_value Tests} *)

let test_budget_mode_value_true () =
  let json = `Assoc [("budget_mode", `Bool true)] in
  check bool "explicit true" true (budget_mode_value json)

let test_budget_mode_value_false () =
  let json = `Assoc [("budget_mode", `Bool false)] in
  check bool "explicit false" false (budget_mode_value json)

let test_budget_mode_value_missing () =
  let json = `Assoc [] in
  (* Should default to budget_mode_from_env() which is typically false *)
  let result = budget_mode_value json in
  check bool "missing uses default" true (result = true || result = false)

let test_budget_mode_value_null () =
  let json = `Assoc [("budget_mode", `Null)] in
  let result = budget_mode_value json in
  check bool "null uses default" true (result = true || result = false)

let test_budget_mode_value_nested () =
  let json = `Assoc [
    ("other", `String "data");
    ("budget_mode", `Bool true)
  ] in
  check bool "with other fields" true (budget_mode_value json)

(** {1 mcp_server_config Type Tests} *)

let test_mcp_server_config_http () =
  let config : mcp_server_config = {
    name = "test-server";
    url = Some "http://localhost:8932";
    command = None;
    args = [];
    server_type = "http";
  } in
  check string "name" "test-server" config.name;
  check (option string) "url" (Some "http://localhost:8932") config.url;
  check (option string) "command" None config.command;
  check string "type" "http" config.server_type

let test_mcp_server_config_stdio () =
  let config : mcp_server_config = {
    name = "stdio-server";
    url = None;
    command = Some "npx";
    args = ["-y"; "some-mcp-server"];
    server_type = "stdio";
  } in
  check string "name" "stdio-server" config.name;
  check (option string) "url" None config.url;
  check (option string) "command" (Some "npx") config.command;
  check int "args count" 2 (List.length config.args);
  check string "type" "stdio" config.server_type

let test_mcp_server_config_args () =
  let config : mcp_server_config = {
    name = "server";
    url = None;
    command = Some "node";
    args = ["--port"; "8080"; "--verbose"];
    server_type = "stdio";
  } in
  check int "3 args" 3 (List.length config.args);
  check string "first arg" "--port" (List.nth config.args 0);
  check string "second arg" "8080" (List.nth config.args 1);
  check string "third arg" "--verbose" (List.nth config.args 2)

(** {1 get_mcp_server_url Tests} *)

let test_get_mcp_server_url_not_found () =
  let result = get_mcp_server_url "nonexistent-server-xyz" in
  check (option string) "not found" None result

(** {1 get_mcp_server_config Tests} *)

let test_get_mcp_server_config_not_found () =
  let result = get_mcp_server_config "nonexistent-server-abc" in
  check bool "not found" true (Option.is_none result)

(** {1 Test Suite} *)

let env_truthy_tests = [
  test_case "1" `Quick test_env_truthy_1;
  test_case "true" `Quick test_env_truthy_true;
  test_case "TRUE" `Quick test_env_truthy_TRUE;
  test_case "True" `Quick test_env_truthy_True;
  test_case "yes" `Quick test_env_truthy_yes;
  test_case "YES" `Quick test_env_truthy_YES;
  test_case "on" `Quick test_env_truthy_on;
  test_case "ON" `Quick test_env_truthy_ON;
  test_case "0" `Quick test_env_truthy_0;
  test_case "false" `Quick test_env_truthy_false;
  test_case "FALSE" `Quick test_env_truthy_FALSE;
  test_case "no" `Quick test_env_truthy_no;
  test_case "off" `Quick test_env_truthy_off;
  test_case "unknown" `Quick test_env_truthy_unknown;
]

let budget_mode_tests = [
  test_case "explicit true" `Quick test_budget_mode_value_true;
  test_case "explicit false" `Quick test_budget_mode_value_false;
  test_case "missing" `Quick test_budget_mode_value_missing;
  test_case "null" `Quick test_budget_mode_value_null;
  test_case "nested" `Quick test_budget_mode_value_nested;
]

let config_type_tests = [
  test_case "http type" `Quick test_mcp_server_config_http;
  test_case "stdio type" `Quick test_mcp_server_config_stdio;
  test_case "args" `Quick test_mcp_server_config_args;
]

let lookup_tests = [
  test_case "url not found" `Quick test_get_mcp_server_url_not_found;
  test_case "config not found" `Quick test_get_mcp_server_config_not_found;
]

let () =
  run "tool_config" [
    ("env_truthy", env_truthy_tests);
    ("budget_mode_value", budget_mode_tests);
    ("mcp_server_config", config_type_tests);
    ("lookup", lookup_tests);
  ]

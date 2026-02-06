(** Tests for Agent_types module — Agent core type definitions
    Targets: role_to_string, string_to_role, message_to_json,
    tool_call_to_json, estimate_tokens_of_message, default values *)

open Alcotest

(** {1 Role Conversion} *)

let test_role_to_string () =
  check string "user" "user" (Agent_types.role_to_string Agent_types.User);
  check string "assistant" "assistant" (Agent_types.role_to_string Agent_types.Assistant);
  check string "tool" "tool" (Agent_types.role_to_string Agent_types.Tool);
  check string "system" "system" (Agent_types.role_to_string Agent_types.System)

let test_string_to_role () =
  check bool "user" true (Agent_types.string_to_role "user" = Agent_types.User);
  check bool "assistant" true (Agent_types.string_to_role "assistant" = Agent_types.Assistant);
  check bool "tool" true (Agent_types.string_to_role "tool" = Agent_types.Tool);
  check bool "system" true (Agent_types.string_to_role "system" = Agent_types.System);
  check bool "unknown defaults to user" true
    (Agent_types.string_to_role "unknown" = Agent_types.User)

let test_role_roundtrip () =
  let roles = [Agent_types.User; Agent_types.Assistant; Agent_types.Tool; Agent_types.System] in
  List.iter (fun r ->
    let s = Agent_types.role_to_string r in
    let r' = Agent_types.string_to_role s in
    check bool (Printf.sprintf "roundtrip %s" s) true (r = r')
  ) roles

(** {1 Message to JSON} *)

let test_message_to_json_basic () =
  let msg = Agent_types.{
    role = User;
    content = "Hello";
    tool_calls = None;
    name = None;
  } in
  let json = Agent_types.message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "user" (json |> member "role" |> to_string);
  check string "content" "Hello" (json |> member "content" |> to_string);
  check bool "no name" true (json |> member "name" = `Null)

let test_message_to_json_with_name () =
  let msg = Agent_types.{
    role = Tool;
    content = "result";
    tool_calls = None;
    name = Some "search_tool";
  } in
  let json = Agent_types.message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "tool" (json |> member "role" |> to_string);
  check string "name" "search_tool" (json |> member "name" |> to_string)

(** {1 Tool Call to JSON} *)

let test_tool_call_to_json () =
  let tc = Agent_types.{
    id = "tc_001";
    name = "get_weather";
    arguments = `Assoc [("city", `String "Seoul")];
  } in
  let json = Agent_types.tool_call_to_json tc in
  let open Yojson.Safe.Util in
  check string "id" "tc_001" (json |> member "id" |> to_string);
  check string "name" "get_weather" (json |> member "name" |> to_string);
  let args = json |> member "arguments" in
  check string "city" "Seoul" (args |> member "city" |> to_string)

(** {1 Token Estimation} *)

let test_estimate_tokens_basic () =
  let msg = Agent_types.{
    role = User;
    content = "Hello world";  (* 11 chars / 4 = 2 + 10 overhead = 12 *)
    tool_calls = None;
    name = None;
  } in
  let tokens = Agent_types.estimate_tokens_of_message msg in
  check bool "positive tokens" true (tokens > 0);
  (* 11 / 4 = 2, + 0 (no name) + 10 overhead = 12 *)
  check int "expected tokens" 12 tokens

let test_estimate_tokens_with_name () =
  let msg = Agent_types.{
    role = Tool;
    content = "result data here";  (* 16 chars / 4 = 4 *)
    tool_calls = None;
    name = Some "search";  (* 6 chars / 4 = 1 *)
  } in
  let tokens = Agent_types.estimate_tokens_of_message msg in
  (* 4 + 1 + 10 = 15 *)
  check int "expected tokens with name" 15 tokens

let test_estimate_tokens_empty () =
  let msg = Agent_types.{
    role = User;
    content = "";
    tool_calls = None;
    name = None;
  } in
  let tokens = Agent_types.estimate_tokens_of_message msg in
  (* 0 + 0 + 10 = 10 (overhead only) *)
  check int "overhead only" 10 tokens

(** {1 Default Values} *)

let test_default_retry_policy () =
  let p = Agent_types.default_retry_policy in
  check bool "max_attempts positive" true (p.max_attempts > 0);
  check bool "initial_delay positive" true (p.initial_delay_ms > 0);
  check bool "max_delay positive" true (p.max_delay_ms > 0);
  check bool "multiplier > 1" true (p.backoff_multiplier > 1.0)

let test_default_loop_config () =
  let c = Agent_types.default_loop_config in
  check int "max_turns" 10 c.max_turns;
  check int "max_messages" 50 c.max_messages;
  check int "timeout_ms" 60_000 c.timeout_ms

(** {1 Test Suite} *)

let () =
  run "agent_types" [
    ("role", [
      test_case "to_string" `Quick test_role_to_string;
      test_case "of_string" `Quick test_string_to_role;
      test_case "roundtrip" `Quick test_role_roundtrip;
    ]);
    ("message_json", [
      test_case "basic" `Quick test_message_to_json_basic;
      test_case "with name" `Quick test_message_to_json_with_name;
    ]);
    ("tool_call_json", [
      test_case "to_json" `Quick test_tool_call_to_json;
    ]);
    ("token_estimation", [
      test_case "basic" `Quick test_estimate_tokens_basic;
      test_case "with name" `Quick test_estimate_tokens_with_name;
      test_case "empty" `Quick test_estimate_tokens_empty;
    ]);
    ("defaults", [
      test_case "retry_policy" `Quick test_default_retry_policy;
      test_case "loop_config" `Quick test_default_loop_config;
    ]);
  ]

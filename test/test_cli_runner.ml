(** Tests for cli_runner - especially the shell injection-safe stdin function *)

open Alcotest
open Lwt.Syntax

(** Helper to run Lwt tests *)
let lwt_test f () = Lwt_main.run (f ())

(** Test run_command_with_stdin: basic functionality *)
let test_stdin_basic () =
  let* result = Llm_mcp.Cli_runner.run_command_with_stdin
    ~timeout:5
    ~stdin_data:"hello world"
    "cat"
    []
  in
  match result with
  | Ok r ->
      check int "exit code 0" 0 r.exit_code;
      check bool "output contains input" true
        (String.trim r.stdout = "hello world");
      Lwt.return_unit
  | Error _ ->
      fail "unexpected error"

(** Test shell injection safety: malicious input should NOT execute *)
let test_stdin_no_shell_injection () =
  (* This would be dangerous if passed through shell:
     ; rm -rf / ; echo pwned
     But with direct exec, it's just literal stdin data *)
  let malicious = "; rm -rf / ; echo pwned\n$(whoami)\n`id`" in
  let* result = Llm_mcp.Cli_runner.run_command_with_stdin
    ~timeout:5
    ~stdin_data:malicious
    "cat"
    []
  in
  match result with
  | Ok r ->
      check int "exit code 0" 0 r.exit_code;
      (* Output should be the literal malicious string, not executed *)
      check bool "literal output" true
        (String.trim r.stdout = String.trim malicious);
      (* Should NOT contain "pwned" from command execution *)
      check bool "no command execution" false
        (String.sub r.stdout 0 (min 5 (String.length r.stdout)) = "root\n");
      Lwt.return_unit
  | Error _ ->
      fail "unexpected error"

(** Test arguments are passed correctly (no shell escaping needed) *)
let test_stdin_with_args () =
  (* Test that arguments with special chars work correctly *)
  let* result = Llm_mcp.Cli_runner.run_command_with_stdin
    ~timeout:5
    ~stdin_data:"test input"
    "sh"
    ["-c"; "cat; echo done"]  (* args passed as array, not string *)
  in
  match result with
  | Ok r ->
      check int "exit code 0" 0 r.exit_code;
      check bool "has test input" true
        (String.length r.stdout > 0 &&
         String.sub r.stdout 0 (min 10 (String.length r.stdout)) = "test input");
      Lwt.return_unit
  | Error _ ->
      fail "unexpected error"

(** Test timeout handling *)
let test_stdin_timeout () =
  let* result = Llm_mcp.Cli_runner.run_command_with_stdin
    ~timeout:1
    ~stdin_data:"waiting..."
    "sleep"
    ["10"]  (* This should timeout *)
  in
  match result with
  | Error (Llm_mcp.Cli_runner.Timeout t) ->
      check int "timeout value" 1 t;
      Lwt.return_unit
  | Ok _ ->
      fail "expected timeout"
  | Error (Llm_mcp.Cli_runner.ProcessError _) ->
      (* Some systems might return process error instead *)
      Lwt.return_unit

(** Test EPIPE handling: process exits before reading all input *)
let test_stdin_epipe () =
  (* head -n1 will close stdin after reading one line *)
  let* result = Llm_mcp.Cli_runner.run_command_with_stdin
    ~timeout:5
    ~stdin_data:"line1\nline2\nline3\nline4\nline5\n"
    "head"
    ["-n"; "1"]
  in
  match result with
  | Ok r ->
      check int "exit code 0" 0 r.exit_code;
      check bool "got first line" true
        (String.trim r.stdout = "line1");
      Lwt.return_unit
  | Error _ ->
      fail "EPIPE should be handled gracefully"

(** Test empty stdin *)
let test_stdin_empty () =
  let* result = Llm_mcp.Cli_runner.run_command_with_stdin
    ~timeout:5
    ~stdin_data:""
    "cat"
    []
  in
  match result with
  | Ok r ->
      check int "exit code 0" 0 r.exit_code;
      check bool "empty output" true (String.trim r.stdout = "");
      Lwt.return_unit
  | Error _ ->
      fail "unexpected error"

(** Test large stdin data *)
let test_stdin_large () =
  let large_data = String.make 100000 'x' in  (* 100KB *)
  let* result = Llm_mcp.Cli_runner.run_command_with_stdin
    ~timeout:10
    ~stdin_data:large_data
    "wc"
    ["-c"]
  in
  match result with
  | Ok r ->
      check int "exit code 0" 0 r.exit_code;
      let count = int_of_string (String.trim r.stdout) in
      check int "byte count" 100000 count;
      Lwt.return_unit
  | Error _ ->
      fail "unexpected error with large input"

let () =
  run "Llm_mcp.Cli_runner" [
    "run_command_with_stdin", [
      test_case "basic" `Quick (lwt_test test_stdin_basic);
      test_case "no shell injection" `Quick (lwt_test test_stdin_no_shell_injection);
      test_case "with args" `Quick (lwt_test test_stdin_with_args);
      test_case "timeout" `Quick (lwt_test test_stdin_timeout);
      test_case "EPIPE handling" `Quick (lwt_test test_stdin_epipe);
      test_case "empty stdin" `Quick (lwt_test test_stdin_empty);
      test_case "large stdin" `Slow (lwt_test test_stdin_large);
    ];
  ]

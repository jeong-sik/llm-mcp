(** Test CLI Runner Eio - Direct-style subprocess execution *)

open Llm_mcp.Cli_runner_eio

let () = Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let clock = Eio.Stdenv.clock env in

    (* Test 1: Simple echo command *)
    let () =
      match run_command ~sw ~proc_mgr ~clock ~timeout:5 "echo" ["hello"; "world"] with
      | Ok r ->
        assert (r.exit_code = 0);
        assert (String.trim r.stdout = "hello world");
        Printf.printf "[OK] echo test passed (stdout=%S)\n%!" r.stdout
      | Error (Timeout _) ->
        failwith "Unexpected timeout"
      | Error (ProcessError e) ->
        failwith ("Process error: " ^ e)
    in

    (* Test 2: Exit code *)
    let () =
      match run_command ~sw ~proc_mgr ~clock ~timeout:5 "false" [] with
      | Ok r ->
        assert (r.exit_code <> 0);
        Printf.printf "[OK] exit code test passed (code=%d)\n%!" r.exit_code
      | Error (Timeout _) ->
        failwith "Unexpected timeout"
      | Error (ProcessError e) ->
        failwith ("Process error: " ^ e)
    in

    (* Test 3: Stderr capture *)
    let () =
      match run_command ~sw ~proc_mgr ~clock ~timeout:5 "sh" ["-c"; "echo error >&2"] with
      | Ok r ->
        assert (String.trim r.stderr = "error");
        Printf.printf "[OK] stderr test passed (stderr=%S)\n%!" r.stderr
      | Error (Timeout _) ->
        failwith "Unexpected timeout"
      | Error (ProcessError e) ->
        failwith ("Process error: " ^ e)
    in

    (* Test 4: Timeout (intentionally short) *)
    let () =
      match run_command ~sw ~proc_mgr ~clock ~timeout:1 "sleep" ["10"] with
      | Ok _ ->
        failwith "Should have timed out"
      | Error (Timeout t) ->
        Printf.printf "[OK] timeout test passed (timeout=%ds)\n%!" t
      | Error (ProcessError e) ->
        failwith ("Process error: " ^ e)
    in

    (* Test 5: Streaming command *)
    let () =
      let lines = ref [] in
      let on_line line = lines := line :: !lines in
      match run_streaming_command ~sw ~proc_mgr ~clock ~timeout:5 ~on_line
              "sh" ["-c"; "echo line1; echo line2; echo line3"] with
      | Ok exit_code ->
        assert (exit_code = 0);
        let collected = List.rev !lines in
        assert (List.length collected = 3);
        Printf.printf "[OK] streaming test passed (lines=%d)\n%!" (List.length collected)
      | Error (Timeout _) ->
        failwith "Unexpected timeout"
      | Error (ProcessError e) ->
        failwith ("Process error: " ^ e)
    in

    (* Test 6: Command with stdin *)
    let () =
      match run_command_with_stdin ~sw ~proc_mgr ~clock ~timeout:5
              ~stdin_data:"hello from stdin\n" "cat" [] with
      | Ok r ->
        assert (r.exit_code = 0);
        assert (String.trim r.stdout = "hello from stdin");
        Printf.printf "[OK] stdin test passed (stdout=%S)\n%!" r.stdout
      | Error (Timeout _) ->
        failwith "Unexpected timeout"
      | Error (ProcessError e) ->
        failwith ("Process error: " ^ e)
    in

    (* Test 7: get_output helper *)
    let () =
      let result = { stdout = "  primary output  "; stderr = "secondary"; exit_code = 0 } in
      assert (get_output result = "primary output");
      let result2 = { stdout = "  "; stderr = "fallback"; exit_code = 0 } in
      assert (get_output result2 = "fallback");
      Printf.printf "[OK] get_output helper test passed\n%!"
    in

    (* Test 8: Convenience wrapper with env *)
    let () =
      match run_with_env ~sw ~env ~timeout:5 "echo" ["via"; "env"] with
      | Ok r ->
        assert (r.exit_code = 0);
        assert (String.trim r.stdout = "via env");
        Printf.printf "[OK] run_with_env test passed\n%!"
      | Error _ ->
        failwith "run_with_env failed"
    in

    Printf.printf "\n✅ All 8 CLI runner Eio tests passed!\n%!"

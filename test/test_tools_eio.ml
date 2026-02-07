(** Test Tools Eio - Direct-style LLM execution *)

open Tools_eio
open Types

let () = Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let clock = Eio.Stdenv.clock env in

    (* Test 1: Re-exported pure functions work *)
    let () =
      (* Test budget_mode_value - extracts boolean from JSON *)
      let json_with_budget = `Assoc [("budget_mode", `Bool true)] in
      let json_without_budget = `Assoc [("budget_mode", `Bool false)] in
      let json_default = `Assoc [] in
      let bm_true = budget_mode_value json_with_budget in
      let bm_false = budget_mode_value json_without_budget in
      let bm_default = budget_mode_value json_default in
      assert (bm_true = true);
      assert (bm_false = false);
      assert (bm_default = false);  (* default is false unless env set *)
      Printf.printf "[OK] budget_mode_value test passed\n%!"
    in

    (* Test 2: Gemini error classification works *)
    let () =
      let rate_err = classify_gemini_error "RESOURCE_EXHAUSTED: Out of quota" in
      assert (rate_err = Some RateLimitError);  (* quota triggers RateLimitError *)
      let rate_err2 = classify_gemini_error "rate limit exceeded" in
      assert (rate_err2 = Some RateLimitError);
      let auth_err = classify_gemini_error "400 Bad Request: Invalid API key" in
      assert (auth_err = Some AuthenticationError);
      let func_err = classify_gemini_error "function response parts mismatch" in
      assert (func_err = Some FunctionCallSyncError);
      let no_err = classify_gemini_error "Hello, this is a normal response" in
      assert (no_err = None);
      Printf.printf "[OK] Gemini error classification test passed\n%!"
    in

    (* Test 3: Gemini error recoverability *)
    let () =
      assert (is_recoverable_gemini_error FunctionCallSyncError = true);
      assert (is_recoverable_gemini_error RateLimitError = true);
      assert (is_recoverable_gemini_error AuthenticationError = false);
      assert (is_recoverable_gemini_error ContextTooLongError = false);
      Printf.printf "[OK] Gemini error recoverability test passed\n%!"
    in

    (* Test 4: MCP call with missing server *)
    let () =
      let result = call_mcp ~sw ~proc_mgr ~clock
        ~server_name:"nonexistent_server"
        ~tool_name:"test_tool"
        ~arguments:(`Assoc [("key", `String "value")])
        ~timeout:5
      in
      assert (String.sub result 0 5 = "Error");
      assert (String.length result > 20);  (* Contains error message *)
      Printf.printf "[OK] MCP missing server test passed (result=%s)\n%!" (String.sub result 0 (min 60 (String.length result)))
    in

    (* Test 4b: MCP HTTP response parsing (SSE + plain JSON) *)
    let () =
      let plain =
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"OK\"}]}}" in
      let sse =
        "event: message\n\
data: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"OK\"}]}}\n\n" in
      (match parse_mcp_http_response plain with
       | Some "OK" -> ()
       | other -> failwith ("parse_mcp_http_response plain failed: " ^ (Option.value other ~default:"(none)")));
      (match parse_mcp_http_response sse with
       | Some "OK" -> ()
       | other -> failwith ("parse_mcp_http_response sse failed: " ^ (Option.value other ~default:"(none)")));
      Printf.printf "[OK] MCP HTTP response parsing test passed\n%!"
    in

    (* Test 5: Execute OllamaList (tests parsing logic, may fail if ollama not installed) *)
    let () =
      let result = execute ~sw ~proc_mgr ~clock OllamaList in
      assert (result.model = "ollama_list");
      (* Note: returncode may be 0 or -1 depending on ollama installation *)
      Printf.printf "[OK] OllamaList execution test passed (returncode=%d)\n%!" result.returncode
    in

    (* Test 6: Execute with invalid Gemini args (missing prompt) *)
    let () =
      let invalid_args = Gemini {
        prompt = "";  (* Empty prompt *)
        model = "gemini-2.5-flash";
        thinking_level = Low;
        yolo = false;
        output_format = Text;
        timeout = 30;
        stream = false;
        use_cli = false;  (* Use direct API for tests *)
        fallback_to_api = false;
      } in
      let result = execute ~sw ~proc_mgr ~clock invalid_args in
      (* Should still execute but will fail due to empty prompt *)
      assert (String.length result.model > 0);
      Printf.printf "[OK] Invalid Gemini args test passed (model=%s)\n%!" result.model
    in

    (* Test 7: Execute with invalid Claude args *)
    let () =
      let invalid_args = Claude {
        prompt = "";  (* Empty prompt *)
        model = "sonnet";
        long_context = false;
        system_prompt = None;
        output_format = Text;
        allowed_tools = [];
        working_directory = "";
        timeout = 30;
        stream = false;
        use_cli = false;  (* Use direct API for tests *)
        fallback_to_api = false;
        api_key = None;
      } in
      let result = execute ~sw ~proc_mgr ~clock invalid_args in
      assert (String.length result.model > 0);
      Printf.printf "[OK] Invalid Claude args test passed (model=%s)\n%!" result.model
    in

    (* Test 8: Parse functions work (re-export test) *)
    let () =
      let gemini_json = `Assoc [
        ("prompt", `String "Hello");
        ("model", `String "gemini-2.5-flash");
        ("thinking_level", `String "low");
      ] in
      match parse_gemini_args gemini_json with
      | Gemini g ->
          assert (g.prompt = "Hello");
          assert (g.model = "gemini-2.5-flash");
          Printf.printf "[OK] parse_gemini_args test passed\n%!"
      | _ ->
          failwith "parse_gemini_args should return Gemini variant"
    in

    (* Test 9: Build command functions (re-export test) *)
    let () =
      let args = Ollama {
        prompt = "Test";
        model = "qwen3-coder:30b";
        system_prompt = None;
        temperature = 0.5;
        timeout = 60;
        stream = false;
        tools = None;
      } in
      match build_ollama_curl_cmd args with
      | Ok cmd_list ->
          assert (List.length cmd_list > 0);
          assert (List.hd cmd_list = "curl");
          Printf.printf "[OK] build_ollama_curl_cmd test passed (args=%d)\n%!" (List.length cmd_list)
      | Error err ->
          failwith ("build_ollama_curl_cmd failed: " ^ err)
    in

    (* Test 10: Exponential backoff calculation *)
    let () =
      let delay1 = exponential_backoff ~base_delay:1.0 0 in
      let delay2 = exponential_backoff ~base_delay:1.0 1 in
      let delay3 = exponential_backoff ~base_delay:1.0 2 in
      assert (delay1 = 1.0);
      assert (delay2 = 2.0);
      assert (delay3 = 4.0);
      Printf.printf "[OK] exponential_backoff test passed\n%!"
    in

    (* Test 11: Convenience wrapper execute_with_env *)
    let () =
      let result = execute_with_env ~sw ~env OllamaList in
      assert (result.model = "ollama_list");
      Printf.printf "[OK] execute_with_env test passed\n%!"
    in

    (* Test 12: Stream delta runtime toggle - get/set/get cycle *)
    let () =
      (* Get initial state *)
      let initial = get_stream_delta () in

      (* Toggle to opposite *)
      let new_val = set_stream_delta (not initial) in
      assert (new_val = not initial);

      (* Verify get returns new value *)
      let after_set = get_stream_delta () in
      assert (after_set = not initial);

      (* Restore original *)
      let _ = set_stream_delta initial in
      let restored = get_stream_delta () in
      assert (restored = initial);

      Printf.printf "[OK] stream_delta runtime toggle test passed\n%!"
    in

    (* Test 13: Stream delta execute - SetStreamDelta returns correct model *)
    let () =
      let result = execute ~sw ~proc_mgr ~clock (SetStreamDelta { enabled = true }) in
      assert (result.model = "set_stream_delta");
      assert (result.returncode = 0);
      assert (List.mem_assoc "enabled" result.extra);
      assert (List.mem_assoc "was" result.extra);
      Printf.printf "[OK] SetStreamDelta execute test passed\n%!"
    in

    (* Test 14: Stream delta execute - GetStreamDelta returns correct model *)
    let () =
      let result = execute ~sw ~proc_mgr ~clock GetStreamDelta in
      assert (result.model = "get_stream_delta");
      assert (result.returncode = 0);
      assert (List.mem_assoc "enabled" result.extra);
      assert (List.mem_assoc "source" result.extra);
      Printf.printf "[OK] GetStreamDelta execute test passed\n%!"
    in

    (* Test 15: chain.run can execute a preset by chain_id (registry lookup) *)
    let () =
      let open Chain_types in
      let chain_id = "chain-run-registry-test" in
      let node : node = {
        id = "echo1";
        node_type = Tool { name = "echo"; args = `Assoc [("input", `String "hi")] };
        input_mapping = [];
        output_key = None;
        depends_on = None;
      } in
      let chain = make_chain ~id:chain_id ~nodes:[node] ~output:"echo1" () in
      Chain_registry.register chain;
      let result =
        execute_with_tracing ~sw ~proc_mgr ~clock
          (ChainRun {
            chain = None;
            mermaid = None;
            chain_id = Some chain_id;
            input = None;
            trace = false;
            checkpoint_enabled = false;
            timeout = Some 30;
          })
      in
      assert (result.model = "chain.run");
      assert (result.returncode = 0);
      assert (result.response = "hi");
      Printf.printf "[OK] chain.run registry chain_id test passed\n%!"
    in

    (* Test 15: Stream delta 'was' value captures previous state correctly *)
    let () =
      (* Set to known state first *)
      let _ = set_stream_delta false in

      (* Now set to true and check 'was' *)
      let result = execute ~sw ~proc_mgr ~clock (SetStreamDelta { enabled = true }) in
      let was_val = List.assoc "was" result.extra in
      assert (was_val = "false");  (* was false before we set to true *)

      (* Set back to false and check 'was' *)
      let result2 = execute ~sw ~proc_mgr ~clock (SetStreamDelta { enabled = false }) in
      let was_val2 = List.assoc "was" result2.extra in
      assert (was_val2 = "true");  (* was true before we set to false *)

      Printf.printf "[OK] stream_delta 'was' value test passed\n%!"
    in

    Printf.printf "\n✅ All 15 tools_eio tests passed!\n%!"

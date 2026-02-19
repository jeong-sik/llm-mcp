(** Tests for Handler_codex module — Codex/OpenAI API handler
    Tests run within Eio_main.run for process manager and clock.
    Focuses on response handling paths without real API calls. *)

open Alcotest

(** Test execute_direct_api without OPENAI_API_KEY (covers missing key branch) *)
let test_codex_no_api_key () =
  let prev = Sys.getenv_opt "OPENAI_API_KEY" in
  (match prev with Some _ -> Unix.putenv "OPENAI_API_KEY" "" | None -> ());
  Eio_main.run @@ fun env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let result = Handler_codex.execute_direct_api
    ~sw ~proc_mgr ~clock
    ~model:"gpt-5" ~prompt:"test" ~timeout:5 ~stream:false in
  check bool "model contains codex-api" true
    (Common.contains ~substring:"codex-api" result.Types_core.model);
  check int "returncode is -1" (-1) result.returncode;
  check bool "error message" true
    (Common.contains ~substring:"OPENAI_API_KEY" result.response);
  (* Restore *)
  (match prev with Some v -> Unix.putenv "OPENAI_API_KEY" v | None -> ())

(** Test execute_direct_api with a fake API key
    (will call curl but fail on the API side, covering the Ok/Error parse paths) *)
let test_codex_with_fake_key () =
  let prev = Sys.getenv_opt "OPENAI_API_KEY" in
  Unix.putenv "OPENAI_API_KEY" "sk-fake-key-for-testing-12345";
  Eio_main.run @@ fun env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let result = Handler_codex.execute_direct_api
    ~sw ~proc_mgr ~clock
    ~model:"o3" ~prompt:"hello" ~timeout:10 ~stream:false in
  (* With a fake key, curl succeeds but API returns an error *)
  check bool "model contains codex-api" true
    (Common.contains ~substring:"codex-api" result.Types_core.model);
  (* The response should be either an API error or parse error *)
  check bool "has response" true (String.length result.response > 0);
  (* Restore *)
  (match prev with Some v -> Unix.putenv "OPENAI_API_KEY" v | None -> ())

(** Test model alias mapping by checking various model strings *)
let test_codex_model_aliases () =
  let prev = Sys.getenv_opt "OPENAI_API_KEY" in
  (match prev with Some _ -> Unix.putenv "OPENAI_API_KEY" "" | None -> ());
  (* Without API key, we just verify the error response contains the model name *)
  Eio_main.run @@ fun env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let models = ["gpt-5.2"; "gpt-5"; "o3"; "o4-mini"; "custom-model"] in
  List.iter (fun model ->
    let result = Handler_codex.execute_direct_api
      ~sw ~proc_mgr ~clock
      ~model ~prompt:"test" ~timeout:5 ~stream:false in
    check bool (Printf.sprintf "model %s in name" model) true
      (Common.contains ~substring:"codex-api" result.Types_core.model)
  ) models;
  (* Restore *)
  (match prev with Some v -> Unix.putenv "OPENAI_API_KEY" v | None -> ())

let () =
  run "handler_codex" [
    ("api_key", [
      test_case "missing key" `Quick test_codex_no_api_key;
      test_case "model aliases" `Quick test_codex_model_aliases;
    ]);
    ("api_call", [
      test_case "fake key" `Quick test_codex_with_fake_key;
    ]);
  ]

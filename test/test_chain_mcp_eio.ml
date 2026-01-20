(** MCP Chain Tool Integration Tests (Eio) *)

open Alcotest

module Server = Mcp_server_eio

let run_eio f =
  Eio_main.run @@ fun env ->
  f env

let get_result_text json =
  let open Yojson.Safe.Util in
  match json |> member "result" |> member "content" |> to_list with
  | first :: _ -> first |> member "text" |> to_string
  | [] -> ""

let get_is_error json =
  let open Yojson.Safe.Util in
  json |> member "result" |> member "isError" |> to_bool

let get_session_id json =
  let open Yojson.Safe.Util in
  json |> member "result" |> member "sessionId" |> to_string

let test_chain_run_and_validate () =
  run_eio @@ fun env ->
  let store = Server.create_session_store () in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let headers = [] in

  Eio.Switch.run @@ fun sw ->
    let init_req = {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|} in
    let (_session_opt, init_resp) =
      Server.handle_request ~sw ~proc_mgr ~clock ~store ~headers init_req
    in
    let session_id = get_session_id init_resp in

    let chain_json = `Assoc [
      ("id", `String "mcp_chain");
      ("nodes", `List [
        `Assoc [
          ("id", `String "a");
          ("type", `String "llm");
          ("model", `String "stub");
          ("prompt", `String "ping");
        ];
      ]);
      ("output", `String "a");
    ] in

    let run_params = `Assoc [
      ("name", `String "chain.run");
      ("arguments", `Assoc [
        ("chain", chain_json);
        ("trace", `Bool false);
        ("timeout", `Int 10);
      ]);
      ("sessionId", `String session_id);
    ] in
    let run_req = `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int 2);
      ("method", `String "tools/call");
      ("params", run_params);
    ] |> Yojson.Safe.to_string in
    let (_session_opt, run_resp) =
      Server.handle_request ~sw ~proc_mgr ~clock ~store ~headers run_req
    in

    check bool "chain.run isError=false" false (get_is_error run_resp);
    check string "chain.run output" "[stub]ping" (get_result_text run_resp);

    let validate_params = `Assoc [
      ("name", `String "chain.validate");
      ("arguments", `Assoc [
        ("chain", chain_json);
      ]);
      ("sessionId", `String session_id);
    ] in
    let validate_req = `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int 3);
      ("method", `String "tools/call");
      ("params", validate_params);
    ] |> Yojson.Safe.to_string in
    let (_session_opt, validate_resp) =
      Server.handle_request ~sw ~proc_mgr ~clock ~store ~headers validate_req
    in

    check bool "chain.validate isError=false" false (get_is_error validate_resp);
    check string "chain.validate output"
      "Chain 'mcp_chain' is valid" (get_result_text validate_resp)

let () =
  run "chain_mcp_eio" [
    ("chain", [
      test_case "chain.run/validate" `Quick test_chain_run_and_validate;
    ]);
  ]

open Alcotest

let test_shutdown_notification_json_escapes_reason () =
  let reason = "SIG\"TERM\nslash\\path" in
  let json = Http_server_eio.shutdown_notification_json reason in
  let parsed = Yojson.Safe.from_string json in
  let open Yojson.Safe.Util in
  check string "jsonrpc" "2.0" (parsed |> member "jsonrpc" |> to_string);
  check string "method" "notifications/shutdown"
    (parsed |> member "method" |> to_string);
  check string "reason preserved" reason
    (parsed |> member "params" |> member "reason" |> to_string)

let () =
  run "Http_server_eio" [
    ("shutdown_notification", [
      test_case "escapes reason" `Quick test_shutdown_notification_json_escapes_reason;
    ]);
  ]

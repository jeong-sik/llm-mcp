(** Tests for Safe_parse module *)

let test_int_default () =
  (* Valid int *)
  let result = Safe_parse.int ~context:"test" ~default:0 "42" in
  Alcotest.(check int) "valid int" 42 result;

  (* Invalid int -> default *)
  let result = Safe_parse.int ~context:"test" ~default:99 "not-a-number" in
  Alcotest.(check int) "invalid uses default" 99 result

let test_int_opt () =
  Alcotest.(check (option int)) "valid" (Some 123) (Safe_parse.int_opt "123");
  Alcotest.(check (option int)) "invalid" None (Safe_parse.int_opt "abc")

let test_float_default () =
  let result = Safe_parse.float ~context:"test" ~default:0.0 "3.14" in
  Alcotest.(check (float 0.01)) "valid float" 3.14 result;

  let result = Safe_parse.float ~context:"test" ~default:1.5 "bad" in
  Alcotest.(check (float 0.01)) "invalid uses default" 1.5 result

let test_bool_default () =
  Alcotest.(check bool) "true" true (Safe_parse.bool ~context:"t" ~default:false "true");
  Alcotest.(check bool) "1" true (Safe_parse.bool ~context:"t" ~default:false "1");
  Alcotest.(check bool) "yes" true (Safe_parse.bool ~context:"t" ~default:false "yes");
  Alcotest.(check bool) "false" false (Safe_parse.bool ~context:"t" ~default:true "false");
  Alcotest.(check bool) "0" false (Safe_parse.bool ~context:"t" ~default:true "0");
  Alcotest.(check bool) "no" false (Safe_parse.bool ~context:"t" ~default:true "no");
  Alcotest.(check bool) "invalid->default" true (Safe_parse.bool ~context:"t" ~default:true "maybe")

let test_json_string () =
  let json = `Assoc [("name", `String "hello")] in
  let result = Safe_parse.json_string ~context:"test" ~default:"default" json "name" in
  Alcotest.(check string) "valid string" "hello" result;

  let result = Safe_parse.json_string ~context:"test" ~default:"fallback" json "missing" in
  Alcotest.(check string) "missing uses default" "fallback" result

let test_json_int () =
  let json = `Assoc [("count", `Int 42)] in
  let result = Safe_parse.json_int ~context:"test" ~default:0 json "count" in
  Alcotest.(check int) "valid int" 42 result;

  let result = Safe_parse.json_int ~context:"test" ~default:99 json "missing" in
  Alcotest.(check int) "missing uses default" 99 result

let test_json_of_string_opt () =
  (* Valid JSON *)
  (match Safe_parse.json_of_string_opt {|{"x":1}|} with
   | Some _ -> ()
   | None -> Alcotest.fail "expected Some for valid json");

  (* Invalid JSON *)
  (match Safe_parse.json_of_string_opt "not json" with
   | None -> ()
   | Some _ -> Alcotest.fail "expected None for invalid json")

let () =
  (* Enable parse warnings for testing *)
  Unix.putenv "LLM_MCP_PARSE_WARN" "1";

  Alcotest.run "Safe_parse" [
    ("int", [
      Alcotest.test_case "int with default" `Quick test_int_default;
      Alcotest.test_case "int_opt" `Quick test_int_opt;
    ]);
    ("float", [
      Alcotest.test_case "float with default" `Quick test_float_default;
    ]);
    ("bool", [
      Alcotest.test_case "bool with default" `Quick test_bool_default;
    ]);
    ("json", [
      Alcotest.test_case "json_string" `Quick test_json_string;
      Alcotest.test_case "json_int" `Quick test_json_int;
      Alcotest.test_case "json_of_string_opt" `Quick test_json_of_string_opt;
    ]);
  ]

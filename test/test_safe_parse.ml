(** Tests for Safe_parse module — comprehensive coverage *)

open Alcotest

(* === Primitive Parsers === *)

let test_int_default () =
  check int "valid" 42 (Safe_parse.int ~context:"t" ~default:0 "42");
  check int "negative" (-7) (Safe_parse.int ~context:"t" ~default:0 "-7");
  check int "invalid" 0 (Safe_parse.int ~context:"t" ~default:0 "abc");
  check int "empty" 0 (Safe_parse.int ~context:"t" ~default:0 "")

let test_int_opt () =
  check (option int) "valid" (Some 42) (Safe_parse.int_opt "42");
  check (option int) "negative" (Some (-3)) (Safe_parse.int_opt "-3");
  check (option int) "invalid" None (Safe_parse.int_opt "abc");
  check (option int) "empty" None (Safe_parse.int_opt "")

let test_float_default () =
  let eps = 0.001 in
  let close a b = Float.abs (a -. b) < eps in
  check bool "valid" true (close 3.14 (Safe_parse.float ~context:"t" ~default:0.0 "3.14"));
  check bool "int-like" true (close 42.0 (Safe_parse.float ~context:"t" ~default:0.0 "42"));
  check bool "invalid" true (close 0.0 (Safe_parse.float ~context:"t" ~default:0.0 "abc"))

let test_float_opt () =
  let eps = 0.001 in
  let close a b = Float.abs (a -. b) < eps in
  (match Safe_parse.float_opt "3.14" with
   | Some f -> check bool "valid float" true (close f 3.14)
   | None -> fail "expected Some for valid float");
  (match Safe_parse.float_opt "42" with
   | Some f -> check bool "int-like" true (close f 42.0)
   | None -> fail "expected Some for int-like");
  check (option (float eps)) "invalid" None (Safe_parse.float_opt "abc");
  check (option (float eps)) "empty" None (Safe_parse.float_opt "")

let test_bool_default () =
  check bool "true" true (Safe_parse.bool ~context:"t" ~default:false "true");
  check bool "false" false (Safe_parse.bool ~context:"t" ~default:true "false");
  check bool "1" true (Safe_parse.bool ~context:"t" ~default:false "1");
  check bool "0" false (Safe_parse.bool ~context:"t" ~default:true "0");
  check bool "yes" true (Safe_parse.bool ~context:"t" ~default:false "yes");
  check bool "no" false (Safe_parse.bool ~context:"t" ~default:true "no");
  check bool "YES uppercase" true (Safe_parse.bool ~context:"t" ~default:false "YES");
  check bool "FALSE uppercase" false (Safe_parse.bool ~context:"t" ~default:true "FALSE");
  check bool "invalid->default" false (Safe_parse.bool ~context:"t" ~default:false "xyz");
  check bool "invalid->default true" true (Safe_parse.bool ~context:"t" ~default:true "maybe")

(* === Environment Variable Parsers === *)

let test_env_int () =
  Unix.putenv "TEST_SP_INT" "42";
  check int "from env" 42 (Safe_parse.env_int ~var:"TEST_SP_INT" ~default:0);
  Unix.putenv "TEST_SP_INT" "not_int";
  check int "invalid env" 0 (Safe_parse.env_int ~var:"TEST_SP_INT" ~default:0);
  check int "missing env" 99 (Safe_parse.env_int ~var:"TEST_SP_INT_NONEXISTENT" ~default:99)

let test_env_float () =
  let eps = 0.001 in
  let close a b = Float.abs (a -. b) < eps in
  Unix.putenv "TEST_SP_FLOAT" "2.718";
  check bool "from env" true (close 2.718 (Safe_parse.env_float ~var:"TEST_SP_FLOAT" ~default:0.0));
  Unix.putenv "TEST_SP_FLOAT" "bad";
  check bool "invalid env" true (close 1.0 (Safe_parse.env_float ~var:"TEST_SP_FLOAT" ~default:1.0));
  check bool "missing env" true (close 3.0 (Safe_parse.env_float ~var:"TEST_SP_FLOAT_NONE" ~default:3.0))

let test_env_bool () =
  Unix.putenv "TEST_SP_BOOL" "true";
  check bool "true env" true (Safe_parse.env_bool ~var:"TEST_SP_BOOL" ~default:false);
  Unix.putenv "TEST_SP_BOOL" "0";
  check bool "0 env" false (Safe_parse.env_bool ~var:"TEST_SP_BOOL" ~default:true);
  Unix.putenv "TEST_SP_BOOL" "garbage";
  check bool "invalid env" false (Safe_parse.env_bool ~var:"TEST_SP_BOOL" ~default:false);
  check bool "missing env" true (Safe_parse.env_bool ~var:"TEST_SP_BOOL_NONE" ~default:true)

(* === JSON Parsers === *)

let test_json_string () =
  let json = `Assoc [("name", `String "hello")] in
  check string "found" "hello" (Safe_parse.json_string ~context:"t" ~default:"" json "name");
  check string "missing" "" (Safe_parse.json_string ~context:"t" ~default:"" json "nope");
  let json2 = `Assoc [("num", `Int 42)] in
  check string "type mismatch" "" (Safe_parse.json_string ~context:"t" ~default:"" json2 "num")

let test_json_string_opt () =
  let json = `Assoc [("name", `String "hi")] in
  check (option string) "found" (Some "hi") (Safe_parse.json_string_opt json "name");
  check (option string) "missing" None (Safe_parse.json_string_opt json "nope");
  let json2 = `Assoc [("n", `Int 1)] in
  check (option string) "type mismatch" None (Safe_parse.json_string_opt json2 "n")

let test_json_int () =
  let json = `Assoc [("count", `Int 10)] in
  check int "found" 10 (Safe_parse.json_int ~context:"t" ~default:0 json "count");
  check int "missing" 0 (Safe_parse.json_int ~context:"t" ~default:0 json "nope")

let test_json_int_opt () =
  let json = `Assoc [("count", `Int 5)] in
  check (option int) "found" (Some 5) (Safe_parse.json_int_opt json "count");
  check (option int) "missing" None (Safe_parse.json_int_opt json "nope");
  let json2 = `Assoc [("s", `String "x")] in
  check (option int) "type mismatch" None (Safe_parse.json_int_opt json2 "s")

let test_json_float () =
  let eps = 0.001 in
  let close a b = Float.abs (a -. b) < eps in
  let json = `Assoc [("rate", `Float 0.95)] in
  check bool "found" true (close 0.95 (Safe_parse.json_float ~context:"t" ~default:0.0 json "rate"));
  check bool "missing" true (close 1.0 (Safe_parse.json_float ~context:"t" ~default:1.0 json "nope"));
  let json2 = `Assoc [("s", `String "x")] in
  check bool "type mismatch" true (close 2.0 (Safe_parse.json_float ~context:"t" ~default:2.0 json2 "s"))

let test_json_bool () =
  let json = `Assoc [("flag", `Bool true)] in
  check bool "found true" true (Safe_parse.json_bool ~context:"t" ~default:false json "flag");
  check bool "missing" false (Safe_parse.json_bool ~context:"t" ~default:false json "nope");
  let json2 = `Assoc [("n", `Int 1)] in
  check bool "type mismatch" true (Safe_parse.json_bool ~context:"t" ~default:true json2 "n")

let test_json_list () =
  let json = `Assoc [("items", `List [`Int 1; `Int 2])] in
  check int "found list len" 2 (List.length (Safe_parse.json_list ~context:"t" json "items"));
  check int "missing -> empty" 0 (List.length (Safe_parse.json_list ~context:"t" json "nope"));
  let json2 = `Assoc [("s", `String "x")] in
  check int "type mismatch -> empty" 0 (List.length (Safe_parse.json_list ~context:"t" json2 "s"))

(* === JSON Parsing === *)

let test_json_of_string_opt () =
  check bool "valid" true (Option.is_some (Safe_parse.json_of_string_opt "{}"));
  check bool "valid array" true (Option.is_some (Safe_parse.json_of_string_opt "[1,2]"));
  check bool "invalid" true (Option.is_none (Safe_parse.json_of_string_opt "not json"))

let test_json_of_string_with_default () =
  let default = `Assoc [] in
  let result = Safe_parse.json_of_string ~context:"t" ~default {|{"a":1}|} in
  (match result with
   | `Assoc [("a", `Int 1)] -> ()
   | _ -> fail "expected parsed JSON");
  let result2 = Safe_parse.json_of_string ~context:"t" ~default "not json" in
  check bool "invalid -> default" true (result2 = default)

(* === Exception-Safe Execution === *)

let test_try_or () =
  check int "success" 42
    (Safe_parse.try_or ~context:"t" ~fallback:(fun () -> 0) (fun () -> 42));
  check int "failure" 0
    (Safe_parse.try_or ~context:"t" ~fallback:(fun () -> 0) (fun () -> failwith "boom"));
  check string "different exn" "default"
    (Safe_parse.try_or ~context:"t" ~fallback:(fun () -> "default")
       (fun () -> raise Not_found))

let test_try_opt () =
  check (option int) "success" (Some 42)
    (Safe_parse.try_opt ~context:"t" (fun () -> 42));
  check (option int) "failure" None
    (Safe_parse.try_opt ~context:"t" (fun () -> failwith "boom"));
  check (option string) "different exn" None
    (Safe_parse.try_opt ~context:"t" (fun () -> raise Not_found))

let () =
  (* Enable parse warnings for coverage of warn path *)
  Unix.putenv "LLM_MCP_PARSE_WARN" "1";

  run "Safe_parse" [
    ("int", [
      test_case "default" `Quick test_int_default;
      test_case "opt" `Quick test_int_opt;
    ]);
    ("float", [
      test_case "default" `Quick test_float_default;
      test_case "opt" `Quick test_float_opt;
    ]);
    ("bool", [
      test_case "default" `Quick test_bool_default;
    ]);
    ("env", [
      test_case "int" `Quick test_env_int;
      test_case "float" `Quick test_env_float;
      test_case "bool" `Quick test_env_bool;
    ]);
    ("json_string", [
      test_case "default" `Quick test_json_string;
      test_case "opt" `Quick test_json_string_opt;
    ]);
    ("json_int", [
      test_case "default" `Quick test_json_int;
      test_case "opt" `Quick test_json_int_opt;
    ]);
    ("json_float", [
      test_case "default" `Quick test_json_float;
    ]);
    ("json_bool", [
      test_case "default" `Quick test_json_bool;
    ]);
    ("json_list", [
      test_case "default" `Quick test_json_list;
    ]);
    ("json_of_string", [
      test_case "opt" `Quick test_json_of_string_opt;
      test_case "with_default" `Quick test_json_of_string_with_default;
    ]);
    ("try", [
      test_case "try_or" `Quick test_try_or;
      test_case "try_opt" `Quick test_try_opt;
    ]);
  ]

(** Tests for Chain_category module

    Category theory abstractions tests:
    - Result_monad: functor, applicative, monad operations
    - Result_kleisli: arrow combinators
    - Verdict_monoid: verdict combining with fail-fast
    - Confidence_monoid: statistical aggregations
    - Trace_monoid: trace accumulation
    - Token_monoid: token usage summation
    - Function_profunctor: input/output transformations
    - Utility functions: compose, curry, flip, const
    - Laws verification: monad laws, monoid laws
*)

open Alcotest
open Chain_category

(** {1 Result_monad Tests} *)

let test_result_pure () =
  check (result int string) "pure lifts" (Ok 42) (Result_monad.pure 42)

let test_result_map () =
  let r = Result_monad.map (fun x -> x * 2) (Ok 21) in
  check (result int string) "map Ok" (Ok 42) r;
  let e = Result_monad.map (fun x -> x * 2) (Error "fail") in
  check (result int string) "map Error" (Error "fail") e

let test_result_bind () =
  let double x = Ok (x * 2) in
  let r = Result_monad.bind (Ok 21) double in
  check (result int string) "bind Ok" (Ok 42) r;
  let e = Result_monad.bind (Error "fail") double in
  check (result int string) "bind Error" (Error "fail") e

let test_result_join () =
  let nested = Ok (Ok 42) in
  check (result int string) "join flattens" (Ok 42) (Result_monad.join nested)

let test_result_ap () =
  let f = Ok (fun x -> x + 1) in
  let x = Ok 41 in
  check (result int string) "ap Ok" (Ok 42) (Result_monad.ap f x);
  let ef = Error "no func" in
  check (result int string) "ap Error func" (Error "no func") (Result_monad.ap ef x)

let test_result_map2 () =
  let r = Result_monad.map2 ( + ) (Ok 20) (Ok 22) in
  check (result int string) "map2 Ok" (Ok 42) r;
  let e = Result_monad.map2 ( + ) (Ok 20) (Error "fail") in
  check (result int string) "map2 Error" (Error "fail") e

let test_result_sequence () =
  let xs = [Ok 1; Ok 2; Ok 3] in
  check (result (list int) string) "sequence all Ok" (Ok [1;2;3]) (Result_monad.sequence xs);
  let ys = [Ok 1; Error "fail"; Ok 3] in
  check (result (list int) string) "sequence with Error" (Error "fail") (Result_monad.sequence ys)

let test_result_kleisli () =
  let f x = Ok (x + 10) in
  let g x = Ok (x * 2) in
  let fg = Result_monad.( >=> ) f g in
  check (result int string) "kleisli compose" (Ok 42) (fg 11)

let test_result_run () =
  check int "run Ok" 42 (Result_monad.run (Ok 42));
  check_raises "run Error" (Failure "oops") (fun () -> ignore (Result_monad.run (Error "oops")))

let test_result_catch () =
  let ok = Result_monad.catch (fun () -> 42) in
  check (result int string) "catch success" (Ok 42) ok;
  let err = Result_monad.catch (fun () -> failwith "boom") in
  match err with
  | Error e -> check bool "catch failure" true (String.length e > 0)
  | Ok _ -> fail "expected Error"

let test_result_map_error () =
  let r = Result_monad.map_error String.uppercase_ascii (Error "fail") in
  check (result int string) "map_error" (Error "FAIL") r;
  let ok = Result_monad.map_error String.uppercase_ascii (Ok 42) in
  check (result int string) "map_error Ok unchanged" (Ok 42) ok

(** {1 Result_kleisli Tests} *)

let test_kleisli_arr () =
  let f = Result_kleisli.arr (fun x -> x * 2) in
  check (result int string) "arr" (Ok 84) (f 42)

let test_kleisli_compose () =
  let f = Result_kleisli.arr (fun x -> x + 1) in
  let g = Result_kleisli.arr (fun x -> x * 2) in
  let fg = Result_kleisli.( >>> ) f g in
  check (result int string) ">>> compose" (Ok 86) (fg 42)

let test_kleisli_fanout () =
  let f = Result_kleisli.arr (fun x -> x + 1) in
  let g = Result_kleisli.arr (fun x -> x - 1) in
  let fg = Result_kleisli.( &&& ) f g in
  check (result (pair int int) string) "&&& fanout" (Ok (43, 41)) (fg 42)

let test_kleisli_parallel () =
  let f = Result_kleisli.arr (fun x -> x * 2) in
  let g = Result_kleisli.arr (fun x -> x + 10) in
  let fg = Result_kleisli.( *** ) f g in
  check (result (pair int int) string) "*** parallel" (Ok (84, 52)) (fg (42, 42))

let test_kleisli_first () =
  let f = Result_kleisli.arr (fun x -> x * 2) in
  let ff = Result_kleisli.first f in
  check (result (pair int string) string) "first" (Ok (84, "keep")) (ff (42, "keep"))

let test_kleisli_second () =
  let f = Result_kleisli.arr (fun x -> x * 2) in
  let sf = Result_kleisli.second f in
  check (result (pair string int) string) "second" (Ok ("keep", 84)) (sf ("keep", 42))

let test_kleisli_from_option () =
  let f = Result_kleisli.from_option ~error:"not found" (fun x -> if x > 0 then Some x else None) in
  check (result int string) "from_option Some" (Ok 42) (f 42);
  check (result int string) "from_option None" (Error "not found") (f (-1))

let test_kleisli_guard () =
  let positive = Result_kleisli.guard ~error:"not positive" (fun x -> x > 0) in
  check (result int string) "guard pass" (Ok 42) (positive 42);
  check (result int string) "guard fail" (Error "not positive") (positive (-1))

(** {1 Verdict_monoid Tests} *)

let verdict_testable = testable
  (fun ppf v ->
    let s = match v with
      | Pass s -> "Pass:" ^ s
      | Warn s -> "Warn:" ^ s
      | Fail s -> "Fail:" ^ s
      | Defer s -> "Defer:" ^ s
    in
    Fmt.pf ppf "%s" s)
  (fun a b -> a = b)

let test_verdict_empty () =
  match Verdict_monoid.empty with
  | Pass _ -> ()
  | _ -> fail "empty should be Pass"

let test_verdict_fail_first () =
  let r = Verdict_monoid.concat (Pass "ok") (Fail "bad") in
  check verdict_testable "fail dominates" (Fail "bad") r;
  let r2 = Verdict_monoid.concat (Fail "first") (Pass "ok") in
  check verdict_testable "fail first" (Fail "first") r2

let test_verdict_warn_combine () =
  let r = Verdict_monoid.concat (Warn "w1") (Warn "w2") in
  match r with
  | Warn combined -> check bool "warns combined" true (String.length combined > 3)
  | _ -> fail "expected Warn"

let test_verdict_pass_combine () =
  let r = Verdict_monoid.concat (Pass "a") (Pass "b") in
  match r with
  | Pass combined -> check bool "passes combined" true (String.length combined > 3)
  | _ -> fail "expected Pass"

let test_verdict_concat_all () =
  let vs = [Pass "a"; Warn "w"; Pass "b"] in
  let r = Verdict_monoid.concat_all vs in
  match r with
  | Warn _ -> ()
  | _ -> fail "expected Warn in concat_all"

(** {1 Confidence_monoid Tests} *)

let test_confidence_empty () =
  check (float 0.001) "empty is 1.0" 1.0 Confidence_monoid.empty

let test_confidence_concat () =
  let r = Confidence_monoid.concat 0.6 0.8 in
  check (float 0.001) "concat average" 0.7 r

let test_confidence_concat_all () =
  let xs = [0.5; 0.7; 0.9] in
  let r = Confidence_monoid.concat_all xs in
  check (float 0.001) "concat_all average" 0.7 r

let test_confidence_geometric () =
  let xs = [1.0; 1.0; 1.0] in
  let r = Confidence_monoid.geometric xs in
  check (float 0.001) "geometric all 1s" 1.0 r;
  let ys = [0.5; 0.5] in
  let r2 = Confidence_monoid.geometric ys in
  check (float 0.001) "geometric 0.5s" 0.5 r2

let test_confidence_harmonic () =
  let xs = [1.0; 1.0] in
  let r = Confidence_monoid.harmonic xs in
  check (float 0.001) "harmonic all 1s" 1.0 r

let test_confidence_weighted () =
  let weights = [1.0; 1.0] in
  let values = [0.6; 0.8] in
  let r = Confidence_monoid.weighted weights values in
  check (float 0.001) "weighted equal" 0.7 r;
  let weights2 = [2.0; 1.0] in
  let r2 = Confidence_monoid.weighted weights2 values in
  (* (2*0.6 + 1*0.8) / 3 = 2.0/3 ≈ 0.667 *)
  check (float 0.01) "weighted unequal" 0.667 r2

(** {1 Trace_monoid Tests} *)

let test_trace_empty () =
  check (list (pair string (float 0.001))) "empty trace" [] Trace_monoid.empty

let test_trace_concat () =
  let a = [("step1", 0.1)] in
  let b = [("step2", 0.2)] in
  let r = Trace_monoid.concat a b in
  check int "concat length" 2 (List.length r)

let test_trace_concat_all () =
  let xs = [[("a", 0.1)]; [("b", 0.2)]; [("c", 0.3)]] in
  let r = Trace_monoid.concat_all xs in
  check int "concat_all length" 3 (List.length r)

(** {1 Token_monoid Tests} *)

let test_token_empty () =
  let e = Token_monoid.empty in
  check int "empty total" 0 e.total_tokens

let test_token_concat () =
  let a : token_usage = { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 0.01 } in
  let b : token_usage = { prompt_tokens = 200; completion_tokens = 100; total_tokens = 300; estimated_cost_usd = 0.02 } in
  let r = Token_monoid.concat a b in
  check int "concat prompt" 300 r.prompt_tokens;
  check int "concat completion" 150 r.completion_tokens;
  check int "concat total" 450 r.total_tokens;
  check (float 0.001) "concat cost" 0.03 r.estimated_cost_usd

let test_token_concat_all () =
  let xs = [
    { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 0.01 };
    { prompt_tokens = 200; completion_tokens = 100; total_tokens = 300; estimated_cost_usd = 0.02 };
  ] in
  let r = Token_monoid.concat_all xs in
  check int "concat_all total" 450 r.total_tokens

(** {1 Function_profunctor Tests} *)

let test_profunctor_dimap () =
  let inc x = x + 1 in
  let double x = x * 2 in
  let f x = x + 10 in
  (* dimap inc double f = fun x -> double (f (inc x)) *)
  let df = Function_profunctor.dimap inc double f in
  (* df 5 = double (f (inc 5)) = double (f 6) = double 16 = 32 *)
  check int "dimap" 32 (df 5)

let test_profunctor_lmap () =
  let inc x = x + 1 in
  let f x = x * 2 in
  let lf = Function_profunctor.lmap inc f in
  (* lmap inc f = fun x -> f (inc x) = (x+1)*2 *)
  check int "lmap" 12 (lf 5)

let test_profunctor_rmap () =
  let double x = x * 2 in
  let f x = x + 10 in
  let rf = Function_profunctor.rmap double f in
  (* rmap double f = fun x -> double (f x) = (x+10)*2 *)
  check int "rmap" 30 (rf 5)

(** {1 Utility Functions Tests} *)

let test_identity () =
  check int "identity int" 42 (identity 42);
  check string "identity string" "hello" (identity "hello")

let test_compose () =
  let f x = x * 2 in
  let g x = x + 1 in
  let fg = compose f g in
  (* (f . g) x = f (g x) = (x + 1) * 2 *)
  check int "compose" 12 (fg 5)

let test_compose_infix () =
  let f x = x * 2 in
  let g x = x + 1 in
  let fg = f << g in
  check int "<< infix" 12 (fg 5)

let test_pipe_infix () =
  let f x = x + 1 in
  let g x = x * 2 in
  let fg = f >> g in
  (* (f >> g) x = g (f x) = (x + 1) * 2 *)
  check int ">> pipe" 12 (fg 5)

let test_flip () =
  let sub a b = a - b in
  let flipped = flip sub in
  (* flip sub x y = sub y x *)
  check int "flip" 3 (flipped 2 5)

let test_const () =
  let always42_str = const 42 in
  check int "const ignores string arg" 42 (always42_str "anything");
  let always42_int = const 42 in
  check int "const ignores int arg" 42 (always42_int 999)

let test_curry () =
  let add_pair (x, y) = x + y in
  let curried = curry add_pair in
  check int "curry" 7 (curried 3 4)

let test_uncurry () =
  let add x y = x + y in
  let uncurried = uncurry add in
  check int "uncurry" 7 (uncurried (3, 4))

(** {1 Laws Verification Tests} *)

let result_equal a b = a = b

module ResultMonadLaws = Laws.Monad(Result_monad)
module TokenMonoidLaws = Laws.Monoid(Token_monoid)

let test_monad_left_identity () =
  let x = 21 in
  let f x = Ok (x * 2) in
  let law = ResultMonadLaws.left_identity_law x f result_equal in
  check bool "left identity" true law

let test_monad_right_identity () =
  let m = Ok 42 in
  let law = ResultMonadLaws.right_identity_law m result_equal in
  check bool "right identity" true law

let test_monad_associativity () =
  let m = Ok 10 in
  let f x = Ok (x + 1) in
  let g x = Ok (x * 2) in
  let law = ResultMonadLaws.associativity_law m f g result_equal in
  check bool "associativity" true law

let test_monoid_left_identity () =
  let x : token_usage = { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 0.01 } in
  let law = TokenMonoidLaws.left_identity_law x in
  check bool "monoid left identity" true law

let test_monoid_right_identity () =
  let x : token_usage = { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 0.01 } in
  let law = TokenMonoidLaws.right_identity_law x in
  check bool "monoid right identity" true law

let test_monoid_associativity () =
  (* Use exact integer-representable floats to avoid precision issues *)
  let a : token_usage = { prompt_tokens = 100; completion_tokens = 50; total_tokens = 150; estimated_cost_usd = 1.0 } in
  let b : token_usage = { prompt_tokens = 200; completion_tokens = 100; total_tokens = 300; estimated_cost_usd = 2.0 } in
  let c : token_usage = { prompt_tokens = 50; completion_tokens = 25; total_tokens = 75; estimated_cost_usd = 3.0 } in
  let law = TokenMonoidLaws.associativity_law a b c in
  check bool "monoid associativity" true law

(** {1 Test Suite} *)

let result_monad_tests = [
  test_case "pure" `Quick test_result_pure;
  test_case "map" `Quick test_result_map;
  test_case "bind" `Quick test_result_bind;
  test_case "join" `Quick test_result_join;
  test_case "ap" `Quick test_result_ap;
  test_case "map2" `Quick test_result_map2;
  test_case "sequence" `Quick test_result_sequence;
  test_case "kleisli" `Quick test_result_kleisli;
  test_case "run" `Quick test_result_run;
  test_case "catch" `Quick test_result_catch;
  test_case "map_error" `Quick test_result_map_error;
]

let result_kleisli_tests = [
  test_case "arr" `Quick test_kleisli_arr;
  test_case ">>>" `Quick test_kleisli_compose;
  test_case "&&&" `Quick test_kleisli_fanout;
  test_case "***" `Quick test_kleisli_parallel;
  test_case "first" `Quick test_kleisli_first;
  test_case "second" `Quick test_kleisli_second;
  test_case "from_option" `Quick test_kleisli_from_option;
  test_case "guard" `Quick test_kleisli_guard;
]

let verdict_tests = [
  test_case "empty" `Quick test_verdict_empty;
  test_case "fail first" `Quick test_verdict_fail_first;
  test_case "warn combine" `Quick test_verdict_warn_combine;
  test_case "pass combine" `Quick test_verdict_pass_combine;
  test_case "concat_all" `Quick test_verdict_concat_all;
]

let confidence_tests = [
  test_case "empty" `Quick test_confidence_empty;
  test_case "concat" `Quick test_confidence_concat;
  test_case "concat_all" `Quick test_confidence_concat_all;
  test_case "geometric" `Quick test_confidence_geometric;
  test_case "harmonic" `Quick test_confidence_harmonic;
  test_case "weighted" `Quick test_confidence_weighted;
]

let trace_tests = [
  test_case "empty" `Quick test_trace_empty;
  test_case "concat" `Quick test_trace_concat;
  test_case "concat_all" `Quick test_trace_concat_all;
]

let token_tests = [
  test_case "empty" `Quick test_token_empty;
  test_case "concat" `Quick test_token_concat;
  test_case "concat_all" `Quick test_token_concat_all;
]

let profunctor_tests = [
  test_case "dimap" `Quick test_profunctor_dimap;
  test_case "lmap" `Quick test_profunctor_lmap;
  test_case "rmap" `Quick test_profunctor_rmap;
]

let utility_tests = [
  test_case "identity" `Quick test_identity;
  test_case "compose" `Quick test_compose;
  test_case "<<" `Quick test_compose_infix;
  test_case ">>" `Quick test_pipe_infix;
  test_case "flip" `Quick test_flip;
  test_case "const" `Quick test_const;
  test_case "curry" `Quick test_curry;
  test_case "uncurry" `Quick test_uncurry;
]

let laws_tests = [
  test_case "monad left identity" `Quick test_monad_left_identity;
  test_case "monad right identity" `Quick test_monad_right_identity;
  test_case "monad associativity" `Quick test_monad_associativity;
  test_case "monoid left identity" `Quick test_monoid_left_identity;
  test_case "monoid right identity" `Quick test_monoid_right_identity;
  test_case "monoid associativity" `Quick test_monoid_associativity;
]

let () =
  run "chain_category" [
    ("Result_monad", result_monad_tests);
    ("Result_kleisli", result_kleisli_tests);
    ("Verdict_monoid", verdict_tests);
    ("Confidence_monoid", confidence_tests);
    ("Trace_monoid", trace_tests);
    ("Token_monoid", token_tests);
    ("Function_profunctor", profunctor_tests);
    ("Utility functions", utility_tests);
    ("Laws", laws_tests);
  ]

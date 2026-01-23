(** Test Spec DSL - SSOT-Driven Validation *)

open Agent_core_eio.Spec_dsl

let () = Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun _sw ->

    (* ===== Test 1: Basic SPEC definition ===== *)
    let () =
      (* Use simple int state for easier testing *)
      let module Counter_spec : SPEC with type input = int
                                      and type output = int
                                      and type state = int = struct
        type input = int
        type output = int
        type state = int  (* value itself *)

        let precondition s i = i > 0 && s + i <= 100
        let postcondition s i o = o = s + i
        let invariant s = s >= 0 && s <= 100
        let transition s i = s + i
      end in

      (* Test precondition *)
      let state = 50 in
      assert (Counter_spec.precondition state 10 = true);
      assert (Counter_spec.precondition state 60 = false);  (* would exceed 100 *)
      assert (Counter_spec.precondition state (-5) = false);  (* negative *)

      (* Test postcondition *)
      assert (Counter_spec.postcondition state 10 60 = true);
      assert (Counter_spec.postcondition state 10 50 = false);

      (* Test invariant *)
      assert (Counter_spec.invariant state = true);
      assert (Counter_spec.invariant (-1) = false);
      assert (Counter_spec.invariant 101 = false);

      (* Test transition *)
      let new_state = Counter_spec.transition state 10 in
      assert (new_state = 60);

      Printf.printf "[OK] Basic SPEC definition\n%!"
    in

    (* ===== Test 2: IMPL with Make_verified ===== *)
    let () =
      let module Counter_spec : SPEC with type input = int
                                      and type output = int
                                      and type state = int = struct
        type input = int
        type output = int
        type state = int

        let precondition s i = i > 0 && s + i <= 100
        let postcondition s i o = o = s + i
        let invariant s = s >= 0 && s <= 100
        let transition s i = s + i
      end in

      let module Counter_impl : IMPL with type input = int
                                      and type output = int
                                      and type state = int = struct
        include Counter_spec
        let run s i = (s + i, s + i)
      end in

      let module Verified = Make_verified(Counter_spec)(Counter_impl) in

      (* Valid operation *)
      let result1 = Verified.run_verified 50 10 in
      assert (match result1 with Verified (60, 60) -> true | _ -> false);

      (* Precondition violation *)
      let result2 = Verified.run_verified 50 (-5) in
      assert (match result2 with Violation (Precondition_violated _) -> true | _ -> false);

      (* Invariant violation (state > 100) *)
      let result3 = Verified.run_verified 50 60 in
      assert (match result3 with Violation (Precondition_violated _) -> true | _ -> false);

      Printf.printf "[OK] Make_verified wrapper\n%!"
    in

    (* ===== Test 3: run_verified_exn ===== *)
    let () =
      let module Simple_spec : SPEC with type input = int
                                     and type output = int
                                     and type state = int = struct
        type input = int
        type output = int
        type state = int

        let precondition _ i = i > 0
        let postcondition _ i o = o = i * 2
        let invariant _ = true
        let transition s _ = s
      end in

      let module Simple_impl : IMPL with type input = int
                                     and type output = int
                                     and type state = int = struct
        include Simple_spec
        let run s i = (i * 2, s)
      end in

      let module Verified = Make_verified(Simple_spec)(Simple_impl) in

      (* Should succeed *)
      let (output, _) = Verified.run_verified_exn 0 5 in
      assert (output = 10);

      (* Should raise *)
      let raised = try
        let _ = Verified.run_verified_exn 0 (-1) in
        false
      with Failure _ -> true
      in
      assert raised;

      Printf.printf "[OK] run_verified_exn\n%!"
    in

    (* ===== Test 4: to_validator conversion ===== *)
    let () =
      let module Double_spec : SPEC with type input = int
                                     and type output = int
                                     and type state = unit = struct
        type input = int
        type output = int
        type state = unit

        let precondition _ i = i >= 0
        let postcondition _ i o = o = i * 2
        let invariant _ = true
        let transition s _ = s
      end in

      let module Double_impl : IMPL with type input = int
                                     and type output = int
                                     and type state = unit = struct
        include Double_spec
        let run () i = (i * 2, ())
      end in

      let module Verified = Make_verified(Double_spec)(Double_impl) in

      let validator = Verified.to_validator ~name:"double_check" in
      let module V = (val validator) in

      (* Valid input *)
      let r1 = V.validate ((), 5) in
      assert (match r1.verdict with Agent_core_eio.Validator.Pass _ -> true | _ -> false);
      assert (r1.context = Some 10);

      (* Invalid input (negative) *)
      let r2 = V.validate ((), -1) in
      assert (match r2.verdict with Agent_core_eio.Validator.Fail _ -> true | _ -> false);
      assert (r2.context = None);

      Printf.printf "[OK] to_validator conversion\n%!"
    in

    (* ===== Test 5: Versioned spec ===== *)
    let () =
      let module V1_spec : VERSIONED_SPEC with type input = int
                                           and type output = int
                                           and type state = int = struct
        type input = int
        type output = int
        type state = int

        let version = 1
        let changelog = ["Initial version"]

        let precondition _ i = i > 0
        let postcondition _ _ _ = true
        let invariant s = s >= 0
        let transition s i = s + i
      end in

      assert (V1_spec.version = 1);
      assert (List.length V1_spec.changelog = 1);

      Printf.printf "[OK] Versioned spec\n%!"
    in

    (* ===== Test 6: Refinement types ===== *)
    let () =
      let module Percent = Bounded_int(struct let min = 0 let max = 100 end) in

      assert (Percent.refine 50 <> None);
      assert (Percent.refine (-1) = None);
      assert (Percent.refine 101 = None);

      let valid = match Percent.refine 75 with
        | Some v -> Percent.unrefine v = 75
        | None -> false
      in
      assert valid;

      Printf.printf "[OK] Bounded_int refinement\n%!"
    in

    (* ===== Test 7: Non-empty string refinement ===== *)
    let () =
      assert (Non_empty_string.refine "hello" <> None);
      assert (Non_empty_string.refine "" = None);

      let valid = match Non_empty_string.refine "test" with
        | Some s -> Non_empty_string.unrefine s = "test"
        | None -> false
      in
      assert valid;

      Printf.printf "[OK] Non_empty_string refinement\n%!"
    in

    (* ===== Test 8: Spec registry ===== *)
    let () =
      Spec_registry.register ~name:"counter" ~version:1 ();
      Spec_registry.register ~name:"counter" ~version:2 ();
      Spec_registry.register ~name:"timer" ~version:1 ();

      assert (Spec_registry.find "counter" <> None);
      assert (Spec_registry.find "nonexistent" = None);

      let entry = Spec_registry.find "counter" in
      assert (match entry with Some e -> e.version = 2 | None -> false);

      let all = Spec_registry.list_all () in
      assert (List.length all >= 2);

      Printf.printf "[OK] Spec registry\n%!"
    in

    (* ===== Test 9: Contract documentation ===== *)
    let () =
      let doc : contract_doc = {
        name = "increment";
        description = "Increments the counter";
        preconditions = ["n > 0"; "value + n <= 100"];
        postconditions = ["result = value + n"];
        invariants = ["0 <= value <= 100"];
        examples = [("5", "55"); ("10", "60")];
      } in

      let tests = doc_to_test_cases doc in
      assert (List.length tests = 2);

      Printf.printf "[OK] Contract documentation\n%!"
    in

    Printf.printf "\n✅ All 9 spec DSL tests passed!\n%!"

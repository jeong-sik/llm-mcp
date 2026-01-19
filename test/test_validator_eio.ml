(** Test Validator_eio - Composable Meta-Validator *)

open Agent_core_eio.Validator

let () = Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in

    (* Test 1: Basic verdict types *)
    let () =
      let v1 = Pass "test passed" in
      let v2 = Warn "warning" in
      let v3 = Fail "failed" in
      let v4 = Defer "deferred" in
      assert (match v1 with Pass _ -> true | _ -> false);
      assert (match v2 with Warn _ -> true | _ -> false);
      assert (match v3 with Fail _ -> true | _ -> false);
      assert (match v4 with Defer _ -> true | _ -> false);
      Printf.printf "[OK] verdict types\n%!"
    in

    (* Test 2: Result type *)
    let () =
      let r = {
        verdict = Pass "ok";
        confidence = 0.9;
        context = ();
        children = [];
        metadata = [("key", "value")];
      } in
      assert (r.confidence = 0.9);
      assert (List.length r.metadata = 1);
      Printf.printf "[OK] result type\n%!"
    in

    (* Test 3: Simple validator module *)
    let () =
      let module Simple_validator : VALIDATOR
        with type state = int and type context = string = struct
        type state = int
        type context = string
        let name = "simple"
        let validate n =
          if n > 0 then
            { verdict = Pass "positive";
              confidence = 1.0;
              context = "ok";
              children = [];
              metadata = []; }
          else
            { verdict = Fail "not positive";
              confidence = 0.0;
              context = "fail";
              children = [];
              metadata = []; }
      end in
      let r1 = Simple_validator.validate 5 in
      let r2 = Simple_validator.validate 0 in
      assert (match r1.verdict with Pass _ -> true | _ -> false);
      assert (match r2.verdict with Fail _ -> true | _ -> false);
      Printf.printf "[OK] simple validator\n%!"
    in

    (* Test 4: Compose.sequence *)
    let () =
      let module V1 : VALIDATOR with type state = int and type context = unit = struct
        type state = int
        type context = unit
        let name = "v1"
        let validate n =
          { verdict = if n > 0 then Pass "v1 ok" else Fail "v1 fail";
            confidence = 0.8;
            context = ();
            children = [];
            metadata = []; }
      end in
      let module V2 : VALIDATOR with type state = int and type context = unit = struct
        type state = int
        type context = unit
        let name = "v2"
        let validate n =
          { verdict = if n > 5 then Pass "v2 ok" else Warn "v2 warn";
            confidence = 0.9;
            context = ();
            children = [];
            metadata = []; }
      end in
      let module Seq = (val Compose.sequence (module V1) (module V2)) in
      assert (Seq.name = "v1 >> v2");
      let r = Seq.validate 10 in
      assert (List.length r.children = 2);
      Printf.printf "[OK] compose sequence\n%!"
    in

    (* Test 5: Compose.parallel *)
    let () =
      let module V1 : VALIDATOR with type state = int and type context = unit = struct
        type state = int
        type context = unit
        let name = "v1"
        let validate _ =
          Eio.Time.sleep clock 0.01;
          { verdict = Pass "v1";
            confidence = 0.8;
            context = ();
            children = [];
            metadata = []; }
      end in
      let module V2 : VALIDATOR with type state = int and type context = unit = struct
        type state = int
        type context = unit
        let name = "v2"
        let validate _ =
          Eio.Time.sleep clock 0.01;
          { verdict = Pass "v2";
            confidence = 0.9;
            context = ();
            children = [];
            metadata = []; }
      end in
      let module Par = (val Compose.parallel ~sw (module V1) (module V2)) in
      assert (Par.name = "v1 || v2");
      let r = Par.validate 1 in
      assert (List.length r.children = 2);
      Printf.printf "[OK] compose parallel\n%!"
    in

    (* Test 6: Meta.create with AllMustPass *)
    let () =
      let module V1 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v1"
        let validate () =
          { verdict = Pass "ok"; confidence = 1.0; context = (); children = []; metadata = []; }
      end in
      let module V2 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v2"
        let validate () =
          { verdict = Pass "ok"; confidence = 0.9; context = (); children = []; metadata = []; }
      end in
      let module M = (val Meta.create ~sw ~name:"meta" ~policy:Meta.AllMustPass
        [(module V1 : VALIDATOR with type state = unit and type context = unit);
         (module V2)]) in
      let r = M.validate () in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      assert (List.length r.children = 2);
      Printf.printf "[OK] meta AllMustPass\n%!"
    in

    (* Test 7: Meta.create with MajorityPass *)
    let () =
      let module V1 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v1"
        let validate () =
          { verdict = Pass "ok"; confidence = 1.0; context = (); children = []; metadata = []; }
      end in
      let module V2 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v2"
        let validate () =
          { verdict = Fail "fail"; confidence = 0.0; context = (); children = []; metadata = []; }
      end in
      let module V3 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v3"
        let validate () =
          { verdict = Pass "ok"; confidence = 0.8; context = (); children = []; metadata = []; }
      end in
      let module M = (val Meta.create ~sw ~name:"meta" ~policy:Meta.MajorityPass
        [(module V1 : VALIDATOR with type state = unit and type context = unit);
         (module V2); (module V3)]) in
      let r = M.validate () in
      (* 2/3 pass = majority *)
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] meta MajorityPass\n%!"
    in

    (* Test 8: Meta.nested (2-level) *)
    let () =
      let module V1 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v1"
        let validate () =
          { verdict = Pass "ok"; confidence = 1.0; context = (); children = []; metadata = []; }
      end in
      let module Nested = (val Meta.nested ~sw ~name:"nested"
        ~outer_policy:Meta.AllMustPass
        ~inner_policy:Meta.AnyPass
        [(module V1 : VALIDATOR with type state = unit and type context = unit)]) in
      let r = Nested.validate () in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      (* Nested has 1 child (inner), inner has 1 child (v1) *)
      assert (List.length r.children = 1);
      Printf.printf "[OK] meta nested\n%!"
    in

    (* Test 9: Compose.quorum *)
    let () =
      let module V1 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v1"
        let validate () =
          { verdict = Pass "ok"; confidence = 1.0; context = (); children = []; metadata = []; }
      end in
      let module V2 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v2"
        let validate () =
          { verdict = Fail "fail"; confidence = 0.0; context = (); children = []; metadata = []; }
      end in
      let module V3 : VALIDATOR with type state = unit and type context = unit = struct
        type state = unit
        type context = unit
        let name = "v3"
        let validate () =
          { verdict = Pass "ok"; confidence = 0.9; context = (); children = []; metadata = []; }
      end in
      let validators = [
        (module V1 : VALIDATOR with type state = unit and type context = unit);
        (module V2);
        (module V3);
      ] in
      let module Q = (val Compose.quorum ~sw ~required:2 validators) in
      assert (Q.name = "quorum(2/3)");
      let r = Q.validate () in
      (* 2/3 required, 2 passed *)
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] compose quorum\n%!"
    in

    (* Test 10: make_timeout_validator *)
    let () =
      let now = Eio.Time.now clock in
      let module T = (val make_timeout_validator
        ~get_now:(fun () -> now)
        ~deadline:(now +. 120.0)) in
      let r = T.validate () in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      assert (r.context > 60.0);  (* 120초 남음 *)
      Printf.printf "[OK] timeout validator\n%!"
    in

    (* Test 11: Monadic bind - 동적 Validator 선택 *)
    let () =
      let module V1 : VALIDATOR with type state = int and type context = int = struct
        type state = int
        type context = int
        let name = "score_check"
        let validate n =
          { verdict = Pass "checked"; confidence = 1.0; context = n;
            children = []; metadata = []; }
      end in

      (* 점수에 따라 다른 Validator 선택 *)
      let choose_next (r : int result) =
        if r.context >= 80 then
          (module struct
            type state = int
            type context = int
            let name = "excellent"
            let validate _ =
              { verdict = Pass "A grade"; confidence = 1.0; context = r.context;
                children = []; metadata = [("grade", "A")]; }
          end : VALIDATOR with type state = int and type context = int)
        else
          (module struct
            type state = int
            type context = int
            let name = "needs_work"
            let validate _ =
              { verdict = Warn "needs improvement"; confidence = 0.7; context = r.context;
                children = []; metadata = [("grade", "C")]; }
          end : VALIDATOR with type state = int and type context = int)
      in

      let module Bound = (val Compose.bind (module V1) choose_next) in

      (* 90점 -> excellent 경로 *)
      let r1 = Bound.validate 90 in
      assert (match r1.verdict with Pass "A grade" -> true | _ -> false);
      assert (List.exists (fun (k, v) -> k = "grade" && v = "A") r1.metadata);

      (* 60점 -> needs_work 경로 *)
      let r2 = Bound.validate 60 in
      assert (match r2.verdict with Warn _ -> true | _ -> false);
      assert (List.exists (fun (k, v) -> k = "grade" && v = "C") r2.metadata);

      Printf.printf "[OK] monadic bind (dynamic selection)\n%!"
    in

    (* Test 12: Functor map *)
    let () =
      let module V : VALIDATOR with type state = int and type context = int = struct
        type state = int
        type context = int
        let name = "doubler"
        let validate n =
          { verdict = Pass "doubled"; confidence = 1.0; context = n * 2;
            children = []; metadata = []; }
      end in

      let module Mapped = (val Compose.map (fun x -> string_of_int x) (module V)) in
      let r = Mapped.validate 21 in
      assert (r.context = "42");  (* int -> string 변환 *)
      Printf.printf "[OK] functor map\n%!"
    in

    (* Test 13: Monad Associativity Law *)
    (* bind (bind m f) g ≡ bind m (λx. bind (f x) g) *)
    let () =
      let module M : VALIDATOR with type state = int and type context = int = struct
        type state = int
        type context = int
        let name = "m"
        let validate n =
          { verdict = Pass "m"; confidence = 1.0; context = n;
            children = []; metadata = []; }
      end in

      (* f: int result -> Validator (doubles context) *)
      let f (_r : int result) : (module VALIDATOR with type state = int and type context = int) =
        (module struct
          type state = int
          type context = int
          let name = "f"
          let validate n =
            { verdict = Pass "f"; confidence = 1.0; context = n * 2;
              children = []; metadata = []; }
        end)
      in

      (* g: int result -> Validator (adds 10 to context) *)
      let g (_r : int result) : (module VALIDATOR with type state = int and type context = int) =
        (module struct
          type state = int
          type context = int
          let name = "g"
          let validate _ =
            { verdict = Pass "g"; confidence = 1.0; context = _r.context + 10;
              children = []; metadata = []; }
        end)
      in

      (* Left side: bind (bind m f) g *)
      let module MF = (val Compose.bind (module M) f) in
      let module Left = (val Compose.bind (module MF) g) in

      (* Right side: bind m (λx. bind (f x) g) *)
      let fg (r : int result) =
        let (module F_result) = f r in
        Compose.bind (module F_result) g
      in
      let module Right = (val Compose.bind (module M) fg) in

      let left_result = Left.validate 5 in
      let right_result = Right.validate 5 in

      (* Both should produce same final context: (5 * 2) + 10 = 20 *)
      assert (left_result.context = right_result.context);
      assert (left_result.context = 20);

      Printf.printf "[OK] monad associativity law\n%!"
    in

    (* Test 14: Functor Identity Law *)
    (* map id ≡ id *)
    let () =
      let module V : VALIDATOR with type state = int and type context = int = struct
        type state = int
        type context = int
        let name = "v"
        let validate n =
          { verdict = Pass "v"; confidence = 0.9; context = n * 3;
            children = []; metadata = [("key", "value")]; }
      end in

      let module Mapped = (val Compose.map (fun x -> x) (module V)) in
      let original = V.validate 7 in
      let mapped = Mapped.validate 7 in

      assert (original.context = mapped.context);
      assert (original.confidence = mapped.confidence);
      assert (match original.verdict, mapped.verdict with
        | Pass a, Pass b -> a = b
        | _ -> false);

      Printf.printf "[OK] functor identity law\n%!"
    in

    (* Test 15: Functor Composition Law *)
    (* map (f . g) ≡ map f . map g *)
    let () =
      let module V : VALIDATOR with type state = int and type context = int = struct
        type state = int
        type context = int
        let name = "v"
        let validate n =
          { verdict = Pass "v"; confidence = 1.0; context = n;
            children = []; metadata = []; }
      end in

      let f x = x * 2 in
      let g x = x + 3 in

      (* Left: map (f . g) *)
      let module Left = (val Compose.map (fun x -> f (g x)) (module V)) in

      (* Right: map f . map g *)
      let module G_mapped = (val Compose.map g (module V)) in
      let module Right = (val Compose.map f (module G_mapped)) in

      let left_result = Left.validate 10 in
      let right_result = Right.validate 10 in

      (* Both should produce: (10 + 3) * 2 = 26 *)
      assert (left_result.context = right_result.context);
      assert (left_result.context = 26);

      Printf.printf "[OK] functor composition law\n%!"
    in

    (* Test 16: map preserves children info in metadata *)
    let () =
      (* Create a validator that has children *)
      let module V1 : VALIDATOR with type state = int and type context = unit = struct
        type state = int
        type context = unit
        let name = "v1"
        let validate _ =
          { verdict = Pass "v1 ok"; confidence = 0.8; context = ();
            children = []; metadata = []; }
      end in
      let module V2 : VALIDATOR with type state = int and type context = unit = struct
        type state = int
        type context = unit
        let name = "v2"
        let validate _ =
          { verdict = Pass "v2 ok"; confidence = 0.9; context = ();
            children = []; metadata = []; }
      end in

      (* Sequence creates children *)
      let module Seq = (val Compose.sequence (module V1) (module V2)) in

      (* Map over sequence result *)
      let module Mapped = (val Compose.map (fun () -> "mapped") (module Seq)) in

      let r = Mapped.validate 1 in

      (* Check children info preserved in metadata *)
      assert (List.assoc_opt "children_count" r.metadata = Some "2");
      assert (List.assoc_opt "child_0_verdict" r.metadata = Some "Pass:v1 ok");
      assert (List.assoc_opt "child_1_verdict" r.metadata = Some "Pass:v2 ok");

      Printf.printf "[OK] map preserves children in metadata\n%!"
    in

    let _ = sw in  (* suppress unused warning *)
    let _ = clock in

    Printf.printf "\n✅ All 16 validator tests passed!\n%!"

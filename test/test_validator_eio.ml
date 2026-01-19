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

    Printf.printf "\n✅ All 10 validator tests passed!\n%!"

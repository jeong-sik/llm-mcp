(** Test Validator Presets and Spec DSL *)

open Agent_core_eio.Validator
open Agent_core_eio.Validator_presets

let () = Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in

    (* ===== Test Validators ===== *)

    let module Always_pass : VALIDATOR with type state = int and type context = unit = struct
      type state = int
      type context = unit
      let name = "always_pass"
      let validate _ =
        { verdict = Pass "ok"; confidence = 1.0; context = (); children = []; metadata = []; }
    end in

    let module Always_fail : VALIDATOR with type state = int and type context = unit = struct
      type state = int
      type context = unit
      let name = "always_fail"
      let validate _ =
        { verdict = Fail "nope"; confidence = 0.0; context = (); children = []; metadata = []; }
    end in

    let module Check_positive : VALIDATOR with type state = int and type context = unit = struct
      type state = int
      type context = unit
      let name = "check_positive"
      let validate n =
        if n > 0 then
          { verdict = Pass "positive"; confidence = 1.0; context = (); children = []; metadata = []; }
        else
          { verdict = Fail "not positive"; confidence = 0.0; context = (); children = []; metadata = []; }
    end in

    let module Slow_pass : VALIDATOR with type state = int and type context = unit = struct
      type state = int
      type context = unit
      let name = "slow_pass"
      let validate _ =
        Eio.Time.sleep clock 0.01;
        { verdict = Pass "slow ok"; confidence = 0.9; context = (); children = []; metadata = []; }
    end in

    (* Test 1: Pipeline *)
    let () =
      let pipeline = Pipeline.create ~sw [
        (module Always_pass : VALIDATOR with type state = int and type context = unit);
        (module Check_positive);
      ] in
      let module P = (val pipeline) in
      let r = P.validate 5 in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Pipeline pattern\n%!"
    in

    (* Test 2: Pipeline with failure *)
    let () =
      let pipeline = Pipeline.create ~sw [
        (module Always_pass : VALIDATOR with type state = int and type context = unit);
        (module Always_fail);
        (module Check_positive);  (* should not reach *)
      ] in
      let module P = (val pipeline) in
      let r = P.validate 5 in
      assert (match r.verdict with Fail _ -> true | _ -> false);
      Printf.printf "[OK] Pipeline stops on failure\n%!"
    in

    (* Test 3: Named Pipeline *)
    let () =
      let pipeline = Pipeline.named ~sw ~name:"my_pipeline" [
        (module Always_pass : VALIDATOR with type state = int and type context = unit);
        (module Check_positive);
      ] in
      let module P = (val pipeline) in
      assert (P.name = "my_pipeline");
      Printf.printf "[OK] Named pipeline\n%!"
    in

    (* Test 4: Fanout *)
    let () =
      let fanout = Fanout.create ~sw [
        (module Always_pass : VALIDATOR with type state = int and type context = unit);
        (module Slow_pass);
        (module Check_positive);
      ] in
      let module F = (val fanout) in
      let r = F.validate 5 in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Fanout pattern\n%!"
    in

    (* Test 5: Fanout with policy *)
    let () =
      let fanout = Fanout.with_policy ~sw ~policy:Meta.MajorityPass [
        (module Always_pass : VALIDATOR with type state = int and type context = unit);
        (module Always_fail);
        (module Check_positive);
      ] in
      let module F = (val fanout) in
      let r = F.validate 5 in
      (* 2/3 pass = majority *)
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Fanout with MajorityPass policy\n%!"
    in

    (* Test 6: Quorum majority *)
    let () =
      let quorum = Quorum.majority ~sw [
        (module Always_pass : VALIDATOR with type state = int and type context = unit);
        (module Always_fail);
        (module Check_positive);
      ] in
      let module Q = (val quorum) in
      let r = Q.validate 5 in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Quorum majority\n%!"
    in

    (* Test 7: Quorum unanimous *)
    let () =
      let quorum = Quorum.unanimous ~sw [
        (module Always_pass : VALIDATOR with type state = int and type context = unit);
        (module Always_fail);
      ] in
      let module Q = (val quorum) in
      let r = Q.validate 5 in
      (* 1/2 pass, need all *)
      assert (match r.verdict with Fail _ -> true | _ -> false);
      Printf.printf "[OK] Quorum unanimous (fails when not all pass)\n%!"
    in

    (* Test 8: Quorum any *)
    let () =
      let quorum = Quorum.any ~sw [
        (module Always_fail : VALIDATOR with type state = int and type context = unit);
        (module Always_fail);
        (module Check_positive);
      ] in
      let module Q = (val quorum) in
      let r = Q.validate 5 in
      (* 1/3 pass, need any *)
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Quorum any\n%!"
    in

    (* Test 9: Gate on_state *)
    let () =
      let gate = Gate.on_state
        ~condition:(fun n -> n > 10)
        (module Always_fail : VALIDATOR with type state = int and type context = unit)
      in
      let module G = (val gate) in
      let r = G.validate 5 in  (* condition not met, should skip *)
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Gate skips when condition not met\n%!"
    in

    (* Test 10: Gate executes when condition met *)
    let () =
      let gate = Gate.on_state
        ~condition:(fun n -> n > 0)
        (module Check_positive : VALIDATOR with type state = int and type context = unit)
      in
      let module G = (val gate) in
      let r = G.validate 5 in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Gate executes when condition met\n%!"
    in

    (* Test 11: Feature flag *)
    let () =
      let flag_enabled = ref false in
      let gate = Gate.feature_flag
        ~flag_name:"new_feature"
        ~is_enabled:(fun () -> !flag_enabled)
        (module Always_fail : VALIDATOR with type state = int and type context = unit)
      in
      let module G = (val gate) in

      (* Flag off: should pass (skipped) *)
      let r1 = G.validate 5 in
      assert (match r1.verdict with Pass _ -> true | _ -> false);

      (* Flag on: should fail *)
      flag_enabled := true;
      let r2 = G.validate 5 in
      assert (match r2.verdict with Fail _ -> true | _ -> false);
      Printf.printf "[OK] Feature flag gate\n%!"
    in

    (* Test 12: Layered two-layer *)
    let () =
      let layered = Layered.two_layer ~sw
        ~layer1_name:"code" ~layer1_policy:Meta.AllMustPass
        ~layer2_name:"ops" ~layer2_policy:Meta.AnyPass
        ~outer_policy:Meta.AllMustPass
        [(module Always_pass : VALIDATOR with type state = int and type context = unit);
         (module Check_positive)]
        [(module Always_fail : VALIDATOR with type state = int and type context = unit);
         (module Slow_pass)]
      in
      let module L = (val layered) in
      let r = L.validate 5 in
      (* code: all pass (2/2), ops: any pass (1/2), outer: all pass *)
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Layered two-layer\n%!"
    in

    (* Test 13: Weighted vote *)
    let () =
      let weighted = Weighted_vote.create ~sw ~threshold:0.5 [
        { weight = 2.0; validator = (module Always_pass : VALIDATOR with type state = int and type context = unit) };
        { weight = 1.0; validator = (module Always_fail) };
        { weight = 1.0; validator = (module Check_positive) };
      ] in
      let module W = (val weighted) in
      let r = W.validate 5 in
      (* Score: 2.0 + 0 + 1.0 = 3.0 / 4.0 = 75% >= 50% *)
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Weighted vote\n%!"
    in

    (* Test 14: Weighted vote fails threshold *)
    let () =
      let weighted = Weighted_vote.create ~sw ~threshold:0.8 [
        { weight = 1.0; validator = (module Always_pass : VALIDATOR with type state = int and type context = unit) };
        { weight = 1.0; validator = (module Always_fail) };
        { weight = 1.0; validator = (module Always_fail) };
      ] in
      let module W = (val weighted) in
      let r = W.validate 5 in
      (* Score: 1.0 / 3.0 = 33% < 80% *)
      assert (match r.verdict with Fail _ -> true | _ -> false);
      Printf.printf "[OK] Weighted vote fails threshold\n%!"
    in

    (* Test 15: Diamond pattern *)
    let () =
      let diamond = Diamond.simple ~sw
        ~pre:(module Always_pass : VALIDATOR with type state = int and type context = unit)
        ~branch_a:(module Check_positive)
        ~branch_b:(module Slow_pass)
        ~post:(module Always_pass)
      in
      let module D = (val diamond) in
      let r = D.validate 5 in
      assert (match r.verdict with Pass _ -> true | _ -> false);
      Printf.printf "[OK] Diamond pattern\n%!"
    in

    Printf.printf "\n✅ All 15 preset tests passed!\n%!"

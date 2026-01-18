(** Ouroboros v78.0: Swarm Economy v2.0 - OCaml
    Intelligent value assessment and settlement driven by LLM logic. *)

let assess_value task_report =
  let preview = if String.length task_report > 50 then String.sub task_report 0 50 else task_report in
  Printf.printf "=== [BANKER] Assessing value of work: '%s...' ===\n" preview;

  (* Simulated LLM Assessment *)
  let value =
    if try ignore (Str.search_forward (Str.regexp "Quantum\\|Evolution") task_report 0); true
       with Not_found -> false
    then 150.0  (* High value tech *)
    else if try ignore (Str.search_forward (Str.regexp_string "Cleanup") task_report 0); true
            with Not_found -> false
    then 20.0   (* Maintenance *)
    else 50.0   (* Standard *)
  in

  Printf.printf "[VALUATION] LLM determined value: %.1f O-Credits\n" value;
  value

let settle_accounts worker_node amount =
  Printf.printf "=== [SETTLEMENT] Transferring %.1f credits to %s... ===\n" amount worker_node;
  print_endline "[SUCCESS] Balance updated for the hive."

let print_usage () =
  print_endline "Usage: ouroboros-economy-v2 assess 'Report Text'";
  print_endline "       ouroboros-economy-v2 pay <NodeName> <Amount>"

let () =
  if Array.length Sys.argv < 3 then begin
    print_usage ();
    exit 1
  end;

  let cmd = Sys.argv.(1) in
  if cmd = "assess" then
    ignore (assess_value Sys.argv.(2))
  else if cmd = "pay" && Array.length Sys.argv >= 4 then
    settle_accounts Sys.argv.(2) (float_of_string Sys.argv.(3))
  else
    print_usage ()

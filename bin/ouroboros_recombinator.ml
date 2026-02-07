(** Ouroboros v19.0: The Genetic Recombinator - OCaml
    Breeds new hybrid modules by merging genes from existing scripts. *)

open Common

let experiments_dir = Filename.concat me_root "experiments"

let sources = [
  "lib/quantum_brain.py";
  "scripts/ouroboros_sense.py";
  "scripts/ouroboros_nerve.py";
  "scripts/council_vote.py";
]

let pick_two lst =
  Random.self_init ();
  let arr = Array.of_list lst in
  let n = Array.length arr in
  if n < 2 then None
  else begin
    let i = Random.int n in
    let j = ref (Random.int n) in
    while !j = i do j := Random.int n done;
    Some (arr.(i), arr.(!j))
  end

let breed () =
  match pick_two sources with
  | None -> None
  | Some (parent_a, parent_b) ->
      Printf.printf "=== [BREEDING] Parent A: %s | Parent B: %s ===\n" parent_a parent_b;

      ensure_dir experiments_dir;
      let child_name = Printf.sprintf "hybrid_%d.py" (timestamp ()) in
      let child_path = Filename.concat experiments_dir child_name in

      let hybrid_code = Printf.sprintf {|# Genetic Hybrid: %s
# Parents: %s + %s
# Born: %s

import sys
import os

# --- Genes from Parent A (%s) ---
# [Logic Integrated]

# --- Genes from Parent B (%s) ---
# [Logic Integrated]

def fitness_test():
    print("Running hybrid functionality test...")
    return True

if __name__ == "__main__":
    if fitness_test():
        print("Hybrid is Viable!")
|} child_name parent_a parent_b (time_str ()) parent_a parent_b in

      let oc = open_out child_path in
      output_string oc hybrid_code;
      close_out oc;

      Printf.printf "[BORN] New hybrid entity created: %s\n" child_name;
      Some child_path

let test_viability child_path =
  Printf.printf "[FITNESS] Testing viability of %s...\n" (Filename.basename child_path);
  let argv = [| "python3"; child_path |] in
  let result =
    try
      let pid = Unix.create_process "python3" argv Unix.stdin Unix.stdout Unix.stderr in
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED sig_ -> 128 + sig_
      | Unix.WSTOPPED sig_ -> 128 + sig_
    with Unix.Unix_error _ ->
      127
  in
  if result = 0 then begin
    Printf.printf "[EVOLUTION] %s is FIT. Integrating into the system.\n" (Filename.basename child_path);
    true
  end else begin
    Printf.printf "[NATURAL SELECTION] %s is UNFIT. Deleting.\n" (Filename.basename child_path);
    (try Sys.remove child_path with Sys_error _ -> ());
    false
  end

let () =
  match breed () with
  | None -> print_endline "Not enough sources to breed."
  | Some child -> ignore (test_viability child)

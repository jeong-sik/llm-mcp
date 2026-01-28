(** Tests for Chain_registry module - Chain storage and retrieval

    Tests registry operations:
    - register/lookup cycle
    - exists check
    - unregister
    - list_ids, list_all
    - stats computation
    - version incrementing
*)

open Alcotest
open Chain_types
open Chain_registry

(** {1 Test Helpers} *)

let make_unique_id base =
  Printf.sprintf "%s_%d" base (Random.int 1_000_000)

(** Create a minimal test chain using the helper function *)
let make_test_chain id =
  let node = {
    id = "start";
    node_type = Llm {
      model = "test";
      system = None;
      prompt = "test prompt";
      timeout = None;
      tools = None;
      prompt_ref = None;
      prompt_vars = [];
    };
    input_mapping = [];
    output_key = None;
    depends_on = None;
  } in
  make_chain ~id ~nodes:[node] ~output:"start"
    ~name:("Test " ^ id) ~description:("Test chain " ^ id) ()

(** {1 Register/Lookup Tests} *)

let test_register_and_lookup () =
  let chain_id = make_unique_id "test_chain" in
  let chain = make_test_chain chain_id in
  register chain;
  let found = lookup chain_id in
  check bool "found" true (Option.is_some found);
  let retrieved = Option.get found in
  check string "id matches" chain_id retrieved.id

let test_lookup_missing () =
  let missing_id = make_unique_id "missing" in
  let result = lookup missing_id in
  check bool "not found" true (Option.is_none result)

let test_lookup_exn_missing () =
  let missing_id = make_unique_id "missing_exn" in
  try
    let _ = lookup_exn missing_id in
    fail "should raise Not_found"
  with Not_found -> ()

let test_lookup_entry_metadata () =
  let chain_id = make_unique_id "entry_test" in
  let chain = make_test_chain chain_id in
  let desc = "Test description" in
  register ~description:desc chain;
  let entry = lookup_entry chain_id in
  check bool "found entry" true (Option.is_some entry);
  let e = Option.get entry in
  check string "chain id" chain_id e.chain.id;
  check int "version 1" 1 e.version;
  check bool "has timestamp" true (e.registered_at > 0.0);
  check (option string) "description" (Some desc) e.description

(** {1 Exists Tests} *)

let test_exists_registered () =
  let chain_id = make_unique_id "exists_test" in
  let chain = make_test_chain chain_id in
  register chain;
  check bool "exists" true (exists chain_id)

let test_exists_not_registered () =
  let missing_id = make_unique_id "not_exists" in
  check bool "not exists" false (exists missing_id)

(** {1 Unregister Tests} *)

let test_unregister_existing () =
  let chain_id = make_unique_id "unregister_test" in
  let chain = make_test_chain chain_id in
  register chain;
  check bool "exists before" true (exists chain_id);
  let removed = unregister chain_id in
  check bool "removed" true removed;
  check bool "not exists after" false (exists chain_id)

let test_unregister_missing () =
  let missing_id = make_unique_id "unregister_missing" in
  let removed = unregister missing_id in
  check bool "not removed" false removed

(** {1 Version Tests} *)

let test_version_increments () =
  let chain_id = make_unique_id "version_test" in
  let chain = make_test_chain chain_id in

  (* First registration *)
  register chain;
  let entry1 = lookup_entry chain_id in
  check int "version 1" 1 (Option.get entry1).version;

  (* Re-register same ID *)
  register chain;
  let entry2 = lookup_entry chain_id in
  check int "version 2" 2 (Option.get entry2).version;

  (* Third time *)
  register chain;
  let entry3 = lookup_entry chain_id in
  check int "version 3" 3 (Option.get entry3).version

(** {1 List Tests} *)

let test_list_ids_includes_registered () =
  let chain_id = make_unique_id "list_test" in
  let chain = make_test_chain chain_id in
  register chain;
  let ids = list_ids () in
  check bool "contains id" true (List.mem chain_id ids)

let test_list_all_includes_registered () =
  let chain_id = make_unique_id "list_all_test" in
  let chain = make_test_chain chain_id in
  register chain;
  let chains = list_all () in
  let found = List.exists (fun c -> c.id = chain_id) chains in
  check bool "contains chain" true found

let test_list_entries_has_metadata () =
  let chain_id = make_unique_id "list_entries_test" in
  let chain = make_test_chain chain_id in
  register chain;
  let entries = list_entries () in
  let found = List.find_opt (fun (id, _) -> id = chain_id) entries in
  check bool "found" true (Option.is_some found);
  let (_, entry) = Option.get found in
  check int "version" 1 entry.version

(** {1 Stats Tests} *)

let test_stats_counts_chains () =
  let chain_id = make_unique_id "stats_test" in
  let chain = make_test_chain chain_id in
  register chain;
  let s = stats () in
  check bool "has chains" true (s.total_chains >= 1)

let test_stats_counts_nodes () =
  let chain_id = make_unique_id "stats_nodes_test" in
  let chain = make_test_chain chain_id in
  register chain;
  let s = stats () in
  (* Our test chain has 1 node *)
  check bool "has nodes" true (s.total_nodes >= 1)

let test_stats_tracks_oldest_newest () =
  let chain_id = make_unique_id "stats_time_test" in
  let chain = make_test_chain chain_id in
  register chain;
  let s = stats () in
  check bool "has oldest" true (Option.is_some s.oldest_chain);
  check bool "has newest" true (Option.is_some s.newest_chain)

(** {1 Test Suite} *)

let register_lookup_tests = [
  test_case "register and lookup" `Quick test_register_and_lookup;
  test_case "lookup missing" `Quick test_lookup_missing;
  test_case "lookup_exn missing" `Quick test_lookup_exn_missing;
  test_case "lookup entry metadata" `Quick test_lookup_entry_metadata;
]

let exists_tests = [
  test_case "exists registered" `Quick test_exists_registered;
  test_case "exists not registered" `Quick test_exists_not_registered;
]

let unregister_tests = [
  test_case "unregister existing" `Quick test_unregister_existing;
  test_case "unregister missing" `Quick test_unregister_missing;
]

let version_tests = [
  test_case "version increments" `Quick test_version_increments;
]

let list_tests = [
  test_case "list_ids" `Quick test_list_ids_includes_registered;
  test_case "list_all" `Quick test_list_all_includes_registered;
  test_case "list_entries" `Quick test_list_entries_has_metadata;
]

let stats_tests = [
  test_case "counts chains" `Quick test_stats_counts_chains;
  test_case "counts nodes" `Quick test_stats_counts_nodes;
  test_case "tracks time" `Quick test_stats_tracks_oldest_newest;
]

let () =
  Random.init (int_of_float (Unix.gettimeofday () *. 1000.0));
  run "chain_registry" [
    ("register_lookup", register_lookup_tests);
    ("exists", exists_tests);
    ("unregister", unregister_tests);
    ("version", version_tests);
    ("list", list_tests);
    ("stats", stats_tests);
  ]

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

let () = Random.init 42
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
      prompt_vars = []; thinking = false;
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

let test_load_from_dir_registers_chains () =
  (* Isolated registry state for this test *)
  clear ();
  let dir = Printf.sprintf "/tmp/llm-mcp-chain-registry-load-%d-%d"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.0)) in
  Unix.mkdir dir 0o755;

  let chain_id = make_unique_id "load_dir_test" in
  let chain = make_test_chain chain_id in
  let path = Filename.concat dir (chain_id ^ ".json") in
  let json = chain_to_yojson chain |> Yojson.Safe.pretty_to_string in
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc json);

  let loaded, errors = load_from_dir dir in
  check int "loaded" 1 loaded;
  check int "errors" 0 (List.length errors);
  check bool "exists after load" true (exists chain_id);
  check bool "lookup after load" true (Option.is_some (lookup chain_id));

  (* Best-effort cleanup *)
  (try Sys.remove path with _ -> ());
  (try Unix.rmdir dir with _ -> ())

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
  test_case "load_from_dir registers chains" `Quick test_load_from_dir_registers_chains;
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

(** {1 Error Logging Tests (PR #155)} *)

(** Redirect Unix stderr to a pipe and collect output *)
let capture_stderr f =
  let buf = Buffer.create 256 in
  let pipe_read, pipe_write = Unix.pipe () in
  let old_stderr = Unix.dup Unix.stderr in
  Unix.dup2 pipe_write Unix.stderr;
  Unix.close pipe_write;
  (try f () with _ -> ());
  Unix.dup2 old_stderr Unix.stderr;
  Unix.close old_stderr;
  let tmp = Bytes.create 4096 in
  let rec drain () =
    match Unix.read pipe_read tmp 0 4096 with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf tmp 0 n; drain ()
    | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
  in
  drain ();
  Unix.close pipe_read;
  Buffer.contents buf

(** of_json: an entry whose "chain" field is missing/invalid must log to stderr.
    The overall call returns Ok with skipped entry rather than failing entirely. *)
let test_of_json_invalid_entry_logs_stderr () =
  clear ();
  (* Build a JSON list with one valid chain and one entry with a broken chain *)
  let valid_chain = make_test_chain (make_unique_id "valid") in
  let valid_json = Chain_types.chain_to_yojson valid_chain in
  let valid_entry = `Assoc [
    ("chain", valid_json);
    ("version", `Int 1);
    ("registered_at", `Float (Unix.gettimeofday ()));
    ("description", `Null);
  ] in
  let invalid_entry = `Assoc [
    ("chain", `Assoc [("id", `String ""); ("bad_field", `Bool true)]);
    ("version", `Int 1);
    ("registered_at", `Float (Unix.gettimeofday ()));
    ("description", `Null);
  ] in
  let json = `List [valid_entry; invalid_entry] in
  let stderr_output = capture_stderr (fun () ->
    match of_json json with
    | Ok _ -> ()
    | Error _ -> ()
  ) in
  check bool "stderr not empty for invalid entry" true (String.length stderr_output > 0);
  let has_prefix = try
    let _ = Str.search_forward (Str.regexp_string "[chain_registry]") stderr_output 0 in
    true
  with Not_found -> false in
  check bool "stderr contains [chain_registry] prefix" true has_prefix

(** of_json: completely valid input must not write to stderr and must return the count *)
let test_of_json_valid_no_stderr () =
  clear ();
  let chain = make_test_chain (make_unique_id "valid_nolog") in
  let chain_json = Chain_types.chain_to_yojson chain in
  let entry = `Assoc [
    ("chain", chain_json);
    ("version", `Int 1);
    ("registered_at", `Float (Unix.gettimeofday ()));
    ("description", `Null);
  ] in
  let json = `List [entry] in
  let result = ref (Ok 0) in
  let stderr_output = capture_stderr (fun () ->
    result := of_json json
  ) in
  check bool "no stderr for valid input" true (String.length stderr_output = 0);
  (match !result with
   | Ok n -> check bool "loaded 1 entry" true (n >= 1)
   | Error msg -> Alcotest.fail ("of_json failed: " ^ msg))

(** load_from_dir: corrupt JSON file is captured as error, not silently lost *)
let test_load_from_dir_corrupt_file_returns_error () =
  clear ();
  let dir = Printf.sprintf "/tmp/llm-mcp-cr-corrupt-%d-%d"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.0)) in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "corrupt.json" in
  Out_channel.with_open_text path (fun oc ->
    Out_channel.output_string oc "not valid json {{{");
  let loaded, errors = load_from_dir dir in
  (try Sys.remove path with _ -> ());
  (try Unix.rmdir dir with _ -> ());
  check int "loaded 0 chains from corrupt dir" 0 loaded;
  check bool "errors list is non-empty" true (List.length errors > 0)

let error_logging_tests = [
  test_case "of_json invalid entry logs stderr" `Quick test_of_json_invalid_entry_logs_stderr;
  test_case "of_json valid no stderr" `Quick test_of_json_valid_no_stderr;
  test_case "load_from_dir corrupt returns error" `Quick test_load_from_dir_corrupt_file_returns_error;
]

let () =
  Random.init 42;
  run "chain_registry" [
    ("register_lookup", register_lookup_tests);
    ("exists", exists_tests);
    ("unregister", unregister_tests);
    ("version", version_tests);
    ("list", list_tests);
    ("stats", stats_tests);
    ("error_logging", error_logging_tests);
  ]

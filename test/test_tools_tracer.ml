(** Tests for Tools_tracer API key resolution (incl. aliases). *)

open Alcotest

let set_env name value = Unix.putenv name value
let unset_env name = Unix.putenv name ""

let with_env name value f =
  let prev = Sys.getenv_opt name in
  (match value with
   | Some v -> set_env name v
   | None -> unset_env name);
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> set_env name v
      | None -> unset_env name)
    f

let test_get_api_key_gemini_prefers_primary () =
  with_env "GEMINI_API_KEY" (Some "k_primary") (fun () ->
    with_env "GOOGLE_AI_API_KEY" (Some "k_alias") (fun () ->
      check string "uses GEMINI_API_KEY first" "k_primary" (Tools_tracer.get_api_key "GEMINI_API_KEY")
    )
  )

let test_get_api_key_gemini_falls_back_to_alias () =
  with_env "GEMINI_API_KEY" None (fun () ->
    with_env "GOOGLE_AI_API_KEY" (Some "k_alias") (fun () ->
      check string "uses GOOGLE_AI_API_KEY alias" "k_alias" (Tools_tracer.get_api_key "GEMINI_API_KEY")
    )
  )

let test_get_api_key_gemini_missing () =
  with_env "GEMINI_API_KEY" None (fun () ->
    with_env "GOOGLE_AI_API_KEY" None (fun () ->
      check string "missing returns empty" "" (Tools_tracer.get_api_key "GEMINI_API_KEY")
    )
  )

let () =
  run "Tools_tracer" [
    ("api_key",
     [
       test_case "gemini prefers primary" `Quick test_get_api_key_gemini_prefers_primary;
       test_case "gemini fallback to alias" `Quick test_get_api_key_gemini_falls_back_to_alias;
       test_case "gemini missing" `Quick test_get_api_key_gemini_missing;
     ])
  ]


(** Tests for gemini_list parsing (no network). *)

open Alcotest

let ids (models : Yojson.Safe.t list) : string list =
  let open Yojson.Safe.Util in
  List.filter_map (fun m ->
    try Some (m |> member "id" |> to_string) with _ -> None
  ) models

let sample =
  Yojson.Safe.from_string {|
{
  "models": [
    {
      "name": "models/gemini-2.5-flash",
      "displayName": "Gemini 2.5 Flash",
      "description": "fast",
      "inputTokenLimit": 100,
      "outputTokenLimit": 10,
      "supportedGenerationMethods": ["generateContent", "countTokens"]
    },
    {
      "name": "models/embedding-001",
      "supportedGenerationMethods": ["embedContent"]
    }
  ]
}
|}

let test_parse_generate_only () =
  let models = Handler_gemini.parse_models_response ~include_all:false ~filter:None sample in
  check (list string) "ids" ["gemini-2.5-flash"] (ids models)

let test_parse_include_all () =
  let models = Handler_gemini.parse_models_response ~include_all:true ~filter:None sample in
  (* Order preserved from input *)
  check (list string) "ids" ["gemini-2.5-flash"; "embedding-001"] (ids models)

let test_parse_filter () =
  let models = Handler_gemini.parse_models_response ~include_all:true ~filter:(Some "FLASH") sample in
  check (list string) "ids" ["gemini-2.5-flash"] (ids models)

let () =
  run "gemini_list" [
    ("parse", [
      test_case "generateContent only by default" `Quick test_parse_generate_only;
      test_case "include_all=true includes non-generate models" `Quick test_parse_include_all;
      test_case "filter is case-insensitive substring match" `Quick test_parse_filter;
    ]);
  ]


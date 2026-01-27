open Chain_mermaid_parser

let test name mermaid expected_pass =
  match parse_chain mermaid, expected_pass with
  | Ok _, true -> Printf.printf "PASS %s (parsed)\n" name; true
  | Error _, false -> Printf.printf "PASS %s (rejected as expected)\n" name; true
  | Ok _, false -> Printf.printf "FAIL %s (should have failed)\n" name; false
  | Error e, true -> Printf.printf "FAIL %s: %s\n" name e; false

let () =
  let results = [
    (* Should pass *)
    test "Threshold_valid" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=0.8}
    a --> t|} true;
    
    test "Threshold_negative" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=-0.5}
    a --> t|} true;
    
    test "Threshold_scientific" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=1e-3}
    a --> t|} true;
    
    (* Should fail *)
    test "Threshold_empty_value" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=}
    a --> t|} false;
    
    test "Threshold_text_value" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:>=abc}
    a --> t|} false;
    
    test "Threshold_no_operator" {|graph LR
    a["LLM:gemini 's'"]
    t{Threshold:0.8}
    a --> t|} false;
  ] in
  let passed = List.filter Fun.id results |> List.length in
  Printf.printf "\nEdge cases: %d/%d\n" passed (List.length results);
  exit (if passed = List.length results then 0 else 1)

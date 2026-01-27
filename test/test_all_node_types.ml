(* Comprehensive node type validation test - parse only *)
open Chain_mermaid_parser

let test name mermaid =
  match parse_chain mermaid with
  | Error e -> Printf.printf "❌ %s: %s\n" name e; false
  | Ok _chain -> Printf.printf "✅ %s\n" name; true

let () =
  let results = [
    test "Threshold" {|graph LR
    a["LLM:gemini 'score'"]
    t{Threshold:>=0.8}
    a --> t|};
    
    test "FeedbackLoop" {|graph LR
    gen["LLM:gemini 'generate'"]
    loop[["FeedbackLoop:quality,3,0.8"]]
    gen --> loop|};
    
    test "StreamMerge" {|graph LR
    a["LLM:gemini 'stream1'"]
    b["LLM:claude 'stream2'"]
    sm[["StreamMerge:concat,2,30"]]
    a --> sm
    b --> sm|};
    
    test "Adapter" {|graph LR
    data["LLM:gemini 'data'"]
    adapt>/"Adapter:Extract .result"/]
    data --> adapt|};
    
    test "Gate" {|graph LR
    check["LLM:gemini 'check'"]
    gate{Gate:score > 0.5}
    yes["LLM:claude 'approve'"]
    no["LLM:codex 'reject'"]
    check --> gate
    gate -->|yes| yes
    gate -->|no| no|};
    
    test "Quorum" {|graph LR
    a["LLM:gemini 'vote1'"]
    b["LLM:claude 'vote2'"]
    c["LLM:codex 'vote3'"]
    q{Quorum:2}
    a --> q
    b --> q
    c --> q|};
    
    test "Merge" {|graph LR
    a["LLM:gemini 'part1'"]
    b["LLM:claude 'part2'"]
    m{Merge:concat}
    a --> m
    b --> m|};
    
    test "Race" {|graph LR
    fast["LLM:gemini 'fast'"]
    slow["LLM:claude 'slow'"]
    race("Race:10")
    fast --> race
    slow --> race|};
    
    test "Fallback" {|graph LR
    main["LLM:gemini 'main'"]
    backup["LLM:claude 'backup'"]
    fb("Fallback")
    main --> fb
    backup --> fb|};
    
    test "Retry" {|graph LR
    unreliable["LLM:gemini 'unreliable'"]
    retry("Retry:3")
    unreliable --> retry|};
    
    test "Evaluator" {|graph LR
    c1["LLM:gemini 'candidate1'"]
    c2["LLM:claude 'candidate2'"]
    eval{Evaluator:llm_judge:Best:0.7}
    c1 --> eval
    c2 --> eval|};
    
    test "ChainRef" {|graph LR
    input["LLM:gemini 'input'"]
    ref[["ChainRef:other_chain"]]
    input --> ref|};
    
    test "Tool" {|graph LR
    input["LLM:gemini 'input'"]
    tool["Tool:search {}"]
    input --> tool|};
    
    test "MASC_broadcast" {|graph LR
    work["LLM:gemini 'work'"]
    notify(("MASC:broadcast"))
    work --> notify|};
    
    test "MASC_listen" {|graph LR
    wait(("MASC:listen"))
    process["LLM:gemini 'process'"]
    wait --> process|};
    
    test "Pipeline" {|graph LR
    a["LLM:gemini 'a'"]
    b["LLM:claude 'b'"]
    pipe[["Pipeline"]]
    a --> pipe
    b --> pipe|};
    
    test "Fanout" {|graph LR
    a["LLM:gemini 'a'"]
    b["LLM:claude 'b'"]
    fan[["Fanout"]]
    a --> fan
    b --> fan|};
    
    test "Map" {|graph LR
    items["LLM:gemini 'items'"]
    mapped[["Map:transform,items"]]
    items --> mapped|};
    
    test "Spawn" {|graph LR
    worker["LLM:gemini 'worker'"]
    spawn[["Spawn:clean,worker"]]
    worker --> spawn|};
    
    (* Fixed: GoalDriven format is metric:op:value:max_iter with lowercase op *)
    test "GoalDriven" {|graph LR
    action["LLM:gemini 'action'"]
    gd{GoalDriven:score:gte:0.8:5}
    action --> gd|};
    
    (* Fixed: MCTS format is policy:iterations for greedy *)
    test "MCTS_greedy" {|graph LR
    s1["LLM:gemini 's1'"]
    s2["LLM:claude 's2'"]
    mcts{MCTS:greedy:10}
    s1 --> mcts
    s2 --> mcts|};
    
    (* MCTS with ucb1 policy: policy:param:iterations *)
    test "MCTS_ucb1" {|graph LR
    s1["LLM:gemini 's1'"]
    s2["LLM:claude 's2'"]
    mcts{MCTS:ucb1:1.41:10}
    s1 --> mcts
    s2 --> mcts|};
    
    test "Bind" {|graph LR
    data["LLM:gemini 'data'"]
    bound[["Bind:process,data"]]
    data --> bound|};
    
    test "Cache" {|graph LR
    llm["LLM:gemini 'cached'"]
    cache[["Cache:60s,llm"]]
    llm --> cache|};
    
    test "Batch" {|graph LR
    items["LLM:gemini 'items'"]
    batch[["Batch:10,2,items"]]
    items --> batch|};
  ] in
  let passed = List.filter Fun.id results |> List.length in
  let total = List.length results in
  Printf.printf "\n════════════════════════════\n";
  Printf.printf "Results: %d/%d passed\n" passed total;
  Printf.printf "════════════════════════════\n";
  if passed < total then exit 1 else exit 0

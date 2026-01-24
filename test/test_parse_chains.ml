let () =
  let path = "data/chains/magi-code-review.json" in
  let content = In_channel.with_open_text path In_channel.input_all in
  let json = Yojson.Safe.from_string content in
  match Chain_parser.chain_of_yojson json with
  | Ok chain -> Printf.printf "✅ %s: %d nodes\n" chain.Chain_types.id (List.length chain.nodes)
  | Error e -> Printf.printf "❌ %s\n" e

open Cmdliner

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let is_json_file name =
  let len = String.length name in
  len > 5 && String.sub name (len - 5) 5 = ".json"

let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#39;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let render_chain ~path ~title =
  let json_str = read_file path in
  let json = Yojson.Safe.from_string json_str in
  match Chain_parser.parse_chain json with
  | Error e ->
      Printf.sprintf "<section class=\"card error\"><h2>%s</h2><pre>%s</pre></section>\n"
        (html_escape title) (html_escape e)
  | Ok chain ->
      let mermaid = Chain_mermaid_parser.chain_to_mermaid ~styled:true chain in
      let header =
        Printf.sprintf
          "<section class=\"card\"><h2>%s</h2><div class=\"meta\">id: %s · nodes: %d</div><pre class=\"mermaid\">%s</pre></section>\n"
          (html_escape title)
          (html_escape chain.Chain_types.id)
          (List.length chain.Chain_types.nodes)
          (html_escape mermaid)
      in
      header

let build_html ~dir ~out =
  let entries = Sys.readdir dir |> Array.to_list in
  let json_files =
    entries
    |> List.filter (fun name -> is_json_file name)
    |> List.sort String.compare
  in
  let sections =
    json_files
    |> List.map (fun name ->
      let path = Filename.concat dir name in
      render_chain ~path ~title:name)
    |> String.concat "\n"
  in
  let html =
    "<!doctype html>\n" ^
    "<html>\n<head>\n<meta charset=\"utf-8\">\n" ^
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n" ^
    "<title>LLM-MCP Chain Presets</title>\n" ^
    "<script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>\n" ^
    "<style>\n" ^
    "body{margin:0;padding:24px;font-family:ui-sans-serif,system-ui;background:#0f1115;color:#e6e6e6;}\n" ^
    "h1{font-size:22px;margin:0 0 16px;}\n" ^
    ".grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:16px;}\n" ^
    ".card{background:#151821;border:1px solid #2a2f3a;border-radius:12px;padding:14px;}\n" ^
    ".card h2{font-size:14px;margin:0 0 6px;color:#9bd1ff;}\n" ^
    ".meta{font-size:12px;color:#9aa3b2;margin-bottom:8px;}\n" ^
    ".error{border-color:#ff6b6b;}\n" ^
    "pre.mermaid{background:#0f1115;border-radius:8px;padding:8px;overflow:auto;}\n" ^
    "</style>\n" ^
    "</head>\n<body>\n" ^
    "<h1>LLM-MCP Chain Presets</h1>\n" ^
    "<div class=\"grid\">\n" ^
    sections ^
    "</div>\n" ^
    "<script>mermaid.initialize({startOnLoad:true,theme:'dark',securityLevel:'loose'});</script>\n" ^
    "</body>\n</html>\n"
  in
  let oc = open_out out in
  output_string oc html;
  close_out oc

let dir_arg =
  let doc = "Preset JSON directory (default: data/chains)" in
  Arg.(value & opt string "data/chains" & info ["dir"] ~docv:"DIR" ~doc)

let out_arg =
  let doc = "Output HTML path" in
  Arg.(value & opt string "tools/chain-presets/index.html" & info ["out"] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Render all chain preset JSON files into a Mermaid HTML gallery" in
  let info = Cmd.info "chain-presets-viewer" ~doc in
  Cmd.v info Term.(const (fun dir out -> build_html ~dir ~out) $ dir_arg $ out_arg)

let () = exit (Cmd.eval cmd)

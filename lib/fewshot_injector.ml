(** Few-shot Injector - Auto-inject examples based on keywords in prompt

    Usage in tool_parsers.ml:
    let (prompt', injected) = Fewshot_injector.inject_safe prompt in
    (* prompt' contains few-shot if detected, injected = Some "filename.md" *)
*)

(** Keyword to few-shot file mapping *)
let keyword_map = [
  (* OCaml 5.x Effects *)
  ("ocaml 5", "ocaml-5x-effects.md");
  ("effect.t", "ocaml-5x-effects.md");
  ("effect handler", "ocaml-5x-effects.md");
  ("effect.deep", "ocaml-5x-effects.md");
  ("effect.perform", "ocaml-5x-effects.md");
  ("algebraic effect", "ocaml-5x-effects.md");

  (* Rust Async/Tokio *)
  ("tokio", "rust-async.md");
  ("rust async", "rust-async.md");
  ("async fn rust", "rust-async.md");
  ("#[tokio::main]", "rust-async.md");
  ("rust select!", "rust-async.md");
  ("tokio::spawn", "rust-async.md");
  ("rust channel", "rust-async.md");

  (* React Server Components *)
  ("server component", "react-server.md");
  ("use server", "react-server.md");
  ("server action", "react-server.md");
  ("react rsc", "react-server.md");
  ("next.js 14", "react-server.md");
  ("app router", "react-server.md");
  ("revalidatepath", "react-server.md");

  (* TypeScript 5.x *)
  ("typescript 5", "typescript-5x.md");
  ("const type param", "typescript-5x.md");
  ("satisfies", "typescript-5x.md");
  ("using declaration", "typescript-5x.md");
  ("symbol.dispose", "typescript-5x.md");
  ("noinfer", "typescript-5x.md");
  ("ts decorator", "typescript-5x.md");

  (* Swift Concurrency *)
  ("swift async", "swift-concurrency.md");
  ("swift await", "swift-concurrency.md");
  ("swift actor", "swift-concurrency.md");
  ("@mainactor", "swift-concurrency.md");
  ("taskgroup", "swift-concurrency.md");
  ("async let swift", "swift-concurrency.md");
  ("asyncsequence", "swift-concurrency.md");
  ("swift sendable", "swift-concurrency.md");

  (* Haskell Modern - placeholder *)
  ("haskell lens", "haskell-modern.md");
  ("monad transformer", "haskell-modern.md");
  ("mtl", "haskell-modern.md");

  (* MCP Protocol *)
  ("mcp server", "mcp-protocol.md");
  ("mcp tool", "mcp-protocol.md");
  ("mcp protocol", "mcp-protocol.md");
  ("inputschema", "mcp-protocol.md");
  ("tools/call", "mcp-protocol.md");
  ("tools/list", "mcp-protocol.md");
  ("modelcontextprotocol", "mcp-protocol.md");
  ("mcp resource", "mcp-protocol.md");
  ("mcp prompt", "mcp-protocol.md");

  (* Vertex AI SDK *)
  ("vertex ai", "vertex-ai-sdk.md");
  ("vertexai", "vertex-ai-sdk.md");
  ("generativemodel", "vertex-ai-sdk.md");
  ("gemini-1.5", "vertex-ai-sdk.md");
  ("google cloud ai", "vertex-ai-sdk.md");
  ("generate_content", "vertex-ai-sdk.md");
  ("generatecontent", "vertex-ai-sdk.md");
  ("@google-cloud/vertexai", "vertex-ai-sdk.md");
]

(** Check if haystack contains needle (case-insensitive) *)
let contains_ci haystack needle =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let nlen = String.length n in
  let hlen = String.length h in
  if nlen > hlen then false
  else
    let rec check i =
      if i + nlen > hlen then false
      else if String.sub h i nlen = n then true
      else check (i + 1)
    in
    check 0

(** Base directory for few-shot examples *)
let fewshot_dir () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  home ^ "/me/data/few-shot-examples"

(** Load few-shot content from file *)
let load_fewshot filename =
  let path = fewshot_dir () ^ "/" ^ filename in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      Some s
    with _ -> None
  else None

(** Detect keywords and find matching few-shot file *)
let detect_fewshot prompt =
  List.find_map (fun (keyword, filename) ->
    if contains_ci prompt keyword then Some filename
    else None
  ) keyword_map

(** Main injection function - returns (enhanced_prompt, Some filename) or (prompt, None) *)
let inject prompt =
  match detect_fewshot prompt with
  | None -> (prompt, None)
  | Some filename ->
      match load_fewshot filename with
      | None -> (prompt, None)
      | Some content ->
          let enhanced = Printf.sprintf
            "Here are correct syntax examples for reference:\n\n%s\n\n---\nNow answer this question using the syntax shown above:\n\n%s"
            content prompt
          in
          (enhanced, Some filename)

(** Check if prompt already contains few-shot marker *)
let has_fewshot_marker prompt =
  contains_ci prompt "Here are correct syntax examples" ||
  contains_ci prompt "using the syntax shown above"

(** Safe injection - only if not already injected *)
let inject_safe prompt =
  if has_fewshot_marker prompt then (prompt, None)
  else inject prompt

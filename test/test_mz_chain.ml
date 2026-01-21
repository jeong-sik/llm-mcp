(** MZ Chain Test - 10 Step Chain *)

let run_eio f =
  Eio_main.run @@ fun env ->
  f env

let test_mz_chain () =
  run_eio @@ fun env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in

  Eio.Switch.run @@ fun sw ->
    let chain_json = `Assoc [
      ("id", `String "mz_chain_10_steps");
      ("nodes", `List [
        (* Step 1-4: Quotes *)
        `Assoc [
          ("id", `String "step1"); ("type", `String "llm");
          ("model", `String "ollama:qwen3:1.7b");
          ("prompt", `String "Give me a short inspiring quote about Passion.");
        ];
        `Assoc [
          ("id", `String "step2"); ("type", `String "llm");
          ("model", `String "ollama:qwen3:1.7b");
          ("prompt", `String "Give me a short inspiring quote about Persistence.");
        ];
        `Assoc [
          ("id", `String "step3"); ("type", `String "llm");
          ("model", `String "ollama:qwen3:1.7b");
          ("prompt", `String "Give me a short inspiring quote about Innovation.");
        ];
        `Assoc [
          ("id", `String "step4"); ("type", `String "llm");
          ("model", `String "ollama:qwen3:1.7b");
          ("prompt", `String "Give me a short inspiring quote about Rest.");
        ];

        (* Step 5: Poem *)
        `Assoc [
          ("id", `String "step5"); ("type", `String "llm");
          ("model", `String "ollama:llama3.2");
          ("prompt", `String "Write a short Korean poem combining these quotes:\n1. {{step1}}\n2. {{step2}}\n3. {{step3}}\n4. {{step4}}");
          ("input_mapping", `Assoc [
            ("step1", `String "step1"); ("step2", `String "step2");
            ("step3", `String "step3"); ("step4", `String "step4");
          ]);
        ];

        (* Step 6-9: Intermediate processing *)
        `Assoc [
          ("id", `String "step6"); ("type", `String "llm");
          ("model", `String "ollama:qwen3:1.7b");
          ("prompt", `String "Analyze the sentiment of this poem: {{step5}}");
          ("input_mapping", `Assoc [("step5", `String "step5")]);
        ];
        `Assoc [
          ("id", `String "step7"); ("type", `String "llm");
          ("model", `String "ollama:llama3.2");
          ("prompt", `String "Extract 3 keywords from this poem in Korean: {{step5}}");
          ("input_mapping", `Assoc [("step5", `String "step5")]);
        ];
        `Assoc [
          ("id", `String "step8"); ("type", `String "llm");
          ("model", `String "ollama:qwen3:1.7b");
          ("prompt", `String "Suggest a blog post title for this content: {{step5}}");
          ("input_mapping", `Assoc [("step5", `String "step5")]);
        ];
        `Assoc [
          ("id", `String "step9"); ("type", `String "llm");
          ("model", `String "ollama:llama3.2");
          ("prompt", `String "Summarize the poem in one sentence: {{step5}}");
          ("input_mapping", `Assoc [("step5", `String "step5")]);
        ];

        (* Step 10: MZ Style Conversion *)
        `Assoc [
          ("id", `String "step10"); ("type", `String "llm");
          ("model", `String "ollama:llama3.2");
          ("prompt", `String "이 시를 한국의 'MZ세대'가 쓰는 말투(완전 힙하고, 이모지 많이 쓰고, 줄임말 쓰고, 반말)로 바꿔줘. 내용은 유지하되 말투를 완전히 바꿔야 해:\n\n{{step5}}");
          ("input_mapping", `Assoc [("step5", `String "step5")]);
        ];
      ]);
      ("output", `String "step10");
      ("config", `Assoc [ ("timeout", `Int 300); ]);
    ] in

    Printf.printf "🚀 Starting 10-step MZ chain execution...\n%!";
    let result = Tools_eio.execute_chain ~sw ~proc_mgr ~clock ~chain_json ~trace:true ~timeout:300 in
    
    if result.returncode = 0 then
      Printf.printf "✅ Final MZ Result:\n%s\n" result.response
    else
      Printf.printf "❌ Chain failed:\n%s\n" result.response

let () =
  test_mz_chain ()

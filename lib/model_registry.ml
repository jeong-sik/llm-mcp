(** Model Category Registry

    Maps semantic categories (reasoning, coding, general, etc.)
    to concrete model names. Allows Chain DSL to use
    [LLM:reasoning 'prompt'] instead of [LLM:ollama:falcon-h1r 'prompt'].

    v2: Dynamic availability filtering via [set_available_models]. *)

type category =
  | Reasoning
  | Coding
  | General
  | Multimodal
  | Embedding

type model_info = {
  name : string;
  category : category;
  backend : string;
  context_length : int;
  thinking_style : bool;
}

let category_to_string = function
  | Reasoning -> "reasoning"
  | Coding -> "coding"
  | General -> "general"
  | Multimodal -> "multimodal"
  | Embedding -> "embedding"

let category_of_string s =
  match String.lowercase_ascii s with
  | "reasoning" -> Some Reasoning
  | "coding" | "code" -> Some Coding
  | "general" -> Some General
  | "multimodal" | "vision" -> Some Multimodal
  | "embedding" | "embed" -> Some Embedding
  | _ -> None

(** Static registry of known models and their categories.
    Ordered by preference within each category (first = default).
    
    Benchmarks (reference):
    - AIME 2024: Math reasoning
    - SWE-bench: Software engineering
    - HumanEval: Code generation
    - MMLU: General knowledge *)
let registry : model_info list = [
  (* ═══════════════════════════════════════════════════════════════
     Reasoning - Math, logic, planning
     ═══════════════════════════════════════════════════════════════ *)
  { name = "hf.co/tiiuae/Falcon-H1R-7B-GGUF:Q8_0";
    category = Reasoning; backend = "ollama";
    context_length = 256000; thinking_style = true };  (* AIME 83.1% *)
  { name = "hf.co/unsloth/DeepSeek-R1-0528-Qwen3-8B-GGUF:Q4_K_M";
    category = Reasoning; backend = "ollama";
    context_length = 128000; thinking_style = true };
  { name = "deepseek-r1:32b";
    category = Reasoning; backend = "ollama";
    context_length = 128000; thinking_style = true };  (* AIME 79.8% *)
  { name = "deepseek-r1:14b";
    category = Reasoning; backend = "ollama";
    context_length = 128000; thinking_style = true };
  { name = "deepseek-r1:8b";
    category = Reasoning; backend = "ollama";
    context_length = 128000; thinking_style = true };  (* AIME 70% *)
  { name = "deepseek-r1:1.5b";
    category = Reasoning; backend = "ollama";
    context_length = 16384; thinking_style = true };
  { name = "qwq:32b";
    category = Reasoning; backend = "ollama";
    context_length = 32768; thinking_style = true };  (* AIME 50% *)

  (* ═══════════════════════════════════════════════════════════════
     Coding - Code generation, debugging, refactoring
     ═══════════════════════════════════════════════════════════════ *)
  { name = "devstral";
    category = Coding; backend = "ollama";
    context_length = 128000; thinking_style = false };  (* SWE 72.2% *)
  { name = "qwen3-coder:30b";
    category = Coding; backend = "ollama";
    context_length = 262144; thinking_style = true };  (* SWE 69.6% *)
  { name = "qwen2.5-coder:32b";
    category = Coding; backend = "ollama";
    context_length = 128000; thinking_style = false };  (* HumanEval 92.7% *)
  { name = "qwen2.5-coder:14b";
    category = Coding; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "qwen2.5-coder:7b";
    category = Coding; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "codestral";
    category = Coding; backend = "ollama";
    context_length = 32000; thinking_style = false };
  { name = "starcoder2:15b";
    category = Coding; backend = "ollama";
    context_length = 16384; thinking_style = false };

  (* ═══════════════════════════════════════════════════════════════
     General - Chat, instruction following, knowledge
     ═══════════════════════════════════════════════════════════════ *)
  { name = "glm-4.7-flash:latest";
    category = General; backend = "ollama";
    context_length = 131072; thinking_style = false };  (* SWE 73.8% *)
  { name = "qwen2.5:32b";
    category = General; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "qwen2.5:14b";
    category = General; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "qwen2.5:7b";
    category = General; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "qwen3:1.7b";
    category = General; backend = "ollama";
    context_length = 32768; thinking_style = true };
  { name = "llama3.3:70b";
    category = General; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "llama3.2:3b";
    category = General; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "gemma2:27b";
    category = General; backend = "ollama";
    context_length = 8192; thinking_style = false };
  { name = "gemma2:9b";
    category = General; backend = "ollama";
    context_length = 8192; thinking_style = false };
  { name = "phi3:14b";
    category = General; backend = "ollama";
    context_length = 128000; thinking_style = false };
  { name = "mistral:7b";
    category = General; backend = "ollama";
    context_length = 32768; thinking_style = false };

  (* ═══════════════════════════════════════════════════════════════
     Multimodal - Vision + text
     ═══════════════════════════════════════════════════════════════ *)
  { name = "gemini";
    category = Multimodal; backend = "gemini";
    context_length = 1048576; thinking_style = false };
  { name = "llava:34b";
    category = Multimodal; backend = "ollama";
    context_length = 4096; thinking_style = false };
  { name = "llava:13b";
    category = Multimodal; backend = "ollama";
    context_length = 4096; thinking_style = false };
  { name = "llava:7b";
    category = Multimodal; backend = "ollama";
    context_length = 4096; thinking_style = false };
  { name = "qwen2-vl:7b";
    category = Multimodal; backend = "ollama";
    context_length = 32768; thinking_style = false };

  (* ═══════════════════════════════════════════════════════════════
     Embedding - Vector embeddings for RAG
     ═══════════════════════════════════════════════════════════════ *)
  { name = "nomic-embed-text";
    category = Embedding; backend = "ollama";
    context_length = 8192; thinking_style = false };
  { name = "bge-m3";
    category = Embedding; backend = "ollama";
    context_length = 8192; thinking_style = false };
  { name = "mxbai-embed-large";
    category = Embedding; backend = "ollama";
    context_length = 512; thinking_style = false };
  { name = "snowflake-arctic-embed";
    category = Embedding; backend = "ollama";
    context_length = 512; thinking_style = false };
]

(* ──────────────────────────────────────────────────────────────
   Dynamic availability (v2)
   Caller sets available models via set_available_models.
   resolve() then filters by availability with fallback chain.
   ────────────────────────────────────────────────────────────── *)

(** Mutable set of available model names from ollama list.
    Empty = no filtering (all models considered available). *)
let available_models : (string, unit) Hashtbl.t = Hashtbl.create 32

let set_available_models (names : string list) =
  Hashtbl.clear available_models;
  List.iter (fun name -> Hashtbl.add available_models name ()) names

let clear_available_models () =
  Hashtbl.clear available_models

let is_available (m : model_info) : bool =
  (* Non-ollama backends (gemini, claude) are always available *)
  if m.backend <> "ollama" then true
  (* If no models set, assume all available (backwards compat) *)
  else if Hashtbl.length available_models = 0 then true
  (* Check if model name is in available set *)
  else Hashtbl.mem available_models m.name

let all () = registry

let by_category cat =
  List.filter (fun m -> m.category = cat) registry

(** Resolve a category string to a concrete "backend:model" name.
    Filters by availability and falls back through the list.
    Returns [None] if the input is not a recognized category
    (i.e. it's already a concrete model name and should pass through). *)
let resolve s =
  match category_of_string s with
  | None -> None
  | Some cat ->
    (* Filter by availability, preserving preference order *)
    let candidates =
      by_category cat
      |> List.filter is_available
    in
    match candidates with
    | [] ->
      (* Fallback: return first in registry even if unavailable *)
      (match by_category cat with
       | [] -> None
       | best :: _ ->
         if best.backend = "ollama" then
           Some (Printf.sprintf "ollama:%s" best.name)
         else
           Some best.backend)
    | best :: _ ->
      if best.backend = "ollama" then
        Some (Printf.sprintf "ollama:%s" best.name)
      else
        Some best.backend

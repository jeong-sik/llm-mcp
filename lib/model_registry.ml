(** Model Category Registry

    Maps semantic categories (reasoning, coding, general, etc.)
    to concrete model names. Allows Chain DSL to use
    [LLM:reasoning 'prompt'] instead of [LLM:ollama:falcon-h1r 'prompt'].

    v1: hardcoded registry. v2 will add dynamic ollama list filtering. *)

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
    Ordered by preference within each category (first = default). *)
let registry : model_info list = [
  (* Reasoning *)
  { name = "hf.co/tiiuae/Falcon-H1R-7B-GGUF:Q8_0";
    category = Reasoning; backend = "ollama";
    context_length = 32768; thinking_style = true };
  { name = "hf.co/unsloth/DeepSeek-R1-0528-Qwen3-8B-GGUF:Q4_K_M";
    category = Reasoning; backend = "ollama";
    context_length = 32768; thinking_style = true };
  { name = "deepseek-r1:1.5b";
    category = Reasoning; backend = "ollama";
    context_length = 16384; thinking_style = true };

  (* Coding *)
  { name = "qwen3-coder:30b";
    category = Coding; backend = "ollama";
    context_length = 262144; thinking_style = true };

  (* General *)
  { name = "qwen3:1.7b";
    category = General; backend = "ollama";
    context_length = 32768; thinking_style = true };
  { name = "glm-4.7-flash:latest";
    category = General; backend = "ollama";
    context_length = 131072; thinking_style = false };

  (* Multimodal *)
  { name = "gemini";
    category = Multimodal; backend = "gemini";
    context_length = 1048576; thinking_style = false };

  (* Embedding *)
  { name = "nomic-embed-text";
    category = Embedding; backend = "ollama";
    context_length = 8192; thinking_style = false };
]

let all () = registry

let by_category cat =
  List.filter (fun m -> m.category = cat) registry

(** Resolve a category string to a concrete "backend:model" name.
    Returns [None] if the input is not a recognized category
    (i.e. it's already a concrete model name and should pass through). *)
let resolve s =
  match category_of_string s with
  | None -> None
  | Some cat ->
    match by_category cat with
    | [] -> None
    | best :: _ ->
      (* Prepend backend prefix for ollama models *)
      if best.backend = "ollama" then
        Some (Printf.sprintf "ollama:%s" best.name)
      else
        Some best.backend

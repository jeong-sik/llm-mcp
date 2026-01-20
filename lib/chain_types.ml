(** Chain Types - Core type definitions for Chain DSL

    This module defines the fundamental types for the Chain Engine:
    - node_type: The 11 supported node types (Llm, Tool, Pipeline, etc.)
    - node: A single execution unit with id and type
    - chain: A complete chain definition with nodes and config
    - chain_result: The result of chain execution

    Category theory concepts:
    - Functor (map): Transform node outputs
    - Monad (bind): Sequential dependencies
    - Monoid (merge): Combine parallel results
*)

(** Configuration for chain execution *)
type chain_config = {
  max_depth : int;        (** Maximum recursion depth for subgraphs *)
  max_concurrency : int;  (** Max parallel executions per model *)
  timeout : int;          (** Default timeout in seconds *)
  trace : bool;           (** Enable execution tracing *)
}
[@@deriving yojson]

(** Default configuration values *)
let default_config = {
  max_depth = 4;
  max_concurrency = 3;
  timeout = 300;
  trace = false;
}

(** Merge strategy for combining parallel results *)
type merge_strategy =
  | First           (** Take first successful result *)
  | Last            (** Take last successful result *)
  | Concat          (** Concatenate all results *)
  | WeightedAvg     (** Weighted average (for numeric) *)
  | Custom of string  (** Custom merge function name *)
[@@deriving yojson]

(** The 11 supported node types *)
type node_type =
  | Llm of {
      model : string;     (** Model name: gemini, claude, codex, ollama:* *)
      prompt : string;    (** Prompt template with {{var}} placeholders *)
      timeout : int option;
    }
  | Tool of {
      name : string;      (** MCP tool name *)
      args : Yojson.Safe.t;  (** Tool arguments as JSON *)
    }
  | Pipeline of node list    (** Sequential execution: a >> b >> c *)
  | Fanout of node list      (** Parallel execution: a ||| b ||| c *)
  | Quorum of {
      required : int;        (** Minimum successes needed *)
      nodes : node list;
    }
  | Gate of {
      condition : string;    (** Condition expression *)
      then_node : node;      (** Execute if true *)
      else_node : node option;  (** Execute if false *)
    }
  | Subgraph of chain        (** Inline nested chain *)
  | ChainRef of string       (** Reference to registered chain *)
  | Map of {
      func : string;         (** Transformation function name *)
      inner : node;          (** Node to transform output of *)
    }
  | Bind of {
      func : string;         (** Dynamic routing function *)
      inner : node;          (** Node to get input from *)
    }
  | Merge of {
      strategy : merge_strategy;
      nodes : node list;
    }
[@@deriving yojson]

(** A single execution node *)
and node = {
  id : string;                           (** Unique node identifier *)
  node_type : node_type;                 (** The node's type and config *)
  input_mapping : (string * string) list;  (** Map: param -> {{node.output}} *)
}
[@@deriving yojson]

(** A complete chain definition *)
and chain = {
  id : string;           (** Chain identifier *)
  nodes : node list;     (** List of nodes in the chain *)
  output : string;       (** ID of the output node *)
  config : chain_config; (** Execution configuration *)
}
[@@deriving yojson]

(** A single trace entry for debugging *)
type trace_entry = {
  node_id : string;
  node_type_name : string;
  start_time : float;
  end_time : float;
  status : [ `Success | `Failure | `Skipped ];
  output_preview : string option;
  error : string option;
}
[@@deriving yojson]

(** Token usage tracking *)
type token_usage = {
  prompt_tokens : int;
  completion_tokens : int;
  total_tokens : int;
  estimated_cost_usd : float;
}
[@@deriving yojson]

let empty_token_usage = {
  prompt_tokens = 0;
  completion_tokens = 0;
  total_tokens = 0;
  estimated_cost_usd = 0.0;
}

(** Result of chain execution *)
type chain_result = {
  chain_id : string;
  output : string;
  success : bool;
  trace : trace_entry list;
  token_usage : token_usage;
  duration_ms : int;
  metadata : (string * string) list;
}
[@@deriving yojson]

(** Execution plan produced by compiler *)
type execution_plan = {
  chain : chain;
  execution_order : string list;  (** Topologically sorted node IDs *)
  parallel_groups : string list list;  (** Groups that can run in parallel *)
  depth : int;  (** Maximum nesting depth *)
}
[@@deriving yojson]

(** Helper: Get node type name as string *)
let node_type_name = function
  | Llm _ -> "llm"
  | Tool _ -> "tool"
  | Pipeline _ -> "pipeline"
  | Fanout _ -> "fanout"
  | Quorum _ -> "quorum"
  | Gate _ -> "gate"
  | Subgraph _ -> "subgraph"
  | ChainRef _ -> "chain_ref"
  | Map _ -> "map"
  | Bind _ -> "bind"
  | Merge _ -> "merge"

(** Helper: Create a simple LLM node *)
let make_llm_node ~id ~model ~prompt ?timeout () =
  { id; node_type = Llm { model; prompt; timeout }; input_mapping = [] }

(** Helper: Create a simple tool node *)
let make_tool_node ~id ~name ~args =
  { id; node_type = Tool { name; args }; input_mapping = [] }

(** Helper: Create a pipeline from nodes *)
let make_pipeline ~id nodes =
  { id; node_type = Pipeline nodes; input_mapping = [] }

(** Helper: Create a fanout from nodes *)
let make_fanout ~id nodes =
  { id; node_type = Fanout nodes; input_mapping = [] }

(** Helper: Create a quorum node *)
let make_quorum ~id ~required nodes =
  { id; node_type = Quorum { required; nodes }; input_mapping = [] }

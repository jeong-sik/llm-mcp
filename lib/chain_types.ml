(** Chain Types - Core type definitions for Chain DSL

    This module defines the fundamental types for the Chain Engine:
    - node_type: The 12 supported node types (Llm, Tool, Pipeline, Threshold, etc.)
    - node: A single execution unit with id and type
    - chain: A complete chain definition with nodes and config
    - chain_result: The result of chain execution

    Category theory concepts:
    - Functor (map): Transform node outputs
    - Monad (bind): Sequential dependencies
    - Monoid (merge): Combine parallel results
*)

(** Mermaid diagram direction for visualization *)
type direction =
  | LR  (** Left to Right - horizontal flow *)
  | RL  (** Right to Left - reverse horizontal *)
  | TB  (** Top to Bottom - vertical/hierarchical *)
  | BT  (** Bottom to Top - reverse vertical *)
[@@deriving yojson]

let direction_to_string = function
  | LR -> "LR" | RL -> "RL" | TB -> "TB" | BT -> "BT"

let direction_of_string = function
  | "LR" -> LR | "RL" -> RL
  | "TB" | "TD" -> TB  (* TD is alias for TB *)
  | "BT" -> BT
  | _ -> LR  (* default *)

(** Configuration for chain execution *)
type chain_config = {
  max_depth : int;        (** Maximum recursion depth for subgraphs *)
  max_concurrency : int;  (** Max parallel executions per model *)
  timeout : int;          (** Default timeout in seconds *)
  trace : bool;           (** Enable execution tracing *)
  direction : direction;  (** Mermaid diagram direction for WYSIWYE *)
}
[@@deriving yojson]

(** Default configuration values *)
let default_config = {
  max_depth = 4;
  max_concurrency = 3;
  timeout = 300;
  trace = false;
  direction = LR;
}

(** Merge strategy for combining parallel results *)
type merge_strategy =
  | First           (** Take first successful result *)
  | Last            (** Take last successful result *)
  | Concat          (** Concatenate all results *)
  | WeightedAvg     (** Weighted average (for numeric) *)
  | Custom of string  (** Custom merge function name *)
[@@deriving yojson]

(** Threshold comparison operators *)
type threshold_op =
  | Gt   (** > greater than *)
  | Gte  (** >= greater than or equal *)
  | Lt   (** < less than *)
  | Lte  (** <= less than or equal *)
  | Eq   (** = equal *)
  | Neq  (** != not equal *)
[@@deriving yojson]

(** Selection strategy for Evaluator node *)
type select_strategy =
  | Best              (** Select highest score *)
  | Worst             (** Select lowest score (for debugging) *)
  | AboveThreshold of float  (** First candidate above threshold *)
  | WeightedRandom    (** Random selection weighted by scores *)
[@@deriving yojson]

(** The 13 supported node types *)
type node_type =
  | Llm of {
      model : string;     (** Model name: gemini, claude, codex, ollama:* *)
      prompt : string;    (** Prompt template with {{var}} placeholders *)
      timeout : int option;
      tools : Yojson.Safe.t option;  (** MCP tools for function calling (Ollama) *)
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
  | Threshold of {
      metric : string;           (** Metric name: "confidence", "coverage", "latency", "score" *)
      operator : threshold_op;   (** Comparison operator *)
      value : float;             (** Threshold value *)
      input_node : node;         (** Node to get value from *)
      on_pass : node option;     (** Execute if condition passes *)
      on_fail : node option;     (** Execute if condition fails *)
    }
  | GoalDriven of {
      goal_metric : string;        (** Target metric: "coverage", "score", "success_rate" *)
      goal_operator : threshold_op; (** Comparison operator for goal condition *)
      goal_value : float;          (** Target value to achieve *)
      action_node : node;          (** Node to execute repeatedly *)
      measure_func : string;       (** Metric measurement function: "exec_test", "call_api", "parse_json" *)
      max_iterations : int;        (** Maximum iteration count *)
      strategy_hints : (string * string) list;  (** Strategy hints: [("below_50", "fast"), ("above_50", "accurate")] *)
      conversational : bool;       (** Enable conversational mode with context accumulation *)
      relay_models : string list;  (** Models to rotate through: ["gemini"; "claude"; "codex"] *)
    }
  | Evaluator of {
      candidates : node list;      (** Candidate nodes to evaluate *)
      scoring_func : string;       (** Scoring function: "llm_judge", "regex_match", "json_schema", "custom" *)
      scoring_prompt : string option;  (** Prompt for LLM judge scoring *)
      select_strategy : select_strategy;  (** Selection strategy *)
      min_score : float option;    (** Minimum score threshold (fails if none meet it) *)
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
  | Threshold _ -> "threshold"
  | GoalDriven _ -> "goal_driven"
  | Evaluator _ -> "evaluator"

(** Helper: Create a simple LLM node *)
let make_llm_node ~id ~model ~prompt ?timeout ?tools () =
  { id; node_type = Llm { model; prompt; timeout; tools }; input_mapping = [] }

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

(** Helper: Create a threshold node *)
let make_threshold ~id ~metric ~operator ~value ~input_node ?on_pass ?on_fail () =
  { id; node_type = Threshold { metric; operator; value; input_node; on_pass; on_fail }; input_mapping = [] }

(** Helper: Create a goal-driven iterative node *)
let make_goal_driven ~id ~goal_metric ~goal_operator ~goal_value
    ~action_node ~measure_func ~max_iterations ?(strategy_hints=[])
    ?(conversational=false) ?(relay_models=[]) () =
  { id; node_type = GoalDriven {
      goal_metric; goal_operator; goal_value;
      action_node; measure_func; max_iterations; strategy_hints;
      conversational; relay_models
    }; input_mapping = [] }

(** Helper: Create an evaluator node *)
let make_evaluator ~id ~candidates ~scoring_func ?scoring_prompt ~select_strategy ?min_score () =
  { id; node_type = Evaluator { candidates; scoring_func; scoring_prompt; select_strategy; min_score }; input_mapping = [] }

(** {1 Batch Execution Types - Phase 5} *)

(** Batch priority levels *)
type batch_priority =
  | High
  | Normal
  | Low
[@@deriving yojson]

(** Retry configuration for batch execution *)
type retry_config = {
  max_retries: int;           (** Maximum retry attempts *)
  initial_delay_ms: int;      (** Initial delay before first retry *)
  backoff_multiplier: float;  (** Exponential backoff multiplier *)
  max_delay_ms: int;          (** Maximum delay between retries *)
}
[@@deriving yojson]

(** Default retry configuration *)
let default_retry_config = {
  max_retries = 3;
  initial_delay_ms = 1000;
  backoff_multiplier = 2.0;
  max_delay_ms = 30000;
}

(** Batch execution configuration *)
type batch_config = {
  batch_max_concurrent: int;  (** Maximum concurrent chain executions *)
  rate_limit_per_min: int;    (** Rate limit per minute per model *)
  retry_policy: retry_config; (** Retry configuration *)
  priority: batch_priority;   (** Batch priority level *)
}
[@@deriving yojson]

(** Default batch configuration *)
let default_batch_config = {
  batch_max_concurrent = 5;
  rate_limit_per_min = 60;
  retry_policy = default_retry_config;
  priority = Normal;
}

(** Batch execution statistics *)
type batch_stats = {
  total_chains: int;          (** Total chains in batch *)
  completed: int;             (** Successfully completed chains *)
  failed: int;                (** Failed chains *)
  total_duration_ms: int;     (** Total execution time *)
  total_tokens: Chain_category.token_usage;  (** Aggregated token usage *)
  avg_duration_ms: float;     (** Average chain duration *)
}
[@@deriving yojson]

(** Result of batch execution *)
type batch_result = {
  batch_id: string;                       (** Unique batch identifier *)
  results: (string * chain_result) list;  (** Chain ID to result mapping *)
  stats: batch_stats;                     (** Execution statistics *)
  failed_chains: (string * string) list;  (** Chain ID to error mapping *)
}
[@@deriving yojson]

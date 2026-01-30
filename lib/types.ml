(** Tool types and JSON schemas for MAGI Trinity

    This module re-exports all domain-specific type modules for
    backward compatibility. All types are available as before:
    - Types.tool_args, Types.tool_result, etc.

    The actual implementations are split into:
    - Types_core: Foundation types (thinking_level, tool_result, etc.)
    - Types_glm: GLM function calling types
    - Types_gemini: Gemini error classification
    - Types_llm: tool_args discriminated union
    - Types_compact: Compact protocol (response_format, encoding)
    - Types_schema: MCP tool schemas *)

(* Foundation types *)
include Types_core

(* GLM function calling *)
include Types_glm

(* Gemini error classification *)
include Types_gemini

(* Tool arguments discriminated union *)
include Types_llm

(* Compact protocol *)
include Types_compact

(* Tool schemas *)
include Types_schema

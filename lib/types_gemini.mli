(** Gemini Error Classification *)

type gemini_error =
  | FunctionCallSyncError
  | ContextTooLongError
  | RateLimitError
  | AuthenticationError
  | UnknownGeminiError of string

val str_contains : substring:string -> string -> bool
val classify_gemini_error : string -> gemini_error option
val string_of_gemini_error : gemini_error -> string
val is_recoverable_gemini_error : gemini_error -> bool

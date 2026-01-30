(** Gemini Error Classification

    Specialized error handling for Gemini API responses.
    Classifies errors for retry strategy and user feedback.

    NOTE: "FunctionCallSyncError" is a particularly tricky Gemini bug
    that occurs when conversation history gets corrupted during
    context compression or long sessions. *)

(** Gemini error types *)
type gemini_error =
  | FunctionCallSyncError   (** "number of function response parts" - recoverable with retry *)
  | ContextTooLongError     (** Context exceeds limit - need to truncate prompt *)
  | RateLimitError          (** API rate limit - need backoff *)
  | AuthenticationError     (** Invalid API key or token expired *)
  | UnknownGeminiError of string  (** Unclassified error *)

(** Check if string contains substring (using Str module) *)
let str_contains ~substring s =
  try
    let _ = Str.search_forward (Str.regexp_string substring) s 0 in
    true
  with Not_found -> false

(** Classify Gemini API error from response text *)
let classify_gemini_error response =
  let s = String.lowercase_ascii response in
  if String.length s > 0 then begin
    (* Function call sync error - most common, recoverable *)
    if str_contains ~substring:"function response parts" s ||
       str_contains ~substring:"function call parts" s ||
       (str_contains ~substring:"invalid_argument" s && str_contains ~substring:"function" s)
    then Some FunctionCallSyncError
    (* Context too long *)
    else if str_contains ~substring:"context" s &&
            (str_contains ~substring:"too long" s || str_contains ~substring:"exceeds" s)
    then Some ContextTooLongError
    (* Rate limit *)
    else if str_contains ~substring:"rate limit" s ||
            str_contains ~substring:"quota" s ||
            str_contains ~substring:"resource_exhausted" s
    then Some RateLimitError
    (* Authentication *)
    else if str_contains ~substring:"authentication" s ||
            str_contains ~substring:"unauthorized" s ||
            str_contains ~substring:"api key" s
    then Some AuthenticationError
    (* Check for generic HTTP error indicators - must look like an error message *)
    (* More specific patterns to avoid false positives on normal responses *)
    else if str_contains ~substring:"error:" s && str_contains ~substring:"400" s
    then Some (UnknownGeminiError "HTTP 400 Bad Request")
    else if str_contains ~substring:"error 400" s
    then Some (UnknownGeminiError "HTTP 400 Bad Request")
    else if str_contains ~substring:"400 bad request" s
    then Some (UnknownGeminiError "HTTP 400 Bad Request")
    else None
  end
  else None

(** Convert gemini_error to human-readable string *)
let string_of_gemini_error = function
  | FunctionCallSyncError -> "FunctionCallSyncError"
  | ContextTooLongError -> "ContextTooLongError"
  | RateLimitError -> "RateLimitError"
  | AuthenticationError -> "AuthenticationError"
  | UnknownGeminiError msg -> Printf.sprintf "UnknownGeminiError(%s)" msg

(** Check if error is recoverable with retry *)
let is_recoverable_gemini_error = function
  | FunctionCallSyncError -> true   (* Retry with fresh session *)
  | RateLimitError -> true          (* Retry after backoff *)
  | ContextTooLongError -> false    (* Need user intervention *)
  | AuthenticationError -> false    (* Need user intervention *)
  | UnknownGeminiError _ -> false   (* Don't know if safe to retry *)

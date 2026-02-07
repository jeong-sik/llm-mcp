(** vision_gate.ml - Local vision gate using Ollama vision models
    OCaml replacement for scripts/vision_gate_local_llm.py *)

open Cmdliner

let ollama_chat_url = "http://127.0.0.1:11434/api/chat"
let default_model = "llava:13b"
let blocked_issues = [ "blur_overlay"; "dimmed_overlay"; "annotations" ]
let stddev_clear_min = 0.15

let system_prompt =
  {|You are a strict reference gate for Figma exports.
Decide whether the image is a clean UI reference suitable for pixel matching.

Reject if you see any of:
- blur/dim overlay
- disabled background modal overlay
- annotation numbers/arrows/callouts
- partial modal cut-off or obvious obstruction

Important clarifications:
- Normal icons, alerts, error states, and badges are NOT obstructions.
- Do NOT flag obstruction unless parts of the UI are actually covered.
- Do NOT flag blur unless text/UI is visibly blurred or washed out by an overlay.

Return JSON only:
{
  "is_clean_reference": boolean,
  "issues": string[],
  "confidence": number,
  "notes": string
}

Valid issue labels:
- blur_overlay
- dimmed_overlay
- annotations
- obstruction
- partial_capture
- other|}

let user_prompt = "Gate this image for reference quality.\nBe conservative: if unsure, reject."

type gate_result = {
  is_clean_reference : bool;
  issues : string list;
  confidence : float;
  notes : string;
}

let run_capture_stdout prog args =
  let argv = Array.of_list (prog :: args) in
  let env = Unix.environment () in
  let devnull_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o644 in
  let devnull_err = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o644 in
  let stdout_r, stdout_w = Unix.pipe () in
  let pid = Unix.create_process_env prog argv env devnull_in stdout_w devnull_err in
  Unix.close devnull_in;
  Unix.close devnull_err;
  Unix.close stdout_w;
  let output =
    let ic = Unix.in_channel_of_descr stdout_r in
    Fun.protect
      ~finally:(fun () -> (try close_in ic with _ -> ()))
      (fun () -> In_channel.input_all ic)
  in
  let _pid, _status = Unix.waitpid [] pid in
  output

let encode_image_b64 path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let data = really_input_string ic n in
  close_in ic;
  Base64.encode_string data

let contains_substr haystack needle =
  try
    let _ = Re.Pcre.exec ~rex:(Re.Pcre.regexp (Re.Pcre.quote needle)) haystack in
    true
  with Not_found -> false

let canonicalize_issue text =
  let t = String.lowercase_ascii (String.trim text) in
  if List.exists (fun k -> contains_substr t k) [ "blur"; "blurry" ] then "blur_overlay"
  else if List.exists (fun k -> contains_substr t k) [ "dim"; "overlay"; "disabled" ] then
    "dimmed_overlay"
  else if List.exists (fun k -> contains_substr t k) [ "annot"; "callout"; "arrow"; "badge"; "number" ]
  then "annotations"
  else if List.exists (fun k -> contains_substr t k) [ "obstruct"; "block"; "cover" ] then
    "obstruction"
  else if List.exists (fun k -> contains_substr t k) [ "partial"; "cropped"; "cut"; "off" ] then
    "partial_capture"
  else "other"

let parse_confidence value =
  try
    match value with
    | `Float f -> min 1.0 (max 0.0 (if f > 1.0 && f <= 100.0 then f /. 100.0 else f))
    | `Int i ->
        let f = float_of_int i in
        min 1.0 (max 0.0 (if f > 1.0 && f <= 100.0 then f /. 100.0 else f))
    | `String s -> (
        match String.lowercase_ascii (String.trim s) with
        | "low" -> 0.3
        | "medium" | "med" -> 0.6
        | "high" -> 0.85
        | _ -> 0.0)
    | _ -> 0.0
  with _ -> 0.0

let parse_result json =
  let open Yojson.Safe.Util in
  let issues =
    try
      json |> member "issues" |> to_list
      |> List.map (fun x -> canonicalize_issue (to_string x))
      |> List.sort_uniq String.compare
    with _ -> []
  in
  let has_blocked = List.exists (fun i -> List.mem i blocked_issues) issues in
  let is_clean =
    try
      let v = json |> member "is_clean_reference" |> to_bool in
      v && not has_blocked
    with _ -> not has_blocked
  in
  let confidence = parse_confidence (member "confidence" json) in
  let notes = try json |> member "notes" |> to_string with _ -> "" in
  { is_clean_reference = is_clean; issues; confidence; notes }

let magick_gray_stats path =
  try
    let line =
      run_capture_stdout "magick"
        [
          path;
          "-colorspace";
          "Gray";
          "-format";
          "%[fx:mean] %[fx:standard_deviation]";
          "info:";
        ]
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> List.find (fun s -> s <> "")
    in
    match String.split_on_char ' ' (String.trim line) with
    | [ mean_s; std_s ] -> Some (float_of_string mean_s, float_of_string std_s)
    | _ -> None
  with _ -> None

let reconcile_with_stats result stats =
  match stats with
  | None -> result
  | Some (_mean, stddev) ->
      if stddev >= stddev_clear_min then
        let filtered =
          List.filter (fun i -> i <> "blur_overlay" && i <> "dimmed_overlay") result.issues
        in
        let notes =
          if List.length filtered <> List.length result.issues then
            result.notes ^ Printf.sprintf " [stats] stddev=%.3f -> overlay flags downgraded." stddev
          else result.notes
        in
        let has_blocked = List.exists (fun i -> List.mem i blocked_issues) filtered in
        { result with issues = filtered; notes; is_clean_reference = not has_blocked }
      else result

let call_ollama model image_b64 =
  let payload =
    `Assoc
      [
        ("model", `String model);
        ("stream", `Bool false);
        ("format", `String "json");
        ( "messages",
          `List
            [
              `Assoc [ ("role", `String "system"); ("content", `String system_prompt) ];
              `Assoc
                [
                  ("role", `String "user");
                  ("content", `String user_prompt);
                  ("images", `List [ `String image_b64 ]);
                ];
            ] );
      ]
  in
  let body = Yojson.Safe.to_string payload in
  run_capture_stdout "curl"
    [
      "-s";
      "-X";
      "POST";
      ollama_chat_url;
      "-H";
      "Content-Type: application/json";
      "-d";
      body;
    ]

let extract_json text =
  let re = Re.Pcre.regexp "\\{[\\s\\S]*\\}" in
  match Re.exec_opt re text with
  | Some g -> Re.Group.get g 0
  | None -> failwith "No JSON object found"

let run image_path model json_out =
  if not (Sys.file_exists image_path) then (
    Printf.eprintf "[vision-gate] missing image: %s\n" image_path;
    exit 2);

  let image_b64 = encode_image_b64 image_path in
  let raw_response = call_ollama model image_b64 in

  let content =
    try
      let json = Yojson.Safe.from_string raw_response in
      Yojson.Safe.Util.(json |> member "message" |> member "content" |> to_string)
    with _ -> raw_response
  in

  let parsed_json =
    try Yojson.Safe.from_string (extract_json content) with e ->
      Printf.eprintf "[vision-gate] parse failed: %s\n" (Printexc.to_string e);
      exit 4
  in

  let result = parse_result parsed_json in
  let result = reconcile_with_stats result (magick_gray_stats image_path) in

  let passed = result.is_clean_reference && not (List.exists (fun i -> List.mem i blocked_issues) result.issues) in

  (* Output JSON if requested *)
  (match json_out with
  | Some path ->
      let out =
        `Assoc
          [
            ("image", `String image_path);
            ("model", `String model);
            ("passed", `Bool passed);
            ("is_clean_reference", `Bool result.is_clean_reference);
            ("issues", `List (List.map (fun s -> `String s) result.issues));
            ("confidence", `Float result.confidence);
            ("notes", `String result.notes);
          ]
      in
      let oc = open_out path in
      output_string oc (Yojson.Safe.pretty_to_string out);
      close_out oc
  | None -> ());

  let verdict = if passed then "PASS" else "FAIL" in
  let issues_str = if result.issues = [] then "-" else String.concat ", " result.issues in
  Printf.printf "[vision-gate] %s :: %s\n" verdict (Filename.basename image_path);
  Printf.printf "[vision-gate] issues=%s confidence=%.2f\n" issues_str result.confidence;
  if result.notes <> "" then Printf.printf "[vision-gate] notes=%s\n" result.notes;

  if passed then () else exit 1

let image_arg =
  let doc = "Path to the reference image" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE" ~doc)

let model_arg =
  let doc = "Ollama model" in
  Arg.(value & opt string default_model & info [ "model"; "m" ] ~docv:"MODEL" ~doc)

let json_out_arg =
  let doc = "Optional path to write parsed JSON result" in
  Arg.(value & opt (some string) None & info [ "json-out" ] ~docv:"PATH" ~doc)

let cmd =
  let doc = "Local vision gate via Ollama vision model" in
  let info = Cmd.info "vision-gate" ~doc in
  Cmd.v info Term.(const run $ image_arg $ model_arg $ json_out_arg)

let () = exit (Cmd.eval cmd)

(** skill_activator - Skill Activator for Forced Evaluation Hook

    OCaml port of .claude/hooks/skill-activator.py
    Matches user prompts against skill-rules.json triggers.

    Usage:
      skill_activator --query "query" [--category "cat"] [--json]
*)

open Cmdliner

(** Get ME_ROOT from Common module *)
let me_root () = Common.me_root

(** Rules file path *)
let rules_file () =
  Filename.concat (me_root ()) ".claude/skills/skill-rules.json"

(** Load skill rules from JSON file *)
let load_skill_rules () : Yojson.Safe.t option =
  let path = rules_file () in
  if Sys.file_exists path then
    try
      Some (Yojson.Safe.from_file path)
    with Yojson.Json_error _ | Sys_error _ ->
      prerr_endline "[WARN] Failed to load skill-rules.json";
      None
  else
    None

(** String contains substring (case-insensitive) *)
let contains_ci haystack needle =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let nlen = String.length n in
  let hlen = String.length h in
  if nlen > hlen then false
  else
    let rec check i =
      if i > hlen - nlen then false
      else if String.sub h i nlen = n then true
      else check (i + 1)
    in
    check 0

(** Check if any keyword matches in prompt *)
let match_keywords prompt keywords =
  List.exists (contains_ci prompt) keywords

(** Check if any regex pattern matches in prompt *)
let match_intent_patterns prompt patterns =
  List.exists (fun pattern ->
    try
      let re = Re.Pcre.regexp ~flags:[`CASELESS] pattern in
      Re.execp re prompt
    with _ -> false  (* Skip invalid regex - Re.Pcre can raise various exceptions *)
  ) patterns

(** Check if any negative pattern matches *)
let check_negative_patterns prompt patterns =
  List.exists (contains_ci prompt) patterns

(** Extract string list from JSON *)
let json_to_string_list json =
  let open Yojson.Safe.Util in
  try
    json |> to_list |> List.map to_string
  with Type_error _ | Yojson.Json_error _ -> []

(** Extract optional string from JSON *)
let json_to_string_opt json =
  let open Yojson.Safe.Util in
  try Some (to_string json) with Type_error _ -> None

(** Skill match result *)
type skill_match = {
  name : string;
  match_type : string;
  enforcement : string;
  priority : string;
  category : string;
  description : string;
  block_message : string option;
}

(** Find all skills that match the given prompt *)
let find_matching_skills prompt rules =
  let open Yojson.Safe.Util in
  let skills =
    try rules |> member "skills" |> to_assoc
    with Type_error _ | Yojson.Json_error _ -> []
  in
  let matches = List.filter_map (fun (skill_name, config) ->
    let triggers =
      try config |> member "promptTriggers"
      with Type_error _ -> `Null
    in

    (* Check negative patterns first *)
    let neg_patterns = json_to_string_list (triggers |> member "negativePatterns") in
    if neg_patterns <> [] && check_negative_patterns prompt neg_patterns then
      None
    else begin
      (* Check keywords *)
      let keywords = json_to_string_list (triggers |> member "keywords") in
      let keyword_match = keywords <> [] && match_keywords prompt keywords in

      (* Check intent patterns *)
      let intent_patterns = json_to_string_list (triggers |> member "intentPatterns") in
      let intent_match = intent_patterns <> [] && match_intent_patterns prompt intent_patterns in

      if keyword_match || intent_match then
        let enforcement =
          config |> member "enforcement" |> json_to_string_opt
          |> Option.value ~default:"suggest"
        in
        let priority =
          config |> member "priority" |> json_to_string_opt
          |> Option.value ~default:"medium"
        in
        let category =
          config |> member "category" |> json_to_string_opt
          |> Option.value ~default:"workflow"
        in
        let description =
          config |> member "description" |> json_to_string_opt
          |> Option.value ~default:""
        in
        let block_message =
          config |> member "blockMessage" |> json_to_string_opt
        in
        Some {
          name = skill_name;
          match_type = if keyword_match then "keyword" else "intent";
          enforcement;
          priority;
          category;
          description;
          block_message;
        }
      else
        None
    end
  ) skills in

  (* Sort by priority *)
  let priority_order = function
    | "critical" -> 0
    | "high" -> 1
    | "medium" -> 2
    | "low" -> 3
    | _ -> 4
  in
  List.sort (fun a b -> compare (priority_order a.priority) (priority_order b.priority)) matches

(** Generate skill activation directive *)
let generate_directive matches =
  let critical = List.filter (fun m -> m.priority = "critical") matches in
  let high = List.filter (fun m -> m.priority = "high") matches in
  let medium_low = List.filter (fun m -> m.priority = "medium" || m.priority = "low") matches in

  let buf = Buffer.create 1024 in
  let add s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  add "<SKILL_EVALUATION>";
  add "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
  add "🔍 Skill Check - Before Implementation";
  add "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
  add "";
  add "**STEP 1 - EVALUATE** (YES/NO with reason):";
  add "| Skill | Applicable? |";
  add "|-------|-------------|";

  List.iter (fun m ->
    let icon = match m.enforcement with
      | "block" -> "🚫"
      | "warn" -> "⚠️"
      | "suggest" -> "💡"
      | _ -> ""
    in
    add (Printf.sprintf "| %s %s | ? |" icon m.name)
  ) matches;

  add "";

  if critical <> [] then begin
    add "**⚠️ CRITICAL SKILLS (REQUIRED)**:";
    List.iter (fun m ->
      add (Printf.sprintf "  → %s: %s" m.name m.description);
      Option.iter (fun msg ->
        let truncated = if String.length msg > 100 then String.sub msg 0 100 ^ "..." else msg in
        add (Printf.sprintf "     %s" truncated)
      ) m.block_message
    ) critical;
    add ""
  end;

  if high <> [] then begin
    add "**📚 RECOMMENDED SKILLS**:";
    List.iter (fun m ->
      add (Printf.sprintf "  → %s: %s" m.name m.description)
    ) high;
    add ""
  end;

  if medium_low <> [] then begin
    add "**💡 SUGGESTED SKILLS**:";
    List.iter (fun m ->
      add (Printf.sprintf "  → %s" m.name)
    ) medium_low;
    add ""
  end;

  add "**STEP 2 - ACTIVATE** (Required for YES answers):";
  add {|Use Skill tool: {"tool": "Skill", "skill": "<name>"}|};
  add "";
  add "**STEP 3 - IMPLEMENT** (After Step 2)";
  add "";
  add "**Note**: Activating applicable skills improves response quality.";
  add "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
  add "</SKILL_EVALUATION>";

  Buffer.contents buf

(** Convert match to JSON *)
let match_to_json m =
  `Assoc [
    ("name", `String m.name);
    ("matchType", `String m.match_type);
    ("enforcement", `String m.enforcement);
    ("priority", `String m.priority);
    ("category", `String m.category);
    ("description", `String m.description);
    ("blockMessage", match m.block_message with Some s -> `String s | None -> `Null);
  ]

(** CLI argument definitions *)
let query_arg =
  let doc = "User query to match" in
  Arg.(required & opt (some string) None & info ["query"] ~doc)

let category_arg =
  let doc = "Routing category (optional)" in
  Arg.(value & opt string "" & info ["category"] ~doc)

let json_arg =
  let doc = "Output JSON instead of directive" in
  Arg.(value & flag & info ["json"] ~doc)

let main query _category json_output =
  match load_skill_rules () with
  | None ->
      if json_output then
        print_endline {|{"count": 0, "skills": []}|};
      ()
  | Some rules ->
      let matches = find_matching_skills query rules in
      if json_output then begin
        let skill_names = List.map (fun m -> `String m.name) matches in
        let details = List.map match_to_json matches in
        let directive = if matches <> [] then generate_directive matches else "" in
        let result = `Assoc [
          ("count", `Int (List.length matches));
          ("skills", `List skill_names);
          ("details", `List details);
          ("directive", `String directive);
        ] in
        print_endline (Yojson.Safe.to_string result)
      end else begin
        if matches <> [] then
          print_string (generate_directive matches)
      end

let cmd =
  let doc = "Skill activator for forced evaluation" in
  let info = Cmd.info "skill_activator" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const main $ query_arg $ category_arg $ json_arg)

let () = ignore (Cmd.eval cmd)

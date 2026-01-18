(** council_vote - MAGI Agent Council voting system

    Ouroboros v7.0: Hive Mind (Agent Council)
    Convokes a council of AI agents to vote on complex decisions.

    Usage:
      council-vote "Topic/Proposal"
*)

type vote = Approve | Reject | Conditional

type agent = {
  name: string;
  role: string;
  bias: string;
}

let agents = [
  { name = "Gemini 3 Pro (CASPER)"; role = "1M Context & Web Search"; bias = "Innovation" };
  { name = "Claude Opus 4.5 (BALTHASAR)"; role = "Ethics & 80.9% SWE-Score"; bias = "Caution" };
  { name = "GPT-5.2-Codex (MELCHIOR)"; role = "Tool Use & Efficient Implementation"; bias = "Pragmatism" };
]

let vote_to_string = function
  | Approve -> "APPROVE"
  | Reject -> "REJECT"
  | Conditional -> "CONDITIONAL"

let vote_icon = function
  | Approve -> "✅"
  | Reject -> "❌"
  | Conditional -> "🤔"

let contains_word word text =
  let lower = String.lowercase_ascii text in
  try
    let _ = Str.search_forward (Str.regexp_string word) lower 0 in true
  with Not_found -> false

let simulate_reasoning agent topic =
  let has_delete = contains_word "delete" topic || contains_word "destroy" topic in
  let has_evolve = contains_word "evolve" topic || contains_word "upgrade" topic in

  if has_delete then
    match agent.bias with
    | "Caution" -> (Reject, "Too risky. Needs human verification first.")
    | "Innovation" -> (Conditional, "Allow if backups exist.")
    | _ -> (Conditional, "Only if it saves significant cost.")
  else if has_evolve then
    match agent.bias with
    | "Innovation" -> (Approve, "Evolution is essential for survival.")
    | "Pragmatism" -> (Approve, "Changes seem efficient and low-cost.")
    | _ -> (Conditional, "Proceed with rollback plan.")
  else
    (* Random choice simulation - use topic length as seed *)
    let choice = (String.length topic) mod 2 in
    if choice = 0 then (Approve, "Looks reasonable based on my heuristics.")
    else (Conditional, "Looks reasonable based on my heuristics.")

let tally_votes votes =
  let counts = Hashtbl.create 3 in
  Hashtbl.add counts Approve 0;
  Hashtbl.add counts Reject 0;
  Hashtbl.add counts Conditional 0;

  let claude_vote = ref None in

  List.iter (fun (agent, vote, _reason) ->
    let curr = Hashtbl.find counts vote in
    Hashtbl.replace counts vote (curr + 1);
    if contains_word "claude" (String.lowercase_ascii agent.name) then
      claude_vote := Some vote
  ) votes;

  (* Claude has veto power *)
  match !claude_vote with
  | Some Reject ->
      print_endline "🛡️  Governance Override: Claude utilized VETO power.";
      Reject
  | _ ->
      let approve_count = Hashtbl.find counts Approve in
      let reject_count = Hashtbl.find counts Reject in
      let conditional_count = Hashtbl.find counts Conditional in

      let winner =
        if approve_count >= reject_count && approve_count >= conditional_count then Approve
        else if reject_count >= approve_count && reject_count >= conditional_count then Reject
        else Conditional
      in
      Printf.printf "📊 Final Verdict: %s (Approved: %d, Rejected: %d)\n"
        (vote_to_string winner) approve_count reject_count;
      winner

let consult topic =
  Printf.printf "🏛️  Convoking Agent Council for: '%s'\n\n" topic;

  let votes = List.map (fun agent ->
    let (vote, reason) = simulate_reasoning agent topic in
    Printf.printf "  %s [%s] (%s): %s\n     \"%s\"\n\n"
      (vote_icon vote) agent.name agent.role (vote_to_string vote) reason;
    (agent, vote, reason)
  ) agents in

  let _ = tally_votes votes in
  ()

let () =
  if Array.length Sys.argv < 2 then begin
    print_endline "Usage: council-vote 'Topic/Proposal'";
    exit 1
  end;
  consult Sys.argv.(1)

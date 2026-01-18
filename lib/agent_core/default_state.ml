(** Default State Manager - Simple implementation of STATE_MANAGER

    Provides a straightforward state management with:
    - Message list with turn tracking
    - Sliding window for memory management
    - System message preservation during trimming
*)

open Agent_types

type t = {
  messages : message list;
  turn : int;
}

let create () = {
  messages = [];
  turn = 1;
}

let add_message state msg = {
  state with messages = state.messages @ [msg]
}

let add_messages state msgs = {
  state with messages = state.messages @ msgs
}

let get_messages state = state.messages

let get_turn state = state.turn

let increment_turn state = {
  state with turn = state.turn + 1
}

let message_count state = List.length state.messages

let estimate_tokens state =
  List.fold_left (fun acc msg ->
    acc + estimate_tokens_of_message msg
  ) 0 state.messages

(** Trim to sliding window, preserving system messages *)
let trim_to_window state ~max_messages =
  let messages = state.messages in
  let count = List.length messages in

  if count <= max_messages then
    state
  else begin
    (* Separate system messages from others *)
    let system_msgs, other_msgs = List.partition (fun msg ->
      msg.role = System
    ) messages in

    (* Calculate how many non-system messages to keep *)
    let system_count = List.length system_msgs in
    let keep_count = max 1 (max_messages - system_count) in

    (* Keep the most recent messages *)
    let recent = List.rev other_msgs in  (* newest first *)
    let kept =
      let rec take n acc = function
        | [] -> acc
        | x :: xs ->
          if n <= 0 then acc
          else take (n - 1) (x :: acc) xs
      in
      take keep_count [] recent  (* returns chronological order: oldest first *)
    in

    (* Reconstruct: system messages first, then kept messages *)
    { state with messages = system_msgs @ kept }
  end

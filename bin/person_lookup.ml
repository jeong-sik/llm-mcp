(** person_lookup - Korean name detection and Neo4j Person lookup

    OCaml port of .claude/hooks/person-lookup.py
    Reads user message from stdin, extracts Korean names, looks up in Neo4j.

    Usage:
      echo "정한길이 드럼작업한다고 X4 빌려감" | person_lookup
*)

(** Korean particles to strip (2-char first, then 1-char) *)
let particles_2 = ["에게"; "한테"; "에서"; "부터"; "까지"; "처럼"; "보다"; "라고"; "이랑"; "이형"; "이누나"; "이언니"; "이오빠"]
let particles_1 = ["이"; "가"; "을"; "를"; "은"; "는"; "의"; "에"; "와"; "과"; "도"; "만"; "로"; "야"; "형"; "씨"; "님"]

(** Common words to exclude (not names) *)
let exclude_words = [
  "그리고"; "하지만"; "그래서"; "왜냐면"; "그러나"; "그런데";
  "이것"; "저것"; "그것"; "여기"; "저기"; "거기";
  "오늘"; "내일"; "어제"; "지금"; "나중"; "한다";
  "진짜"; "정말"; "너무"; "아주"; "매우"; "있어";
  "이거"; "저거"; "그거"; "뭐야"; "왜요"; "없어";
  "좋아"; "싫어"; "괜찮"; "미안"; "감사"; "빌려";
  "드럼"; "작업"; "헤드폰"; "주문"; "구매";
  "오케이"; "시발"; "뭐가"; "어떻게"; "그냥"
]

(** Check if string ends with suffix *)
let ends_with s suffix =
  let slen = String.length s in
  let plen = String.length suffix in
  slen > plen && String.sub s (slen - plen) plen = suffix

(** Strip Korean particles from word (iteratively) *)
let rec strip_korean_particles word =
  let prev = word in
  (* Try 2-char particles first *)
  let word = List.fold_left (fun w p ->
    if ends_with w p && String.length w > String.length p + 1
    then String.sub w 0 (String.length w - String.length p)
    else w
  ) word particles_2 in
  (* Then 1-char particles *)
  let word = List.fold_left (fun w p ->
    if ends_with w p && String.length w > String.length p + 1
    then String.sub w 0 (String.length w - String.length p)
    else w
  ) word particles_1 in
  if word = prev then word else strip_korean_particles word

(** Check if character is Korean Hangul (UTF-8) *)
let is_korean_char s i =
  if i + 2 >= String.length s then false
  else
    let b0 = Char.code s.[i] in
    let b1 = Char.code s.[i + 1] in
    let b2 = Char.code s.[i + 2] in
    (* Korean Hangul syllables: U+AC00 to U+D7A3 *)
    (* UTF-8: 0xEA 0x80 0x80 to 0xED 0x9E 0xA3 *)
    b0 >= 0xEA && b0 <= 0xED &&
    b1 >= 0x80 && b1 <= 0xBF &&
    b2 >= 0x80 && b2 <= 0xBF

(** Count Korean characters in string *)
let count_korean_chars s =
  let len = String.length s in
  let rec count i acc =
    if i >= len then acc
    else if is_korean_char s i then count (i + 3) (acc + 1)
    else count (i + 1) acc
  in
  count 0 0

(** Extract consecutive Korean syllables from text *)
let extract_korean_sequences text =
  let len = String.length text in
  let sequences = ref [] in
  let current = Buffer.create 12 in
  let i = ref 0 in
  while !i < len do
    if !i + 2 < len && is_korean_char text !i then begin
      (* Add 3-byte UTF-8 Korean char *)
      Buffer.add_substring current text !i 3;
      i := !i + 3
    end else begin
      (* End of Korean sequence *)
      if Buffer.length current > 0 then begin
        sequences := Buffer.contents current :: !sequences;
        Buffer.clear current
      end;
      i := !i + 1
    end
  done;
  (* Don't forget last sequence *)
  if Buffer.length current > 0 then
    sequences := Buffer.contents current :: !sequences;
  List.rev !sequences

(** Extract Korean name candidates from text *)
let extract_korean_names text =
  (* Get all Korean sequences *)
  let sequences = extract_korean_sequences text in
  let candidates = List.filter (fun s ->
    let char_count = count_korean_chars s in
    char_count >= 2 && char_count <= 5
  ) sequences in

  (* Strip particles *)
  let stripped = List.map strip_korean_particles candidates in

  (* Filter: 2-4 chars, not in exclude list *)
  let filtered = List.filter (fun name ->
    let char_count = count_korean_chars name in
    char_count >= 2 && char_count <= 4 &&
    not (List.mem name exclude_words)
  ) stripped in

  (* Deduplicate *)
  List.sort_uniq String.compare filtered

(** Lookup person by name in Neo4j using Bolt protocol (Eio) *)
let lookup_person ~clock conn name =
  (* Exact match first *)
  let exact_query = {|
    MATCH (p:Person)
    WHERE p.name = $name OR p.nickname = $name OR p.displayName = $name
    OPTIONAL MATCH (p)-[r:MEMBER_OF]->(group)
    RETURN p, collect(DISTINCT group.name) as groups
    LIMIT 1
  |} in

  let params = `Assoc [("name", `String name)] in
  let result = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher:exact_query ~params () in

  match result with
  | Error _ -> None
  | Ok json ->
      let open Yojson.Safe.Util in
      try
        let records = json |> member "records" |> to_list in
        if List.length records = 0 then begin
          (* Try partial match *)
          if String.length name >= 6 then begin  (* 2 Korean chars = 6 bytes *)
            let partial_query = {|
              MATCH (p:Person)
              WHERE p.name CONTAINS $name OR p.displayName CONTAINS $name
              OPTIONAL MATCH (p)-[r:MEMBER_OF]->(group)
              RETURN p, collect(DISTINCT group.name) as groups
              LIMIT 1
            |} in
            let partial_result = Neo4j_bolt_eio.Bolt.query ~clock conn ~cypher:partial_query ~params () in
            match partial_result with
            | Error _ -> None
            | Ok pjson ->
                let precords = pjson |> member "records" |> to_list in
                if List.length precords = 0 then None
                else begin
                  (* records[0] = [[person, groups]] *)
                  let outer_row = List.hd precords |> to_list in
                  let inner_row = List.hd outer_row |> to_list in
                  let person = List.hd inner_row in
                  (* Extract person properties from _struct format *)
                  let props = try person |> member "fields" |> to_list |> fun l ->
                    if List.length l >= 3 then List.nth l 2 else `Null
                    with Type_error _ | Not_found -> person in
                  let groups = try List.nth inner_row 1 |> to_list |> List.filter_map (fun g ->
                    try Some (to_string g) with Type_error _ -> None
                  ) with Type_error _ -> [] in
                  Some (props, groups)
                end
          end
          else None
        end
        else begin
          (* records[0] = [[person, groups]] *)
          let outer_row = List.hd records |> to_list in
          let inner_row = List.hd outer_row |> to_list in
          let person = List.hd inner_row in
          (* Extract person properties from _struct format *)
          let props = try person |> member "fields" |> to_list |> fun l ->
            if List.length l >= 3 then List.nth l 2 else `Null
            with Type_error _ | Not_found -> person in
          let groups = try List.nth inner_row 1 |> to_list |> List.filter_map (fun g ->
            try Some (to_string g) with Type_error _ -> None
          ) with Type_error _ -> [] in
          Some (props, groups)
        end
      with Type_error _ -> None

(** Format person context for output *)
let format_person_context person groups =
  let open Yojson.Safe.Util in
  let buf = Buffer.create 256 in

  let name = try person |> member "name" |> to_string with Type_error _ ->
    try person |> member "displayName" |> to_string with Type_error _ -> "Unknown"
  in
  let nickname = try Some (person |> member "nickname" |> to_string) with Type_error _ -> None in

  Buffer.add_string buf (Printf.sprintf "👤 **%s**" name);
  (match nickname with Some n -> Buffer.add_string buf (Printf.sprintf " (%s)" n) | None -> ());
  Buffer.add_char buf '\n';

  (* Roles *)
  (try
    let roles = person |> member "roles" |> to_list |> List.map to_string in
    if List.length roles > 0 then
      Buffer.add_string buf (Printf.sprintf "   역할: %s\n" (String.concat ", " roles))
  with Type_error _ -> ());

  (* Instrument *)
  (try
    let inst = person |> member "instrument" |> to_string in
    Buffer.add_string buf (Printf.sprintf "   악기: %s\n" inst)
  with Type_error _ -> ());

  (* Company *)
  (try
    let company = person |> member "company" |> to_string in
    Buffer.add_string buf (Printf.sprintf "   회사: %s\n" company)
  with Type_error _ -> ());

  (* Groups *)
  if List.length groups > 0 then
    Buffer.add_string buf (Printf.sprintf "   소속: %s\n" (String.concat ", " groups));

  (* Teams *)
  (try
    let nba = try Some (person |> member "nba_team" |> to_string) with Type_error _ -> None in
    let epl = try Some (person |> member "epl_team" |> to_string) with Type_error _ -> None in
    let teams = List.filter_map (fun x -> x) [
      Option.map (fun t -> "NBA: " ^ t) nba;
      Option.map (fun t -> "EPL: " ^ t) epl;
    ] in
    if List.length teams > 0 then
      Buffer.add_string buf (Printf.sprintf "   팀: %s\n" (String.concat ", " teams))
  with Type_error _ -> ());

  (* Wine preference *)
  (try
    let wine = person |> member "wine_preference" |> to_string in
    Buffer.add_string buf (Printf.sprintf "   🍷: %s\n" wine)
  with Type_error _ -> ());

  Buffer.contents buf

(** Main function (Eio) *)
let main ~sw ~net ~clock =
  (* Read from stdin *)
  let user_message =
    let buf = Buffer.create 256 in
    try
      while true do
        let line = input_line stdin in
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      done;
      Buffer.contents buf
    with End_of_file -> Buffer.contents buf
  in

  let message = String.trim user_message in
  if String.length message = 0 then
    print_endline "{}"
  else begin
    (* Extract Korean names *)
    let names = extract_korean_names message in
    if List.length names = 0 then
      print_endline "{}"
    else begin
      (* Connect to Neo4j using Eio *)
      let config = Neo4j_bolt_eio.Bolt.default_config in
      match Neo4j_bolt_eio.Bolt.connect ~sw ~net ~clock ~config () with
      | Error e ->
          prerr_endline (Printf.sprintf "[person-lookup] Neo4j connection failed: %s"
            (Neo4j_bolt_eio.Bolt.error_to_string e));
          prerr_endline (Printf.sprintf "[person-lookup] Extracted names: %s"
            (String.concat ", " names));
          print_endline "{}"
      | Ok conn ->
          (* Lookup each name *)
          let results = ref [] in
          List.iter (fun name ->
            let person_opt = lookup_person ~clock conn name in
            match person_opt with
            | Some (person, groups) ->
                results := (name, person, groups) :: !results
            | None -> ()
          ) names;

          (* Close connection *)
          Neo4j_bolt_eio.Bolt.close conn;

          (* Format output *)
          if List.length !results = 0 then begin
            prerr_endline (Printf.sprintf "[person-lookup] No matches found for: %s"
              (String.concat ", " names));
            print_endline "{}"
          end else begin
            let context_parts = List.map (fun (_, person, groups) ->
              format_person_context person groups
            ) !results in
            let context = String.concat "\n" context_parts in

            let output = `Assoc [
              ("found", `Int (List.length !results));
              ("names", `List (List.map (fun (n, _, _) -> `String n) !results));
              ("context", `String context);
            ] in
            print_endline (Yojson.Safe.to_string output)
          end
    end
  end

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  main ~sw ~net ~clock

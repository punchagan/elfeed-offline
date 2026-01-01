module Filter = struct
  type t =
    { must_have: string list
    ; must_not_have: string list
    ; before_ms: float option
    ; after_ms: float option
    ; matches: string list
    ; not_matches: string list
    ; limit: int option
    ; feeds: string list
    ; not_feeds: string list }

  let empty =
    { must_have= []
    ; must_not_have= []
    ; before_ms= None
    ; after_ms= None
    ; matches= []
    ; not_matches= []
    ; limit= None
    ; feeds= []
    ; not_feeds= [] }

  let pp pp fmt (f : t) =
    Format.fprintf fmt
      "{ must_have=[%s]; must_not_have=[%s]; before_ms=%s; after_ms=%s; \
       matches=[%s]; not_matches=[%s]; limit=%s; feeds=[%s]; not_feeds=[%s] }"
      (String.concat "; " f.must_have)
      (String.concat "; " f.must_not_have)
      (match f.before_ms with None -> "None" | Some v -> string_of_float v)
      (match f.after_ms with None -> "None" | Some v -> string_of_float v)
      (String.concat "; " f.matches)
      (String.concat "; " f.not_matches)
      (match f.limit with None -> "None" | Some v -> string_of_int v)
      (String.concat "; " f.feeds)
      (String.concat "; " f.not_feeds)
end

module Parse = struct
  let parse_single_date_to_s ?(now : float option) (date_str : string) =
    if
      (* String can either be YYYY-mm-dd or x-days-old/ago *)
      String.ends_with ~suffix:"-days-ago" date_str
      || String.ends_with ~suffix:"-days-old" date_str
    then
      let days_str =
        String.sub date_str 0
          (String.length date_str - String.length "-days-ago")
      in
      match int_of_string_opt days_str with
      | Some days -> (
          let now =
            match now with
            | Some t ->
                t
            | None ->
                Ptime_clock.now () |> Ptime.to_float_s
          in
          match Ptime.Span.of_d_ps (days, 0L) with
          | None ->
              None
          | Some diff ->
              let t = Ptime.Span.to_float_s diff in
              Some (now -. t) )
      | None ->
          None
    else
      let ds = String.split_on_char '-' date_str in
      match List.map int_of_string_opt ds with
      | [Some year; Some month; Some day] ->
          (* NOTE: now is not used with absolute dates *)
          Ptime.of_date (year, month, day) |> Option.map Ptime.to_float_s
      | _ ->
          None

  let parse_date_to_s ?(now : float option) (date_str : string) =
    match Astring.String.cut ~sep:"--" date_str with
    | Some (start_str, end_str) -> (
        let after_ms = parse_single_date_to_s ?now start_str in
        let before_ms = parse_single_date_to_s ?now end_str in
        match (after_ms, before_ms) with
        | Some a, Some b ->
            if b < a then (Some b, Some a) else (Some a, Some b)
        | _ ->
            (None, None) )
    | None ->
        (parse_single_date_to_s ?now date_str, None)

  let parse_query (query : string) : Filter.t =
    let query_terms = String.split_on_char ' ' query in
    query_terms |> List.rev
    |> List.fold_left
         (fun acc term ->
           let term = String.trim term in
           if String.length term = 0 then acc
           else
             match String.get term 0 with
             | '+' ->
                 let tag = String.sub term 1 (String.length term - 1) in
                 {acc with Filter.must_have= tag :: acc.Filter.must_have}
             | '-' ->
                 let tag = String.sub term 1 (String.length term - 1) in
                 {acc with Filter.must_not_have= tag :: acc.Filter.must_not_have}
             | '#' ->
                 let limit = String.sub term 1 (String.length term - 1) in
                 {acc with Filter.limit= int_of_string_opt limit}
             | '=' ->
                 let feed_url = String.sub term 1 (String.length term - 1) in
                 {acc with Filter.feeds= feed_url :: acc.Filter.feeds}
             | '~' ->
                 let feed_url = String.sub term 1 (String.length term - 1) in
                 {acc with Filter.not_feeds= feed_url :: acc.Filter.not_feeds}
             | '@' -> (
                 let date_str = String.sub term 1 (String.length term - 1) in
                 let after_s, before_s = parse_date_to_s date_str in
                 let acc =
                   match after_s with
                   | Some s ->
                       {acc with Filter.after_ms= Some (s *. 1000.)}
                   | None ->
                       acc
                 in
                 match before_s with
                 | Some s ->
                     {acc with Filter.before_ms= Some (s *. 1000.)}
                 | None ->
                     acc )
             | '!' ->
                 let match_term = String.sub term 1 (String.length term - 1) in
                 { acc with
                   Filter.not_matches= match_term :: acc.Filter.not_matches }
             | _ ->
                 {acc with Filter.matches= term :: acc.Filter.matches} )
         Filter.empty
end

let entry_matches_filter (entry : State.entry) (filter : Filter.t) : bool =
  let has_all_must_have =
    List.for_all
      (fun tag -> List.exists (String.equal tag) entry.tags)
      filter.must_have
  in
  let has_no_must_not_have =
    List.for_all
      (fun tag -> not (List.exists (String.equal tag) entry.tags))
      filter.must_not_have
  in
  let before_ok =
    match filter.before_ms with
    | None ->
        true
    | Some before_ms ->
        entry.published_ms < before_ms
  in
  let after_ok =
    match filter.after_ms with
    | None ->
        true
    | Some after_ms ->
        entry.published_ms >= after_ms
  in
  let url = Uri.of_string entry.feed.url in
  let hostname = Uri.host url in
  (* TODO: Currently only allow feed search by hostname *)
  let feeds_ok =
    List.length filter.feeds = 0
    || List.exists
         (fun feed_url ->
           match hostname with
           | Some host ->
               String.equal host feed_url
           | None ->
               String.equal entry.feed.url feed_url )
         filter.feeds
  in
  let not_feeds_ok =
    List.for_all
      (fun feed_url ->
        not
          ( match hostname with
          | Some host ->
              String.equal host feed_url
          | None ->
              String.equal entry.feed.url feed_url ) )
      filter.not_feeds
  in
  let matches_ok =
    (* NOTE: We use AND semantics for multiple terms  *)
    let search =
      String.concat ".*" filter.matches |> String.trim |> Str.regexp_case_fold
    in
    try
      Str.search_forward search entry.title 0 |> ignore ;
      true
    with Not_found -> false
  in
  let not_matches_ok =
    (* NOTE: We use OR semantics for multiple terms. We ensure none of the
       terms are present in the title *)
    let rec aux = function
      | [] ->
          true
      | term :: rest -> (
          let search = Str.regexp_case_fold (String.trim term) in
          try
            Str.search_forward search entry.title 0 |> ignore ;
            false
          with Not_found -> aux rest )
    in
    aux filter.not_matches
  in
  has_all_must_have && has_no_must_not_have && before_ok && after_ok && feeds_ok
  && not_feeds_ok && matches_ok && not_matches_ok

let filter_results ~(query : string) (entries : State.entry list) =
  let filter = Parse.parse_query query in
  let filtered_entries =
    List.filter (fun e -> entry_matches_filter e filter) entries
  in
  match filter.Filter.limit with
  | None ->
      filtered_entries
  | Some limit ->
      List.take limit filtered_entries

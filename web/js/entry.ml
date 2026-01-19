open Brr

let format_date (ms : float) =
  let ms = Jv.of_float ms in
  let d = Jv.new' (Jv.get Jv.global "Date") [|ms|] in
  let year = Jv.to_int (Jv.call d "getFullYear" [||]) in
  let month = Jv.to_int (Jv.call d "getMonth" [||]) + 1 in
  let day = Jv.to_int (Jv.call d "getDate" [||]) in
  Printf.sprintf "%04d-%02d-%02d" year month day |> Jstr.of_string

let search_add_remove_tag evt =
  Ev.prevent_default evt ;
  Ev.stop_propagation evt ;
  let target = Ev.target evt in
  let tag_text =
    Ev.target_to_jv target |> El.of_jv |> El.text_content |> Jstr.to_string
  in
  let tag_text = String.sub tag_text 1 (String.length tag_text - 1) in
  let tag_text = String.cat "+" tag_text |> Jstr.of_string in
  let q_el = Util.get_element_by_id_exn "q" in
  let current_q = El.prop El.Prop.value q_el in
  let new_q = Util.add_or_remove_substring current_q tag_text in
  Util.set_query new_q ; Util.submit_search_form ()

let search_add_remove_feed_url evt =
  Ev.prevent_default evt ;
  Ev.stop_propagation evt ;
  let target = Ev.target evt in
  let feed_url =
    Ev.target_to_jv target |> El.of_jv
    |> El.at (Jstr.of_string "data-url")
    |> Option.map Jstr.to_string
  in
  match feed_url with
  | None ->
      ()
  | Some feed_title ->
      let search_text = Printf.sprintf "=%s" feed_title |> Jstr.v in
      let q_el = Util.get_element_by_id_exn "q" in
      let current_q = El.prop El.Prop.value q_el in
      let new_q = Util.add_or_remove_substring current_q search_text in
      Util.set_query new_q ; Util.submit_search_form ()

let entry_of_jv e : State.entry =
  let title = Jv.get e "title" |> Jv.to_string in
  let feed_data = Jv.get e "feed" in
  let feed_title = Jv.get feed_data "title" |> Jv.to_string in
  let feed_url = Jv.get feed_data "url" |> Jv.to_string in
  let feed : State.feed = {title= feed_title; url= feed_url} in
  let published_ms = Jv.get e "date" |> Jv.to_float in
  let tags = Jv.get e "tags" |> Jv.to_list Jv.to_string in
  let is_unread = List.exists (String.equal "unread") tags in
  let is_starred = List.exists (String.equal "starred") tags in
  let webid = Jv.get e "webid" |> Jv.to_string in
  let link = Jv.get e "link" |> Jv.to_string in
  let content_hash = Jv.get e "content" |> Jv.to_string in
  { webid
  ; title
  ; link
  ; content_hash
  ; feed
  ; tags
  ; is_unread
  ; is_starred
  ; published_ms }

let make_entry (data : State.entry) =
  let title_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "title")]
      (Jstr.of_string "span")
      [data.title |> Jstr.v |> El.txt]
  in
  let feed_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "feed")]
      (Jstr.of_string "span")
      [data.feed.title |> Jstr.v |> El.txt]
  in
  let feed_hostname =
    match data.feed.url |> Jstr.v |> Uri.of_jstr with
    | Ok uri ->
        Some (Uri.host uri)
    | Error _ ->
        None
  in
  El.set_at (Jstr.of_string "data-url") feed_hostname feed_el ;
  Ev.listen Ev.click search_add_remove_feed_url (El.as_target feed_el) |> ignore ;
  let date = format_date data.published_ms in
  let date_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "date")]
      (Jstr.of_string "span")
      [El.txt date]
  in
  let tags_el =
    let tag_chip tag =
      if String.equal tag "unread" || String.equal tag "starred" then None
      else
        let label = Printf.sprintf "#%s" tag in
        Some
          (El.v
             ~at:[At.v At.Name.class' (Jstr.of_string "tag")]
             (Jstr.of_string "span")
             [label |> Jstr.v |> El.txt] )
    in
    let chips = List.filter_map tag_chip data.tags in
    (* Click handler for tags *)
    List.iter
      (fun tag_el ->
        Ev.listen Ev.click search_add_remove_tag (El.as_target tag_el) |> ignore )
      chips ;
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "tags")]
      (Jstr.of_string "div") chips
  in
  let star_btn_el = Icons.star_button_el data in
  Ev.listen Ev.click
    (fun e ->
      Ev.prevent_default e ;
      Ev.stop_propagation e ;
      if data.is_starred then Actions.unstar_entry data.webid
      else Actions.star_entry data.webid )
    (El.as_target star_btn_el)
  |> ignore ;
  let read_unread_btn_el = Icons.read_unread_el data in
  Ev.listen Ev.click
    (fun e ->
      Ev.prevent_default e ;
      Ev.stop_propagation e ;
      if data.is_unread then Actions.mark_as_read data.webid
      else Actions.mark_as_unread data.webid )
    (El.as_target read_unread_btn_el)
  |> ignore ;
  let icon_span =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "icon-span")]
      (Jstr.of_string "span")
      [star_btn_el; read_unread_btn_el]
  in
  let controls_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "entry-controls")]
      (Jstr.of_string "div") [date_el; icon_span]
  in
  let is_selected =
    match State.state.selected_index with
    | Some idx when List.nth_opt State.state.results idx = Some data.webid ->
        true
    | _ ->
        false
  in
  let selected_class =
    if is_selected then Jstr.of_string "selected" else Jstr.empty
  in
  let entry =
    El.v
      ~at:
        [ "entry" |> Jstr.v |> At.class'
        ; (if data.is_unread then "unread" else "") |> Jstr.v |> At.class'
        ; selected_class |> At.class' ]
      (Jstr.of_string "div")
      [controls_el; title_el; feed_el; tags_el]
  in
  Ev.listen Ev.click
    (fun _ ->
      match List.find_index (fun id -> id = data.webid) State.state.results with
      | None ->
          ()
      | Some idx ->
          State.state.selected_index <- Some idx ;
          Actions.open_selected_entry () )
    (El.as_target entry)
  |> ignore ;
  entry

let update_entry_tags ~(state : State.model) (entry : State.entry) =
  let state_added_tags =
    match Hashtbl.find_opt state.tags_added entry.webid with
    | Some tags ->
        tags
    | None ->
        []
  in
  let state_removed_tags =
    match Hashtbl.find_opt state.tags_removed entry.webid with
    | Some tags ->
        tags
    | None ->
        []
  in
  let updated_tags =
    entry.tags
    |> List.filter (fun t -> not (List.mem t state_removed_tags))
    |> fun ts -> ts @ state_added_tags
  in
  let is_unread = List.exists (String.equal "unread") updated_tags in
  let is_starred = List.exists (String.equal "starred") updated_tags in
  {entry with tags= updated_tags; is_unread; is_starred}

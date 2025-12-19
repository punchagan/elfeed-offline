open Brr
module Fetch = Brr_io.Fetch
module Message = Brr_io.Message
module Sw = Brr_webworkers.Service_worker
open Util
open Nav
open State

let set_status_prefetch msg =
  let el = get_element_by_id_exn "prefetch-status" in
  let text_node = El.txt (Jstr.v msg) in
  El.set_children el [text_node]

let get_query () =
  let q_el = get_element_by_id_exn "q" in
  El.prop El.Prop.value q_el |> Jstr.trim

let format_date (ms : float) =
  let ms = Jv.of_float ms in
  let d = Jv.new' (Jv.get Jv.global "Date") [|ms|] in
  let year = Jv.to_int (Jv.call d "getFullYear" [||]) in
  let month = Jv.to_int (Jv.call d "getMonth" [||]) + 1 in
  let day = Jv.to_int (Jv.call d "getDate" [||]) in
  Printf.sprintf "%04d-%02d-%02d" year month day |> Jstr.of_string

let prefetch_top_n ?(n = 30) _click_evt =
  let ids =
    state.results |> List.take n
    |> List.map (fun webid ->
        Hashtbl.find state.entries webid
        |> fun e -> e.content_hash |> Jv.of_string )
  in
  let container = Sw.Container.of_navigator G.navigator in
  match Sw.Container.controller container with
  | Some w ->
      let worker = Sw.as_worker w in
      Brr_webworkers.Worker.post worker
        (Jv.obj
           [|("type", Jv.of_string "PREFETCH"); ("ids", ids |> Jv.of_jv_list)|] ) ;
      set_status_prefetch (Printf.sprintf "Starting… 0/%d" (List.length ids))
  | None ->
      set_status_prefetch "No service worker found."

let search_add_remove_tag evt =
  Ev.prevent_default evt ;
  Ev.stop_propagation evt ;
  let target = Ev.target evt in
  let tag_text =
    Ev.target_to_jv target |> El.of_jv |> El.text_content |> Jstr.to_string
  in
  let tag_text = String.sub tag_text 1 (String.length tag_text - 1) in
  let tag_text = String.cat "+" tag_text |> Jstr.of_string in
  let q_el = get_element_by_id_exn "q" in
  let current_q = El.prop El.Prop.value q_el in
  let new_q = add_or_remove_substring current_q tag_text in
  El.set_prop El.Prop.value new_q q_el ;
  submit_search_form ()

let search_add_remove_feed_url evt =
  Ev.prevent_default evt ;
  Ev.stop_propagation evt ;
  let target = Ev.target evt in
  let feed_title =
    Ev.target_to_jv target |> El.of_jv
    |> El.at (Jstr.of_string "data-url")
    |> Option.map Jstr.to_string
  in
  match feed_title with
  | None ->
      ()
  | Some feed_title ->
      let search_text = Printf.sprintf "=%s" feed_title |> Jstr.v in
      let q_el = get_element_by_id_exn "q" in
      let current_q = El.prop El.Prop.value q_el in
      let new_q = add_or_remove_substring current_q search_text in
      El.set_prop El.Prop.value new_q q_el ;
      submit_search_form ()

let make_entry (data : entry) =
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
  El.set_at (Jstr.of_string "data-url") (Some (Jstr.v data.feed.url)) feed_el ;
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
  let entry =
    El.v
      ~at:
        [ "entry" |> Jstr.v |> At.class'
        ; (if data.is_unread then "unread" else "") |> Jstr.v |> At.class' ]
      (Jstr.of_string "div")
      [date_el; title_el; feed_el; tags_el]
  in
  let _ =
    Ev.listen Ev.click
      (fun _ ->
        let content_el = get_element_by_id_exn "content" in
        let content_hash = data.content_hash in
        let content_url = Printf.sprintf "/elfeed/content/%s" content_hash in
        (* Set src of IFrame *)
        El.set_at At.Name.src (Some (Jstr.v content_url)) content_el ;
        (* Set reading mode *)
        Document.body G.document |> El.set_class (Jstr.of_string "reading") true ;
        state.selected <- Some data.webid ;
        render_nav () )
      (El.as_target entry)
  in
  entry

let update_app_state data =
  let _entries =
    data
    |> Jv.to_list (fun e ->
        let title = Jv.get e "title" |> Jv.to_string in
        let feed_data = Jv.get e "feed" in
        let feed_title = Jv.get feed_data "title" |> Jv.to_string in
        let feed_url = Jv.get feed_data "url" |> Jv.to_string in
        let feed = {title= feed_title; url= feed_url} in
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
        ; published_ms } )
  in
  let results = List.map (fun e -> e.webid) _entries in
  List.iter (fun e -> Hashtbl.replace state.entries e.webid e) _entries ;
  state.results <- results ;
  ()

let display_results response =
  let open Fut.Result_syntax in
  let headers = Fetch.Response.headers response in
  let* data = response |> Fetch.Response.as_body |> Fetch.Body.json in
  update_app_state data ;
  ( match state.selected with
  | Some webid ->
      if not (List.mem webid state.results) then close_entry ()
      else render_nav ()
  | None ->
      () ) ;
  let children =
    state.results
    |> List.map (fun webid -> Hashtbl.find state.entries webid)
    |> List.map make_entry
  in
  let results_el = get_element_by_id_exn "results" in
  El.set_children results_el children ;
  let n = data |> Jv.to_jv_list |> List.length in
  let loaded = "loaded " ^ string_of_int n ^ " items" in
  let status =
    match Fetch.Headers.find (Jstr.of_string "X-Cache") headers with
    | Some t ->
        let suffix =
          match Jstr.to_string t with
          | "HIT" ->
              " (from cache)"
          | "HIT-X" ->
              " (from cache; Emacs elfeed server offline)"
          | "NEAR" ->
              " (from latest cached search)"
          | _ ->
              " (from unknown cached entry)"
        in
        loaded ^ suffix
    | None ->
        loaded ^ " (from the server)"
  in
  set_status status ; Fut.ok ()

let do_search () =
  let base = "/elfeed/search?q=" |> Jstr.of_string in
  let q = get_query () in
  match q |> Uri.encode_component with
  | Error e ->
      let msg =
        e |> Jv.Error.message |> Jstr.to_string
        |> Printf.sprintf "Search aborted: %s"
      in
      msg |> Jstr.v |> Jv.Error.v |> Fut.error
  | Ok s -> (
      let open Fut.Result_syntax in
      let url = Jstr.append base s in
      let resp = Fetch.url url in
      let* response = resp in
      let status = Fetch.Response.status response in
      match status with
      | 200 ->
          display_results response
      | 500 | 403 (* Emacs elfeed server stopped, but httpd running *) ->
          Fut.error (Jv.Error.v (Jstr.v "Is the Emacs elfeed server running?"))
      | _ ->
          Fut.error
            (Jv.Error.v
               (Jstr.v "offline or backend unavailable & no cache data found") )
      )

let search () =
  Fut.await (do_search ()) (fun r ->
      match r with
      | Error e ->
          e |> Jv.Error.message |> Jstr.to_string
          |> Printf.sprintf "Search failed: %s"
          |> set_status
      | _ ->
          () )

let on_message e =
  let data = e |> Ev.as_type |> Message.Ev.data in
  match Jv.to_string (Jv.get data "type") with
  | "PREFETCH_PROGRESS" ->
      let done_ = Jv.to_int (Jv.get data "done") in
      let total = Jv.to_int (Jv.get data "total") in
      set_status_prefetch (Printf.sprintf "Saving… %d/%d" done_ total)
  | "PREFETCH_DONE" ->
      let total = Jv.to_int (Jv.get data "total") in
      set_status_prefetch (Printf.sprintf "Saved %d items." total)
  | "PREFETCH_STOP" ->
      let reason = Jv.to_string (Jv.get data "reason") in
      set_status_prefetch (Printf.sprintf "Stopped: %s" reason)
  | _ ->
      ()

let setup_handlers () =
  (* Hook up search-form submit event handler *)
  let form_el = get_element_by_id_exn "search-form" in
  let submit = Ev.Type.create (Jstr.v "submit") in
  Ev.listen submit
    (fun e -> Ev.prevent_default e ; set_status "searching ..." ; search ())
    (El.as_target form_el)
  |> ignore ;
  (* Hook up offline btn click handler *)
  let n = 100 in
  let offline_btn_el = get_element_by_id_exn "save-offline" in
  let text_node =
    n |> Printf.sprintf "Save top %d offline" |> Jstr.of_string |> El.txt
  in
  El.set_children offline_btn_el [text_node] ;
  Ev.listen Ev.click (prefetch_top_n ~n) (El.as_target offline_btn_el) |> ignore ;
  (* Hook up handlers for nav buttons *)
  setup_nav_handlers () ;
  (* Hook up message listener  *)
  let container = Sw.Container.of_navigator G.navigator in
  match
    Ev.listen Message.Ev.message on_message (Sw.Container.as_target container)
  with
  | _ ->
      ()
  | exception Jv.Error _ ->
      set_status_prefetch "Failed to setup service worker!"

let () =
  setup_handlers () ;
  (* Initial load *)
  let q_el = get_element_by_id_exn "q" in
  El.set_at At.Name.value (Some (Jstr.of_string "@30-days-old +unread")) q_el ;
  search () ;
  render_nav ()

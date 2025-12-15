open Brr
module Fetch = Brr_io.Fetch
module Message = Brr_io.Message
module Sw = Brr_webworkers.Service_worker

let submit = Ev.Type.create (Jstr.of_string "submit")

let last_entries : Jv.t list ref = ref []

let get_element_by_id_exn id =
  match id |> Jstr.of_string |> Document.find_el_by_id G.document with
  | Some el ->
      el
  | _ ->
      "Failed to find element with id: " ^ id |> failwith

let set_status txt =
  let status_el = get_element_by_id_exn "status" in
  let text_node = El.txt (Jstr.v txt) in
  El.set_children status_el [text_node]

let set_status_prefetch msg =
  let el = get_element_by_id_exn "prefetch-status" in
  let text_node = El.txt (Jstr.v msg) in
  El.set_children el [text_node]

let get_query () =
  let q_el = get_element_by_id_exn "q" in
  El.prop El.Prop.value q_el |> Jstr.trim

let format_date (data : Jv.t) =
  let ms = Jv.get data "date" in
  let d = Jv.new' (Jv.get Jv.global "Date") [|ms|] in
  let year = Jv.to_int (Jv.call d "getFullYear" [||]) in
  let month = Jv.to_int (Jv.call d "getMonth" [||]) + 1 in
  let day = Jv.to_int (Jv.call d "getDate" [||]) in
  Printf.sprintf "%04d-%02d-%02d" year month day |> Jstr.of_string

let prefetch_top_n ?(n = 30) _click_evt =
  let ids =
    !last_entries |> List.take n
    |> List.map (fun entry -> Jv.get entry "content")
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

let submit_search_form () =
  let submit_event = Ev.create submit in
  let form_el = get_element_by_id_exn "search-form" in
  Ev.dispatch submit_event (El.as_target form_el) |> ignore

let add_tag_to_search evt =
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
  let new_q =
    if Jstr.is_empty current_q then tag_text
    else if Jstr.find_sub ~sub:tag_text current_q <> None then current_q
    else Jstr.append current_q (Jstr.append (Jstr.of_string " ") tag_text)
  in
  El.set_prop El.Prop.value new_q q_el ;
  submit_search_form ()

let add_feed_url_to_search evt =
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
      let tag_text = Printf.sprintf "=%s" feed_title |> Jstr.of_string in
      let q_el = get_element_by_id_exn "q" in
      let current_q = El.prop El.Prop.value q_el in
      if Jstr.find_sub ~sub:tag_text current_q <> None then ()
      else
        let new_q =
          if Jstr.is_empty current_q then tag_text
          else Jstr.append current_q (Jstr.append (Jstr.of_string " ") tag_text)
        in
        El.set_prop El.Prop.value new_q q_el ;
        submit_search_form ()

let set_open_original (href_opt : Jstr.t option) =
  let link_el = get_element_by_id_exn "open-original" in
  let target_attr = Jstr.of_string "target" in
  match href_opt with
  | Some href ->
      El.set_at At.Name.href (Some href) link_el ;
      El.set_at target_attr (Some (Jstr.v "_blank")) link_el ;
      El.set_at At.Name.rel (Some (Jstr.v "noopener")) link_el ;
      El.set_class (Jstr.v "muted") false link_el ;
      El.set_at (Jstr.v "aria-disabled") (Some (Jstr.v "false")) link_el
  | None ->
      (* No URL available; make it look disabled *)
      El.set_at At.Name.href (Some (Jstr.v "#")) link_el ;
      El.set_class (Jstr.v "muted") true link_el ;
      El.set_at (Jstr.v "aria-disabled") (Some (Jstr.v "true")) link_el

let make_entry data =
  let title = Jv.get data "title" |> Jv.to_jstr in
  let title_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "title")]
      (Jstr.of_string "span")
      [El.txt title]
  in
  let feed = Jv.get data "feed" |> (fun x -> Jv.get x "title") |> Jv.to_jstr in
  let feed_url =
    Jv.get data "feed" |> (fun x -> Jv.get x "url") |> Jv.to_jstr
  in
  let feed_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "feed")]
      (Jstr.of_string "span")
      [El.txt feed]
  in
  El.set_at (Jstr.of_string "data-url") (Some feed_url) feed_el ;
  Ev.listen Ev.click add_feed_url_to_search (El.as_target feed_el) |> ignore ;
  let date = format_date data in
  let date_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "date")]
      (Jstr.of_string "span")
      [El.txt date]
  in
  let tags = try Jv.get data "tags" |> Jv.to_list Jv.to_jstr with _ -> [] in
  let tags_el =
    let tag_chip (t : Jstr.t) =
      let label = Jstr.(append (of_string "#") t) in
      El.v
        ~at:[At.v At.Name.class' (Jstr.of_string "tag")]
        (Jstr.of_string "span")
        [El.txt label]
    in
    let chips = List.map tag_chip tags in
    (* Click handler for tags *)
    List.iter
      (fun tag_el ->
        Ev.listen Ev.click add_tag_to_search (El.as_target tag_el) |> ignore )
      chips ;
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "tags")]
      (Jstr.of_string "div") chips
  in
  let entry =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "entry")]
      (Jstr.of_string "div")
      [date_el; title_el; feed_el; tags_el]
  in
  let _ =
    Ev.listen Ev.click
      (fun _ ->
        let content_el = get_element_by_id_exn "content" in
        let content_hash = Jv.get data "content" |> Jv.to_jstr in
        let content_url =
          Jstr.append (Jstr.of_string "/elfeed/content/") content_hash
        in
        (* Set src of IFrame *)
        El.set_at At.Name.src (Some content_url) content_el ;
        (* Set reading mode *)
        Document.body G.document |> El.set_class (Jstr.of_string "reading") true ;
        (* Set current entry href *)
        Some (Jv.get data "link" |> Jv.to_jstr) |> set_open_original ;
        (* Set data property on mark-read & mark-unread button *)
        let webid = Jv.get data "webid" in
        let mark_read_btn_el = get_element_by_id_exn "mark-read" in
        let mark_unread_btn_el = get_element_by_id_exn "mark-unread" in
        El.set_at
          (Jstr.of_string "data-webid")
          (Some (Jv.to_jstr webid))
          mark_read_btn_el ;
        El.set_at
          (Jstr.of_string "data-webid")
          (Some (Jv.to_jstr webid))
          mark_unread_btn_el ;
        () )
      (El.as_target entry)
  in
  entry

let display_results response =
  let open Fut.Result_syntax in
  let headers = Fetch.Response.headers response in
  let* data = response |> Fetch.Response.as_body |> Fetch.Body.json in
  let data_list = Jv.to_jv_list data in
  last_entries := data_list ;
  let children = data |> Jv.to_jv_list |> List.map make_entry in
  let results_el = get_element_by_id_exn "results" in
  El.set_children results_el children ;
  let n = data_list |> List.length in
  let loaded = "loaded " ^ string_of_int n ^ " items" in
  let status =
    match Fetch.Headers.find (Jstr.of_string "X-Cache") headers with
    | Some t ->
        let suffix =
          match Jstr.to_string t with
          | "HIT" ->
              " (from cache)"
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

let search () =
  let base = "/elfeed/search?q=" |> Jstr.of_string in
  let q = get_query () in
  match q |> Uri.encode_component with
  | Error e ->
      let msg =
        e |> Jv.Error.message |> Jstr.to_string
        |> fun s -> "Failed to search with error:" ^ s
      in
      set_status msg ; failwith msg
  | Ok s ->
      let open Fut.Result_syntax in
      let url = Jstr.append base s in
      let resp = Fetch.url url in
      let* response = resp in
      let status = Fetch.Response.status response in
      ignore
        ( if status <> 200 then (
            set_status "offline or backend unavailable & no cache data found" ;
            failwith "Server returned with status code: " ^ string_of_int status
            )
          else (
            Fut.await (display_results response) (fun _ -> ()) ;
            "" ) ) ;
      Fut.ok ()

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

let update_tag_data data =
  let url = "/elfeed/tags" |> Jstr.of_string in
  let body = data |> Json.encode |> Fetch.Body.of_jstr in
  let init = Fetch.Request.init ~body ~method':(Jstr.of_string "put") () in
  Fut.await (Fetch.url url ~init) (fun response ->
      match response with
      | Error _ ->
          set_status "Failed to update tag data!"
      | Ok response ->
          let status = Fetch.Response.status response in
          if status = 202 then
            (* Request has been cached by service worker *)
            set_status (Printf.sprintf "Tags will be updated when online")
          else submit_search_form () )

let mark_entry_as_read web_id =
  let data =
    Jv.obj
      [| ("entries", [Jstr.of_string web_id] |> Jv.of_jstr_list)
       ; ("remove", [Jstr.of_string "unread"] |> Jv.of_jstr_list) |]
  in
  update_tag_data data

let mark_entry_as_unread web_id =
  let data =
    Jv.obj
      [| ("entries", [Jstr.of_string web_id] |> Jv.of_jstr_list)
       ; ("add", [Jstr.of_string "unread"] |> Jv.of_jstr_list) |]
  in
  update_tag_data data

let setup_handlers () =
  (* Hook up search-form submit event handler *)
  let form_el = get_element_by_id_exn "search-form" in
  Ev.listen submit
    (fun e ->
      Ev.prevent_default e ;
      set_status "searching ..." ;
      Fut.await (search ()) (fun _ -> ()) )
    (El.as_target form_el)
  |> ignore ;
  (* Hook up back-btn click handler *)
  let back_btn_el = get_element_by_id_exn "back" in
  Ev.listen Ev.click
    (fun _ ->
      Document.body G.document |> El.set_class (Jstr.of_string "reading") false ;
      let content_el = get_element_by_id_exn "content" in
      El.set_at At.Name.src (Some (Jstr.v "about:blank")) content_el )
    (El.as_target back_btn_el)
  |> ignore ;
  (* Hook up mark-as-read handler *)
  let mark_read_btn_el = get_element_by_id_exn "mark-read" in
  Ev.listen Ev.click
    (fun _ ->
      let web_id = El.at (Jstr.of_string "data-webid") mark_read_btn_el in
      web_id
      |> Option.iter (fun id -> id |> Jstr.to_string |> mark_entry_as_read) )
    (El.as_target mark_read_btn_el)
  |> ignore ;
  (* Hook up mark-as-unread handler *)
  let mark_unread_btn_el = get_element_by_id_exn "mark-unread" in
  Ev.listen Ev.click
    (fun _ ->
      let web_id = El.at (Jstr.of_string "data-webid") mark_unread_btn_el in
      web_id
      |> Option.iter (fun id -> id |> Jstr.to_string |> mark_entry_as_unread) )
    (El.as_target mark_unread_btn_el)
  |> ignore ;
  (* Hook up offline btn click handler *)
  let n = 100 in
  let offline_btn_el = get_element_by_id_exn "save-offline" in
  let text_node =
    n |> Printf.sprintf "Save top %d offline" |> Jstr.of_string |> El.txt
  in
  El.set_children offline_btn_el [text_node] ;
  Ev.listen Ev.click (prefetch_top_n ~n) (El.as_target offline_btn_el) |> ignore ;
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
  submit_search_form ()

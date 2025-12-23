open Brr
module Fetch = Brr_io.Fetch
module Message = Brr_io.Message
module Sw = Brr_webworkers.Service_worker
open Elfeed_offline_web
open Util
open Nav
open State

let get_query () =
  let q_el = get_element_by_id_exn "q" in
  El.prop El.Prop.value q_el |> Jstr.trim

let prefetch_top_n ?(n = 30) _click_evt =
  let container = Sw.Container.of_navigator G.navigator in
  match Sw.Container.controller container with
  | Some w ->
      let worker = Sw.as_worker w in
      let hashes =
        state.results |> List.take n
        |> List.map (fun webid ->
            Hashtbl.find state.entries webid
            |> fun e -> e.content_hash |> Jv.of_string )
      in
      Brr_webworkers.Worker.post worker
        (Jv.obj
           [| ("type", Jv.of_string "PREFETCH")
            ; ("content_hashes", hashes |> Jv.of_jv_list) |] ) ;
      set_status (Printf.sprintf "Starting… 0/%d" (List.length hashes))
  | None ->
      set_status "No service worker found."

let update_app_state ~cached data =
  let _entries =
    data |> Jv.to_jv_list
    |> List.filter (Jv.has "content")
    |> List.map (fun e ->
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
  List.iter (fun e -> Hashtbl.replace state.entries e.webid e) _entries ;
  let q_el = get_element_by_id_exn "q" in
  let query = El.prop El.Prop.value q_el |> Jstr.to_string in
  let results =
    (if cached then Offline_search.filter_results ~query _entries else _entries)
    |> List.map (fun e -> e.webid)
  in
  state.results <- results

let display_results response =
  let open Fut.Result_syntax in
  let headers = Fetch.Response.headers response in
  let* data = response |> Fetch.Response.as_body |> Fetch.Body.json in
  let cache_header =
    Fetch.Headers.find (Jstr.of_string "X-Cache") headers
    |> Option.map Jstr.to_string
  in
  let cached =
    cache_header
    |> Option.map (( = ) "OFFLINE-SEARCH")
    |> Option.value ~default:false
  in
  update_app_state ~cached data ;
  ( match state.selected with
  | Some webid ->
      if not (List.mem webid state.results) then close_entry ()
      else render_nav ()
  | None ->
      () ) ;
  let children =
    state.results
    |> List.map (fun webid -> Hashtbl.find state.entries webid)
    |> List.map Entry.make_entry
  in
  let results_el = get_element_by_id_exn "results" in
  El.set_children results_el children ;
  let n = data |> Jv.to_jv_list |> List.length in
  let loaded = "loaded " ^ string_of_int n ^ " items" in
  let status =
    match cache_header with
    | Some t ->
        let suffix =
          match t with
          | "HIT" ->
              " (from cache)"
          | "HIT-X" ->
              " (from cache; Emacs elfeed server offline)"
          | "OFFLINE-SEARCH" ->
              " (from offline-search-cache)"
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
      set_status (Printf.sprintf "Saving… %d/%d" done_ total)
  | "PREFETCH_DONE" ->
      let total = Jv.to_int (Jv.get data "total") in
      set_status (Printf.sprintf "Saved %d items." total)
  | "PREFETCH_SEARCH_DONE" ->
      set_status
        (Printf.sprintf "Prefetched search query data for offline search")
  | "PREFETCH_SEARCH_FAILED" ->
      set_status
        (Printf.sprintf
           "Failed to prefetch search query data for offline search" )
  | "PREFETCH_STOP" ->
      let reason = Jv.to_string (Jv.get data "reason") in
      set_status (Printf.sprintf "Stopped: %s" reason)
  | "PREFETCH_ERROR" ->
      let id = Jv.to_string (Jv.get data "msg") in
      set_status (Printf.sprintf "Error: %s" id)
  | _ ->
      ()

let mark_all_as_read _ =
  let data =
    Jv.obj
      [| ("entries", state.results |> Jv.of_list Jv.of_string)
       ; ("remove", [Jstr.of_string "unread"] |> Jv.of_jstr_list) |]
  in
  Api.update_tag_data data
    (fun status ->
      if status = 202 then
        set_status (Printf.sprintf "Tags will be updated when online")
      else search () )
    (fun () -> set_status "Failed to mark all as read!")

let confirm_mark_all_as_read evt =
  Ev.prevent_default evt ;
  Ev.stop_propagation evt ;
  let confirm = Jv.get Jv.global "confirm" in
  let prompt = Jv.get Jv.global "prompt" in
  match List.length state.results with
  | 0 ->
      set_status "No entries to mark as read."
  | n when n < 10 ->
      let msg = Printf.sprintf "Mark all %d entries as read?" n in
      let ok =
        Jv.call confirm "call" [|Jv.undefined; Jv.of_string msg|] |> Jv.to_bool
      in
      if ok then mark_all_as_read () else ()
  | n ->
      let msg =
        Printf.sprintf
          "Mark all entries %d as read? Type READ-%d below to confirm." n n
      in
      let user_input =
        Jv.call prompt "call" [|Jv.undefined; Jv.of_string msg|] |> Jv.to_string
      in
      let expected_input = Printf.sprintf "READ-%d" n in
      if String.equal user_input expected_input then mark_all_as_read ()
      else set_status "Mark all as read cancelled."

let search_add_remove_starred evt =
  let q_el = get_element_by_id_exn "q" in
  let current_q = El.prop El.Prop.value q_el in
  let text = Jstr.v "+starred" in
  let new_q = add_or_remove_substring current_q text in
  El.set_prop El.Prop.value new_q q_el ;
  search ()

let search_add_remove_unread evt =
  let q_el = get_element_by_id_exn "q" in
  let current_q = El.prop El.Prop.value q_el in
  let text = Jstr.v "+unread" in
  let new_q = add_or_remove_substring current_q text in
  El.set_prop El.Prop.value new_q q_el ;
  search ()

let setup_handlers () =
  (* Hook up search-form submit event handler *)
  let form_el = get_element_by_id_exn "search-form" in
  let submit = Ev.Type.create (Jstr.v "submit") in
  Ev.listen submit
    (fun e -> Ev.prevent_default e ; set_status "searching ..." ; search ())
    (El.as_target form_el)
  |> ignore ;
  (* Hook up save-offline btn click handler *)
  let n = 100 in
  let offline_btn_el = get_element_by_id_exn "save-offline" in
  Ev.listen Ev.click (prefetch_top_n ~n) (El.as_target offline_btn_el) |> ignore ;
  (* Hook up mark-all-read btn click handler *)
  let mark_all_read_btn_el = get_element_by_id_exn "mark-all-read" in
  Ev.listen Ev.click confirm_mark_all_as_read
    (El.as_target mark_all_read_btn_el)
  |> ignore ;
  (* Hook up toggle-starred btn click handler *)
  let toggle_starred_btn_el = get_element_by_id_exn "toggle-starred" in
  Ev.listen Ev.click search_add_remove_starred
    (El.as_target toggle_starred_btn_el)
  |> ignore ;
  (* Hook up toggle-unread btn click handler *)
  let toggle_unread_btn_el = get_element_by_id_exn "toggle-unread" in
  Ev.listen Ev.click search_add_remove_unread
    (El.as_target toggle_unread_btn_el)
  |> ignore ;
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
      set_status "Failed to setup service worker!"

let () =
  setup_handlers () ;
  (* Initial load *)
  let q_el = get_element_by_id_exn "q" in
  El.set_at At.Name.value (Some (Jstr.of_string "@30-days-old +unread")) q_el ;
  search () ;
  render_nav ()

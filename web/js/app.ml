open Brr
module Fetch = Brr_io.Fetch
module Message = Brr_io.Message
module Sw = Brr_webworkers.Service_worker
open Elfeed_offline_web
open Util
open Nav
module Msg = Elfeed_shared.Elfeed_message
open Brr_lwd

let entries_doc = Lwd.get State.update_entries |> Lwd.map ~f:(fun _ -> ())

let update_entries () =
  let root = Lwd.observe entries_doc in
  let render () =
    Lwd.quick_sample root ;
    let webids_to_update =
      (State.state.tags_removed |> Hashtbl.to_seq_keys |> List.of_seq)
      @ (State.state.tags_added |> Hashtbl.to_seq_keys |> List.of_seq)
      @ State.state.results
    in
    (* Update entry state *)
    webids_to_update
    |> List.map (fun webid -> Hashtbl.find State.state.entries webid)
    |> List.map (Entry.update_entry_tags ~state:State.state)
    |> List.iter (fun (e : State.entry) ->
        Hashtbl.replace State.state.entries e.webid e ) ;
    (* Re-filter results based on updated tags and current query *)
    let query = get_query () |> Jstr.to_string in
    let updated_entries =
      State.state.results
      |> List.map (fun webid -> Hashtbl.find State.state.entries webid)
      |> Offline_search.filter_results ~query
      |> List.map (fun (e : State.entry) -> e.webid)
    in
    State.state.results <- updated_entries ;
    (* Trigger a rerender of the UI components *)
    State.bump_epoch ()
  in
  Lwd.set_on_invalidate root render ;
  render ()

let results_sidebar_doc : Elwd.t Lwd.t =
  let children : El.t Lwd_seq.t Lwd.t =
    Lwd.get State.epoch_v
    |> Lwd.map ~f:(fun _ ->
        State.state.results
        |> List.map (fun webid -> Hashtbl.find State.state.entries webid)
        |> List.map (fun entry -> Entry.make_entry entry)
        |> Lwd_seq.of_list )
  in
  (* container for results; children are reactive *)
  Elwd.div ~at:[`P (At.class' (Jstr.v "results"))] [`S children]

let mount_into (host : El.t) (doc : Elwd.t Lwd.t) =
  let root = Lwd.observe doc in
  let render () = El.set_children host [Lwd.quick_sample root] in
  Lwd.set_on_invalidate root (fun _ ->
      G.request_animation_frame (fun _ -> render ()) |> ignore ) ;
  render ()

let prefetch_top_n ?(n = 30) _click_evt =
  let hashes =
    State.state.results |> List.take n
    |> List.map (fun webid ->
        Hashtbl.find State.state.entries webid |> fun e -> e.content_hash )
  in
  if Msg.request_prefetch hashes then ()
  else set_status "No service worker found."

let confirm_cache_delete _click_evt =
  let confirm = Jv.get Jv.global "confirm" in
  let msg = "Are you sure you want to clear the offline cache?" in
  let ok =
    Jv.call confirm "call" [|Jv.undefined; Jv.of_string msg|] |> Jv.to_bool
  in
  if ok then
    let container = Sw.Container.of_navigator G.navigator in
    match Sw.Container.controller container with
    | Some w ->
        let worker = Sw.as_worker w in
        let msg = Msg.Delete_cache |> Msg.to_jv in
        Brr_webworkers.Worker.post worker msg ;
        set_status "Requested cache deletion."
    | None ->
        set_status "No service worker found."
  else ()

let update_app_state data =
  let _entries =
    data |> Jv.to_jv_list
    |> List.filter (Jv.has "content")
    |> List.map Entry.entry_of_jv
  in
  List.iter
    (fun (e : State.entry) -> Hashtbl.replace State.state.entries e.webid e)
    _entries ;
  let results = List.map (fun (e : State.entry) -> e.webid) _entries in
  State.state.results <- results ;
  State.bump_update_entries ()

let display_results response =
  let open Fut.Result_syntax in
  let* data = response |> Fetch.Response.as_body |> Fetch.Body.json in
  (* NOTE: We are always using offline search, since we are being cache-first
     now. This means any discrepancies between the two searches are more likely
     to be user visible than before. *)
  update_app_state data ;
  ( match State.state.selected with
  | Some webid ->
      if not (List.mem webid State.state.results) then close_entry ()
  | None ->
      () ) ;
  let n = List.length State.state.results in
  let status = "found " ^ string_of_int n ^ " items" in
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
      | 401 ->
          Fut.error
            (Jv.Error.v
               (Jstr.v
                  "Unauthorized: Login using your Elfeed offline credentials" ) )
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
  try
    match Msg.of_jv data with
    | Prefetch_started {total} ->
        set_status (Printf.sprintf "Starting… 0/%d" total)
    | Prefetch_done {total} ->
        set_status (Printf.sprintf "Saved %d items." total)
    | Prefetch_progress {done_; total} ->
        set_status (Printf.sprintf "Saving… %d/%d" done_ total)
    | Prefetch_error {msg} ->
        set_status (Printf.sprintf "Error: %s" msg)
    | Search_update {delay} ->
        if delay < 0.5 then
          (* When the previous response was probably not "read" by the user,
           automatically update the UI. *)
          search ()
        else
          set_status
            "UPDATED results available. Submit search to see updated results."
    | Cache_cleared status ->
        if status then set_status "Offline cache cleared."
        else set_status "Failed to clear offline cache."
    | Offline_tags updates ->
        List.iter
          (fun {Msg.webid; tags; action} ->
            match action with
            | `Add ->
                State.add_tags webid tags
            | `Remove ->
                State.remove_tags webid tags )
          updates ;
        State.bump_update_entries ()
    | Prefetch_request _ | Delete_cache | Tag_update _ ->
        Console.warn
          [Jv.of_string "Received unexpected message from SW in app.ml"; data] ;
        ()
  with Msg.Parse_error _ ->
    Console.log [Jv.of_string "Failed to parse message received in app.ml"; data]

let mark_all_as_read _ =
  List.iter
    (fun webid -> State.remove_tags webid ["unread"])
    State.state.results ;
  State.bump_update_entries ()

let confirm_mark_all_as_read evt =
  Ev.prevent_default evt ;
  Ev.stop_propagation evt ;
  let confirm = Jv.get Jv.global "confirm" in
  let prompt = Jv.get Jv.global "prompt" in
  match List.length State.state.results with
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
  set_query new_q ; search ()

let search_add_remove_unread evt =
  let q_el = get_element_by_id_exn "q" in
  let current_q = El.prop El.Prop.value q_el in
  let text = Jstr.v "+unread" in
  let new_q = add_or_remove_substring current_q text in
  set_query new_q ; search ()

let setup_handlers () =
  (* Hook up changes to q input *)
  let q_el = get_element_by_id_exn "q" in
  Ev.listen Ev.change
    (fun _e ->
      let q = El.prop El.Prop.value q_el |> Jstr.trim in
      State.state.search_query <- Jstr.to_string q )
    (El.as_target q_el)
  |> ignore ;
  (* Hook up hashchange so that history works correctly *)
  Ev.listen Ev.hashchange
    (fun _e ->
      Console.log [Jv.of_string "hashchange event"] ;
      Location.set_state_from_location_hash () ;
      Util.set_query (Jstr.of_string State.state.search_query) ;
      search () )
    (Window.as_target G.window)
  |> ignore ;
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
  (* Hook up clear-cache btn click handler *)
  let clear_cache_btn_el = get_element_by_id_exn "clear-cache" in
  Ev.listen Ev.click confirm_cache_delete (El.as_target clear_cache_btn_el)
  |> ignore ;
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
  (* Mount sidebar results *)
  let results_el = get_element_by_id_exn "results" in
  mount_into results_el results_sidebar_doc ;
  (* Hook up entries updates *)
  update_entries () ;
  (* Set state from URL params *)
  Location.set_state_from_location_hash () ;
  Location.hook_location_update () ;
  (* Setup heartbeat *)
  Heartbeat.heartbeat () ;
  (* Initial load *)
  let q_el = get_element_by_id_exn "q" in
  El.set_at At.Name.value (Some (Jstr.of_string State.state.search_query)) q_el ;
  search () ;
  State.bump_epoch ()

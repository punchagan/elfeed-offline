open Brr
open State
open Util
module Clipboard = Brr_io.Clipboard

module Tags = struct
  let post_tags_update () =
    let module Msg = Elfeed_shared.Elfeed_message in
    let tags_added =
      Hashtbl.fold
        (fun webid tags acc -> {Msg.webid; tags; action= `Add} :: acc)
        state.tags_added []
    in
    let tags_removed =
      Hashtbl.fold
        (fun webid tags acc -> {Msg.webid; tags; action= `Remove} :: acc)
        state.tags_removed []
    in
    let updates = tags_added @ tags_removed in
    if List.length updates = 0 then ()
    else
      let module Sw = Brr_webworkers.Service_worker in
      let container = Sw.Container.of_navigator G.navigator in
      match Sw.Container.controller container with
      | Some w ->
          let worker = Sw.as_worker w in
          let msg = Msg.Tag_update updates |> Msg.to_jv in
          Brr_webworkers.Worker.post worker msg
      | None ->
          set_status "No service worker found."

  let mark_entry_as_read web_id =
    State.remove_tags web_id ["unread"] ;
    post_tags_update ()

  let mark_entry_as_unread web_id =
    State.add_tags web_id ["unread"] ;
    post_tags_update ()

  let star_entry web_id =
    State.add_tags web_id ["starred"] ;
    post_tags_update ()

  let unstar_entry web_id =
    State.remove_tags web_id ["starred"] ;
    post_tags_update ()
end

let status_msg = Lwd.var ""

let set_status_msg msg = Lwd.update (fun _ -> msg) status_msg

let hook_nav_status () =
  let root = Lwd.observe (Lwd.get status_msg) in
  let render () =
    let msg = Lwd.quick_sample root in
    let status_el = get_element_by_id_exn "nav-status" in
    El.set_at At.Name.class' (Some (Jstr.v "empty")) status_el ;
    set_text status_el msg ;
    G.request_animation_frame (fun _ ->
        El.set_at At.Name.class' (Some (Jstr.v "set")) status_el )
    |> ignore
  in
  Lwd.set_on_invalidate root (fun _ ->
      G.request_animation_frame (fun _ -> render ()) |> ignore ) ;
  render ()

let open_selected_entry () =
  match state.selected_index with
  | None ->
      ()
  | Some idx -> (
      let opened = List.nth_opt state.results idx in
      match opened with
      | None ->
          ()
      | Some webid ->
          (* Set reading mode *)
          Document.body G.document
          |> El.set_class (Jstr.of_string "reading") true ;
          (* Set focus to iframe for arrow keys, etc to work for navigation *)
          let content_el = get_element_by_id_exn "content" in
          El.set_has_focus true content_el ;
          State.state.opened <- Some webid ;
          State.bump_epoch () )

let close_entry () =
  Document.body G.document |> El.set_class (Jstr.of_string "reading") false ;
  let content_el = get_element_by_id_exn "content" in
  El.set_at At.Name.src (Some (Jstr.v "/start.html")) content_el ;
  state.opened <- None ;
  set_status_msg "" ;
  State.bump_epoch ()

let open_prev_entry () =
  match state.opened with
  | None ->
      ()
  | Some webid -> (
      let results = state.results in
      let current_index =
        List.find_index (fun id -> id = webid) state.results
      in
      match (current_index, state.selected_index) with
      | Some index, _ | _, Some index ->
          if index > 0 then (
            let prev_webid = List.nth results (index - 1) in
            state.opened <- Some prev_webid ;
            state.selected_index <- Some (index - 1) ;
            State.bump_epoch () )
      | _ ->
          () )

let open_next_entry () =
  match state.opened with
  | None ->
      ()
  | Some webid -> (
      let results = state.results in
      let current_index =
        List.find_index (fun id -> id = webid) state.results
      in
      match (current_index, state.selected_index) with
      | Some index, _ | _, Some index ->
          if index < List.length results - 1 then (
            let next_index =
              if Option.is_some current_index then index + 1 else index
            in
            let next_webid = List.nth results next_index in
            state.selected_index <- Some next_index ;
            state.opened <- Some next_webid ;
            State.bump_epoch () )
      | _ ->
          () )

let select_next_entry () =
  ( match state.selected_index with
  | None ->
      state.selected_index <- Some 0
  | Some idx ->
      let new' = min (List.length state.results - 1) (idx + 1) in
      state.selected_index <- Some new' ) ;
  State.bump_epoch ()

let select_prev_entry () =
  ( match state.selected_index with
  | None ->
      state.selected_index <- Some (List.length state.results - 1)
  | Some idx ->
      let new' = max 0 (idx - 1) in
      state.selected_index <- Some new' ) ;
  State.bump_epoch ()

let copy_entry_url () =
  let open_entry =
    match state.opened with
    | None ->
        None
    | Some webid ->
        Hashtbl.find_opt state.entries webid
  in
  match open_entry with
  | None ->
      ()
  | Some entry ->
      let link = entry.link |> Jstr.v in
      let clipboard = Clipboard.of_navigator G.navigator in
      Fut.await (Clipboard.write_text clipboard link) (fun result ->
          match result with
          | Ok () ->
              set_status_msg "URL copied to clipboard"
          | Error _ ->
              set_status_msg "Failed to copy URL to clipboard" )

let share_entry () =
  let open_entry =
    match state.opened with
    | None ->
        None
    | Some webid ->
        Hashtbl.find_opt state.entries webid
  in
  match open_entry with
  | None ->
      ()
  | Some entry -> (
      let nav_jv = Navigator.to_jv G.navigator in
      match Jv.get nav_jv "share" with
      | t when Jv.is_none t ->
          Console.log [Jv.of_string "Share API not supported"] ;
          copy_entry_url ()
      | share ->
          let d = Jv.obj [||] in
          Jv.set d "title" (Jv.of_string entry.title) ;
          Jv.set d "url" (Jv.of_string entry.link) ;
          let selection =
            let iframe = get_element_by_id_exn "content" in
            let doc = Jv.get (El.to_jv iframe) "contentDocument" in
            let s = Jv.call doc "getSelection" [||] in
            Jv.call s "toString" [||]
          in
          Jv.set d "text" selection ;
          let p = Jv.call nav_jv "share" [|d|] in
          Fut.await (Fut.of_promise' p ~ok:Fun.id ~error:Fun.id) (function
            | Ok _ ->
                set_status_msg "Entry shared successfully"
            | Error _ ->
                set_status_msg "Failed to share entry" ) )

let browse_entry () =
  let open_entry =
    match state.opened with
    | None ->
        None
    | Some webid ->
        Hashtbl.find_opt state.entries webid
  in
  match open_entry with
  | None ->
      ()
  | Some entry ->
      let link = entry.link |> Jstr.v in
      Window.open' G.window link |> ignore

let mark_as_read () =
  match state.opened with
  | None ->
      ()
  | Some webid ->
      Tags.mark_entry_as_read webid

let mark_as_unread () =
  match state.opened with
  | None ->
      ()
  | Some webid ->
      Tags.mark_entry_as_unread webid

let star_entry () =
  match state.opened with None -> () | Some webid -> Tags.star_entry webid

let unstar_entry () =
  match state.opened with None -> () | Some webid -> Tags.unstar_entry webid

let focus_search_input () =
  let search_input_el = get_element_by_id_exn "q" in
  El.set_has_focus true search_input_el

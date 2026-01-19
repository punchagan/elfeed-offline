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

let close_entry _ =
  Document.body G.document |> El.set_class (Jstr.of_string "reading") false ;
  let content_el = get_element_by_id_exn "content" in
  El.set_at At.Name.src (Some (Jstr.v "about:blank")) content_el ;
  state.opened <- None ;
  set_status_msg "" ;
  State.bump_epoch ()

let goto_prev_entry _ =
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

let goto_next_entry _ =
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

let copy_entry_url _ =
  match state.opened with
  | None ->
      ()
  | Some webid ->
      let link =
        Hashtbl.find state.entries webid |> (fun x -> x.link) |> Jstr.v
      in
      let clipboard = Clipboard.of_navigator G.navigator in
      Fut.await (Clipboard.write_text clipboard link) (fun result ->
          match result with
          | Ok () ->
              set_status_msg "URL copied to clipboard"
          | Error _ ->
              set_status_msg "Failed to copy URL to clipboard" )

let mark_as_read _ =
  match state.opened with
  | None ->
      ()
  | Some webid ->
      Tags.mark_entry_as_read webid

let mark_as_unread _ =
  match state.opened with
  | None ->
      ()
  | Some webid ->
      Tags.mark_entry_as_unread webid

let star_entry _ =
  match state.opened with None -> () | Some webid -> Tags.star_entry webid

let unstar_entry _ =
  match state.opened with None -> () | Some webid -> Tags.unstar_entry webid

open Brr
open Util
module Clipboard = Brr_io.Clipboard
open State
open Brr_lwd

let status_msg = ref ""

let set_button_enabled el enabled =
  let enabled_attr = if enabled then None else Some (Jstr.v "true") in
  El.set_at At.Name.disabled enabled_attr el ;
  El.set_at (Jstr.v "aria-disabled")
    (Some (Jstr.v (if enabled then "false" else "true")))
    el

let set_button_visible el visible =
  let style_attr = Jstr.of_string "style" in
  if visible then El.set_at style_attr None el
  else El.set_at style_attr (Some (Jstr.v "display: none;")) el

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

let render_nav () =
  let mark_read_btn = get_element_by_id_exn "mark-read" in
  let mark_unread_btn = get_element_by_id_exn "mark-unread" in
  let star_btn = get_element_by_id_exn "star-entry" in
  let unstar_btn = get_element_by_id_exn "unstar-entry" in
  let copy_url_btn = get_element_by_id_exn "copy-url" in
  let back_btn_el = get_element_by_id_exn "back" in
  let title_el = get_element_by_id_exn "nav-title" in
  let feed_el = get_element_by_id_exn "nav-feed" in
  let entry_nav_el = get_element_by_id_exn "entry-nav" in
  match state.selected with
  | None ->
      (* Hide the entry-nav section *)
      set_button_visible entry_nav_el false ;
      set_text title_el "" ;
      set_text feed_el ""
  | Some s ->
      set_button_visible entry_nav_el true ;
      (* Read/Unread buttons *)
      set_button_enabled mark_read_btn true ;
      set_button_enabled mark_unread_btn true ;
      let entry = Hashtbl.find state.entries s in
      set_button_visible mark_read_btn entry.is_unread ;
      set_button_visible mark_unread_btn (not entry.is_unread) ;
      (* Star/Unstar buttons *)
      set_button_enabled star_btn (not entry.is_starred) ;
      set_button_enabled unstar_btn entry.is_starred ;
      set_button_visible star_btn (not entry.is_starred) ;
      set_button_visible unstar_btn entry.is_starred ;
      (* Other buttons *)
      set_button_enabled back_btn_el true ;
      set_button_enabled copy_url_btn true ;
      let title = entry.title in
      set_text title_el title ;
      let feed = entry.feed.title in
      set_text feed_el feed ;
      set_open_original (Some (Jstr.v entry.link))

let render_nav_status () =
  let status_el = get_element_by_id_exn "nav-status" in
  El.set_at At.Name.class' (Some (Jstr.v "empty")) status_el ;
  set_text status_el !status_msg ;
  G.request_animation_frame (fun _ ->
      El.set_at At.Name.class' (Some (Jstr.v "set")) status_el )
  |> ignore

(* HACK: To reuse the existing navbar in the HTML *)
let navbar_doc = Lwd.get State.epoch_v |> Lwd.map ~f:(fun _ -> ())

let hook_render_nav doc =
  let root = Lwd.observe doc in
  let render () =
    Lwd.quick_sample root ; render_nav () ; render_nav_status ()
  in
  Lwd.set_on_invalidate root (fun _ ->
      G.request_animation_frame (fun _ -> render ()) |> ignore ) ;
  render ()

let tag_update_success status =
  let msg =
    (* Request has been cached by service worker *)
    if status = 202 then "Tags will be updated when online"
    else "Tag data updated successfully"
  in
  status_msg := msg ;
  render_nav_status () ;
  submit_search_form ()

let tag_update_failure () =
  status_msg := "Failed to update tag data!" ;
  render_nav_status ()

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

let close_entry _ =
  Document.body G.document |> El.set_class (Jstr.of_string "reading") false ;
  let content_el = get_element_by_id_exn "content" in
  El.set_at At.Name.src (Some (Jstr.v "about:blank")) content_el ;
  state.selected <- None ;
  status_msg := "" ;
  State.bump_epoch ()

let setup_nav_handlers () =
  (* Hook up render to navbar_doc *)
  hook_render_nav navbar_doc ;
  (* Hook up back-btn click handler *)
  let back_btn_el = get_element_by_id_exn "back" in
  Ev.listen Ev.click close_entry (El.as_target back_btn_el) |> ignore ;
  (* Hook up mark-as-read handler *)
  let mark_read_btn_el = get_element_by_id_exn "mark-read" in
  Ev.listen Ev.click
    (fun _ ->
      match state.selected with
      | None ->
          ()
      | Some webid ->
          mark_entry_as_read webid )
    (El.as_target mark_read_btn_el)
  |> ignore ;
  (* Hook up mark-as-unread handler *)
  let mark_unread_btn_el = get_element_by_id_exn "mark-unread" in
  Ev.listen Ev.click
    (fun _ ->
      match state.selected with
      | None ->
          ()
      | Some webid ->
          mark_entry_as_unread webid )
    (El.as_target mark_unread_btn_el)
  |> ignore ;
  (* Hook up star-entry handler *)
  let star_btn_el = get_element_by_id_exn "star-entry" in
  Ev.listen Ev.click
    (fun _ ->
      match state.selected with None -> () | Some webid -> star_entry webid )
    (El.as_target star_btn_el)
  |> ignore ;
  (* Hook up unstar-entry handler *)
  let unstar_btn_el = get_element_by_id_exn "unstar-entry" in
  Ev.listen Ev.click
    (fun _ ->
      match state.selected with None -> () | Some webid -> unstar_entry webid )
    (El.as_target unstar_btn_el)
  |> ignore ;
  (* Hook up copy-url handler *)
  let copy_url_btn_el = get_element_by_id_exn "copy-url" in
  Ev.listen Ev.click
    (fun _ ->
      match state.selected with
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
                  status_msg := "URL copied to clipboard" ;
                  render_nav_status ()
              | Error _ ->
                  status_msg := "Failed to copy URL to clipboard" ;
                  render_nav_status () ) )
    (El.as_target copy_url_btn_el)
  |> ignore

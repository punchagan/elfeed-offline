open Brr
open Util
open State
open Brr_lwd

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

let replace_iframe_location (el : El.t) (path : Jstr.t) =
  let win = Jv.get (El.to_jv el) "contentWindow" in
  let loc = Jv.get win "location" in
  let pathname = Jv.get loc "pathname" in
  if not (Jstr.equal (Jv.to_jstr pathname) path) then
    ignore @@ Jv.call loc "replace" [|Jv.of_jstr path|]

let render_nav () =
  let has_share_api = Jv.get Jv.global "navigator" |> Jv.has "share" in
  let mark_read_btn = get_element_by_id_exn "mark-read" in
  let mark_unread_btn = get_element_by_id_exn "mark-unread" in
  let star_btn = get_element_by_id_exn "star-entry" in
  let unstar_btn = get_element_by_id_exn "unstar-entry" in
  let copy_url_btn = get_element_by_id_exn "copy-url" in
  let share_btn = get_element_by_id_exn "share-entry" in
  let close_btn_el = get_element_by_id_exn "close-entry" in
  let next_btn_el = get_element_by_id_exn "next-entry" in
  let prev_btn_el = get_element_by_id_exn "prev-entry" in
  let title_el = get_element_by_id_exn "nav-title" in
  let feed_el = get_element_by_id_exn "nav-feed" in
  let entry_nav_el = get_element_by_id_exn "entry-nav" in
  match state.opened with
  | None ->
      (* Hide the entry-nav section *)
      set_button_visible entry_nav_el false ;
      set_text title_el "" ;
      set_text feed_el ""
  | Some opened_id -> (
      let entry = Hashtbl.find_opt state.entries opened_id in
      match entry with
      | None ->
          ()
      | Some entry ->
          (* Set IFrame source if required *)
          let content_el = get_element_by_id_exn "content" in
          let content_url =
            Printf.sprintf "/elfeed/content/%s" entry.content_hash |> Jstr.v
          in
          replace_iframe_location content_el content_url ;
          (* Show the entry-nav section *)
          set_button_visible entry_nav_el true ;
          (* Read/Unread buttons *)
          set_button_enabled mark_read_btn true ;
          set_button_enabled mark_unread_btn true ;
          set_button_visible mark_read_btn entry.is_unread ;
          set_button_visible mark_unread_btn (not entry.is_unread) ;
          (* Star/Unstar buttons *)
          set_button_enabled star_btn (not entry.is_starred) ;
          set_button_enabled unstar_btn entry.is_starred ;
          set_button_visible star_btn (not entry.is_starred) ;
          set_button_visible unstar_btn entry.is_starred ;
          (* Next/Prev buttons *)
          let results = state.results in
          let current_index =
            List.find_index (fun id -> id = opened_id) state.results
          in
          let index_ =
            if Option.is_some current_index then current_index
            else state.selected_index
          in
          ( match index_ with
          | None ->
              set_button_enabled next_btn_el false ;
              set_button_enabled prev_btn_el false
          | Some index ->
              let has_next =
                (* Enable next button if the currently opened item is not in
                   the results list, and there's only one item remaining. *)
                (List.length results = 1 && current_index = None)
                || index < List.length results - 1
              in
              set_button_enabled next_btn_el has_next ;
              let has_prev = index > 0 in
              set_button_enabled prev_btn_el has_prev ) ;
          (* Other buttons *)
          set_button_enabled close_btn_el true ;
          set_button_visible share_btn has_share_api ;
          set_button_enabled share_btn has_share_api ;
          set_button_visible copy_url_btn (not has_share_api) ;
          set_button_enabled copy_url_btn (not has_share_api) ;
          (* Set title and feed name *)
          let title = entry.title in
          set_text title_el title ;
          let feed = entry.feed.title in
          set_text feed_el feed ;
          set_open_original (Some (Jstr.v entry.link)) )

(* HACK: To reuse the existing navbar in the HTML *)
let navbar_doc = Lwd.get State.epoch_v |> Lwd.map ~f:(fun _ -> ())

let hook_render_nav doc =
  let root = Lwd.observe doc in
  let render () = Lwd.quick_sample root ; render_nav () in
  Lwd.set_on_invalidate root (fun _ ->
      G.request_animation_frame (fun _ -> render ()) |> ignore ) ;
  render ()

let setup_nav_handlers () =
  (* Hook up render to navbar_doc *)
  hook_render_nav navbar_doc ;
  Actions.hook_nav_status () ;
  (* Hook up close-btn click handler *)
  let close_btn_el = get_element_by_id_exn "close-entry" in
  Ev.listen Ev.click
    (fun _evt -> Actions.close_entry ())
    (El.as_target close_btn_el)
  |> ignore ;
  (* Hook up prev-btn click handler *)
  let prev_btn_el = get_element_by_id_exn "prev-entry" in
  Ev.listen Ev.click
    (fun _evt -> Actions.open_prev_entry ())
    (El.as_target prev_btn_el)
  |> ignore ;
  (* Hook up next-btn click handler *)
  let next_btn_el = get_element_by_id_exn "next-entry" in
  Ev.listen Ev.click
    (fun _evt -> Actions.open_next_entry ())
    (El.as_target next_btn_el)
  |> ignore ;
  (* Hook up mark-as-read handler *)
  let mark_read_btn_el = get_element_by_id_exn "mark-read" in
  Ev.listen Ev.click
    (fun _evt -> Actions.mark_as_read ())
    (El.as_target mark_read_btn_el)
  |> ignore ;
  (* Hook up mark-as-unread handler *)
  let mark_unread_btn_el = get_element_by_id_exn "mark-unread" in
  Ev.listen Ev.click
    (fun _evt -> Actions.mark_as_unread ())
    (El.as_target mark_unread_btn_el)
  |> ignore ;
  (* Hook up star-entry handler *)
  let star_btn_el = get_element_by_id_exn "star-entry" in
  Ev.listen Ev.click
    (fun _evt -> Actions.star_entry ())
    (El.as_target star_btn_el)
  |> ignore ;
  (* Hook up unstar-entry handler *)
  let unstar_btn_el = get_element_by_id_exn "unstar-entry" in
  Ev.listen Ev.click
    (fun _evt -> Actions.unstar_entry ())
    (El.as_target unstar_btn_el)
  |> ignore ;
  (* Hook up copy-url handler *)
  let copy_url_btn_el = get_element_by_id_exn "copy-url" in
  Ev.listen Ev.click
    (fun _evt -> Actions.copy_entry_url ())
    (El.as_target copy_url_btn_el)
  |> ignore ;
  (* Hook up share button handler *)
  let share_btn_el = get_element_by_id_exn "share-entry" in
  Ev.listen Ev.click
    (fun _evt -> Actions.share_entry ())
    (El.as_target share_btn_el)
  |> ignore

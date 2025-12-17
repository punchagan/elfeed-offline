open Brr
open Util
open Tag

type selection = {webid: Jstr.t; link: Jstr.t}

let selected : selection option ref = ref None

let set_button_enabled el enabled =
  let enabled_attr = if enabled then None else Some (Jstr.v "true") in
  El.set_at At.Name.disabled enabled_attr el ;
  El.set_at (Jstr.v "aria-disabled")
    (Some (Jstr.v (if enabled then "false" else "true")))
    el

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
  let back_btn_el = get_element_by_id_exn "back" in
  match !selected with
  | None ->
      set_button_enabled mark_read_btn false ;
      set_button_enabled mark_unread_btn false ;
      set_button_enabled back_btn_el false ;
      set_open_original None
  | Some s ->
      set_button_enabled mark_read_btn true ;
      set_button_enabled mark_unread_btn true ;
      set_button_enabled back_btn_el true ;
      set_open_original (Some s.link)

let setup_nav_handlers () =
  (* Hook up back-btn click handler *)
  let back_btn_el = get_element_by_id_exn "back" in
  Ev.listen Ev.click
    (fun _ ->
      Document.body G.document |> El.set_class (Jstr.of_string "reading") false ;
      let content_el = get_element_by_id_exn "content" in
      El.set_at At.Name.src (Some (Jstr.v "about:blank")) content_el ;
      selected := None ;
      render_nav () )
    (El.as_target back_btn_el)
  |> ignore ;
  (* Hook up mark-as-read handler *)
  let mark_read_btn_el = get_element_by_id_exn "mark-read" in
  Ev.listen Ev.click
    (fun _ ->
      match !selected with
      | None ->
          ()
      | Some s ->
          s.webid |> Jstr.to_string |> mark_entry_as_read )
    (El.as_target mark_read_btn_el)
  |> ignore ;
  (* Hook up mark-as-unread handler *)
  let mark_unread_btn_el = get_element_by_id_exn "mark-unread" in
  Ev.listen Ev.click
    (fun _ ->
      match !selected with
      | None ->
          ()
      | Some s ->
          s.webid |> Jstr.to_string |> mark_entry_as_unread )
    (El.as_target mark_unread_btn_el)
  |> ignore

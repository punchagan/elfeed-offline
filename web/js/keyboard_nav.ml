open Brr

let show_help = Lwd.var false

type shortcut = {key: string; description: string; action: unit -> unit}

type category = {name: string; shortcuts: shortcut list}

let shortcuts =
  [ { name= "Global Actions"
    ; shortcuts=
        [ { key= "?"
          ; description= "Toggle this help"
          ; action= (fun () -> Lwd.update not show_help) }
        ; { key= "/"
          ; description= "Focus search input"
          ; action= Actions.focus_search_input } ] }
  ; { name= "Search Input Actions"
    ; shortcuts=
        [ { key= "Esc" (* HACK: to get search across shortcuts by key to work *)
          ; description= "Remove focus from search input"
          ; action= (fun () -> ()) } ] }
  ; { name= "Selected Entry Actions"
    ; shortcuts=
        [ { key= "Enter"
          ; description= "Open selected entry"
          ; action= Actions.open_selected_entry }
        ; { key= "j"
          ; description= "Select the next entry"
          ; action= Actions.select_next_entry }
        ; { key= "k"
          ; description= "Select the prev entry"
          ; action= Actions.select_prev_entry } ] }
  ; { name= "Open Entry Actions"
    ; shortcuts=
        [ { key= "Escape"
          ; description= "Close entry"
          ; action= Actions.close_entry }
        ; { key= "n"
          ; description= "Open the next entry"
          ; action= Actions.open_next_entry }
        ; { key= "p"
          ; description= "Open the previous entry"
          ; action= Actions.open_prev_entry }
        ; {key= "s"; description= "Star entry"; action= Actions.star_entry}
        ; {key= "S"; description= "Unstar entry"; action= Actions.unstar_entry}
        ; { key= "u"
          ; description= "Mark as unread"
          ; action= Actions.mark_as_unread }
        ; {key= "U"; description= "Mark as read"; action= Actions.mark_as_read}
        ; { key= "y"
          ; description= "Copy entry URL"
          ; action= Actions.copy_entry_url }
        ; { key= "b"
          ; description= "Browse entry in new tab"
          ; action= Actions.browse_entry } ] } ]

let shortcuts' =
  List.flatten (List.map (fun {shortcuts} -> shortcuts) shortcuts)

let add_help_dialog () =
  let help_content =
    List.map
      (fun {shortcuts= cat_shortcuts; name} ->
        let rows =
          List.map
            (fun sc ->
              let key_el = El.v (Jstr.v "kbd") [El.txt (Jstr.v sc.key)] in
              let desc_el =
                El.v (Jstr.v "span") [El.txt (Jstr.v sc.description)]
              in
              El.v
                ~at:[At.v At.Name.class' (Jstr.v "shortcut-row")]
                (Jstr.v "div") [key_el; desc_el] )
            cat_shortcuts
        in
        let category_el =
          El.v
            ~at:[At.v At.Name.class' (Jstr.v "shortcut-category")]
            (Jstr.v "div")
            [El.txt (Jstr.v name)]
        in
        category_el :: rows )
      shortcuts
    |> List.flatten
  in
  let content_el =
    El.v
      ~at:
        [ At.v At.Name.class' (Jstr.v "shortcut-content")
        ; At.v (Jstr.v "role") (Jstr.v "document") ]
      (Jstr.v "div") help_content
  in
  let header =
    let title_el =
      El.v
        ~at:[At.v At.Name.class' (Jstr.v "shortcut-title")]
        (Jstr.v "header")
        [El.txt (Jstr.v "Keyboard Shortcuts")]
    in
    let close_btn_el =
      let btn =
        El.button
          ~at:[At.v At.Name.class' (Jstr.v "shortcut-close-btn")]
          [El.txt (Jstr.v "×")]
      in
      Ev.listen Ev.click
        (fun evt -> Lwd.update not show_help ; Ev.prevent_default evt)
        (El.as_target btn)
      |> ignore ;
      btn
    in
    El.v
      ~at:[At.v At.Name.class' (Jstr.v "shortcut-header")]
      (Jstr.v "div") [title_el; close_btn_el]
  in
  let dialog_el =
    El.v
      ~at:
        [ At.v At.Name.class' (Jstr.v "shortcut-dialog")
        ; At.id (Jstr.v "help-dialog") ]
      (Jstr.v "dialog") [header; content_el]
  in
  El.append_children (Document.body G.document) [dialog_el]

let hook_render_help_dialog () =
  let root = Lwd.observe (Lwd.get show_help) in
  let render () =
    let show = Lwd.quick_sample root in
    let dialog_el = Util.get_element_by_id_exn "help-dialog" in
    let dialog_jv = El.to_jv dialog_el in
    if show then Jv.call dialog_jv "showModal" Jv.[||] |> ignore
    else Jv.call dialog_jv "close" Jv.[||] |> ignore
  in
  Lwd.set_on_invalidate root (fun _ ->
      G.request_animation_frame (fun _ -> render ()) |> ignore ) ;
  render ()

(** Handler to trigger keyboard shortcuts*)
let handler evt =
  let el = evt |> Ev.target |> Ev.target_to_jv |> El.of_jv in
  let k = Ev.as_type evt in
  let key = k |> Ev.Keyboard.key |> Jstr.to_string in
  if Ev.Keyboard.alt_key k || Ev.Keyboard.meta_key k || Ev.Keyboard.ctrl_key k
  then ()
  else if
    El.has_tag_name (Jstr.v "input") el
    || El.has_tag_name (Jstr.v "textarea") el
  then
    match key with
    | "Escape" ->
        Ev.prevent_default evt ;
        Ev.stop_propagation evt ;
        El.set_has_focus false el
    | _ ->
        ()
  else
    match List.find_opt (fun sc -> sc.key = key) shortcuts' with
    | Some sc ->
        Ev.prevent_default evt ; Ev.stop_propagation evt ; sc.action ()
    | None ->
        ()

let setup_keyboard_handlers () =
  add_help_dialog () ;
  (* Hook up render to help_dialog_doc *)
  hook_render_help_dialog () ;
  (* Hook up keydown handler *)
  Ev.listen Ev.keydown handler (Document.as_target G.document) |> ignore ;
  (* Hook up keydown handler everytime iframe document changes *)
  let content_el = Util.get_element_by_id_exn "content" in
  Ev.listen Ev.load
    (fun _e ->
      let content_el = Util.get_element_by_id_exn "content" in
      let iframe_doc = Jv.get (El.to_jv content_el) "contentDocument" in
      Ev.listen Ev.keydown handler (iframe_doc |> El.of_jv |> El.as_target)
      |> ignore )
    (El.as_target content_el)
  |> ignore

open Brr

let add_or_remove_substring s sub =
  match Jstr.find_sub ~sub s with
  | Some pos ->
      (* Remove sub from s *)
      let n = Jstr.length sub in
      let before = Jstr.sub ~start:0 ~len:pos s in
      let after = Jstr.sub ~start:(pos + n) ~len:(Jstr.length s - pos - n) s in
      Jstr.append before after
  | None ->
      (* Add sub to s *)
      if Jstr.is_empty s then sub
      else Jstr.append (Jstr.trim s) (Jstr.append (Jstr.of_string " ") sub)

let get_element_by_id_exn id =
  match id |> Jstr.of_string |> Document.find_el_by_id G.document with
  | Some el ->
      el
  | _ ->
      "Failed to find element with id: " ^ id |> failwith

let set_text el txt =
  let text_node = El.txt (Jstr.v txt) in
  El.set_children el [text_node]

let set_status txt =
  let status_el = get_element_by_id_exn "status" in
  set_text status_el txt

let submit_search_form () =
  let submit = Ev.Type.create (Jstr.v "submit") in
  let submit_event = Ev.create submit in
  let form_el = get_element_by_id_exn "search-form" in
  Ev.dispatch submit_event (El.as_target form_el) |> ignore

let get_query () =
  let q_el = get_element_by_id_exn "q" in
  El.prop El.Prop.value q_el |> Jstr.trim

let set_query q =
  let q_el = get_element_by_id_exn "q" in
  El.set_prop El.Prop.value q q_el

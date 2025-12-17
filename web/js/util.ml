open Brr

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

let submit_search_form () =
  let submit = Ev.Type.create (Jstr.v "submit") in
  let submit_event = Ev.create submit in
  let form_el = get_element_by_id_exn "search-form" in
  Ev.dispatch submit_event (El.as_target form_el) |> ignore

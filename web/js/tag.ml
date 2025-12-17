open Brr
module Fetch = Brr_io.Fetch
open Util

let update_tag_data data =
  let url = "/elfeed/tags" |> Jstr.of_string in
  let body = data |> Json.encode |> Fetch.Body.of_jstr in
  let init = Fetch.Request.init ~body ~method':(Jstr.of_string "put") () in
  Fut.await (Fetch.url url ~init) (fun response ->
      match response with
      | Error _ ->
          set_status "Failed to update tag data!"
      | Ok response ->
          let status = Fetch.Response.status response in
          if status = 202 then
            (* Request has been cached by service worker *)
            set_status (Printf.sprintf "Tags will be updated when online")
          else submit_search_form () )

let mark_entry_as_read web_id =
  let data =
    Jv.obj
      [| ("entries", [Jstr.of_string web_id] |> Jv.of_jstr_list)
       ; ("remove", [Jstr.of_string "unread"] |> Jv.of_jstr_list) |]
  in
  update_tag_data data

let mark_entry_as_unread web_id =
  let data =
    Jv.obj
      [| ("entries", [Jstr.of_string web_id] |> Jv.of_jstr_list)
       ; ("add", [Jstr.of_string "unread"] |> Jv.of_jstr_list) |]
  in
  update_tag_data data

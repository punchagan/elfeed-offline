open Brr
module Fetch = Brr_io.Fetch
open Util

let update_tag_data data success failure =
  let url = "/elfeed/tags" |> Jstr.of_string in
  let body = data |> Json.encode |> Fetch.Body.of_jstr in
  let init = Fetch.Request.init ~body ~method':(Jstr.of_string "put") () in
  Fut.await (Fetch.url url ~init) (fun response ->
      match response with
      | Error _ ->
          failure ()
      | Ok response ->
          let status = Fetch.Response.status response in
          success status )

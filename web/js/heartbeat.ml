open Brr
open Brr_io
module Msg = Elfeed_shared.Elfeed_message

let delay_ms = 60 * 1000 (* Check once every minute *)

let timeout = 3 * 1000 (* 3 second timeout for fetch requests *)

let storage = Storage.local G.window

let key = Jstr.v "lastUpdate"

let set_last_update ts =
  Storage.set_item storage key (Jstr.v (string_of_float ts)) |> ignore

let rec heartbeat () =
  let signal =
    let s = Jv.get Jv.global "AbortSignal" in
    Jv.call s "timeout" [|Jv.of_int timeout|] |> Abort.Signal.of_jv
  in
  let init = Fetch.Request.init ~signal () in
  let req = "/elfeed/update" |> Jstr.v |> Fetch.Request.v ~init in
  Fut.await (Fetch.request req) (fun response ->
      let online_status_el = Util.get_element_by_id_exn "offline-indicator" in
      let succesful =
        match response with
        | Ok resp ->
            Fetch.Response.ok resp
        | Error _ ->
            false
      in
      ( match (succesful, response) with
      | true, Ok response ->
          Console.log [Jstr.v "Heartbeat successful"] ;
          El.set_class (Jstr.v "offline") false online_status_el ;
          State.state.online <- true ;
          (* Attempt to synchronize tags on every successful heartbeat *)
          let msg = Msg.Synchronize_tags in
          Msg.send_message msg |> ignore ;
          let open Fut.Result_syntax in
          let body = Fetch.Response.as_body response in
          (* Request prefetch for search data, if necessary  *)
          let _ =
            let* ts = Fetch.Body.text body in
            let old =
              Storage.get_item storage key |> Option.value ~default:(Jstr.v "0")
            in
            if ts > old then
              Msg.request_prefetch ~notify:false ~notify_last_update:true
                ~prefetch_search:true []
              |> ignore ;
            Fut.ok ()
          in
          ()
      | _ ->
          Console.log [Jstr.v "Heartbeat failed"] ;
          let msg =
            if not (Result.is_ok response) then
              "Elfeed-offline server is not reachable"
            else
              let resp = Result.get_ok response in
              let status = Fetch.Response.status resp in
              (* HACK for the static site build *)
              if status = 404 then
                "Demo of Elfeed-offline UI running without a backend server"
              else "The Elfeed web (Emacs) server is not responding"
          in
          let msg_el = Util.get_element_by_id_exn "offline-msg" in
          Util.set_text msg_el msg ;
          El.set_class (Jstr.v "offline") true online_status_el ;
          State.state.online <- false ) ;
      Fut.await (Fut.tick ~ms:delay_ms) (fun () -> heartbeat ()) ;
      State.bump_epoch () )

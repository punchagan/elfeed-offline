open Brr
open Brr_io
module Msg = Elfeed_shared.Elfeed_message

let delay_ms = 60 * 1000 (* Check once every minute *)

let timeout = 5 * 1000 (* 5 second timeout for fetch requests *)

let storage = Storage.local G.window

let key = Jstr.v "lastUpdate"

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
          let open Fut.Result_syntax in
          let body = Fetch.Response.as_body response in
          let _ =
            let* ts = Fetch.Body.text body in
            let old =
              Storage.get_item storage key |> Option.value ~default:(Jstr.v "0")
            in
            if ts > old then (
              Storage.set_item storage key ts |> ignore ;
              Msg.request_prefetch [] |> ignore ) ;
            Fut.ok ()
          in
          ()
      | _ ->
          Console.log [Jstr.v "Heartbeat failed"] ;
          let msg =
            if Result.is_ok response then
              "The Elfeed web (Emacs) server is not responding"
            else "Elfeed-offline server is not reachable"
          in
          let msg_el = Util.get_element_by_id_exn "offline-msg" in
          Util.set_text msg_el msg ;
          El.set_class (Jstr.v "offline") true online_status_el ;
          State.state.online <- false ) ;
      Fut.await (Fut.tick ~ms:delay_ms) (fun () -> heartbeat ()) ;
      State.bump_epoch () )

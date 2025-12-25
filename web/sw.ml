open Brr
open Brr_io
module Cache = Fetch.Cache
module Cache_storage = Cache.Storage
module Workers = Brr_webworkers
module Sw = Workers.Service_worker

module Config = struct
  let c_shell = "shell-v1" |> Jstr.v

  let c_content = "content-v1" |> Jstr.v

  let caches = [c_shell; c_content]

  let shell =
    ["/"; "/index.html"; "/manifest.webmanifest"; "/css/app.css"; "/js/app.js"]
    |> List.map Jstr.v
end

let on_install e =
  let request_shell =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let requests = List.map Fetch.Request.v Config.shell in
    let* cache = Cache_storage.open' storage Config.c_shell in
    Fetch.Cache.add_all cache requests
  in
  Ev.Extendable.wait_until (Ev.as_type e) request_shell ;
  Sw.G.skip_waiting () |> ignore ;
  Console.(log [Jv.of_string "Service worker installed and resources cached."]) ;
  ()

let on_activate e =
  let delete_old_caches =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* keys = Cache_storage.keys storage in
    let old_caches =
      List.filter (fun k -> not (List.mem k Config.caches)) keys
    in
    let _ =
      List.iter (fun k -> Cache_storage.delete storage k |> ignore) old_caches
    in
    Fut.ok ()
  in
  Sw.Clients.claim Sw.G.clients |> ignore ;
  Ev.Extendable.wait_until (Ev.as_type e) delete_old_caches ;
  Console.(log [Jv.of_string "Service worker activated."]) ;
  ()

let () =
  let self = Ev.target_of_jv Jv.global in
  Ev.listen Ev.install on_install self |> ignore ;
  Ev.listen Ev.activate on_activate self |> ignore

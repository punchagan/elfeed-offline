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

  let alternate_search_param = "@999-days-old"
end

(** Helpers to return content by strategy  *)
module Fetch_strategy = struct
  let _network_request_and_cache ~cache request =
    let open Fut.Result_syntax in
    let* response = Fetch.request request in
    let clone = Fetch.Response.of_response response in
    let status = Fetch.Response.status response in
    (* Cache the request only on success *)
    if status >= 200 && status < 300 then
      Fetch.Cache.put cache request clone |> ignore ;
    Fut.ok response

  let cache_first request cache_name =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage cache_name in
    let* cached_response = Fetch.Cache.match' cache request in
    match cached_response with
    | Some response ->
        Fut.ok response
    | None ->
        _network_request_and_cache ~cache request

  let cache_first_with_refresh request cache_name =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage cache_name in
    let* cached_response = Fetch.Cache.match' cache request in
    match cached_response with
    | Some response ->
        let _ = _network_request_and_cache ~cache request in
        Fut.ok response
    | None ->
        _network_request_and_cache ~cache request

  let cache_first_with_alternate_and_refresh request alternate cache_name =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage cache_name in
    let* cached_response = Fetch.Cache.match' cache request in
    match cached_response with
    | Some response ->
        let network_response = _network_request_and_cache ~cache request in
        (* TODO: Allow passing a callback for this await *)
        Fut.await network_response (fun _ -> ()) ;
        Fut.ok response
    | None -> (
        let* alternate_cached = Fetch.Cache.match' cache alternate in
        match alternate_cached with
        | Some response ->
            let network_response = _network_request_and_cache ~cache request in
            (* TODO: Use callback from user here *)
            Fut.await network_response (fun _ -> ()) ;
            Fut.ok response
        | None ->
            _network_request_and_cache ~cache request )
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

let on_fetch e =
  let e = Ev.as_type e in
  let request = Fetch.Ev.request e in
  let url = Fetch.Request.url request in
  let _ =
    let open Result.Syntax in
    let* uri = Uri.of_jstr url in
    let path = Uri.path uri in
    ( match path with
    (* We update the SHELL on install, so use what's in the cache! But,
         refresh for next time. *)
    | s when List.mem s Config.shell ->
        let response =
          Fetch_strategy.cache_first_with_refresh request Config.c_shell
        in
        Fetch.Ev.respond_with e response
    (* Content at a hash would never change *)
    | s when Jstr.starts_with ~prefix:(Jstr.of_string "/elfeed/content") path ->
        let response = Fetch_strategy.cache_first request Config.c_content in
        Fetch.Ev.respond_with e response
    (* Return cached response for /elfeed/search *)
    | s when Jstr.starts_with ~prefix:(Jstr.of_string "/elfeed/search") path ->
        let q_param =
          Config.alternate_search_param |> Jstr.v |> Uri.encode_component
          |> Result.get_ok
        in
        let alternate =
          Jstr.append (Jstr.v "/elfeed/search?q=") q_param |> Fetch.Request.v
        in
        let response =
          Fetch_strategy.cache_first_with_alternate_and_refresh request
            alternate Config.c_content
        in
        Fetch.Ev.respond_with e response
    | _ ->
        Console.log [Jv.of_string "SW fetch"; Jv.of_jstr path] ;
        () ) ;
    Result.ok ()
  in
  ()

let () =
  let self = Ev.target_of_jv Jv.global in
  ignore (Ev.listen Ev.install on_install self) ;
  ignore (Ev.listen Ev.activate on_activate self) ;
  ignore (Ev.listen Fetch.Ev.fetch on_fetch self)

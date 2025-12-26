open Brr
open Brr_io
module Cache = Fetch.Cache
module Cache_storage = Cache.Storage
module Workers = Brr_webworkers
module Sw = Workers.Service_worker
module Msg = Elfeed_shared.Elfeed_message

(** Configuration for caches and resources to cache  *)
module Config = struct
  let c_shell = "shell-v1" |> Jstr.v

  let c_content = "content-v1" |> Jstr.v

  let caches = [c_shell; c_content]

  let shell =
    ["/"; "/index.html"; "/manifest.webmanifest"; "/css/app.css"; "/js/app.js"]
    |> List.map Jstr.v
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

  let network_only_and_cache request cache_name =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage cache_name in
    _network_request_and_cache ~cache request

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

  let cache_first_with_alternate_and_refresh ~onrefresh request alternate
      cache_name =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage cache_name in
    let* cached_response = Fetch.Cache.match' cache request in
    match cached_response with
    | Some response ->
        let network_response = _network_request_and_cache ~cache request in
        let old = Fetch.Response.of_response response in
        Fut.await network_response (onrefresh ~old) ;
        Fut.ok response
    | None -> (
        let* alternate_cached = Fetch.Cache.match' cache alternate in
        match alternate_cached with
        | Some response ->
            let network_response = _network_request_and_cache ~cache request in
            let old = Fetch.Response.of_response response in
            Fut.await network_response (onrefresh ~old) ;
            Fut.ok response
        | None ->
            _network_request_and_cache ~cache request )
end

module Notify = struct
  let notify_all msg =
    let jv = Msg.to_jv msg in
    let open Fut.Result_syntax in
    let* clients = Sw.Clients.match_all Sw.G.clients in
    let _ = List.iter (fun client -> Sw.Client.post client jv) clients in
    Fut.ok ()

  let search_update () =
    let now = Ptime_clock.now () in
    fun ~old r ->
      match r with
      | Ok response ->
          if Fetch.Response.status response <> 200 then ()
          else
            let open Fut.Result_syntax in
            let _ =
              let* old = Fetch.Response.as_body old |> Fetch.Body.json in
              let old = Jv.to_jv_list old in
              let* new_ = Fetch.Response.as_body response |> Fetch.Body.json in
              let new_ = Jv.to_jv_list new_ in
              let old_ids =
                List.map (fun jv -> Jv.get jv "webid" |> Jv.to_string) old
              in
              let new_ids =
                List.map (fun jv -> Jv.get jv "webid" |> Jv.to_string) new_
              in
              let is_different =
                List.length old_ids <> List.length new_ids
                || List.exists (fun jv -> not (List.mem jv old_ids)) new_ids
              in
              if not is_different then Fut.ok ()
              else
                let elapsed =
                  Ptime.diff (Ptime_clock.now ()) now |> Ptime.Span.to_float_s
                in
                let (msg : Msg.t) = Search_update {delay= elapsed} in
                notify_all msg
            in
            ()
      | Error e ->
          Console.log [Jv.of_string "Search cache failed to update."; e] ;
          ()
end

module Prefetch = struct
  let alternate_search_param = "@999-days-old"

  let concurrency = 4

  (* Delay before firing prefetch_alternate_search_with_content to prevent
     overloading the server during a user action. *)
  let delay_ms = 5000

  let alternate_search_req () =
    let q_param =
      alternate_search_param |> Jstr.v |> Uri.encode_component |> Result.get_ok
    in
    Jstr.append (Jstr.v "/elfeed/search?q=") q_param |> Fetch.Request.v

  let prefetch_content ?(notify = true) hashes =
    let total = List.length hashes in
    let q = Queue.create () in
    List.iter (fun h -> Queue.push h q) hashes ;
    let done_ = ref 0 in
    let running = ref 0 in
    let finished = ref false in
    let finish_if_done () =
      if (not !finished) && !running = 0 && Queue.is_empty q then (
        finished := true ;
        if notify then
          ( match !done_ with
            | 0 ->
                let msg = "Failed to prefetch any content hashes." in
                Notify.notify_all (Prefetch_error {msg})
            | d ->
                Notify.notify_all (Prefetch_done {total= d}) )
          |> ignore )
    in
    let rec worker () =
      if Queue.is_empty q then (decr running ; finish_if_done ())
      else
        let hash = Queue.pop q in
        let request =
          Fetch.Request.v
            (Jstr.append (Jstr.v "/elfeed/content/") (Jstr.v hash))
        in
        Fut.await (Fetch_strategy.cache_first request Config.c_content)
          (fun r ->
            if notify then
              ( match r with
                | Ok _ ->
                    incr done_ ;
                    Notify.notify_all (Prefetch_progress {done_= !done_; total})
                | Error _ ->
                    Notify.notify_all (Prefetch_error {msg= hash}) )
              |> ignore ;
            (* Best-effort fetch; don’t kill the pool on error *)
            ignore (worker ()) )
    in
    if notify then Notify.notify_all (Prefetch_started {total}) |> ignore ;
    let n = min concurrency total in
    running := n ;
    for _ = 1 to n do
      worker ()
    done ;
    if notify && total = 0 then
      Notify.notify_all (Prefetch_done {total= 0}) |> ignore

  let prefetch_alternate_search_with_content () =
    let open Fut.Result_syntax in
    let request = alternate_search_req () in
    let response =
      Fetch_strategy.network_only_and_cache request Config.c_content
    in
    Fut.await response (function
      | Ok response ->
          let open Fut.Result_syntax in
          let _ =
            let* response =
              Fetch.Response.as_body response |> Fetch.Body.json
            in
            let hashes =
              response |> Jv.to_jv_list
              |> List.filter (Jv.has "content")
              |> List.map (fun jv -> Jv.get jv "content" |> Jv.to_string)
            in
            prefetch_content ~notify:false hashes ;
            Fut.ok ()
          in
          Console.log [Jv.of_string "Prefetched alternate search content."]
      | Error e ->
          Console.log
            [Jv.of_string "Failed to prefetch alternate search content."; e] )
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
        let alternate = Prefetch.alternate_search_req () in
        let response =
          Fetch_strategy.cache_first_with_alternate_and_refresh
            ~onrefresh:(Notify.search_update ()) request alternate
            Config.c_content
        in
        Fetch.Ev.respond_with e response
    | _ ->
        Console.log [Jv.of_string "SW fetch"; Jv.of_jstr path] ;
        () ) ;
    Result.ok ()
  in
  ()

let on_message e =
  let data = e |> Ev.as_type |> Message.Ev.data in
  try
    match Msg.of_jv data with
    | Prefetch_request {hashes} ->
        Prefetch.prefetch_content hashes |> ignore ;
        Fut.await (Fut.tick ~ms:Prefetch.delay_ms) (fun () ->
            Prefetch.prefetch_alternate_search_with_content () )
    | _ ->
        Console.warn [Jv.of_string "Received unexpected message type"] ;
        ()
  with Msg.Parse_error _ ->
    Console.warn [Jv.of_string "Failed to parse message in SW"] ;
    ()

let () =
  let self = Ev.target_of_jv Jv.global in
  ignore (Ev.listen Ev.install on_install self) ;
  ignore (Ev.listen Ev.activate on_activate self) ;
  ignore (Ev.listen Fetch.Ev.fetch on_fetch self) ;
  ignore (Ev.listen Message.Ev.message on_message self)

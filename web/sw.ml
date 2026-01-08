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

  let c_tags = "tags-v1" |> Jstr.v

  let caches = [c_shell; c_content; c_tags]

  let shell =
    ["/"; "/index.html"; "/manifest.webmanifest"; "/css/app.css"; "/js/app.js"]
    |> List.map Jstr.v

  let default_query = "@30-days-ago +unread"

  let alternate_query = "@999-days-old"
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
  let concurrency = 4

  (* Delay before firing prefetch_alternate_search_with_content to prevent
     overloading the server during a user action. *)
  let delay_ms = 5000

  let clear_other_cache_keys () =
    let keep_keys = [Config.default_query; Config.alternate_query] in
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage Config.c_content in
    let* keys = Cache.keys cache in
    let old_keys =
      List.filter
        (fun k ->
          let uri = Uri.of_jstr (Fetch.Request.url k) |> Result.get_ok in
          match Uri.path uri |> Jstr.to_string with
          | "/elfeed/search" -> (
              let query_params = Uri.query_params uri in
              let search_query_opt =
                Uri.Params.find (Jstr.v "q") query_params
              in
              match search_query_opt with
              | None ->
                  false
              | Some q ->
                  let q = q |> Jstr.trim |> Jstr.to_string in
                  not (List.mem q keep_keys) )
          | _ ->
              false )
        keys
    in
    List.iter (fun k -> Cache.delete cache k |> ignore) old_keys ;
    Fut.ok ()

  let alternate_search_req () =
    let q_param =
      Config.alternate_query |> Jstr.v |> Uri.encode_component |> Result.get_ok
    in
    Jstr.append (Jstr.v "/elfeed/search?q=") q_param |> Fetch.Request.v

  let request_last_update_and_notify () =
    let open Fut.Result_syntax in
    let request = Fetch.Request.v (Jstr.v "/elfeed/update") in
    let* response = Fetch.request request in
    let* body = Fetch.Response.as_body response |> Fetch.Body.text in
    let ts = float_of_string (Jstr.to_string body) in
    let msg = Msg.Set_last_update {timestamp= ts} in
    Notify.notify_all msg

  let prefetch_content ?(notify = true) ?(notify_last_update = false) hashes =
    let total = List.length hashes in
    let q = Queue.create () in
    List.iter (fun h -> Queue.push h q) hashes ;
    let done_ = ref 0 in
    let running = ref 0 in
    let finished = ref false in
    let finish_if_done () =
      if (not !finished) && !running = 0 && Queue.is_empty q then (
        finished := true ;
        if notify_last_update then request_last_update_and_notify () |> ignore ;
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

  let prefetch_alternate_search_with_content ~notify_last_update () =
    let open Fut.Result_syntax in
    let request = alternate_search_req () in
    let response =
      Fetch_strategy.network_only_and_cache request Config.c_content
    in
    Fut.await response (function
      | Ok response ->
          if not (Fetch.Response.ok response) then
            Console.log
              [ Jv.of_string "Failed to prefetch alternate search content."
              ; response ]
          else
            let _ =
              let open Fut.Result_syntax in
              let* response =
                Fetch.Response.as_body response |> Fetch.Body.json
              in
              let hashes =
                response |> Jv.to_jv_list
                |> List.filter (Jv.has "content")
                |> List.map (fun jv -> Jv.get jv "content" |> Jv.to_string)
              in
              Console.log
                [ Jv.of_string
                    (Printf.sprintf "Prefetching %d content hashes."
                       (List.length hashes) ) ] ;
              prefetch_content ~notify:false ~notify_last_update hashes ;
              clear_other_cache_keys () |> ignore ;
              Console.log [Jv.of_string "Prefetched alternate search content."] ;
              Fut.ok ()
            in
            ()
      | Error e ->
          Console.log
            [Jv.of_string "Failed to prefetch alternate search content."; e] )
end

module Tags = struct
  let pending_request = Jstr.v "pending-updates" |> Fetch.Request.v

  let mk_tags_request ~(web_id : string) ~(tags : string list)
      ~(action : [`Add | `Remove]) =
    let action_key = match action with `Add -> "add" | `Remove -> "remove" in
    let body_json =
      Jv.obj
        [| ("entries", Jv.of_jstr_list [Jstr.v web_id])
         ; (action_key, Jv.of_jstr_list (List.map Jstr.v tags)) |]
      |> Json.encode
    in
    let headers =
      Fetch.Headers.of_assoc [(Jstr.v "content-type", Jstr.v "application/json")]
    in
    let init =
      Fetch.Request.init ~method':(Jstr.v "PUT") ~headers
        ~credentials:Fetch.Request.Credentials.same_origin
        ~body:(Fetch.Body.of_jstr body_json :> Fetch.Body.init)
        ()
    in
    Fetch.Request.v ~init (Jstr.v "/elfeed/tags")

  let get_updates () =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage Config.c_tags in
    let* updates = Cache.match' cache pending_request in
    match updates with
    | None ->
        Console.log [Jv.of_string "No pending tag updates in cache."] ;
        Fut.ok []
    | Some response -> (
        let* body = Fetch.Response.as_body response |> Fetch.Body.text in
        match Json.decode body with
        | Ok jv ->
            let updates_jv = Jv.to_jv_list jv in
            let updates = List.map Msg.tag_update_of_jv updates_jv in
            Fut.ok updates
        | Error e ->
            Console.log
              [Jv.of_string "Failed to parse pending tag updates from cache."; e] ;
            Fut.error e )

  let set_updates updates =
    let body =
      Jv.of_jv_list (List.map Msg.tag_update_to_jv updates) |> Json.encode
    in
    let headers =
      Fetch.Headers.of_assoc [(Jstr.v "content-type", Jstr.v "application/json")]
    in
    let init = Fetch.Response.init ~headers ~status:200 () in
    let response =
      Fetch.Response.v ~init
        ~body:(Fetch.Body.of_jstr body :> Fetch.Body.init)
        ()
    in
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage Config.c_tags in
    let* () = Cache.put cache pending_request response in
    Fut.ok ()

  let notify_pending_updates () =
    let open Fut.Result_syntax in
    let _ =
      let* updates = get_updates () in
      if List.length updates = 0 then Fut.ok ()
      else (
        Notify.notify_all (Msg.Offline_tags updates) |> ignore ;
        let msg =
          Printf.sprintf "You have %d pending tag updates to sync."
            (List.length updates)
        in
        Console.log [Jv.of_string msg] ;
        Fut.ok () )
    in
    ()

  let sync_update ({webid; tags; action} : Msg.tag_update) =
    let request = mk_tags_request ~web_id:webid ~tags ~action in
    Fetch.request request

  (* NOTE: We replay the updates instead of combining add and remove requests
     since there could be cases in which a webid disappears on the server (if
     the entry is deleted?) and the server then responds with a 404. Instead of
     complex logic to discover the failing webid, we choose to simply replay
     the changes here. *)
  let sync_updates () =
    let open Fut.Result_syntax in
    let q = Queue.create () in
    let rec worker () =
      if Queue.is_empty q then (
        notify_pending_updates () ;
        Prefetch.prefetch_alternate_search_with_content
          ~notify_last_update:false () ;
        () )
      else
        let update = Queue.pop q in
        Fut.await (sync_update update) (function
          | Error e ->
              (* Server offline; Nothing to do *)
              let msg =
                "Tag update: failed due to network error for " ^ update.webid
              in
              Console.log [Jv.of_string msg; e] ;
              ()
          | Ok r ->
              let status = Fetch.Response.status r in
              let _ =
                let* updates = get_updates () in
                let msg =
                  if status = 200 then
                    Printf.sprintf "Tag update: succeeded for %s" update.webid
                  else if status = 404 then
                    (* webid not present on server; ignore *)
                    Printf.sprintf
                      "Tag update: webid not found on server for %s"
                      update.webid
                  else
                    Printf.sprintf "Tag update: failed for %s with status %d"
                      update.webid status
                in
                Console.log [Jv.of_string msg] ;
                Fut.await
                  ( match status with
                  | 200 | 404 ->
                      let remaining_updates =
                        List.filter (fun u -> u <> update) updates
                      in
                      set_updates remaining_updates
                  | _ ->
                      Fut.ok () )
                  (function Ok _ -> worker () | _ -> ()) ;
                Fut.ok ()
              in
              () )
    in
    let _ =
      let* updates = get_updates () in
      List.iter (fun h -> Queue.push h q) updates ;
      (if not (Queue.is_empty q) then worker ()) |> Fut.ok
    in
    ()

  let persist_tag_updates_and_sync updates =
    Fut.await (set_updates updates) (function
      | Ok _ ->
          Console.log [Jv.of_string "Tag updates persisted for later sync."] ;
          sync_updates () |> ignore
      | Error e ->
          Console.log
            [Jv.of_string "Failed to persist tag updates for later sync."; e] )
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
        Fetch.Ev.respond_with e response ;
        Tags.sync_updates ()
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
    | Prefetch_request {hashes; notify; prefetch_search; notify_last_update} ->
        let n = List.length hashes in
        if n > 0 then Prefetch.prefetch_content ~notify hashes |> ignore ;
        if prefetch_search then
          let delay_ms = if n > 0 then Prefetch.delay_ms else 0 in
          Fut.await (Fut.tick ~ms:delay_ms) (fun () ->
              Prefetch.prefetch_alternate_search_with_content
                ~notify_last_update () )
    | Delete_cache ->
        let storage = Fetch.caches () in
        Fut.await (Cache_storage.delete storage Config.c_content) (function
          | Ok _ ->
              Notify.notify_all (Cache_cleared true) |> ignore
          | Error e ->
              Notify.notify_all (Cache_cleared false) |> ignore )
    | Tag_update updates ->
        Tags.persist_tag_updates_and_sync updates |> ignore
    | Offline_tags_request ->
        Tags.notify_pending_updates ()
    | Search_update _
    | Prefetch_started _
    | Prefetch_done _
    | Prefetch_progress _
    | Prefetch_error _
    | Cache_cleared _
    | Offline_tags _
    | Set_last_update _ ->
        Console.warn [Jv.of_string "Received unexpected message type"] ;
        ()
  with
  | Msg.Parse_error _ ->
      Console.warn [Jv.of_string "Failed to parse message in SW"] ;
      ()
  | e ->
      Console.error [Jv.of_string "Unknown error while parsing message"; e] ;
      ()

let () =
  let self = Ev.target_of_jv Jv.global in
  ignore (Ev.listen Ev.install on_install self) ;
  ignore (Ev.listen Ev.activate on_activate self) ;
  ignore (Ev.listen Fetch.Ev.fetch on_fetch self) ;
  ignore (Ev.listen Message.Ev.message on_message self)

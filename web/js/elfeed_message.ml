open Brr
module Sw = Brr_webworkers.Service_worker

type tag_update = {webid: string; tags: string list; action: [`Add | `Remove]}

type t =
  (* Messages from SW to app *)
  | Search_update of {delay: float}
  | Prefetch_started of {total: int}
  | Prefetch_done of {total: int}
  | Prefetch_progress of {done_: int; total: int}
  | Prefetch_error of {msg: string}
  | Cache_cleared of bool
  | Offline_tags of tag_update list
  | Set_last_update of {timestamp: float}
  (* Messages from app to SW *)
  | Prefetch_request of
      { hashes: string list
      ; notify: bool
      ; prefetch_search: bool
      ; notify_last_update: bool }
      (** Request the service worker to prefetch entries with given hashes *)
  | Delete_cache
      (** Request the service worker to delete all cached content  *)
  | Tag_update of tag_update list
      (** Update tags stored in the service worker *)
  | Offline_tags_request
      (** Request the service worker for any stored offline tags*)
  | Synchronize_tags
      (** Request the service worker to attempt syncing tags with server*)

exception Parse_error of string

let type_ = function
  | Search_update _ ->
      "SEARCH_UPDATE"
  | Prefetch_started _ ->
      "PREFETCH_STARTED"
  | Prefetch_done _ ->
      "PREFETCH_DONE"
  | Prefetch_progress _ ->
      "PREFETCH_PROGRESS"
  | Prefetch_error _ ->
      "PREFETCH_ERROR"
  | Prefetch_request _ ->
      "PREFETCH_REQUEST"
  | Delete_cache ->
      "DELETE_CACHE"
  | Cache_cleared _ ->
      "CACHE_CLEARED"
  | Tag_update _ ->
      "TAG_UPDATE"
  | Offline_tags _ ->
      "OFFLINE_TAGS"
  | Offline_tags_request ->
      "OFFLINE_TAGS_REQUEST"
  | Set_last_update _ ->
      "SET_LAST_UPDATE"
  | Synchronize_tags ->
      "SYNCHRONIZE_TAGS"

let tag_update_to_jv {webid; tags; action} =
  let u = Jv.obj [||] in
  Jv.set u "webid" (Jv.of_string webid) ;
  Jv.set u "tags" (Jv.of_jstr_list (List.map Jstr.of_string tags)) ;
  let action_str = match action with `Add -> "ADD" | `Remove -> "REMOVE" in
  Jv.set u "action" (Jv.of_string action_str) ;
  u

let tag_update_of_jv u =
  let webid = Jv.get u "webid" |> Jv.to_string in
  let tags = Jv.get u "tags" |> Jv.to_jstr_list |> List.map Jstr.to_string in
  let action =
    match Jv.get u "action" |> Jv.to_string with
    | "ADD" ->
        `Add
    | "REMOVE" ->
        `Remove
    | x ->
        raise (Parse_error ("Unknown tag update action: " ^ x))
  in
  {webid; tags; action}

let to_jv m =
  let o = Jv.obj [||] in
  Jv.set o "type" (Jv.of_string (type_ m)) ;
  match m with
  | Search_update {delay} ->
      Jv.set o "delay" (Jv.of_float delay) ;
      o
  | Prefetch_started {total} ->
      Jv.set o "total" (Jv.of_int total) ;
      o
  | Prefetch_done {total} ->
      Jv.set o "total" (Jv.of_int total) ;
      o
  | Prefetch_progress {done_; total} ->
      Jv.set o "done" (Jv.of_int done_) ;
      Jv.set o "total" (Jv.of_int total) ;
      o
  | Prefetch_error {msg} ->
      Jv.set o "msg" (Jv.of_string msg) ;
      o
  | Prefetch_request {hashes; prefetch_search; notify; notify_last_update} ->
      Jv.set o "hashes" (Jv.of_jstr_list (List.map Jstr.of_string hashes)) ;
      Jv.set o "prefetch_search" (Jv.of_bool prefetch_search) ;
      Jv.set o "notify" (Jv.of_bool notify) ;
      Jv.set o "notify_last_update" (Jv.of_bool notify_last_update) ;
      o
  | Delete_cache ->
      o
  | Cache_cleared status ->
      Jv.set o "status" (Jv.of_bool status) ;
      o
  | Tag_update updates ->
      let jv_updates = updates |> List.map tag_update_to_jv in
      Jv.set o "updates" (Jv.of_jv_list jv_updates) ;
      o
  | Offline_tags updates ->
      let jv_updates = updates |> List.map tag_update_to_jv in
      Jv.set o "updates" (Jv.of_jv_list jv_updates) ;
      o
  | Offline_tags_request ->
      o
  | Set_last_update {timestamp} ->
      Jv.set o "timestamp" (Jv.of_float timestamp) ;
      o
  | Synchronize_tags ->
      o

let of_jv (v : Jv.t) : t =
  match Jv.get v "type" |> Jv.to_string with
  | "SEARCH_UPDATE" ->
      Search_update {delay= Jv.get v "delay" |> Jv.to_float}
  | "PREFETCH_STARTED" ->
      Prefetch_started {total= Jv.get v "total" |> Jv.to_int}
  | "PREFETCH_DONE" ->
      Prefetch_done {total= Jv.get v "total" |> Jv.to_int}
  | "PREFETCH_PROGRESS" ->
      Prefetch_progress
        { done_= Jv.get v "done" |> Jv.to_int
        ; total= Jv.get v "total" |> Jv.to_int }
  | "PREFETCH_ERROR" ->
      Prefetch_error {msg= Jv.get v "msg" |> Jv.to_string}
  | "PREFETCH_REQUEST" ->
      Prefetch_request
        { hashes= Jv.get v "hashes" |> Jv.to_jstr_list |> List.map Jstr.to_string
        ; prefetch_search= Jv.get v "prefetch_search" |> Jv.to_bool
        ; notify= Jv.get v "notify" |> Jv.to_bool
        ; notify_last_update= Jv.get v "notify_last_update" |> Jv.to_bool }
  | "DELETE_CACHE" ->
      Delete_cache
  | "CACHE_CLEARED" ->
      Cache_cleared (Jv.get v "status" |> Jv.to_bool)
  | "TAG_UPDATE" ->
      let updates_jv = Jv.get v "updates" |> Jv.to_jv_list in
      let updates = updates_jv |> List.map tag_update_of_jv in
      Tag_update updates
  | "OFFLINE_TAGS" ->
      let updates_jv = Jv.get v "updates" |> Jv.to_jv_list in
      let updates = updates_jv |> List.map tag_update_of_jv in
      Offline_tags updates
  | "OFFLINE_TAGS_REQUEST" ->
      Offline_tags_request
  | "SET_LAST_UPDATE" ->
      Set_last_update {timestamp= Jv.get v "timestamp" |> Jv.to_float}
  | "SYNCHRONIZE_TAGS" ->
      Synchronize_tags
  | x ->
      raise (Parse_error x)

let send_message msg =
  let container = Sw.Container.of_navigator G.navigator in
  match Sw.Container.controller container with
  | Some w ->
      let worker = Sw.as_worker w in
      let msg = msg |> to_jv in
      Brr_webworkers.Worker.post worker msg ;
      true
  | None ->
      false

let request_prefetch ?(notify = true) ?(notify_last_update = false)
    ?(prefetch_search = false) hashes =
  let msg =
    Prefetch_request {hashes; notify; prefetch_search; notify_last_update}
  in
  send_message msg

open Brr

type t =
  (* Messages from SW to app *)
  | Search_update of {delay: float}
  | Prefetch_started of {total: int}
  | Prefetch_done of {total: int}
  | Prefetch_progress of {done_: int; total: int}
  | Prefetch_error of {msg: string}
  (* Messages from app to SW *)
  | Prefetch_request of {hashes: string list}

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
  | Prefetch_request {hashes} ->
      Jv.set o "hashes" (Jv.of_jstr_list (List.map Jstr.of_string hashes)) ;
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
        {hashes= Jv.get v "hashes" |> Jv.to_jstr_list |> List.map Jstr.to_string}
  | x ->
      raise (Parse_error x)

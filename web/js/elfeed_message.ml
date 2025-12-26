open Brr

type t =
  | Search_update of {delay: float}
  | Prefetch_progress of {done_: int; total: int}
  | Prefetch_done of {total: int}
  | Prefetch_error of {msg: string}

exception Parse_error of string

let type_ = function
  | Search_update _ ->
      "SEARCH_UPDATE"
  | Prefetch_progress _ ->
      "PREFETCH_PROGRESS"
  | Prefetch_done _ ->
      "PREFETCH_DONE"
  | Prefetch_error _ ->
      "PREFETCH_ERROR"

let to_jv m =
  let o = Jv.obj [||] in
  Jv.set o "type" (Jv.of_string (type_ m)) ;
  match m with
  | Search_update {delay} ->
      Jv.set o "delay" (Jv.of_float delay) ;
      o
  | Prefetch_progress {done_; total} ->
      Jv.set o "done" (Jv.of_int done_) ;
      Jv.set o "total" (Jv.of_int total) ;
      o
  | Prefetch_done {total} ->
      Jv.set o "total" (Jv.of_int total) ;
      o
  | Prefetch_error {msg} ->
      Jv.set o "msg" (Jv.of_string msg) ;
      o

let of_jv (v : Jv.t) : t =
  match Jv.get v "type" |> Jv.to_string with
  | "SEARCH_UPDATE" ->
      Search_update {delay= Jv.get v "delay" |> Jv.to_float}
  | "PREFETCH_PROGRESS" ->
      Prefetch_progress
        { done_= Jv.get v "done" |> Jv.to_int
        ; total= Jv.get v "total" |> Jv.to_int }
  | "PREFETCH_DONE" ->
      Prefetch_done {total= Jv.get v "total" |> Jv.to_int}
  | "PREFETCH_ERROR" ->
      Prefetch_error {msg= Jv.get v "msg" |> Jv.to_string}
  | x ->
      raise (Parse_error x)

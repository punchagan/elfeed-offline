type feed = {title: string; url: string}

type entry =
  { webid: string
  ; title: string
  ; link: string
  ; content_hash: string
  ; feed: feed
  ; tags: string list
  ; is_unread: bool
  ; is_starred: bool
  ; published_ms: float }

type entry_map = (string, entry) Hashtbl.t

type model =
  { mutable entries: entry_map
  ; mutable results: string list
  ; mutable selected: string option }

let state = {results= []; selected= None; entries= Hashtbl.create 30}

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

type tag_map = (string, string list) Hashtbl.t

type model =
  { mutable entries: entry_map
  ; mutable results: string list
  ; mutable opened: string option
  ; mutable search_query: string
  ; mutable tags_added: tag_map
  ; mutable tags_removed: tag_map
  ; mutable online: bool }

let state =
  { entries= Hashtbl.create 30
  ; results= []
  ; opened= None
  ; search_query= "@30-days-ago +unread"
  ; tags_added= Hashtbl.create 10
  ; tags_removed= Hashtbl.create 10
  ; online= true }

(* Reactive state *)

let update_entries = Lwd.var 0

let bump_update_entries () = Lwd.update (fun n -> n + 1) update_entries

let epoch_v = Lwd.var 0

let bump_epoch () = Lwd.update (fun n -> n + 1) epoch_v

(* State update helpers *)
let add_tags webid tags =
  (* Add tags to tags_added if not present *)
  let existing =
    match Hashtbl.find_opt state.tags_added webid with
    | Some ts ->
        ts
    | None ->
        []
  in
  let new_tags =
    List.fold_left
      (fun acc t -> if List.mem t acc then acc else t :: acc)
      existing tags
  in
  Hashtbl.replace state.tags_added webid new_tags ;
  (* Also remove from tags_removed if present *)
  let removed =
    match Hashtbl.find_opt state.tags_removed webid with
    | Some ts ->
        ts
    | None ->
        []
  in
  let updated_removed = List.filter (fun t -> not (List.mem t tags)) removed in
  if updated_removed = [] then Hashtbl.remove state.tags_removed webid
  else Hashtbl.replace state.tags_removed webid updated_removed ;
  bump_update_entries ()

let remove_tags webid tags =
  (* Add tags to tags_removed if not present *)
  let existing =
    match Hashtbl.find_opt state.tags_removed webid with
    | Some ts ->
        ts
    | None ->
        []
  in
  let new_tags =
    List.fold_left
      (fun acc t -> if List.mem t acc then acc else t :: acc)
      existing tags
  in
  Hashtbl.replace state.tags_removed webid new_tags ;
  (* Also remove from tags_added if present *)
  let added =
    match Hashtbl.find_opt state.tags_added webid with
    | Some ts ->
        ts
    | None ->
        []
  in
  let updated_added = List.filter (fun t -> not (List.mem t tags)) added in
  if updated_added = [] then Hashtbl.remove state.tags_added webid
  else Hashtbl.replace state.tags_added webid updated_added ;
  bump_update_entries ()

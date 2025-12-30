open Brr
open Brr_lwd

let location_doc = Lwd.get State.epoch_v |> Lwd.map ~f:(fun _ -> ())

let set_state_from_location_hash () =
  let location = G.window |> Window.location in
  let params = Uri.fragment_params location in
  ( match Uri.Params.find (Jstr.v "q") params with
  | None ->
      ()
  | Some v ->
      (* TODO :Uri decode?? *)
      State.state.search_query <- Jstr.to_string v ) ;
  match Uri.Params.find (Jstr.v "webid") params with
  | None ->
      ()
  | Some v ->
      State.state.selected <- Some (Jstr.to_string v)

let set_location_hash () =
  let location = G.window |> Window.location in
  let fragments_assoc =
    if State.state.search_query <> "" then
      [(Jstr.v "q", Jstr.v State.state.search_query)]
    else []
  in
  let fragments =
    ( match State.state.selected with
      | Some webid ->
          (Jstr.v "webid", Jstr.v webid) :: fragments_assoc
      | None ->
          fragments_assoc )
    |> Uri.Params.of_assoc
  in
  let uri = Uri.with_fragment_params location fragments in
  Window.set_location G.window uri

let hook_location_update () =
  let root = Lwd.observe location_doc in
  let update_hash () = Lwd.quick_sample root ; set_location_hash () in
  Lwd.set_on_invalidate root update_hash ;
  update_hash ()

open Brr
module Fetch = Brr_io.Fetch

let submit = Ev.Type.create (Jstr.of_string "submit")

let get_element_by_id_exn id =
  match id |> Jstr.of_string |> Document.find_el_by_id G.document with
  | Some el ->
      el
  | _ ->
      "Failed to find element with id: " ^ id |> failwith

let set_status txt =
  let status_el = get_element_by_id_exn "status" in
  let text_node = El.txt (Jstr.v txt) in
  El.set_children status_el [text_node]

let get_query () =
  let q_el = get_element_by_id_exn "q" in
  El.prop El.Prop.value q_el |> Jstr.trim

let format_date (data : Jv.t) =
  let ms = Jv.get data "date" in
  let d = Jv.new' (Jv.get Jv.global "Date") [|ms|] in
  let year = Jv.to_int (Jv.call d "getFullYear" [||]) in
  let month = Jv.to_int (Jv.call d "getMonth" [||]) + 1 in
  let day = Jv.to_int (Jv.call d "getDate" [||]) in
  Printf.sprintf "%04d-%02d-%02d" year month day |> Jstr.of_string

let make_entry data =
  let title = Jv.get data "title" |> Jv.to_jstr in
  let title_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "title")]
      (Jstr.of_string "span")
      [El.txt title]
  in
  let feed = Jv.get data "feed" |> (fun x -> Jv.get x "title") |> Jv.to_jstr in
  let feed_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "feed")]
      (Jstr.of_string "span")
      [El.txt feed]
  in
  let date = format_date data in
  let date_el =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "date")]
      (Jstr.of_string "span")
      [El.txt date]
  in
  let entry =
    El.v
      ~at:[At.v At.Name.class' (Jstr.of_string "entry")]
      (Jstr.of_string "div")
      [date_el; title_el; feed_el]
  in
  let _ =
    Ev.listen Ev.click
      (fun _ ->
        let content_el = get_element_by_id_exn "content" in
        let content_hash = Jv.get data "content" |> Jv.to_jstr in
        let content_url =
          Jstr.append (Jstr.of_string "/elfeed/content/") content_hash
        in
        (* Set src of IFrame *)
        El.set_at At.Name.src (Some content_url) content_el ;
        (* Set reading mode *)
        Document.body G.document |> El.set_class (Jstr.of_string "reading") true ;
        () )
      (El.as_target entry)
  in
  entry

let display_results response =
  let open Fut.Result_syntax in
  let headers = Fetch.Response.headers response in
  let* data = response |> Fetch.Response.as_body |> Fetch.Body.json in
  let data_list = Jv.to_jv_list data in
  let children = data |> Jv.to_jv_list |> List.map make_entry in
  let results_el = get_element_by_id_exn "results" in
  El.set_children results_el children ;
  let n = data_list |> List.length in
  let loaded = "loaded " ^ string_of_int n ^ " items" in
  let status =
    match Fetch.Headers.find (Jstr.of_string "X-Cache") headers with
    | Some t ->
        let suffix =
          match Jstr.to_string t with
          | "HIT" ->
              " (from cache)"
          | "NEAR" ->
              " (from latest cached search)"
          | _ ->
              " (from unknown cached entry)"
        in
        loaded ^ suffix
    | None ->
        loaded ^ " (from the server)"
  in
  set_status status ; Fut.ok ()

let search () =
  let base = "/elfeed/search?q=" |> Jstr.of_string in
  let q = get_query () in
  match q |> Uri.encode_component with
  | Error e ->
      let msg =
        e |> Jv.Error.message |> Jstr.to_string
        |> fun s -> "Failed to search with error:" ^ s
      in
      set_status msg ; failwith msg
  | Ok s ->
      let open Fut.Result_syntax in
      let url = Jstr.append base s in
      let resp = Fetch.url url in
      let* response = resp in
      let status = Fetch.Response.status response in
      ignore
        ( if status <> 200 then (
            set_status "offline or backend unavailable & no cache data found" ;
            failwith "Server returned with status code: " ^ string_of_int status
            )
          else (
            Fut.await (display_results response) (fun _ -> ()) ;
            "" ) ) ;
      Fut.ok ()

let () =
  (* Hook up search-form submit event handler *)
  let form_el = get_element_by_id_exn "search-form" in
  let _listener =
    Ev.listen submit
      (fun e ->
        Ev.prevent_default e ;
        set_status "searching ..." ;
        Fut.await (search ()) (fun _ -> ()) )
      (El.as_target form_el)
  in
  (* Hook up back-btn click handler *)
  let back_btn_el = get_element_by_id_exn "back" in
  let _ =
    Ev.listen Ev.click
      (fun _ ->
        Document.body G.document
        |> El.set_class (Jstr.of_string "reading") false )
      (El.as_target back_btn_el)
  in
  (* Initial load *)
  let q_el = get_element_by_id_exn "q" in
  El.set_at At.Name.value (Some (Jstr.of_string "@30-days-old")) q_el ;
  Fut.await (search ()) (fun _ -> ())

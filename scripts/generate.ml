open Lwt.Infix

let feeds =
  [ "https://ocaml.org/planet.xml"
  ; "https://planet.emacslife.com/atom.xml"
  ; "https://planet.haskell.org/atom.xml"
  ; "https://perl.theplanetarium.org/atom.xml"
  ; "https://planet.scheme.org/atom.xml"
  ; "https://planet.clojure.in/atom.xml"
    (* FIXME: RSS support  "https://planet.lisp.org/rss20.xml" *)
    (* FIXME: RSS support  "https://planetpython.org/rss20.xml" *) ]

(* Hash function to generate webid similar to Elfeed *)
let hash_entry ~feed_url ~entry_id =
  let s = feed_url ^ entry_id in
  let hash = Hashtbl.hash s in
  Printf.sprintf "%x" hash

(* Hash function for content *)
let hash_content content =
  let hash = Hashtbl.hash content in
  Printf.sprintf "%x" hash

let text_construct_to_string (tc : Syndic.Atom.text_construct) =
  match tc with
  | Text s ->
      s
  | Html (_, s) ->
      s
  | Xhtml (_, _) ->
      "unknown xhtml"

let content_to_string (content : Syndic.Atom.content) =
  match content with
  | Syndic.Atom.Text s ->
      s
  | Syndic.Atom.Html (_, s) ->
      s
  | Syndic.Atom.Xhtml (_, _) ->
      "unknown xhtml"
  | Syndic.Atom.Mime (_, s) ->
      s
  | Syndic.Atom.Src (_, uri) ->
      Uri.to_string uri

let entry_to_json feed_url feed_title (entry : Syndic.Atom.entry) =
  let entry_id = Uri.to_string entry.Syndic.Atom.id in
  let webid = hash_entry ~feed_url ~entry_id in
  let title = text_construct_to_string entry.title in
  let link =
    match entry.links with
    | [] ->
        entry_id
    | link :: _ ->
        Uri.to_string link.Syndic.Atom.href
  in
  let content =
    match entry.content with
    | Some content_val ->
        content_to_string content_val
    | None -> (
      match entry.summary with
      | Some summary ->
          text_construct_to_string summary
      | None ->
          "" )
  in
  let wrapped_content = Elfeed_offline.Proxy.wrapped_html content in
  let content_hash = hash_content wrapped_content ^ ".html" in
  let date =
    match entry.Syndic.Atom.published with
    | Some date ->
        date
    | None ->
        entry.Syndic.Atom.updated
  in
  let date_ms = Ptime.to_float_s date *. 1000.0 in
  let tags = List.map (fun cat -> cat.Syndic.Atom.term) entry.categories in
  let tags = "unread" :: tags in
  `Assoc
    [ ("webid", `String webid)
    ; ("title", `String title)
    ; ("link", `String link)
    ; ("content", `String content_hash)
    ; ("full_content", `String wrapped_content)
    ; ("date", `Float date_ms)
    ; ("tags", `List (List.map (fun t -> `String t) tags))
    ; ("feed", `Assoc [("title", `String feed_title); ("url", `String feed_url)])
    ]

(* Write content files for each entry *)
let write_content_files ~content_dir entries =
  let open Yojson.Safe.Util in
  List.iter
    (fun entry ->
      let json = to_assoc entry in
      let content = List.assoc "full_content" json |> to_string in
      let content_hash = List.assoc "content" json |> to_string in
      let content_file = Filename.concat content_dir content_hash in
      if not (Sys.file_exists content_file) then (
        let oc = open_out content_file in
        output_string oc content ; close_out oc ) )
    entries

(* Fetch and parse an Atom feed *)
let fetch_feed url =
  let uri = Uri.of_string url in
  Cohttp_lwt_unix.Client.get uri
  >>= fun (response, content) ->
  Cohttp_lwt.Body.to_string content
  >>= fun body ->
  let source = `String (0, body) in
  let input = Xmlm.make_input source in
  match Syndic.Atom.parse ~xmlbase:uri input with
  | exception e ->
      Printf.eprintf "Error parsing feed %s: %s\n" url (Printexc.to_string e) ;
      Lwt.return []
  | feed ->
      let title = feed.title |> text_construct_to_string in
      let entries = feed.entries in
      Printf.printf "Parsed Atom feed: %s (%d entries)\n" title
        (List.length entries) ;
      let entries = List.map (entry_to_json url title) entries in
      Lwt.return entries

let () =
  let open Yojson.Safe in
  let open Yojson.Safe.Util in
  (* Fetch all feeds and collect entries *)
  let all_entries =
    Lwt_main.run
      ( Lwt_list.map_p fetch_feed feeds
      >>= fun entries_list ->
      let all_entries = List.flatten entries_list in
      let sorted_entries =
        List.sort
          (fun a b ->
            let date_a = to_assoc a |> List.assoc "date" |> to_float in
            let date_b = to_assoc b |> List.assoc "date" |> to_float in
            compare date_b date_a )
          all_entries
      in
      Lwt.return sorted_entries )
  in
  (* Write start_html *)
  let oc = open_out "web/start.html" in
  output_string oc (Elfeed_offline.Proxy.start_html ~is_static:true) ;
  close_out oc ;
  Printf.printf "Generated start.html page entries\n" ;
  (* Write JSON output *)
  let elfeed_dir = "web/elfeed" in
  if not (Sys.file_exists elfeed_dir && Sys.is_directory elfeed_dir) then
    Unix.mkdir elfeed_dir 0o755 ;
  let entries =
    List.map
      (fun e ->
        to_assoc e
        |> List.filter (fun (x, y) -> x <> "full_content")
        |> fun assoc -> `Assoc assoc )
      all_entries
  in
  let json_output = `List entries in
  let json_string = pretty_to_string json_output in
  let oc = open_out (elfeed_dir ^ "/search") in
  output_string oc json_string ;
  close_out oc ;
  Printf.printf "Generated sample.json with %d entries\n" (List.length entries) ;
  (* Write content files *)
  let content_dir = "web/elfeed/content" in
  if not (Sys.file_exists content_dir && Sys.is_directory content_dir) then
    Unix.mkdir content_dir 0o755 ;
  write_content_files ~content_dir all_entries ;
  Printf.printf "Generated content directory with %d entries\n"
    (List.length all_entries)

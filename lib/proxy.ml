open Lwt.Infix

let upstream = Uri.of_string "http://127.0.0.1:8080"

let forward (req : Dream.request) meth =
  let client_uri = Uri.of_string (Dream.target req) in
  let target = Uri.with_path upstream (Uri.path client_uri) in
  let target = Uri.with_query target (Uri.query client_uri) in
  let headers =
    Cohttp.Header.of_list
      (List.filter_map
         (fun name ->
           match Dream.header req name with
           | None ->
               None
           | Some v ->
               Some (name, v) )
         (* forward a small allowlist; add more if needed *)
         ["content-type"; "accept"; "authorization"; "cookie"; "user-agent"] )
  in
  Dream.body req
  >>= fun body ->
  Cohttp_lwt_unix.Client.call ~headers
    ~body:(Cohttp_lwt.Body.of_string body)
    meth target
  >>= fun (resp, content) ->
  Cohttp_lwt.Body.to_string content
  >>= fun s ->
  let headers = Http.Response.headers resp |> Cohttp.Header.to_list in
  let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  Dream.respond ~headers ~status:(Dream.int_to_status status) s

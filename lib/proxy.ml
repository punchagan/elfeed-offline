open Lwt.Infix

let is_content_uri uri =
  uri |> Uri.path |> String.starts_with ~prefix:"/elfeed/content/"

let wrapped_html html =
  Printf.sprintf
    {|
<!doctype html>
<html>
    <head>
        <style type='text/css'>
        :root {
            color-scheme: light dark;
        }
        img {
            max-width: 100%%;
            height: auto;
            object-fit: contain;
        }
        .elfeed-offline-start {
          max-width: 42rem;
          margin: 15vh auto 0;
          padding: 0 1.5rem;
          text-align: center;
          color: CanvasText;
          font-family: system-ui,-apple-system,Segoe UI,Roboto,sans-serif;
        }
        .elfeed-offline-start h1 {
          font-size: 1.4rem;
          font-weight: 600;
          margin-bottom: 1rem;
        }
        .elfeed-offline-start .primary {
          font-size: 1rem;
          margin-bottom: 0.75rem;
        }
        .elfeed-offline-start .secondary {
          font-size: 0.9rem;
          opacity: 0.75;
          margin-bottom: 1.25rem;
        }
        .elfeed-offline-start .hint {
          font-size: 0.85rem;
          opacity: 0.6;
        }
        </style>
        <meta charset='utf-8' />
        <base target='_blank' />
    </head>
    <body>
        %s
    </body>
</html>
|}
    html

let start_page _ =
  let html =
    {|
<div class="elfeed-offline-start">
  <h1>Elfeed Offline</h1>
  <p class="primary">
    Select an entry from the sidebar to start reading.
  </p>
  <p class="secondary">
    Articles are loaded here and can be read offline once cached.
  </p>
  <p id="shortcuts-hint" class="hint">
    Tip: Press <kbd>?</kbd> to see keyboard shortcuts.
  </p>
</div>
|}
  in
  Dream.respond ~status:`OK (wrapped_html html)

(** [sanitize_headers] strips out HTTP/1 headers before returning the response.
    Some clients for example Safari and even newer versions of curl are pretty
    strict about this and fail if there are HTTP/1 headers which are invalid in
    HTTP/2 in the response. *)
let sanitize_headers headers =
  let invalid_h2_headers =
    [ "connection"
    ; "keep-alive"
    ; "transfer-encoding"
    ; "upgrade"
    ; "proxy-connection" ]
  in
  let additional_headers =
    ["content-length" (* Wrapped HTML etc would change the content length *)]
  in
  let to_filter = invalid_h2_headers @ additional_headers in
  List.filter
    (fun (k, _) -> not (List.mem (String.lowercase_ascii k) to_filter))
    headers

let forward ~upstream ~method' (req : Dream.request) =
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
  Lwt.catch
    (fun () ->
      Cohttp_lwt_unix.Client.call ~headers
        ~body:(Cohttp_lwt.Body.of_string body)
        method' target
      >>= fun (resp, content) ->
      Cohttp_lwt.Body.to_string content
      >>= fun s ->
      let headers = Http.Response.headers resp |> Cohttp.Header.to_list in
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let headers = sanitize_headers headers in
      let html = if is_content_uri client_uri then wrapped_html s else s in
      if status >= 400 && is_content_uri client_uri then
        let error_html =
          wrapped_html
            (Printf.sprintf
               "<h1>Error %d</h1><p>Failed to load content from the Elfeed \
                server. Is it offline?</p>"
               status )
        in
        let headers =
          List.filter
            (fun (k, _) -> not (String.equal k "Content-Type"))
            headers
        in
        let status = if status >= 500 then 404 else status in
        Dream.respond ~headers ~status:(Dream.int_to_status status) error_html
      else Dream.respond ~headers ~status:(Dream.int_to_status status) html )
    (fun _exn ->
      let error_html =
        wrapped_html
          "<h1>Connection Error</h1><p>Failed to connect to the Elfeed server. \
           Is it offline?</p>"
      in
      Dream.respond ~status:`Service_Unavailable error_html )

let make_basic_auth_middleware ~username ~password () =
  let unauthorized () =
    Dream.respond ~status:`Unauthorized
      ~headers:[("WWW-Authenticate", "Basic realm=\"Elfeed Proxy\"")]
      "Unauthorized"
  in
  let basic_auth_middleware handler req =
    (* Ignore URLs not starting /elfeed/. The shell can be served to non
       authenticated users too. *)
    if not (String.starts_with ~prefix:"/elfeed/" (Dream.target req)) then
      handler req
    else
      match Dream.header req "Authorization" with
      | None ->
          unauthorized ()
      | Some auth_header -> (
        match String.split_on_char ' ' auth_header with
        | ["Basic"; encoded] -> (
          match Base64.decode encoded with
          | Error _ ->
              unauthorized ()
          | Ok decoded -> (
            match String.split_on_char ':' decoded with
            | [user; pass] ->
                if String.equal user username && String.equal pass password then
                  handler req
                else unauthorized ()
            | _ ->
                unauthorized () ) )
        | _ ->
            unauthorized () )
  in
  basic_auth_middleware

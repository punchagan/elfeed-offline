module Proxy = Elfeed_offline.Proxy

let () =
  let upstream = Uri.of_string "http://127.0.0.1:8080" in
  let certificate_file = "ssl/server.pem" in
  let key_file = "ssl/server.key" in
  if not (Sys.file_exists certificate_file && Sys.file_exists key_file) then
    failwith
      "SSL certificate or key file not found. Please generate them by running \
       the make-cert.sh script." ;
  let username = Sys.getenv_opt "ELFEED_USERNAME" in
  let password = Sys.getenv_opt "ELFEED_PASSWORD" in
  let auth =
    match (username, password) with
    | Some username, Some password ->
        Proxy.make_basic_auth_middleware ~username ~password ()
    | Some _, _ | _, Some _ ->
        failwith
          "Both ELFEED_USERNAME and ELFEED_PASSWORD must be set for \
           authentication."
    | _ ->
        fun handler req -> handler req
  in
  let open Dream in
  let routes =
    [ (* Proxy /elfeed endpoints to the Elfeed server *)
      get "/elfeed/**" (Proxy.forward ~upstream ~method':`GET)
    ; put "/elfeed/**" (Proxy.forward ~upstream ~method':`PUT)
    ; post "/elfeed/**" (Proxy.forward ~upstream ~method':`POST)
    ; (* Web stuff *)
      get "/" (fun req -> redirect req "/index.html")
    ; get "/**" (static "./web") ]
  in
  run ~interface:"0.0.0.0" ~port:9000 ~certificate_file ~key_file ~tls:true
  @@ logger @@ auth @@ router routes

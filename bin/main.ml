module Proxy = Elfeed_offline.Proxy

let main ~no_auth =
  let upstream = Uri.of_string "http://127.0.0.1:8080" in
  let certificate_file = "ssl/server.pem" in
  let key_file = "ssl/server.key" in
  if not (Sys.file_exists certificate_file && Sys.file_exists key_file) then
    failwith
      "SSL certificate or key file not found. Please generate them by running \
       the make-cert.sh script." ;
  let auth =
    if no_auth then fun handler req -> handler req
    else
      let username = Sys.getenv_opt "ELFEED_USERNAME" in
      let password = Sys.getenv_opt "ELFEED_PASSWORD" in
      match (username, password) with
      | Some username, Some password ->
          Proxy.make_basic_auth_middleware ~username ~password ()
      | _ ->
          Printf.eprintf
            "Authentication is enabled but ELFEED_USERNAME and/or \
             ELFEED_PASSWORD environment variables are not set. Either set \
             both variables or run with --no-auth to disable authentication.\n" ;
          exit 1
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

let () =
  let open Cmdliner in
  let open Term.Syntax in
  let no_auth =
    let doc = "Disable authentication (allow unauthenticated access)" in
    Arg.(value & flag & info ["no-auth"] ~doc)
  in
  let term =
    let+ no_auth = no_auth in
    main ~no_auth
  in
  let cmd =
    let doc = "Elfeed offline proxy server" in
    let info = Cmd.info "elfeed-offline" ~doc in
    Cmd.v info term
  in
  exit (Cmd.eval cmd)

module Proxy = Elfeed_offline.Proxy

let main ~upstream ~port ~interface ~certificate_file ~key_file ~no_auth () =
  if not (Sys.file_exists certificate_file && Sys.file_exists key_file) then
    failwith
      (Printf.sprintf
         "SSL certificate or key file not found at '%s' and '%s'. Please \
          generate them by running the make-cert.sh script or specify custom \
          paths with --cert and --key."
         certificate_file key_file ) ;
  let auth =
    if no_auth then fun handler req -> handler req
    else
      let username = Sys.getenv_opt "ELFEED_USERNAME" in
      let password = Sys.getenv_opt "ELFEED_PASSWORD" in
      match (username, password) with
      | Some username, Some password ->
          Proxy.make_basic_auth_middleware ~username ~password ()
      | _ ->
          failwith
            "Authentication is enabled but ELFEED_USERNAME and/or \
             ELFEED_PASSWORD environment variables are not set. Either set \
             both variables or run with --no-auth to disable authentication."
  in
  let open Dream in
  let routes =
    [ (* Proxy /elfeed endpoints to the Elfeed server *)
      get "/elfeed/**" (Proxy.forward ~upstream ~method':`GET)
    ; put "/elfeed/**" (Proxy.forward ~upstream ~method':`PUT)
    ; post "/elfeed/**" (Proxy.forward ~upstream ~method':`POST)
    ; (* Web stuff *)
      get "/" (fun req -> redirect req "/index.html")
    ; get "/start" Proxy.start_page
    ; get "/**" (static "./web") ]
  in
  run ~interface ~port ~certificate_file ~key_file ~tls:true
  @@ logger @@ auth @@ router routes

let uri_conv =
  let parse s =
    try Ok (Uri.of_string s) with _ -> Error (`Msg "Invalid URI")
  in
  let print fmt uri = Format.fprintf fmt "%s" (Uri.to_string uri) in
  Cmdliner.Arg.conv (parse, print)

let () =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  let elfeed_web =
    let doc = "Emacs Elfeed web server URL" in
    let default_uri = Uri.of_string "http://127.0.0.1:8080" in
    Arg.(value & opt uri_conv default_uri & info ["w"; "elfeed-web-url"] ~doc)
  in
  let port =
    let doc = "Port to listen on" in
    Arg.(value & opt int 9000 & info ["p"; "port"] ~doc)
  in
  let interface =
    let doc = "Network interface to bind to" in
    Arg.(value & opt string "0.0.0.0" & info ["i"; "interface"] ~doc)
  in
  let certificate_file =
    let doc = "Path to SSL certificate file" in
    Arg.(
      value
      & opt string "ssl/server.pem"
      & info ["cert"; "ssl-certificate"] ~doc )
  in
  let key_file =
    let doc = "Path to SSL private key file" in
    Arg.(value & opt string "ssl/server.key" & info ["ssl-key"; "key"] ~doc)
  in
  let no_auth =
    let doc = "Disable authentication (allow unauthenticated access)" in
    Arg.(value & flag & info ["no-auth"] ~doc)
  in
  let term =
    let+ no_auth = no_auth
    and+ elfeed_web = elfeed_web
    and+ port = port
    and+ interface = interface
    and+ certificate_file = certificate_file
    and+ key_file = key_file in
    main ~upstream:elfeed_web ~port ~interface ~certificate_file ~key_file
      ~no_auth ()
  in
  let cmd =
    let doc = "Elfeed offline proxy server" in
    let info = Cmd.info "elfeed-offline" ~doc in
    Cmd.v info term
  in
  exit (Cmd.eval cmd)

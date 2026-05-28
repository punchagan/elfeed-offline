module Proxy = Elfeed_offline.Proxy

let main ~upstream ~port ~interface ~certificate_file ~key_file ~auth () =
  let open Dream in
  let static_routes =
    List.filter_map
      (fun file ->
        let path = "/" ^ file in
        match Web_content.read file with
        | Some content ->
            Some (get path (fun _ -> respond content))
        | None ->
            Printf.eprintf "Warning: Failed to read content for file '%s'\n"
              file ;
            None )
      Web_content.file_list
  in
  let routes =
    [ (* Proxy /elfeed endpoints to the Elfeed server *)
      get "/elfeed/**" (Proxy.forward ~upstream ~method':`GET)
    ; put "/elfeed/**" (Proxy.forward ~upstream ~method':`PUT)
    ; post "/elfeed/**" (Proxy.forward ~upstream ~method':`POST)
    ; (* Web stuff *)
      get "/" (fun req -> redirect req "/index.html")
    ; get "/start.html" Proxy.start_page ]
  in
  run ~interface ~port ~certificate_file ~key_file ~tls:true
  @@ logger @@ auth
  @@ router (routes @ static_routes)

let uri_conv =
  let parse s =
    try Ok (Uri.of_string s) with _ -> Error (`Msg "Invalid URI")
  in
  let print fmt uri = Format.fprintf fmt "%s" (Uri.to_string uri) in
  Cmdliner.Arg.conv (parse, print)

let need_cert_file ~flag ~kind path =
  if Sys.file_exists path then Ok path
  else
    Error
      (`Msg
         (Printf.sprintf
            "%s not found at %S. Generate it with the make-cert.sh script, or \
             pass a path with %s."
            kind path flag ) )

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
      & opt filepath "ssl/server.pem"
      & info ["cert"; "ssl-certificate"] ~doc )
  in
  let key_file =
    let doc = "Path to SSL private key file" in
    Arg.(value & opt filepath "ssl/server.key" & info ["ssl-key"; "key"] ~doc)
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
    let open Result.Syntax in
    let* _ =
      need_cert_file ~flag:"--cert" ~kind:"SSL certificate file" key_file
    in
    let* _ = need_cert_file ~flag:"--key" ~kind:"SSL private key" key_file in
    let* auth =
      if no_auth then Ok (fun handler req -> handler req)
      else
        let username = Sys.getenv_opt "ELFEED_USERNAME" in
        let password = Sys.getenv_opt "ELFEED_PASSWORD" in
        match (username, password) with
        | Some username, Some password ->
            Ok (Proxy.make_basic_auth_middleware ~username ~password ())
        | _ ->
            Error
              (`Msg
                 "Authentication is enabled but ELFEED_USERNAME and/or \
                  ELFEED_PASSWORD environment variables are not set. Set both \
                  or pass  --no-auth." )
    in
    Ok
      (main ~upstream:elfeed_web ~port ~interface ~certificate_file ~key_file
         ~auth () )
  in
  let term = Term.term_result term in
  let cmd =
    let doc = "Elfeed offline proxy server" in
    let info = Cmd.info "elfeed-offline" ~doc in
    Cmd.v info term
  in
  exit (Cmd.eval cmd)

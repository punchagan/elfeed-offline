module Proxy = Elfeed_offline.Proxy

let () =
  let open Dream in
  run ~interface:"0.0.0.0" ~port:9000 ~certificate_file:"ssl/server.pem"
    ~key_file:"ssl/server.key" ~tls:true
  @@ logger
  @@ router
       [ (* Proxy /elfeed endpoints to the Elfeed server *)
         get "/elfeed/**" (fun req -> Proxy.forward req `GET)
       ; put "/elfeed/**" (fun req -> Proxy.forward req `PUT)
       ; post "/elfeed/**" (fun req -> Proxy.forward req `POST)
       ; (* Web stuff *)
         get "/" (fun req -> redirect req "/index.html")
       ; get "/**" (static "./web") ]

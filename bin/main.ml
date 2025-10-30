module Proxy = Elfeed_offline.Proxy

let () =
  let open Dream in
  run ~port:9000 @@ logger
  @@ router
       [ (* Proxy /elfeed endpoints to the Elfeed server *)
         get "/elfeed/**" (fun req -> Proxy.forward req `GET)
       ; put "/elfeed/**" (fun req -> Proxy.forward req `PUT)
       ; post "/elfeed/**" (fun req -> Proxy.forward req `POST)
       ; (* Web stuff *)
         get "/" (fun req -> redirect req "/index.html")
       ; get "/**" (static "./web") ]

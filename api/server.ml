open Lwt.Infix
open Capnp_rpc_lwt

let local ~logger ~notify =
  let module Cluster = Raw.Service.Cluster in
  Cluster.local @@ object
    inherit Cluster.service

    method register_impl params release_param_caps =
      let open Cluster.Register in
      let hostname = Params.hostname_get params in
      let callback = Params.callback_get params in
      release_param_caps ();
      match callback with
      | None -> Service.fail "No callback parameter!"
      | Some callback ->
        Service.return_lwt @@ fun () ->
        
        Capability.with_ref callback (notify ~hostname) >>= fun () ->

  end

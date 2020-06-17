open Capnp_rpc_lwt

let local ~register =
  let module X = Raw.Service.Registration in
  X.local @@ object
    inherit X.service

    method register_impl params release_param_caps =
      let open X.Register in
      let name = Params.name_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let queue = register ~name in
      Results.queue_set results (Some queue);
      Capability.dec_ref queue;
      Service.return response
  end

module X = Raw.Client.Registration

type t = X.t Capability.t

let register t ~name =
  let open X.Register in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.name_set params name;
  Capability.call_for_caps t method_id request Results.queue_get_pipelined

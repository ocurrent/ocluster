open Capnp_rpc_lwt

let local fn =
  let module X = Raw.Service.Progress in
  X.local @@ object
    inherit X.service

    method report_impl params release_param_caps =
      let open X.Report in
      let msg = Params.status_get params in
      release_param_caps ();
      fn msg;
      Service.return_empty ()
  end

module X = Raw.Client.Progress

type t = X.t Capability.t

let report t msg =
  let open X.Report in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.status_set params msg;
  Capability.call_for_unit t method_id request

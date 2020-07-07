open Lwt.Infix
open Capnp_rpc_lwt

let local ~dump =
  let module X = Raw.Service.PoolAdmin in
  X.local @@ object
    inherit X.service

    method dump_impl _params release_param_caps =
      let open X.Dump in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.state_set results (dump ());
      Service.return response
  end

module X = Raw.Client.PoolAdmin

type t = X.t Capability.t

let dump t =
  let open X.Dump in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value_exn t method_id request >|= Results.state_get

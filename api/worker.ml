open Capnp_rpc_lwt

let ( >>!= ) = Lwt_result.bind

let local ~metrics =
  let module X = Raw.Service.Worker in
  X.local @@ object
    inherit X.service

    method metrics_impl _params release_param_caps =
      let open X.Metrics in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let version, data = metrics () in
      Results.version_set results version;
      Results.data_set results data;
      Service.return response
  end

module X = Raw.Client.Worker

type t = X.t Capability.t

let metrics t =
  let open X.Metrics in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request >>!= fun results ->
  Lwt_result.return (Results.version_get results, Results.data_get results)

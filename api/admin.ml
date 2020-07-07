open Lwt.Infix
open Capnp_rpc_lwt

let local ~pools ~pool =
  let module X = Raw.Service.Admin in
  X.local @@ object
    inherit X.service

    method pools_impl _params release_param_caps =
      let open X.Pools in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.names_set_list results (pools ()) |> ignore;
      Service.return response

    method pool_impl params release_param_caps =
      let open X.Pool in
      let name = Params.name_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let cap = pool name in
      Results.pool_set results (Some cap);
      Capability.dec_ref cap;
      Service.return response
  end

module X = Raw.Client.Admin

type t = X.t Capability.t

let pools t =
  let open X.Pools in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value_exn t method_id request >|= fun results ->
  Results.names_get_list results

let pool t name =
  let open X.Pool in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.name_set params name;
  Capability.call_for_caps t method_id request Results.pool_get_pipelined

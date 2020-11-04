open Lwt.Infix
open Capnp_rpc_lwt

let ( >>!= ) = Lwt_result.bind

let local ~pools ~pool ~add_client ~remove_client ~list_clients =
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

    method add_client_impl params release_param_caps =
      let open X.AddClient in
      let id = Params.id_get params in
      release_param_caps ();
      Service.return_lwt @@ fun () ->
      add_client id >>!= fun cap ->
      let response, results = Service.Response.create Results.init_pointer in
      Results.cap_set results (Some cap);
      Capability.dec_ref cap;
      Lwt.return_ok response

    method remove_client_impl params release_param_caps =
      let open X.RemoveClient in
      let id = Params.id_get params in
      release_param_caps ();
      Service.return_lwt @@ fun () ->
      remove_client id >>!= fun () ->
      Lwt_result.return (Service.Response.create_empty ())

    method list_clients_impl _params release_param_caps =
      let open X.ListClients in
      release_param_caps ();
      Service.return_lwt @@ fun () ->
      list_clients () >>= fun clients ->
      let response, results = Service.Response.create Results.init_pointer in
      let _ : _ Capnp.Array.t = Results.clients_set_list results clients in
      Lwt.return_ok response
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

let add_client t id =
  let open X.AddClient in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params id;
  Capability.call_for_caps t method_id request Results.cap_get_pipelined

let remove_client t id =
  let open X.RemoveClient in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params id;
  Capability.call_for_unit_exn t method_id request

let list_clients t =
  let open X.ListClients in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value_exn t method_id request >|= Results.clients_get_list

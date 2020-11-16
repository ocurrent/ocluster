open Lwt.Infix
open Capnp_rpc_lwt

type worker_info = {
  name : string;
  active : bool;
}

let local ~show ~workers ~worker ~set_active ~update ~set_rate =
  let module X = Raw.Service.PoolAdmin in
  X.local @@ object
    inherit X.service

    method show_impl _params release_param_caps =
      let open X.Show in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.state_set results (show ());
      Service.return response

    method workers_impl _params release_param_caps =
      let open X.Workers in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let items = workers () in
      let arr = Results.workers_init results (List.length items) in
      items |> List.iteri (fun i { name; active } ->
          let slot = Capnp.Array.get arr i in
          let module B = Raw.Builder.WorkerInfo in
          B.name_set slot name;
          B.active_set slot active
        );
      Service.return response

    method set_active_impl params release_param_caps =
      let open X.SetActive in
      let worker = Params.worker_get params in
      let active = Params.active_get params in
      release_param_caps ();
      match set_active worker active with
      | Ok () -> Service.return_empty ()
      | Error `Unknown_worker -> Service.fail "Unknown worker"

    method worker_impl params release_param_caps =
      let open X.Worker in
      let name = Params.worker_get params in
      release_param_caps ();
      match worker name with
      | None -> Service.fail "Unknown worker"
      | Some cap ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.worker_set results (Some cap);
        Capability.dec_ref cap;
        Service.return response

    method update_impl params release_param_caps =
      let open X.Update in
      let name = Params.worker_get params in
      release_param_caps ();
      update name

    method set_rate_impl params release_param_caps =
      let open X.SetRate in
      let client_id = Params.id_get params in
      let rate = Params.rate_get params in
      release_param_caps ();
      match set_rate ~client_id rate with
      | Ok () -> Service.return_empty ()
      | Error `No_such_user -> Service.fail "No such user"
  end

module X = Raw.Client.PoolAdmin

type t = X.t Capability.t

let show t =
  let open X.Show in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value_exn t method_id request >|= Results.state_get

let workers t =
  let open X.Workers in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value_exn t method_id request >|= fun results ->
  let module R = Raw.Reader.WorkerInfo in
  Results.workers_get_list results |> List.map @@ fun worker ->
  let name = R.name_get worker in
  let active = R.active_get worker in
  { name; active }

let worker t worker =
  let open X.Worker in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.worker_set params worker;
  Capability.call_for_caps t method_id request Results.worker_get_pipelined

let set_active t worker active =
  let open X.SetActive in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.worker_set params worker;
  Params.active_set params active;
  Capability.call_for_unit_exn t method_id request

let update t worker =
  let open X.Update in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.worker_set params worker;
  Capability.call_for_unit t method_id request

let set_rate t ~client_id rate =
  let open X.SetRate in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params client_id;
  Params.rate_set params rate;
  Capability.call_for_unit_exn t method_id request

open Lwt.Infix
open Capnp_rpc_lwt

type worker_info = {
  name : string;
  active : bool;
}

let local ~dump ~workers ~set_active =
  let module X = Raw.Service.PoolAdmin in
  X.local @@ object
    inherit X.service

    method dump_impl _params release_param_caps =
      let open X.Dump in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.state_set results (dump ());
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
  end

module X = Raw.Client.PoolAdmin

type t = X.t Capability.t

let dump t =
  let open X.Dump in
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

let set_active t worker active =
  let open X.SetActive in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.worker_set params worker;
  Params.active_set params active;
  Capability.call_for_unit_exn t method_id request

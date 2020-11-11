open Capnp_rpc_lwt

let local ~register =
  let module X = Raw.Service.Registration in
  X.local @@ object
    inherit X.service

    method register_impl params release_param_caps =
      let open X.Register in
      let name = Params.name_get params in
      let capacity =
        let x = Params.capacity_get_int_exn params in
        if x > 0 then x
        else 32   (* Old workers don't report their capacity. *)
      in
      let worker = Params.worker_get params in
      release_param_caps ();
      match worker with
      | None -> Service.fail "Missing worker argument!"
      | Some worker ->
        let response, results = Service.Response.create Results.init_pointer in
        let queue = register ~name ~capacity worker in
        Results.queue_set results (Some queue);
        Capability.dec_ref queue;
        Service.return response
  end

module X = Raw.Client.Registration

type t = X.t Capability.t

let register t ~name ~capacity worker =
  let open X.Register in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.name_set params name;
  Params.worker_set params (Some worker);
  Params.capacity_set_int_exn params capacity;
  Capability.call_for_caps t method_id request Results.queue_get_pipelined

open Capnp_rpc_lwt

type t = Raw.Service.Ticket.t Capability.t

let ( >>!= ) = Lwt_result.bind

let local ~job ~cancel ~release =
  let module X = Raw.Service.Ticket in
  X.local @@ object
    inherit X.service

    method job_impl _params release_param_caps =
      let open X.Job in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.job_set results (Some job);
      Service.return response

    method cancel_impl _params release_param_caps =
      release_param_caps ();
      Service.return_lwt @@ fun () ->
      cancel () >>!= fun () ->
      let response = Service.Response.create_empty () in
      Lwt_result.return response

    method! release =
      Capability.dec_ref job;
      release ()
  end

module X = Raw.Client.Ticket

let job t =
  let open X.Job in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.job_get_pipelined

let cancel t =
  let open X.Cancel in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_unit t method_id request

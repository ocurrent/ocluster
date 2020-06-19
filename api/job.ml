open Lwt.Infix
open Capnp_rpc_lwt

type t = Raw.Service.Job.t Capability.t

let local ~switch ~outcome ~stream_log_data =
  let module X = Raw.Service.Job in
  X.local @@ object
    inherit X.service

    method log_impl params release_param_caps =
      let open X.Log in
      release_param_caps ();
      let start = Params.start_get params in
      Service.return_lwt @@ fun () ->
      stream_log_data ~start >|= fun (log, next) ->
      let response, results = Service.Response.create Results.init_pointer in
      Results.log_set results log;
      Results.next_set results next;
      Ok response

    method status_impl _params release_param_caps =
      release_param_caps ();
      Service.return_lwt @@ fun () ->
      outcome >|= function
      | Ok () -> Ok (Service.Response.create_empty ())
      | Error (`Msg m) -> Error (`Capnp (`Exception (Capnp_rpc.Exception.v m)))

    method! release =
      Lwt.async (fun () -> Lwt_switch.turn_off switch)
  end

module X = Raw.Client.Job

let log t start =
  let open X.Log in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.start_set params start;
  Capability.call_for_value t method_id request |> Lwt_result.map @@ fun x ->
  (Results.log_get x, Results.next_get x)

let status t =
  let open X.Status in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_unit t method_id request

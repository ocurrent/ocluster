open Lwt.Infix
open Capnp_rpc_lwt

let ( >>!= ) = Lwt_result.bind

let local ~metrics ~self_update =
  let module X = Raw.Service.Worker in
  X.local @@ object
    inherit X.service

    method metrics_impl params release_param_caps =
      let open X.Metrics in
      let source = Params.source_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let collect source =
        Service.return_lwt @@ fun () ->
        metrics source >|= function
        | Ok (content_type, data) ->
          Results.content_type_set results content_type;
          Results.data_set results data;
          Ok response
        | Error (`Msg msg) ->
          Error (`Capnp (Capnp_rpc.Error.exn "%s" msg))
      in
      match source with
      | Agent -> collect `Agent
      | Host -> collect `Host
      | Undefined _ -> Service.fail "Unknown metrics source"

    method self_update_impl _params release_param_caps =
      release_param_caps ();
      Service.return_lwt @@ fun () ->
      self_update () >|= function
      | Error (`Msg m) -> Error (`Capnp (Capnp_rpc.Error.exn "%s" m))
      | Ok () -> Ok (Service.Response.create_empty ())
  end

module X = Raw.Client.Worker

type t = X.t Capability.t

let metrics t ~source =
  let open X.Metrics in
  let request, params = Capability.Request.create Params.init_pointer in
  let source =
    match source with
    | `Agent -> Raw.Builder.Worker.MetricsSource.Agent
    | `Host -> Raw.Builder.Worker.MetricsSource.Host
  in
  Params.source_set params source;
  Capability.call_for_value t method_id request >>!= fun results ->
  Lwt_result.return (Results.content_type_get results, Results.data_get results)

let self_update t =
  let open X.SelfUpdate in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_unit t method_id request >|= function
  | Ok () -> failwith "update reported success, but should have failed with a disconnection error!"
  | Error (`Capnp (`Exception {ty = `Disconnected; _})) -> Ok ()
  | Error _ as e -> e

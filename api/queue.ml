open Lwt.Infix
open Capnp_rpc_lwt

type job_desc = {
  dockerfile : string;
  cache_hint : string;
}

let local ~pop ~release =
  let module X = Raw.Service.Queue in
  X.local @@ object
    inherit X.service

    method pop_impl params release_param_caps =
      let open X.Pop in
      let job = Params.job_get params in
      release_param_caps ();
      match job with
      | None -> Service.fail "Missing job!"
      | Some job ->
        Service.return_lwt @@ fun () ->
        pop ~job |> Lwt_result.map @@ fun { dockerfile; cache_hint } ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.dockerfile_set results dockerfile;
        Results.cache_hint_set results cache_hint;
        response

    method! release = release ()
  end

module X = Raw.Client.Queue

type t = X.t Capability.t

let pop t job =
  let open X.Pop in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.job_set params (Some job);
  Capability.call_for_value_exn t method_id request >|= fun descr ->
  let dockerfile = Results.dockerfile_get descr in
  let cache_hint = Results.cache_hint_get descr in
  { dockerfile; cache_hint }

open Capnp_rpc_lwt

let local ~submit =
  let module X = Raw.Service.Submission in
  X.local @@ object
    inherit X.service

    method submit_impl params release_param_caps =
      let open X.Submit in
      release_param_caps ();
      let descr = Params.descr_get params in
      let job = submit descr in
      let response, results = Service.Response.create Results.init_pointer in
      Results.job_set results (Some job);
      Capability.dec_ref job;
      Service.return response
  end

module X = Raw.Client.Submission

type t = X.t Capability.t

let submit ?src t ~dockerfile ~cache_hint =
  let open X.Submit in
  let module JD = Raw.Builder.JobDescr in
  let request, params = Capability.Request.create Params.init_pointer in
  let b = Params.descr_get params in
  JD.dockerfile_set b dockerfile;
  JD.cache_hint_set b cache_hint;
  src |> Option.iter (fun (repo, commits) ->
      let _ : _ Capnp.Array.t = JD.commits_set_list b commits in
      JD.repository_set b repo;
    );
  Capability.call_for_caps t method_id request Results.job_get_pipelined

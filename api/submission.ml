open Capnp_rpc_lwt

let local ~submit =
  let module X = Raw.Service.Submission in
  X.local @@ object
    inherit X.service

    method submit_impl params release_param_caps =
      let open X.Submit in
      release_param_caps ();
      let pool = Params.pool_get params in
      let descr = Params.descr_get params in
      let urgent = Params.urgent_get params in
      let job = submit ~pool ~urgent descr in
      let response, results = Service.Response.create Results.init_pointer in
      Results.job_set results (Some job);
      Capability.dec_ref job;
      Service.return response
  end

module X = Raw.Client.Submission

type t = X.t Capability.t

type action =
  | Docker_build of Docker.Spec.t

let docker_build ?push_to ?(options=Docker.Spec.defaults) dockerfile =
  Docker_build { Docker.Spec.dockerfile; options; push_to }

let get_action descr =
  let module JD = Raw.Reader.JobDescr in
  let action = JD.docker_build_get descr in
  Docker_build (Docker.Spec.read action)

let submit ?src ?(urgent=false) t ~pool ~action ~cache_hint =
  let open X.Submit in
  let module JD = Raw.Builder.JobDescr in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.pool_set params pool;
  Params.urgent_set params urgent;
  let b = Params.descr_get params in
  let Docker_build action = action in
  let db = JD.docker_build_get b in
  Docker.Spec.init db action;
  JD.cache_hint_set b cache_hint;
  src |> Option.iter (fun (repo, commits) ->
      let _ : _ Capnp.Array.t = JD.commits_set_list b commits in
      JD.repository_set b repo;
    );
  Capability.call_for_caps t method_id request Results.job_get_pipelined

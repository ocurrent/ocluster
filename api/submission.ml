open Capnp_rpc_lwt

let local ~submit ~sturdy_ref =
  let module X = Raw.Service.Submission in
  Persistence.with_sturdy_ref sturdy_ref X.local @@ object
    inherit X.service

    method submit_impl params release_param_caps =
      let open X.Submit in
      release_param_caps ();
      let pool = Params.pool_get params in
      let descr = Params.descr_get params in
      let urgent = Params.urgent_get params in
      Service.return_lwt @@ fun () ->
      let ticket = submit ~pool ~urgent descr in
      let response, results = Service.Response.create Results.init_pointer in
      Results.ticket_set results (Some ticket);
      Capability.dec_ref ticket;
      Lwt_result.return response
  end

module X = Raw.Client.Submission

type t = X.t Capability.t

type action =
  | Docker_build of Docker.Spec.t
  | Obuilder_build of Obuilder_job.Spec.t
  | Custom_build of Custom.t

let docker_build ?push_to ?(options=Docker.Spec.defaults) dockerfile =
  Docker_build { Docker.Spec.dockerfile; options; push_to }

let obuilder_build spec =
  Obuilder_build { Obuilder_job.Spec.spec = `Contents spec }

let custom_build c = Custom_build c

let get_action descr =
  let module JD = Raw.Reader.JobDescr in
  match JD.action_get descr |> JD.Action.get with
  | DockerBuild action -> Docker_build (Docker.Spec.read action)
  | Obuilder action -> Obuilder_build (Obuilder_job.Spec.read action)
  | Custom action -> Custom_build (Custom.read action)
  | Undefined x -> Fmt.failwith "Unknown action type %d" x

let submit ?src ?(urgent=false) ?(secrets=[]) t ~pool ~action ~cache_hint =
  let open X.Submit in
  let module JD = Raw.Builder.JobDescr in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.pool_set params pool;
  Params.urgent_set params urgent;
  let b = Params.descr_get params in
  let act = JD.action_init b in
  begin match action with
    | Docker_build action ->
      let b = JD.Action.docker_build_init act in
      Docker.Spec.init b action
    | Obuilder_build action ->
      let b = JD.Action.obuilder_init act in
      Obuilder_job.Spec.init b action
    | Custom_build action ->
      let b = JD.Action.custom_init act in
      Custom.init b action
  end;
  JD.cache_hint_set b cache_hint;
  src |> Option.iter (fun (repo, commits) ->
      let _ : _ Capnp.Array.t = JD.commits_set_list b commits in
      JD.repository_set b repo;
    );
  let secrets_array = JD.secrets_init b (List.length secrets) in
  List.iteri (fun i (id, value) ->
    let secret = Capnp.Array.get secrets_array i in
    Raw.Builder.Secret.id_set secret id;
    Raw.Builder.Secret.value_set secret value
  ) secrets;
  Capability.call_for_caps t method_id request Results.ticket_get_pipelined

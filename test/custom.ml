open Lwt.Infix

module Spec = struct
  let obuilder_spec_to_custom spec builder =
    let open Cluster_api.Raw in
    let obuilder = Builder.OBuilder.init_pointer builder in
    Builder.OBuilder.spec_set obuilder spec

  let dockerfile_to_custom spec =
    let open Cluster_api.Raw in
    let custom = Builder.Custom.init_root () in
    let builder = Builder.Custom.payload_get custom in
    let df = Builder.DockerBuild.Dockerfile.init_pointer builder in
    Builder.DockerBuild.Dockerfile.contents_set df spec;
    let r = Reader.Custom.of_builder custom in
    Reader.Custom.payload_get r
end

(* Using the underlying Connection API to submit custom build jobs which are really
   just dockerfile builds in disguise. *)
module Build = struct
  module Op = struct
    type t = Current_ocluster.Connection.t
    let ( >>!= ) = Lwt_result.bind
    let id = "mock-ocluster-build"

    (* Build Pool *)
    module Key = Current.String
    (* Dockerfile Spec *)
    module Value = Current.String

    module Outcome = Current.String

    let pp = Fmt.(pair string string)

    let auto_cancel = true
    let latched = true

    let run t job pool value =
      let action = Cluster_api.Submission.custom_build @@ Cluster_api.Custom.v ~kind:"dockerfile" @@ Spec.obuilder_spec_to_custom value in
      let build_pool = Current_ocluster.Connection.pool ~job ~pool ~action ~cache_hint:"" t in
      Current.Job.start_with ~pool:build_pool job ~level:Current.Level.Average >>= fun build_job ->
      Capnp_rpc_lwt.Capability.with_ref build_job (Current_ocluster.Connection.run_job ~job)
  end

  module BC = Current_cache.Generic (Op)

  let build_dockerfile t pool spec =
    let open Current.Syntax in
    Current.component "mock custom cluster build" |>
    let> () = Current.return () in 
    BC.run t pool spec
end

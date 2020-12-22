let program_name = "obuilder_pipeline"

open Current.Syntax

let () =
  Logging.init ()

let repo = { Current_github.Repo_id.owner = "ocurrent"; name = "obuilder" }
let pool = "linux-x86_64"
let timeout = Duration.of_min 60

let example_spec =
  let open Obuilder_spec in
  stage ~from:"busybox" [
    copy ["."] ~dst:"/src";
    shell ["sh"; "-c"];
    run "ls -l /src"
  ]
  |> Fmt.to_to_string Obuilder_spec.pp

let pipeline ~cluster () =
  let src =
    let+ src = Current_github.Api.Anonymous.head_of repo (`Ref "refs/heads/master") in
    [ src ]
  in
  let spec = Current.return { Cluster_api.Obuilder_job.Spec.spec = `Contents example_spec } in
  let cluster = Current_ocluster.with_timeout (Some timeout) cluster in
  let cache_hint = "ocluster-example" in
  Current_ocluster.build_obuilder cluster ~cache_hint ~src ~pool spec

let main config mode submission_uri =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let connection = Current_ocluster.Connection.create submission_cap in
  let cluster = Current_ocluster.v connection in
  let engine = Current.Engine.create ~config (pipeline ~cluster) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let submission_service =
  Arg.required @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let cmd =
  let doc = "Run an OBuilder build on a cluster." in
  Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ submission_service)),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)

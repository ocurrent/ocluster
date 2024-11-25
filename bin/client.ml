open Lwt.Infix
open Capnp_rpc_lwt

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let read_first_line path =
  let ch = open_in_bin path in
  Fun.protect (fun () -> input_line ch)
    ~finally:(fun () -> close_in ch)

let rec tail job start =
  Cluster_api.Job.log job start >>= function
  | Error (`Capnp e) -> Fmt.failwith "Error tailing logs: %a" Capnp_rpc.Error.pp e
  | Ok ("", _) -> Lwt.return_unit
  | Ok (data, next) ->
    output_string stdout data;
    flush stdout;
    tail job next

let run cap_path fn =
  try
    Lwt_main.run begin
      let vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Cap_file.load vat cap_path |> or_die in
      Capnp_rpc_unix.with_cap_exn sr fn
    end
  with Failure msg ->
    Printf.eprintf "%s\n%!" msg;
    exit 1

type submit_options_common = {
  submission_path : string;
  pool : string;
  repository : string option;
  commits : string list;
  cache_hint : string;
  urgent : bool;
  secrets : (string * string) list;
}

let get_action = function
  | `Docker (dockerfile, push_to, options) ->
    begin match dockerfile with
      | `Context_path path -> Lwt.return (`Path path)
      | `Local_path path ->
        Lwt_io.(with_file ~mode:input) path (Lwt_io.read ?count:None) >|= fun data ->
        `Contents data
    end >|= fun dockerfile ->
    Cluster_api.Submission.docker_build ?push_to ~options dockerfile
  | `Obuilder path ->
    Lwt_io.(with_file ~mode:input) path (Lwt_io.read ?count:None) >|= fun spec ->
    Cluster_api.Submission.obuilder_build spec

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let submit () { submission_path; pool; repository; commits; cache_hint; urgent; secrets } spec =
  let src =
    match repository, commits with
    | None, [] -> None
    | None, _ -> failwith "BUG: commits but no repository!"
    | Some repo, [] -> Fmt.failwith "No commits requested from repository %S!" repo
    | Some repo, commits -> Some (repo, commits)
  in
  run submission_path @@ fun submission_service ->
  get_action spec >>= fun action ->
  let secrets = List.map (fun (id, path) -> id, read_whole_file path) secrets in
  Capability.with_ref (Cluster_api.Submission.submit submission_service ~urgent ~pool ~action ~cache_hint ~secrets ?src) @@ fun ticket ->
  Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  let result = Cluster_api.Job.result job in
  Fmt.pr "Tailing log:@.";
  tail job 0L >>= fun () ->
  result >|= function
  | Ok "" -> ()
  | Ok x -> Fmt.pr "Result: %S@." x
  | Error (`Capnp e) ->
    Fmt.pr "%a.@." Capnp_rpc.Error.pp e;
    exit 1

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of submission.cap file from ocluster-scheduler."
    ~docv:"ADDR"
    ["c"; "connect"]

let local_obuilder =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the local OBuilder spec to submit."
    ~docv:"PATH"
    ["local-file"]

let local_dockerfile =
  Arg.value @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the local Dockerfile to submit."
    ~docv:"PATH"
    ["local-dockerfile"]

let context_dockerfile =
  Arg.value @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Path of the Dockerfile within the commit."
    ~docv:"PATH"
    ["context-dockerfile"]

let dockerfile =
  let make local_dockerfile context_dockerfile =
    match local_dockerfile, context_dockerfile with
    | None, None -> Ok (`Context_path "Dockerfile")
    | Some local, None -> Ok (`Local_path local)
    | None, Some context -> Ok (`Context_path context)
    | Some _, Some _ -> Error ("Can't use --local-dockerfile and --context-dockerfile together!")
  in
  Term.(term_result' (const make $ local_dockerfile $ context_dockerfile))

let repo =
  Arg.value @@
  Arg.pos 0 Arg.(some string) None @@
  Arg.info
    ~doc:"URL of the source Git repository."
    ~docv:"URL"
    []

let commits =
  Arg.value @@
  Arg.(pos_right 0 string) [] @@
  Arg.info
    ~doc:"Git commit to use as context (full commit hash)."
    ~docv:"HASH"
    []

let pool =
  Arg.required @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Pool to use."
    ~docv:"ID"
    ["pool"]

let cache_hint =
  Arg.value @@
  Arg.(opt string) "" @@
  Arg.info
    ~doc:"Hint used to group similar builds to improve caching."
    ~docv:"STRING"
    ["cache-hint"]

let urgent =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Add job to the urgent queue."
    ["urgent"]

let push_to =
  let target_conv = Arg.conv Cluster_api.Docker.Image_id.(of_string, pp) in
  Arg.value @@
  Arg.(opt (some target_conv)) None @@
  Arg.info
    ~doc:"Where to docker-push the result."
    ~docv:"REPO:TAG"
    ["push-to"]

let push_user =
  Arg.value @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Docker registry user account to use when pushing."
    ~docv:"USER"
    ["push-user"]

let push_password_file =
  Arg.value @@
  Arg.(opt (some file)) None @@
  Arg.info
    ~doc:"File containing Docker registry password."
    ~docv:"PATH"
    ["push-password"]

let build_args =
  Arg.value @@
  Arg.(opt_all string) [] @@
  Arg.info
    ~doc:"Docker build argument; triggers OCurrent rebuild on change."
    ~docv:"ARG"
    ["build-arg"]

let nontriggering_build_args =
  Arg.value @@
  Arg.(opt_all string) [] @@
  Arg.info
    ~doc:"Docker build argument; doesn't trigger OCurrent rebuild on change."
    ~docv:"ARG"
    ["nontriggering-build-arg"]

let secrets =
  (Arg.value @@
   Arg.(opt_all (pair ~sep:':' string file)) [] @@
   Arg.info
     ~doc:"Provide a secret under the form id:file."
     ~docv:"SECRET"
     ["secret"])

let squash =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Whether to squash the layers."
    ["squash"]

let buildkit =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Whether to use BuildKit to build."
    ["buildkit"]

let include_git =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Include the .git clone in the build context."
    ["include-git"]

let push_to =
  let make target user password =
    match target, user, password with
    | None, None, None -> None
    | None, _, _ ->
      Fmt.failwith "Must use --push-to with --push-user/--push-password"
    | Some target, Some user, Some password_file ->
      let password = read_first_line password_file in
      Some { Cluster_api.Docker.Spec.target; auth = Some (user, password) }
    | Some target, None, None ->
      Some { Cluster_api.Docker.Spec.target; auth = None }
    | _, None, Some _
    | _, Some _, None -> Fmt.failwith "Must use --push-user with --push-password"
  in
  Term.(const make $ push_to $ push_user $ push_password_file)

let build_options =
  let make build_args nontriggering_build_args squash buildkit include_git =
    { Cluster_api.Docker.Spec.build_args; nontriggering_build_args; squash; buildkit; include_git }
  in
  Term.(const make $ build_args $ nontriggering_build_args $ squash $ buildkit $ include_git)

let submit_options_common =
  let make submission_path pool repository commits cache_hint urgent secrets =
    { submission_path; pool; repository; commits; cache_hint; urgent; secrets }
  in
  Term.(const make $ connect_addr $ pool $ repo $ commits $ cache_hint $ urgent $ secrets)

let submit_docker_options =
  let make dockerfile push_to build_options =
    `Docker (dockerfile, push_to, build_options)
  in
  Term.(const make $ dockerfile $ push_to $ build_options)

let submit_docker =
  let doc = "Submit a Docker build to the scheduler." in
  let info = Cmd.info "submit-docker" ~doc in
  Cmd.v info
    Term.(const submit $ Logging.cmdliner $ submit_options_common $ submit_docker_options)

let submit_obuilder_options =
  let make spec =
    `Obuilder spec
  in
  Term.(const make $ local_obuilder)

let submit_obuilder =
  let doc = "Submit an OBuilder build to the scheduler." in
  let info = Cmd.info "submit-obuilder" ~doc in
  Cmd.v info
    Term.(const submit $ Logging.cmdliner $ submit_options_common $ submit_obuilder_options)

let cmds = [submit_docker; submit_obuilder]

let () =
  let doc = "a command-line client for the ocluster-scheduler" in
  let info = Cmd.info "ocluster-client" ~doc ~version:Version.t in
  exit (Cmd.eval ~argv:Options.argv @@ Cmd.group info cmds)

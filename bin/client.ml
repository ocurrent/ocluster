open Lwt.Infix
open Capnp_rpc_lwt

let () =
  Logging.init ()

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
      Sturdy_ref.connect_exn sr >>= fun service ->
      Capability.with_ref service fn
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

let submit { submission_path; pool; repository; commits; cache_hint; urgent } spec =
  let src =
    match repository, commits with
    | None, [] -> None
    | None, _ -> failwith "BUG: commits but no repository!"
    | Some repo, [] -> Fmt.failwith "No commits requested from repository %S!" repo
    | Some repo, commits -> Some (repo, commits)
  in
  run submission_path @@ fun submission_service ->
  get_action spec >>= fun action ->
  Capability.with_ref (Cluster_api.Submission.submit submission_service ~urgent ~pool ~action ~cache_hint ?src) @@ fun ticket ->
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

let show cap_path pool =
  run cap_path @@ fun admin_service ->
  match pool with
  | None ->
    Cluster_api.Admin.pools admin_service >|= fun pools ->
    List.iter print_endline pools
  | Some pool ->
    Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
    Cluster_api.Pool_admin.dump pool >|= fun status ->
    print_endline (String.trim status)

let set_active active cap_path pool worker =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
  match worker with
  | Some worker ->
    Cluster_api.Pool_admin.set_active pool worker active
  | None ->
    Cluster_api.Pool_admin.workers pool >|= function
    | [] ->
      Fmt.epr "No workers connected to pool!@.";
      exit 1
    | workers ->
      let pp_active f = function
        | true -> Fmt.string f "active"
        | false -> Fmt.string f "paused"
      in
      let pp_worker_info f { Cluster_api.Pool_admin.name; active } =
        Fmt.pf f "%s (%a)" name pp_active active
      in
      Fmt.epr "@[<v>Specify which worker you want to affect. Candidates are:@,%a@."
        Fmt.(list ~sep:cut pp_worker_info) workers;
      exit 1

let pp_worker_info f { Cluster_api.Pool_admin.name; _ } =
  Fmt.string f name

let update cap_path pool worker =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
  match worker with
  | Some worker ->
    begin
      Cluster_api.Pool_admin.update pool worker >|= function
      | Ok () -> Fmt.pr "Restarted@."
      | Error (`Capnp ex) ->
        Fmt.pr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
    end
  | None ->
    Cluster_api.Pool_admin.workers pool >>= function
    | [] ->
      Fmt.epr "No workers connected to pool!@.";
      exit 1
    | w :: ws ->
      Fmt.pr "Testing update on first worker in pool: %S@." w.name;
      Cluster_api.Pool_admin.update pool w.name >>= function
      | Error (`Capnp ex) ->
        Fmt.pr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
      | Ok () ->
        Fmt.pr "Canary updated OK. Updating the others: [%a]@."
          Fmt.(list ~sep:sp pp_worker_info) ws;
        ws
        |> List.map (fun (w:Cluster_api.Pool_admin.worker_info) ->
            Cluster_api.Pool_admin.update pool w.name >|= function
            | Ok () -> Fmt.pr "%S restarted OK.@." w.name
            | Error (`Capnp ex) ->
              Fmt.pr "%S: %a@." w.name Capnp_rpc.Error.pp ex;
              failwith "Failed update(s)"
          )
        |> Lwt.join >|= fun () ->
        Fmt.pr "All pool workers restarted.@."

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.required @@
  Arg.pos 0 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of .cap file from build-scheduler"
    ~docv:"ADDR"
    []

let local_obuilder =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the local OBuilder spec to submit"
    ~docv:"PATH"
    ["local-file"]

let local_dockerfile =
  Arg.value @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the local Dockerfile to submit"
    ~docv:"PATH"
    ["local-dockerfile"]

let context_dockerfile =
  Arg.value @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Path of the Dockerfile within the commit"
    ~docv:"PATH"
    ["context-dockerfile"]

let dockerfile =
  let make local_dockerfile context_dockerfile =
    match local_dockerfile, context_dockerfile with
    | None, None -> `Ok (`Context_path "Dockerfile")
    | Some local, None -> `Ok (`Local_path local)
    | None, Some context -> `Ok (`Context_path context)
    | Some _, Some _ -> `Error (false, "Can't use --local-dockerfile and --context-dockerfile together!")
  in
  Term.(ret (pure make $ local_dockerfile $ context_dockerfile))

let repo =
  Arg.value @@
  Arg.pos 1 Arg.(some string) None @@
  Arg.info
    ~doc:"URL of the source Git repository"
    ~docv:"URL"
    []

let commits =
  Arg.value @@
  Arg.(pos_right 1 string) [] @@
  Arg.info
    ~doc:"Git commit to use as context (full commit hash)"
    ~docv:"HASH"
    []

let pool =
  Arg.required @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Pool to use"
    ~docv:"ID"
    ["pool"]

let cache_hint =
  Arg.value @@
  Arg.(opt string) "" @@
  Arg.info
    ~doc:"Hint used to group similar builds to improve caching"
    ~docv:"STRING"
    ["cache-hint"]

let urgent =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Add job to the urgent queue"
    ["urgent"]

let push_to =
  let target_conv = Arg.conv Cluster_api.Docker.Image_id.(of_string, pp) in
  Arg.value @@
  Arg.(opt (some target_conv)) None @@
  Arg.info
    ~doc:"Where to docker-push the result"
    ~docv:"REPO:TAG"
    ["push-to"]

let push_user =
  Arg.value @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Docker registry user account to use when pushing"
    ~docv:"USER"
    ["push-user"]

let push_password_file =
  Arg.value @@
  Arg.(opt (some file)) None @@
  Arg.info
    ~doc:"File containing Docker registry password"
    ~docv:"PATH"
    ["push-password"]

let build_args =
  Arg.value @@
  Arg.(opt_all string) [] @@
  Arg.info
    ~doc:"Docker build argument"
    ~docv:"ARG"
    ["build-arg"]

let squash =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Whether to squash the layers"
    ["squash"]

let buildkit =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Whether to use BuildKit to build"
    ["buildkit"]

let include_git =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Include the .git clone in the build context"
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
  Term.(pure make $ push_to $ push_user $ push_password_file)

let build_options =
  let make build_args squash buildkit include_git =
    { Cluster_api.Docker.Spec.build_args; squash; buildkit; include_git }
  in
  Term.(pure make $ build_args $ squash $ buildkit $ include_git)

let submit_options_common =
  let make submission_path pool repository commits cache_hint urgent =
    { submission_path; pool; repository; commits; cache_hint; urgent }
  in
  Term.(pure make $ connect_addr $ pool $ repo $ commits $ cache_hint $ urgent)

let submit_docker_options =
  let make dockerfile push_to build_options =
    `Docker (dockerfile, push_to, build_options)
  in
  Term.(pure make $ dockerfile $ push_to $ build_options)

let submit_docker =
  let doc = "Submit a Docker build to the scheduler" in
  Term.(const submit $ submit_options_common $ submit_docker_options),
  Term.info "submit-docker" ~doc

let submit_obuilder_options =
  let make spec =
    `Obuilder spec
  in
  Term.(pure make $ local_obuilder)

let submit_obuilder =
  let doc = "Submit an OBuilder build to the scheduler" in
  Term.(const submit $ submit_options_common $ submit_obuilder_options),
  Term.info "submit-obuilder" ~doc

let pool_pos =
  Arg.pos 1 Arg.(some string) None @@
  Arg.info
    ~doc:"Pool to use"
    ~docv:"POOL"
    []

let worker =
  Arg.value @@
  Arg.pos 2 Arg.(some string) None @@
  Arg.info
    ~doc:"Worker id"
    ~docv:"WORKER"
    []

let show =
  let doc = "Show information about a service, pool or worker" in
  Term.(const show $ connect_addr $ Arg.value pool_pos),
  Term.info "show" ~doc

let pause =
  let doc = "Set a worker to be unavailable for further jobs" in
  Term.(const (set_active false) $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "pause" ~doc

let unpause =
  let doc = "Resume a paused worker" in
  Term.(const (set_active true) $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "unpause" ~doc

let update =
  let doc = "Drain and then update worker(s)" in
  Term.(const update $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "update" ~doc

let cmds = [submit_docker; submit_obuilder; show; pause; unpause; update]

let default_cmd =
  let doc = "a command-lint client for the build-scheduler" in
  let sdocs = Manpage.s_common_options in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "ocluster-client" ~doc ~sdocs ~version:Version.t

let () = Term.(exit @@ eval_choice default_cmd cmds)

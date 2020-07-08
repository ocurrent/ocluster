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

let submit submission_path pool dockerfile repository commits cache_hint urgent push_to build_args =
  let src =
    match repository, commits with
    | None, [] -> None
    | None, _ -> failwith "BUG: commits but no repository!"
    | Some repo, [] -> Fmt.failwith "No commits requested from repository %S!" repo
    | Some repo, commits -> Some (repo, commits)
  in
  run submission_path @@ fun submission_service ->
  Lwt_io.(with_file ~mode:input) dockerfile (Lwt_io.read ?count:None) >>= fun dockerfile ->
  let action = Cluster_api.Submission.docker_build ?push_to ~build_args dockerfile in
  let job = Cluster_api.Submission.submit submission_service ~urgent ~pool ~action ~cache_hint ?src in
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

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.required @@
  Arg.pos 0 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of .cap file from build-scheduler"
    ~docv:"ADDR"
    []

let dockerfile =
  Arg.required @@
  Arg.pos 1 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the Dockerfile to build"
    ~docv:"PATH"
    []

let repo =
  Arg.value @@
  Arg.pos 2 Arg.(some string) None @@
  Arg.info
    ~doc:"URL of the source Git repository"
    ~docv:"URL"
    []

let commits =
  Arg.value @@
  Arg.(pos_right 2 string) [] @@
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

let push_to =
  let make target user password =
    match target, user, password with
    | None, _, _ -> None
    | Some target, Some user, Some password_file ->
      let password = read_first_line password_file in
      Some { Cluster_api.Docker.Spec.target; user; password }
    | Some _, None, _ -> Fmt.failwith "Must use --push-user with --push-to"
    | Some _, Some _, None -> Fmt.failwith "Must use --push-password with --push-to"
  in
  Term.(pure make $ push_to $ push_user $ push_password_file)

let submit =
  let doc = "Submit a build to the scheduler" in
  Term.(const submit $ connect_addr $ pool $ dockerfile $ repo $ commits $ cache_hint $ urgent $ push_to $ build_args),
  Term.info "submit" ~doc

let pool_pos =
  Arg.value @@
  Arg.pos 1 Arg.(some string) None @@
  Arg.info
    ~doc:"Pool to use"
    ~docv:"ID"
    []

let show =
  let doc = "Show information about a service, pool or worker" in
  Term.(const show $ connect_addr $ pool_pos),
  Term.info "show" ~doc

let cmds = [submit; show]

let default_cmd =
  let doc = "a command-lint client for the build-scheduler" in
  let sdocs = Manpage.s_common_options in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "build-client" ~doc ~sdocs

let () = Term.(exit @@ eval_choice default_cmd cmds)

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
  Api.Job.log job start >>= function
  | Error (`Capnp e) -> Fmt.failwith "Error tailing logs: %a" Capnp_rpc.Error.pp e
  | Ok ("", _) -> Lwt.return_unit
  | Ok (data, next) ->
    output_string stdout data;
    flush stdout;
    tail job next

let main submission_path pool dockerfile repository commits cache_hint urgent push_to =
  let src =
    match repository, commits with
    | None, [] -> None
    | None, _ -> failwith "BUG: commits but no repository!"
    | Some repo, [] -> Fmt.failwith "No commits requested from repository %S!" repo
    | Some repo, commits -> Some (repo, commits)
  in
  try
    Lwt_main.run begin
      Lwt_io.(with_file ~mode:input) dockerfile (Lwt_io.read ?count:None) >>= fun dockerfile ->
      let vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Cap_file.load vat submission_path |> or_die in
      Sturdy_ref.connect_exn sr >>= fun submission_service ->
      let action = Api.Submission.docker_build ?push_to dockerfile in
      let job = Api.Submission.submit submission_service ~urgent ~pool ~action ~cache_hint ?src in
      Capability.dec_ref submission_service;
      let result = Api.Job.result job in
      Fmt.pr "Tailing log:@.";
      tail job 0L >>= fun () ->
      result >|= function
      | Ok "" -> ()
      | Ok x -> Fmt.pr "Result: %S@." x
      | Error (`Capnp e) ->
        Fmt.pr "%a.@." Capnp_rpc.Error.pp e;
        exit 1
    end
  with Failure msg ->
    Printf.eprintf "%s\n%!" msg;
    exit 1

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.value @@
  Arg.opt Arg.file "./capnp-secrets/submission.cap" @@
  Arg.info
    ~doc:"Path of submission.cap from build-scheduler"
    ~docv:"ADDR"
    ["submission-service"]

let dockerfile =
  Arg.required @@
  Arg.pos 0 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the Dockerfile to build"
    ~docv:"PATH"
    []

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
  let target_conv = Arg.conv Api.Submission.Target.(of_string, pp) in
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

let push_to =
  let make target user password =
    match target, user, password with
    | None, _, _ -> None
    | Some target, Some user, Some password_file ->
      let password = read_first_line password_file in
      Some { Api.Submission.Docker_build.target; user; password }
    | Some _, None, _ -> Fmt.failwith "Must use --push-user with --push-to"
    | Some _, Some _, None -> Fmt.failwith "Must use --push-password with --push-to"
  in
  Term.(pure make $ push_to $ push_user $ push_password_file)

let cmd =
  let doc = "Submit a build to the scheduler" in
  Term.(const main $ connect_addr $ pool $ dockerfile $ repo $ commits $ cache_hint $ urgent $ push_to),
  Term.info "build-client" ~doc

let () = Term.(exit @@ eval cmd)

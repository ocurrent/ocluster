open Lwt.Infix
open Capnp_rpc_lwt

let () =
  Logging.init ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let rec tail job start =
  Api.Job.log job start >>= function
  | Error (`Capnp e) -> Fmt.failwith "Error tailing logs: %a" Capnp_rpc.Error.pp e
  | Ok ("", _) -> Lwt.return_unit
  | Ok (data, next) ->
    output_string stdout data;
    flush stdout;
    tail job next

let main submission_path pool dockerfile repository commits cache_hint urgent =
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
      let job = Api.Submission.submit submission_service ~urgent ~pool ~dockerfile ~cache_hint ?src in
      Capability.dec_ref submission_service;
      (*     let status = Api.Job.exit_status job in *)
      Fmt.pr "Tailing log:@.";
      tail job 0L >>= fun () ->
      Fmt.pr "Job complete.@.";
      Lwt.return_unit
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

let cmd =
  let doc = "Submit a build to the scheduler" in
  Term.(const main $ connect_addr $ pool $ dockerfile $ repo $ commits $ cache_hint $ urgent),
  Term.info "build-client" ~doc

let () = Term.(exit @@ eval cmd)

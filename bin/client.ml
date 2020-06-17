open Lwt.Infix

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

let main submission_path dockerfile =
  Lwt_main.run begin
    Lwt_io.(with_file ~mode:input) dockerfile (Lwt_io.read ?count:None) >>= fun dockerfile ->
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat submission_path |> or_die in
    Capnp_rpc_lwt.Sturdy_ref.connect_exn sr >>= fun submission_service ->
    let cache_hint = "TODO" in
    let job = Api.Submission.submit submission_service ~dockerfile ~cache_hint in
    (*     let status = Api.Job.exit_status job in *)
    Fmt.pr "Tailing log:@.";
    tail job 0L >>= fun () ->
    Fmt.pr "Job complete.@.";
    Lwt.return_unit
  end

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

let cmd =
  let doc = "Submit a build to the scheduler" in
  Term.(const main $ connect_addr $ dockerfile),
  Term.info "build-client" ~doc

let () = Term.(exit @@ eval cmd)

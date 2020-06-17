open Lwt.Infix

let () =
  Logging.init ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let main registration_path capacity =
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
    Capnp_rpc_lwt.Sturdy_ref.connect_exn sr >>= fun registration_service ->
    Build_worker.run ~capacity registration_service
  end

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.value @@
  Arg.opt Arg.file "./capnp-secrets/register.cap" @@
  Arg.info
    ~doc:"Path of register.cap from build-scheduler"
    ~docv:"ADDR"
    ["registration-service"]

let capacity =
  Arg.value @@
  Arg.opt Arg.int 10 @@
  Arg.info
    ~doc:"The number of builds that can run in parallel"
    ~docv:"N"
    ["capacity"]

let cmd =
  let doc = "Run a build worker" in
  Term.(const main $ connect_addr $ capacity),
  Term.info "build-worker" ~doc

let () = Term.(exit @@ eval cmd)

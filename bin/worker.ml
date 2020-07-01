let () =
  Logging.init ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let main registration_path capacity name allow_push =
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
    Build_worker.run ~capacity ~name ~allow_push sr
  end

(* Command-line parsing *)

open Cmdliner

let worker_name =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Unique builder name"
    ~docv:"ID"
    ["name"]

let connect_addr =
  Arg.required @@
  Arg.pos 0 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of register.cap from build-scheduler"
    ~docv:"ADDR"
    []

let capacity =
  Arg.value @@
  Arg.opt Arg.int 10 @@
  Arg.info
    ~doc:"The number of builds that can run in parallel"
    ~docv:"N"
    ["capacity"]

let allow_push =
  Arg.value @@
  Arg.opt Arg.(list string) [] @@
  Arg.info
    ~doc:"Docker repositories to which users can push"
    ~docv:"REPO"
    ["allow-push"]

let cmd =
  let doc = "Run a build worker" in
  Term.(const main $ connect_addr $ capacity $ worker_name $ allow_push),
  Term.info "build-worker" ~doc

let () = Term.(exit @@ eval cmd)

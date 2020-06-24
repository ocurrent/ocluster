open Lwt.Infix
module Restorer = Capnp_rpc_net.Restorer

let ( / ) = Filename.concat

let () =
  Logging.init ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let export ~secrets_dir ~vat ~name id =
  let path = secrets_dir / (name ^ ".cap") in
  Capnp_rpc_unix.Cap_file.save_service vat id path |> or_die;
  Logs.app (fun f -> f "Wrote capability reference to %S" path)

let main capnp secrets_dir =
  Lwt_main.run begin
    let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri capnp in
    let services = Restorer.Table.create make_sturdy in
    let submission_id = Capnp_rpc_unix.Vat_config.derived_id capnp "submission" in
    let register_id = Capnp_rpc_unix.Vat_config.derived_id capnp "register" in
    let sched = Build_scheduler.create () in
    Restorer.Table.add services register_id (Build_scheduler.registration_service sched);
    Restorer.Table.add services submission_id (Build_scheduler.submission_service sched);
    let restore = Restorer.of_table services in
    Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
    export ~secrets_dir ~vat ~name:"register" register_id;
    export ~secrets_dir ~vat ~name:"submission" submission_id;
    fst @@ Lwt.wait ()  (* Wait forever *)
  end

(* Command-line parsing *)

open Cmdliner

let capnp_address =
  Arg.value @@
  Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None @@
  Arg.info
    ~doc:"Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let secrets_dir =
  Arg.required @@
  Arg.(opt (some dir)) (Some "./capnp-secrets") @@
  Arg.info
    ~doc:"Directory in which to store the Cap'n Proto secrets"
    ~docv:"DIR"
    ["secrets-dir"]

let cmd =
  let doc = "Manage build workers" in
  Term.(const main $ Capnp_rpc_unix.Vat_config.cmd $ secrets_dir),
  Term.info "build-scheduler" ~doc

let () = Term.(exit @@ eval cmd)

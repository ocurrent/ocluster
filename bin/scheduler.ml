open Lwt.Infix
open Capnp_rpc_lwt
module Restorer = Capnp_rpc_net.Restorer

let ( / ) = Filename.concat

let setup_log ?(formatter=Format.err_formatter) default_level =
  Prometheus_unix.Logging.init ~formatter ?default_level ();
  ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let export ~secrets_dir ~vat ~name id =
  let path = secrets_dir / (name ^ ".cap") in
  Capnp_rpc_unix.Cap_file.save_service vat id path |> or_die;
  Logs.app (fun f -> f "Wrote capability reference to %S" path)

(* [Lwt.choose] crashes if given an empty list. *)
let lwt_choose_safely = function
  | [] -> fst @@ Lwt.wait ()
  | xs -> Lwt.choose xs

let dir_exists d =
  match Unix.lstat d with
  | Unix.{ st_kind = S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> false

module Web = struct
  (* Provide metrics over HTTP. *)

  module Server = Cohttp_lwt_unix.Server

  let get_metrics ~sched ~pool ~worker ~source =
    match Cluster_scheduler.pool sched pool with
    | None ->
      Server.respond_error ~status:`Bad_request ~body:"No such pool" ()
    | Some pool_api ->
      match Cluster_scheduler.Pool_api.worker pool_api worker with
      | None -> Server.respond_error ~status:`Bad_request ~body:"Worker not connected" ()
      | Some worker_api ->
        Capability.with_ref worker_api @@ fun worker_api ->
        Cluster_api.Worker.metrics worker_api ~source >>= function
        | Error (`Capnp e) ->
          Logs.warn (fun f -> f "Error getting metrics for %S/%S: %a" pool worker Capnp_rpc.Error.pp e);
          Server.respond_error ~status:`Internal_server_error ~body:"Worker metrics collection failed" ()
        | Ok (content_type, data) ->
          let headers = Cohttp.Header.init_with "Content-Type" content_type in
          Server.respond_string ~status:`OK ~headers ~body:data ()

  let callback sched _conn req _body =
    let open Cohttp in
    let uri = Request.uri req in
    match Request.meth req, Astring.String.cuts ~empty:false ~sep:"/" (Uri.path uri) with
    | `GET, ["metrics"] ->
      let data = Prometheus.CollectorRegistry.(collect default) in
      let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
      let headers = Header.init_with "Content-Type" "text/plain; version=0.0.4" in
      Server.respond_string ~status:`OK ~headers ~body ()
    | `GET, ["pool"; pool; "worker"; worker; "metrics"] -> get_metrics ~sched ~pool ~worker ~source:`Agent
    | `GET, ["pool"; pool; "worker"; worker; "host-metrics"] -> get_metrics ~sched ~pool ~worker ~source:`Host
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()

  let serve ~sched = function
    | None -> []
    | Some port ->
      let mode = `TCP (`Port port) in
      let callback = callback sched in
      let thread = Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) in
      [thread]
end

let provision_client ~admin ~secrets_dir id =
  let path = secrets_dir / (Printf.sprintf "submit-%s.cap" id) in
  if Sys.file_exists path then Lwt.return_unit
  else (
    Capability.with_ref (Cluster_api.Admin.add_client admin id) @@ fun client ->
    Persistence.save_exn client >|= fun uri ->
    let data = Uri.to_string uri ^ "\n" in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 path in
    output_string oc data;
    close_out oc;
    Logs.app (fun f -> f "Wrote capability reference to %S" path)
  )

let main default_level ?formatter capnp secrets_dir pools prometheus_config state_dir default_clients =
  setup_log ?formatter default_level;
  if not (dir_exists state_dir) then Unix.mkdir state_dir 0o755;
  let db = Sqlite3.db_open (state_dir / "scheduler.db") in
  Sqlite3.busy_timeout db 1000;
  Db.exec_literal db "PRAGMA journal_mode=WAL";
  Db.exec_literal db "PRAGMA synchronous=NORMAL";
  let sched = Cluster_scheduler.create pools ~db in
  Lwt_main.run begin
    let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri capnp in
    let load ~validate ~sturdy_ref = function
      | Cluster_scheduler.Client, name ->
        Lwt.return @@ Restorer.grant (Cluster_scheduler.submission_service ~validate ~sturdy_ref sched name)
      | (ty, _) -> Fmt.failwith "Unknown SturdyRef type %a found in database!" Cluster_scheduler.Sqlite_loader.Ty.pp ty
    in
    let loader = Cluster_scheduler.Sqlite_loader.create ~make_sturdy ~load db in
    let services = Restorer.Table.of_loader (module Cluster_scheduler.Sqlite_loader) loader in
    let restore = Restorer.of_table services in
    let admin_id = Capnp_rpc_unix.Vat_config.derived_id capnp "admin" in
    let admin = Cluster_scheduler.admin_service sched ~restore ~loader in
    Restorer.Table.add services admin_id admin;
    let exports =
      Cluster_scheduler.registration_services sched |> List.map (fun (id, service) ->
          let name = "pool-" ^ id in
          let id = Capnp_rpc_unix.Vat_config.derived_id capnp name in
          Restorer.Table.add services id service;
          export ~secrets_dir ~name id
        )
    in
    default_clients |> Lwt_list.iter_s (provision_client ~admin ~secrets_dir) >>= fun () ->
    Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
    export ~secrets_dir ~vat ~name:"admin" admin_id;
    exports |> List.iter (fun f -> f ~vat);
    lwt_choose_safely (Web.serve ~sched prometheus_config)  (* Wait forever *)
  end

(* Command-line parsing *)

let main ~install (default_level, args1) ((capnp, secrets_dir, pools, prometheus_config, state_dir, default_clients), args2) =
  let (name, display, text) = ("ocluster-scheduler", "OCluster Scheduler", "Manage build workers") in
  if install then
    `Ok (Winsvc_wrapper.install name display text (args1 @args2))
  else
    `Ok (Winsvc_wrapper.run name state_dir (fun ?formatter () ->
             main default_level ?formatter capnp secrets_dir pools prometheus_config state_dir default_clients))

open Cmdliner

let pools =
  Arg.non_empty @@
  Arg.opt Arg.(list string) [] @@
  Arg.info
    ~doc:"Names of pools to create (e.g. linux-arm32,linux-ppc64)"
    ~docv:"POOLS"
    ["pools"]

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

let state_dir =
  Arg.required @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Directory in which to store the service's state"
    ~docv:"DIR"
    ["state-dir"]

let listen_prometheus =
  let doc =
    Arg.info ~docs:"MONITORING OPTIONS" ~docv:"PORT" ~doc:
      "Port on which to provide Prometheus metrics over HTTP."
      ["listen-prometheus"]
  in
  Arg.(value @@ opt (some int) None doc)

let default_clients =
  Arg.value @@
  Arg.(opt (list string)) [] @@
  Arg.info
    ~doc:"Clients to provision automatically"
    ~docv:"NAME"
    ["default-clients"]

let scheduler_opts_t =
  let scheduler_opts capnp secrets_dir pools prometheus_config state_dir default_clients  =
    (capnp, secrets_dir, pools, prometheus_config, state_dir, default_clients) in
  Term.(with_used_args
    (const scheduler_opts $ Capnp_rpc_unix.Vat_config.cmd $ secrets_dir $ pools
     $ listen_prometheus $ state_dir $ default_clients))

let cmd ~install =
  let doc = "Manage build workers" in
  let man = [
    `P "On $(b,Windows), specify '$(b,install)' as the first \
        command-line paramater to install the scheduler as a Windows \
        service with the specified parameters, and '$(b,remove)' to \
        remove the scheduler from the services." ] in
  Term.(ret (const (main ~install) $ with_used_args (Logs_cli.level ()) $ scheduler_opts_t)),
  Term.info "ocluster-scheduler" ~doc ~man ~version:Version.t

let () =
  match Array.to_list Sys.argv with
  | hd :: "install" :: argv ->
    Term.(exit @@ eval ~argv:(Array.of_list (hd :: argv)) (cmd ~install:true))
  | _ :: "remove" :: args ->
    if args <> [] then begin
      prerr_endline "'remove' should be used only once, in first position.";
      exit 1
    end else
      Winsvc_wrapper.remove "ocluster-scheduler"
  | _ ->
    Term.(exit @@ eval (cmd ~install:false))

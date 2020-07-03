open Lwt.Infix
module Restorer = Capnp_rpc_net.Restorer
module Api = Build_scheduler_api

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
  let callback sched _conn req _body =
    let open Cohttp in
    let uri = Request.uri req in
    match Request.meth req, Astring.String.cuts ~empty:false ~sep:"/" (Uri.path uri) with
    | `GET, ["metrics"] ->
      let data = Prometheus.CollectorRegistry.(collect default) in
      let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
      let headers = Header.init_with "Content-Type" "text/plain; version=0.0.4" in
      Server.respond_string ~status:`OK ~headers ~body ()
    | `GET, ["pool"; pool; "worker"; worker; "metrics"] ->
      begin match Build_scheduler.pool sched pool with
        | None ->
          Server.respond_error ~status:`Bad_request ~body:"No such pool" ()
        | Some pool_api ->
          match Build_scheduler.Pool_api.worker pool_api worker with
          | None -> Server.respond_error ~status:`Bad_request ~body:"Worker not connected" ()
          | Some worker_api ->
            Capnp_rpc_lwt.Capability.with_ref worker_api @@ fun worker_api ->
            Api.Worker.metrics worker_api >>= function
            | Error (`Capnp e) ->
              Logs.warn (fun f -> f "Error getting metrics for %S/%S: %a" pool worker Capnp_rpc.Error.pp e);
              Server.respond_error ~status:`Internal_server_error ~body:"Worker metrics collection failed" ()
            | Ok (version, data) ->
              let headers = Header.init_with "Content-Type" ("text/plain; version=" ^ version) in
              Server.respond_string ~status:`OK ~headers ~body:data ()
      end
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()

  let serve ~sched = function
    | None -> []
    | Some port ->
      let mode = `TCP (`Port port) in
      let callback = callback sched in
      let thread = Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) in
      [thread]
end

let main capnp secrets_dir pools prometheus_config state_dir =
  if not (dir_exists state_dir) then Unix.mkdir state_dir 0o755;
  let db = Sqlite3.db_open (state_dir / "scheduler.db") in
  Sqlite3.busy_timeout db 1000;
  Db.exec_literal db "PRAGMA journal_mode=WAL";
  Db.exec_literal db "PRAGMA synchronous=NORMAL";
  Lwt_main.run begin
    let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri capnp in
    let services = Restorer.Table.create make_sturdy in
    let submission_id = Capnp_rpc_unix.Vat_config.derived_id capnp "submission" in
    let sched = Build_scheduler.create ~db pools in
    Restorer.Table.add services submission_id (Build_scheduler.submission_service sched);
    let exports =
      Build_scheduler.registration_services sched |> List.map (fun (id, service) ->
          let name = "pool-" ^ id in
          let id = Capnp_rpc_unix.Vat_config.derived_id capnp name in
          Restorer.Table.add services id service;
          export ~secrets_dir ~name id
        )
    in
    let restore = Restorer.of_table services in
    Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
    export ~secrets_dir ~vat ~name:"submission" submission_id;
    exports |> List.iter (fun f -> f ~vat);
    lwt_choose_safely (Web.serve ~sched prometheus_config)  (* Wait forever *)
  end

(* Command-line parsing *)

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
  let open! Cmdliner in
  let doc =
    Arg.info ~docs:"MONITORING OPTIONS" ~docv:"PORT" ~doc:
      "Port on which to provide Prometheus metrics over HTTP."
      ["listen-prometheus"]
  in
  Arg.(value @@ opt (some int) None doc)

let cmd =
  let doc = "Manage build workers" in
  Term.(const main $ Capnp_rpc_unix.Vat_config.cmd $ secrets_dir $ pools $ listen_prometheus $ state_dir),
  Term.info "build-scheduler" ~doc

let () = Term.(exit @@ eval cmd)

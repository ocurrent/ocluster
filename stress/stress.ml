open Capnp_rpc_lwt
open Lwt.Infix

module Restorer = Capnp_rpc_net.Restorer

module Api = Cluster_api

let () =
  Printexc.record_backtrace true

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

let or_die = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let rec opam_repo_update () =
  Utils.sleep (Duration.of_sec 10) >>= fun () ->
  Utils.update_opam_repository ();
  Utils.sleep (Duration.of_hour 1) >>= opam_repo_update

let main prometheus_config =
  let vat_config =
    let socket = Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:5000  in
    Capnp_rpc_unix.Vat_config.create socket
      ~secret_key:`Ephemeral
      ~serve_tls:false
      ~public_address:socket
  in
  Lwt_main.run begin
    Mirage_crypto_rng_lwt.initialize ();
    let db = Sqlite3.db_open ":memory:" in
    Lwt.finalize (fun () ->
        let sched = Cluster_scheduler.create ~db ["linux-x86_64"] in
        let load ~validate ~sturdy_ref = function
          | Cluster_scheduler.Client, name ->
            Lwt.return @@ Restorer.grant (Cluster_scheduler.submission_service ~validate ~sturdy_ref sched name)
          | (ty, _) -> Fmt.failwith "Unknown SturdyRef type %a found in database!" Cluster_scheduler.Sqlite_loader.Ty.pp ty
        in
        let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri vat_config in
        let loader = Cluster_scheduler.Sqlite_loader.create ~make_sturdy ~load db in
        let services = Restorer.Table.of_loader (module Cluster_scheduler.Sqlite_loader) loader in
        let restore = Restorer.of_table services in
        Capnp_rpc_unix.serve ~restore vat_config >>= fun vat ->
        Capability.with_ref (Cluster_scheduler.admin_service ~loader ~restore sched) @@ fun admin ->
        let admin_id = Restorer.Id.public "admin" in
        Restorer.Table.add services admin_id admin;
        Capnp_rpc_unix.Cap_file.save_service vat admin_id "./capnp-secrets/stress-admin.cap" |> or_die;
        Capability.with_ref (Api.Admin.add_client admin "ocaml-ci") @@ fun ocaml_ci ->
        Capability.with_ref (Api.Admin.add_client admin "health") @@ fun health ->
        Capability.with_ref (Api.Admin.pool admin "linux-x86_64") @@ fun x86_64_admin ->
        Api.Pool_admin.set_rate x86_64_admin ~client_id:"ocaml-ci" 100.0 >>= fun () ->
        match Cluster_scheduler.registration_services sched with
        | [] | _ :: _ :: _ -> assert false
        | [(_, x86_64_reg)] ->
          Capability.with_ref x86_64_reg @@ fun x86_64_reg ->
          Lwt.choose ([
              Ci.thread ocaml_ci;
              Health_check.thread health;
              opam_repo_update ();
              Worker.thread x86_64_reg ~name:"build1" ~capacity:32;
              Worker.thread x86_64_reg ~name:"build2" ~capacity:64;
              Worker.thread x86_64_reg ~name:"build3" ~capacity:64;
              Worker.thread x86_64_reg ~name:"build4" ~capacity:64;
              Worker.thread x86_64_reg ~name:"build5" ~capacity:128;
            ] @ Prometheus_unix.serve prometheus_config)
      )
      (fun () -> if Sqlite3.db_close db then Lwt.return_unit else failwith "close: DB busy!")
  end

open Cmdliner

let cmd =
  let doc = "Test the scheduler" in
  let info = Cmd.info "stress" ~doc in
  Cmd.v info Term.(const main $ Prometheus_unix.opts)

let () = exit @@ Cmd.eval cmd

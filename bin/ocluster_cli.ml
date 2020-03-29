open Lwt.Infix
open Capnp_rpc_lwt
open Ocluster_api

(* Verbose logging *)
let pp_qid f = function
  | None -> ()
  | Some x ->
      let s = Stdint.Uint32.to_string x in
      Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
    let qid = Logs.Tag.find Capnp_rpc.Debug.qid_tag tags in
    let print _ =
      Fmt.(pf stdout) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stdout
      ("%a %a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let run_client cluster =
  let hostname = "alpha" in
  let cmd = ("find", [| "/tmp/"; "-type"; "f" |]) in
  Capability.with_ref cluster @@ fun t ->
  let agent = Client.Cluster.find ~hostname t in
  Client.Agent.exec ~cmd agent >>= function
  | Error (`Capnp err) ->
      Fmt.pr "error: %a\n%!" Capnp_rpc.Error.pp err;
      exit 1
  | Ok r ->
      Logs.info (fun l ->
          l "exit: %ld\nstdout: %s\nstderr %s" r.Client.Agent.exit_code
            r.Client.Agent.stdout r.Client.Agent.stderr);
      Lwt.return ()

let connect uri =
  Lwt_main.run
    ( Logs.info (fun l ->
          l "Connecting to cluster service at: %a" Uri.pp_hum uri);
      let client_vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
      Sturdy_ref.connect_exn sr >>= fun proxy_to_service ->
      run_client proxy_to_service )

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd =
  let doc = "run the client" in
  (Term.(const connect $ connect_addr), Term.info "connect" ~doc)

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Debug);
  Term.eval connect_cmd |> Term.exit

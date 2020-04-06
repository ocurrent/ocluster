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
  Capability.with_ref Ocluster.Server.agent @@ fun callback ->
  match Ocluster.Agents.hostinfo () with
  | Error (`Msg msg) ->
      Logs.err (fun l -> l "hostinfo: %s" msg);
      exit 1
  | Ok hostinfo ->
      Client.ClusterMember.register ~hostname ~callback ~hostinfo cluster
      >>= fun () ->
      let t, _ = Lwt.wait () in
      Logs.info (fun l -> l "Registered with cluster");
      (* TODO register sig handler for unregister *)
      t

let connect uri =
  Lwt_main.run
    ( Logs.info (fun l ->
          l "Connecting to cluster service at: %a@." Uri.pp_hum uri);
      let client_vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
      Sturdy_ref.connect_exn sr >>= fun proxy_to_service ->
      run_client proxy_to_service )

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd =
  let doc = "run the agent" in
  (Term.(const connect $ connect_addr), Term.info "agent" ~doc)

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Debug);
  Term.eval connect_cmd |> Term.exit

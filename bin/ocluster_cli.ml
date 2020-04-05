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
  let cmd = ("find", [| "find"; "/tmp/" |]) in
  Capability.with_ref cluster @@ fun t ->
  let agent = Client.ClusterUser.find ~hostname t in
  let stdout_s, stdout_push = Lwt_stream.create () in
  let stderr_s, stderr_push = Lwt_stream.create () in
  let on_complete_t, on_complete_u = Lwt.task () in
  let pout =
    Ocluster.Server.process_out stdout_push stderr_push on_complete_u
  in
  let pin = Client.Agent.spawn cmd pout agent in
  let output_stdout_t = Lwt_stream.iter_s Lwt_io.(write stdout) stdout_s in
  let output_stderr_t = Lwt_stream.iter_s Lwt_io.(write stderr) stderr_s in
  let exit_t =
    on_complete_t >>= fun exit_code ->
    output_stdout_t >>= fun _ ->
    Logs.info (fun l -> l "exit code %ld" exit_code);
    Capability.dec_ref pin;
    exit (Int32.to_int exit_code)
  in
  output_stdout_t <&> output_stderr_t <&> exit_t

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

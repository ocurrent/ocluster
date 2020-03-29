open Lwt.Infix
open Capnp_rpc_net
open Ocluster

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

let cap_file = "cluster.cap"

let serve config =
  Agents.init () >>= fun agents ->
  Lwt_main.run
    (let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
     let restore = Restorer.single service_id (Server.cluster agents) in
     Capnp_rpc_unix.serve config ~restore >>= fun vat ->
     match Capnp_rpc_unix.Cap_file.save_service vat service_id cap_file with
     | Error (`Msg m) -> failwith m
     | Ok () ->
         Fmt.pr "Server running. Connect using %S.@." cap_file;
         fst @@ Lwt.wait ()
     (* Wait forever *))

open Cmdliner

let serve_cmd =
  ( Term.(const serve $ Capnp_rpc_unix.Vat_config.cmd),
    let doc = "run the server" in
    Term.info "serve" ~doc )

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Debug);
  Term.eval serve_cmd |> Term.exit

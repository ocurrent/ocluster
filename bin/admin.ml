open Lwt.Infix
open Capnp_rpc_lwt

let () =
  Logging.init ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run cap_path fn =
  try
    Lwt_main.run begin
      let vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Cap_file.load vat cap_path |> or_die in
      Sturdy_ref.connect_exn sr >>= fun service ->
      Capability.with_ref service fn
    end
  with Failure msg ->
    Printf.eprintf "%s\n%!" msg;
    exit 1

let show cap_path pool =
  run cap_path @@ fun admin_service ->
  match pool with
  | None ->
    Cluster_api.Admin.pools admin_service >|= fun pools ->
    List.iter print_endline pools
  | Some pool ->
    Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
    Cluster_api.Pool_admin.show pool >|= fun status ->
    print_endline (String.trim status)

let set_active active cap_path pool worker =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
  match worker with
  | Some worker ->
    Cluster_api.Pool_admin.set_active pool worker active
  | None ->
    Cluster_api.Pool_admin.workers pool >|= function
    | [] ->
      Fmt.epr "No workers connected to pool!@.";
      exit 1
    | workers ->
      let pp_active f = function
        | true -> Fmt.string f "active"
        | false -> Fmt.string f "paused"
      in
      let pp_worker_info f { Cluster_api.Pool_admin.name; active } =
        Fmt.pf f "%s (%a)" name pp_active active
      in
      Fmt.epr "@[<v>Specify which worker you want to affect. Candidates are:@,%a@."
        Fmt.(list ~sep:cut pp_worker_info) workers;
      exit 1

let pp_worker_info f { Cluster_api.Pool_admin.name; _ } =
  Fmt.string f name

let update cap_path pool worker =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
  match worker with
  | Some worker ->
    begin
      Cluster_api.Pool_admin.update pool worker >|= function
      | Ok () -> Fmt.pr "Restarted@."
      | Error (`Capnp ex) ->
        Fmt.pr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
    end
  | None ->
    Cluster_api.Pool_admin.workers pool >>= function
    | [] ->
      Fmt.epr "No workers connected to pool!@.";
      exit 1
    | w :: ws ->
      Fmt.pr "Testing update on first worker in pool: %S@." w.name;
      Cluster_api.Pool_admin.update pool w.name >>= function
      | Error (`Capnp ex) ->
        Fmt.pr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
      | Ok () ->
        Fmt.pr "Canary updated OK. Updating the others: [%a]@."
          Fmt.(list ~sep:sp pp_worker_info) ws;
        ws
        |> List.map (fun (w:Cluster_api.Pool_admin.worker_info) ->
            Cluster_api.Pool_admin.update pool w.name >|= function
            | Ok () -> Fmt.pr "%S restarted OK.@." w.name
            | Error (`Capnp ex) ->
              Fmt.pr "%S: %a@." w.name Capnp_rpc.Error.pp ex;
              failwith "Failed update(s)"
          )
        |> Lwt.join >|= fun () ->
        Fmt.pr "All pool workers restarted.@."

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.required @@
  Arg.pos 0 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of admin.cap file from build-scheduler"
    ~docv:"ADDR"
    []

let pool_pos =
  Arg.pos 1 Arg.(some string) None @@
  Arg.info
    ~doc:"Pool to use"
    ~docv:"POOL"
    []

let worker =
  Arg.value @@
  Arg.pos 2 Arg.(some string) None @@
  Arg.info
    ~doc:"Worker id"
    ~docv:"WORKER"
    []

let show =
  let doc = "Show information about a service, pool or worker" in
  Term.(const show $ connect_addr $ Arg.value pool_pos),
  Term.info "show" ~doc

let pause =
  let doc = "Set a worker to be unavailable for further jobs" in
  Term.(const (set_active false) $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "pause" ~doc

let unpause =
  let doc = "Resume a paused worker" in
  Term.(const (set_active true) $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "unpause" ~doc

let update =
  let doc = "Drain and then update worker(s)" in
  Term.(const update $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "update" ~doc

let cmds = [show; pause; unpause; update]

let default_cmd =
  let doc = "a command-line admin client for the build-scheduler" in
  let sdocs = Manpage.s_common_options in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "ocluster-admin" ~doc ~sdocs ~version:Version.t

let () = Term.(exit @@ eval_choice default_cmd cmds)

open Lwt.Infix
open Capnp_rpc_lwt

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run cap_path fn =
  try
    Lwt_main.run begin
      let vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Cap_file.load vat cap_path |> or_die in
      Capnp_rpc_unix.with_cap_exn sr fn
    end
  with Failure msg ->
    Printf.eprintf "%s\n%!" msg;
    exit 1

let add_client () cap_path id =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.add_client admin_service id) @@ fun client ->
  Persistence.save_exn client >|= fun uri ->
  print_endline (Uri.to_string uri)

let remove_client () cap_path id =
  run cap_path @@ fun admin_service ->
  Cluster_api.Admin.remove_client admin_service id

let list_clients () cap_path =
  run cap_path @@ fun admin_service ->
  Cluster_api.Admin.list_clients admin_service >|= function
  | [] -> Fmt.epr "No clients.@."
  | clients -> List.iter print_endline clients

let set_rate () cap_path pool_id client_id rate =
  run cap_path @@ fun admin_service ->
  let pool = Cluster_api.Admin.pool admin_service pool_id in
  Cluster_api.Pool_admin.set_rate pool ~client_id rate

let show () cap_path pool =
  run cap_path @@ fun admin_service ->
  match pool with
  | None ->
    Cluster_api.Admin.pools admin_service >|= fun pools ->
    List.iter print_endline pools
  | Some pool ->
    Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
    Cluster_api.Pool_admin.show pool >|= fun status ->
    print_endline (String.trim status)

let with_progress label =
  Capability.with_ref (Cluster_api.Progress.local (Fmt.pr "%s: %s@." label))

let drain pool workers =
  Fmt.pr "Waiting for jobs to finish...@.";
  let jobs = workers |> List.map (fun w ->
      with_progress w @@ fun progress ->
      Cluster_api.Pool_admin.drain ~progress pool w
    ) in
  Lwt.join jobs

let set_active active () all auto_create wait cap_path pool worker =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
  match worker with
  | Some worker ->
    Cluster_api.Pool_admin.set_active ~auto_create pool worker active >>= fun () ->
    begin
      if wait then drain pool [worker]
      else Lwt.return_unit
    end >|= fun () ->
    Fmt.pr "Success.@."
  | None ->
    Cluster_api.Pool_admin.workers pool >>= function
    | [] ->
      Fmt.pr "No workers connected to pool (nothing to do).@.";
      Lwt.return_unit
    | workers when all ->
      let workers =
        workers |> List.filter_map (fun { Cluster_api.Pool_admin.name; active = prev; connected = _ } ->
          if prev = active then (Fmt.pr "(worker %S is already done)@." name; None)
          else Some name
        )
      in
      if workers = [] then (
        Fmt.pr "Nothing to do.@.";
        Lwt.return_unit
      ) else (
        Fmt.pr "Updating %a...@." Fmt.(list ~sep:comma string) workers;
        let set worker =
          Cluster_api.Pool_admin.set_active pool worker active
        in
        Lwt.join (List.map set workers) >>= fun () ->
        begin
          if wait then drain pool workers
          else Lwt.return_unit
        end >|= fun () ->
        Fmt.pr "Success.@."
      )
    | workers ->
      Fmt.epr "@[<v>Specify which worker you want to affect (or use --all).@,Candidates are:@,%a@."
        Fmt.(list ~sep:cut Cluster_api.Pool_admin.pp_worker_info) workers;
      exit 1

let pp_worker_name f { Cluster_api.Pool_admin.name; _ } =
  Fmt.string f name

let update () cap_path pool worker =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
  match worker with
  | Some worker ->
    begin
      with_progress worker @@ fun progress ->
      Cluster_api.Pool_admin.update ~progress pool worker >|= function
      | Ok () -> Fmt.pr "Restarted@."
      | Error (`Capnp ex) ->
        Fmt.pr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
    end
  | None ->
    let is_connected w = w.Cluster_api.Pool_admin.connected in
    Cluster_api.Pool_admin.workers pool >>= fun workers ->
    let connected, disconnected = List.partition is_connected workers in
    match connected with
    | [] ->
      Fmt.epr "No workers connected to pool!@.";
      exit 1
    | w :: ws ->
      Fmt.pr "Testing update on first worker in pool: %S@." w.name;
      with_progress w.name (fun progress -> Cluster_api.Pool_admin.update ~progress pool w.name)
      >>= function
      | Error (`Capnp ex) ->
        Fmt.pr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
      | Ok () ->
        Fmt.pr "Canary updated OK. Updating the others: [%a]@."
          Fmt.(list ~sep:sp pp_worker_name) ws;
        ws
        |> List.map (fun (w:Cluster_api.Pool_admin.worker_info) ->
            with_progress w.name @@ fun progress ->
            Cluster_api.Pool_admin.update ~progress pool w.name >|= function
            | Ok () -> Fmt.pr "%S restarted OK.@." w.name
            | Error (`Capnp ex) ->
              Fmt.pr "%S: %a@." w.name Capnp_rpc.Error.pp ex;
              failwith "Failed update(s)"
          )
        |> Lwt.join >|= fun () ->
        Fmt.pr "All pool workers restarted.@.";
        if disconnected <> [] then (
          Fmt.pr "@[<v2>WARNING: disconnected workers not updated:@,%a@]@."
            Fmt.(list ~sep:cut pp_worker_name) disconnected
        )

let forget () cap_path pool worker =
  run cap_path @@ fun admin_service ->
  Capability.with_ref (Cluster_api.Admin.pool admin_service pool) @@ fun pool ->
  match worker with
  | Some worker ->
    begin
      Cluster_api.Pool_admin.forget pool worker >|= function
      | Ok () -> ()
      | Error (`Capnp ex) ->
        Fmt.pr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
    end
  | None ->
    Cluster_api.Pool_admin.workers pool >>= function
    | [] ->
      Fmt.epr "No workers in pool!@.";
      exit 1
    | workers ->
      let disconnected = List.filter (fun i -> not i.Cluster_api.Pool_admin.connected) workers in
      let workers = if disconnected = [] then workers else disconnected in
      Fmt.epr "@[<v>Specify which worker you want to forget.@,Candidates are:@,%a@."
        Fmt.(list ~sep:cut Cluster_api.Pool_admin.pp_worker_info) workers;
      exit 1

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of admin.cap file from build-scheduler"
    ~docv:"ADDR"
    ["c"; "connect"]

let client_id ~pos =
  Arg.pos pos Arg.(some string) None @@
  Arg.info
    ~doc:"Unique name or ID for the client"
    ~docv:"ID"
    []

let pool_pos =
  Arg.pos 0 Arg.(some string) None @@
  Arg.info
    ~doc:"Pool to use"
    ~docv:"POOL"
    []

let rate ~pos =
  Arg.pos pos Arg.(some float) None @@
  Arg.info
    ~doc:"Number of parallel jobs"
    ~docv:"RATE"
    []

let worker =
  Arg.value @@
  Arg.pos 1 Arg.(some string) None @@
  Arg.info
    ~doc:"Worker id"
    ~docv:"WORKER"
    []

let all =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"All workers"
    ["all"]

let auto_create =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Create worker first if unknown"
    ["auto-create"]

let wait =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Wait until no jobs are running"
    ["wait"]

let add_client =
  let doc = "Create a new client endpoint for submitting jobs" in
  Term.(const add_client $ Logging.term $ connect_addr $ Arg.required (client_id ~pos:0)),
  Term.info "add-client" ~doc

let remove_client =
  let doc = "Unregister a client." in
  Term.(const remove_client $ Logging.term $ connect_addr $ Arg.required (client_id ~pos:0)),
  Term.info "remove-client" ~doc

let list_clients =
  let doc = "List registered clients" in
  Term.(const list_clients $ Logging.term $ connect_addr),
  Term.info "list-clients" ~doc

let set_rate =
  let doc = "Set expected number of parallel jobs for a pool/client combination" in
  Term.(const set_rate $ Logging.term $ connect_addr $ Arg.required pool_pos $ Arg.required (client_id ~pos:1) $ Arg.required (rate ~pos:2)),
  Term.info "set-rate" ~doc

let show =
  let doc = "Show information about a service, pool or worker" in
  Term.(const show $ Logging.term $ connect_addr $ Arg.value pool_pos),
  Term.info "show" ~doc

let pause =
  let doc = "Set a worker to be unavailable for further jobs" in
  Term.(const (set_active false) $ Logging.term $ all $ auto_create $ wait $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "pause" ~doc

let unpause =
  let doc = "Resume a paused worker" in
  Term.(const (set_active true) $ Logging.term $ all $ auto_create $ const false $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "unpause" ~doc

let update =
  let doc = "Drain and then update worker(s)" in
  Term.(const update $ Logging.term $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "update" ~doc

let forget =
  let doc = "Forget about an old worker" in
  Term.(const forget $ Logging.term $ connect_addr $ Arg.required pool_pos $ worker),
  Term.info "forget" ~doc

let cmds = [add_client; remove_client; list_clients; set_rate; show; pause; unpause; update; forget]

let default_cmd =
  let doc = "a command-line admin client for the build-scheduler" in
  let sdocs = Manpage.s_common_options in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "ocluster-admin" ~doc ~sdocs ~version:Version.t

let () = Term.(exit @@ eval_choice ~argv:Options.argv default_cmd cmds)

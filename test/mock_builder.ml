open Lwt.Infix

let ( / ) = Filename.concat

let state_dir = Sys.getcwd () / "var"

type outcome = (string, [`Cancelled | `Msg of string]) result

type t = {
  replies : (string, (outcome Lwt.t * outcome Lwt.u)) Hashtbl.t;
  cond : unit Lwt_condition.t;
}

let create () =
  { replies = Hashtbl.create 10; cond = Lwt_condition.create () }

let reply t id =
  match Hashtbl.find_opt t.replies id with
  | Some r -> r
  | None ->
    let r = Lwt.wait () in
    Hashtbl.add t.replies id r;
    Lwt_condition.broadcast t.cond ();
    r

let get t id = fst (reply t id)
let set t id v = Lwt.wakeup (snd (reply t id)) v

let rec await t id =
  match Hashtbl.find_opt t.replies id with
  | Some (result, _) -> Lwt.return result
  | None ->
    Logs.info (fun f -> f "Waiting for %S job to arrive" id);
    Lwt_condition.wait t.cond >>= fun () ->
    await t id

let build t ~switch ~log ~src:_ ~secrets:_ v =
  let docker_build dockerfile =
    match dockerfile with
    | `Path _ -> assert false
    | `Contents dockerfile ->
      Logs.info (fun f -> f "Mock build got %S" dockerfile);
      Cluster_worker.Log_data.write log (Fmt.str "Building %s@." dockerfile);
      let reply = get t dockerfile in
      Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
          if Lwt.state reply = Lwt.Sleep then
            set t dockerfile @@ Error `Cancelled;
          Lwt.return_unit
        ) >>= fun () ->
      reply >|= fun reply ->
      Hashtbl.remove t.replies dockerfile;
      reply
  in
  match v with
  | `Obuilder _ -> assert false
  | `Docker (v, _) -> docker_build v
  | `Custom c ->
    let open Cluster_api in
    match Custom.kind c with
    | "dockerfile" ->
      let module Db = Raw.Reader.DockerBuild in
      let payload = Custom.payload c in
      let r = Raw.Reader.of_pointer payload in
      let contents =
        match Db.Dockerfile.get (Db.dockerfile_get r) with
        | Contents c -> `Contents c
        | Path p -> `Path p
        | Undefined _ -> Fmt.failwith "Unknown Dockerfile file"
      in
      docker_build contents
    | "obuilder" as kind ->
      let payload = Custom.payload c in
      let obuilder_spec = Raw.Reader.OBuilder.spec_get @@ Raw.Reader.of_pointer payload in
      Logs.info (fun f -> f "Got mock custom build %S" kind);
      Cluster_worker.Log_data.write log (Fmt.str "Building job %s@." kind);
      Cluster_worker.Log_data.write log (Fmt.str "Got spec %s@." obuilder_spec);
      let reply = get t kind in
      Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
          if Lwt.state reply = Lwt.Sleep then
            set t kind @@ Error `Cancelled;
          Lwt.return_unit
        ) >>= fun () ->
      reply >|= fun reply ->
      Hashtbl.remove t.replies kind;
      reply
    | v -> invalid_arg ("Unsupported kind of custom build: " ^ v)

let update () =
  Logs.info (fun f -> f "Mock download updates…");
  Lwt.return (fun () -> failwith "Mock restart")

let run ?(capacity=1) ?(name="worker-1") ~switch t registration_service =
  let thread = Cluster_worker.run ~switch ~capacity ~name ~build:(build t) ~update ~state_dir registration_service in
  Lwt.on_failure thread
    (fun ex -> if Lwt_switch.is_on switch then raise ex)

let run_remote ~builder_switch ~network_switch ?(capacity=1) ?(name="worker-1") t registration_service =
  let thread =
    let registration_service = Mock_network.remote ~switch:network_switch registration_service in
    Cluster_worker.run ~switch:builder_switch ~capacity ~name ~build:(build t) ~update ~state_dir registration_service
  in
  Lwt.on_failure thread
    (fun ex ->
       Logs.debug (fun f -> f "Mock builder failed: %a" Fmt.exn ex);
       if Lwt_switch.is_on builder_switch && Lwt_switch.is_on network_switch then raise ex
    )

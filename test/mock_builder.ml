open Lwt.Infix

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

let docker_build t ~switch ~log ~src:_ ~options:_ = function
  | `Path _ -> assert false
  | `Contents dockerfile ->
    Logs.info (fun f -> f "Mock build got %S" dockerfile);
    Cluster_worker.Log_data.write log (Fmt.strf "Building %s@." dockerfile);
    let reply = get t dockerfile in
    Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
        if Lwt.state reply = Lwt.Sleep then
          set t dockerfile @@ Error `Cancelled;
        Lwt.return_unit
      ) >>= fun () ->
    reply >|= fun reply ->
    Hashtbl.remove t.replies dockerfile;
    reply

let run ?(capacity=1) ?(name="worker-1") ~switch t registration_service =
  let thread = Cluster_worker.run ~switch ~capacity ~name ~docker_build:(docker_build t) registration_service in
  Lwt.on_failure thread
    (fun ex -> if Lwt_switch.is_on switch then raise ex)

let run_remote ~builder_switch ~network_switch ?(capacity=1) ?(name="worker-1") t registration_service =
  let thread =
    let registration_service = Mock_network.remote ~switch:network_switch registration_service in
    Cluster_worker.run ~switch:builder_switch ~capacity ~name ~docker_build:(docker_build t) registration_service
  in
  Lwt.on_failure thread
    (fun ex ->
       Logs.debug (fun f -> f "Mock builder failed: %a" Fmt.exn ex); 
       if Lwt_switch.is_on builder_switch && Lwt_switch.is_on network_switch then raise ex
    )

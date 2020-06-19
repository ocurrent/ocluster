open Lwt.Infix
open Capnp_rpc_lwt

type t = {
  replies : (string, (Unix.process_status Lwt.t * Unix.process_status Lwt.u)) Hashtbl.t;
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

let build t ~switch ~stdin ~stdout =
  Lwt_io.read stdin >>= fun dockerfile ->
  Logs.info (fun f -> f "Mock build got %S" dockerfile);
  Lwt_io.close stdin >>= fun () ->
  Lwt_io.write stdout (Fmt.strf "Building %s@." dockerfile) >>= fun () ->
  Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
      set t dockerfile (Unix.WSIGNALED (-11));
      Lwt.return_unit
    ) >>= fun () ->
  get t dockerfile >>= fun reply ->
  Hashtbl.remove t.replies dockerfile;
  Lwt_io.close stdout >|= fun () ->
  reply

let docker_build t () =
  let status, set_status = Lwt.wait () in
  let from_stdin, to_stdin = Lwt_io.pipe () in
  let from_stdout, to_stdout = Lwt_io.pipe () in
  let switch = Lwt_switch.create () in
  Lwt.async (fun () -> build t ~switch ~stdin:from_stdin ~stdout:to_stdout >|= Lwt.wakeup set_status);
  object
    method status = status
    method stdin = to_stdin
    method stdout = from_stdout
    method terminate =
      if Lwt.state status = Lwt.Sleep then (
        Logs.info (fun f -> f "Cancelling job");
        Lwt.async (fun () -> Lwt_switch.turn_off switch)
      )
  end

let run ?(capacity=1) ~switch t registration_service =
  let thread = Build_worker.run ~switch ~capacity ~docker_build:(docker_build t) registration_service in
  Lwt.on_failure thread
    (fun ex -> if Lwt_switch.is_on switch then raise ex)

let run_remote ~builder_switch ~network_switch ?(capacity=1) t registration_service =
  let thread =
    Capability.with_ref (Mock_network.remote ~switch:network_switch registration_service) @@ fun registration_service ->
    Build_worker.run ~switch:builder_switch ~capacity ~docker_build:(docker_build t) registration_service
  in
  Lwt.on_failure thread
    (fun ex ->
       Logs.debug (fun f -> f "Mock builder failed: %a" Fmt.exn ex); 
       if Lwt_switch.is_on builder_switch && Lwt_switch.is_on network_switch then raise ex
    )

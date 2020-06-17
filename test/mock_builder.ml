open Lwt.Infix

type t = {
  replies : (string, (Unix.process_status Lwt.t * Unix.process_status Lwt.u)) Hashtbl.t
}

let create () =
  { replies = Hashtbl.create 10 }

let reply t id =
  match Hashtbl.find_opt t.replies id with
  | Some r -> r
  | None ->
    let r = Lwt.wait () in
    Hashtbl.add t.replies id r;
    r

let get t id = fst (reply t id)
let set t id v = Lwt.wakeup (snd (reply t id)) v

let build t ~stdin ~stdout =
  Lwt_io.read stdin >>= fun dockerfile ->
  Logs.info (fun f -> f "Mock build got %S" dockerfile);
  Lwt_io.close stdin >>= fun () ->
  Lwt_io.write stdout (Fmt.strf "Building %s@." dockerfile) >>= fun () ->
  get t dockerfile >>= fun reply ->
  Hashtbl.remove t.replies dockerfile;
  Lwt_io.close stdout >|= fun () ->
  reply

let docker_build t () =
  let status, set_status = Lwt.wait () in
  let from_stdin, to_stdin = Lwt_io.pipe () in
  let from_stdout, to_stdout = Lwt_io.pipe () in
  Lwt.async (fun () -> build t ~stdin:from_stdin ~stdout:to_stdout >|= Lwt.wakeup set_status);
  object
    method status = status
    method stdin = to_stdin
    method stdout = from_stdout
  end

let run ?(capacity=1) t registration_service =
  Lwt.async
    (fun () ->
       Build_worker.run ~capacity ~docker_build:(docker_build t) registration_service
    )

let run_remote ~switch ?(capacity=1) t registration_service =
  let registration_service = Mock_network.remote ~switch registration_service in
  let thread = Build_worker.run ~capacity ~docker_build:(docker_build t) registration_service in
  Lwt.on_failure thread
    (fun ex -> if Lwt_switch.is_on switch then raise ex)

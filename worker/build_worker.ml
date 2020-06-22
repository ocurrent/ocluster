open Lwt.Infix
open Capnp_rpc_lwt

module Log_data = Log_data
module Process = Process

let build ~switch ~docker_build ~log descr =
  let module R = Api.Raw.Reader.JobDescr in
  let dockerfile = R.dockerfile_get descr in
  let cache_hint = R.cache_hint_get descr in
  Log.info (fun f -> f "Got request to build (%s):\n%s" cache_hint (String.trim dockerfile));
  docker_build ~switch ~log dockerfile >|= function
  | Error `Cancelled ->
    Log_data.write log (Fmt.strf "Job cancelled");
    Log.info (fun f -> f "Job cancelled");
    Error (`Msg "Build cancelled")
  | Ok () ->
    Log_data.write log "Job succeeded\n";
    Log.info (fun f -> f "Job succeeded");
    Ok ()
  | Error (`Exit_code n) ->
    Log_data.write log (Fmt.strf "Docker build exited with status %d\n" n);
    Log.info (fun f -> f "Job failed");
    Error (`Msg "Build failed")
  | Error (`Msg msg) ->
    Log_data.write log (msg ^ "\n");
    Log.info (fun f -> f "Job failed: %s" msg);
    Error (`Msg "Build failed")

let docker_build ~switch ~log dockerfile =
  Process.exec ~switch ~log ~stdin:dockerfile ["docker"; "build"]

let run ?switch ?(docker_build=docker_build) ~capacity registration_service =
  let cond = Lwt_condition.create () in
  let in_use = ref 0 in
  let pop_thread = ref None in
  Lwt_switch.add_hook_or_exec switch (fun () ->
      Log.info (fun f -> f "Switch turned off. Will shut down.");
      !pop_thread |> Option.iter Lwt.cancel;
      Lwt_condition.broadcast cond ();
      Lwt.return_unit
    )
  >>= fun () ->
  Capability.with_ref (Api.Registration.register registration_service ~name:"worker-1") @@ fun queue ->
  let rec loop () =
    match switch with
    | Some switch when not (Lwt_switch.is_on switch) ->
      Log.info (fun f -> f "Builder shutting down (switch turned off)");
      Lwt.return ()
    | _ ->
      if !in_use >= capacity then (
        Log.info (fun f -> f "At capacity. Waiting for a build to finish before requesting more...");
        Lwt_condition.wait cond >>= loop
      ) else (
        incr in_use;
        let outcome, set_outcome = Lwt.wait () in
        let log = Log_data.create () in
        Log.info (fun f -> f "Requesting a new job...");
        let switch = Lwt_switch.create () in
        let pop =
          Capability.with_ref (Api.Job.local ~switch ~outcome ~stream_log_data:(Log_data.stream log)) @@ fun job ->
          Api.Queue.pop queue job
        in
        pop_thread := Some pop;
        pop >>= fun request ->
        Lwt.async (fun () ->
            Lwt.finalize
              (fun () ->
                 Lwt.try_bind
                   (fun () -> build ~switch ~docker_build ~log request)
                   (fun outcome ->
                      Log_data.close log;
                      Lwt.wakeup set_outcome outcome;
                      Lwt.return_unit)
                   (fun ex ->
                      Log.warn (fun f -> f "Build failed: %a" Fmt.exn ex);
                      Log_data.write log (Fmt.strf "Uncaught exception: %a" Fmt.exn ex);
                      Log_data.close log;
                      Lwt.wakeup_exn set_outcome ex;
                      Lwt.return_unit)
              )
              (fun () ->
                 decr in_use;
                 Lwt_switch.turn_off switch >>= fun () ->
                 Lwt_condition.broadcast cond ();
                 Lwt.return_unit)
          );
        loop ()
      )
  in
  loop ()

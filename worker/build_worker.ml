open Lwt.Infix
open Capnp_rpc_lwt

module Log_data = Log_data
module Process = Process

module Metrics = struct
  open Prometheus

  let namespace = "scheduler"
  let subsystem = "worker"

  let running_jobs =
    let help = "Number of jobs currently running" in
    Gauge.v ~help ~namespace ~subsystem "running_jobs"
end

let min_reconnect_time = 10.0   (* Don't try to connect more than once per 10 seconds *)

type t = {
  build : switch:Lwt_switch.t -> log:Log_data.t -> src:string -> string -> (unit, Process.error) Lwt_result.t;
  registration_service : Api.Raw.Client.Registration.t Sturdy_ref.t;
  capacity : int;
  mutable in_use : int;                (* Number of active builds *)
  cond : unit Lwt_condition.t;         (* Fires when a build finishes (or switch turned off) *)
  mutable cancel : unit -> unit;       (* Called if switch is turned off *)
}

let build ~switch ~log t descr =
  let module R = Api.Raw.Reader.JobDescr in
  let dockerfile = R.dockerfile_get descr in
  let cache_hint = R.cache_hint_get descr in
  Log.info (fun f -> f "Got request to build (%s):\n%s" cache_hint (String.trim dockerfile));
  begin
    Context.with_build_context ~switch ~log descr @@ fun src ->
    t.build ~switch ~log ~src dockerfile
  end
  >|= function
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

let loop ~switch t queue =
  let rec loop () =
    match switch with
    | Some switch when not (Lwt_switch.is_on switch) ->
      Log.info (fun f -> f "Builder shutting down (switch turned off)");
      Lwt.return `Cancelled
    | _ ->
      if t.in_use >= t.capacity then (
        Log.info (fun f -> f "At capacity. Waiting for a build to finish before requesting more...");
        Lwt_condition.wait t.cond >>= loop
      ) else (
        let outcome, set_outcome = Lwt.wait () in
        let log = Log_data.create () in
        Log.info (fun f -> f "Requesting a new job...");
        let switch = Lwt_switch.create () in
        let pop =
          Capability.with_ref (Api.Job.local ~switch ~outcome ~stream_log_data:(Log_data.stream log)) @@ fun job ->
          Api.Queue.pop queue job
        in
        t.cancel <- (fun () -> Lwt.cancel pop);
        pop >>= fun request ->
        t.in_use <- t.in_use + 1;
        Prometheus.Gauge.set Metrics.running_jobs (float_of_int t.in_use);
        Lwt.async (fun () ->
            Lwt.finalize
              (fun () ->
                 Lwt.try_bind
                   (fun () -> build ~switch ~log t request)
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
                 t.in_use <- t.in_use - 1;
                 Prometheus.Gauge.set Metrics.running_jobs (float_of_int t.in_use);
                 Lwt_switch.turn_off switch >>= fun () ->
                 Lwt_condition.broadcast t.cond ();
                 Lwt.return_unit)
          );
        loop ()
      )
  in
  loop ()

let docker_build ~switch ~log ~src dockerfile =
  Process.exec ~switch ~log ~stdin:dockerfile ["docker"; "build"; "-f"; "-"; src]

let metrics () =
  let data = Prometheus.CollectorRegistry.(collect default) in
  "0.0.4", Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data

let run ?switch ?(docker_build=docker_build) ~capacity ~name registration_service =
  let t = {
    registration_service;
    build = docker_build;
    cond = Lwt_condition.create ();
    capacity;
    in_use = 0;
    cancel = ignore;
  } in
  Lwt_switch.add_hook_or_exec switch (fun () ->
      Log.info (fun f -> f "Switch turned off. Will shut down.");
      t.cancel ();
      Lwt_condition.broadcast t.cond ();
      Lwt.return_unit
    )
  >>= fun () ->
  let rec reconnect () =
    let connect_time = Unix.gettimeofday () in
    Lwt.catch
      (fun () ->
         Sturdy_ref.connect_exn t.registration_service >>= fun reg ->
         Capability.with_ref reg @@ fun reg ->
         let queue =
           let api = Api.Worker.local ~metrics in
           let queue = Api.Registration.register reg ~name api in
           Capability.dec_ref api;
           queue
         in
         Capability.with_ref queue @@ fun queue ->
         Lwt.catch
           (fun () -> loop ~switch t queue)
           (fun ex ->
              Lwt.pause () >>= fun () ->
              match Capability.problem queue, switch with
              | _, Some switch when not (Lwt_switch.is_on switch) -> Lwt.return `Cancelled
              | Some problem, _ ->
                Log.info (fun f -> f "Worker loop failed (probably because queue connection failed): %a" Fmt.exn ex);
                Lwt.fail (Failure (Fmt.to_to_string Capnp_rpc.Exception.pp problem))    (* Will retry *)
              | None, _ ->
                Lwt.return (`Crash ex)
           )
      )
      (fun ex ->
         let delay = max 0.0 (connect_time +. min_reconnect_time -. Unix.gettimeofday ()) in
         Log.info (fun f -> f "Lost connection to scheduler (%a). Will retry in %.1fs..." Fmt.exn ex delay);
         Lwt_unix.sleep delay >>= reconnect
      )
  in
  reconnect () >>= function
  | `Cancelled -> Lwt.return_unit
  | `Crash ex -> Lwt.fail ex

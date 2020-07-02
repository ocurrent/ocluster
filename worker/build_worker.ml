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

let ( >>!= ) = Lwt_result.bind

let docker_push_lock = Lwt_mutex.create ()

let read_file path =
  let ch = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ch)
    (fun () ->
       let len = in_channel_length ch in
       really_input_string ch len
    )

let min_reconnect_time = 10.0   (* Don't try to connect more than once per 10 seconds *)

type t = {
  build : switch:Lwt_switch.t -> log:Log_data.t -> src:string -> string -> (string, Process.error) Lwt_result.t;
  registration_service : Api.Raw.Client.Registration.t Sturdy_ref.t;
  capacity : int;
  mutable in_use : int;                (* Number of active builds *)
  cond : unit Lwt_condition.t;         (* Fires when a build finishes (or switch turned off) *)
  mutable cancel : unit -> unit;       (* Called if switch is turned off *)
  allow_push : string list;            (* Repositories users can push to *)
}

let docker_push ~switch ~log t hash { Api.Submission.Docker_build.target; user; password } =
  let repo = Api.Submission.Target.repo target in
  let target = Api.Submission.Target.to_string target in
  Log.info (fun f -> f "Push %S to %S as user %S" hash target user);
  Log_data.info log "Pushing %S to %S as user %S" hash target user;
  (* "docker push" rather stupidly requires us to tag the image locally with the same
     name that we're trying to push to before we can push. However, until we push we
     don't know whether the user has permission to access that repository. For example,
     the user could ask us to push to "ocurrent/build-worker" and we'd tag that before
     realising they didn't have access. So for now, only allow pushing to repositories
     listed in [allow_push]. *)
  if not (List.mem repo t.allow_push) then
    Lwt_result.fail (`Msg (Fmt.strf "To allow pushing to this repository, start the worker with --allow-push %S" repo))
  else (
    Lwt_mutex.with_lock docker_push_lock @@ fun () ->
    Lwt_io.with_temp_dir ~prefix:"build-worker-" ~suffix:"-docker" @@ fun config_dir ->
    let docker args = "docker" :: "--config" :: config_dir :: args in
    Process.exec ~switch ~log ~stdin:password ~stderr:`Keep @@ docker ["login"; "--password-stdin"; "--username"; user] >>= function
    | Error (`Exit_code _) ->
      Lwt_result.fail (`Msg (Fmt.strf "Failed to docker-login as %S" user))
    | Error _ as e ->
      Lwt.return e
    | Ok () ->
      Process.exec ~switch ~log @@ docker ["tag"; "--"; hash; target] >>!= fun () ->
      Process.exec ~switch ~log @@ docker ["push"; "--"; target] >>!= fun () ->
      Lwt_process.pread_line ("", [| "docker"; "image"; "inspect"; "-f"; "{{index .RepoDigests 0}}"; "--"; target |]) >>= function
      | "" -> Lwt_result.fail (`Msg "Failed to read RepoDigests for newly-pushed image!")
      | repo_id -> Lwt_result.return repo_id
  )

let build ~switch ~log t descr =
  let module R = Api.Raw.Reader.JobDescr in
  let cache_hint = R.cache_hint_get descr in
  let Docker_build { dockerfile; push_to } = Api.Submission.get_action descr in
  Log.info (fun f -> f "Got request to build (%s):\n%s" cache_hint (String.trim dockerfile));
  begin
    Context.with_build_context ~switch ~log descr @@ fun src ->
    t.build ~switch ~log ~src dockerfile >>!= fun hash ->
    match push_to with
    | None -> Lwt_result.return ""
    | Some target -> docker_push ~switch ~log t hash target
  end
  >|= function
  | Error `Cancelled ->
    Log_data.write log (Fmt.strf "Job cancelled");
    Log.info (fun f -> f "Job cancelled");
    Error (`Msg "Build cancelled")
  | Ok output ->
    Log_data.write log "Job succeeded\n";
    Log.info (fun f -> f "Job succeeded");
    Ok output
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
                      Log_data.write log (Fmt.strf "Uncaught exception: %a@." Fmt.exn ex);
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
  let iid_file = Filename.temp_file "build-worker-" ".iid" in
  Lwt.finalize
    (fun () ->
       Process.exec ~switch ~log ~stdin:dockerfile ["docker"; "build"; "--iidfile"; iid_file; "-f"; "-"; src] >>!= fun () ->
       Lwt_result.return (String.trim (read_file iid_file))
    )
    (fun () ->
       if Sys.file_exists iid_file then Lwt_unix.unlink iid_file
       else Lwt.return_unit
    )

let metrics () =
  let data = Prometheus.CollectorRegistry.(collect default) in
  "0.0.4", Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data

let run ?switch ?(docker_build=docker_build) ?(allow_push=[]) ~capacity ~name registration_service =
  let t = {
    registration_service;
    build = docker_build;
    cond = Lwt_condition.create ();
    capacity;
    in_use = 0;
    cancel = ignore;
    allow_push;
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

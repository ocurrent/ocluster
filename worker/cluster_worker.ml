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
  name : string;
  build :
    switch:Lwt_switch.t ->
    log:Log_data.t ->
    src:string ->
    build_args:string list ->
    string ->
    (string, Process.error) Lwt_result.t;
  registration_service : Cluster_api.Raw.Client.Registration.t Sturdy_ref.t;
  capacity : int;
  mutable in_use : int;                (* Number of active builds *)
  cond : unit Lwt_condition.t;         (* Fires when a build finishes (or switch turned off) *)
  mutable cancel : unit -> unit;       (* Called if switch is turned off *)
  allow_push : string list;            (* Repositories users can push to *)
}

let docker_push ~switch ~log t hash { Cluster_api.Docker.Spec.target; user; password } =
  let repo = Cluster_api.Docker.Image_id.repo target in
  let target = Cluster_api.Docker.Image_id.to_string target in
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
      Lwt_process.pread_line ("", [| "docker"; "image"; "inspect"; "-f"; "{{ range index .RepoDigests}}{{ . }} {{ end }}"; "--"; target |]) >>= function
      | "" -> Lwt_result.fail (`Msg "Failed to read RepoDigests for newly-pushed image!")
      | ids ->
        let open Astring in
        (* Sometimes this mysteriously includes multi-arch manifests from other repositories. Possibly it happens when
           the image is something we're also running locally (e.g. our image has the same hash as "ocurrent/build-worker:live").
           We don't want to return a multi-arch image here, because "docker manifest" would reject that. *)
        match List.find_opt (String.is_prefix ~affix:(repo ^ "@")) (String.cuts ~sep:" " ids) with
        | Some repo_id -> Lwt_result.return repo_id
        | None -> Lwt_result.fail (`Msg (Fmt.strf "Can't find target repository '%s@...' in list %S!" repo ids))
  )

let build ~switch ~log t descr =
  let module R = Cluster_api.Raw.Reader.JobDescr in
  let cache_hint = R.cache_hint_get descr in
  let Docker_build { dockerfile; build_args; push_to } = Cluster_api.Submission.get_action descr in
  Log.info (fun f -> f "Got request to build (%s):\n%s" cache_hint (String.trim dockerfile));
  begin
    Context.with_build_context ~switch ~log descr @@ fun src ->
    t.build ~switch ~log ~src ~build_args dockerfile >>!= fun hash ->
    match push_to with
    | None -> Lwt_result.return ""
    | Some target -> docker_push ~switch ~log t hash target
  end
  >|= function
  | Error `Cancelled ->
    Log_data.write log "Job cancelled\n";
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
          Capability.with_ref (Cluster_api.Job.local ~switch ~outcome ~stream_log_data:(Log_data.stream log)) @@ fun job ->
          Cluster_api.Queue.pop queue job
        in
        t.cancel <- (fun () -> Lwt.cancel pop);
        pop >>= fun request ->
        t.in_use <- t.in_use + 1;
        Prometheus.Gauge.set Metrics.running_jobs (float_of_int t.in_use);
        Lwt.async (fun () ->
            Lwt.finalize
              (fun () ->
                 Lwt.try_bind
                   (fun () ->
                      Log_data.info log "Building on %s" t.name;
                      build ~switch ~log t request
                   )
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

let docker_build ~switch ~log ~src ~build_args dockerfile =
  let iid_file = Filename.temp_file "build-worker-" ".iid" in
  Lwt.finalize
    (fun () ->
       let args =
         List.concat_map (fun x -> ["--build-arg"; x]) build_args
         @ ["--pull"; "--iidfile"; iid_file; "-f"; "-"; src]
       in
       Logs.info (fun f -> f "docker build @[%a@]" Fmt.(list ~sep:sp (quote string)) args);
       Process.exec ~switch ~log ~stdin:dockerfile ("docker" :: "build" :: args) >>!= fun () ->
       Lwt_result.return (String.trim (read_file iid_file))
    )
    (fun () ->
       if Sys.file_exists iid_file then Lwt_unix.unlink iid_file
       else Lwt.return_unit
    )

let metrics = function
  | `Agent ->
    let data = Prometheus.CollectorRegistry.(collect default) in
    let content_type = "text/plain; version=0.0.4; charset=utf-8" in
    Lwt_result.return (content_type, Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data)
  | `Host ->
    Lwt.catch
      (fun () ->
         Cohttp_lwt_unix.Client.get (Uri.of_string "http://127.0.0.1:9100/metrics") >>= fun (resp, body) ->
         match Cohttp.Response.status resp with
         | `OK ->
           begin match Cohttp.Header.get (Cohttp.Response.headers resp) "content-type" with
             | Some content_type ->
               body |> Cohttp_lwt.Body.to_string >|= fun body ->
               Ok (content_type, body)
             | None ->
               Lwt.return @@ Fmt.error_msg "Missing Content-Type in HTTP response from prometheus-node-exporter"
           end
         | code ->
           Log.warn (fun f -> f "prometheus-node-exporter: %s" (Cohttp.Code.string_of_status code));
           Lwt.return @@ Fmt.error_msg "prometheus-node-exporter: %s" (Cohttp.Code.string_of_status code)
      )
      (fun ex ->
         Log.warn (fun f -> f "Failed to connect to prometheus-node-exporter: %a" Fmt.exn ex);
         Lwt.return @@ Fmt.error_msg "Failed to connect to prometheus-node-exporter"
      )

let run ?switch ?(docker_build=docker_build) ?(allow_push=[]) ~capacity ~name registration_service =
  let t = {
    name;
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
           let api = Cluster_api.Worker.local ~metrics in
           let queue = Cluster_api.Registration.register reg ~name api in
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

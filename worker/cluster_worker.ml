open Lwt.Infix
open Capnp_rpc_lwt

module Log_data = Log_data
module Process = Process

module Metrics = struct
  open Prometheus

  let namespace = "ocluster"
  let subsystem = "worker"

  let jobs_accepted =
    let help = "Number of jobs accepted in total" in
    Counter.v ~help ~namespace ~subsystem "jobs_accepted_total"

  let job_time =
    let help = "Time jobs ran for" in
    Summary.v_label ~label_name:"result" ~help ~namespace ~subsystem "job_time_seconds"

  let docker_push_time =
    let help = "Time uploading to Docker Hub" in
    Summary.v ~help ~namespace ~subsystem "docker_push_time_seconds"

  let docker_prune_time =
    let help = "Time spent pruning Docker cache" in
    Summary.v ~help ~namespace ~subsystem "docker_prune_time_seconds"

  let running_jobs =
    let help = "Number of jobs currently running" in
    Gauge.v ~help ~namespace ~subsystem "running_jobs"
end

module Self_update = struct
  let service = "builder_agent"
  let repo = "ocurrent/ocluster-worker"
  let tag = "live"
end

let buildkit_env =
  let orig = Unix.environment () |> Array.to_list in
  "DOCKER_BUILDKIT=1" :: orig |> Array.of_list

let ( >>!= ) = Lwt_result.bind
let ( / ) = Filename.concat

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
    options:Cluster_api.Docker.Spec.options ->
    [ `Contents of string | `Path of string ] ->
    (string, [`Cancelled | `Msg of string]) Lwt_result.t;
  prune_threshold : float option;      (* docker-prune when free space is lower than this (percentage) *)
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
    let login_cmd = docker ["login"; "--password-stdin"; "--username"; user] in
    Process.exec ~label:"docker-login" ~switch ~log ~stdin:password ~stderr:`Keep login_cmd >>= function
    | Error (`Exit_code _) ->
      Lwt_result.fail (`Msg (Fmt.strf "Failed to docker-login as %S" user))
    | Error (`Msg _ | `Cancelled as e) -> Lwt_result.fail e
    | Ok () ->
      Process.check_call ~label:"docker-tag" ~switch ~log @@ docker ["tag"; "--"; hash; target] >>!= fun () ->
      Process.check_call ~label:"docker-push" ~switch ~log @@ docker ["push"; "--"; target] >>!= fun () ->
      Lwt_process.pread_line ("", [| "docker"; "image"; "inspect"; "-f"; "{{ range index .RepoDigests }}{{ . }} {{ end }}"; "--"; target |]) >>= function
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
  let Docker_build { dockerfile; options; push_to } = Cluster_api.Submission.get_action descr in
  Log.info (fun f ->
      match dockerfile with
      | `Contents contents -> f "Got request to build (%s):\n%s" cache_hint (String.trim contents)
      | `Path path -> f "Got request to build %S (%s)" path cache_hint
    );
  begin
    Context.with_build_context ~log descr @@ fun src ->
    t.build ~switch ~log ~src ~options dockerfile >>!= fun hash ->
    match push_to with
    | None -> Lwt_result.return ""
    | Some target ->
      Prometheus.Summary.time Metrics.docker_push_time Unix.gettimeofday
        (fun () -> docker_push ~switch ~log t hash target)
  end
  >|= function
  | Error `Cancelled ->
    Log_data.write log "Job cancelled\n";
    Log.info (fun f -> f "Job cancelled");
    Error (`Msg "Build cancelled"), "cancelled"
  | Ok output ->
    Log_data.write log "Job succeeded\n";
    Log.info (fun f -> f "Job succeeded");
    Ok output, "ok"
  | Error (`Msg msg) ->
    Log_data.write log (msg ^ "\n");
    Log.info (fun f -> f "Job failed: %s" msg);
    Error (`Msg "Build failed"), "fail"

let check_docker_partition t =
  match t.prune_threshold with
  | None -> Lwt_result.return ()
  | Some prune_threshold ->
    Lwt_process.pread ("", [| "df"; "/var/lib/docker"; "--output=pcent" |]) >|= fun lines ->
    match String.split_on_char '\n' (String.trim lines) with
    | [_; result] ->
      let used =
        try Scanf.sscanf result " %f%%" Fun.id
        with _ -> Fmt.failwith "Expected %S, got %S" "xx%" result
      in
      let free = 100. -. used in
      Log.info (fun f -> f "Docker partition: %.0f%% free" free);
      if free < prune_threshold then Error `Disk_space_low
      else Ok ()
    | _ ->
      Fmt.failwith "Expected two lines from df, but got:@,%S" lines

let rec maybe_prune t queue =
  check_docker_partition t >>= function
  | Ok () -> Lwt.return_unit
  | Error `Disk_space_low ->
    Log.info (fun f -> f "Disk-space low. Will finish current jobs and then prune.");
    Cluster_api.Queue.set_active queue false >>= fun () ->
    let rec drain () =
      if t.in_use = 0 then Lwt.return_unit
      else Lwt_condition.wait t.cond >>= drain
    in
    drain () >>= fun () ->
    Log.info (fun f -> f "All jobs finished. Pruning...");
    Prometheus.Summary.time Metrics.docker_prune_time Unix.gettimeofday
      (fun () ->
         Lwt_process.exec ("", [| "docker"; "system"; "prune"; "-af" |]) >>= function
         | Unix.WEXITED 0 ->
           Lwt_process.exec ("", [| "docker"; "builder"; "prune"; "-af" |])
         | e -> Lwt.return e
      )
    >>= function
    | Unix.WEXITED 0 ->
      begin
        check_docker_partition t >>= function
        | Ok () ->
          Log.info (fun f -> f "Prune complete. Re-activating queue...");
          Cluster_api.Queue.set_active queue true
        | Error `Disk_space_low ->
          Log.warn (fun f -> f "Disk-space still low after pruning! Will retry in one hour.");
          Unix.sleep (60 * 60);
          maybe_prune t queue
      end
    | _ ->
      Log.warn (fun f -> f "docker prune command failed! Will retry in one hour.");
      Unix.sleep (60 * 60);
      maybe_prune t queue

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
        maybe_prune t queue >>= fun () ->
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
        Prometheus.Counter.inc_one Metrics.jobs_accepted;
        Lwt.async (fun () ->
            Lwt.finalize
              (fun () ->
                 let t0 = Unix.gettimeofday () in
                 Lwt.try_bind
                   (fun () ->
                      Log_data.info log "Building on %s" t.name;
                      build ~switch ~log t request
                   )
                   (fun (outcome, metric_label) ->
                      let t1 = Unix.gettimeofday () in
                      Prometheus.Summary.observe (Metrics.job_time metric_label) (t1 -. t0);
                      Log_data.close log;
                      Lwt.wakeup set_outcome outcome;
                      Lwt.return_unit)
                   (fun ex ->
                      let t1 = Unix.gettimeofday () in
                      Prometheus.Summary.observe (Metrics.job_time "error") (t1 -. t0);
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

let error_msg fmt =
  fmt |> Fmt.kstrf @@ fun x -> Error (`Msg x)

(* Check [path] points below [src]. Don't follow symlinks. *)
let check_contains ~path src =
  match Fpath.of_string path with
  | Error (`Msg m) -> Error (`Msg m)
  | Ok path ->
    let path = Fpath.normalize path in
    if Fpath.is_abs path then error_msg "%a is an absolute path!" Fpath.pp path
    else (
      let rec aux ~src = function
        | [] -> error_msg "Empty path!"
        | x :: _ when Fpath.is_rel_seg x -> error_msg "Relative segment in %a" Fpath.pp path
        | "" :: _ -> error_msg "Empty segment in %a!" Fpath.pp path
        | x :: xs ->
          let src = src / x in
          match Unix.lstat src with
          | Unix.{ st_kind = S_DIR; _ } -> aux ~src xs
          | Unix.{ st_kind = S_REG; _ } when xs = [] -> Ok src
          | _ -> error_msg "%S is not a directory (in %a)" x Fpath.pp path
          | exception Unix.Unix_error(Unix.ENOENT, _, _) -> error_msg "%S does not exist (in %a)" x Fpath.pp path
      in
      aux ~src (Fpath.segs path)
    )

let write_to_file ~path data =
  Lwt_io.(with_file ~mode:output) ~flags:Unix.[O_TRUNC; O_CREAT; O_RDWR] path @@ fun ch ->
  Lwt_io.write_from_string_exactly ch data 0 (String.length data)

let docker_build ~switch ~log ~src ~options dockerfile =
  let iid_file = Filename.temp_file "build-worker-" ".iid" in
  Lwt.finalize
    (fun () ->
       begin
         match dockerfile with
         | `Contents contents ->
           let path = src / "Dockerfile" in
           write_to_file ~path contents >>= fun () ->
           Lwt_result.return path
         | `Path "-" -> Lwt_result.fail (`Msg "Path cannot be '-'!")
         | `Path path ->
           match check_contains ~path src with
           | Ok path -> Lwt_result.return path
           | Error e -> Lwt_result.fail e
       end >>!= fun dockerpath ->
       let { Cluster_api.Docker.Spec.build_args; squash; buildkit; include_git = _ } = options in
       let args =
         List.concat_map (fun x -> ["--build-arg"; x]) build_args
         @ (if squash then ["--squash"] else [])
         @ ["--pull"; "--iidfile"; iid_file; "-f"; dockerpath; src]
       in
       Log.info (fun f -> f "docker build @[%a@]" Fmt.(list ~sep:sp (quote string)) args);
       let env = if buildkit then Some buildkit_env else None in
       Process.check_call ~label:"docker-build" ?env ~switch ~log ("docker" :: "build" :: args) >>!= fun () ->
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

let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED x -> Fmt.failwith "Sub-process failed with exit code %d" x
  | Unix.WSIGNALED x -> Fmt.failwith "Sub-process failed with signal %d" x
  | Unix.WSTOPPED x -> Fmt.failwith "Sub-process stopped with signal %d" x

let self_update t queue =
  let image_name = Printf.sprintf "%s:%s" Self_update.repo Self_update.tag in
  Lwt_process.exec ("", [| "docker"; "pull"; image_name |]) >|= check_exit_status >>= fun () ->
  Lwt_process.pread_line ("", [| "docker"; "image"; "inspect"; "-f";
                                 "{{ range index .RepoDigests }}{{ . }} {{ end }}"; "--"; image_name |]) >>= fun new_repo_ids ->
  let new_repo_ids = Astring.String.cuts ~sep:" " new_repo_ids in
  Lwt_process.pread_line ("", [| "docker"; "service"; "inspect"; Self_update.service;
                                 "--format"; "{{ .Spec.TaskTemplate.ContainerSpec.Image }}" |]) >>= fun current_repo_id ->
  match String.trim current_repo_id with
  | "" -> Lwt.return @@ error_msg "Failed to inspect service %S" Self_update.service
  | current_repo_id ->
    Log.info (fun f -> f "@[<v>Checking for updates. Running now:@,%s@,Latest available:@,%a@]" current_repo_id
                 Fmt.(list ~sep:cut string) new_repo_ids);
    if List.mem current_repo_id new_repo_ids then (
      Log.info (fun f -> f "Already running the latest version");
      Lwt_result.return false
    ) else (
      let affix = Self_update.repo ^ "@" in
      match List.find_opt (Astring.String.is_prefix ~affix) new_repo_ids with
      | None ->
        Lwt.return @@ error_msg "No new image starts with %S!" affix
      | Some id ->
        Log.info (fun f -> f "Need to update service %s -> %s" current_repo_id id);
        Cluster_api.Queue.set_active queue false >|= fun () ->
        Lwt.async (fun () ->
            Lwt_unix.sleep 1.0 >>= fun () ->
            let rec drain () =
              if t.in_use = 0 then Lwt.return_unit
              else Lwt_condition.wait t.cond >>= drain
            in
            drain () >>= fun () ->
            Log.info (fun f -> f "All jobs finished. Updating...");
            Lwt_process.exec ("", [| "docker"; "service"; "update"; "--image"; id; Self_update.service |])
            >|= check_exit_status >>= fun () ->
            Log.warn (fun f -> f "Update succeeded but... we should probably have stopped running by now.");
            Lwt_unix.sleep 10.0 >>= fun () ->
            exit 1      (* Give up *)
          );
        Ok true
    )

let run ?switch ?(docker_build=docker_build) ?(allow_push=[]) ?prune_threshold ~capacity ~name registration_service =
  begin match prune_threshold with
    | None -> Log.info (fun f -> f "Prune threshold not set. Will not check for low disk-space!")
    | Some frac when frac < 0.0 || frac > 100.0 -> Fmt.invalid_arg "prune_threshold must be in the range 0 to 100"
    | Some _ -> ()
  end;
  let t = {
    name;
    prune_threshold;
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
           let update = ref (fun () -> Lwt_result.fail (`Msg "Not initialised yet")) in
           let api = Cluster_api.Worker.local ~metrics ~self_update:(fun () -> !update ()) in
           let queue = Cluster_api.Registration.register reg ~name api in
           Capability.dec_ref api;
           update := (fun () -> self_update t queue);
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

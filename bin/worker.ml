open Lwt.Infix

let setup_log ?style_renderer ?formatter default_level =
  (* Smother Cap'n Proto and TLS logging sources *)
  let levels = ["capnp-rpc"; "tls.config"; "tls.tracing"; "endpoint"]
               |> List.map (fun src -> src, Logs.Warning) in
  Prometheus_unix.Logging.init ?formatter ?default_level ~levels ();
  Fmt_tty.setup_std_outputs ?style_renderer ();
  ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED x -> Fmt.failwith "Sub-process failed with exit code %d" x
  | Unix.WSIGNALED x -> Fmt.failwith "Sub-process failed with signal %d" x
  | Unix.WSTOPPED x -> Fmt.failwith "Sub-process stopped with signal %d" x

module Self_update = struct
  let service = "builder_agent"
  let repo = "ocurrent/ocluster-worker"
  let tag = "live"
end

let update_docker () =
  let image_name = Printf.sprintf "%s:%s" Self_update.repo Self_update.tag in
  Lwt_process.exec ("", [| "docker"; "pull"; image_name |]) >|= check_exit_status >>= fun () ->
  Lwt_process.pread_line ("", [| "docker"; "image"; "inspect"; "-f";
                                 "{{ range index .RepoDigests }}{{ . }} {{ end }}"; "--"; image_name |]) >|= fun new_repo_ids ->
  let new_repo_ids = Astring.String.cuts ~sep:" " new_repo_ids in
  let affix = Self_update.repo ^ "@" in
  match List.find_opt (Astring.String.is_prefix ~affix) new_repo_ids with
  | None ->
    Fmt.failwith "No new image starts with %S!" affix
  | Some id ->
    Logs.info (fun f -> f "Latest service version is %s" id);
    fun () ->
      Lwt_process.exec ("", [| "docker"; "service"; "update"; "--image"; id; Self_update.service |])
      >|= check_exit_status

(* Respond to update requests by doing nothing, on the assumption that the
   admin has updated the local package version. *)
let update_normal () =
  Lwt.return (fun () -> Lwt.return_unit)

let main ?style_renderer level ?formatter registration_path capacity name allow_push healthcheck_period prune_threshold docker_max_df_size obuilder_prune_threshold obuilder_prune_limit state_dir obuilder additional_metrics =
  setup_log ?style_renderer ?formatter level;
  let update =
    if Sys.file_exists "/.dockerenv" then update_docker
    else update_normal
  in
  let _check_for_duplicates =
    List.fold_left (fun acc (v, _) -> if List.mem v acc then failwith ("Duplicate additional metrics name: " ^ v) else v :: acc) [] additional_metrics
  in
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
    Cluster_worker.run ~capacity ~name ~allow_push ~healthcheck_period ?prune_threshold ?docker_max_df_size ~obuilder_prune_threshold ~obuilder_prune_limit ?obuilder ~additional_metrics ~state_dir ~update sr
  end

(* Command-line parsing *)
let main ~install (style_renderer, args1) (level, args2) ((registration_path, capacity, name, allow_push, healthcheck_period, prune_threshold, docker_max_df_size, obuilder_prune_threshold, obuilder_prune_limit, state_dir, obuilder, additional_metrics), args3) =
  if install then
    Ok (Winsvc_wrapper.install name "OCluster Worker" "Run a build worker" (args1 @ args2 @ args3))
  else
    Ok (Winsvc_wrapper.run name state_dir (fun ?formatter () ->
      main ?style_renderer level ?formatter registration_path capacity name allow_push healthcheck_period prune_threshold docker_max_df_size obuilder_prune_threshold obuilder_prune_limit state_dir obuilder additional_metrics))


open Cmdliner

let worker_name =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Unique builder name."
    ~docv:"ID"
    ["name"]

let connect_addr =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of register.cap from ocluster-scheduler."
    ~docv:"ADDR"
    ["c"; "connect"]

let capacity =
  Arg.value @@
  Arg.opt Arg.int 10 @@
  Arg.info
    ~doc:"The number of builds that can run in parallel."
    ~docv:"N"
    ["capacity"]

let healthcheck_period =
  Arg.value @@
  Arg.opt Arg.float 600.0 @@
  Arg.info
    ~doc:"Number of second between obuilder health checks. Zero disables health checks."
    ~docv:"SECONDS"
    ["obuilder-healthcheck"]

let prune_threshold =
  Arg.value @@
  Arg.opt Arg.(some float) None @@
  Arg.info
    ~doc:"Run 'docker system prune' when /var/lib/docker's free space falls below this (0-100). \
          If you don't have a partition for /var/lib/docker, then you can use $(b,--docker-max-df-size)."
    ~docv:"PERCENTAGE"
    ["prune-threshold"]

let docker_max_df_size =
  Arg.value @@
  Arg.opt Arg.(some float) None @@
  Arg.info
    ~doc:"Run `docker system df` to get the amount of memory being taken up by the images and if this is \
          greater than this we run `docker system prune`."
    ~docv:"GIGABYTES"
    ["docker-max-df-size"]

let obuilder_prune_threshold =
  Arg.value @@
  Arg.opt Arg.float 30.0 @@
  Arg.info
    ~doc:"If using OBuilder, this threshold is used to prune the stored builds if the free space falls below this (0-100)."
    ~docv:"PERCENTAGE"
    ~docs:"OBUILDER"
    ["obuilder-prune-threshold"]

let obuilder_prune_limit =
  Arg.value @@
  Arg.opt Arg.int 100 @@
  Arg.info
    ~doc:"Number of items to prune from obuilder when the threshold is reached."
    ~docv:"ITEMS"
    ~docs:"OBUILDER"
    ["obuilder-prune-limit"]

let allow_push =
  Arg.value @@
  Arg.opt Arg.(list string) [] @@
  Arg.info
    ~doc:"Docker repositories to which users can push."
    ~docv:"REPO"
    ["allow-push"]

let state_dir =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Directory for caches, etc (e.g. /var/lib/ocluster-worker)."
    ~docv:"PATH"
    ["state-dir"]

let additional_metric_conv =
  let of_string s = match Astring.String.cut ~sep:":" s with
    | Some (name, uri) -> Ok (name, Uri.of_string uri)
    | None -> Error (`Msg ("Malformed additional metric " ^ s))
  in
  let pp ppf (name, uri) = Fmt.pf ppf "%s:%a" name Uri.pp uri in
  Arg.conv (of_string, pp)

let additional_metrics =
  Arg.value @@
  Arg.opt Arg.(list additional_metric_conv) [] @@
  Arg.info
    ~doc:"Additional prometheus endpoints to scrape in the form <name>:<uri> \
    presented as a comma separated list."
    ~docv:"METRICS"
    ["additional-metrics"]

module Obuilder_config = struct
  (** Parse cli arguments for Obuilder.Store_spec.t *)
  let v =
    let open Obuilder.Store_spec in
    Term.(const of_t
          $ Arg.value @@ store ~docs:"OBUILDER" ["obuilder-store"]
          $ Arg.value @@ rsync_mode_opt)

  (** Parse cli arguments for t and initialise a [store]. *)
  let cmdliner =
    Term.(const Obuilder.Store_spec.to_store $ v)

  let v =
    let make native_conf docker_conf = function
      | `Native, store -> Some (Cluster_worker.Obuilder_config.v (`Native native_conf) store)
      | `Docker, store -> Some (Cluster_worker.Obuilder_config.v (`Docker docker_conf) store)
    in
    Term.(const make $ Obuilder.Native_sandbox.cmdliner $ Obuilder.Docker_sandbox.cmdliner $ cmdliner)
end

let worker_opts_t =
  let worker_opts registration_path capacity name allow_push healthcheck_period prune_threshold docker_max_df_size obuilder_prune_threshold obuilder_prune_limit state_dir obuilder additional_metrics =
    (registration_path, capacity, name, allow_push, healthcheck_period, prune_threshold, docker_max_df_size, obuilder_prune_threshold, obuilder_prune_limit, state_dir, obuilder, additional_metrics) in
  Term.(with_used_args
    (const worker_opts $ connect_addr $ capacity $ worker_name $ allow_push $ healthcheck_period
     $ prune_threshold $ docker_max_df_size $ obuilder_prune_threshold $ obuilder_prune_limit $ state_dir $ Obuilder_config.v $ additional_metrics))

let cmd ~install =
  let doc = "Run a build worker" in
  let man = [
    `P "On $(b,Windows), specify '$(b,install)' as the first \
        command-line parameter to install the worker as a Windows \
        service with the specified parameters, and '$(b,remove) \
        $(i,name)' to remove the worker $(i,name) from the services." ] in
  let info = Cmd.info "ocluster-worker" ~doc ~man ~version:Version.t in
  let docs = Manpage.s_common_options in
  Cmd.v info
    Term.(term_result'
      (const (main ~install)
       $ with_used_args (Fmt_cli.style_renderer ~docs ())
       $ with_used_args (Logs_cli.level ~docs ())
       $ worker_opts_t))

let () =
  let remove name args =
    if args <> [] then begin
      prerr_endline "'remove' should be used only once, in first position.";
      exit 1
    end else
      Winsvc_wrapper.remove name
  in
  match Array.to_list Sys.argv with
  | hd :: "install" :: argv ->
    exit (Cmd.eval ~argv:(Array.of_list (hd :: argv)) (cmd ~install:true))
  | _ :: "remove" :: name :: args -> remove name args
  | _ :: name :: args when Astring.String.is_prefix ~affix:"remove=" name -> remove name args
  | _ ->
    exit (Cmd.eval (cmd ~install:false))

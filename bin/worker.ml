open Lwt.Infix

let setup_log ?(formatter=Format.err_formatter) default_level =
  Prometheus_unix.Logging.init ~formatter ?default_level ();
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
  Lwt.return (fun () -> Lwt.return ())

let main default_level ?formatter registration_path capacity name allow_push prune_threshold state_dir obuilder =
  setup_log ?formatter default_level;
  let update =
    if Sys.file_exists "/.dockerenv" then update_docker
    else update_normal
  in
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
    Cluster_worker.run ~capacity ~name ~allow_push ?prune_threshold ?obuilder ~state_dir ~update sr
  end

(* Command-line parsing *)

let main ~install (default_level, args1) ((registration_path, capacity, name, allow_push, prune_threshold, state_dir, obuilder), args2) =
  let (name', display, text) = ("ocluster-worker", "OCluster Worker", "Run a build worker") in
  if install then
    `Ok (Winsvc_wrapper.install name' display text (args1 @ args2))
  else
    `Ok (Winsvc_wrapper.run name' state_dir (fun ?formatter () ->
             main default_level ?formatter registration_path capacity name allow_push prune_threshold state_dir obuilder))

open Cmdliner

let worker_name =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Unique builder name"
    ~docv:"ID"
    ["name"]

let connect_addr =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of register.cap from build-scheduler"
    ~docv:"ADDR"
    ["c"; "connect"]

let capacity =
  Arg.value @@
  Arg.opt Arg.int 10 @@
  Arg.info
    ~doc:"The number of builds that can run in parallel"
    ~docv:"N"
    ["capacity"]

let prune_threshold =
  Arg.value @@
  Arg.opt Arg.(some float) None @@
  Arg.info
    ~doc:"Run 'docker system prune' when /var/lib/docker's free space falls below this (0-100)"
    ~docv:"PERCENTAGE"
    ["prune-threshold"]

let allow_push =
  Arg.value @@
  Arg.opt Arg.(list string) [] @@
  Arg.info
    ~doc:"Docker repositories to which users can push"
    ~docv:"REPO"
    ["allow-push"]

let state_dir =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Directory for caches, etc (e.g. /var/lib/ocluster-worker)"
    ~docv:"PATH"
    ["state-dir"]

module Obuilder_config = struct
  let store_t = Arg.conv Obuilder.Store_spec.(of_string, pp)

  let store =
    Arg.value @@
    Arg.opt Arg.(some store_t) None @@
    Arg.info
      ~doc:"zfs:pool or btrfs:/path for the OBuilder cache"
      ~docv:"STORE"
      ["obuilder-store"]

  let v =
    let make sandbox_config = function
      | None -> None
      | Some store -> Some (Cluster_worker.Obuilder_config.v sandbox_config store)
    in
    let open Cmdliner.Term in
    Term.pure make $ Obuilder.Runc_sandbox.cmdliner $ store
end

let worker_opts_t =
  let worker_opts registration_path capacity name allow_push prune_threshold state_dir obuilder =
    (registration_path, capacity, name, allow_push, prune_threshold, state_dir, obuilder) in
  Term.(with_used_args
    (const worker_opts $ connect_addr $ capacity $ worker_name $ allow_push
     $ prune_threshold $ state_dir $ Obuilder_config.v))

let cmd ~install =
  let doc = "Run a build worker" in
  let man = [
    `P "On $(b,Windows), specify '$(b,install)' as the first \
        command-line paramater to install the worker as a Windows \
        service with the specified parameters, and '$(b,remove)' to \
        remove the worker from the services." ] in
  Term.(ret (const (main ~install) $ with_used_args (Logs_cli.level ()) $ worker_opts_t)),
  Term.info "ocluster-worker" ~doc ~man ~version:Version.t

let () =
  match Array.to_list Sys.argv with
  | hd :: "install" :: argv ->
    Term.(exit @@ eval ~argv:(Array.of_list (hd :: argv)) (cmd ~install:true))
  | _ :: "remove" :: args ->
    if args <> [] then begin
      prerr_endline "'remove' should be used only once, in first position.";
      exit 1
    end else
      Winsvc_wrapper.remove "ocluster-worker"
  | _ ->
    Term.(exit @@ eval (cmd ~install:false))

open Lwt.Infix

let () =
  Prometheus_unix.Logging.init ()

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

let main registration_path capacity name allow_push prune_threshold state_dir obuilder nix_cache =
  let update =
    if Sys.file_exists "/.dockerenv" then update_docker
    else update_normal
  in
  let nix = Cluster_worker.Nix_config.v ~cache:nix_cache ~state_dir in
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
    Cluster_worker.run ~capacity ~name ~allow_push ?prune_threshold ?obuilder ~nix ~state_dir ~update sr
  end

(* Command-line parsing *)

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
  Arg.pos 0 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of register.cap from build-scheduler"
    ~docv:"ADDR"
    []

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

  let fast_sync =
    Arg.value @@
    Arg.flag @@
    Arg.info
      ~doc:"Ignore sync syscalls for OBuilder builds (requires runc >= 1.0.0-rc92)"
      ["fast-sync"]

  let v =
    let make fast_sync = function
      | None -> None
      | Some store -> Some (Cluster_worker.Obuilder_config.v ~fast_sync store)
    in
    let open Cmdliner.Term in
    Term.pure make $ fast_sync $ store
end

let nix_cache =
  Arg.value @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"cache URI to upload built nix results (passed to `nix copy`)"
    ~docv:"CACHE"
    ["nix-cache"]

let cmd =
  let doc = "Run a build worker" in
  Term.(const main $ connect_addr $ capacity $ worker_name $ allow_push $ prune_threshold $ state_dir $ Obuilder_config.v $ nix_cache),
  Term.info "ocluster-worker" ~doc ~version:Version.t

let () = Term.(exit @@ eval cmd)

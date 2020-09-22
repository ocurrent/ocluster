let () =
  Logging.init ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let main registration_path capacity name allow_push prune_threshold obuilder =
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
    Cluster_worker.run ~capacity ~name ~allow_push ?prune_threshold ?obuilder sr
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

module Obuilder_config = struct
  let store_t =
    let parse s =
      match Astring.String.cut s ~sep:":" with
      | Some ("zfs", pool) -> Ok (`Zfs pool)
      | Some ("btrfs", path) -> Ok (`Btrfs path)
      | _ -> Error (`Msg "Store must start with zfs: or btrfs:")
    in
    let pp f = function
      | `Zfs pool -> Fmt.pf f "zfs:%s" pool
      | `Btrfs path -> Fmt.pf f "btrfs:%s" path
    in
    Arg.conv (parse, pp)

  let v =
    Arg.value @@
    Arg.opt Arg.(some store_t) None @@
    Arg.info
      ~doc:"zfs:pool or btrfs:/path for the OBuilder cache"
      ~docv:"STORE"
      ["obuilder-store"]
end

let cmd =
  let doc = "Run a build worker" in
  Term.(const main $ connect_addr $ capacity $ worker_name $ allow_push $ prune_threshold $ Obuilder_config.v),
  Term.info "ocluster-worker" ~doc ~version:Version.t

let () = Term.(exit @@ eval cmd)

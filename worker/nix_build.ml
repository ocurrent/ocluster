module Spec = Cluster_api.Nix_build.Spec
module Build_log = Obuilder.Build_log

let ( >>!= ) = Lwt_result.bind

module Config = struct
  type t = {
    cache: string option;
    state_dir: string;
  }
  
  let v ~state_dir ~cache = { cache; state_dir }
end

module Runc = Obuilder.Runc_sandbox

module Sandbox = struct
  let ( / ) = Filename.concat
  open Lwt.Infix
  open Config
  type t = { runc: Runc.t; config: Config.t }

  let config t = t.config
  
  let base config = config.state_dir / "nix"

  (* place where Runc_sandbox puts config *)
  let runc_state config = base config / "runc"

  (* persistent /nix/store *)
  let nix_mount config = base config / "nix"

  (* persistent /root (only ~/.cache is actually mounted) *)
  let home_dir config = base config / "home"

  let create config = Runc.create ~runc_state_dir:(runc_state config) () >|= fun runc ->
    { runc; config }

  let init_dirs ~log ~switch config =
    (* make all the dirs *)
    [
      base config;
      runc_state config;
      home_dir config;
      home_dir config / ".cache";
    ] |> List.iter Obuilder.Os.ensure_dir;

      
    let nix_dir = nix_mount config in
    let nix_version = "2.3.10" in
    (* TODO make some kind of stamp so that we rebuild nix store when this code changes.
       Or, just make it a proper obuilder spec
    *)
    (match Obuilder.Os.check_dir nix_dir with
    | `Present -> Lwt_result.return ()
    | `Missing ->
      Log.info (fun f -> f "Setting up new nix store in %s\n" nix_dir);

      let (sys, _sha256) = match "Linux.x86_64" with
      | "Linux.x86_64" -> ("x86_64-linux", "2ea0cd17d53b2e860ec8e17b6de578aff1b11ebaf57117714a250bfd02768834")
      | _ -> failwith "TODO other platforms, get from https://releases.nixos.org/nix/nix-2.3.10/install"
      in

      let url = Format.sprintf
        "https://releases.nixos.org/nix/nix-%s/nix-%s-%s.tar.xz"
        nix_version nix_version sys
      in
      
      Lwt_io.with_temp_dir ~parent:(base config) (fun tmp ->
        let tarball = tmp / "nix-install" in
        (* TODO use native LWT code, just hacking together something here.
         * Potentially this could be an actual obuilder spec...
         *)
        Process.check_call ~switch ~label:"nix-download" ~log ["bash"; "-euxc";
          [
            Format.sprintf "cd '%s'" tmp;
            Format.sprintf "curl -L '%s' -o '%s'" url tarball;
            (* TODO verify hash *)
            "mkdir nix";
            Format.sprintf "tar -xJf '%s' -C nix --strip-components=1" tarball;
            Format.sprintf "rm '%s'" tarball;
            "cd nix";
            (* There should only be one derivation path of e.g. XXXXXXX-nix-2.3.0/bin,
               symlink it to the well-known /nix/store/bin so we always know the original nix
               derivation *)
            "mkdir init";
            "mv .reginfo init";
            "cd init";
            Format.sprintf "ln -s ../store/*-nix-%s/bin nix" nix_version;
            Format.sprintf "ln -s ../store/*-bash-*/bin bash";
            Format.sprintf "ln -s ../store/*-coreutils-*/bin coreutils";
          ] |> String.concat "\n"
        ] >>!= (fun _ ->
          Process.check_call ~switch ~label:"nix-download" ~log
            [ "mv"; tmp / "nix"; nix_dir];
        )
      )
    )
    
  let run { runc; config } ~switch ~log argv =
    (* Unlike obuilder, we lazily initialize the rootfs. TODO don't try to setup multiple times concurrently *)
    init_dirs ~log ~switch config >>!= fun () ->
    let cancelled, cancel_handle = Lwt.wait () in
    let cancel () =
      Lwt.wakeup_later cancel_handle ();
      Lwt.return_unit
    in
    Lwt_switch.add_hook_or_exec (Some switch) cancel >>= fun () ->
    let init_and_run = ["/nix/init/bash/bash"; "-euxc";
      [
        "mkdir -p /etc/nix /tmp /usr/bin";
        "ln -sfn /nix/init/coreutils/env /usr/bin/env";
         (* We do as much setup as possible in here instad of the rootfs, since
            blowing away the rootfs is expensive *)
         (* TODO the non-nix parts can be plain ocaml code, doesn't need to be funnelled into bash *)
        "cat > /etc/nix/nix.conf <<EOF";
          "build-users-group ="; (* disable multiuser *)
          "gc-keep-outputs = true";
          "sandbox = false"; (* we're already sandboxing! *)
          "gc-keep-derivations = true";
          "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
          "substituters = https://cache.nixos.org";
        "EOF";
        (* init DB: (requires /nix to be mounted) *)
        "nix-store --load-db < /nix/init/.reginfo"; (* TODO is this idempotent *)

        (* Make sure git, tar, etc are on the path: *)
        "nix-build --out-link /nix/init/util --expr '";
          "with import <nixpkgs> {}; symlinkJoin { name = \"util\"; paths = [ gitMinimal gnutar ]; }";
        "'";

        (* Now run the actual user program *)
        "exec \"$@\"";
      ] |> String.concat "\n";
      "--";
    ] @ argv in
  
    (* TODO get a git clone of nixos/nixpkgs instead of using this *)
    let nixpkgs_path = Unix.readlink @@
      "/home/" ^ Unix.getenv "SUDO_USER" ^ "/.nix-defexpr/channels/nixpkgs" in

    (* TODO hardcode this to.... something? *)
    let curl_ca = Unix.readlink (Unix.getenv "CURL_CA_BUNDLE") in
    let runc_config = Obuilder.Config.v
      ~cwd:"/" ~argv:init_and_run ~user:Obuilder_spec.{uid = 0; gid = 0}
      ~network:["host"] ~hostname:"nix-root"
      ~env:[
        "NIX_PATH", "/nix-channels";
        "USER", "root";
        "HOME", "/root";
        "PATH", ["nix"; "coreutils"; "util/bin"] |> List.map(fun p -> "/nix/init" / p) |> String.concat ":";
        "CURL_CA_BUNDLE", curl_ca;
        "NIX_SSL_CERT_FILE", curl_ca;
      ]
      ~mounts:Obuilder.Config.Mount.([
        (* TODO make root readonly? *)
        { src = nix_mount config; dst = "/nix" };
        { src = (home_dir config) / ".cache"; dst = "/root/.cache" };
        { src = nixpkgs_path; dst = "/nix-channels/nixpkgs" };
        { src = "/etc/services"; dst = "/etc/services" };
        { src = curl_ca; dst = curl_ca };
      ])
    in
    let log_file = Filename.concat (base config) "log" in

    (* Runc wants an obuilder_log type, but we have a Log_data type. TODO This seems awkward :/ *)
    let log_to_client msg = Log_data.write log msg in
    let () = if Sys.file_exists log_file then Unix.unlink log_file in
    Build_log.create log_file >>= fun obuilder_log ->
    let log_consumed = Build_log.tail ~switch obuilder_log (log_to_client) in

    Lwt_io.with_temp_dir ~parent:(base config) (fun tmp ->
      (* TODO why's it _my_ job to make this rootfs directory whose name is an
      implementation detail of the runc config? *)
      Obuilder.Os.ensure_dir (tmp / "rootfs");
      Runc.run ~cancelled ~log:obuilder_log runc runc_config tmp >>= fun result ->
      log_consumed >>= fun _ ->
        Lwt.return (result |> Result.map (fun () -> "TODO: (trimmed) process stdout"))
    )
end

open Spec

let create = Sandbox.create

let upload ~config ~switch ~log path =
  (* this is safe to run on the host, since it's just uploading built derivations *)
  match config.Config.cache with
  | None -> Lwt_result.return path
  | Some cache ->
      Process.check_call ~label:"nix-copy" ~switch ~log ["nix"; "copy"; "--to"; cache; path]
      |> Lwt_result.map (fun () -> path)


(* TODO: enable restrict-eval and other hardening settings -- https://nixos.org/manual/nix/stable/#name-11 *)

let rec build t ~switch ~log =
  let upload = upload ~config:(Sandbox.config t) ~switch ~log in
  function
  | Build (`Drv drv) ->
    (* TODO remove --check, it's just there to skip short-circuiting builds *)
    Sandbox.run t ~switch ~log ["nix-store"; "--realise"; "--check"; drv] >>!= upload
  | Eval (`Expr expr) ->
    Sandbox.run t ~switch ~log ["nix-instantiate"; "--expr"; expr] >>!= upload
  | Run { drv; exe; args } -> (
    build t ~switch ~log (Build drv) >>!= fun impl ->
    let cmd = [Filename.concat impl exe] @ args in
    Sandbox.run t ~switch ~log cmd
  )

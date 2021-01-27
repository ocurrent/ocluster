module Spec = Cluster_api.Nix_build.Spec
module Build_log = Obuilder.Build_log
module Os = Obuilder.Os

let ( >>!= ) = Lwt_result.bind

module Config = struct
  type t = {
    cache: string option;
    state_dir: string;
  }
  
  let v ~state_dir ~cache = { cache; state_dir }
end

module Runc = Obuilder.Runc_sandbox

module LogBuffer = struct
  type t = {
    buffer_rev: string list ref;
    handler: Runc.log_handler;
  }

  (* we log both stdout/stderr, but only accumulate stdout *)
  let make ~log =
    let buffer_rev = ref [] in
    let handler = `Tagged (fun src contents ->
      try
        let () = match src with
          | `Stdout -> buffer_rev := contents :: !buffer_rev
          | `Stderr -> ()
        in
        (* TODO only emit complete lines at a time, to prevent interleaving *)
        (* Or even better: maintain the tagging in log_data so they can be rendered differently *)
        Log_data.write log contents;
        Lwt.return_unit
      with e -> Lwt.fail e
    ) in
    { buffer_rev; handler }
    
  let stdout t = !(t.buffer_rev) |> List.rev |> String.concat "" |> String.trim
end

module Sandbox = struct
  let ( / ) = Filename.concat
  open Lwt.Infix
  open Config
  type t = { runc: Runc.t; config: Config.t }
  
  type err = [ `Cancelled | `Msg of string]

  let config t = t.config
  
  module D = struct
    let base config = config.state_dir / "nix"

    (* place where Runc_sandbox puts config *)
    let runc_state config = base config / "runc"

    (* persistent data root, /nix/store, /root/.cache *)
    let data config = base config / "data"

    (* only etc/nix/nix.conf is mounted *)
    let etc config = data config / "etc"

    (* where we keep status files *)
    let status config = data config / "status"

    let nix config = data config / "nix"

    (* persistent /root (only ~/.cache is actually mounted) *)
    let home config = data config / "home"
  end
  
  let rec mkdir_p path =
    try Os.ensure_dir path
    with Unix.Unix_error _ as e -> (
      let parent = Filename.dirname path in
      if parent <> path then (
        mkdir_p parent;
        Os.ensure_dir path
      ) else raise e
    )

  let create config =
    mkdir_p @@ D.runc_state config;
    Runc.create ~runc_state_dir:(D.runc_state config) () >|= fun runc ->
    { runc; config }
    
  (* TODO prevent running init steps concurrently *)
  let init_step ~switch ~log ~config ~id ?path ?stamp fn =
    let stat_file = (D.status config) / id in
    let clear_and_setup () =
      let log_path = match path with Some p -> p | None -> "(unknown path)" in
      Log.info (fun f -> f "Setting up %s (in %s)" id log_path);
      let () = try Unix.unlink stat_file
        with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      in
      (match path with
        | Some path -> (
          (* ensure we only ever write inside the data dir *)
          let expected_prefix = D.data config in
          let prefix = String.sub path 0 (String.length expected_prefix) in
          if prefix = expected_prefix then (
            Process.check_call ~switch ~label:id ~log ["rm"; "-rf"; path] >>!= fn >>!= fun () ->
              try
                let _: Unix.stats = Unix.lstat path in
                Lwt_result.return ()
              with Unix.Unix_error _ ->
                Lwt_result.fail @@ `Msg (Fmt.str "Initialization step %s didn't create path %s" id path)
          ) else Lwt_result.fail @@ `Msg (Fmt.str "Attempted to overwrite non-data directory %s" path)
        )
        | None -> fn ()
      )
    in
    (match stamp with
      | None -> clear_and_setup ()
      | Some stamp -> (
        Lwt_result.catch (Lwt_io.(with_file ~mode:input) stat_file Lwt_io.read) >>= function
        | Ok current when String.trim current = stamp ->
          Lwt_result.return @@ Log.debug (fun f -> f "Initialization %s has already been run (%s)" id stamp)
        | _ ->
          clear_and_setup () >>!= fun () ->
          Os.write_file ~path:stat_file stamp |> Lwt.map Result.ok
      )
    )

  let nix_version = "2.3.10"
  let nix_stamp = nix_version ^ "-1" (* NOTE: increment suffix when altering nix setup code*)

  let init_dirs ~log ~switch config =
    (* ensure all parent / empty dirs *)
    [
      D.data config;
      D.status config;
      D.home config / ".cache";
    ] |> List.iter mkdir_p;

    let init_step = init_step ~switch ~log ~config in
    let exec ?label ?env cmd = Process.check_call ~switch ?env
      ~label:(match label with Some l -> l | None -> "setup-nix") ~log cmd in

    init_step ~id:"nix-unpack" ~path:(D.nix config) ~stamp:nix_stamp (fun () ->
      (* TODO get current arch rather than hardcoding *)
      let (sys, _sha256) = match "Linux.x86_64" with
      | "Linux.x86_64" -> ("x86_64-linux", "2ea0cd17d53b2e860ec8e17b6de578aff1b11ebaf57117714a250bfd02768834")
      | _ -> failwith "TODO other platforms, get from https://releases.nixos.org/nix/nix-2.3.10/install"
      in

      let url = Format.sprintf
        "https://releases.nixos.org/nix/nix-%s/nix-%s-%s.tar.xz"
        nix_version nix_version sys
      in
      
      Lwt_io.with_temp_dir ~parent:(D.data config) (fun tmp ->
        let tarball = tmp / "nix-install" in
        let unpack = tmp / "unpack" in
        let dest = D.nix config in
        
        mkdir_p (dest / "init");
        mkdir_p (tmp / "unpack");

        let downloaded = exec [ "curl"; "-L"; url; "-o"; tarball ] in
        let unpacked = downloaded >>!= fun () ->
          (* TODO verify hash *)
          exec [ "tar"; "-xJf"; tarball; "-C"; tmp / "unpack"; "--strip-components=1" ]
        in

        let store_dir = unpacked >>!= fun () ->
          let rec make_links = function
            | [] -> Lwt_result.return ()
            | (impl, name) :: links -> (
              exec [ "ln"; "-sfn";
                "../store" / impl / "bin";
                dest / "init" / name
              ] >>!= fun () -> make_links links
            )
          in
          (* `try` here is a bit lazy, just catch any sys / unix / not_found errors in the whole block *)
          let link_pairs = try
            Unix.rename (unpack / "store") (dest / "store");
            Unix.rename (unpack / ".reginfo") (dest / "init/.reginfo");
            let store_impls = Sys.readdir (dest / "store") |> Array.to_list in
            let find name =
              let impl = List.find (fun entry ->
                let parts = String.split_on_char '-' entry in
                List.nth_opt parts 1 = Some(name)
              ) store_impls
              in (impl, name)
            in
            Ok [
              find "nix";
              find "bash";
              find "coreutils";
              find "gnutar";
              find "gzip";
            ]
          with
            | Sys_error msg -> Error (`Msg msg)
            | Unix.Unix_error (_, syscall, _) -> Error (`Msg syscall)
            | Not_found -> Error (`Msg "Couldn't find expected nix store contents")
          in
          Lwt_result.lift link_pairs >>!= make_links
        in
        store_dir
      )
    ) >>!= fun () ->

    init_step ~id:"nix-conf" (fun () ->
      let etc = D.etc config in
      mkdir_p (etc / "nix");
      Os.write_file ~path:(etc / "nix" / "nix.conf") ([
        "build-users-group ="; (* disable multiuser *)
        "gc-keep-outputs = true";
        "sandbox = false"; (* we're already sandboxing! *)
        "gc-keep-derivations = true";
        "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
        "substituters = https://cache.nixos.org";
      ] |> String.concat "\n") |> Lwt_result.ok
    )
    

  let runc_raw { runc; config } ~switch ~cancelled ~log argv =
    (* like `run` but without any initialization, because we use runc for some init steps *)

    (* TODO hardcode this to.... something? *)
    let curl_ca = Unix.readlink (Unix.getenv "CURL_CA_BUNDLE") in
    let runc_config = Obuilder.Config.v
      ~cwd:"/" ~argv ~user:Obuilder_spec.{uid = 0; gid = 0}
      ~network:["host"] ~hostname:"nix-root"
      ~env:[
        "USER", "root";
        "HOME", "/root";
        "PATH", ["nix"; "bash"; "gnutar"; "coreutils"; "gzip"; "util/bin"] |> List.map(fun p -> "/nix/init" / p) |> String.concat ":";
        "CURL_CA_BUNDLE", curl_ca;
        "NIX_SSL_CERT_FILE", curl_ca;
      ]
      ~mounts:Obuilder.Config.Mount.([
        { src = D.nix config; dst = "/nix" };
        { src = (D.etc config) / "nix/nix.conf"; dst = "/etc/nix/nix.conf" };
        { src = (D.home config) / ".cache"; dst = "/root/.cache" };
        { src = curl_ca; dst = curl_ca };

        (* seems innocuous, and fetching complains without it *)
        { src = "/etc/services"; dst = "/etc/services" };
      ])
    in

    Lwt_io.with_temp_dir ~parent:(D.data config) (fun tmp ->
      let rootfs = tmp / "rootfs" in
      mkdir_p (rootfs / "tmp");
      mkdir_p (rootfs / "usr" / "bin");
      Process.check_call ~switch ~label:"link env" ~log
        ["ln"; "-sfn"; "/nix/init/coreutils/env"; rootfs / "usr/bin/env"] >>!= fun () ->

      let log_buffer = LogBuffer.make ~log in
      Runc.run ~cancelled ~log:log_buffer.handler runc runc_config tmp >>= fun runc_result ->
      (* Build_log.finish obuilder_log >>= fun () -> *)
      Lwt.return runc_result >>!= fun () ->
      Lwt_result.return @@ LogBuffer.stdout log_buffer
    )

  let run t ~switch ~log argv =
    let init_step = init_step ~switch ~log ~config:t.config in
    
    (* init dirs does all the static initialization *)
    init_dirs ~log ~switch t.config >>!= fun () ->
      
    (* TODO feels awkward to have both cancelled and switch *)
    let cancelled, cancel_handle = Lwt.wait () in
    let cancel () =
      Lwt.wakeup_later cancel_handle ();
      Lwt.return_unit
    in
    Lwt_switch.add_hook_or_exec (Some switch) cancel >>= fun () ->

    (* Once dirs are initialized, we also need to do some init stages _inside_ the
       container, because they rely on e.g. /nix actually being present at that path *)

    init_step ~id:"nix-db" ~stamp:nix_version (fun () ->
      runc_raw t ~switch ~log ~cancelled [
        "bash"; "-euxc"; "nix-store --load-db < /nix/init/.reginfo";
      ] |> Lwt_result.map ignore
    ) >>!= fun () ->
    (* For stuff like builtins.fetchGit to work, we need `git` already on the path.
       We don't need a terribly fresh nixpkgs for this, the commit below is the HEAD
       of the nixos-20.08 channel as of 2021-01-26
    *)
    let utils_expr = [
      "let";
      "  nixpkgs = builtins.fetchTarball {";
      "    url = https://github.com/NixOS/nixpkgs/archive/a058d005b3cbb370bf171ebce01839dd6ff52222.tar.gz;";
      "    sha256 = \"154mpqw0ya31hzgz9hggg1rb26yx8d00rsj9l90ndsdldrssgvbb\";";
      "  };";
      "in with (import nixpkgs {});";
      "symlinkJoin { name = \"util\"; paths = [ gitMinimal gnutar ]; }"
    ] |> String.concat "\n" in

    init_step ~id:"util-bin" ~stamp:utils_expr ~path:(D.nix t.config / "init" / "util") (fun () ->
      runc_raw t ~switch ~log ~cancelled [
        "nix-build"; "--out-link"; "/nix/init/util"; "--expr"; utils_expr
      ] |> Lwt_result.map ignore
    ) >>!= fun () ->
    runc_raw t ~switch ~log ~cancelled argv
end

open Spec

let create = Sandbox.create

let upload t ~switch ~log path =
  (* TODO need to supply additional config so the container has access to credentials
   * (but only when uploading)
   *)
  let result = Lwt_result.return path in
  match (Sandbox.config t).Config.cache with
  | None -> result
  | Some cache ->
    Sandbox.run t ~switch ~log ["nix"; "copy"; "--to"; cache; path] >>!= fun (_output:string) ->
    result

(* TODO: enable restrict-eval and other hardening settings -- https://nixos.org/manual/nix/stable/#name-11 *)

let rec build t ~switch ~log =
  let upload = upload t ~switch ~log in
  function
  | Build (`Drv drv) ->
    (* TODO remove --check, it's just there to skip short-circuiting builds *)
    Sandbox.run t ~switch ~log
      ["nix-store"; "--realise"; "--check"; drv] >>!= upload

  | Eval (`Expr expr) ->
    Sandbox.run t ~switch ~log
      ["nix-instantiate"; "--expr"; expr] >>!= upload

  | Run { drv; exe; args } -> (
    build t ~switch ~log (Build drv) >>!= fun impl ->
    let cmd = [Filename.concat impl exe] @ args in
    Sandbox.run t ~switch ~log cmd
  )

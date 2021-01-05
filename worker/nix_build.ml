module Spec = Cluster_api.Nix_build.Spec

let ( >>!= ) = Lwt_result.bind

module Config = struct
  type t = {
    cache: string option;
  }
  
  let v ~cache = { cache }
end

open Spec

(* TODO: running nix-* directly on this machine may be risky.
   We should run commands in a sandbox with only access to /nix/store
   (platform-dependent).

   TODO: enable restrict-eval and other hardening settings -- https://nixos.org/manual/nix/stable/#name-11
   *)

let upload ~config ~switch ~log path =
  match config.Config.cache with
  | None -> Lwt_result.return path
  | Some cache ->
      Process.check_call ~label:"nix-copy" ~switch ~log ["nix"; "copy"; "--to"; cache; path]
      |> Lwt_result.map (fun () -> path)

let rec build ~config ~switch ~log =
  let upload = upload ~config ~switch ~log in
  function
  | Build (`Drv drv) -> (
      Process.check_output ~label:"nix-build" ~switch ~log ["nix-store"; "--realise"; drv]
      |> Lwt_result.map String.trim
      >>!= upload
    )
  | Eval (`Expr expr) -> (
      Process.check_output ~label:"nix-instantiate" ~switch ~log ["nix-instantiate"; "--expr"; expr]
      |> Lwt_result.map String.trim
      >>!= upload
  )
  | Run { drv; exe; args } -> (
    build ~config ~switch ~log (Build drv) >>!= fun impl ->
    let cmd = [Filename.concat impl exe] @ args in
    Process.check_output ~label:"nix-exec" ~switch ~log cmd
    |> Lwt_result.map String.trim
  )

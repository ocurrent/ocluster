open Lwt.Infix

module Spec = Cluster_api.Nix_build.Spec

let ( >>!= ) = Lwt_result.bind

let build ~switch ~log spec =
  (* First we have to add the .drv into our local store.
   * This validates that the output path corresponds
   * to the given .drv contents, and `nix-store --realise`
   * requires a store path anyway.
   *)

  Lwt_io.with_temp_dir ~prefix:"build-worker-" ~suffix:"-nix" @@ fun tmpdir ->
  let open Spec in
  (* TODO need ProcessProcess.check_output *)
  match (spec.filename, spec.drv) with
  | (`Filename filename, `Contents contents) -> (
    (* assert String.ends_with ".drv" drv.filename; *)
    let drv_src_path = Filename.concat tmpdir filename in
    Lwt_io.with_file ~mode:Lwt_io.output drv_src_path (fun drv_file ->
      Lwt_io.write drv_file contents
    ) >>= fun () ->
    Process.check_output ~label:"nix-store --add" ~switch ~log ["nix-store"; "--add"; drv_src_path] >>!= fun drv_store_path ->
    Process.check_output ~label:"nix-store" ~switch ~log ["nix-store"; "--realise"; drv_store_path]
    |> Lwt_result.map (fun built_path -> String.trim built_path)
  )

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
  match (spec.filename, spec.drv) with
  | (`Filename filename, `Contents contents) -> (
    Lwt.return (
      if Astring.String.is_suffix ~affix:".drv" filename then Ok ()
      else Error (`Msg ("Not a .drv file: "^filename))
    ) >>!= fun () ->
    let drv_src_path = Filename.concat tmpdir filename in
    Lwt_io.with_file ~mode:Lwt_io.output drv_src_path (fun drv_file ->
      Lwt_io.write drv_file contents
        |> Lwt.map (fun () -> Ok ())
    ) >>!= fun () ->
    Process.check_output ~label:"nix-store --add" ~switch ~log ["nix-store"; "--add"; drv_src_path] >>!= fun drv_store_path ->
    Process.check_output ~label:"nix-store" ~switch ~log ["nix-store"; "--no-out-link"; "--realise"; drv_store_path]
    |> Lwt_result.map (fun built_path -> String.trim built_path)
  )

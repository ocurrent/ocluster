let (let++) a b = Lwt_result.map b a

type prep = string

let file id = Fpath.(v "/tmp" / "ocluster-artifacts" // v (id ^ ".tar.gz"))

type t = {
  expiry_date: float;
  id: string
}


let wormhole_save_to ~code file =
  Lwt_process.exec ("wormhole", [|"wormhole"; "receive"; code; "--accept-file"; "-o"; Fpath.to_string file|])
  |> Lwt.map (function 
    | Unix.WEXITED 0 -> Ok ()
    | Unix.WEXITED n -> Error (`Msg (Fmt.str "Process exited (%d)" n))
    | Unix.WSIGNALED n -> Error (`Msg (Fmt.str "Process signaled (%d)" n))
    | Unix.WSTOPPED n -> Error (`Msg (Fmt.str "Process stopped (%d)" n)))

let create ?(expire_after=(Duration.of_day 7)) ~code id =
  let file = file id in
  let parent = Fpath.parent file in
  let _ = Bos.OS.Dir.create parent in
  let expiry_date = 
    Unix.gettimeofday () +. (Duration.to_f expire_after)
  in
  let++ () = wormhole_save_to ~code file in 
  {id; expiry_date}

let extract ~folder build = 
  let open Obuilder_spec in
  stage ~child_builds:["artifacts_build", build] ~from:"debian" [
    user ~uid:0 ~gid:0;
    workdir "/src";
    run "apt update && apt install -y magic-wormhole";
    copy ~from:(`Build "artifacts_build") [folder] ~dst:"artifacts";
    run "tar -czvf artifacts.tar.gz artifacts/";
    run "wormhole send --hide-progress artifacts.tar.gz"
  ]

let id {id; _} = id

let size {id; _} = 
  let file = file id in
  Unix.(stat (Fpath.to_string file) ).st_size

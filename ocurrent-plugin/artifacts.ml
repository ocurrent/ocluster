let (let++) a b = Lwt_result.map b a
let (let**) = Lwt_result.bind

type prep = string

let tmp_file id = Fpath.(v "/tmp" / "ocluster-artifacts" // v (id ^ ".tar.gz"))

let store = Current.state_dir "ocluster-artifacts"

let artifacts_path id = Fpath.v id 

let file id = Fpath.(store // artifacts_path id)


type t = {
  expiry_date: float;
  id: string
}


let wormhole_save_to ~code file =
  Lwt_process.exec ("wormhole", [|"wormhole"; "receive"; code; "--accept-file"; "-o"; Fpath.to_string file|])
  |> Lwt.map (function 
    | Unix.WEXITED 0 -> Ok ()
    | Unix.WEXITED n -> Error (`Msg (Fmt.str "Wormhole process exited (%d)" n))
    | Unix.WSIGNALED n -> Error (`Msg (Fmt.str "Wormhole process signaled (%d)" n))
    | Unix.WSTOPPED n -> Error (`Msg (Fmt.str "Wormhole process stopped (%d)" n)))

let extract_content file destination =
  let _ = Bos.OS.Dir.create destination in
  Lwt_process.exec ("tar", [|"tar"; "-xzf"; Fpath.to_string file; "-C"; Fpath.to_string destination |])
  |> Lwt.map (function 
    | Unix.WEXITED 0 -> Ok ()
    | Unix.WEXITED n -> Error (`Msg (Fmt.str "Tar process exited (%d)" n))
    | Unix.WSIGNALED n -> Error (`Msg (Fmt.str "Tar process signaled (%d)" n))
    | Unix.WSTOPPED n -> Error (`Msg (Fmt.str "Tar process stopped (%d)" n)))

let create ?(expire_after=(Duration.of_day 7)) ~code id =
  let tmp_file = tmp_file id in
  let parent = Fpath.parent tmp_file in
  let _ = Bos.OS.Dir.create parent in
  let expiry_date = 
    Unix.gettimeofday () +. (Duration.to_f expire_after)
  in
  let** () = wormhole_save_to ~code tmp_file in 
  let++ () = extract_content tmp_file (file id) in
  let _ = Bos.OS.Path.move tmp_file (Fpath.add_ext ".tar.gz" (file id)) in
  {id; expiry_date}

let extract ~folder build = 
  let open Obuilder_spec in
  stage ~child_builds:["artifacts_build", build] ~from:"debian" [
    user ~uid:0 ~gid:0;
    workdir "/src";
    run "apt update && apt install -y magic-wormhole";
    copy ~from:(`Build "artifacts_build") [folder] ~dst:"artifacts";
    run "tar -czvf artifacts.tar.gz artifacts/";
    run "wormhole send --hide-progress  artifacts.tar.gz"
  ]

let id {id; _} = id

let public_path {id; _} = Fpath.(v"/artifacts" // artifacts_path id)
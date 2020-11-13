(* opam-health-check submits the whole of opam repository at once. *)

open Lwt.Infix

let platforms = ["fedora-31"; "fedora-32"; "debian-9"; "debian-10"; "opensuse"; "ubuntu"; "ubuntu-lts"]

let n_packages = 3000

(* Submit one job for each platform *)
let submit_batch ~i ~pkg submit_cap =
  platforms
  |> List.map (fun platform ->
      let cache_hint = Printf.sprintf "%d-%d-%s" !Utils.opam_repo_head pkg platform in
      Utils.submit submit_cap ~name:"opam-health-check" ~i ~pool:"linux-x86_64" ~urgent:false ~cache_hint
    )
  |> Lwt.join

let thread submit_cap =
  let i = ref 0 in
  let rec aux () =
    Logs.info (fun f -> f "Health-check time!");
    List.init n_packages (fun pkg -> submit_batch ~i ~pkg submit_cap) |>
    Lwt.join >>= fun () ->
    Utils.sleep (Duration.of_hour 4) >>= fun () ->
    aux ()
  in
  aux ()

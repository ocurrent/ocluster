(* ocaml-ci submits a steady stream of small jobs, plus occasional large batches due to opam-repository updates. *)

open Lwt.Infix

let platforms = ["fedora-31"; "fedora-32"; "debian-9"; "debian-10"; "opensuse"; "ubuntu"; "ubuntu-lts"]

let n_targets = 300

(* Submit one job for each platform *)
let submit_batch ~i ~pkg ~urgent submit_cap =
  platforms
  |> List.map (fun platform ->
      let cache_hint = Printf.sprintf "%d-%d-%s" !Utils.opam_repo_head pkg platform in
      Utils.submit submit_cap ~name:"ocaml-ci" ~i ~pool:"linux-x86_64" ~urgent ~cache_hint
    )
  |> Lwt.join

let thread submit_cap =
  let i = ref 0 in
  let rec aux () =
    Utils.sleep (Duration.of_min 10) >>= fun () ->
    let pkg = Random.int n_targets in
    Lwt.async (fun () -> submit_batch ~i ~pkg ~urgent:true submit_cap);
    aux ()
  in
  let rec opam_repo () =
    Lwt_condition.wait Utils.opam_repository_updated >>= fun () ->
    List.init (Random.int n_targets) (fun pkg -> submit_batch ~i ~pkg ~urgent:false submit_cap)
    |> Lwt.join
    >>= opam_repo
  in
  Lwt.choose [
    aux ();
    opam_repo ();
  ]

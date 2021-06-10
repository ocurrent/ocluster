open Lwt.Infix
open Capnp_rpc_lwt

module Api = Cluster_api

let speed_multiplier = 60.0

let sleep d =
  Lwt_unix.sleep (Duration.to_f d /. speed_multiplier)

let submit cap ~name ~i ~pool ~urgent ~cache_hint =
  let job_id = Printf.sprintf "%s-%d" name !i in
  incr i;
  let descr = Api.Submission.obuilder_build job_id in
  Capability.with_ref (Api.Submission.submit cap ~action:descr ~pool ~urgent ~cache_hint) @@ fun ticket ->
  Capability.with_ref (Api.Ticket.job ticket) @@ fun job ->
  Capability.await_settled_exn job >>= fun () ->
  Logs.info (fun f -> f "%s: job %S running" name job_id);
  Api.Job.result job >|= function
  | Ok _ -> Logs.info (fun f -> f "%s : job %S finished" name job_id);
  | Error (`Capnp ex) -> Logs.info (fun f -> f "%s: job %S failed: %a" name job_id Capnp_rpc.Error.pp ex)

let opam_repo_head = ref 0

let opam_repository_updated : unit Lwt_condition.t = Lwt_condition.create ()

let update_opam_repository () =
  incr opam_repo_head;
  Lwt_condition.broadcast opam_repository_updated ()

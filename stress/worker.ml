open Lwt.Infix
open Capnp_rpc_lwt

module Api = Cluster_api

module Metrics = struct
  open Prometheus

  let namespace = "ocluster"
  let subsystem = "worker"

  let running_jobs =
    let help = "Number of jobs currently running" in
    Gauge.v_label ~label_name:"worker" ~help ~namespace ~subsystem "running_jobs"
end

module Hint_set = Set.Make(String)

type t = {
  name : string;
  capacity : int;
  mutable in_use : int;                (* Number of active builds *)
  cond : unit Lwt_condition.t;         (* Fires when a build finishes (or switch turned off) *)
  mutable cached : Hint_set.t;
}

let thread ~name ~capacity pool =
  let src = Logs.Src.create name ~doc:"fake worker" in
  let (module Log : Logs.LOG) = Logs.src_log src in
  let worker =
    let metrics _ = failwith "fake metrics" in
    let self_update _ = failwith "fake self_update" in
    Api.Worker.local ~metrics ~self_update
  in
  Capability.with_ref (Api.Registration.register pool ~name ~capacity worker) @@ fun queue ->
  let t = { name; capacity; in_use = 0; cond = Lwt_condition.create (); cached = Hint_set.empty } in
  let rec loop () =
    if t.in_use >= t.capacity then (
      Log.info (fun f -> f "At capacity. Waiting for a build to finish before requesting more…");
      Lwt_condition.wait t.cond >>= loop
    ) else (
      let outcome, set_outcome = Lwt.wait () in
      Log.info (fun f -> f "Requesting a new job…");
      let switch = Lwt_switch.create () in
      let pop =
        let stream_log_data ~start:_ = 
          outcome >>= fun _ ->
          Lwt.return ("", 0L)
        in
        Capability.with_ref (Cluster_api.Job.local ~switch ~outcome ~stream_log_data) @@ fun job ->
        Cluster_api.Queue.pop queue job
      in
      pop >>= fun request ->
      let cache_hint = Cluster_api.Raw.Reader.JobDescr.cache_hint_get request in
      let is_cached = Hint_set.mem cache_hint t.cached in
      t.cached <- Hint_set.add cache_hint t.cached;
      t.in_use <- t.in_use + 1;
      Prometheus.Gauge.inc_one (Metrics.running_jobs name);
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
               let d = if is_cached then Duration.of_sec 15 else Duration.of_min 15 in
               Utils.sleep d >>= fun () ->
               Lwt.wakeup set_outcome (Ok "");
               Lwt.return_unit)
            (fun () ->
               t.in_use <- t.in_use - 1;
               Prometheus.Gauge.dec_one (Metrics.running_jobs name);
               Lwt_switch.turn_off switch >>= fun () ->
               Lwt_condition.broadcast t.cond ();
               Lwt.return_unit)
        );
      loop ()
    )
  in
  loop ()

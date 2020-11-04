open Lwt.Infix
open Capnp_rpc_lwt

let ( >>!= ) = Lwt_result.bind

module Metrics = struct
  open Prometheus

  let namespace = "ocluster"
  let subsystem = "ocurrent"

  let queue =
    let help = "Items in cluster queue by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem "queue_state"

  let queue_connect = queue "connect"
  let queue_rate_limit = queue "rate-limit"
  let queue_get_ticket = queue "get-ticket"
  let queue_get_worker = queue "get-worker"
end

(* This is shared by all jobs. *)
type t = {
  sr : [`Submission_f4e8a768b32a7c42] Sturdy_ref.t;
  mutable sched : Cluster_api.Submission.t Lwt.t;
  (* Limit how many items we queue up at the scheduler (including assigned to workers)
     for each (OCluster pool, urgency). *)
  rate_limits : ((string * bool), unit Lwt_pool.t) Hashtbl.t;
  max_pipeline : int;
}

(* Return a proxy to the scheduler, starting a new connection if we don't
   currently have a working one. *)
let sched ~job t =
  let conn = t in
  match Lwt.state conn.sched with
  | Lwt.Return cap when Capability.problem cap = None -> Lwt.return cap
  | Lwt.Sleep ->
    Current.Job.log job "Connecting to build cluster...";
    conn.sched      (* Already connecting; join that effort *)
  | _ ->
    Current.Job.log job "Connecting to build cluster...";
    let rec aux () =
      Lwt.catch
        (fun () ->
           Sturdy_ref.connect_exn conn.sr >>= fun cap ->
           Capability.wait_until_settled cap >|= fun () ->
           cap
        )
        (fun ex ->
           Log.warn (fun f -> f "Error connecting to build cluster (will retry): %a" Fmt.exn ex);
           Lwt_unix.sleep 10.0 >>= fun () ->
           aux ()
        )
    in
    conn.sched <- aux ();
    conn.sched

let rate_limit t pool urgent =
  let key = (pool, urgent) in
  match Hashtbl.find_opt t.rate_limits key with
  | Some limiter -> limiter
  | None ->
    let limiter = Lwt_pool.create t.max_pipeline Lwt.return in
    Hashtbl.add t.rate_limits key limiter;
    limiter

let urgent_if_high = function
  | `High -> true
  | `Low -> false

(* This is called by [Current.Job] once the confirmation threshold allows the job to be submitted. *)
let submit ~job ~pool ~action ~cache_hint ?src ~urgent t ~priority ~switch:_ =
  let urgent = urgent priority in
  let limiter_thread = ref None in
  let stage = ref `Init in
  let cancelled, set_cancelled = Lwt.wait () in
  let cancel () =
    if Lwt.is_sleeping cancelled then Lwt.wakeup set_cancelled (Error `Cancelled);
    match !stage with
    | `Init | `Got_worker -> Lwt.return_unit
    | `Rate_limit ->
      Option.iter Lwt.cancel !limiter_thread;         (* Waiting for [Pool.use] *)
      Lwt.return_unit
    | `Get_ticket ticket ->                          (* Waiting for worker *)
      Cluster_api.Ticket.cancel ticket >|= function
      | Ok () -> ()
      | Error (`Capnp e) -> Current.Job.log job "Cancel ticket failed: %a" Capnp_rpc.Error.pp e
  in
  let rec aux () =
    Prometheus.Gauge.inc_one Metrics.queue_connect;
    let sched = sched ~job t >|= Result.ok in
    Lwt.choose [sched; cancelled] >>= fun sched ->
    Prometheus.Gauge.dec_one Metrics.queue_connect;
    match sched with
    | Error `Cancelled -> Lwt.(fail Canceled)
    | Ok sched ->
      stage := `Rate_limit;
      Prometheus.Gauge.inc_one Metrics.queue_rate_limit;
      let use_thread =
        Lwt.catch
          (fun () ->
             Lwt_pool.use (rate_limit t pool urgent)
               (fun () ->
                  Prometheus.Gauge.dec_one Metrics.queue_rate_limit;
                  let ticket = Cluster_api.Submission.submit ~urgent ?src sched ~pool ~action ~cache_hint in
                  let build_job = Cluster_api.Ticket.job ticket in
                  stage := `Get_ticket ticket;       (* Allow the user to cancel it now. *)
                  Prometheus.Gauge.inc_one Metrics.queue_get_ticket;
                  Capability.wait_until_settled ticket >>= fun () ->
                  Prometheus.Gauge.dec_one Metrics.queue_get_ticket;
                  Current.Job.log job "Waiting for worker...";
                  Prometheus.Gauge.inc_one Metrics.queue_get_worker;
                  Capability.wait_until_settled build_job >>= fun () ->
                  Prometheus.Gauge.dec_one Metrics.queue_get_worker;
                  Capability.dec_ref ticket;
                  stage := `Got_worker;
                  Lwt.return build_job
               )
          )
          (function
            | Lwt.Canceled as ex ->
              if !stage = `Rate_limit then Prometheus.Gauge.dec_one Metrics.queue_rate_limit
              else Log.warn (fun f -> f "Cancelled at unexpected point!");
              Lwt.fail ex
            | ex ->
              Lwt.fail ex
          )
      in
      limiter_thread := Some use_thread;
      use_thread >>= fun build_job ->
      Lwt.pause () >>= fun () ->
      match Capability.problem build_job with
      | None -> Lwt.return build_job
      | Some err ->
        if Capability.problem sched = None then (
          (* The job failed but we're still connected to the scheduler. Report the error. *)
          Lwt.fail_with (Fmt.strf "%a" Capnp_rpc.Exception.pp err)
        ) else (
          limiter_thread := None;
          stage := `Init;
          aux ()
        )
  in
  aux (), cancel

let tail ~job build_job =
  let rec aux start =
    Cluster_api.Job.log build_job start >>= function
    | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Lwt_result.return ()
    | Ok (data, next) ->
      Current.Job.write job data;
      aux next
  in aux 0L

let run_job ~job build_job =
  let on_cancel _ =
    Cluster_api.Job.cancel build_job >|= function
    | Ok () -> ()
    | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
  in
  Current.Job.with_handler job ~on_cancel @@ fun () ->
  let result = Cluster_api.Job.result build_job in
  tail ~job build_job >>!= fun () ->
  result >>= function
  | Error (`Capnp e) -> Lwt_result.fail (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
  | Ok _ as x -> Lwt.return x

let create ?(max_pipeline=200) sr =
  let rate_limits = Hashtbl.create 10 in
  { sr; sched = Lwt.fail_with "init"; rate_limits; max_pipeline }

let pool ~job ~pool ~action ~cache_hint ?src ?(urgent=urgent_if_high) t =
  Current.Pool.of_fn ~label:"OCluster" @@ submit ~job ~pool ~action ~cache_hint ~urgent ?src t

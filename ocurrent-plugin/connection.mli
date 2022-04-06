(** A connection to the OCluster scheduler.
    Provides automatic reconnection on failure, queuing with rate limiting,
    metrics reporting, etc. This can be passed to [Current_ocluster.v] for
    simple cases, or it can used directly to implement custom pipeline steps if
    that isn't flexible enough. *)

open Capnp_rpc_lwt

type t

val create :
  ?max_pipeline:int -> 
  Cluster_api.Raw.Client.Submission.t Sturdy_ref.t ->
  t
(** [create submission_service] is a connection that submits jobs to [submission_service].
    @param max_pipeline : how many items to queue up at the scheduler per (pool, urgency). *)

val pool :
  job:Current.Job.t ->
  pool:string ->
  action:Cluster_api.Submission.send_action ->
  cache_hint:string ->
  ?src:string * string list ->
  ?secrets:(string * string) list ->
  ?urgent:([`High | `Low] -> bool) ->
  t ->
  Cluster_api.Raw.Client.Job.t Capnp_rpc_lwt.Capability.t Current.Pool.t
(** [pool ~job ~pool ~action ~cache_hint t] is a resource pool, suitable for passing to [Current.Job.start_with].
    Submits [action] to the pool named [pool] at [t].
    If [t] is disconnected, it will keep trying to reconnect until it succeeds.
    @param job Used to write log messages.
    @param cache_hint Hint to the scheduler (similar jobs should be assigned to the same machine, if possible).
    @param urgent Used to calculate the job's urgency, possibly using the OCurrent priority.
                  The default is to mark high priority jobs as urgent. *)

val run_job :
  job:Current.Job.t ->
  Cluster_api.Raw.Client.Job.t Capability.t ->
  (string, [> `Msg of string ]) result Lwt.t
(** [run_job ~job ocluster_job] tails the log of [ocluster_job] to [job] and then returns the job's result. *)
